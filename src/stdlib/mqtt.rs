// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#[cfg(feature="rumqttc")]
use crate::VVal;
#[allow(unused_imports)]
use crate::{Env, StackAction};
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[cfg(feature="rumqttc")]
use rumqttc::{MqttOptions, Client, QoS, Connection, Event, Packet};
use crate::compiler::*;
#[cfg(feature="rumqttc")]
use std::rc::Rc;
#[cfg(feature="rumqttc")]
use std::cell::RefCell;

#[cfg(feature="rumqttc")]
#[derive(Clone)]
struct VMQTTClient {
    client: Client,
}

#[cfg(feature="rumqttc")]
impl VMQTTClient {
    pub fn new(client: Client) -> Self {
        Self {
            client: client,
        }
    }
}

#[cfg(feature="rumqttc")]
impl VValUserData for VMQTTClient {
    fn s(&self) -> String {
        format!("$<MQTTClient>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        Some(Box::new(VMQTTClient {
            client: self.client.clone()
        }))
    }
}

impl crate::threads::ThreadSafeUsr for VMQTTClient {
    fn to_vval(&self) -> VVal {
        VVal::Usr(Box::new(VMQTTClient {
            client: self.client.clone()
        }))
    }
}

#[cfg(feature="rumqttc")]
#[derive(Clone)]
struct VMQTTConn {
    con: Rc<RefCell<Connection>>,
}

#[cfg(feature="rumqttc")]
impl VMQTTConn {
    pub fn new(con: Connection) -> Self {
        Self {
            con: Rc::new(RefCell::new(con)),
        }
    }
}

#[cfg(feature="rumqttc")]
impl VValUserData for VMQTTConn {
    fn s(&self) -> String {
        format!("$<MQTTConnection>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature="rumqttc")]
    st.fun("mqtt:client:new", |env: &mut Env, _argc: usize| {
        let mut opts =
            MqttOptions::new(
                env.arg(0).s_raw(),
                env.arg(1).s_raw(),
                env.arg(2).i() as u16);
        opts.set_keep_alive(std::time::Duration::from_secs(5));
        let (client, connection) = Client::new(opts, 25);
        Ok(VVal::pair(
            VVal::new_usr(VMQTTClient::new(client)),
            VVal::new_usr(VMQTTConn::new(connection))))
    }, Some(3), Some(3), false);

    st.fun("mqtt:client:publish", |env: &mut Env, _argc: usize| {
        let mut cl = env.arg(0);
        let topic = env.arg(1);
        let msg = env.arg(2);
        let res = cl.with_usr_ref(|cl: &mut VMQTTClient| -> Result<VVal, rumqttc::ClientError> {

            topic.with_s_ref(|s|
                msg.with_bv_ref(|bv|
                    cl.client.publish(s, QoS::AtLeastOnce, false, bv)))?;

            Ok(VVal::Bol(true))
        }).unwrap_or_else(||
            Ok(env.new_err(format!(
                "mqtt:client:publish: First argument not a VMQTTClient handle! {}",
                cl.s()))));

        match res {
            Err(e) =>
                Ok(env.new_err(
                    format!("MQTT Publish Error: {}", e))),
            Ok(v) => Ok(v),
        }
    }, Some(3), Some(3), false);

    st.fun("mqtt:client:subscribe", |env: &mut Env, _argc: usize| {
        let mut cl = env.arg(0);
        let res = cl.with_usr_ref(
            |cl: &mut VMQTTClient| -> Result<VVal, rumqttc::ClientError> {

                env.arg(1).with_s_ref(|s|
                    cl.client.subscribe(s, QoS::AtMostOnce))?;

                Ok(VVal::Bol(true))
            }).unwrap_or_else(||
                Ok(env.new_err(format!(
                    "mqtt:client:subscribe: First argument not a VMQTTClient handle! {}",
                    cl.s()))));

        match res {
            Err(e) =>
                Ok(env.new_err(
                    format!("MQTT Publish Error: {}", e))),
            Ok(v) => Ok(v),
        }
    }, Some(2), Some(2), false);

    st.fun("mqtt:client:connection:run", |env: &mut Env, _argc: usize| {
        let mut con = env.arg(0);
        let mut cb = env.arg(1);

        let chan = cb.with_usr_ref(|chan: &mut crate::threads::AValChannel| {
            chan.fork_sender_direct()
        });

        let chan =
            if let Some(chan) = chan {
               match chan {
                    Ok(chan) => Some(chan),
                    Err(err) => {
                        return
                            Ok(VVal::err_msg(
                                &format!("Failed to fork sender, can't get lock: {}", err)));
                    },
               }
            } else {
                None
            };

        con.with_usr_ref(|con: &mut VMQTTConn| {
            for notification in con.con.borrow_mut().iter() {
                let notification =
                    match notification {
                        Err(e) => {
                            return Ok(env.new_err(
                                format!("MQTT Connection Run Error: {}", e)));
                        },
                        Ok(n) => n,
                    };

                match notification {
                    Event::Incoming(inc) => {
                        match inc {
                            Packet::Publish(pubpkt) => {
                                if let Some(chan) = &chan {
                                    chan.send(&VVal::pair(
                                        VVal::new_str_mv(pubpkt.topic),
                                        VVal::new_byt(
                                            pubpkt.payload.as_ref().to_vec())));

                                } else {
                                    env.push(VVal::new_str_mv(pubpkt.topic));
                                    env.push(VVal::new_byt(pubpkt.payload.as_ref().to_vec()));
                                    match cb.call_internal(env, 2) {
                                        Ok(_)                      => { },
                                        Err(StackAction::Break(_)) => { break; },
                                        Err(StackAction::Next)     => { },
                                        Err(e)                     => { env.popn(2); return Err(e); },
                                    };
                                    env.popn(2);
                                }
                            },
                            _ => { },
                        }
                    },
                    _ => {}
                }
            }

            Ok(VVal::Bol(true))
        }).unwrap_or_else(||
            Ok(env.new_err(format!(
                "mqtt:client:connection:run: First argument not a VMQTTConn handle! {}",
                con.s()))))
    }, Some(2), Some(2), false);
}

//#[cfg(feature="reqwest")]
//#[derive(Debug)]
//enum VHttpClError {
//    ReqwestError(reqwest::Error),
//    InvalidHeaderName,
//    InvalidHeaderValue,
//}
//
//#[cfg(feature="reqwest")]
//impl std::fmt::Display for VHttpClError {
//    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        match self {
//            VHttpClError::ReqwestError(e)    => write!(f, "{}", e),
//            VHttpClError::InvalidHeaderName  => write!(f, "InvalidHeaderName"),
//            VHttpClError::InvalidHeaderValue => write!(f, "InvalidHeaderValue"),
//        }
//    }
//}
//
