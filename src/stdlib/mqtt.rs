// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#[cfg(feature="rumqttd")]
use crate::first_addr;

#[cfg(feature="rumqttc")]
use crate::VVal;
#[allow(unused_imports)]
use crate::{Env, StackAction};
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[cfg(feature="rumqttc")]
use rumqttc::{MqttOptions, Client, QoS, Connection, Event, Packet};
#[cfg(feature="rumqttd")]
use librumqttd::{Broker, Config};
use crate::compiler::*;
#[cfg(feature="rumqttc")]
use std::rc::Rc;
#[cfg(feature="rumqttc")]
use std::cell::RefCell;

#[cfg(feature="rumqttc")]
use std::sync::{Arc, Mutex};

#[cfg(feature="rumqttc")]
struct ThreadClientHandle {
    client:      Option<Client>,
    subscribe:   Vec<String>,
}

#[cfg(feature="rumqttc")]
impl ThreadClientHandle {
    fn with_client<F: FnMut(&mut Client) -> Result<(), rumqttc::ClientError>>(&mut self, mut fun: F) -> Result<(), DetClientError> {
        if let Some(client) = self.client.as_mut() {
            match fun(client) {
                Ok(()) => Ok(()),
                Err(e) => Err(DetClientError::ClientError(e)),
            }
        } else {
            Err(DetClientError::NotConnected)
        }
    }
}

#[cfg(feature="rumqttc")]
#[derive(Debug)]
pub enum DetClientError {
    NotConnected,
    ClientError(rumqttc::ClientError),
}

#[cfg(feature="rumqttc")]
impl std::fmt::Display for DetClientError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DetClientError::NotConnected => write!(f, "Not Connected"),
            DetClientError::ClientError(err) =>
                write!(f, "MQTT Client Error: {}", err),
        }
    }
}

#[cfg(feature="rumqttc")]
#[derive(Clone)]
struct DetachedMQTTClient {
    options:     MqttOptions,
    chan:        crate::threads::AValChannel,
    client:      Arc<Mutex<ThreadClientHandle>>,
}

#[cfg(feature="rumqttc")]
impl DetachedMQTTClient {
    pub fn new(chan: crate::threads::AValChannel, id: &str, host: &str, port: u16) -> Self {
        let mut options = MqttOptions::new(id, host, port);
        options.set_keep_alive(std::time::Duration::from_secs(5));
        Self {
            options,
            chan,
            client: Arc::new(Mutex::new(ThreadClientHandle {
                client:    None,
                subscribe: vec![],
            })),
        }
    }

    pub fn publish(&self, topic: &str, payload: &[u8]) -> Result<(), DetClientError> {
        if let Ok(mut hdl) = self.client.lock() {
            hdl.with_client(|cl| {
                cl.publish(topic, QoS::AtLeastOnce, false, payload)
            })

        } else {
            Err(DetClientError::NotConnected)
        }
    }

    pub fn subscribe(&self, topic: &str) -> Result<(), DetClientError> {
        if let Ok(mut hdl) = self.client.lock() {
            hdl.subscribe.push(topic.to_string());
            hdl.with_client(|cl| cl.subscribe(topic, QoS::AtLeastOnce))

        } else {
            Err(DetClientError::NotConnected)
        }
    }

    pub fn start(&mut self) {
        let chan    = self.chan.clone();
        let client  = self.client.clone();
        let options = self.options.clone();

        std::thread::spawn(move || {
            loop {
                let mut con = None;

                if let Ok(mut hdl) = client.lock() {
                    let (client, connection) = Client::new(options.clone(), 25);
                    hdl.client = Some(client);

                    let mut retry = false;
                    let topics = hdl.subscribe.clone();
                    for topic in topics.iter() {
                        if let Err(e) =
                            hdl.client
                                .as_mut()
                                .unwrap()
                                .subscribe(topic, QoS::AtMostOnce)
                        {
                            chan.send(&VVal::pair(
                                VVal::new_sym("$WL/error/subscribe"),
                                VVal::new_str_mv(format!("{}", e))));
                            retry = true;
                            break;
                        }
                    }

                    if retry {
                        hdl.client = None;
                        break;
                    }

                    con = Some(connection);
                }

                if let Some(mut connection) = con {
                    chan.send(&VVal::pair(
                        VVal::new_sym("$WL/connected"), VVal::None));

                    for noti in connection.iter() {
                        let noti =
                            match noti {
                                Err(e) => {
                                    chan.send(&VVal::pair(
                                        VVal::new_sym("$WL/error"),
                                        VVal::new_str_mv(format!("{}", e))));
                                    break;
                                },
                                Ok(noti) => noti,
                            };

                        match noti {
                            Event::Incoming(inc) => {
                                match inc {
                                    Packet::Publish(pubpkt) => {
                                        chan.send(&VVal::pair(
                                            VVal::new_str_mv(pubpkt.topic),
                                            VVal::new_byt(
                                                pubpkt.payload.as_ref().to_vec())));
                                    },
                                    _ => { },
                                }
                            },
                            _ => { }
                        }
                    }
                }

                if let Ok(mut hdl) = client.lock() {
                    hdl.client = None;
                }
                std::thread::sleep(std::time::Duration::from_secs(5));
            }
        });
    }
}

#[cfg(feature="rumqttc")]
impl VValUserData for DetachedMQTTClient {
    fn s(&self) -> String {
        format!("$<DetachedMQTTClient>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv_ref();
        match key {
            "subscribe" => {
                if argv.len() != 1 {
                    return
                        Err(StackAction::panic_str(
                            "subscribe method expects 1 argument".to_string(),
                            None,
                            env.argv()))
                }

                let ret = argv[0].with_s_ref(|s| self.subscribe(s));
                match ret {
                    Ok(_)  => Ok(VVal::Bol(true)),
                    Err(e) => Ok(env.new_err(format!("subscribe Error: {}", e)))
                }
            },
            "publish" => {
                if argv.len() != 2 {
                    return
                        Err(StackAction::panic_str(
                            "publish method expects 2 argument".to_string(),
                            None,
                            env.argv()))
                }

                let ret =
                    argv[0].with_s_ref(|topic|
                        argv[1].with_bv_ref(|payload|
                            self.publish(topic, payload)));
                match ret {
                    Ok(_)  => Ok(VVal::Bol(true)),
                    Err(e) => Ok(env.new_err(format!("publish Error: {}", e)))
                }
            },
            _ => {
                Err(StackAction::panic_str(
                    format!("unknown method called: {}", key),
                    None,
                    env.argv()))
            },
        }
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        Some(Box::new(self.clone()))
    }
}

#[cfg(feature="rumqttc")]
impl crate::threads::ThreadSafeUsr for DetachedMQTTClient {
    fn to_vval(&self) -> VVal {
        VVal::Usr(Box::new(self.clone()))
    }
}

#[cfg(feature="rumqttd")]
#[derive(Clone)]
struct MQTTBroker {
}

#[cfg(feature="rumqttd")]
fn cfg2broker_config(env: &mut Env, cfg: VVal) -> Result<Config, VVal>  {
//    let servers_cfg = cfg.v_k("servers");

//    servers_cfg.with_iter(|it| {
//        let mut i = 0;
//        for (v, k) in it {
//            i += 1;

    let mut servers = std::collections::HashMap::new();
    let listen = first_addr!(cfg.v_k("listen"), env)?;
    let srv = librumqttd::ServerSettings {
        listen,
        next_connection_delay_ms: 1,
        connections: librumqttd::ConnectionSettings {
            connection_timeout_ms: 100,
            max_client_id_len:     256,
            throttle_delay_ms:     0,
            max_payload_size:      10240,
            max_inflight_count:    500,
            max_inflight_size:     10240,
            login_credentials: None,
        },
        cert: None,
    };

    servers.insert(format!("{}", 1), srv);

    let cons_listen = first_addr!(cfg.v_k("console_listen"), env)?;

    let config = Config {
        id: cfg.v_ik("id") as usize,
        servers,
        cluster: None,
        replicator: None,
        console: librumqttd::ConsoleSettings {
            listen: cons_listen,
        },
        router: Default::default(),
    };

    Ok(config)
}

#[cfg(feature="rumqttd")]
impl MQTTBroker {
    pub fn setup(env: &mut Env, cfg: VVal) -> Result<Self, VVal> {
        let config = cfg2broker_config(env, cfg)?;
        let mut broker = Broker::new(config);

        std::thread::spawn(move || {
            broker.start().unwrap();
            // TODO: Log errors?!
            println!("BROKER STRATED");
        });

        Ok(Self { })
    }
}

#[cfg(feature="rumqttd")]
impl VValUserData for MQTTBroker {
    fn s(&self) -> String {
        format!("$<MQTTBroker>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv_ref();
        match key {
            "get_link" => {
                if argv.len() != 0 {
                    return
                        Err(StackAction::panic_str(
                            "link method expects no argument".to_string(),
                            None,
                            env.argv()))
                }

//                let ret = argv[0].with_s_ref(|s| self.link.clone()(s));
//                match ret {
//                    Ok(_)  => Ok(VVal::Bol(true)),
//                    Err(e) => Ok(env.new_err(format!("subscribe Error: {}", e)))
//                }
                Ok(VVal::None)
            },
            _ => {
                Err(StackAction::panic_str(
                    format!("unknown method called: {}", key),
                    None,
                    env.argv()))
            },
        }
    }
}

//#[cfg(feature="rumqttd")]
//impl crate::threads::ThreadSafeUsr for MQTTBroker {
//    fn to_vval(&self) -> VVal {
//        VVal::Usr(Box::new(VMQTTClient {
//            client: self.client.clone()
//        }))
//    }
//}

#[cfg(feature="rumqttd")]
#[derive(Clone)]
struct MQTTLink {
    con: Rc<RefCell<Connection>>,
}

#[cfg(feature="rumqttd")]
impl MQTTLink {
    pub fn new(con: Connection) -> Self {
        Self {
            con: Rc::new(RefCell::new(con)),
        }
    }
}

#[cfg(feature="rumqttd")]
impl VValUserData for MQTTLink {
    fn s(&self) -> String {
        format!("$<MQTTBrokerLink>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

//#[cfg(feature="rumqttd")]
//impl crate::threads::ThreadSafeUsr for MQTTLink {
//    fn to_vval(&self) -> VVal {
//        VVal::Usr(Box::new(MQTTLink {
//            client: self.client.clone()
//        }))
//    }
//}


#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature="rumqttc")]
    st.fun("mqtt:client:new", |env: &mut Env, _argc: usize| {
        let mut chan = env.arg(0);
        let chan =
            chan.with_usr_ref(|chan: &mut crate::threads::AValChannel| {
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
                return
                    Ok(env.new_err(format!(
                        "mqtt:client:detached:new: First argument not a std:sync:mpsc handle! {}",
                        env.arg(0).s())));
            };

        let mut cl =
            DetachedMQTTClient::new(
                chan.unwrap(),
                &env.arg(1).s_raw(),
                &env.arg(2).s_raw(),
                env.arg(3).i() as u16);
        cl.start();
        Ok(VVal::new_usr(cl))
    }, Some(4), Some(4), false);

    #[cfg(feature="rumqttd")]
    st.fun("mqtt:broker:new", |env: &mut Env, _argc: usize| {
        let config = env.arg(0);

        match MQTTBroker::setup(env, config) {
            Ok(broker) => Ok(VVal::new_usr(broker)),
            Err(ev)    => Ok(ev),
        }
    }, Some(1), Some(1), false);
}

/*


!chan = std:sync:mpsc:new[];

!broker = std:mqtt:broker:new ${ ... config here ..., link = "client_id" };
!link = broker.get_link[];
link.subscribe "test/me";
link.publish "test/me" $b"payload";

while $t {
    std:displayln chan.recv[];
}


*/
