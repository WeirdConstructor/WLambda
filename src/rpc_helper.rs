// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Provides a helper to create a WLambda evaluation worker thread in your program.

This is helpful if you want to provide a main thread that defines and evaluates
a WLambda program in a multi threaded Rust program.

For instance it's used by a HTTP server I wrote that used worker threads to
handle requests. In that environment the central WLambda main thread was then
queried for executing the requests. This of course makes WLambda the bottle
neck for request responses. But if WLambda decided to just serve a file, the
file I/O could be done outside the WLambda control flow.
*/

use crate::vval::*;
use crate::threads::*;
use crate::compiler::*;

const RPC_MSG_CALL : i64 = 1;
const RPC_MSG_SEND : i64 = 2;

#[derive(Clone)]
pub struct RPCHandle {
    free_queue:     AValChannel,
    error_channel:  AValChannel,
    request_queue:  AValChannel,
}

impl Default for RPCHandle {
    fn default() -> Self { Self::new() }
}


impl RPCHandle {
    pub fn new() -> Self {
        Self {
            free_queue:     AValChannel::new_direct(),
            error_channel:  AValChannel::new_direct(),
            request_queue:  AValChannel::new_direct(),
        }
    }

    pub fn register_global_functions(&self, prefix: &str, ctx: &mut EvalContext) {
        let caller = self.clone();
        ctx.set_global_var(&format!("{}_call", prefix),
            &VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    let args =
                        if argc == 1 { VVal::None }
                        else {
                            let a = VVal::vec();
                            for i in 1..argc {
                                a.push(env.arg(i).clone());
                            }
                            a
                        };
                    Ok(caller.call(&env.arg(0).s_raw(), args))
                }, Some(1), None, false));

        let caller = self.clone();
        ctx.set_global_var(&format!("{}_send", prefix),
            &VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    let args =
                        if argc == 1 { VVal::None }
                        else {
                            let a = VVal::vec();
                            for i in 1..argc {
                                a.push(env.arg(i).clone());
                            }
                            a
                        };
                    caller.send(&env.arg(0).s_raw(), args);
                    Ok(VVal::None)
                }, Some(1), None, false));
    }

    fn get_request(&self) -> Option<AtomicAValSlot> {
        match self.free_queue.try_recv() {
            VVal::Opt(Some(o)) => {
                if let VVal::Usr(mut ud) = (*o).clone() {
                    if let Some(ud) = ud.as_any().downcast_ref::<AtomicAValSlot>() {
                        Some(ud.clone())
                    } else {
                        Some(AtomicAValSlot::new())
                    }
                } else {
                    None
                }
            },
            VVal::Opt(None)
            | VVal::None => Some(AtomicAValSlot::new()),
            _ => None,
        }
    }

    pub fn call(&self, target: &str, args: VVal) -> VVal {
        let resp = self.get_request().expect("AtomicAValSlot allocation");

        let v = VVal::vec();
        v.push(VVal::Int(RPC_MSG_CALL));
        v.push(VVal::Usr(Box::new(resp.clone())));
        v.push(VVal::new_sym(target));
        v.push(args);

        self.request_queue.send(&v);
        let ret = resp.recv_timeout(None);
        self.free_queue.send(&v.at(1).expect("the AtomicAValSlot"));
        ret
    }

    pub fn send(&self, target: &str, args: VVal) {
        let v = VVal::vec();
        v.push(VVal::Int(RPC_MSG_SEND));
        v.push(VVal::None);
        v.push(VVal::new_sym(target));
        v.push(args);

        self.request_queue.send(&v);
    }

    pub fn fetch_error(&self) -> Option<VVal> {
        let msg = self.error_channel.try_recv();
        if msg.is_some() {
            Some(msg)
        } else {
            None
        }
    }
}

pub enum RPCHandlerError {
    Disconnected,
}

pub fn rpc_handler(
    ctx: &mut EvalContext,
    handle: &RPCHandle,
    interval_timeout: std::time::Duration)
{
    let quit = std::rc::Rc::new(std::cell::RefCell::new(false));

    let qr = quit.clone();
    ctx.set_global_var(
        "thread:quit",
        &VValFun::new_fun(move |_env: &mut Env, _argc: usize| {
            *qr.borrow_mut() = true;
            Ok(VVal::None)
        }, Some(0), Some(0), false));

    loop {
        if let Err(RPCHandlerError::Disconnected) =
            rpc_handler_step(ctx, handle, interval_timeout)
        {
            break;
        }

        if *quit.borrow() { break; }
    }
}

pub fn rpc_handler_step(
    ctx: &mut EvalContext,
    handle: &RPCHandle,
    timeout: std::time::Duration) -> Result<(), RPCHandlerError>
{
    let res = handle.request_queue.recv_timeout(Some(timeout));
    if res.is_err() {
        Err(RPCHandlerError::Disconnected)

    } else if let VVal::Opt(Some(m)) = res {
        let cmd  = m.at(0).unwrap_or(VVal::None).i();
        let resp = m.at(1).unwrap_or(VVal::None);
        let name = m.at(2).unwrap_or(VVal::None);
        let args = m.at(3).unwrap_or(VVal::None);

        match cmd {
            RPC_MSG_CALL => {
                if let VVal::Usr(mut resp) = resp {
                    let resp =
                        resp.as_any()
                            .downcast_mut::<AtomicAValSlot>()
                            .expect("AtomicAValSlot in RPC_MSG_CALL");

                    name.with_s_ref(|name| {
                        if let Some(v) = ctx.get_global_var(name) {
                            let arg =
                                if args.is_none() { vec![] }
                                else { args.to_vec() };
                            let ret = match ctx.call(&v, &arg) {
                                Ok(v)  => v,
                                Err(e) =>
                                    VVal::err_msg(
                                        &format!("Panic in call to '{}': {:?}",
                                                 name, e)),
                            };
                            resp.send(&ret);
                        } else {
                            resp.send(
                                &VVal::err_msg(
                                    &format!("No such global on call: {}", name)));
                        }
                    });
                } else {
                    panic!("Didn't get a AtomicAValSlot in RPC_MSG_CALL");
                }
            },
            RPC_MSG_SEND => {
                name.with_s_ref(|name| {
                    if let Some(v) = ctx.get_global_var(name) {
                        let arg =
                            if args.is_none() { vec![] }
                            else { args.to_vec() };
                        let ret = ctx.call(&v, &arg).unwrap_or(VVal::None);
                        if ret.is_err() {
                            handle.error_channel.send(
                                &VVal::err_msg(&format!("Error on send: {}", ret.s())));
                        }
                    } else {
                        handle.error_channel.send(
                            &VVal::err_msg(&format!("No such global on send: {}", name)));
                    }
                });
            },
            _ => (),
        }

        Ok(())
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn check_rpc() {
        use crate::vval::*;
        use crate::rpc_helper::*;

        // Get some random user thread:
        let mut ctx = crate::compiler::EvalContext::new_default();

        let msg_handle = RPCHandle::new();
        let sender = msg_handle.clone();
        sender.register_global_functions("worker", &mut ctx);

        let t = std::thread::spawn(move || {
            let mut ctx = crate::compiler::EvalContext::new_default();

            ctx.eval("!:global X = 123").unwrap();

            rpc_handler(
                &mut ctx, &msg_handle, std::time::Duration::from_secs(1));
        });

        ctx.eval("worker_send :mfeofe").unwrap();
        let ret = ctx.eval("worker_call :displayln \"hello world from worker thread!\";").unwrap();
        assert_eq!(ret.s(), "$e $[\"No such global on call: displayln\",\"\"]");
        ctx.eval("std:assert_eq (worker_call :std:eval \"X\") 123;").unwrap();

        sender.call("thread:quit", VVal::None);

        let err = sender.fetch_error().unwrap();
        assert_eq!(err.s(), "$o($e $[\"No such global on send: mfeofe\",\"\"])");

        t.join().unwrap();
    }
}
