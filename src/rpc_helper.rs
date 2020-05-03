use crate::vval::*;
use crate::threads::*;
use crate::compiler::*;

const RPC_MSG_CALL : i64 = 1;
const RPC_MSG_SEND : i64 = 2;

#[derive(Clone)]
pub struct RPCHandle {
    free_queue: AValChannel,
    request_queue:  AValChannel,
}

impl RPCHandle {
    pub fn new() -> Self {
        Self {
            free_queue:     AValChannel::new_direct(),
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
            Ok(VVal::Usr(ud)) => {
                if let Some(ud) = cl_ud.as_any().downcast_mut::<AtomicAValSlot>() {
                    Some(ud)
                } else {
                    Some(AtomicAValSlot::new()),
                }
            },
            Ok(_) | Err(TryRecvError::Empty) => Some(AtomicAValSlot::new()),
            Err(e) => None,
        }
    }

    pub fn call(&self, target: &str, args: VVal) -> VVal {
        let v = VVal::vec();
        v.push(VVal::Int(RPC_MSG_CALL));
        v.push(VVal::new_sym(target));
        v.push(args);
        self.send_channel.send(&v);
        self.reply_channel.recv_timeout(None)
    }

    pub fn send(&self, target: &str, args: VVal) {
        let v = VVal::vec();
        v.push(VVal::Int(RPC_MSG_SEND));
        v.push(VVal::new_sym(target));
        v.push(args);
        self.send_channel.send(&v);
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
        match rpc_handler_step(ctx, handle, interval_timeout) {
            Err(RPCHandlerError::Disconnected) => {
                break;
            },
            _ => (),
        }
        if *quit.borrow() { break; }
    }
}

pub fn rpc_handler_step(
    ctx: &mut EvalContext,
    handle: &RPCHandle,
    timeout: std::time::Duration) -> Result<(), RPCHandlerError>
{
    let res = handle.send_channel.recv_timeout(Some(timeout));
    if res.is_err() {
        Err(RPCHandlerError::Disconnected)

    } else if let VVal::Opt(Some(m)) = res {
        let cmd  = m.at(0).unwrap_or_else(|| VVal::None).i();
        let name = m.at(1).unwrap_or_else(|| VVal::None);
        let args = m.at(2).unwrap_or_else(|| VVal::None);

        match cmd {
            RPC_MSG_CALL => {
                name.with_s_ref(|name| {
                    if let Some(v) = ctx.get_global_var(name) {
                        let arg =
                            if args.is_none() { vec![] }
                            else { args.to_vec() };
                        let ret = ctx.call(&v, &arg).unwrap_or_else(|_| VVal::None);
                        handle.reply_channel.send(&ret);
                    } else {
                        handle.reply_channel.send(
                            &VVal::err_msg(&format!("No such global on call: {}", name)));
                    }
                });
            },
            RPC_MSG_SEND => {
                name.with_s_ref(|name| {
                    if let Some(v) = ctx.get_global_var(name) {
                        let arg =
                            if args.is_none() { vec![] }
                            else { args.to_vec() };
                        let ret = ctx.call(&v, &arg).unwrap_or_else(|_| VVal::None);
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
