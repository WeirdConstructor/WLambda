// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Threading API:

```
#[cfg(feature="regex")]
{
    use wlambda::vval::*;

    // Get some random user thread:
    let mut ctx = wlambda::EvalContext::new_default();

    let mut msg_handle = wlambda::threads::MsgHandle::new();
    // You may register on multiple threads, but only one thread can use it at a time.
    let sender = msg_handle.sender();
    sender.register_on_as(&mut ctx, "worker");

    let t = std::thread::spawn(move || {
        let quit = std::rc::Rc::new(std::cell::RefCell::new(false));

        let global_t = wlambda::GlobalEnv::new_default();

        let qr = quit.clone();
        global_t.borrow_mut()
            .add_func("thread:quit", move |env: &mut Env, _argc: usize| {
                *qr.borrow_mut() = true;
                Ok(VVal::Nul)
            }, Some(0), Some(0));

        let mut ctx = wlambda::EvalContext::new(global_t);

        ctx.eval("!:global X = 123");

        // msg_handle.run(&mut ctx);
        // or alternatively:

        loop {
            // Tries to handle one RPC call within 10ms.
            if let None = msg_handle.step(&mut ctx, &std::time::Duration::from_millis(10)) {
                break;
            }

            if *quit.borrow() { break; }

            // do some other work here, that is not blocking the thread indefinitely.
        }
    });

    // Calls the global `displayln` in the Worker thread with the supplied arguments.
    ctx.eval("worker_call :displayln \"hello world from worker thread!\";").unwrap();

    ctx.eval("std:assert_eq (worker_call :std:eval \"X\") 123;").unwrap();

    sender.call("thread:quit", VVal::Nul);

    t.join();
}
```

The alternative async messaging API, that does not provide any return values
from the Thread. However, you could theoretically generate two message handles
for a two way communication.

```
#[cfg(feature="regex")]
{
    use wlambda::vval::*;

    // Get some random user thread:
    let mut ctx = wlambda::EvalContext::new_default();

    let mut msg_handle = wlambda::threads::MsgHandle::new();

    let sender = msg_handle.sender();

    // You may register on multiple threads, but only one thread can use it at a time.
    sender.register_on_as(&mut ctx, "worker");

    let t = std::thread::spawn(move || {
        let mut ctx  = wlambda::EvalContext::new_default();

        // This also implicitly defines a thread:quit:
        msg_handle.run(&mut ctx);
    });

    sender.call("thread:quit", VVal::Nul);

    t.join();
}

```
*/

#[allow(unused_imports)]
use crate::compiler::EvalContext;
#[allow(unused_imports)]
use crate::vval::*;
#[allow(unused_imports)]
use std::collections::VecDeque;
#[allow(unused_imports)]
use std::sync::{Arc, Mutex, Condvar};

/// The Sender sends RPC calls to the Receiver thread.
/// Any values passed by WLambda code are serialized into msgpack
/// internally and transmitted to the thread
/// in String form.
/// This means, your values must not be cyclic or contain non serializable
/// data like external handles.
///
/// The Sender also provides a method for storing a sending function
/// in the global variables for the EvalContext.
///
///```
/// let mut ctx = wlambda::compiler::EvalContext::new_default();
///
/// let mut msg_handle = wlambda::threads::MsgHandle::new();
/// let sender = msg_handle.sender();
///
/// // You may register on multiple threads, but only one thread can use it at a time.
/// sender.register_on_as(&mut ctx, "worker");
///
/// // start thread here...
///```
#[derive(Debug, Clone)]
#[cfg(feature="rmp-serde")]
pub struct Sender {
    receiver: Arc<Receiver>,
}

#[cfg(feature="rmp-serde")]
impl Sender {
    fn new(receiver: Arc<Receiver>) -> Self {
        Sender { receiver }
    }

    /// Registers a call and message sending function on the supplied EvalContext.
    /// You can call this function with the global variable name
    /// of the thread function you want to call and additional
    /// arguments to that function.
    ///
    /// ```no_run
    /// let mut ctx = wlambda::compiler::EvalContext::new_default();
    ///
    /// let mut msg_handle = wlambda::threads::MsgHandle::new();
    /// let sender = msg_handle.sender();
    ///
    /// sender.register_on_as(&mut ctx, "worker");
    ///
    /// ctx.eval("worker_call :displayln \"hello world from worker thread!\";").unwrap();
    ///
    /// // _send will not wait for the call to be finished or even started.
    /// ctx.eval("worker_send :displayln \"hello world from worker thread!\";").unwrap();
    /// ```
    pub fn register_on_as(&self, ctx: &mut EvalContext, variable_name: &str) {
        let sender = self.clone();
        ctx.set_global_var(&format!("{}_call", variable_name),
            &VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    let args =
                        if argc == 1 { VVal::Nul }
                        else {
                            let a = VVal::vec();
                            for i in 1..argc {
                                a.push(env.arg(i).clone());
                            }
                            a
                        };
                    Ok(sender.call(&env.arg(0).s_raw(), args))
                }, Some(1), None, false));

        let sender = self.clone();
        ctx.set_global_var(&format!("{}_send", variable_name),
            &VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    let args =
                        if argc == 1 { VVal::Nul }
                        else {
                            let a = VVal::vec();
                            for i in 1..argc {
                                a.push(env.arg(i).clone());
                            }
                            a
                        };
                    sender.send(&env.arg(0).s_raw(), args);
                    Ok(VVal::Nul)
                }, Some(1), None, false));
    }

    /// Calls the global variable in the receiver thread
    /// with the given argument vector. You can pass `none` value
    /// as args too.
    pub fn call(&self, var_name: &str, args: VVal) -> VVal {
        let r = &*self.receiver;

        let mut mx = r.mx.lock().unwrap();
        {
            while mx.0 != RecvState::Open {
                mx = r.cv.wait(mx).unwrap();
            }

            mx.0 = RecvState::Call;
            mx.1 = var_name.to_string();
            mx.2 = args.to_msgpack().unwrap();
            mx.3 = false;

            r.cv.notify_all();
        }

        while mx.0 != RecvState::Return {
            mx = r.cv.wait(mx).unwrap();
        }

        let ret =
            if mx.3 {
                VVal::err_msg(&String::from_utf8(mx.2.clone()).unwrap())
            } else {
                VVal::from_msgpack(&mx.2).unwrap()
            };

        mx.0 = RecvState::Open;
        mx.1 = String::from("");
        mx.2 = String::from("").into_bytes();
        mx.3 = false;

        r.cv.notify_all();

        ret
    }

    /// With send you can asynchronously send messages in form of
    /// method calls to the receiver. The Receiver will process all
    /// received messages per `step()`. Multiple senders can send
    /// messages, while multiple Receivers can process them.
    pub fn send(&self, var_name: &str, args: VVal) {
        let r = &*self.receiver;
        let mut mx = r.mx.lock().unwrap();
        while !(mx.0 == RecvState::Open || mx.0 == RecvState::Msg) {
            mx = r.cv.wait(mx).unwrap();
        }
        mx.0 = RecvState::Msg;
        mx.4.push_back((
            var_name.to_string(),
            args.to_msgpack().unwrap()));
    }
}

#[cfg(feature="rmp-serde")]
#[derive(Debug, Copy, Clone, PartialEq)]
enum RecvState {
    Open,
    Msg,
    Call,
    Return,
}

#[cfg(feature="rmp-serde")]
type RecvData = (RecvState, String, Vec<u8>, bool, VecDeque<(String, Vec<u8>)>);
#[cfg(feature="rmp-serde")]
type RecvMutex = Mutex<RecvData>;

#[derive(Debug)]
#[cfg(feature="rmp-serde")]
pub struct Receiver {
    mx: RecvMutex,
    cv: Condvar,
}

#[cfg(feature="rmp-serde")]
impl Receiver {
    fn new() -> Arc<Self> {
        Arc::new(Receiver {
            mx: Mutex::new((
                    RecvState::Open,
                    String::from(""),
                    vec![],
                    false,
                    VecDeque::new())),
            cv: Condvar::new(),
        })
    }
}

#[cfg(feature="rmp-serde")]
fn mx_recv_error(mx: &mut RecvData, s: &str) {
    mx.0 = RecvState::Return;
    mx.2 =
        VVal::err_msg(&format!("return value serialization error ({}): {}", mx.1, s))
        .s().as_bytes().to_vec();
    mx.3 = true;
}

#[cfg(feature="rmp-serde")]
fn mx_return(mx: &mut RecvData, v: &VVal) {
    mx.0 = RecvState::Return;
    match v.to_msgpack() {
        Ok(s) => {
            mx.2 = s;
            mx.3 = false;
        },
        Err(s) => mx_recv_error(mx, &s),
    }
}

/// This a messaging handle for providing receiver and sender handles
/// for the inter thread communication of WLambda instances.
///
/// The communication is internally done either by RPC or by async
/// message passing. The VVal or WLambda values are serialized as msgpack
/// internally for transmission to the
/// other thread. This is not a high speed interface, as serialization
/// and allocations are done.
///
/// The communication between WLambda thread instances can be done by
/// multiple senders and multiple receivers, but bear in mind, that in
/// case of RPC one RPC call will lock out all other sender and
/// receiver thread, as RPC calls are synchronous.
///
/// Pass this MsgHandle to the receiver thread or multiple threads.
/// You can clone this handle if you want to use it in multiple threads.
///
/// **See also** [threads module](index.html)
#[cfg(feature="rmp-serde")]
#[derive(Clone)]
pub struct MsgHandle {
    receiver: Arc<Receiver>,
}

#[cfg(feature="rmp-serde")]
impl MsgHandle {
    pub fn new() -> Self {
        MsgHandle {
            receiver: Receiver::new(),
        }
    }

    /// Returns a Sender handle.
    pub fn sender(&self) -> Sender {
        Sender::new(self.receiver.clone())
    }

    /// Starts executing Sender requests infinitely long.
    /// A global `thread:quit` function is defined, which can be used
    /// to stop this infinitely long loop.
    pub fn run(&mut self, ctx: &mut EvalContext) {
        let quit = std::rc::Rc::new(std::cell::RefCell::new(false));

        let qr = quit.clone();
        ctx.set_global_var(
            "thread:quit",
            &VValFun::new_fun(move |_env: &mut Env, _argc: usize| {
                *qr.borrow_mut() = true;
                Ok(VVal::Nul)
            }, Some(0), Some(0), false));

        loop {
            self.step(ctx, &std::time::Duration::from_secs(1));
            if *quit.borrow() { break; }
        }
    }

    /// Tries to execute a RPC call or received message if the request is
    /// received within _timeout_ duration. Returns `None` if something
    /// went wrong. Otherwise `Some(())` is returned.
    pub fn step(&mut self, ctx: &mut EvalContext, timeout: &std::time::Duration) -> Option<()> {
        let r = &*self.receiver;

        let mut mx = r.mx.lock().unwrap();
        loop {
            let state = mx.0;
            match state {
                RecvState::Call => {
                    if let Some(v) = ctx.get_global_var(&mx.1) {
                        match VVal::from_msgpack(&mx.2) {
                            Ok(args) => {
                                let arg =
                                    if args.is_none() { vec![] }
                                    else { args.to_vec() };
                                match ctx.call(&v, &arg) {
                                    Ok(vret) => {
                                        mx_return(&mut *mx, &vret);
                                    },
                                    Err(sa) => {
                                        mx_recv_error(&mut *mx,
                                            &format!("uncaught stack action calling: {}", sa));
                                    }
                                }
                            },
                            Err(s) => {
                                mx_recv_error(&mut *mx,
                                    &format!("deserialization error: {}", s));
                            }
                        }

                    } else {
                        let gvar = mx.1.clone();
                        mx_recv_error(&mut *mx,
                            &format!("no such global variable: {}", gvar));
                    }

                    r.cv.notify_all();
                    break;
                },
                RecvState::Msg => {
                    std::mem::drop(mx);
                    loop {
                        let mut mx = r.mx.lock().unwrap();
                        if mx.4.is_empty() {
                            mx.0 = RecvState::Open;
                            break;
                        }
                        let (name, ser_val) = mx.4.pop_front().unwrap();
                        std::mem::drop(mx);

                        if let Some(v) = ctx.get_global_var(&name) {
                            if let Ok(args) = VVal::from_msgpack(&ser_val) {
                                let arg =
                                    if args.is_none() { vec![] }
                                    else { args.to_vec() };
                                ctx.call(&v, &arg).unwrap_or(VVal::Nul);
                            }
                        }
                    }
                    r.cv.notify_all();
                    break;
                },
                _ => {
                    // FIXME: As soon as Rust is not nightly anymore,
                    //        use wait_timeout_until for more precise timeout.
                    let res = r.cv.wait_timeout(mx, *timeout).unwrap();
                    mx = res.0;
                    if res.1.timed_out() {
                        break;
                    }
                }
            }
        }

        Some(())
    }
}

#[cfg(feature="rmp-serde")]
impl Default for MsgHandle {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[cfg(feature="rmp-serde")]
mod tests {
    #[cfg(feature="rmp-serde")]
    #[test]
    fn check_rpc() {
        use crate::vval::*;

        // Get some random user thread:
        let mut ctx = crate::compiler::EvalContext::new_default();

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let quit = std::rc::Rc::new(std::cell::RefCell::new(false));

            let global_t = crate::compiler::GlobalEnv::new_default();

            let qr = quit.clone();
            global_t.borrow_mut().add_func("thread:quit", move |_env: &mut Env, _argc: usize| {
                *qr.borrow_mut() = true;
                Ok(VVal::Nul)
            }, Some(0), Some(0));

            let mut ctx = crate::compiler::EvalContext::new(global_t);

            ctx.eval("!:global X = 123").unwrap();

            loop {
                msg_handle.step(&mut ctx, &std::time::Duration::from_secs(1));
                if *quit.borrow() { break; }
            }
        });

        ctx.eval("worker_call :displayln \"hello world from worker thread!\";").unwrap();
        ctx.eval("std:assert_eq (worker_call :std:eval \"X\") 123;").unwrap();

        sender.call("thread:quit", VVal::Nul);

        t.join().unwrap();
    }

    #[cfg(feature="rmp-serde")]
    #[test]
    fn check_rpc_quit() {
        use crate::vval::*;

        // Get some random user thread:
        let mut ctx = crate::compiler::EvalContext::new_default();

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let mut ctx = crate::compiler::EvalContext::new_default();

            ctx.eval("!:global X = 123").unwrap();
            msg_handle.run(&mut ctx);
        });

        ctx.eval("std:assert_eq (worker_call :std:eval \"X\") 123;").unwrap();

        sender.call("thread:quit", VVal::Nul);

        t.join().unwrap();
    }

    #[cfg(feature="rmp-serde")]
    #[test]
    fn check_rpc_msgs() {
        use crate::vval::*;

        let r = std::sync::Arc::new(std::sync::Mutex::new(String::from("")));
        let ri = r.clone();

        // Get some random user thread:
        let mut ctx = crate::compiler::EvalContext::new_default();

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let mut ctx = crate::compiler::EvalContext::new_default();

            ctx.eval(r#"
                !:global X = $[1,2,3,4];
                !:global Y = { std:pop X };
                !:global G = { std:push X (str $[_, _1]); };
                !:global H = { std:push X (_ + _1); };
            "#).unwrap();
            msg_handle.run(&mut ctx);

            {
                let mut i = ri.lock().unwrap();
                std::mem::replace(
                    &mut *i,
                    ctx.eval("$[std:pop X, std:pop X]").unwrap().s());
            }
        });

        sender.send("Y",           VVal::Nul);
        sender.send("Y",           VVal::Nul);
        sender.send("Y",           VVal::Nul);
        ctx.eval("worker_call :G 45 44").unwrap();
        ctx.eval("worker_send :H 11 13").unwrap();
        sender.send("thread:quit", VVal::Nul);

        std::thread::sleep(std::time::Duration::from_secs(2));

        let i = r.lock().unwrap();
        assert_eq!(*i, "$[24,\"$[45,44]\"]", "popping works");

        t.join().unwrap();
    }

    #[cfg(feature="rmp-serde")]
    #[test]
    fn check_rpc_msgs_from_eval() {
        let r = std::sync::Arc::new(std::sync::Mutex::new(0));
        let ri = r.clone();

        // Get some random user thread:
        let mut ctx = crate::compiler::EvalContext::new_default();

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let mut ctx = crate::compiler::EvalContext::new_default();

            ctx.eval(r#"
                !:global X = $[13,2,3,4];
                !:global Y = { std:pop X };
            "#).unwrap();
            msg_handle.run(&mut ctx);

            {
                let mut i = ri.lock().unwrap();
                std::mem::replace(
                    &mut *i,
                    ctx.eval("std:pop X").unwrap().i());
            }
        });


        ctx.eval(r#"
            worker_send :Y;
            worker_send :Y;
            worker_send :Y;
            worker_send :thread:quit;
        "#).unwrap();

        std::thread::sleep(std::time::Duration::from_secs(2));

        let i = r.lock().unwrap();
        assert_eq!(*i, 13, "popping works");

        t.join().unwrap();
    }


    #[cfg(feature="rmp-serde")]
    #[test]
    fn check_rpc_msgs_bytes() {
        let r = std::sync::Arc::new(std::sync::Mutex::new(String::from("")));
        let ri = r.clone();

        // Get some random user thread:
        let mut ctx = crate::compiler::EvalContext::new_default();

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let mut ctx = crate::compiler::EvalContext::new_default();

            ctx.eval(r#"
                !:global X = $[13,2,3,4];
                !:global Y = { .X = _; };
            "#).unwrap();
            msg_handle.run(&mut ctx);

            {
                let mut i = ri.lock().unwrap();
                std::mem::replace(
                    &mut *i, ctx.eval("X").unwrap().s());
            }
        });

        ctx.eval(r#"
            worker_send :Y $b"ABC";
            worker_send :thread:quit;
        "#).unwrap();

        std::thread::sleep(std::time::Duration::from_secs(2));

        let i = r.lock().unwrap();
        assert_eq!(*i, "$b\"ABC\"", "transmitting bytes works");

        t.join().unwrap();
    }
}
