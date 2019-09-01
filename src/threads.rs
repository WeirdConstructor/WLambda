/*!
Threading API:

```
use wlambda::vval::*;

// Get some random user thread:
let global  = wlambda::prelude::create_wlamba_prelude();
let mut ctx = wlambda::compiler::EvalContext::new(global);

let mut msg_handle = wlambda::threads::MsgHandle::new();
// You may register on multiple threads, but only one thread can use it at a time.
let sender = msg_handle.sender();
sender.register_on_as(&mut ctx, "worker");

let t = std::thread::spawn(move || {
    let quit = std::rc::Rc::new(std::cell::RefCell::new(false));

    let global_t = wlambda::prelude::create_wlamba_prelude();

    let qr = quit.clone();
    global_t.borrow_mut()
        .add_func("thread:quit", move |env: &mut Env, _argc: usize| {
            *qr.borrow_mut() = true;
            Ok(VVal::Nul)
        }, Some(0), Some(0));

    let mut ctx = wlambda::compiler::EvalContext::new(global_t);

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
ctx.eval("worker :displayln \"hello world from worker thread!\";").unwrap();

ctx.eval("wl:assert_eq [worker :wl:eval \"X\"] 123;").unwrap();

sender.call("thread:quit", VVal::Nul);

t.join();

```

The alternative async messaging API, that does not provide any return values
from the Thread. However, you could theoretically generate two message handles
for a two way communication.

```
use wlambda::vval::*;

// Get some random user thread:
let global  = wlambda::prelude::create_wlamba_prelude();
let mut ctx = wlambda::compiler::EvalContext::new(global);

let mut msg_handle = wlambda::threads::MsgHandle::new();

let sender = msg_handle.sender();

// You may register on multiple threads, but only one thread can use it at a time.
sender.register_on_as(&mut ctx, "worker");

let t = std::thread::spawn(move || {
    let global_t = wlambda::prelude::create_wlamba_prelude();
    let mut ctx  = wlambda::compiler::EvalContext::new(global_t);

    // This also implicitly defines a thread:quit:
    msg_handle.run(&mut ctx);
});

sender.call("thread:quit", VVal::Nul);

t.join();

```
*/

use crate::compiler::EvalContext;
use crate::vval::*;
use std::sync::{Arc, Mutex, Condvar};

/// The Sender sends RPC calls to the Receiver thread.
/// Any values passed by WLambda code are serialized into JSON (or some
/// equivalent format) internally and transmitted to the thread
/// in String form.
/// This means, your values must not be cyclic or contain non serializable
/// data like external handles.
///
/// The Sender also provides a method for storing a sending function
/// in the global variables for the EvalContext.
///
///```
/// let global  = wlambda::prelude::create_wlamba_prelude();
/// let mut ctx = wlambda::compiler::EvalContext::new(global);
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
#[cfg(feature="serde_json")]
pub struct Sender {
    receiver: Arc<Receiver>,
}

#[cfg(feature="serde_json")]
impl Sender {
    fn new(receiver: Arc<Receiver>) -> Self {
        Sender { receiver }
    }

    /// Registers a call and message sending function on the supplied EvalContext.
    /// You can call this function with the global variable name
    /// of the thread function you want to call and additional
    /// arguments to that function.
    ///
    /// ```
    /// let global  = wlambda::prelude::create_wlamba_prelude();
    /// let mut ctx = wlambda::compiler::EvalContext::new(global);
    ///
    /// let mut msg_handle = wlambda::threads::MsgHandle::new();
    /// let sender = msg_handle.sender();
    ///
    /// sender.register_on_as(&mut ctx, "worker");
    ///
    /// ctx.eval("worker_call :displayln \"hello world from worker thread!\";").unwrap();
    /// ```
    pub fn register_on_as(&self, ctx: &mut EvalContext, variable_name: &str) {
        let sender = self.clone();
        ctx.set_global_var(&format("{}_call", variable_name),
            &VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    let args =
                        if argc == 1 { VVal::Nul }
                        else {
                            let a = VVal::vec();
                            for _ in 1..argc {
                                a.push(env.arg(1).clone());
                            }
                            a
                        };
                    Ok(sender.call(&env.arg(0).s_raw(), args))
                }, Some(1), None));
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
            mx.2 = args.to_json(true).unwrap().into_bytes();
            mx.3 = false;

            r.cv.notify_one();
        }

        while mx.0 != RecvState::Return {
            mx = r.cv.wait(mx).unwrap();
        }

        let ret =
            if mx.3 {
                VVal::err_msg(&String::from_utf8(mx.2.clone()).unwrap())
            } else {
                VVal::from_json(&String::from_utf8(mx.2.clone()).unwrap()).unwrap()
            };

        mx.0 = RecvState::Open;
        mx.1 = String::from("");
        mx.2 = String::from("").into_bytes();
        mx.3 = false;

        r.cv.notify_one();

        ret
    }

    pub fn send(_var_name: &str, _args: VVal) {
    }
}

#[cfg(feature="serde_json")]
#[derive(Debug, Copy, Clone, PartialEq)]
enum RecvState {
    Open,
    Call,
    Return,
}

#[derive(Debug)]
#[cfg(feature="serde_json")]
pub struct Receiver {
    mx: Mutex<(RecvState, String, Vec<u8>, bool)>,
    cv: Condvar,
}

#[cfg(feature="serde_json")]
impl Receiver {
    fn new() -> Arc<Self> {
        Arc::new(Receiver {
            mx: Mutex::new((RecvState::Open, String::from(""), vec![], false)),
            cv: Condvar::new(),
        })
    }
}

#[cfg(feature="serde_json")]
fn mx_recv_error(mx: &mut (RecvState, String, Vec<u8>, bool), s: &str) {
    mx.0 = RecvState::Return;
    mx.2 =
        VVal::err_msg(&format!("return value serialization error: {}", s))
        .to_json(true)
        .unwrap()
        .into_bytes();
    mx.3 = true;
}

#[cfg(feature="serde_json")]
fn mx_return(mx: &mut (RecvState, String, Vec<u8>, bool), v: &VVal) {
    mx.0 = RecvState::Return;
    match v.to_json(true) {
        Ok(s) => {
            mx.2 = s.into_bytes();
            mx.3 = false;
        },
        Err(s) => mx_recv_error(mx, &s),
    }
}

#[cfg(feature="serde_json")]
#[derive(Clone)]
pub struct MsgHandle {
    receiver: Arc<Receiver>,
}

/// This a messaging handle for providing receiver and sender handles
/// for the inter thread communication of WLambda instances.
///
/// The communication is internally done either by RPC or by async
/// message passing. The VVal or WLambda values are serialized as JSON
/// (or some equivalent format) internally for transmission to the
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
#[cfg(feature="serde_json")]
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
            }, Some(0), Some(0)));

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
            if let RecvState::Call = mx.0 {
                if let Some(v) = ctx.get_global_var(&mx.1) {
                    match VVal::from_json(&String::from_utf8(mx.2.clone()).unwrap()) {
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
                                        &format!("uncaught stack action: {:?}", sa));
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

                r.cv.notify_one();
                break;
            } else {
                // FIXME: As soon as Rust is not nightly anymore,
                //        use wait_timeout_until for more precise timeout.
                let res = r.cv.wait_timeout(mx, *timeout).unwrap();
                mx = res.0;
                if res.1.timed_out() {
                    break;
                }
            }
        }

        Some(())
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature="serde_json")]
    #[test]
    fn check_rpc() {
        use crate::vval::*;

        // Get some random user thread:
        let global  = crate::prelude::create_wlamba_prelude();
        let mut ctx = crate::compiler::EvalContext::new(global);

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let quit = std::rc::Rc::new(std::cell::RefCell::new(false));

            let global_t = crate::prelude::create_wlamba_prelude();

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

        ctx.eval("worker :displayln \"hello world from worker thread!\";").unwrap();
        ctx.eval("wl:assert_eq [worker :wl:eval \"X\"] 123;").unwrap();

        sender.call("thread:quit", VVal::Nul);

        t.join().unwrap();
    }

    #[cfg(feature="serde_json")]
    #[test]
    fn check_rpc_quit() {
        use crate::vval::*;

        // Get some random user thread:
        let global  = crate::prelude::create_wlamba_prelude();
        let mut ctx = crate::compiler::EvalContext::new(global);

        let mut msg_handle = crate::threads::MsgHandle::new();
        let sender = msg_handle.sender();
        sender.register_on_as(&mut ctx, "worker");

        let t = std::thread::spawn(move || {
            let global_t = crate::prelude::create_wlamba_prelude();
            let mut ctx = crate::compiler::EvalContext::new(global_t);

            ctx.eval("!:global X = 123").unwrap();
            msg_handle.run(&mut ctx);
        });

        ctx.eval("wl:assert_eq [worker :wl:eval \"X\"] 123;").unwrap();

        sender.call("thread:quit", VVal::Nul);

        t.join().unwrap();
    }
}
