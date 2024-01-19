// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!

This module provides threading functionality for WLambda.
It does not depend on anything else than the Rust standard library.

If you want to implement or specialize thread creation please
refer to the documentation of the `ThreadCreator` trait.

*/

use crate::compiler::*;
use crate::nvec::NVec;
use crate::str_int::*;
use crate::vval::*;

use std::cell::RefCell;
use std::fmt::Formatter;
use std::rc::Rc;
use std::sync::mpsc::{Receiver, RecvTimeoutError, Sender, TryRecvError};
use std::sync::{Arc, Condvar, Mutex, RwLock};

use fnv::FnvHashMap;

pub trait ThreadSafeUsr: Send + Sync {
    fn to_vval(&self) -> VVal;
}

impl std::fmt::Debug for dyn ThreadSafeUsr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<<ThreadSafeUsr>>")
    }
}

/// AVal is a copy-by-value structure for storing the most
/// important data of VVals inside an atomic container (AtomicAVal).
///
/// You can create an AVal from a VVal like this:
/// ```
/// use wlambda::*;
///
/// let av = {
///     let v = VVal::vec();
///     v.push(VVal::Int(1));
///     v.push(VVal::Int(2));
///     v.push(VVal::Int(3));
///
///     AVal::from_vval(&v)
/// };
///
/// /// You get back the VVal like this:
///
/// assert_eq!(av.to_vval().s(), "$[1,2,3]");
/// ```
///
/// And get back the VVal like this:
#[derive(Debug)]
pub enum AVal {
    None,
    Err(Box<AVal>, String),
    Bol(bool),
    Sym(String),
    Str(String),
    Byt(Vec<u8>),
    Int(i64),
    Flt(f64),
    Lst(Vec<AVal>),
    Opt(Option<Box<AVal>>),
    FVec(NVec<f64>),
    IVec(NVec<i64>),
    Pair(Box<(AVal, AVal)>),
    Map(FnvHashMap<String, AVal>),
    Chan(AValChannel),
    Slot(AtomicAValSlot),
    Atom(AtomicAVal),
    Usr(Box<dyn ThreadSafeUsr>),
}

impl AVal {
    /// Takes a path of indices and the start index of that path,
    /// and sets the addressed slot to the given AVal.
    /// This is used by `std:sync:atom:write_at`.
    #[allow(dead_code)]
    pub fn set_at_path(&mut self, path_idx: usize, pth: &VVal, av: AVal) {
        match pth.at(path_idx).unwrap_or(VVal::None) {
            VVal::Int(i) => {
                if let AVal::Lst(ref mut v) = self {
                    if (i as usize) < v.len() {
                        v[i as usize] = av;
                    } else {
                        v.insert(i as usize, av);
                    }
                } else if let AVal::Pair(ref mut v) = self {
                    let i = (i % 2).abs();
                    if i == 0 {
                        v.0 = av;
                    } else {
                        v.1 = av;
                    }
                }
            }
            v => {
                let key = v.s_raw();
                if let AVal::Map(ref mut m) = self {
                    m.insert(key, av);
                }
            }
        }
    }

    /// Converts the AVal back to a VVal.
    ///
    /// ```
    /// use wlambda::*;
    /// assert_eq!(AVal::Sym(String::from("x")).to_vval().s(), ":x");
    /// ```
    pub fn to_vval(&self) -> VVal {
        match self {
            AVal::None => VVal::None,
            AVal::Err(av, pos) => {
                let v = VVal::vec();
                v.push(av.to_vval());
                v.push(VVal::new_str(pos));
                VVal::Err(Rc::new(RefCell::new((v, SynPos::empty()))))
            }
            AVal::Bol(b) => VVal::Bol(*b),
            AVal::Int(i) => VVal::Int(*i),
            AVal::Flt(f) => VVal::Flt(*f),
            AVal::Sym(s) => VVal::new_sym(s),
            AVal::Str(s) => VVal::new_str(s),
            AVal::Byt(b) => VVal::new_byt(b.clone()),
            AVal::Atom(a) => VVal::Usr(Box::new(a.clone())),
            AVal::Chan(a) => VVal::Usr(Box::new(a.clone())),
            AVal::Slot(a) => VVal::Usr(Box::new(a.clone())),
            AVal::Opt(None) => VVal::opt_none(),
            AVal::Opt(Some(a)) => VVal::opt(a.to_vval()),
            AVal::Pair(p) => VVal::pair(p.0.to_vval(), p.1.to_vval()),
            AVal::FVec(v) => VVal::FVec(Box::new(*v)),
            AVal::IVec(v) => VVal::IVec(Box::new(*v)),
            AVal::Usr(u) => u.to_vval(),
            AVal::Lst(l) => {
                let v = VVal::vec();
                for av in l.iter() {
                    v.push(av.to_vval());
                }
                v
            }
            AVal::Map(m) => {
                let mv = VVal::map();
                for (k, v) in m.iter() {
                    let k = s2sym(k);
                    mv.set_key_sym(k, v.to_vval()).expect("AVal->VVal map not used more than once");
                }
                mv
            }
        }
    }

    /// Converts a VVal to an AVal.
    ///
    /// ```
    /// use wlambda::*;
    ///
    /// let av = AVal::from_vval(&VVal::new_sym("x"));
    /// if let AVal::Sym(s) = av {
    ///     assert_eq!(s, "x");
    /// } else {
    ///     assert!(false);
    /// }
    /// ```
    pub fn from_vval(v: &VVal) -> Self {
        match v {
            VVal::None => AVal::None,
            VVal::Err(e) => {
                let eb = e.borrow();
                AVal::Err(Box::new(AVal::from_vval(&eb.0)), format!("{}", eb.1))
            }
            VVal::Bol(b) => AVal::Bol(*b),
            VVal::Sym(s) => AVal::Sym(String::from(s.as_ref())),
            VVal::Str(s) => AVal::Str(s.as_ref().clone()),
            VVal::Byt(b) => AVal::Byt(b.as_ref().clone()),
            VVal::Int(i) => AVal::Int(*i),
            VVal::Flt(f) => AVal::Flt(*f),
            VVal::Opt(None) => AVal::Opt(None),
            VVal::Opt(Some(v)) => AVal::Opt(Some(Box::new(AVal::from_vval(v)))),
            VVal::FVec(v) => AVal::FVec(**v),
            VVal::IVec(v) => AVal::IVec(**v),
            VVal::Pair(p) => AVal::Pair(Box::new((AVal::from_vval(&p.0), AVal::from_vval(&p.1)))),
            VVal::Lst(l) => {
                let mut avec = vec![];
                for vv in l.borrow().iter() {
                    avec.push(AVal::from_vval(vv));
                }
                AVal::Lst(avec)
            }
            VVal::Map(m) => {
                let mut amap = FnvHashMap::with_capacity_and_hasher(2, Default::default());
                for (k, v) in m.borrow().iter() {
                    amap.insert(String::from(k.as_ref()), AVal::from_vval(v));
                }
                AVal::Map(amap)
            }
            VVal::Usr(u) => {
                let mut cl_ud = u.clone_ud();
                if let Some(ud) = cl_ud.as_any().downcast_mut::<AtomicAVal>() {
                    AVal::Atom(ud.clone())
                } else if let Some(ud) = cl_ud.as_any().downcast_mut::<AValChannel>() {
                    AVal::Chan(ud.clone())
                } else if let Some(ud) = cl_ud.as_any().downcast_mut::<AtomicAValSlot>() {
                    AVal::Slot(ud.clone())
                } else if let Some(ud) = cl_ud.as_thread_safe_usr() {
                    AVal::Usr(ud)
                } else {
                    AVal::None
                }
            }
            _ => AVal::None,
        }
    }
}

/// WLambda:
///
/// ```text
/// !atom = std:sync:atom:new 10;
/// !channel = std:sync:mpsc:new[];
/// !thrd = std:spawn_thread $q{
///     !val = main.read;
///     main.write $[1,$[0,1],3];
///
///     main.read_at 0;
///     main.write_at $[1, 0] 320;
///     q.send $["done", 10];
///
/// } ${ main = atom, q = channel };
///
/// !item = queue.pop[];
/// .item = queue.pop_timeout 1000;
///
///
/// ```

/// Wraps an AVal like this: Arc<RwLock<AVal>>.
/// An AtomicAVal is a thread safe container for VVal
/// data structures. It's used by WLambda functions like
/// `std:sync:atom:new`, `std:sync:atom:read` or `std:sync:atom:write`.
///
/// These containers are shared between the threads by passing them
/// to the threads at `std:thread:spawn`.
#[derive(Clone, Debug)]
pub struct AtomicAVal(Arc<RwLock<AVal>>);

impl Default for AtomicAVal {
    fn default() -> Self {
        AtomicAVal::new()
    }
}

#[derive(Debug)]
pub struct AtomicValSlot {
    val: Mutex<(AVal, bool)>,
    cv_send: Condvar,
    cv_recv: Condvar,
}

#[derive(Clone, Debug)]
pub struct AtomicAValSlot(Arc<AtomicValSlot>);

impl Default for AtomicAValSlot {
    fn default() -> Self {
        Self::new()
    }
}

impl AtomicAValSlot {
    pub fn new() -> Self {
        Self(Arc::new(AtomicValSlot {
            val: Mutex::new((AVal::None, false)),
            cv_recv: Condvar::new(),
            cv_send: Condvar::new(),
        }))
    }

    pub fn wait_empty(&self, dur: Option<std::time::Duration>) -> VVal {
        let mut guard = match self.0.val.lock() {
            Ok(guard) => guard,
            Err(e) => return VVal::err_msg(&format!("Failed to receive, can't get lock: {}", e)),
        };

        if !guard.1 {
            return VVal::Bol(true);
        }

        if let Some(dur) = dur {
            loop {
                match self.0.cv_recv.wait_timeout(guard, dur) {
                    Ok(wait_res) => {
                        guard = wait_res.0;

                        if wait_res.1.timed_out() {
                            return VVal::Bol(false);
                        } else if !guard.1 {
                            return VVal::Bol(true);
                        }
                    }
                    Err(e) => {
                        return VVal::err_msg(&format!("Failed to receive, poison error: {}", e));
                    }
                }
            }
        } else {
            loop {
                match self.0.cv_recv.wait(guard) {
                    Ok(next_guard) => {
                        guard = next_guard;

                        if !guard.1 {
                            return VVal::Bol(true);
                        }
                    }
                    Err(e) => {
                        return VVal::err_msg(&format!("Failed to receive, poison error: {}", e));
                    }
                }
            }
        }
    }

    pub fn check_empty(&self) -> VVal {
        match self.0.val.lock() {
            Ok(guard) => VVal::Bol(!guard.1),
            Err(e) => VVal::err_msg(&format!("Failed to check empty, can't get lock: {}", e)),
        }
    }

    pub fn send(&self, msg: &VVal) -> VVal {
        match self.0.val.lock() {
            Ok(mut guard) => {
                *guard = (AVal::from_vval(msg), true);
                self.0.cv_send.notify_one();
                VVal::Bol(true)
            }
            Err(e) => VVal::err_msg(&format!("Failed to send, can't get lock: {}", e)),
        }
    }

    pub fn recv_timeout(&self, dur: Option<std::time::Duration>) -> VVal {
        let mut guard = match self.0.val.lock() {
            Ok(guard) => guard,
            Err(e) => return VVal::err_msg(&format!("Failed to receive, can't get lock: {}", e)),
        };

        if let Some(dur) = dur {
            if guard.1 {
                let val = guard.0.to_vval();
                *guard = (AVal::None, false);
                self.0.cv_recv.notify_one();
                return VVal::opt(val);
            }

            loop {
                match self.0.cv_send.wait_timeout(guard, dur) {
                    Ok(wait_res) => {
                        guard = wait_res.0;

                        if wait_res.1.timed_out() {
                            return VVal::opt_none();
                        } else if guard.1 {
                            let val = guard.0.to_vval();
                            *guard = (AVal::None, false);
                            self.0.cv_recv.notify_one();
                            return VVal::opt(val);
                        }
                    }
                    Err(e) => {
                        return VVal::err_msg(&format!("Failed to receive, poison error: {}", e));
                    }
                }
            }
        } else {
            if guard.1 {
                let val = guard.0.to_vval();
                *guard = (AVal::None, false);
                self.0.cv_recv.notify_one();
                return val;
            }

            loop {
                match self.0.cv_send.wait(guard) {
                    Ok(next_guard) => {
                        guard = next_guard;

                        if guard.1 {
                            let val = guard.0.to_vval();
                            *guard = (AVal::None, false);
                            self.0.cv_recv.notify_one();
                            return val;
                        }
                    }
                    Err(e) => {
                        return VVal::err_msg(&format!("Failed to receive, poison error: {}", e));
                    }
                }
            }
        }
    }

    pub fn try_recv(&self) -> VVal {
        match self.0.val.lock() {
            Ok(mut guard) => {
                if guard.1 {
                    let val = guard.0.to_vval();
                    *guard = (AVal::None, false);
                    self.0.cv_recv.notify_one();
                    VVal::opt(val)
                } else {
                    VVal::opt_none()
                }
            }
            Err(e) => VVal::err_msg(&format!("Failed to receive, can't get lock: {}", e)),
        }
    }
}

impl VValUserData for AtomicAValSlot {
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv_ref();
        match key {
            "recv" => Ok(self.recv_timeout(None)),
            "try_recv" => Ok(self.try_recv()),
            "recv_timeout" => {
                if argv.len() != 1 {
                    return Err(StackAction::panic_str(
                        "recv_timeout method expects 1 argument".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                Ok(self.recv_timeout(Some(argv[0].to_duration()?)))
            }
            "send" => {
                if argv.len() != 1 {
                    return Err(StackAction::panic_str(
                        "send method expects 1 argument".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                Ok(self.send(&argv[0]))
            }
            "check_empty" => Ok(self.check_empty()),
            "wait_empty" => Ok(self.wait_empty(None)),
            "wait_empty_timeout" => {
                if argv.len() != 1 {
                    return Err(StackAction::panic_str(
                        "wait_empty_timeout method expects 1 argument".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                Ok(self.wait_empty(Some(argv[0].to_duration()?)))
            }
            _ => Err(StackAction::panic_str(
                format!("unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

#[derive(Clone)]
pub struct AValChannel {
    sender: Arc<Mutex<Sender<AVal>>>,
    receiver: Arc<Mutex<Receiver<AVal>>>,
}

#[derive(Clone, Debug)]
pub struct ForkSenderError(String);

impl std::fmt::Display for ForkSenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Failed to fork sender, can't get lock: {}", self.0)
    }
}

impl AValChannel {
    pub fn new_direct() -> AValChannel {
        let (send, recv) = std::sync::mpsc::channel();
        Self { sender: Arc::new(Mutex::new(send)), receiver: Arc::new(Mutex::new(recv)) }
    }

    pub fn new_vval() -> VVal {
        VVal::Usr(Box::new(Self::new_direct()))
    }

    /// Fork a sender without having it wrapped into a [VVal::Usr].
    ///
    /// Here is an example on how to get a sender from an std:sync:mpsc handle
    /// from the WLambda side of an API function:
    ///```text
    ///   let chan = if env.arg(2).is_some() {
    ///       let mut chan = env.arg(2);
    ///       let chan =
    ///           chan.with_usr_ref(|chan: &mut AValChannel| {
    ///               chan.fork_sender_direct()
    ///           });
    ///
    ///       if let Ok(chan) = chan {
    ///          match chan {
    ///               Ok(chan) => Some(chan),
    ///               Err(err) => {
    ///                   return
    ///                       Ok(VVal::err_msg(
    ///                           &format!("Failed to fork sender, \
    ///                                     can't get lock: {}", err)));
    ///               }
    ///          }
    ///       } else {
    ///           return
    ///               Ok(env.new_err(format!(
    ///                   "bta:spawn_port_for_address: \
    ///                    channel not a std:sync:mpsc handle! {}",
    ///                   env.arg(2).s())));
    ///       }
    ///   } else {
    ///       None
    ///   };
    ///
    ///   // further in the code:
    ///
    ///   if Some(chan) = chan {
    ///       chan.send(
    ///          &VVal::pair(VVal::new_sym("$WL/connected"), VVal::None));
    ///   }
    ///```
    pub fn fork_sender_direct(&self) -> Result<Self, ForkSenderError> {
        match self.sender.lock() {
            Ok(guard) => {
                let receiver = self.receiver.clone();
                let new_sender: Sender<AVal> = guard.clone();
                Ok(Self { sender: Arc::new(Mutex::new(new_sender)), receiver })
            }
            Err(e) => Err(ForkSenderError(format!("{}", e))),
        }
    }

    /// Fork a sender and wrap it into an [VVal].
    /// If an error occurs, an error [VVal] will be returned.
    pub fn fork_sender(&self) -> VVal {
        match self.fork_sender_direct() {
            Ok(channel) => VVal::Usr(Box::new(channel)),
            Err(ForkSenderError(msg)) => {
                VVal::err_msg(&format!("Failed to fork sender, can't get lock: {}", msg))
            }
        }
    }

    pub fn send(&self, msg: &VVal) -> VVal {
        match self.sender.lock() {
            Ok(guard) => {
                let msg = AVal::from_vval(msg);
                if let Err(e) = guard.send(msg) {
                    VVal::err_msg(&format!("Failed to send: {}", e))
                } else {
                    VVal::Bol(true)
                }
            }
            Err(e) => VVal::err_msg(&format!("Failed to send, can't get lock: {}", e)),
        }
    }

    pub fn recv_timeout(&self, dur: Option<std::time::Duration>) -> VVal {
        match self.receiver.lock() {
            Ok(guard) => match dur {
                None => match guard.recv() {
                    Ok(av) => av.to_vval(),
                    Err(_) => VVal::err_msg("recv disconnected"),
                },
                Some(dur) => match guard.recv_timeout(dur) {
                    Ok(av) => VVal::Opt(Some(Rc::new(av.to_vval()))),
                    Err(RecvTimeoutError::Timeout) => VVal::Opt(None),
                    Err(RecvTimeoutError::Disconnected) => {
                        VVal::err_msg("recv_timeout disconnected")
                    }
                },
            },
            Err(e) => VVal::err_msg(&format!("Failed to receive, can't get lock: {}", e)),
        }
    }

    pub fn try_recv(&self) -> VVal {
        match self.receiver.lock() {
            Ok(guard) => match guard.try_recv() {
                Ok(av) => VVal::Opt(Some(Rc::new(av.to_vval()))),
                Err(TryRecvError::Empty) => VVal::Opt(None),
                Err(e) => VVal::err_msg(&format!("try_recv error: {}", e)),
            },
            Err(e) => VVal::err_msg(&format!("Failed to receive, can't get lock: {}", e)),
        }
    }
}

impl std::fmt::Debug for AValChannel {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<<AValChannel>>")
    }
}

impl VValUserData for AValChannel {
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv_ref();
        match key {
            "recv" => Ok(self.recv_timeout(None)),
            "try_recv" => Ok(self.try_recv()),
            "recv_timeout" => {
                if argv.len() != 1 {
                    return Err(StackAction::panic_str(
                        "recv_timeout method expects 1 argument".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                Ok(self.recv_timeout(Some(argv[0].to_duration()?)))
            }
            "send" => {
                if argv.len() != 1 {
                    return Err(StackAction::panic_str(
                        "send method expects 1 argument".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                Ok(self.send(&argv[0]))
            }
            _ => Err(StackAction::panic_str(
                format!("unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

impl AtomicAVal {
    /// Creates a new empty instance, containing AVal::None.
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(AVal::None)))
    }

    /// Locks and stores the VVal.
    pub fn write(&self, vv: &VVal) -> VVal {
        let new_av = AVal::from_vval(vv);
        if let Ok(mut guard) = self.0.write() {
            *guard = new_av;
            VVal::Bol(true)
        } else {
            VVal::err_msg("Lock Poisoned")
        }
    }

    /// Locks and stores the VVal.
    pub fn swap(&self, vv: &VVal) -> VVal {
        let new_av = AVal::from_vval(vv);
        if let Ok(mut guard) = self.0.write() {
            let ret = guard.to_vval();
            *guard = new_av;
            ret
        } else {
            VVal::err_msg("Lock Poisoned")
        }
    }

    /// Locks and reads the AVal and converts it to a VVal.
    pub fn read(&self) -> VVal {
        if let Ok(guard) = self.0.read() {
            guard.to_vval()
        } else {
            VVal::err_msg("Lock Poisoned")
        }
    }

    /// Locks and stores the VVal at the given key path.
    pub fn store_at(&self, _keypath: &VVal, vv: &VVal) {
        let new_av = AVal::from_vval(vv);
        if let Ok(mut guard) = self.0.write() {
            *guard = new_av;
        }
    }

    /// Locks and reads the AVal at the given key path.
    pub fn read_at(&self, _keypath: &VVal) -> VVal {
        VVal::None
    }
}

impl VValUserData for AtomicAVal {
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let args = env.argv_ref();
        match key {
            "read" => Ok(self.read()),
            "swap" => Ok(self.swap(&args[0])),
            "write" => {
                let v = &args[0];
                self.write(v);
                Ok(v.clone())
            }
            _ => Err(StackAction::panic_str(
                format!("Unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

/// This trait allows WLambda to create new threads.
/// You can either use the `DefaultThreadCreator`
/// default implementation or provide your own. Providing
/// your own might be necessary if you want to customize
/// how a thread is created.
///
/// If you only want to customize the GlobalEnv of newly created
/// threads please consider just providing your own
/// `DefaultGlobalEnvCreator`:
///
/// ```
/// use std::sync::Arc;
/// use std::sync::Mutex;
/// use wlambda::compiler::GlobalEnv;
/// use wlambda::threads::{DefaultThreadCreator, FunctionGlobalEnvCreator};
///
/// let global_env = GlobalEnv::new_default();
/// global_env.borrow_mut().set_thread_creator(
///     Some(Arc::new(Mutex::new(
///         DefaultThreadCreator::new(
///             FunctionGlobalEnvCreator::from(
///                 Box::new(|| GlobalEnv::new_default())))))));
/// ```
///
/// If you want to influence more things on thread creation,
/// see the followign example.
/// Please refer to the source of `DefaultThreadCreator`
/// for a comprehensive example.
///
/// ```
/// use wlambda::*;
/// use wlambda::threads::*;
/// use std::sync::Arc;
/// use std::sync::Mutex;
/// use std::rc::Rc;
/// use std::cell::RefCell;
///
/// // For simplicity we make detached threads here and don't pass any globals.
/// pub struct CustomThreadCreator();
///
/// impl ThreadCreator for CustomThreadCreator {
///     fn new_env(&mut self) -> GlobalEnvRef {
///         GlobalEnv::new_empty_default()
///     }
///
///     fn spawn(&mut self, tc: Arc<Mutex<dyn ThreadCreator>>,
///              _module_resolver: Option<Rc<RefCell<dyn wlambda::compiler::ModuleResolver>>>,
///              code: String,
///              globals: Option<std::vec::Vec<(String, AtomicAVal)>>) -> VVal {
///
///         let tcc = tc.clone();
///         let hdl =
///             std::thread::spawn(move || {
///                 // _module_resolver is interesting, in case you use preloaded files.
///                 // you have a chance here to customize the ModuleResolver in the new GlobalEnv
///                 // here and set preloaded files for it's resolver:
///                 let genv = GlobalEnv::new_empty_default();
///
///                 genv.borrow_mut().set_thread_creator(Some(tcc.clone()));
///
///                 let mut ctx = EvalContext::new(genv);
///
///                 match ctx.eval(&code) {
///                     Ok(v) => AVal::from_vval(&v),
///                     Err(e) => {
///                         AVal::Err(
///                             Box::new(
///                                 AVal::Str(format!("Error in Thread: {}", e))),
///                             String::from("?"))
///                     }
///                 }
///             });
///         VVal::None
///     }
/// }
/// ```
pub trait ThreadCreator: Send {
    /// Spawns a new thread with the given ThreadCreator.
    /// You need to pass the `tc` reference, so that the
    /// GlobalEnv of the thread also knows how to
    /// create new threads. You could even pass a different
    /// ThreadCreator for those.
    ///
    /// `code` is a String containing WLambda code which
    /// is executed after the new thread has been spawned.
    /// `globals` is a mapping of global variable names and
    /// AtomicAVal instances that are loaded into the threads
    /// global environment. This is the only way to share
    /// data between threads.
    fn spawn(
        &mut self,
        tc: Arc<Mutex<dyn ThreadCreator>>,
        module_resolver: Option<Rc<RefCell<dyn ModuleResolver>>>,
        code: String,
        globals: Option<std::vec::Vec<(String, AtomicAVal)>>,
    ) -> VVal;

    fn new_env(&mut self) -> GlobalEnvRef;
}

impl std::fmt::Debug for dyn ThreadCreator {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<<ThreadCreator>>")
    }
}

/// This trait handles creation of the GlobalEnv for new
/// threads. You can reuse the `DefaultThreadCreator` by
/// just providing it a custom `ThreadGlobalEnvCreator` implementation.
/// This suffices in most cases where you want to embed WLambda
/// and provide a custom API to new threads.
pub trait ThreadGlobalEnvCreator: Send {
    fn new_env(&mut self) -> GlobalEnvRef {
        GlobalEnv::new_empty_default()
    }
}

/// This is an easy way to provide a `GlobalEnv` creation
/// function to the DefaultThreadCreator.
///
/// ```
/// use std::sync::Arc;
/// use std::sync::Mutex;
/// use wlambda::{GlobalEnv};
/// use wlambda::threads::{DefaultThreadCreator, FunctionGlobalEnvCreator};
///
/// let global_env = GlobalEnv::new_default();
/// global_env.borrow_mut().set_thread_creator(
///     Some(Arc::new(Mutex::new(
///         DefaultThreadCreator::new(
///             FunctionGlobalEnvCreator::from(
///                 Box::new(|| GlobalEnv::new_default())))))));
/// ```
pub struct FunctionGlobalEnvCreator {
    fun: Box<dyn FnMut() -> GlobalEnvRef + Send>,
}

impl FunctionGlobalEnvCreator {
    /// Creates a new FunctionGlobalEnvCreator from a closure.
    ///
    /// ```
    /// use std::sync::Arc;
    /// use std::sync::Mutex;
    /// use wlambda::{GlobalEnv};
    /// use wlambda::threads::{DefaultThreadCreator, FunctionGlobalEnvCreator};
    ///
    /// let global_env = GlobalEnv::new_default();
    /// global_env.borrow_mut().set_thread_creator(
    ///     Some(Arc::new(Mutex::new(
    ///         DefaultThreadCreator::new(
    ///             FunctionGlobalEnvCreator::from(
    ///                 Box::new(|| GlobalEnv::new_default())))))));
    /// ```
    pub fn from(fun: Box<dyn FnMut() -> GlobalEnvRef + Send>) -> Self {
        Self { fun }
    }
}

impl ThreadGlobalEnvCreator for FunctionGlobalEnvCreator {
    fn new_env(&mut self) -> GlobalEnvRef {
        (self.fun)()
    }
}

/// Default implementation of a ThreadCreator.
///
/// See also `GlobalEnv::new_default` for further information
/// how this may be used.
pub struct DefaultThreadCreator<A: ThreadGlobalEnvCreator>(A);

#[allow(clippy::new_without_default)]
impl<A> DefaultThreadCreator<A>
where
    A: ThreadGlobalEnvCreator,
{
    pub fn new(env_creator: A) -> Self {
        Self(env_creator)
    }
}

/// To join a thread that was created by a DefaultThreadCreator
/// this JoinHandle wrapper is used. It provides a way to wrap it
/// into a `VValUserData` and use it by the WLambda function `std:thread:join`.
#[derive(Clone)]
pub(crate) struct DefaultThreadHandle {
    ready_slot: AtomicAValSlot,
    thread_handle: Rc<RefCell<Option<std::thread::JoinHandle<AVal>>>>,
}

impl DefaultThreadHandle {
    /// Joins the handle, and returns the result VVal of the thread.
    pub fn join(&self) -> VVal {
        let hdl = std::mem::replace(&mut (*self.thread_handle.borrow_mut()), None);
        if let Some(h) = hdl {
            h.join().unwrap().to_vval()
        } else {
            VVal::err_msg("DefaultThreadHandle already joined!")
        }
    }

    /// Returns an AtomicAValSlot, that can be used to check if the thread
    /// successfully started or ran into an error.
    pub fn get_ready_slot(&self) -> VVal {
        VVal::Usr(Box::new(self.ready_slot.clone()))
    }
}

impl VValUserData for DefaultThreadHandle {
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        match key {
            "join" => Ok(self.join()),
            "recv_ready" => {
                if let VVal::Usr(b) = self.get_ready_slot() {
                    b.call_method("recv", env)
                } else {
                    Ok(VVal::err_msg("Invalid _READY value!"))
                }
            }
            _ => Err(StackAction::panic_str(
                format!("Unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
    fn clone_ud(&self) -> Box<dyn crate::vval::VValUserData> {
        Box::new(self.clone())
    }
}

impl<A> ThreadCreator for DefaultThreadCreator<A>
where
    A: ThreadGlobalEnvCreator,
{
    fn new_env(&mut self) -> GlobalEnvRef {
        self.0.new_env()
    }

    fn spawn(
        &mut self,
        tc: Arc<Mutex<dyn ThreadCreator>>,
        module_resolver: Option<Rc<RefCell<dyn ModuleResolver>>>,
        code: String,
        globals: Option<std::vec::Vec<(String, AtomicAVal)>>,
    ) -> VVal {
        let ready = AtomicAValSlot::new();
        let tcc = tc.clone();

        let preloaded_files =
            module_resolver.map(|res| res.borrow().clone_preloaded_files()).flatten();

        let trdy = ready.clone();
        let hdl = std::thread::spawn(move || {
            let genv = match tcc.lock() {
                Ok(mut tcc) => (*tcc).new_env(),
                Err(e) => panic!("Can't lock ThreadCreator: {}", e),
            };

            genv.borrow_mut().set_thread_creator(Some(tcc.clone()));
            if let Some(preloaded_files) = preloaded_files {
                let resolver = Rc::new(RefCell::new(LocalFileModuleResolver::new_with_preloaded(
                    Rc::new(RefCell::new(preloaded_files)),
                )));
                genv.borrow_mut().set_resolver(resolver);
            }

            genv.borrow_mut().set_var("_READY", &VVal::Usr(Box::new(trdy.clone())));

            if let Some(globals) = globals {
                for (k, av) in globals {
                    let v = av.read();
                    genv.borrow_mut().set_var(&k, &v);
                }
            }

            let mut ctx = EvalContext::new(genv);

            match ctx.eval(&code) {
                Ok(v) => AVal::from_vval(&v),
                Err(e) => {
                    eprintln!("Error in thread: {}", e);
                    let ret = AVal::Err(
                        Box::new(AVal::Str(format!("Error in thread: {}", e))),
                        String::from("?"),
                    );
                    trdy.send(&ret.to_vval());
                    ret
                }
            }
        });

        VVal::Usr(Box::new(DefaultThreadHandle {
            ready_slot: ready,
            thread_handle: Rc::new(RefCell::new(Some(hdl))),
        }))
    }
}
