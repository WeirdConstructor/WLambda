// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!

This module provides threading functionality for WLambda.
It does not depend on anything else than the Rust standard library.

If you want to implement or specialize thread creation please
refer to the documentation of the `ThreadCreator` trait.

*/

use crate::vval::*;
use crate::compiler::*;
use crate::nvec::NVec;

use std::rc::Rc;
use std::cell::RefCell;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;
use std::sync::mpsc::{Receiver, Sender, TryRecvError};
use std::fmt::Formatter;

use fnv::FnvHashMap;

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
#[derive(Clone, Debug)]
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
    Atom(AtomicAVal),
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
                    if i == 0 { v.0 = av; }
                    else      { v.1 = av; }
                }
            },
            v => {
                let key = v.s_raw();
                if let AVal::Map(ref mut m) = self {
                    m.insert(key, av);
                }
            },
        }
    }

    /// Converts the AVal back to a VVal.
    ///
    /// ```
    /// use wlambda::*;
    /// assert_eq!(AVal::Sym(String::from("x")).to_vval().s(), ":\"x\"");
    /// ```
    pub fn to_vval(&self) -> VVal {
        match self {
            AVal::None => VVal::None,
            AVal::Err(av, pos) => {
                let v = VVal::vec();
                v.push(av.to_vval());
                v.push(VVal::new_str(pos));
                VVal::Err(Rc::new(RefCell::new((v,
                     SynPos { syn: Syntax::Block, line: 0,
                              col: 0, file: FileRef::new("?"), name: None }))))
            },
            AVal::Bol(b)       => VVal::Bol(*b),
            AVal::Int(i)       => VVal::Int(*i),
            AVal::Flt(f)       => VVal::Flt(*f),
            AVal::Sym(s)       => VVal::new_sym(s),
            AVal::Str(s)       => VVal::new_str(s),
            AVal::Byt(b)       => VVal::new_byt(b.clone()),
            AVal::Atom(a)      => VVal::Usr(Box::new(a.clone())),
            AVal::Opt(None)    => VVal::opt_none(),
            AVal::Opt(Some(a)) => VVal::opt(a.to_vval()),
            AVal::Pair(p)      => VVal::pair(p.0.to_vval(), p.1.to_vval()),
            AVal::FVec(v)      => VVal::FVec(*v),
            AVal::IVec(v)      => VVal::IVec(*v),
            AVal::Lst(l) => {
                let v = VVal::vec();
                for av in l.iter() {
                    v.push(av.to_vval());
                }
                v
            },
            AVal::Map(m) => {
                let mv = VVal::map();
                for (k, v) in m.iter() {
                    mv.set_key_mv(k.clone(), v.to_vval());
                }
                mv
            },
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
                AVal::Err(
                    Box::new(AVal::from_vval(&eb.0)),
                    format!("{}", eb.1))
            },
            VVal::Bol(b)       => AVal::Bol(*b),
            VVal::Sym(s)       => AVal::Sym(String::from(s.as_ref())),
            VVal::Str(s)       => AVal::Str(s.borrow().clone()),
            VVal::Byt(b)       => AVal::Byt(b.borrow().clone()),
            VVal::Int(i)       => AVal::Int(*i),
            VVal::Flt(f)       => AVal::Flt(*f),
            VVal::Opt(None)    => AVal::Opt(None),
            VVal::Opt(Some(v)) => AVal::Opt(Some(Box::new(AVal::from_vval(v)))),
            VVal::FVec(v)      => AVal::FVec(*v),
            VVal::IVec(v)      => AVal::IVec(*v),
            VVal::Pair(p) =>
                AVal::Pair(
                    Box::new((
                        AVal::from_vval(&p.0),
                        AVal::from_vval(&p.1)))),
            VVal::Lst(l) => {
                let mut avec = vec![];
                for vv in l.borrow().iter() {
                    avec.push(AVal::from_vval(vv));
                }
                AVal::Lst(avec)
            },
            VVal::Map(m) => {
                let mut amap =
                    FnvHashMap::with_capacity_and_hasher(2, Default::default());
                for (k, v) in m.borrow().iter() {
                    amap.insert(k.clone(), AVal::from_vval(v));
                }
                AVal::Map(amap)
            },
            VVal::Usr(u) => {
                let mut cl_ud = u.clone_ud();
                if let Some(ud) = cl_ud.as_any().downcast_mut::<AtomicAVal>() {
                    AVal::Atom(ud.clone())

                } else {
                    AVal::None
                }
            },
            _ => AVal::None,
        }
    }
}

/// WLambda:
///
/// ```text
/// !atom = std:sync:atom:new 10;
/// !queue = std:sync:mpsc:new[];
/// !thrd = std:spawn_thread $q{
///     !val = main.read;
///     main.write $[1,$[0,1],3];
///
///     main.read_at 0;
///     main.write_at $[1, 0] 320;
///     q.push $["done", 10];
///
/// } ${ main = atom, q = queue };
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
    fn default() -> Self { AtomicAVal::new() }
}

#[allow(dead_code)]
pub struct AValSender(Rc<Sender<AVal>>);

#[allow(dead_code)]
impl AValSender {
    pub fn send(&self, v: &VVal) -> VVal {
        if let Err(e) = self.0.send(AVal::from_vval(v)) {
            VVal::err_msg(&format!("send error: {}", e))
        } else {
            VVal::Bol(true)
        }
    }
}

#[allow(dead_code)]
pub struct AValReceiver(Arc<Mutex<Receiver<AVal>>>);

#[allow(dead_code)]
impl AValReceiver {
    pub fn try_recv(&self, _timeout: i64) -> VVal {
        match self.0.lock() {
            Ok(guard) => {
                match guard.try_recv() {
                    Ok(av) => av.to_vval(),
                    Err(TryRecvError::Empty) => VVal::None,
                    Err(e) => {
                        VVal::err_msg(&format!("try_recv error: {}", e))
                    }
                }
            },
            Err(e) => {
                VVal::err_msg(&format!("try_recv error: {}", e))
            }
        }
    }
}

//impl VValUserData for Sender {
//    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
//    fn clone_ud(&self) -> Box<dyn VValUserData> {
//        Box::new(self.clone())
//    }
//}
//
//impl VValUserData for ReadLock {
//    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
//    fn clone_ud(&self) -> Box<dyn VValUserData> {
//        Box::new(self.clone())
//    }
//}

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
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
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
/// Please refer to the source of `DefaultThreadCreator`
/// for a comprehensive example.
///
/// ```
/// use wlambda::*;
/// use wlambda::threads::*;
/// use std::sync::Arc;
/// use std::sync::Mutex;
///
/// // For simplicity we make detached threads here and don't pass any globals.
/// pub struct CustomThreadCreator();
///
/// impl ThreadCreator for CustomThreadCreator {
///     fn spawn(&mut self, tc: Arc<Mutex<dyn ThreadCreator>>,
///              code: String,
///              globals: Option<std::vec::Vec<(String, AtomicAVal)>>) -> VVal {
///
///         let tcc = tc.clone();
///         let hdl =
///             std::thread::spawn(move || {
///                 let genv = GlobalEnv::new_empty_default();
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
    fn spawn(&mut self, tc: Arc<Mutex<dyn ThreadCreator>>,
             code: String,
             globals: Option<std::vec::Vec<(String, AtomicAVal)>>) -> VVal;
}

impl std::fmt::Debug for dyn ThreadCreator {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<<ThreadCreator>>")
    }
}

/// Default implementation of a ThreadCreator.
///
/// See also `GlobalEnv::new_default` for further information
/// how this may be used.
pub struct DefaultThreadCreator();

#[allow(clippy::new_without_default)]
impl DefaultThreadCreator {
    pub fn new() -> Self { Self {} }
}

/// To join a thread that was created by a DefaultThreadCreator
/// this JoinHandle wrapper is used. It provides a way to wrap it
/// into a `VValUserData` and use it by the WLambda function `std:thread:join`.
#[derive(Clone)]
pub struct DefaultThreadHandle(Rc<RefCell<Option<std::thread::JoinHandle<AVal>>>>);

impl DefaultThreadHandle {
    /// Joins the handle, and returns the result VVal of the thread.
    pub fn join(&self, env: &mut Env) -> VVal {
        let hdl = std::mem::replace(&mut (*self.0.borrow_mut()), None);
        if let Some(h) = hdl {
            h.join().unwrap().to_vval()
        } else {
            env.new_err(
                "DefaultThreadHandle already joined!".to_string())
        }
    }
}

impl VValUserData for DefaultThreadHandle {
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn crate::vval::VValUserData> {
        Box::new(self.clone())
    }
}

impl ThreadCreator for DefaultThreadCreator {
    fn spawn(&mut self, tc: Arc<Mutex<dyn ThreadCreator>>,
             code: String,
             globals: Option<std::vec::Vec<(String, AtomicAVal)>>) -> VVal {

        let tcc = tc.clone();
        let hdl =
            std::thread::spawn(move || {
                let genv = GlobalEnv::new_empty_default();
                genv.borrow_mut().set_thread_creator(Some(tcc.clone()));

                if let Some(globals) = globals {
                    for (k, av) in globals {
                        genv.borrow_mut().set_var(&k, &VVal::Usr(Box::new(av)));
                    }
                }

                let mut ctx = EvalContext::new(genv);

                match ctx.eval(&code) {
                    Ok(v) => AVal::from_vval(&v),
                    Err(e) => {
                        AVal::Err(
                            Box::new(
                                AVal::Str(format!("Error in Thread: {}", e))),
                            String::from("?"))
                    }
                }
            });
        VVal::Usr(Box::new(DefaultThreadHandle(Rc::new(RefCell::new(Some(hdl))))))
    }
}
