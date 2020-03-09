// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::vval::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;

use fnv::FnvHashMap;

#[derive(Clone, Debug)]
pub enum AVal {
    Nul,
    Err(Box<AVal>, String),
    Bol(bool),
    Sym(String),
    Str(String),
    Byt(Vec<u8>),
    Int(i64),
    Flt(f64),
    Lst(Vec<AVal>),
    Map(FnvHashMap<String, AVal>),
    Atom(AtomicAVal),
}

impl AVal {
    pub fn set_at_path(&mut self, path_idx: usize, pth: &VVal, av: AVal) {
        match pth.at(path_idx).unwrap_or(VVal::Nul) {
            VVal::Int(i) => {
                if let AVal::Lst(ref mut v) = self {
                    if (i as usize) < v.len() {
                        v[i as usize] = av;
                    } else {
                        v.insert(i as usize, av);
                    }
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

    pub fn to_vval(&self) -> VVal {
        match self {
            AVal::Nul    => VVal::Nul,
            AVal::Err(av, pos) => {
                let v = VVal::vec();
                v.push(av.to_vval());
                v.push(VVal::new_str(pos));
                VVal::Err(Rc::new(RefCell::new((v,
                     SynPos { syn: Syntax::Block, line: 0,
                              col: 0, file: FileRef::new("?"), name: None }))))
            },
            AVal::Bol(b) => VVal::Bol(*b),
            AVal::Int(i) => VVal::Int(*i),
            AVal::Flt(f) => VVal::Flt(*f),
            AVal::Sym(s) => VVal::new_sym(s),
            AVal::Str(s) => VVal::new_str(s),
            AVal::Byt(b) => VVal::new_byt(b.clone()),
            AVal::Atom(a) => VVal::Usr(Box::new(a.clone())),
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
                    mv.set_map_key(k.clone(), v.to_vval());
                }
                mv
            },
        }
    }

    pub fn from_vval(v: &VVal) -> Self {
        match v {
            VVal::Nul => AVal::Nul,
            VVal::Err(e) => {
                let eb = e.borrow();
                AVal::Err(
                    Box::new(AVal::from_vval(&eb.0)),
                    format!("{}", eb.1))
            },
            VVal::Bol(b) => AVal::Bol(*b),
            VVal::Sym(s) => AVal::Sym(s.borrow().clone()),
            VVal::Str(s) => AVal::Str(s.borrow().clone()),
            VVal::Byt(b) => AVal::Byt(b.borrow().clone()),
            VVal::Int(i) => AVal::Int(*i),
            VVal::Flt(f) => AVal::Flt(*f),
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
                    AVal::Nul
                }
            },
            _ => AVal::Nul,
        }
    }
}

/// WLambda:
///
/// ```
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

#[derive(Clone, Debug)]
pub struct AtomicAVal(Arc<RwLock<AVal>>);

impl AtomicAVal {
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(AVal::Nul)))
    }

    pub fn store(&self, vv: &VVal) {
        let new_av = AVal::from_vval(vv);
        if let Ok(mut guard) = self.0.write() {
            *guard = new_av;
        }
    }

    pub fn read(&self) -> VVal {
        if let Ok(guard) = self.0.read() {
            guard.to_vval()
        } else {
            return VVal::err_msg("Lock Poisoned");
        }
    }

    pub fn store_at(&self, keypath: &VVal, vv: &VVal) {
        let new_av = AVal::from_vval(vv);
        if let Ok(mut guard) = self.0.write() {
            *guard = new_av;
        }
    }

    pub fn read_at(&self, keypath: &VVal) -> VVal {
        VVal::Nul
    }
}

impl VValUserData for AtomicAVal {
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}
