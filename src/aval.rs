use crate::compiler::EvalContext;
use crate::vval::*;

use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;

use fnv::FnvHashMap;

#[derive(Clone, Debug, PartialEq)]
enum AVal {
    Free,
    Nul,
    Err(usize, String),
    Bol(bool),
    Sym(String),
    Str(String),
    Byt(Vec<u8>),
    Int(i64),
    Flt(f64),
    Lst(Vec<usize>),
    Map(FnvHashMap<String, usize>),
}

#[derive(Clone, Debug)]
struct StoreAVal {
    store:      Vec<(u8, AVal)>,
    next_free:  Option<usize>,
    color:      u8,
}

impl StoreAVal {
    fn new() -> Self {
        Self {
            store: vec![(0, AVal::Free)],
            next_free: Some(0),
            color: 0,
        }
    }

    /*
    fn mark(&mut self, idx: usize, ) {
        self.store[idx].0 = color;
        match &self.store[idx].1 {
            AVal::Lst(vc) => {
                for i in vc.iter() {
                    self.mark_stack.push(*i);
                }
            },
            AVal::Map(mp) => {
                for (_, i) in mp.iter() {
                    self.mark_stack.push(*i);
                }
            },
            _ => (),
        }
    }

    // Must only be called if there are no free/dead elements anymore.
    fn mark_and_free(&mut self) {
        self.mark_stack.clear();
        self.mark_stack.push(0);

        // choose next "alive" color:
        self.color = (self.color + 1) % 3;
        loop {
            let idx = self.mark_stack.pop();
            if let Some(idx) = idx {
                self.store[idx].0 = self.color;
                self.mark(idx);
            } else {
                break;
            }
        }

        self.next_free = None;
        for (i, (clr, av)) in self.store.iter_mut().enumerate() {
            if *clr != self.color {
                *av = AVal::Free;
                self.next_free = Some(i);
            }
        }
    }

    fn replace(&mut self, av: AVal) {
        // reset all elements to be deleted:
        for (_, av) in self.store.iter_mut() {
            *av = AVal::Free;
        }
        self.next_free = Some(0);
        self.store(av);
    }

    fn next_free(&mut self) -> usize {
        let nf = self.next_free;

        if let Some(idx) = nf {
            let len = self.store.len();
            self.next_free = None;
            for i in 1..len {
                let next_i = (idx + i) % len;
                if self.store[next_i].1 == AVal::Free {
                    self.next_free = Some(next_i);
                }
            }

            idx
        } else {
            self.mark_and_free();
            if self.next_free.is_some() {
                self.next_free()
            } else {
                self.store.push((self.color, AVal::Free));
                self.store.len() - 1
            }
        }
    }

    fn store(&mut self, av: AVal) -> usize {
        let idx = self.next_free();
        self.store[idx] = (self.color, av);
        idx
    }
    */
}

struct AtomicAVal(Arc<RwLock<StoreAVal>>);

impl AtomicAVal {
    fn new() -> Self {
        Self(Arc::new(RwLock::new(StoreAVal::new())))
    }

    fn store(&mut self, vv: VVal) {
    }

    fn read(&mut self, vv: VVal) {
    }
}

//impl From<Arc<Mutex<AVal>>> for AVal {
//    fn from(r: Rc<RefCell<Ship>>) -> ShipWlWrapper {
//        ShipWlWrapper(r)
//    }
//}
//
//impl Into<VVal> for ShipWlWrapper {
//    fn into(self) -> VVal { VVal::Usr(Box::new(self)) }
//}
//
//impl VValUserData for ShipWlWrapper {
//    // ...
//    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
//    fn clone_ud(&self) -> Box<dyn wlambda::vval::VValUserData> {
//        Box::new(self.clone())
//    }
//}
