use std::collections::HashMap;
use std::vec::Vec;
use std::rc::Weak;
use std::rc::Rc;
use std::cell::RefCell;

struct StringInterner {
    strmap: HashMap<String, Weak<String>>,
}

impl StringInterner {
    fn new() -> Self {
        Self {
            strmap: HashMap::new(),
        }
    }

    fn collect(&mut self) {
        let mut free = vec![];
        for (k, v) in self.strmap.iter() {
            if let Some(_) = v.upgrade() {
                ()
            } else {
                free.push(k.to_string());
            }
        }

        for k in free.iter() {
            self.strmap.remove(k);
        }
    }

    fn s2sym(&mut self, s: &str) -> Symbol {
        if let Some(wc) = self.strmap.get(s) {
            if let Some(rc) = wc.upgrade() {
                return rc;
            }
        }

        let rc = Rc::new(s.to_string());
        self.strmap.insert(s.to_string(), Rc::downgrade(&rc));
        rc
    }
}

thread_local! {
    static STR_INTERN: RefCell<StringInterner> = RefCell::new(StringInterner::new());
}

pub type Symbol = Rc<String>;

pub fn s2sym(s: &str) -> Symbol {
    STR_INTERN.with(|si| {
        si.borrow_mut().s2sym(s)
    })
}

pub fn string_interner_collect() {
    STR_INTERN.with(|si| {
        si.borrow_mut().collect();
    });
}
