use std::collections::HashMap;
use std::vec::Vec;
use std::rc::Weak;
use std::rc::Rc;
use std::cell::RefCell;

struct StringInterner {
    strmap: HashMap<String, Weak<String>>,
    fixed:  std::vec::Vec<Symbol>,
}

impl StringInterner {
    fn new() -> Self {
        let mut s = Self { strmap: HashMap::new(), fixed: vec![] };
        let mut v = vec![];
        v.push(s.s2sym("x"));
        v.push(s.s2sym("y"));
        v.push(s.s2sym("z"));
        v.push(s.s2sym("w"));
        v.push(s.s2sym("_data"));
        v.push(s.s2sym("_proto"));
        v.push(s.s2sym("r"));
        v.push(s.s2sym("g"));
        v.push(s.s2sym("b"));
        v.push(s.s2sym("a"));
        v.push(s.s2sym("h"));
        v.push(s.s2sym("s"));
        v.push(s.s2sym("v"));
        s.fixed = v;
        s
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
