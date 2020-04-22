use std::collections::HashMap;
use std::vec::Vec;
use std::rc::Weak;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};

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

    fn collect(&mut self) -> i64 {
        let mut count = 0;
        let mut free = vec![];
        for (k, v) in self.strmap.iter() {
            if let Some(_) = v.upgrade() {
                ()
            } else {
                free.push(k.to_string());
            }
        }

        for k in free.iter() {
            count += 1;
            self.strmap.remove(k);
        }

        count
    }

    #[inline]
    fn s2sym(&mut self, s: &str) -> Symbol {
        if let Some(wc) = self.strmap.get(s) {
            if let Some(rc) = wc.upgrade() {
                return Symbol(rc);
            }
        }

        let rc = Rc::new(s.to_string());
        self.strmap.insert(s.to_string(), Rc::downgrade(&rc));
        Symbol(rc)
    }
}

thread_local! {
    static STR_INTERN: RefCell<StringInterner> = RefCell::new(StringInterner::new());
}

pub struct Symbol(Rc<String>);

impl Symbol {
    #[inline]
    pub fn ref_id(&self) -> i64 {
        &*(self.0) as *const String as i64
    }
}

impl PartialEq for Symbol {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let addr = &*(self.0) as *const String as u64;
        state.write_u64(addr);
    }
}

impl std::fmt::Debug for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let addr = &*(self.0) as *const String as u64;
        write!(f, "Symbol({}:{})", addr, *self.0)
    }
}

impl std::clone::Clone for Symbol {
    #[inline]
    fn clone(&self) -> Self { Self(self.0.clone()) }
}

impl std::convert::AsRef<str> for Symbol {
    #[inline]
    fn as_ref(&self) -> &str { &*self.0 }
}

impl std::cmp::PartialOrd for Symbol {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some((&*self.0).cmp(&*other.0))
    }
}

impl std::cmp::Ord for Symbol {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&*self.0).cmp(&*other.0)
    }
}

impl std::fmt::Display for Symbol {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &*self.0)
    }
}

//impl std::string::ToString for Symbol {
//    fn to_string(&self) -> String {
//        (*self.0).clone()
//    }
//}

//impl std::borrow::Borrow<str> for Symbol {
//    fn borrow(&self) -> &str {
//        &*self.0
//    }
//}
//
impl std::ops::Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &str {
        &*self.0
    }
}

pub fn s2sym(s: &str) -> Symbol {
    STR_INTERN.with(|si| {
        si.borrow_mut().s2sym(s)
    })
}

pub fn string_interner_collect() -> i64 {
    STR_INTERN.with(|si| {
        si.borrow_mut().collect()
    })
}

