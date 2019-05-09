// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use std::rc::Rc;
use std::rc::Weak;
use std::cell::RefCell;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Syntax {
    Var,
    Key,
    SetKey,
    Lst,
    Map,
    Expr,
    Func,
    Block,
    Call,
    Assign,
    Def,
    DefRef,
}

pub struct Env {
    pub locals: std::vec::Vec<VVal>,
    pub args:   std::vec::Vec<VVal>,
}

impl Env {
    pub fn new(local_size: usize, args: std::vec::Vec<VVal>) -> Env {
        let mut e = Env {
            locals: Vec::with_capacity(local_size),
            args: args,
        };
        e.locals.resize(local_size, VVal::Nul);
        e
    }

    pub fn arg(&mut self, index: usize) -> VVal { self.args[index].clone() }

    pub fn argv(&mut self) -> VVal {
        let v = VVal::vec();
        for a in self.args.iter() { v.push(a.clone()); }
        v
    }

    pub fn get(&mut self, index: usize) -> VVal {
        if let VVal::Ref(r) = &self.locals[index] {
            r.borrow().clone()
        } else {
            self.locals[index].clone()
        }
    }

    pub fn set(&mut self, index: usize, value: &VVal) {
        self.locals.resize(index + 1, VVal::Nul);
        if let VVal::Ref(r) = &self.locals[index] {
            r.replace(value.clone());
        } else {
            self.locals.insert(index, value.clone());
        }
    }

    pub fn set_consume(&mut self, index: usize, value: VVal) {
        self.locals.resize(index + 1, VVal::Nul);
        self.locals.insert(index, value);
    }
}

pub type EvalNode = Box<Fn(&mut Env) -> Result<VVal,u32>>;
pub type ClosNodeRef = Rc<RefCell<Fn(&std::vec::Vec<(usize, VVal)>, std::vec::Vec<VVal>) -> Result<VVal,u32>>>;

pub struct VValFun {
    pub fun:        ClosNodeRef,
    pub upvalues:   std::vec::Vec<(usize,VVal)>,
}

impl VValFun {
    pub fn new(fun: ClosNodeRef, upvalues: std::vec::Vec<(usize,VVal)>) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalues:   upvalues,
            fun:        fun,
        }))
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum VVal {
    Nul,
    Bol(bool),
    Str(String),
    Sym(String),
    Syn(Syntax),
    Int(i64),
    Flt(f64),
    Lst(Rc<RefCell<std::vec::Vec<VVal>>>),
    Map(Rc<RefCell<std::collections::HashMap<String, VVal>>>),
    Fun(Rc<VValFun>),
//    TMap((u32,Rc<RefCell<std::collections::HashMap<String, VVal>>>)),
    Ref(Rc<RefCell<VVal>>),
    WRef(Weak<RefCell<VVal>>),
}

impl std::fmt::Debug for VValFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "&VValFun")
    }
}

#[allow(dead_code)]
impl VVal {
    pub fn vec() -> VVal {
        return VVal::Lst(Rc::new(RefCell::new(Vec::new())));
    }

    pub fn call(&self, args: std::vec::Vec<VVal>) -> Result<VVal, u32> {
        println!("CALL {}!", self.s());

        match self {
            VVal::Fun(fu) => { (((*fu).fun.borrow()))(&fu.upvalues, args) },
            VVal::Sym(sym) => {
                if args.len() > 0 {
                    match args[0] {
                        VVal::Map(_) => {
                            let v = args[0].get_key(&sym);
                            Ok(if v.is_some() { v.unwrap() } else { VVal::Nul })
                        },
                        _ => Ok(VVal::Nul)
                    }
                } else { Ok(VVal::Nul) }
            },
            _ => { Ok(VVal::Nul) },
        }
    }

    pub fn to_ref(&mut self) -> VVal {
        VVal::Ref(Rc::new(RefCell::new(self.clone())))
    }

    pub fn downgrade(&mut self) -> Option<VVal> {
        if let VVal::Ref(f) = &self {
            Some(VVal::WRef(Rc::downgrade(f)))
        } else {
            None
        }
    }

    pub fn upgrade(&mut self) -> Option<VVal> {
        if let VVal::WRef(f) = &self {
            if let Some(r) = Weak::upgrade(f) {
                Some(VVal::Ref(r))
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn map() -> VVal {
        return VVal::Map(Rc::new(RefCell::new(std::collections::HashMap::new())));
    }

    pub fn sym(s: &str) -> VVal {
        return VVal::Sym(String::from(s));
    }

    pub fn dump_vec_as_str(v: &Rc<RefCell<std::vec::Vec<VVal>>>) -> String {
        let mut out : Vec<String> = Vec::new();
        let mut first = true;
        out.push(String::from("["));
        for s in v.borrow().iter().map(| x | x.s()) {
            if !first { out.push(String::from(",")); }
            else { first = false; }
            out.push(s);
        }
        out.push(String::from("]"));
        out.concat()
    }

    pub fn dump_map_as_str(m: &Rc<RefCell<std::collections::HashMap<String,VVal>>>) -> String {
        let mut out : Vec<String> = Vec::new();
        let mut first = true;
        out.push(String::from("{"));
        let hm = m.borrow();

        let mut keys : Vec<&String> = hm.keys().collect();
        keys.sort();
        for k in keys {
            let v = hm.get(k).unwrap();
            if !first { out.push(String::from(",")); }
            else { first = false; }
            if k.chars().any(|c| c.is_whitespace()) {
                out.push(format!("\"{}\"", k));
            } else {
                out.push(k.to_string());
            }
            out.push(String::from(":"));
            out.push(v.s());
        }
        out.push(String::from("}"));
        out.concat()
    }

    pub fn map_skip<T, R>(&self, mut op: T, skip: usize) -> Vec<R>
        where T: FnMut(&VVal) -> R {

        let mut res = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow_mut().iter().skip(skip) {
                res.push(op(i));
            }
        }
        res
    }

//    fn dump_map_as_str(v: &Rc<RefCell<std::collections::HashMap<String, VVal>>>) -> String {
////        v.iter().map(| x | x.s() + ", ").collect()
//        String::from("")
//    }

    pub fn unshift(&self, val: VVal) -> &VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().insert(0, val);
        }
        self
    }

    pub fn insert_at(&self, index: usize, val: VVal) {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().insert(index, val);
        }
    }

    pub fn set_at(&self, index: usize, val: VVal) {
        if let VVal::Lst(b) = &self {
            b.borrow_mut()[index] = val;
        }
    }

    pub fn at(&self, index: usize) -> Option<VVal> {
        if let VVal::Lst(b) = &self {
            return Some(b.borrow()[index].clone());
        }
        None
    }

    pub fn get_key(&self, key: &str) -> Option<VVal> {
        if let VVal::Map(m) = &self {
            m.borrow().get(&String::from(key)).cloned()
        } else {
            None
        }
    }

    pub fn set_key(&self, key: &str, val: VVal) {
        if let VVal::Map(m) = &self {
            m.borrow_mut().insert(String::from(key), val);
        }
    }

    pub fn push(&self, val: VVal) -> &VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().push(val);
        }
        self
    }

    pub fn s_raw(&self) -> String {
        match self {
            VVal::Str(s)  => s.clone(),
            VVal::Sym(s)  => s.clone(),
            _             => String::from(""),
        }
    }

    pub fn f(&self) -> f64 {
        match self {
            VVal::Str(s)  => s.parse::<f64>().unwrap_or(0.0),
            VVal::Nul     => 0.0,
            VVal::Bol(b)  => if *b { 1.0 } else { 0.0 },
            VVal::Sym(s)  => s.parse::<f64>().unwrap_or(0.0),
            VVal::Syn(s)  => (s.clone() as i64) as f64,
            VVal::Int(i)  => *i as f64,
            VVal::Flt(f)  => *f,
            VVal::Lst(l)  => l.borrow().len() as f64,
            VVal::Map(l)  => l.borrow().len() as f64,
            VVal::Fun(_)  => 1.0,
            VVal::Ref(l)  => (*l).borrow().f(),
            VVal::WRef(l) => {
                match l.upgrade() {
                    Some(v) => v.borrow().f(),
                    None => 0.0,
                }
            },
        }
    }

    pub fn i(&self) -> i64 {
        match self {
            VVal::Str(s)  => s.parse::<i64>().unwrap_or(0),
            VVal::Nul     => 0,
            VVal::Bol(b)  => if *b { 1 } else { 0 },
            VVal::Sym(s)  => s.parse::<i64>().unwrap_or(0),
            VVal::Syn(s)  => s.clone() as i64,
            VVal::Int(i)  => *i,
            VVal::Flt(f)  => (*f as i64),
            VVal::Lst(l)  => l.borrow().len() as i64,
            VVal::Map(l)  => l.borrow().len() as i64,
            VVal::Fun(_)  => 1,
            VVal::Ref(l)  => (*l).borrow().i(),
            VVal::WRef(l) => {
                match l.upgrade() {
                    Some(v) => v.borrow().i(),
                    None => 0,
                }
            },
        }
    }

    pub fn s(&self) -> String {
        match self {
            VVal::Str(s)  => s.clone(),
            VVal::Nul     => format!("$n"),
            VVal::Bol(b)  => if *b { format!("$true") } else { format!("$false") },
            VVal::Sym(s)  => format!("$\"{}\"", s),
            VVal::Syn(s)  => format!("&{:?}", s),
            VVal::Int(i)  => i.to_string(),
            VVal::Flt(f)  => f.to_string(),
            VVal::Lst(l)  => VVal::dump_vec_as_str(l),
            VVal::Map(l)  => VVal::dump_map_as_str(l), // VVal::dump_map_as_str(l),
            VVal::Fun(_)  => format!("&VValFun"),
//            VVal::TMap((t,_l))  => format!("${}{{}}", t), // VVal::dump_map_as_str(l),
            VVal::Ref(l)  => format!("REF[{}]", (*l).borrow().s()),
            VVal::WRef(l) => {
                match l.upgrade() {
                    Some(v) => format!("WREF[{}]", v.borrow().s()),
                    None => String::from("<deadref>"),
                }
            },
        }
    }
}
