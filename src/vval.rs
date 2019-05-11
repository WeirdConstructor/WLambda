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

const STACK_SIZE : usize = 4096;

#[derive(Debug, Clone, PartialEq)]
pub enum VarPos {
    NoPos,
    UpValue(usize),
    Local(usize),
}

pub struct Env {
    pub args:               std::vec::Vec<VVal>,
    pub fun:                Option<Rc<VValFun>>,
    pub cur_locals_size:    usize,
    pub sp:                 usize,
}

impl Env {
    pub fn new_s() -> Env {
        let mut e = Env {
            args:               Vec::with_capacity(STACK_SIZE),
            fun:                None,
            cur_locals_size:    0,
            sp:                 0,
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    pub fn repl_upv(&mut self, fun: Option<Rc<VValFun>>) -> Option<Rc<VValFun>> {
        std::mem::replace(&mut self.fun, fun)
    }

    pub fn repl_locals_size(&mut self, lss: usize) -> usize {
        std::mem::replace(&mut self.cur_locals_size, lss)
    }

    pub fn reserve_locals(&mut self, env_size: usize) -> usize {
        self.sp = self.sp + env_size;
        std::mem::replace(&mut self.cur_locals_size, env_size)
    }

    pub fn set_after(&mut self, offs: usize, v: VVal) {
        self.args[self.sp + offs] = v;
    }

    pub fn push_sp(&mut self, n: usize) {
        self.sp = self.sp + n;
    }

    pub fn push(&mut self, v: VVal) -> usize {
        self.args[self.sp] = v;
        self.sp = self.sp + 1;
        self.sp - 1
    }

    pub fn popn(&mut self, n: usize) {
        if self.sp < n {
            panic!(format!("Stack pointer underflow {} {}", self.sp, n));
        }
        self.sp = self.sp - n;
        println!("POPN {} => {}", n, self.sp);
    }

    pub fn dump_stack(&self) {
        let mut i = 0;
        for v in self.args.iter() {
            println!("    [{:3}] = {}", i, v.s());
            i = i + 1;
            if i >= self.sp { break; }
        }
    }

    pub fn at(&self, pos: usize) -> &VVal {
        println!("VAR AT {} - ({} + {} + 1)", self.sp, self.cur_locals_size, pos);
        &self.args[self.sp - (self.cur_locals_size + pos + 1)]
    }

    pub fn slice(&mut self, size: usize) -> &mut [VVal] {
        println!("SLICE sp={}, locals={} size={}", self.sp, self.cur_locals_size, size);
        &mut self.args[(self.sp - size)..self.sp]
    }

    pub fn argv(&mut self) -> VVal {
        let v = VVal::vec();
        for i in 0..self.sp { v.push(self.args[i].clone()); }
        v
    }

    pub fn get_up_raw(&mut self, i: usize) -> VVal {
        if let Some(v) = &self.fun {
            v.upvalues[i].clone()
        } else {
            VVal::Nul
        }
    }

    pub fn get_up(&mut self, i: usize) -> VVal {
        if let Some(v) = &self.fun {
            if let VVal::Ref(r) = &v.upvalues[i] {
                r.borrow().clone()
            } else {
                v.upvalues[i].clone()
            }
        } else {
            VVal::Nul
        }
    }

    pub fn get_local_raw(&mut self, i: usize) -> VVal {
        let idx = self.sp - (i + 1);
        println!("GET_LOCAL_RAW [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        self.dump_stack();
        self.args[idx].clone()
    }

    pub fn get_local(&mut self, i: usize) -> VVal {
        let idx = self.sp - (i + 1);
        println!("GET_LOCAL [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        self.dump_stack();
        if let VVal::Ref(r) = &self.args[idx] {
            let rr = r.borrow().clone();
            println!("GOT: {:?}", rr);
            rr
        } else {
            self.args[idx].clone()
        }
    }

    pub fn set_up(&mut self, index: usize, value: &VVal) {
        if let Some(v) = self.fun.as_mut() {
            let fun = v.clone();
            println!("SET_UP [{}] ({})=({})", index, fun.upvalues[index].s(), value.s());
            let upv = &fun.upvalues[index];
            if let VVal::Ref(r) = upv {
                r.replace(value.clone());
                return;
            }
            // Will not mutate non ref upvalue!
            // panic!(format!("Cannot mutate a non ref upvalue {} = {}", index, value.s()));
            // But also not panic here.
        }
    }

    pub fn set_local(&mut self, i: usize, value: &VVal) {
        let idx = self.sp - (i + 1);
        println!("SET_LOCAL [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        if let VVal::Ref(r) = &self.args[idx] {
            r.replace(value.clone());
        } else {
            self.args[idx] = value.clone();
        }
    }

    pub fn set_consume(&mut self, i: usize, value: VVal) {
        let idx = self.sp - (i + 1);
        self.args[idx] = value;
    }
}

#[derive(Debug, Clone)]
pub enum StackAction {
//    Panic(String),
    Break(VVal),
}

pub type EvalNode = Box<Fn(&mut Env) -> Result<VVal,StackAction>>;
pub type ClosNodeRef = Rc<RefCell<Fn(&Rc<VValFun>, &mut Env, usize) -> Result<VVal,StackAction>>>;

pub struct VValFun {
    pub fun:        ClosNodeRef,
    pub upvalues:   std::vec::Vec<VVal>,
}

impl VValFun {
    pub fn new(fun: ClosNodeRef, upvalues: std::vec::Vec<VVal>) -> VVal {
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

    pub fn call(&self, env: &mut Env, argc: usize) -> Result<VVal, StackAction> {
        match self {
            VVal::Fun(fu) => { (((*fu).fun.borrow()))(fu, env, argc) },
            VVal::Bol(b) => {
                if *b {
                    if argc > 0 {
                        let v = env.at(1).clone();
                        v.call(env, 0)
                    } else { Ok(VVal::Nul) }
                } else {
                    if argc > 1 {
                        let v = env.at(0).clone();
                        v.call(env, 0)
                    } else { Ok(VVal::Nul) }
                }
            },
            VVal::Sym(sym) => {
                if argc > 0 {
                    let v = env.at(0);
                    match v {
                        VVal::Map(_) => {
                            let v = v.get_key(&sym);
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

    pub fn eq(&self, v: &VVal) -> bool {
        match self {
            VVal::Nul     => { if let VVal::Nul = v { return true; } else { return false; } },
            VVal::Int(ia) => { if let VVal::Int(ib) = v { return ia == ib; } else { return false; } },
            _             => { return false; }
        }
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
        println!("FN PUSH {} v {}", self.s(), val.s());
        if let VVal::Lst(b) = &self {
            println!("FN ! PUSH {} v {}", self.s(), val.s());
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
