// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!

This module provides the core data structures used by the parser,
compiler and evaluator of WLambda.

*/

use std::rc::Rc;
use std::rc::Weak;
use std::cell::RefCell;
use std::fmt;

/// Structure for holding information about origin
/// of an AST node.
#[derive(Debug, Clone, PartialEq)]
pub struct SynPos {
    pub syn:        Syntax,
    pub line:       u32,
    pub col:        u32,
    pub file:       u32,
}

/// Encodes the different types of AST nodes.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Syntax {
    Var,
    Key,
    SetKey,
    Str,
    Lst,
    Map,
    Expr,
    Func,
    Block,
    Call,
    And,
    Or,
    Assign,
    Def,
    DefRef,
    DefWRef,
}

/// The maximum stack size.
///
/// Currently hardcoded, but later the API user will be able to specify it.
const STACK_SIZE : usize = 10240;

/// The runtime environment of the evaluator.
pub struct Env {
    /// The argument stack, limited to `STACK_SIZE`.
    pub args: std::vec::Vec<VVal>,
    /// A reference to the currently executed function.
    ///
    /// Used for accessing the up values and other details
    /// about the function.
    pub fun:  Rc<VValFun>,
    /// The basepointer to reference arguments and
    /// local variables.
    ///
    /// - `bp + n (n >= 0)` references a local variable
    /// - `bp - n (n > 0)` references an argument
    pub bp:   usize,
    /// The current stack pointer.
    pub sp:   usize,
    /// The argument count to the current call.
    pub argc: usize,
}

impl Env {
    pub fn new() -> Env {
        let mut e = Env {
            args:               Vec::with_capacity(STACK_SIZE),
            fun:                VValFun::new_dummy(),
            bp:                 0,
            sp:                 0,
            argc:               0,
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    pub fn set_bp(&mut self, env_size: usize) -> usize {
        let new_bp = self.sp;
        self.sp = self.sp + env_size;
        std::mem::replace(&mut self.bp, new_bp)
    }

    pub fn reset_bp(&mut self, env_size: usize, oldbp: usize) {
        //d// println!("RESET BP FROM:");
        //d// self.dump_stack();
        for i in self.bp..self.sp {
            self.args[i] = VVal::Nul;
        }
        self.sp = self.sp - env_size;
        self.bp = oldbp;
    }

//    pub fn reserve_locals(&mut self, env_size: usize) -> usize {
//        self.sp = self.sp + env_size;
//        std::mem::replace(&mut self.cur_locals_size, env_size)
//    }


    pub fn with_fun_info<T>(&mut self, fu: Rc<VValFun>, argc: usize, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {
        let local_size = fu.local_size;
        let old_argc = std::mem::replace(&mut self.argc, argc);
        let old_fun  = std::mem::replace(&mut self.fun, fu);
        let old_bp   = self.set_bp(local_size);

        let ret = f(self);

        self.reset_bp(local_size, old_bp);
        self.fun  = old_fun;
        self.argc = old_argc;

        ret
    }

    pub fn with_pushed_sp<T>(&mut self, n: usize, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {

        self.push_sp(n);
        let ret = f(self);
        self.popn(n);
        ret
    }

    pub fn push_sp(&mut self, n: usize) {
        self.sp = self.sp + n;
        //d// println!("PUSH_SP {} => {}", n, self.sp);
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
        if n > 0 {
            for i in (self.sp - 1)..((self.sp - n) + 1) {
                //d// println!("POP[{}] {}", i, self.args[i].s());
                self.args[i] = VVal::Nul;
            }
        }
        self.sp = self.sp - n;
        //d// println!("POPN {} => {}", n, self.sp);
    }

    pub fn dump_stack(&self) {
        let mut i = 0;
        println!("* SP={}, BP={}", self.sp, self.bp);
        for il in self.bp..self.sp {
            println!("    LOCAL [{:3}] = {}", i, self.args[il].s());
            i = i + 1;
        }
        i = 0;
        for v in self.args.iter() {
            println!("    GLOBL [{:3}] = {}", i, v.s());
            i = i + 1;
            if i >= self.sp { break; }
        }
        let mut i = 0;
        for u in self.fun.upvalues.iter() {
            println!("  UP[{:3}] = {}", i, u.s());
            i = i + 1;
        }
    }

    pub fn argv(&mut self) -> VVal {
        VVal::vec_from(&self.args[(self.sp - self.argc)..self.sp])
    }

    pub fn get_up_raw(&mut self, i: usize) -> VVal {
        self.fun.upvalues[i].clone()
    }

    pub fn get_up(&mut self, i: usize) -> VVal {
        if let VVal::Ref(r) = &self.fun.upvalues[i] {
            r.borrow().clone()
        } else {
            self.fun.upvalues[i].clone()
        }
    }

    pub fn set_arg(&mut self, i: usize, v: VVal) {
        //d// println!("SET ARG [{}/{}]= {}", i, self.sp - (i + 1), v.s());
        self.args[self.sp - (i + 1)] = v;
    }

    pub fn arg(&self, i: usize) -> VVal {
        if i >= self.argc { return VVal::Nul; }
        let v = &self.args[self.bp - (i + 1)];
        //d// println!("GET ARG [{}/{}] = {}", i, self.sp - (i + 1), v.s());
        match v {
            VVal::Ref(r)     => r.borrow().clone(),
            VVal::DropFun(r) => r.v.clone(),
            VVal::WRef(r)    => r.borrow().clone(),
            VVal::WWRef(r)   => if let Some(v) = r.upgrade() { v.borrow().clone() } else { VVal::Nul },
            v                => v.clone(),
        }
    }

    pub fn get_local_raw(&mut self, i: usize) -> VVal {
        let idx = self.bp + i;
        //d// println!("GET_LOCAL_RAW [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        //d// self.dump_stack();
        self.args[idx].clone()
    }

    pub fn get_local(&mut self, i: usize) -> VVal {
        let idx = self.bp + i;
        //d// println!("GET_LOCAL [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        //d// self.dump_stack();
        match &self.args[idx] {
            VVal::Ref(r)     => r.borrow().clone(),
            VVal::DropFun(r) => r.v.clone(),
            VVal::WRef(r)    => r.borrow().clone(),
            VVal::WWRef(r)   => if let Some(v) = r.upgrade() { v.borrow().clone() } else { VVal::Nul },
            v                => v.clone(),
        }
    }

    pub fn set_up(&mut self, index: usize, value: &VVal) {
        let fun = self.fun.clone();
        //d// println!("SET_UP [{}] ({})=({})", index, fun.upvalues[index].s(), value.s());
        let upv = &fun.upvalues[index];

        match upv {
            VVal::Ref(r)     => { r.replace(value.clone()); }
            VVal::WRef(r)    => { r.replace(value.clone()); }
            VVal::WWRef(r)   => {
                if let Some(r) = Weak::upgrade(r) {
                    r.replace(value.clone());
                }
            },
            _ => {}
        }
        // Will not mutate non ref upvalue!
        // panic!(format!("Cannot mutate a non ref upvalue {} = {}", index, value.s()));
        // But also not panic here.
        return;
    }

    pub fn set_local(&mut self, i: usize, value: &VVal) {
        let idx = self.bp + i;
        //d// println!("SET_LOCAL [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        //
        match &self.args[idx] {
            VVal::Ref(r)     => { r.replace(value.clone()); }
            VVal::WRef(r)    => { r.replace(value.clone()); }
            VVal::WWRef(r)   => {
                if let Some(r) = Weak::upgrade(r) {
                    r.replace(value.clone());
                }
            },
            _ => {
                self.args[idx] = value.clone();
            }
        }
    }

    pub fn set_consume(&mut self, i: usize, value: VVal) {
        let idx = self.bp + i;
        self.args[idx] = value;
    }
}

/// Encodes all kinds of jumps up the call stack, like `break` and `next` in Loops.
///
/// As WLambda is not using a VM, it uses return values of the
/// closure call tree to handle jumping up the stack.
#[derive(Debug, Clone)]
pub enum StackAction {
//    Panic(String),
    Break(VVal),
    Next,
}

pub type EvalNode = Box<Fn(&mut Env) -> Result<VVal,StackAction>>;
pub type ClosNodeRef = Rc<RefCell<Fn(&mut Env, usize) -> Result<VVal,StackAction>>>;

/// This structure is the runtime representation of a WLambda function value.
pub struct VValFun {
    /// The closure that runs the function.
    pub fun:        ClosNodeRef,
    /// Contains any caught upvalues.
    pub upvalues:   std::vec::Vec<VVal>,
    /// The number of local variables defined in this functions.
    ///
    /// This value is used to reserve stack space for storing them.
    pub local_size: usize,
}

impl VValFun {
    pub fn new(fun: ClosNodeRef, upvalues: std::vec::Vec<VVal>, env_size: usize) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalues:   upvalues,
            fun:        fun,
            local_size: env_size,
        }))
    }

    pub fn new_dummy() -> Rc<VValFun> {
        Rc::new(VValFun {
            fun:        Rc::new(RefCell::new(|_: &mut Env, _a: usize| { Ok(VVal::Nul) })),
            upvalues:   Vec::new(),
            local_size: 0,
        })
    }
}

/// Handles calling of destructor functions.
#[derive(Debug, Clone)]
pub struct DropVVal {
    pub v:      VVal,
    pub fun:    VVal,
}

impl Drop for DropVVal {
    #[allow(unused_must_use)]
    fn drop(&mut self) {
        let mut e = Env::new();
        e.push(self.v.clone());
        self.fun.call(&mut e, 1);
    }
}

/// VVal aka. VariantValue is a data structure to represent
/// all kinds of WLambda data structures.
///
/// It's used for the AST, for internal data and for runtime data structures.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum VVal {
    /// The nul value, the default value of all non initialized data.
    Nul,
    /// Representation of a boolean value.
    Bol(bool),
    /// Representation of a symbol or key.
    ///
    /// This one might be interned at some point, so that it only contains
    /// an Rc<SymRef> in future.
    Sym(String),
    /// Representation of a unicode/text string.
    Str(Rc<RefCell<String>>),
    /// Representation of a byte buffer.
    Byt(Rc<RefCell<Vec<u8>>>),
    /// Integer value
    Int(i64),
    /// Float value
    Flt(f64),
    /// A syntax node in the AST, records the position too.
    Syn(SynPos),
    /// A list (or vector) of VVals.
    Lst(Rc<RefCell<std::vec::Vec<VVal>>>),
    /// A mapping of strings to VVals.
    Map(Rc<RefCell<std::collections::HashMap<String, VVal>>>),
    /// A function, see also [VValFun](struct.VValFun.html)
    Fun(Rc<VValFun>),
    /// A guarded VVal, that executes a given function when it is
    /// no longer referenced.
    DropFun(Rc<DropVVal>),
    /// A (strong) reference to a VVal.
    Ref(Rc<RefCell<VVal>>),
    /// A (strong) reference to a VVal, which is cloned
    /// as weak reference when copied into a `VValFun`
    /// as up value.
    WRef(Rc<RefCell<VVal>>),
    /// A weak reference to a VVal. Might turn VVal::Nul anytime.
    WWRef(Weak<RefCell<VVal>>),
}

impl std::fmt::Debug for VValFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "&VValFun")
    }
}

pub fn format_vval_str(s: &str, narrow_ascii: bool) -> String {
    let mut v : Vec<String> = s.chars().map(|c| {
        match c {
            '\\' => { String::from("\\\\") },
            '\n' => { String::from("\\n")  },
            '\t' => { String::from("\\t")  },
            '\r' => { String::from("\\r")  },
            '\0' => { String::from("\\0")  },
            '\'' => { String::from("\\'")  },
            '\"' => { String::from("\\\"") },
            _ if narrow_ascii
                 && c.is_ascii()
                 && (c.is_ascii_alphanumeric()
                     || c.is_ascii_graphic()
                     || c.is_ascii_punctuation()
                     || c == ' ') => { format!("{}", c) },
            _ if narrow_ascii => { format!("\\x{:02X}", c as u32) },
            _ if !narrow_ascii && c.is_ascii_control() => { format!("\\x{:02x}", c as u32) },
            _ if !narrow_ascii && c.is_control() => { c.escape_unicode().to_string() },
            _ => { format!("{}", c) }

        }
    }).collect();
    v.insert(0, String::from("\""));
    v.push(String::from("\""));
    v.concat()
}

pub fn format_vval_byt(v: &Vec<u8>) -> String {
    let mut s = String::from("");
    for b in v.iter() {
        s.push(*b as char);
    }
    format_vval_str(&s, true)
}

#[allow(dead_code)]
impl VVal {
    pub fn new_str(s: &str) -> VVal {
        VVal::Str(Rc::new(RefCell::new(String::from(s))))
    }

    pub fn new_byt(v: Vec<u8>) -> VVal {
        VVal::Byt(Rc::new(RefCell::new(v)))
    }

    pub fn vec() -> VVal {
        return VVal::Lst(Rc::new(RefCell::new(Vec::new())));
    }

    pub fn vec_from(vl: &[VVal]) -> VVal {
        let mut v = Vec::new();
        v.extend_from_slice(vl);
        v.reverse();
        VVal::Lst(Rc::new(RefCell::new(v)))
    }

    pub fn call(&self, env: &mut Env, argc: usize) -> Result<VVal, StackAction> {
        match self {
            VVal::Fun(fu) => {
                env.with_fun_info(fu.clone(), argc, |e: &mut Env| {
                    (((*fu).fun.borrow()))(e, argc)
                })
            },
            VVal::Bol(b) => {
                let old_bp = env.set_bp(0);
                let ret = if *b {
                    if argc > 0 {
                        let v = env.arg(0).clone();
                        v.call(env, 0)
                    } else { Ok(self.clone()) }
                } else {
                    if argc > 1 {
                        let v = env.arg(1).clone();
                        v.call(env, 0)
                    } else { Ok(self.clone()) }
                };
                env.reset_bp(0, old_bp);
                ret
            },
            VVal::Sym(sym) => {
                let old_bp = env.set_bp(0);
                let ret = if argc > 0 {
                    let v = env.arg(0);
                    match v {
                        VVal::Map(_) =>
                            Ok(v.get_key(&sym).unwrap_or(VVal::Nul)),
                        _ => Ok(VVal::Nul)
                    }
                } else { Ok(self.clone()) };
                env.reset_bp(0, old_bp);
                ret
            },
            VVal::Int(i) => {
                let old_bp = env.set_bp(0);
                let ret = if argc > 0 {
                    let v = env.arg(0);
                    match v {
                        VVal::Lst(_) =>
                            Ok(v.at(*i as usize).unwrap_or(VVal::Nul)),
                        _ => Ok(VVal::Nul)
                    }
                } else { Ok(self.clone()) };
                env.reset_bp(0, old_bp);
                ret
            }
            _ => { Ok(self.clone()) },
        }
    }

    pub fn to_ref(&mut self) -> VVal {
        VVal::Ref(Rc::new(RefCell::new(self.clone())))
    }

    pub fn to_wref(&mut self) -> VVal {
        VVal::WRef(Rc::new(RefCell::new(self.clone())))
    }

    pub fn downgrade(&mut self) -> Option<VVal> {
        if let VVal::WRef(f) = &self {
            Some(VVal::WWRef(Rc::downgrade(f)))
        } else {
            None
        }
    }

    pub fn upgrade(&mut self) -> Option<VVal> {
        if let VVal::WWRef(f) = &self {
            if let Some(r) = Weak::upgrade(f) {
                Some(VVal::WRef(r))
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
            VVal::Flt(ia) => { if let VVal::Flt(ib) = v { return ia == ib; } else { return false; } },
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
            if b.borrow().len() > index {
                return Some(b.borrow()[index].clone());
            }
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

    pub fn set_key(&self, key: &VVal, val: VVal) {
        match self {
            VVal::Map(m) => {
                let ks = key.s_raw();
                m.borrow_mut().insert(ks, val);
            },
            VVal::Lst(l) => {
                let idx = key.i() as usize;
                let mut v = l.borrow_mut();
                if v.len() <= idx {
                    v.resize(idx + 1, VVal::Nul);
                }
                v[idx] = val;
            },
            _ => {}
        }
    }

    pub fn push(&self, val: VVal) -> &VVal {
        //d// println!("FN PUSH {} v {}", self.s(), val.s());
        if let VVal::Lst(b) = &self {
            //d// println!("FN ! PUSH {} v {}", self.s(), val.s());
            b.borrow_mut().push(val);
        }
        self
    }

    pub fn s_raw(&self) -> String {
        match self {
            VVal::Str(s)  => s.borrow().clone(),
            VVal::Sym(s)  => s.clone(),
            _             => String::from(""),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            VVal::Str(_)     => String::from("string"),
            VVal::Byt(_)     => String::from("bytes"),
            VVal::Nul        => String::from("nul"),
            VVal::Bol(_)     => String::from("bool"),
            VVal::Sym(_)     => String::from("sym"),
            VVal::Syn(_)     => String::from("syn"),
            VVal::Int(_)     => String::from("int"),
            VVal::Flt(_)     => String::from("float"),
            VVal::Lst(_)     => String::from("vector"),
            VVal::Map(_)     => String::from("map"),
            VVal::Fun(_)     => String::from("function"),
            VVal::DropFun(_) => String::from("drop_function"),
            VVal::Ref(l)     => (*l).borrow().type_name(),
            VVal::WRef(l)    => (*l).borrow().type_name(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().type_name(),
                    None => String::from("nul"),
                }
            },
        }
    }

    pub fn f(&self) -> f64 {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<f64>().unwrap_or(0.0),
            VVal::Byt(s)     => if (*s).borrow().len() > 0 { (*s).borrow()[0] as f64 } else { 0.0 },
            VVal::Nul        => 0.0,
            VVal::Bol(b)     => if *b { 1.0 } else { 0.0 },
            VVal::Sym(s)     => s.parse::<f64>().unwrap_or(0.0),
            VVal::Syn(s)     => (s.syn.clone() as i64) as f64,
            VVal::Int(i)     => *i as f64,
            VVal::Flt(f)     => *f,
            VVal::Lst(l)     => l.borrow().len() as f64,
            VVal::Map(l)     => l.borrow().len() as f64,
            VVal::Fun(_)     => 1.0,
            VVal::DropFun(f) => f.v.f(),
            VVal::Ref(l)     => (*l).borrow().f(),
            VVal::WRef(l)    => (*l).borrow().f(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().f(),
                    None => 0.0,
                }
            },
        }
    }

    pub fn i(&self) -> i64 {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<i64>().unwrap_or(0),
            VVal::Byt(s)     => if (*s).borrow().len() > 0 { (*s).borrow()[0] as i64 } else { 0 as i64 },
            VVal::Nul        => 0,
            VVal::Bol(b)     => if *b { 1 } else { 0 },
            VVal::Sym(s)     => s.parse::<i64>().unwrap_or(0),
            VVal::Syn(s)     => s.syn.clone() as i64,
            VVal::Int(i)     => *i,
            VVal::Flt(f)     => (*f as i64),
            VVal::Lst(l)     => l.borrow().len() as i64,
            VVal::Map(l)     => l.borrow().len() as i64,
            VVal::Fun(_)     => 1,
            VVal::DropFun(f) => f.v.i(),
            VVal::Ref(l)     => (*l).borrow().i(),
            VVal::WRef(l)    => (*l).borrow().i(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().i(),
                    None => 0,
                }
            },
        }
    }

    pub fn b(&self) -> bool {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<i64>().unwrap_or(0) != 0,
            VVal::Byt(s)     => (if (*s).borrow().len() > 0 { (*s).borrow()[0] as i64 } else { 0 as i64 }) != 0,
            VVal::Nul        => false,
            VVal::Bol(b)     => *b,
            VVal::Sym(s)     => s.parse::<i64>().unwrap_or(0) != 0,
            VVal::Syn(s)     => (s.syn.clone() as i64) != 0,
            VVal::Int(i)     => (*i) != 0,
            VVal::Flt(f)     => (*f as i64) != 0,
            VVal::Lst(l)     => (l.borrow().len() as i64) != 0,
            VVal::Map(l)     => (l.borrow().len() as i64) != 0,
            VVal::Fun(_)     => true,
            VVal::DropFun(f) => f.v.i() != 0,
            VVal::Ref(l)     => (*l).borrow().i() != 0,
            VVal::WRef(l)    => (*l).borrow().i() != 0,
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().i() != 0,
                    None => false,
                }
            },
        }
    }

    pub fn s(&self) -> String {
        match self {
            VVal::Str(s)     => format_vval_str(&s.borrow(), false),
            VVal::Byt(s)     => format!("$b{}", format_vval_byt(&s.borrow())),
            VVal::Nul        => format!("$n"),
            VVal::Bol(b)     => if *b { format!("$true") } else { format!("$false") },
            VVal::Sym(s)     => format!("$\"{}\"", s),
            VVal::Syn(s)     => format!("&{:?}", s.syn),
            VVal::Int(i)     => i.to_string(),
            VVal::Flt(f)     => f.to_string(),
            VVal::Lst(l)     => VVal::dump_vec_as_str(l),
            VVal::Map(l)     => VVal::dump_map_as_str(l), // VVal::dump_map_as_str(l),
            VVal::Fun(_)     => format!("&VValFun"),
            VVal::DropFun(f) => f.v.s(),
            VVal::Ref(l)     => format!("REF[{}]", (*l).borrow().s()),
            VVal::WRef(l)    => format!("wREF[{}]", (*l).borrow().s()),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => format!("WREF[{}]", v.borrow().s()),
                    None => format!("$n"),
                }
            },
        }
    }
}
