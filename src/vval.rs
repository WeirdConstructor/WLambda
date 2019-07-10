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
    Err,
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
#[derive(Debug, Clone)]
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
    /// A user defined variable that holds user context information.
    /// See also the [with_user_do](struct.Env.html#method.with_user_do) function.
    pub user: Rc<RefCell<dyn std::any::Any>>,
}

impl Default for Env {
    fn default() -> Self { Self::new() }
}

impl Env {
    pub fn new() -> Env {
        let mut e = Env {
            args:               Vec::with_capacity(STACK_SIZE),
            fun:                VValFun::new_dummy(),
            bp:                 0,
            sp:                 0,
            argc:               0,
            user:               Rc::new(RefCell::new(VVal::vec())),
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    pub fn new_with_user(user: Rc<RefCell<std::any::Any>>) -> Env {
        let mut e = Env {
            args:               Vec::with_capacity(STACK_SIZE),
            fun:                VValFun::new_dummy(),
            bp:                 0,
            sp:                 0,
            argc:               0,
            user,
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    /// Easier access to the user data field in Env
    ///
    /// In the following example the user supplies a registry
    /// vector for storing VVals. A callback is stored, which is
    /// then later executed.
    ///
    /// ```
    /// use wlambda::prelude::create_wlamba_prelude;
    /// use wlambda::vval::VVal;
    /// use wlambda::vval::Env;
    /// use wlambda::compiler::EvalContext;
    /// use std::rc::Rc;
    /// use std::cell::RefCell;
    ///
    /// let global = create_wlamba_prelude();
    ///
    /// global.borrow_mut().add_func("reg", |env: &mut Env, _argc: usize| {
    ///     let fun = env.arg(0);
    ///     env.with_user_do(|v: &mut Vec<VVal>| v.push(fun.clone()));
    ///     Ok(VVal::Nul)
    /// });
    ///
    /// let reg : Rc<RefCell<Vec<VVal>>> =
    ///     Rc::new(RefCell::new(Vec::new()));
    ///
    /// let mut ctx = EvalContext::new_with_user(global, reg.clone());
    /// ctx.eval("reg { _ + 10 }").unwrap();
    ///
    /// let n = reg.borrow_mut()[0].clone();
    /// let ret = ctx.call(&n, &vec![VVal::Int(11)]).unwrap();
    /// assert_eq!(ret.i(), 21);
    /// ```
    pub fn with_user_do<T: 'static, F, X>(&mut self, f: F) -> X
        where F: Fn(&mut T) -> X {
        let mut any = self.user.borrow_mut();
        let ref_reg = any.downcast_mut::<T>().unwrap();
        f(ref_reg)
    }

    pub fn set_bp(&mut self, env_size: usize) -> usize {
        let new_bp = self.sp;
        self.sp += env_size;
        std::mem::replace(&mut self.bp, new_bp)
    }

    pub fn reset_bp(&mut self, env_size: usize, oldbp: usize) {
        for i in self.bp..self.sp {
            self.args[i] = VVal::Nul;
        }
        self.sp -= env_size;
        self.bp = oldbp;
    }

    pub fn with_local_call_info<T>(&mut self, argc: usize, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {
        let local_size = 0;
        let old_argc = std::mem::replace(&mut self.argc, argc);
        let old_bp   = self.set_bp(local_size);

        let ret = f(self);

        self.reset_bp(local_size, old_bp);
        self.argc = old_argc;

        ret
    }

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

    pub fn with_restore_sp<T>(&mut self, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {

        let old_sp = self.sp;
        let ret = f(self);
        self.popn(self.sp - old_sp);
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
        self.sp += n;
        //d// println!("PUSH_SP {} => {}", n, self.sp);
    }

    pub fn push(&mut self, v: VVal) -> usize {
        self.args[self.sp] = v;
        self.sp += 1;
        self.sp - 1
    }

    pub fn popn(&mut self, n: usize) {
        if self.sp < n {
            panic!(format!("Stack pointer underflow {} {}", self.sp, n));
        }
        if n > 0 {
            for i in (self.sp - 1)..=(self.sp - n) {
                //d// println!("POP[{}] {}", i, self.args[i].s());
                self.args[i] = VVal::Nul;
            }
        }
        self.sp -= n;
        //d// println!("POPN {} => {}", n, self.sp);
    }

    /// Prints a dump of the stack state.
    pub fn dump_stack(&self) {
        println!("* SP={}, BP={}", self.sp, self.bp);
        for (i, v) in self.args.iter().enumerate() {
            let mut mark = String::from("");
            if i == self.bp { mark = format!("{} BP", mark); }
            if i == self.sp { mark = format!("{} SP", mark); }
            if !mark.is_empty() { mark = format!("{} ->", mark); }

            println!("    {:9} [{:3}] = {}", mark, i, v.s());
            if i >= self.sp { break; }
        }
        for (i, u) in self.fun.upvalues.iter().enumerate() {
            println!("  UP[{:3}] = {}", i, u.s());
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
        //d// println!("GET ARGC [{}] = {}", i, self.argc);
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
        //d// println!("SET_LOCAL [{}] {} =~> {} ({})", self.sp, i, idx, self.args[idx].s());
        //d// self.dump_stack();
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
    Panic(VVal),
    Return((VVal, VVal)),
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
    pub fn new_val(fun: ClosNodeRef, upvalues: std::vec::Vec<VVal>, env_size: usize) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalues,
            fun,
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
        self.fun.call_internal(&mut e, 1);
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
    /// The err value is a special sentinel value for representing any kind of
    /// application error condition. It's created using the special $e <expr> or $error <expr>
    /// syntax.
    Err(Rc<RefCell<VVal>>),
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

pub fn format_vval_byt(v: &[u8]) -> String {
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

    pub fn new_sym(s: &str) -> VVal {
        VVal::Sym(String::from(s))
    }

    pub fn new_byt(v: Vec<u8>) -> VVal {
        VVal::Byt(Rc::new(RefCell::new(v)))
    }

    pub fn err(v: VVal) -> VVal {
        VVal::Err(Rc::new(RefCell::new(v)))
    }

    pub fn vec() -> VVal {
        VVal::Lst(Rc::new(RefCell::new(Vec::new())))
    }

    pub fn vec_from(vl: &[VVal]) -> VVal {
        let mut v = Vec::new();
        v.extend_from_slice(vl);
        v.reverse();
        VVal::Lst(Rc::new(RefCell::new(v)))
    }

    pub fn call(&self, env: &mut Env, args: &[VVal]) -> Result<VVal, StackAction> {
        let argc = args.len();
        env.with_pushed_sp(argc, |e: &mut Env| {
            for (i, v) in args.iter().enumerate() {
                e.set_arg(i, v.clone());
            }
            self.call_internal(e, argc)
        })
    }

    pub fn call_no_args(&self, env: &mut Env) -> Result<VVal, StackAction> {
        self.call_internal(env, 0)
    }

    pub fn call_internal(&self, env: &mut Env, argc: usize) -> Result<VVal, StackAction> {
        //d// env.dump_stack();
        match self {
            VVal::Fun(fu) => {
                env.with_fun_info(fu.clone(), argc, |e: &mut Env| {
                    ((*fu).fun.borrow())(e, argc)
                })
            },
            VVal::Bol(b) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let idx = if *b { 0 } else { 1 };
                    let ret = if argc > 0 {
                        let v = e.arg(idx).clone();
                        v.call_internal(e, 0)
                    } else { Ok(self.clone()) };
                    ret
                })
            },
            VVal::Err(e) => {
                Err(StackAction::Panic(
                    VVal::new_str(
                        &format!("Called an error value: {}", e.borrow().s()))))
            },
            VVal::Sym(sym) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let ret = if argc > 0 {
                        let v = e.arg(0);
                        match v {
                            VVal::Map(_) =>
                                Ok(v.get_key(&sym).unwrap_or(VVal::Nul)),
                            _ => Ok(VVal::Nul)
                        }
                    } else { Ok(self.clone()) };
                    ret
                })
            },
            VVal::Str(s) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let ret = if argc > 0 {
                        let v = e.arg(0);
                        match v {
                            VVal::Int(i) => {
                                if argc > 1 {
                                    let from = i as usize;
                                    let cnt  = e.arg(1).i() as usize;
                                    let r : String = s.borrow().chars().skip(from).take(cnt).collect();
                                    Ok(VVal::new_str(&r))
                                } else {
                                    let r = s.borrow().chars().nth(i as usize);
                                    match r {
                                        None    => Ok(VVal::Nul),
                                        Some(c) => {
                                            let mut b = [0; 4];
                                            Ok(VVal::new_str(c.encode_utf8(&mut b)))
                                        },
                                    }
                                }
                            },
                            VVal::Lst(_) => {
                                let from = v.at(0).unwrap_or(VVal::Int(0)).i() as usize;
                                let cnt  = v.at(1).unwrap_or(VVal::Int((s.borrow().len() - from) as i64)).i() as usize;
                                let r : String = s.borrow().chars().skip(from).take(cnt).collect();
                                Ok(VVal::new_str(&r))
                            },
                            VVal::Str(s2) => {
                                if argc > 1 {
                                    // TODO: Fix the extra clone here:
                                    let mut accum = s.borrow().clone() + &s2.borrow().clone();
                                    for i in 2..argc {
                                        accum += &e.arg(i).s_raw();
                                    }
                                    Ok(VVal::new_str(&accum))
                                } else {
                                    // TODO: Fix the extra clone here:
                                    Ok(VVal::new_str(&(s.borrow().clone() + &s2.borrow().clone())))
                                }
                            },
                            VVal::Map(_) => Ok(v.get_key(&s.borrow()).unwrap_or(VVal::Nul)),
                            _ => Ok(VVal::Nul)
                        }
                    } else { Ok(self.clone()) };
                    ret
                })
            },
            VVal::Int(i) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let ret = if argc > 0 {
                        let v = e.arg(0);
                        match v {
                            VVal::Lst(_) =>
                                Ok(v.at(*i as usize).unwrap_or(VVal::Nul)),
                            _ => Ok(VVal::Nul)
                        }
                    } else { Ok(self.clone()) };
                    ret
                })
            }
            _ => { Ok(self.clone()) },
        }
    }

    pub fn to_ref(&self) -> VVal {
        VVal::Ref(Rc::new(RefCell::new(self.clone())))
    }

    pub fn to_wref(&self) -> VVal {
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
        VVal::Map(Rc::new(RefCell::new(std::collections::HashMap::new())))
    }

    pub fn sym(s: &str) -> VVal {
        VVal::Sym(String::from(s))
    }

    pub fn eqv(&self, v: &VVal) -> bool {
        match self {
            VVal::Nul     => { if let VVal::Nul = v { return true; } else { return false; } },
            VVal::Int(ia) => { if let VVal::Int(ib) = v { return ia == ib; } else { return false; } },
            VVal::Flt(ia) => { if let VVal::Flt(ib) = v { return (ia - ib).abs() < std::f64::EPSILON; } else { return false; } },
            VVal::Sym(s)  => { if let VVal::Sym(ib) = v { return *s == *ib; } else { return false; } },
            VVal::Str(_)  => { self.s_raw() == v.s_raw() },
            _             => { false }
        }
    }

    pub fn dump_vec_as_str(v: &Rc<RefCell<std::vec::Vec<VVal>>>) -> String {
        let mut out : Vec<String> = Vec::new();
        let mut first = true;
        out.push(String::from("$["));
        for s in v.borrow().iter().map(VVal::s) {
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
        out.push(String::from("${"));
        let hm = m.borrow();

        let mut keys : Vec<&String> = hm.keys().collect();
        keys.sort();
        for k in keys {
            let v = hm.get(k).unwrap();
            if !first { out.push(String::from(",")); }
            else { first = false; }
            if k.chars().any(char::is_whitespace) {
                out.push(format!("\"{}\"", k));
            } else {
                out.push(k.to_string());
            }
            out.push(String::from("="));
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
            _             => self.s(),
        }
    }

    pub fn is_float(&self) -> bool {
        match self { VVal::Flt(_) => true, _ => false }
    }

    pub fn is_int(&self) -> bool {
        match self { VVal::Int(_) => true, _ => false }
    }

    pub fn is_sym(&self) -> bool {
        match self { VVal::Sym(_) => true, _ => false }
    }

    pub fn is_str(&self) -> bool {
        match self { VVal::Str(_) => true, _ => false }
    }

    pub fn is_fun(&self) -> bool {
        match self { VVal::Fun(_) => true, _ => false }
    }

    pub fn is_vec(&self) -> bool {
        match self { VVal::Lst(_) => true, _ => false }
    }

    pub fn is_map(&self) -> bool {
        match self { VVal::Map(_) => true, _ => false }
    }

    pub fn is_nul(&self) -> bool {
        match self { VVal::Nul => true, _ => false }
    }

    pub fn is_err(&self) -> bool {
        match self { VVal::Err(_) => true, _ => false }
    }

    pub fn type_name(&self) -> String {
        match self {
            VVal::Str(_)     => String::from("string"),
            VVal::Byt(_)     => String::from("bytes"),
            VVal::Nul        => String::from("nul"),
            VVal::Err(_)     => String::from("err"),
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

    #[allow(clippy::cast_lossless)]
    pub fn f(&self) -> f64 {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<f64>().unwrap_or(0.0),
            VVal::Byt(s)     => if (*s).borrow().len() > 0 { (*s).borrow()[0] as f64 } else { 0.0 },
            VVal::Nul        => 0.0,
            VVal::Err(_)     => 0.0,
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

    #[allow(clippy::cast_lossless)]
    pub fn i(&self) -> i64 {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<i64>().unwrap_or(0),
            VVal::Byt(s)     => if (*s).borrow().len() > 0 { (*s).borrow()[0] as i64 } else { 0 as i64 },
            VVal::Nul        => 0,
            VVal::Err(_)     => 0,
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

    #[allow(clippy::cast_lossless)]
    pub fn b(&self) -> bool {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<i64>().unwrap_or(0) != 0,
            VVal::Byt(s)     => (if (*s).borrow().len() > 0 { (*s).borrow()[0] as i64 } else { 0 as i64 }) != 0,
            VVal::Nul        => false,
            VVal::Err(_)     => false,
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
            VVal::Nul        => "$n".to_string(),
            VVal::Err(e)     => format!("$e {}", (*e).borrow().s()),
            VVal::Bol(b)     => if *b { "$true".to_string() } else { "$false".to_string() },
            VVal::Sym(s)     => format!(":\"{}\"", s),
            VVal::Syn(s)     => format!("&{:?}", s.syn),
            VVal::Int(i)     => i.to_string(),
            VVal::Flt(f)     => f.to_string(),
            VVal::Lst(l)     => VVal::dump_vec_as_str(l),
            VVal::Map(l)     => VVal::dump_map_as_str(l), // VVal::dump_map_as_str(l),
            VVal::Fun(_)     => "&VValFun".to_string(),
            VVal::DropFun(f) => f.v.s(),
            VVal::Ref(l)     => format!("REF[{}]", (*l).borrow().s()),
            VVal::WRef(l)    => format!("wREF[{}]", (*l).borrow().s()),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => format!("WREF[{}]", v.borrow().s()),
                    None => "$n".to_string(),
                }
            },
        }
    }
}
