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
use std::fmt::{Display, Formatter};

/// Structure for holding information about origin
/// of an AST node.
#[derive(Debug, Clone, PartialEq)]
pub struct SynPos {
    pub syn:        Syntax,
    pub line:       u32,
    pub col:        u32,
    pub file:       u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    pub pos:    SynPos,
    pub msg:    String,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "[{},{}:{}] Compilation Error: {}",
            self.pos.line, self.pos.col, self.pos.file, self.msg)
    }
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
    Apply,
    And,
    Or,
    Assign,
    Def,
    Ref,
    WRef,
    Deref,
    AssignRef,
    DefGlobRef,
    Import,
    Export,
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
    /// The exported names of this module.
    pub exports: std::collections::HashMap<String, VVal>,
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
            exports:            std::collections::HashMap::new(),
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
            exports:            std::collections::HashMap::new(),
            user,
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    /// Returns the passed in user context value.
    pub fn get_user(&self) -> Rc<RefCell<std::any::Any>> {
        self.user.clone()
    }

    /// Easier access to the user data field in Env
    ///
    /// In the following example the user supplies a registry
    /// vector for storing VVals. A callback is stored, which is
    /// then later executed.
    ///
    /// ```
    /// use wlambda::{VVal, EvalContext, GlobalEnv};
    /// use wlambda::vval::Env;
    /// use std::rc::Rc;
    /// use std::cell::RefCell;
    ///
    /// let global = GlobalEnv::new_default();
    ///
    /// global.borrow_mut().add_func("reg", |env: &mut Env, _argc: usize| {
    ///     let fun = env.arg(0);
    ///     env.with_user_do(|v: &mut Vec<VVal>| v.push(fun.clone()));
    ///     Ok(VVal::Nul)
    /// }, Some(1), Some(1));
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
    #[allow(dead_code)]
    pub fn with_user_do<T: 'static, F, X>(&mut self, f: F) -> X
        where F: Fn(&mut T) -> X {
        let mut any = self.user.borrow_mut();
        let ref_reg = any.downcast_mut::<T>().unwrap();
        f(ref_reg)
    }

    pub fn export_name(&mut self, name: &str, value: &VVal) {
        self.exports.insert(String::from(name), value.clone());
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
        //d// println!("OLD FUN: {:?}", old_fun.upvalues);

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
        VVal::vec_from(&self.args[(self.bp - self.argc)..self.bp])
    }

    pub fn get_up_raw(&mut self, i: usize) -> VVal {
        //d// println!("GET UP {}: {:?}", i, self.fun.upvalues);
        self.fun.upvalues[i].clone()
    }

    pub fn get_up(&mut self, i: usize) -> VVal {
        self.fun.upvalues[i].deref()
//        match self.fun.upvalues[i].deref() {
//            VVal::WWRef(l) => { 
//                match l.upgrade() {
//                    Some(v) => v.borrow().clone(),
//                    None => VVal::Nul,
//                }
//            },
//            v => v,
//        }
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
            VVal::DropFun(r) => r.v.clone(),
            v                => v.clone(),
        }
    }

    pub fn get_local(&mut self, i: usize) -> VVal {
        let idx = self.bp + i;
        self.args[idx].clone()
    }

    pub fn set_up(&mut self, index: usize, value: &VVal) {
        let fun = self.fun.clone();
        let upv = &fun.upvalues[index];

        match upv {
            VVal::Ref(r)   => { r.replace(value.clone()); }
            VVal::CRef(r)  => { r.replace(value.clone()); }
            VVal::WWRef(r) => {
                if let Some(r) = Weak::upgrade(r) {
                    r.replace(value.clone());
                }
            },
            _ => {}
        }
    }

    pub fn set_local(&mut self, i: usize, value: &VVal) {
        let idx = self.bp + i;
        self.args[idx] = value.clone();
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
    Panic(VVal, Option<Vec<SynPos>>),
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
    /// Min number of arguments this functions requires.
    pub min_args:   Option<usize>,
    /// Max number of arguments this functions requires.
    pub max_args:   Option<usize>,
    /// The location of the definition of this function.
    pub syn_pos:    Option<SynPos>,
}

impl VValFun {
    /// Creates a new VVal containing the given closure with the given minimum
    /// and maximum parameters (see also [`add_func` of GlobalEnv](compiler/struct.GlobalEnv.html#method.add_func)).
    ///
    /// This is usually useful if you want to add functions to the [EvalContext](compiler/struct.EvalContext.html).
    /// at runtime.
    ///
    ///```rust
    /// use wlambda::compiler::EvalContext;
    /// use wlambda::vval::{VVal, VValFun, Env};
    ///
    /// let mut ctx = wlambda::compiler::EvalContext::new_empty_global_env();
    ///
    /// ctx.set_global_var("xyz",
    ///     &VValFun::new_fun(
    ///         move |env: &mut Env, argc: usize| {
    ///             Ok(VVal::new_str("xyz"))
    ///         }, None, None));
    ///
    /// assert_eq!(ctx.eval("xyz[]").unwrap().s_raw(), "xyz")
    ///```
    pub fn new_fun<T>(fun: T, min_args: Option<usize>, max_args: Option<usize>) -> VVal
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction> {

        VValFun::new_val(Rc::new(RefCell::new(fun)), Vec::new(), 0, min_args, max_args, None)
    }

    pub fn new_fun_with_pos<T>(fun: T, min_args: Option<usize>, max_args: Option<usize>, spos: SynPos) -> VVal
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction> {

        VValFun::new_val(Rc::new(RefCell::new(fun)), Vec::new(), 0, min_args, max_args, Some(spos))
    }

    /// Internal utility function. Use at your own risk, API might change.
    pub fn new_val(fun: ClosNodeRef, upvalues: std::vec::Vec<VVal>,
                   env_size: usize, min_args: Option<usize>,
                   max_args: Option<usize>, syn_pos: Option<SynPos>) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalues,
            fun,
            local_size: env_size,
            min_args,
            max_args,
            syn_pos,
        }))
    }

    /// Returns a dummy function that does nothing.
    pub fn new_dummy() -> Rc<VValFun> {
        Rc::new(VValFun {
            fun:        Rc::new(RefCell::new(|_: &mut Env, _a: usize| { Ok(VVal::Nul) })),
            upvalues:   Vec::new(),
            local_size: 0,
            min_args: None,
            max_args: None,
            syn_pos: None,
        })
    }

    /// Dumps captured up values of this function. Useful only if you want to
    /// debug functions/closures creates by WLambda code.
    pub fn dump_upvals(&self) -> VVal {
        let v = VVal::vec();
        for uv in self.upvalues.iter() {
            v.push(VVal::new_str_mv(uv.s()));
        }
        v
    }
}

/// You can implement your own VVal data type and provide it
/// via global functions:
///
///```
/// use std::rc::Rc;
/// use std::cell::RefCell;
/// use wlambda::vval::Env;
/// use wlambda::{VVal, GlobalEnv};
///
/// #[derive(Clone, Debug)]
/// struct MyType {
///     x: Rc<RefCell<(i64, i64)>>,
/// }
///
/// impl wlambda::vval::VValUserData for MyType {
///     fn s(&self) -> String { format!("$<MyType({:?})>", self.x.borrow()) }
///     fn i(&self) -> i64    { self.x.borrow_mut().1 }
///     fn as_any(&mut self) -> &mut dyn std::any::Any { self }
///     fn clone_ud(&self) -> Box<dyn wlambda::vval::VValUserData> {
///         Box::new(self.clone())
///     }
/// }
///
/// let global_env = GlobalEnv::new_default();
/// global_env.borrow_mut().add_func(
///     "new_mytype",
///     |_env: &mut Env, _argc: usize| {
///         Ok(VVal::Usr(Box::new(MyType { x: Rc::new(RefCell::new((13, 42))) })))
///     }, Some(0), Some(0));
///
/// global_env.borrow_mut().add_func(
///     "modify_mytype",
///     |env: &mut Env, _argc: usize| {
///         Ok(if let VVal::Usr(mut u) = env.arg(0) {
///             if let Some(ud) = u.as_any().downcast_mut::<MyType>() {
///                 ud.x.borrow_mut().0 += 1;
///                 ud.x.borrow_mut().1 *= 2;
///                 VVal::Int(ud.x.borrow().0 + ud.x.borrow().1)
///             } else {
///                 VVal::Nul
///             }
///         } else { VVal::Nul })
///     }, Some(1), Some(1));
///
/// let mut ctx = wlambda::compiler::EvalContext::new(global_env);
///
/// let r = &mut ctx.eval(r#"
///     !x = new_mytype[];
///     !i = modify_mytype x;
///     $[i, x]
/// "#).unwrap();
///
/// assert_eq!(
///     r.s(), "$[98,$<MyType((14, 84))>]", "Userdata implementation works");
///```
#[allow(clippy::borrowed_box)]
pub trait VValUserData {
    /// This method should return a human readable syntax representation
    /// of your VValUserData.
    fn s(&self)     -> String { format!("$<userdata:{:p}>", self) }
    /// If your data has a plain string representation,
    /// you can return the string directly from here.
    fn s_raw(&self) -> String { self.s() }
    /// Returns the i64 representation of your data.
    fn i(&self)     -> i64    { -1 }
    /// Returns the f64 representation of your data.
    fn f(&self)     -> f64    { self.i() as f64 }
    /// Returns the boolean representation of your data. Can for instance
    /// be used to check if your data is _valid_ or something.
    fn b(&self)     -> bool   { true }
    /// Allows you to specify how two instances of your data
    /// should be compared for equivalentness.
    fn eqv(&self, _other: &Box<dyn VValUserData>) -> bool { false }
    /// Should clone your user data instance. Whether you are doing
    /// a deep clone or a shallow cloen or something else is up to you.
    fn clone_ud(&self) -> Box<dyn VValUserData>;
    /// Makes your user data act like a map. This can be useful
    /// for implementing your own registries or data structures.
    /// Implement this method for setting a key to a value.
    fn set_key(&self, _key: &VVal, _val: VVal) { }
    /// This method returns some value that your user data
    /// associates with the given key.
    fn get_key(&self, _key: &str) -> Option<VVal> { None }
    /// This should be implemented simply by returning
    /// a mutable reference to the concrete type self.
    /// It allows you to access your data structure from inside
    /// a function yourself.
    ///
    /// This is a good default implementation for your struct/type:
    ///
    ///```rust,no_run,compile_fail
    /// fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    ///```
    fn as_any(&mut self) -> &mut dyn std::any::Any;
}

impl std::fmt::Debug for VValUserData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.s())
    }
}

impl std::clone::Clone for Box<dyn VValUserData> {
    fn clone(&self) -> Self {
        (**self).clone_ud()
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
    /// The none value, the default value of all non initialized data.
    Nul,
    /// The err value is a special sentinel value for representing any kind of
    /// application error condition. It's created using the special $e <expr> or $error <expr>
    /// syntax.
    Err(Rc<RefCell<(VVal, SynPos)>>),
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
    /// A (still strong) reference to a VVal, which becomes a weak reference if
    /// captured by a closure.
    CRef(Rc<RefCell<VVal>>),
    /// A (weak) reference to a VVal. Might turn VVal::Nul anytime.
    WWRef(Weak<RefCell<VVal>>),
    /// A vval that can box some user data which can later be accessed
    /// from inside user supplied Rust functions via std::any::Any.
    Usr(Box<dyn VValUserData>),
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

    pub fn new_str_mv(s: String) -> VVal {
        VVal::Str(Rc::new(RefCell::new(s)))
    }

    pub fn new_sym(s: &str) -> VVal {
        VVal::Sym(String::from(s))
    }

    pub fn new_byt(v: Vec<u8>) -> VVal {
        VVal::Byt(Rc::new(RefCell::new(v)))
    }

    pub fn err(v: VVal, pos: SynPos) -> VVal {
        VVal::Err(Rc::new(RefCell::new((v, pos))))
    }

    pub fn err_msg(s: &str) -> VVal {
        VVal::Err(Rc::new(RefCell::new(
            (VVal::new_str(s),
             SynPos { syn: Syntax::Block, line: 0, col: 0, file: 0 }))))
    }

    pub fn vec() -> VVal {
        VVal::Lst(Rc::new(RefCell::new(Vec::new())))
    }

    pub fn to_vec(&self) -> Vec<VVal> {
        if let VVal::Lst(l) = self {
            let r : Vec<VVal> = l.borrow_mut().iter().cloned().collect();
            r
        } else {
            vec![self.clone()]
        }
    }

    pub fn vec_from(vl: &[VVal]) -> VVal {
        let mut v = Vec::new();
        v.extend_from_slice(vl);
        v.reverse();
        VVal::Lst(Rc::new(RefCell::new(v)))
    }

    pub fn vec_mv(v: Vec<VVal>) -> VVal {
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
                if let Some(i) = fu.min_args {
                    if argc < i {
                        let line = if let Some(sp) = &fu.syn_pos { sp.line } else { 0 };
                        let col  = if let Some(sp) = &fu.syn_pos { sp.col } else { 0 };
                        let file = if let Some(sp) = &fu.syn_pos { sp.file } else { 0 };
                        return Err(StackAction::Panic(
                            VVal::new_str_mv(format!(
                                "function[{}:{}/{}] expects at least {} arguments, got {}",
                                line, col, file,
                                i, argc)), None));
                    }
                }

                if let Some(i) = fu.max_args {
                    if argc > i {
                        let line = if let Some(sp) = &fu.syn_pos { sp.line } else { 0 };
                        let col  = if let Some(sp) = &fu.syn_pos { sp.col } else { 0 };
                        let file = if let Some(sp) = &fu.syn_pos { sp.file } else { 0 };
                        return Err(StackAction::Panic(
                            VVal::new_str_mv(format!(
                                "function[{}:{}/{}] expects at most {} arguments, got {}",
                                line, col, file,
                                i, argc)), None));
                    }
                }

                env.with_fun_info(fu.clone(), argc, |e: &mut Env| {
                    ((*fu).fun.borrow())(e, argc)
                })
            },
            VVal::Bol(b) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let idx = if *b { 0 } else { 1 };
                    if argc > 0 {
                        let v = e.arg(idx).clone();
                        v.call_internal(e, 0)
                    } else { Ok(self.clone()) }
                })
            },
            VVal::Err(e) => {
                Err(StackAction::Panic(
                    VVal::new_str_mv(
                        format!("Called an error value: {}",
                                 e.borrow().0.s())), None))
            },
            VVal::Sym(sym) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    if argc > 0 {
                        let v = e.arg(0);
                        match v {
                            VVal::Ref(_) | VVal::CRef(_) | VVal::WWRef(_) | VVal::Map(_) =>
                                Ok(v.get_key(&sym).unwrap_or(VVal::Nul)),
                            _ => Ok(VVal::Nul)
                        }
                    } else { Ok(self.clone()) }
                })
            },
            VVal::Lst(l) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let f = e.arg(0);

                    let ret = VVal::vec();
                    for i in l.borrow_mut().iter() {
                        e.push(i.clone());
                        let el = f.call_internal(e, 1);
                        e.popn(1);

                        match el {
                            Ok(v)                      => { ret.push(v); },
                            Err(StackAction::Break(v)) => { ret.push(v); break; },
                            Err(StackAction::Next)     => { },
                            Err(e)                     => { return Err(e); },
                        }
                    }
                    Ok(ret)
                })
            },
            VVal::Byt(vval_bytes) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    if argc > 0 {
                        let first_arg = e.arg(0);
                        match first_arg {
                            VVal::Int(arg_int) => {
                                if argc > 1 {
                                    let from = arg_int as usize;
                                    let cnt  = e.arg(1).i() as usize;
                                    let r : Vec<u8> = vval_bytes.borrow().iter().skip(from).take(cnt).copied().collect();
                                    Ok(VVal::new_byt(r))
                                } else {
                                    let r = vval_bytes.borrow();
                                    if arg_int as usize >= r.len() {
                                        Ok(VVal::Nul)
                                    } else {
                                        Ok(VVal::new_byt(vec![r[arg_int as usize]]))
                                    }
                                }
                            },
                            VVal::Lst(_) => {
                                let from = first_arg.at(0).unwrap_or(VVal::Int(0)).i() as usize;
                                let cnt  = first_arg.at(1).unwrap_or_else(|| VVal::Int((vval_bytes.borrow().len() - from) as i64)).i() as usize;
                                let r : Vec<u8> = vval_bytes.borrow().iter().skip(from).take(cnt).copied().collect();
                                Ok(VVal::new_byt(r))
                            },
                            VVal::Map(_) => Ok(first_arg.get_key(&self.s_raw()).unwrap_or(VVal::Nul)),
                            _ => Ok(VVal::Nul)
                        }
                    } else { Ok(self.clone()) }
                })
            },
            VVal::Str(vval_str) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    if argc > 0 {
                        let first_arg = e.arg(0);
                        match first_arg {
                            VVal::Int(arg_int) => {
                                if argc > 1 {
                                    let from = arg_int as usize;
                                    let cnt  = e.arg(1).i() as usize;
                                    let r : String = vval_str.borrow().chars().skip(from).take(cnt).collect();
                                    Ok(VVal::new_str_mv(r))
                                } else {
                                    let r = vval_str.borrow().chars().nth(arg_int as usize);
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
                                let from = first_arg.at(0).unwrap_or(VVal::Int(0)).i() as usize;
                                let cnt  = first_arg.at(1).unwrap_or_else(|| VVal::Int((vval_str.borrow().len() - from) as i64)).i() as usize;
                                let r : String = vval_str.borrow().chars().skip(from).take(cnt).collect();
                                Ok(VVal::new_str_mv(r))
                            },
                            VVal::Str(s2) => {
                                if argc > 1 {
                                    // TODO: Fix the extra clone here:
                                    let mut accum = vval_str.borrow().clone() + &s2.borrow().clone();
                                    for i in 2..argc {
                                        accum += &e.arg(i).s_raw();
                                    }
                                    Ok(VVal::new_str_mv(accum))
                                } else {
                                    // TODO: Fix the extra clone here:
                                    Ok(VVal::new_str_mv(vval_str.borrow().clone() + &s2.borrow().clone()))
                                }
                            },
                            VVal::Map(_) => Ok(first_arg.get_key(&vval_str.borrow()).unwrap_or(VVal::Nul)),
                            _ => Ok(VVal::Nul)
                        }
                    } else { Ok(self.clone()) }
                })
            },
            VVal::Int(i) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    if argc > 0 {
                        let v = e.arg(0);
                        match v {
                            VVal::Lst(_) =>
                                Ok(v.at(*i as usize).unwrap_or(VVal::Nul)),
                            _ => Ok(v.get_key(&format!("{}", *i))
                                     .unwrap_or(VVal::Nul)),
                        }
                    } else { Ok(self.clone()) }
                })
            }
            _ => { Ok(self.clone()) },
        }
    }

    pub fn to_ref(&self) -> VVal {
        VVal::Ref(Rc::new(RefCell::new(self.clone())))
    }

    pub fn to_wref(&self) -> VVal {
        VVal::CRef(Rc::new(RefCell::new(self.clone())))
    }

    pub fn set_ref(&self, v: VVal) -> VVal {
        match self {
            VVal::Ref(r)     => r.replace(v),
            VVal::CRef(r)     => r.replace(v),
            VVal::WWRef(l)   => {
                if let Some(r) = l.upgrade() {
                    r.replace(v)
                } else {
                    VVal::Nul
                }
            },
            _ => VVal::Nul
        }
    }

    pub fn deref(&self) -> VVal {
        match self {
            VVal::Ref(l)     => (*l).borrow().clone(),
            VVal::CRef(l)     => (*l).borrow().clone(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().clone(),
                    None => VVal::Nul,
                }
            },
            _ => self.clone()
        }
    }

    pub fn downgrade(self) -> VVal {
        match self {
            VVal::Ref(f)  => VVal::WWRef(Rc::downgrade(&f)),
            VVal::CRef(f) => VVal::WWRef(Rc::downgrade(&f)),
            _ => self,
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
            VVal::Byt(s)  => { if let VVal::Byt(s2) = v { s.borrow()[..] == s2.borrow()[..] } else { false } },
            VVal::Usr(u)  => {
                if let VVal::Usr(u2) = v {
                    u.eqv(&u2)
                } else {
                    false
                }
            }
            _             => false,
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

    pub fn map_ok_skip<T, R>(&self, mut op: T, skip: usize) -> Vec<R>
        where T: FnMut(&VVal) -> R {

        let mut res : Vec<R> = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow_mut().iter().skip(skip) {
                res.push(op(i));
            }
        }
        res
    }

    pub fn map_skip<R, E, T>(&self, mut op: T, skip: usize) -> Result<Vec<R>, E>
        where T: FnMut(&VVal) -> Result<R, E> {

        let mut res : Vec<R> = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow_mut().iter().skip(skip) {
                res.push(op(i)?);
            }
        }
        Ok(res)
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
        match self {
            VVal::Ref(_)   => self.deref().get_key(key),
            VVal::CRef(_)  => self.deref().get_key(key),
            VVal::WWRef(_) => self.deref().get_key(key),
            VVal::Map(m) => {
                m.borrow().get(&String::from(key)).cloned()
            },
            VVal::Lst(l) => {
                let idx = usize::from_str_radix(key, 10).unwrap_or(0);
                if idx < l.borrow().len() {
                    Some(l.borrow()[idx].clone())
                } else {
                    Some(VVal::Nul)
                }
            },
            VVal::Usr(u) => u.get_key(key),
            _ => None
        }
    }

    pub fn set_map_key(&self, key: String, val: VVal) {
        match self {
            VVal::Ref(_)   => self.deref().set_map_key(key, val),
            VVal::CRef(_)  => self.deref().set_map_key(key, val),
            VVal::WWRef(_) => self.deref().set_map_key(key, val),
            VVal::Map(m)   => { m.borrow_mut().insert(key, val); },
            _ => (),
        }
    }

    pub fn set_key(&self, key: &VVal, val: VVal) {
        match self {
            VVal::Ref(_)   => self.deref().set_key(key, val),
            VVal::CRef(_)  => self.deref().set_key(key, val),
            VVal::WWRef(_) => self.deref().set_key(key, val),
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
            VVal::Usr(u) => { u.set_key(key, val); },
            _ => {}
        }
    }

    pub fn pop(&self) -> VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().pop().unwrap_or(VVal::Nul)
        } else {
            VVal::Nul
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

    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn len(&self) -> usize {
        match self {
            VVal::Lst(l) => l.borrow().len(),
            VVal::Map(l) => l.borrow().len(),
            VVal::Byt(l) => l.borrow().len(),
            VVal::Str(l) => l.borrow().len(),
            VVal::Sym(l) => l.len(),
            _ => 0,
        }
    }

    pub fn s_len(&self) -> usize {
        match self {
            VVal::Str(s)  => s.borrow().chars().count(),
            VVal::Sym(s)  => s.chars().count(),
            VVal::Usr(s)  => s.s_raw().chars().count(),
            VVal::Byt(b)  => b.borrow().len(),
            _             => self.s().chars().count(),
        }
    }

    pub fn s_raw(&self) -> String {
        match self {
            VVal::Str(s)  => s.borrow().clone(),
            VVal::Sym(s)  => s.clone(),
            VVal::Usr(s)  => s.s_raw(),
            VVal::Byt(s)  => s.borrow().iter().map(|b| *b as char).collect(),
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

    pub fn is_syn(&self) -> bool {
        match self { VVal::Syn(_) => true, _ => false }
    }

    pub fn get_syn_pos(&self) -> SynPos {
        if let VVal::Syn(s) = self {
            s.clone()
        } else {
            SynPos {
                syn: Syntax::Block,
                line: 0, col: 0, file: 0
            }
        }
    }

    pub fn get_syn(&self) -> Syntax {
        if let VVal::Syn(s) = self {
            s.syn.clone()
        } else {
            Syntax::Block
        }
    }

//    pub fn to_eval_panic(&self, msg: String) -> Result<VVal, StackAction> {
//        Err(StackAction::Panic( {
//            msg,
//            pos: self.get_syn_pos(),
//        })
//    }

    pub fn to_compile_err(&self, msg: String) -> Result<EvalNode, CompileError> {
        Err(CompileError {
            msg,
            pos: self.at(0).unwrap_or(VVal::Nul).get_syn_pos(),
        })
    }

    pub fn is_ref(&self) -> bool {
        match self { VVal::Ref(_) => true, VVal::CRef(_) => true, VVal::WWRef(_) => true, _ => false }
    }

    pub fn is_wref(&self) -> bool {
        match self { VVal::WWRef(_) => true, _ => false }
    }

    pub fn is_bool(&self) -> bool {
        match self { VVal::Bol(_) => true, _ => false }
    }

    pub fn is_bytes(&self) -> bool {
        match self { VVal::Byt(_) => true, _ => false }
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

    pub fn is_none(&self) -> bool {
        match self { VVal::Nul => true, _ => false }
    }

    pub fn is_err(&self) -> bool {
        match self { VVal::Err(_) => true, _ => false }
    }

    pub fn type_name(&self) -> String {
        match self {
            VVal::Str(_)     => String::from("string"),
            VVal::Byt(_)     => String::from("bytes"),
            VVal::Nul        => String::from("none"),
            VVal::Err(_)     => String::from("err"),
            VVal::Bol(_)     => String::from("bool"),
            VVal::Sym(_)     => String::from("sym"),
            VVal::Syn(_)     => String::from("syn"),
            VVal::Int(_)     => String::from("int"),
            VVal::Flt(_)     => String::from("float"),
            VVal::Lst(_)     => String::from("vector"),
            VVal::Map(_)     => String::from("map"),
            VVal::Usr(_)     => String::from("userdata"),
            VVal::Fun(_)     => String::from("function"),
            VVal::DropFun(_) => String::from("drop_function"),
            VVal::Ref(_)     => String::from("ref"),
            VVal::CRef(_)    => String::from("cref"),
            VVal::WWRef(_)   => String::from("wref"),
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
            VVal::Usr(u)     => u.f(),
            VVal::Fun(_)     => 1.0,
            VVal::DropFun(f) => f.v.f(),
            VVal::Ref(l)     => (*l).borrow().f(),
            VVal::CRef(l)    => (*l).borrow().f(),
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
            VVal::Usr(u)     => u.i(),
            VVal::Fun(_)     => 1,
            VVal::DropFun(f) => f.v.i(),
            VVal::Ref(l)     => (*l).borrow().i(),
            VVal::CRef(l)    => (*l).borrow().i(),
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
            VVal::Usr(u)     => u.b(),
            VVal::Fun(_)     => true,
            VVal::DropFun(f) => f.v.i() != 0,
            VVal::Ref(l)     => (*l).borrow().i() != 0,
            VVal::CRef(l)    => (*l).borrow().i() != 0,
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
            VVal::Err(e)     => format!("$e {}", (*e).borrow().0.s()),
            VVal::Bol(b)     => if *b { "$true".to_string() } else { "$false".to_string() },
            VVal::Sym(s)     => format!(":\"{}\"", s),
            VVal::Syn(s)     => format!("&{:?}", s.syn),
            VVal::Int(i)     => i.to_string(),
            VVal::Flt(f)     => f.to_string(),
            VVal::Lst(l)     => VVal::dump_vec_as_str(l),
            VVal::Map(l)     => VVal::dump_map_as_str(l), // VVal::dump_map_as_str(l),
            VVal::Usr(u)     => u.s(),
            VVal::Fun(f)     => {
                let min = if f.min_args.is_none() { "any".to_string() }
                          else { format!("{}", f.min_args.unwrap()) };
                let max = if f.max_args.is_none() { "any".to_string() }
                          else { format!("{}", f.max_args.unwrap()) };
                let upvalues : String = f.upvalues.iter().map(|v| v.s()).collect::<Vec<String>>().join(",");
                if let Some(ref sp) = f.syn_pos {
                    format!("&F{{@[{}:{}/{}],amin={},amax={},locals={},upvalues=$[{}]}}",
                            sp.line, sp.col, sp.file,
                            min, max, f.local_size, upvalues)
                } else {
                    format!("&F{{@[0:0/],amin={},amax={},locals={},upvalues=$[{}]}}",
                            min, max, f.local_size, upvalues)
                }
            },
            VVal::DropFun(f) => f.v.s(),
            VVal::Ref(l)     => format!("$&&{}", (*l).borrow().s()),
            VVal::CRef(l)    => format!("$&{}", (*l).borrow().s()),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => format!("$(&){}", v.borrow().s()),
                    None => "$n".to_string(),
                }
            },
        }
    }

    /// Serializes the VVal (non cyclic) structure to a msgpack byte vector.
    #[cfg(feature="rmp-serde")]
    pub fn to_msgpack(&self) -> Result<Vec<u8>, String> {
        match rmp_serde::to_vec(self) {
            Ok(s) => Ok(s),
            Err(e) => Err(format!("to_msgpack failed: {}", e))
        }
    }

    /// Creates a VVal structure from a msgpack byte vector.
    #[cfg(feature="rmp-serde")]
    pub fn from_msgpack(s: &[u8]) -> Result<VVal, String> {
        match rmp_serde::from_read_ref(&s) {
            Ok(v) => Ok(v),
            Err(e) => Err(format!("from_msgpack failed: {}", e)),
        }
    }

    /// Serializes the VVal (non cyclic) structure to a JSON string.
    #[cfg(feature="serde_json")]
    pub fn to_json(&self, not_pretty: bool) -> Result<String, String> {
        if not_pretty {
            match serde_json::to_string(self) {
                Ok(s) => Ok(s),
                Err(e) => Err(format!("to_json failed: {}", e))
            }
        } else {
            match serde_json::to_string_pretty(self) {
                Ok(s) => Ok(s),
                Err(e) => Err(format!("to_json failed: {}", e))
            }
        }
    }

    /// Creates a VVal structure from a JSON string.
    #[cfg(feature="serde_json")]
    pub fn from_json(s: &str) -> Result<VVal, String> {
        match serde_json::from_str(&s) {
            Ok(v) => Ok(v),
            Err(e) => Err(format!("from_json failed: {}", e)),
        }
    }
}

#[cfg(feature="serde")]
impl serde::ser::Serialize for VVal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::ser::Serializer {
        use serde::ser::{SerializeSeq, SerializeMap};

        match self {
            VVal::Str(_)     => serializer.serialize_str(&self.s_raw()),
            VVal::Sym(_)     => serializer.serialize_str(&self.s_raw()),
            VVal::Byt(b)     => serializer.serialize_bytes(&b.borrow()[..]),
            VVal::Nul        => serializer.serialize_none(),
            VVal::Err(_)     => serializer.serialize_str(&self.s()),
            VVal::Bol(b)     => serializer.serialize_bool(*b),
            VVal::Syn(_)     => serializer.serialize_str(&self.s()),
            VVal::Int(i)     => serializer.serialize_i64(*i),
            VVal::Flt(f)     => serializer.serialize_f64(*f),
            VVal::Lst(l)     => {
                let mut seq = serializer.serialize_seq(Some(l.borrow().len()))?;
                for v in l.borrow().iter() {
                    seq.serialize_element(v)?;
                }
                seq.end()
            },
            VVal::Map(l) => {
                let hm = l.borrow();

                let mut map = serializer.serialize_map(Some(l.borrow().len()))?;
                for (k, v) in hm.iter() {
                    map.serialize_entry(k, v)?;
                }
                map.end()
            },
            VVal::Usr(_)     => serializer.serialize_str(&self.s()),
            VVal::Fun(_)     => serializer.serialize_str(&self.s()),
            VVal::DropFun(_) => serializer.serialize_str(&self.s()),
            VVal::Ref(_)     => self.deref().serialize(serializer),
            VVal::CRef(_)    => self.deref().serialize(serializer),
            VVal::WWRef(_)   => self.deref().serialize(serializer),
        }
    }
}

#[cfg(feature="serde")]
struct VValVisitor;

#[cfg(feature="serde")]
impl<'de> serde::de::Visitor<'de> for VValVisitor {
    type Value = VVal;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a VVal")
    }

    fn visit_i128<E>(self, value: i128) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(value as i64)) }
    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(value)) }
    fn visit_i32<E>(self, value: i32) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(i64::from(value))) }
    fn visit_i16<E>(self, value: i16) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(i64::from(value))) }
    fn visit_i8<E>(self, value: i8) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(i64::from(value))) }
    fn visit_u128<E>(self, value: u128) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(value as i64)) }
    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(value as i64)) }
    fn visit_u32<E>(self, value: u32) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(i64::from(value))) }
    fn visit_u16<E>(self, value: u16) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(i64::from(value))) }
    fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Int(i64::from(value))) }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Flt(value)) }
    fn visit_f32<E>(self, value: f32) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Flt(f64::from(value))) }

    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Bol(value)) }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::new_str(value)) }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::new_byt(value.to_vec())) }

    fn visit_none<E>(self) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Nul) }
    fn visit_unit<E>(self) -> Result<Self::Value, E>
        where E: serde::de::Error { Ok(VVal::Nul) }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where A: serde::de::SeqAccess<'de> {

        let v = VVal::vec();

        while let Some(ve) = seq.next_element()? {
            v.push(ve);
        }

        Ok(v)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where A: serde::de::MapAccess<'de> {

        let v = VVal::map();

        while let Some((ke, ve)) = map.next_entry()? {
            let k : VVal = ke;
            v.set_key(&VVal::new_sym(&k.s_raw()), ve);
        }

        Ok(v)
    }
}

#[cfg(feature="serde")]
impl<'de> serde::de::Deserialize<'de> for VVal {
    fn deserialize<D>(deserializer: D) -> Result<VVal, D::Error>
        where D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_any(VValVisitor)
    }
}
