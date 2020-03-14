// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!

This module provides the core data structures used by the parser,
compiler and evaluator of WLambda.

*/

use std::rc::Rc;
use std::rc::Weak;
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Display, Debug, Formatter};

use crate::compiler::{GlobalEnv, GlobalEnvRef};
use crate::nvec::NVector;

use fnv::FnvHashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct FileRef {
    s: Rc<String>,
}

impl Display for FileRef {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", *self.s)
    }
}

impl FileRef {
    pub fn new(s: &str) -> Self {
        FileRef { s: Rc::new(String::from(s)) }
    }
    pub fn s(&self) -> &str { &(*self.s) }
}

/// Structure for holding information about origin
/// of an AST node.
#[derive(Clone, PartialEq)]
pub struct SynPos {
    pub syn:        Syntax,
    pub line:       u32,
    pub col:        u16,
    pub file:       FileRef,
    pub name:       Option<std::rc::Rc<String>>,
}

impl Display for SynPos {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.line > 0 {
            if self.name.is_some() && !self.name.as_ref().unwrap().is_empty() {
                write!(f, "[{},{}:{}({:?})@{}]",
                       self.line, self.col, self.file.s(), self.syn,
                       self.name.as_ref().unwrap())
            } else {
                write!(f, "[{},{}:{}({:?})]",
                       self.line, self.col, self.file.s(), self.syn)
            }
        } else {
            write!(f, "")
        }
    }
}

impl Debug for SynPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
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
    GetKey,
    GetKey2,
    GetKey3,
    GetSym,
    GetSym2,
    GetSym3,
    GetIdx,
    GetIdx2,
    GetIdx3,
    BinOpAdd,
    BinOpSub,
    BinOpMul,
    BinOpDiv,
    BinOpMod,
    BinOpLe,
    BinOpLt,
    BinOpGe,
    BinOpGt,
    BinOpEq,
    Str,
    Lst,
    IVec,
    FVec,
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
    CaptureRef,
    AssignRef,
    DefGlobRef,
    DefConst,
    SelfObj,
    SelfData,
    Import,
    Export,
    DumpStack,
    MapSplice,
    VecSplice,
    Accum,
}

#[derive(Clone)]
pub struct Stdio {
    pub write: Rc<RefCell<dyn std::io::Write>>,
    pub read:  Rc<RefCell<dyn std::io::BufRead>>,
}

impl Stdio {
    pub fn new_rust_std() -> Self {
        Self {
            write: Rc::new(RefCell::new(std::io::stdout())),
            read:  Rc::new(RefCell::new(std::io::BufReader::new(std::io::stdin()))),
        }
    }

    pub fn new_from_mem(input: Rc<RefCell<std::io::Cursor<Vec<u8>>>>,
                        output: Rc<RefCell<Vec<u8>>>)
        -> Self
    {
        Self {
            write: output,
            read: input,
        }
    }
}

impl std::fmt::Debug for Stdio {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "wlambda::vval::Stdio")
    }
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
    /// A stack of the currently called functions.
    ///
    /// Used for accessing the up values, backtrace
    /// and other details about the function.
    pub call_stack: std::vec::Vec<Rc<VValFun>>,
    /// Holds the object of the currently called method:
    pub current_self: VVal,
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
    pub exports: FnvHashMap<String, VVal>,
    /// This is the standard output used for any functions in
    /// WLambda that print something. Such as `std:displayln`
    /// or `std:writeln`.
    pub stdio: Stdio,
    /// Current accumulator value:
    pub accum_val: VVal,
    /// Current accumulator function:
    pub accum_fun: VVal,
    /// A pointer to the global environment, holding stuff like
    /// the module loader and thread creator.
    pub global: GlobalEnvRef,
}

//impl Default for Env {
//    fn default() -> Self { Self::new() }
//}
//
impl Env {
    pub fn new(global: GlobalEnvRef) -> Env {
        let mut e = Env {
            args:               Vec::with_capacity(STACK_SIZE),
            current_self:       VVal::Nul,
            bp:                 0,
            sp:                 0,
            argc:               0,
            user:               Rc::new(RefCell::new(VVal::vec())),
            exports:            FnvHashMap::with_capacity_and_hasher(5, Default::default()),
            stdio:              Stdio::new_rust_std(),
            accum_fun:          VVal::Nul,
            accum_val:          VVal::Nul,
            call_stack:         vec![],
            global
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    pub fn new_with_user(global: GlobalEnvRef, user: Rc<RefCell<dyn std::any::Any>>) -> Env {
        let mut e = Env {
            args:               Vec::with_capacity(STACK_SIZE),
            current_self:       VVal::Nul,
            bp:                 0,
            sp:                 0,
            argc:               0,
            exports:            FnvHashMap::with_capacity_and_hasher(2, Default::default()),
            stdio:              Stdio::new_rust_std(),
            accum_fun:          VVal::Nul,
            accum_val:          VVal::Nul,
            call_stack:         vec![],
            user,
            global,
        };
        e.args.resize(STACK_SIZE, VVal::Nul);
        e
    }

    /// Sets a custom stdio procedure. This can be used to redirect the
    /// standard input/output operations of WLambda to other sinks and sources.
    /// For instance writing and reading from a Buffer when using WASM
    /// or some other embedded application:
    ///
    /// ```
    /// use wlambda::*;
    /// use std::rc::Rc;
    /// use std::cell::RefCell;
    ///
    /// let new_output : Rc<RefCell<Vec<u8>>> =
    ///     Rc::new(RefCell::new(vec![]));
    /// let new_input =
    ///     Rc::new(RefCell::new(std::io::Cursor::new("abc\ndef\n1 2 3 4\n"
    ///                          .to_string().as_bytes().to_vec())));
    ///
    /// let memory_stdio =
    ///     vval::Stdio::new_from_mem(new_input, new_output.clone());
    ///
    /// let mut ctx = EvalContext::new_default();
    /// ctx.local.borrow_mut().set_stdio(memory_stdio);
    ///
    /// ctx.eval("std:displayln :TEST 123").unwrap();
    ///
    /// let output = String::from_utf8(new_output.borrow().clone()).unwrap();
    /// assert_eq!(output, "TEST 123\n");
    ///
    /// let out_lines =
    ///     ctx.eval("!l = $[]; std:io:lines \\std:push l _; l").unwrap().s();
    /// assert_eq!(out_lines, "$[\"abc\\n\",\"def\\n\",\"1 2 3 4\\n\"]");
    /// ```
    pub fn set_stdio(&mut self, stdio: Stdio) {
        self.stdio = stdio;
    }

    /// Returns the passed in user context value.
    pub fn get_user(&self) -> Rc<RefCell<dyn std::any::Any>> {
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

    #[inline]
    pub fn set_bp(&mut self, env_size: usize) -> usize {
        let new_bp = self.sp;
        self.sp += env_size;
        std::mem::replace(&mut self.bp, new_bp)
    }

    #[inline]
    pub fn reset_bp(&mut self, env_size: usize, oldbp: usize) {
        for i in self.bp..self.sp {
            self.args[i] = VVal::Nul;
        }
//        self.sp -= env_size;
        self.popn(env_size);
        self.bp = oldbp;
    }

    pub fn self_object(&self) -> VVal {
        self.current_self.clone()
    }

    #[inline]
    pub fn with_object<T>(&mut self, object: VVal, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {
        let old_self = std::mem::replace(&mut self.current_self, object);
        let ret = f(self);
        std::mem::replace(&mut self.current_self, old_self);
        ret
    }

    #[inline]
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

    #[inline]
    pub fn with_fun_info<T>(&mut self, fu: Rc<VValFun>, argc: usize, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {
        let local_size = fu.local_size;
        let old_argc = std::mem::replace(&mut self.argc, argc);
//        let old_fun  = std::mem::replace(&mut self.fun, fu.clone());
        self.call_stack.push(fu);
        let old_bp   = self.set_bp(local_size);
        //d// println!("OLD FUN: {:?}", old_fun.upvalues);

        let ret = f(self);

        self.reset_bp(local_size, old_bp);
//        self.fun  = old_fun;
        self.call_stack.pop();
        self.argc = old_argc;

        ret
    }

    #[inline]
    pub fn with_restore_sp<T>(&mut self, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {

        let old_sp = self.sp;
        let ret = f(self);
        self.popn(self.sp - old_sp);
        ret
    }

    #[inline]
    pub fn with_pushed_sp<T>(&mut self, n: usize, f: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction> {

        //d// println!("PUSHSP {}", n);
        self.push_sp(n);
        let ret = f(self);
        self.popn(n);
        ret
    }

    #[inline]
    pub fn push_sp(&mut self, n: usize) {
        self.sp += n;
        //d// println!("PUSH_SP {} => {}", n, self.sp);
    }

    #[inline]
    pub fn push(&mut self, v: VVal) -> usize {
        self.args[self.sp] = v;
        self.sp += 1;
        self.sp - 1
    }

    #[inline]
    pub fn pop(&mut self) -> VVal {
        if self.sp < 1 {
            panic!(format!("Stack pointer underflow {} {}", self.sp, 1));
        }
        self.sp -= 1;
        std::mem::replace(&mut self.args[self.sp], VVal::Nul)
    }

    #[inline]
    pub fn popn(&mut self, n: usize) {
        if self.sp < n {
            panic!(format!("Stack pointer underflow {} {}", self.sp, n));
        }
        if n > 0 {
            //d// println!("SP={}, N={}", self.sp, n);
            for i in (self.sp - n)..self.sp {
                //d// println!("POP[{}] {} [of {}]", i, self.args[i].s(), n);
                self.args[i] = VVal::Nul;
            }
        }
        self.sp -= n;
        //d// println!("POPN {} => {}", n, self.sp);
    }

    /// Prints a dump of the stack state.
    pub fn dump_stack(&self) {
        //d// println!("* SP={}, BP={}", self.sp, self.bp);
        for (i, v) in self.args.iter().enumerate() {
            let mut mark = String::from("");
            if i == self.bp { mark = format!("{} BP", mark); }
            if i == self.sp { mark = format!("{} SP", mark); }
            if !mark.is_empty() { mark = format!("{} ->", mark); }

            println!("    {:9} [{:3}] = {}", mark, i, v.s());
            if i >= (1 + self.sp) { break; }
        }
        if !self.call_stack.is_empty() {
            for (i, u) in self.call_stack.last().unwrap().upvalues.iter().enumerate() {
                println!("  UP[{:3}] = {}", i, u.s());
            }
        }
    }

    #[inline]
    pub fn argv(&self) -> VVal {
        VVal::vec_from(&self.args[(self.bp - self.argc)..self.bp])
    }

    #[inline]
    pub fn get_up_raw(&mut self, i: usize) -> VVal {
        //d// println!("GET UP {}: {:?}", i, self.fun.upvalues);
        self.call_stack.last().unwrap().upvalues[i].clone()
    }

    #[inline]
    pub fn get_up_captured_ref(&self, i: usize) -> VVal {
        self.call_stack.last().unwrap().upvalues[i].to_ref()
    }

    #[inline]
    pub fn get_up(&self, i: usize) -> VVal {
        self.call_stack.last().unwrap().upvalues[i].deref()
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

    #[inline]
    pub fn set_arg(&mut self, i: usize, v: VVal) {
        //d// println!("SET ARG [{}/{}]= {}", i, self.sp - (i + 1), v.s());
        self.args[self.sp - (i + 1)] = v;
    }

    #[inline]
    pub fn arg_ref(&self, i: usize) -> Option<&VVal> {
        if i >= self.argc { return None; }
        Some(&self.args[self.bp - (i + 1)])
    }

    #[inline]
    pub fn arg_err_internal(&self, i: usize) -> Option<VVal> {
        let v = &self.args[self.bp - (i + 1)];
        match v {
            VVal::Err(_) => Some(v.clone()),
            _            => None,
        }
    }

    #[inline]
    pub fn arg(&self, i: usize) -> VVal {
        //d// println!("GET ARGC [{}] = {}", i, self.argc);
        if i >= self.argc { return VVal::Nul; }
        let v = &self.args[self.bp - (i + 1)];
        //d// println!("GET ARG [{}/{}] = {}", i, self.sp - (i + 1), v.s());
        match v {
            v => v.clone(),
        }
    }

    pub fn get_local_up_promotion(&mut self, i: usize) -> VVal {
        let idx = self.bp + i;
        match &self.args[idx] {
            VVal::CRef(r) => VVal::WWRef(Rc::downgrade(&r)),
            VVal::Ref(r)  => VVal::Ref(r.clone()),
            VVal::WWRef(r) => VVal::WWRef(r.clone()),
            v => {
                let new_v = v.to_weakened_upvalue_ref();
                self.args[idx] = new_v.clone();
                new_v.downgrade()
            }
        }
    }

    pub fn get_local_captured_ref(&self, i: usize) -> VVal {
        let idx = self.bp + i;
        self.args[idx].to_ref()
    }

    #[inline]
    pub fn get_local(&self, i: usize) -> VVal {
        let idx = self.bp + i;
        match &self.args[idx] {
            VVal::CRef(r)  => r.borrow().clone(),
            v              => v.clone(),
        }
    }

    pub fn assign_ref_up(&mut self, i: usize, value: VVal) {
        let fun = self.call_stack.last().unwrap().clone();
        let upv = &fun.upvalues[i];

        match upv {
            VVal::Ref(r)     => { r.replace(value); }
            VVal::CRef(r)    => { r.replace(value); }
            VVal::WWRef(l)   => {
                if let Some(r) = l.upgrade() {
                    r.replace(value);
                }
            },
            _ => (),
        }
    }

    pub fn assign_ref_local(&mut self, i: usize, value: VVal) {
        let idx = self.bp + i;
        match &mut self.args[idx] {
            VVal::Ref(r)     => { r.replace(value); }
            VVal::CRef(r)    => { r.replace(value); }
            VVal::WWRef(l)   => {
                if let Some(r) = l.upgrade() {
                    r.replace(value);
                }
            },
            v => { *v = value },
        }
    }

    pub fn set_up(&mut self, index: usize, value: &VVal) {
        let fun = self.call_stack.last().unwrap().clone();
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

    pub fn set_consume(&mut self, i: usize, value: VVal) {
        let idx = self.bp + i;
        match &mut self.args[idx] {
            VVal::CRef(r)  => { r.replace(value); }
            v              => { *v = value }
        }
    }

    pub fn with_accum<T>(&mut self, v: VVal, acfun: T) -> Result<VVal, StackAction>
        where T: Fn(&mut Env) -> Result<VVal, StackAction>
    {
        let f =
            match v {
                VVal::Map(_) => {
                    VValFun::new_fun(
                        move |env: &mut Env, _argc: usize| {
                            let k = env.arg(0);
                            let v = env.arg(1);
                            env.accum_val.set_key(&k, v.clone())?;
                            Ok(v)
                        }, Some(2), Some(2), false)
                },
                _ => {
                    VValFun::new_fun(
                        move |env: &mut Env, _argc: usize| {
                            let v = env.arg(0);
                            env.accum_val.accum(&v);
                            Ok(v)
                        }, Some(1), Some(1), false)
                }
            };

        let old_val = std::mem::replace(&mut self.accum_val, v);
        let old_fun = std::mem::replace(&mut self.accum_fun, f);

        acfun(self)?;

        std::mem::replace(&mut self.accum_fun, old_fun);

        Ok(std::mem::replace(&mut self.accum_val, old_val))
    }

    pub fn get_accum_value(&self) -> VVal {
        self.accum_val.clone()
    }

    pub fn get_accum_function(&self) -> VVal {
        self.accum_fun.clone()
    }

    /// Creates a new error VVal with the given string as error message.
    /// Takes care to annotate the error value with the current function
    /// information.
    ///
    ///```
    /// use wlambda::compiler::EvalContext;
    /// use wlambda::vval::{VVal, VValFun, Env};
    ///
    /// let mut ctx = EvalContext::new_default();
    ///
    /// ctx.set_global_var("xyz",
    ///     &VValFun::new_fun(
    ///         move |env: &mut Env, argc: usize| {
    ///             let ok = false;
    ///             if !ok {
    ///                 Ok(env.new_err(
    ///                     format!("Something was not ok!")))
    ///             } else {
    ///                 Ok(VVal::Bol(true))
    ///             }
    ///         }, None, None, false));
    ///```
    pub fn new_err(&self, s: String) -> VVal {
        for i in self.call_stack.iter().rev() {
            if i.syn_pos.is_some() {
                return
                    VVal::err(
                        VVal::new_str_mv(s),
                        i.syn_pos.clone().unwrap());
            }
        };

        VVal::err(
            VVal::new_str_mv(s),
            self.call_stack.last().unwrap().syn_pos.clone().or_else(
                || Some(SynPos { syn: Syntax::Block, line: 0,
                                 col: 0, file: FileRef::new("?"),
                                 name: None })).unwrap())
    }
}

/// Encodes all kinds of jumps up the call stack, like `break` and `next` in Loops.
///
/// As WLambda is not using a VM, it uses return values of the
/// closure call tree to handle jumping up the stack.
#[derive(Clone)]
pub enum StackAction {
    Panic(VVal, Vec<Option<SynPos>>),
    Return((VVal, VVal)),
    Break(VVal),
    Next,
}

impl Display for StackAction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            StackAction::Panic(v, spstk) => {
                let stk : Vec<String> =
                    spstk.iter().map(|s|
                        if let Some(p) = s { format!("{}", p) }
                        else { String::from("[?]") })
                    .collect();
                write!(f, "{} SA::Panic({})", stk.join("=>"), v.s())
            },
            StackAction::Return((l, v)) => write!(f, "SA::Return(lbl={},{})", l.s(), v.s()),
            StackAction::Break(v) => write!(f, "SA::Break({})", v.s()),
            StackAction::Next     => write!(f, "SA::Next"),
        }
    }
}

impl Debug for StackAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl StackAction {
    pub fn panic_borrow(v: &VVal) -> Self {
        Self::panic_msg(format!("Can't mutate borrowed value: {}", v.s()))
    }

    pub fn panic_msg(err: String) -> Self {
        let v = Vec::new();
        StackAction::Panic(VVal::new_str_mv(err), v)
    }
    pub fn panic_str(err: String, sp: Option<SynPos>) -> Self {
        let mut v = Vec::new();
        v.push(sp);
        StackAction::Panic(VVal::new_str_mv(err), v)
    }
    pub fn panic(err: VVal, sp: Option<SynPos>) -> Self {
        let mut v = Vec::new();
        v.push(sp);
        StackAction::Panic(err, v)
    }
    pub fn wrap_panic(self, sp: Option<SynPos>) -> Self {
        match self {
            StackAction::Panic(v, mut stk) => {
                stk.push(sp);
                StackAction::Panic(v, stk)
            },
            _ => self,
        }
    }
}

/// Position of a variable represented in the `CompileEnv`.
#[derive(Debug, Clone)]
pub enum VarPos {
    /// No position of the variable. Mostly placeholder value for non existing variable.
    NoPos,
    /// Variable is stored in upvalue at the specified position.
    UpValue(usize),
    /// Variable is stored in local variables on the stack at the specified position.
    Local(usize),
    /// Variable is stored in the global variables with the given value.
    Global(VVal),
    /// A constant value, major difference to Global is, that it is not a reference
    /// and a slight bit faster.
    Const(VVal),
}

pub type EvalNode = Box<dyn Fn(&mut Env) -> Result<VVal,StackAction>>;
pub type ClosNodeRef = Rc<RefCell<dyn Fn(&mut Env, usize) -> Result<VVal,StackAction>>>;

#[derive(Clone)]
/// This structure is the runtime representation of a WLambda function value.
pub struct VValFun {
    /// The closure that runs the function.
    pub fun:        ClosNodeRef,
    /// The positions of the upvalues that are being captured by this function.
    pub upvalue_pos: Rc<std::vec::Vec<VarPos>>,
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
    /// If true, then this function accepts error values without panic.
    /// Functions by default don't accept errors as argument. It needs to be
    /// explicitly enabled.
    pub err_arg_ok: bool,
    /// The location of the definition of this function.
    pub syn_pos:    Option<SynPos>,
}

impl VValFun {
    /// Creates a new VVal containing the given closure with the given minimum
    /// and maximum parameters (see also [`add_func` of GlobalEnv](compiler/struct.GlobalEnv.html#method.add_func)).
    ///
    /// The `err_arg_ok` parameter specifies whether the function accepts
    /// error values as arguments. If it doesn't, the program will panic
    /// once an error value is encountered. This makes programs more maintainable.
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
    ///         }, None, None, false));
    ///
    /// assert_eq!(ctx.eval("xyz[]").unwrap().s_raw(), "xyz")
    ///```
    pub fn new_fun<T>(fun: T, min_args: Option<usize>, max_args: Option<usize>, err_arg_ok: bool) -> VVal
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction> {

        VValFun::new_val(Rc::new(RefCell::new(fun)), Vec::new(), 0, min_args, max_args, err_arg_ok, None, Rc::new(vec![]))
    }

    pub fn new_fun_with_pos<T>(fun: T, min_args: Option<usize>, max_args: Option<usize>, err_arg_ok: bool, spos: SynPos) -> VVal
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction> {

        VValFun::new_val(Rc::new(RefCell::new(fun)), Vec::new(), 0, min_args, max_args, err_arg_ok, Some(spos), Rc::new(vec![]))
    }

    /// Internal utility function. Use at your own risk, API might change.
    pub fn new_val(fun: ClosNodeRef, upvalues: std::vec::Vec<VVal>,
                   env_size: usize, min_args: Option<usize>,
                   max_args: Option<usize>, err_arg_ok: bool, syn_pos: Option<SynPos>,
                   upvalue_pos: Rc<std::vec::Vec<VarPos>>) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalue_pos,
            upvalues,
            fun,
            local_size: env_size,
            min_args,
            max_args,
            err_arg_ok,
            syn_pos,
        }))
    }

    /// Returns a dummy function that does nothing.
    pub fn new_dummy() -> Rc<VValFun> {
        Rc::new(VValFun {
            fun:         Rc::new(RefCell::new(|_: &mut Env, _a: usize| { Ok(VVal::Nul) })),
            upvalue_pos: Rc::new(vec![]),
            upvalues:    Vec::new(),
            local_size:  0,
            min_args:    None,
            max_args:    None,
            err_arg_ok:  false,
            syn_pos:     None,
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
/// use wlambda::{VVal, GlobalEnv, StackAction};
///
/// #[derive(Clone, Debug)]
/// struct MyType {
///     x: Rc<RefCell<(i64, i64)>>,
/// }
///
/// impl wlambda::vval::VValUserData for MyType {
///     fn s(&self) -> String { format!("$<MyType({:?})>", self.x.borrow()) }
///     fn i(&self) -> i64    { self.x.borrow_mut().1 }
///     fn get_key(&self, key: &str) -> Option<VVal> {
///         Some(VVal::new_str(key))
///     }
///     fn call(&self, args: &[VVal]) -> Result<VVal, StackAction> {
///         if args.len() < 0 {
///             return Err(StackAction::panic_msg(
///                 format!("{} called with too few arguments", self.s())));
///         }
///         Ok(args[0].clone())
///     }
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

/// Sometimes, if your UserData is a Rc<RefCell<...>>, it can pay off defining
/// some wrapper and From/Into traits for easier handling:
///
/// Here an example from a game I worked on:
///```
/// use wlambda::vval::{VVal, StackAction, VValUserData};
/// use std::rc::Rc;
/// use std::cell::RefCell;
///
/// #[derive(Clone)]
/// struct Ship { }
///
/// #[derive(Clone)]
/// struct ShipWlWrapper(Rc<RefCell<Ship>>);
///
/// impl From<Rc<RefCell<Ship>>> for ShipWlWrapper {
///     fn from(r: Rc<RefCell<Ship>>) -> ShipWlWrapper {
///         ShipWlWrapper(r)
///     }
/// }
///
/// impl Into<VVal> for ShipWlWrapper {
///     fn into(self) -> VVal { VVal::Usr(Box::new(self)) }
/// }
///
/// impl VValUserData for ShipWlWrapper {
///     // ...
///     fn as_any(&mut self) -> &mut dyn std::any::Any { self }
///     fn clone_ud(&self) -> Box<dyn wlambda::vval::VValUserData> {
///         Box::new(self.clone())
///     }
/// }
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
    /// a deep clone or a shallow clone or something else is up to you.
    fn clone_ud(&self) -> Box<dyn VValUserData>;
    /// Makes your user data act like a map. This can be useful
    /// for implementing your own registries or data structures.
    /// Implement this method for setting a key to a value.
    fn set_key(&self, _key: &VVal, _val: VVal) -> Result<(), StackAction> { Ok(()) }
    /// This method returns some value that your user data
    /// associates with the given key.
    fn get_key(&self, _key: &str) -> Option<VVal> { None }
    /// This method is called when the user data is called.
    fn call(&self, _args: &[VVal]) -> Result<VVal, StackAction> { Ok(VVal::Nul) }
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

impl std::fmt::Debug for dyn VValUserData {
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
        let global = GlobalEnv::new_default();
        let mut e = Env::new(global);
        e.push(self.v.clone());
        if let Err(e) = self.fun.call_internal(&mut e, 1) {
            eprintln!("Error in drop function: {}", e);
        }
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
    Sym(Rc<RefCell<String>>),
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
    /// A pair
    Pair(Box<(VVal, VVal)>),
    /// A list (or vector) of VVals.
    Lst(Rc<RefCell<std::vec::Vec<VVal>>>),
    /// A mapping of strings to VVals.
    Map(Rc<RefCell<FnvHashMap<String, VVal>>>),
    /// A function, see also [VValFun](struct.VValFun.html)
    Fun(Rc<VValFun>),
    /// A guarded VVal, that executes a given function when it is
    /// no longer referenced.
    DropFun(Rc<DropVVal>),
    /// A (strong) reference to a VVal.
    Ref(Rc<RefCell<VVal>>),
    /// A numerical (mathematical) vector storing integers. See NVector for more information.
    FVec(NVector<f64>),
    /// A numerical (mathematical) vector storing floats. See NVector for more information.
    IVec(NVector<i64>),
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

struct CycleCheck {
    refs: FnvHashMap<i64, i64>,
    backref_counter: i64,
}

impl CycleCheck {
    fn new() -> Self {
        CycleCheck {
            refs: FnvHashMap::with_capacity_and_hasher(2, Default::default()),
            backref_counter: 1,
        }
    }

    fn touch_walk(&mut self, v: &VVal) {
        if self.touch(v).is_some() { return; }

        match v {
            VVal::Err(e) => self.touch_walk(&(*e).borrow().0),
            VVal::Pair(b) => {
                self.touch_walk(&b.0);
                self.touch_walk(&b.1);
            },
            VVal::Lst(l) => {
                for v in l.borrow().iter() { self.touch_walk(v); }
            },
            VVal::Map(l) => {
                for (_k, v) in l.borrow().iter() { self.touch_walk(&v); }
            },
            VVal::DropFun(f) => { self.touch_walk(&f.v); },
            VVal::Fun(f) => {
                for v in f.upvalues.iter() {
                    self.touch_walk(v);
                }
            },
            VVal::Ref(l) => { self.touch_walk(&(*l).borrow()); },
            VVal::CRef(l) => { self.touch_walk(&(*l).borrow()); },
            VVal::WWRef(l) => {
                if let Some(v) = l.upgrade() {
                    self.touch_walk(&(*v).borrow());
                }
            },
              VVal::Str(_)
            | VVal::Byt(_)
            | VVal::Nul
            | VVal::Bol(_)
            | VVal::Sym(_)
            | VVal::Syn(_)
            | VVal::FVec(_)
            | VVal::IVec(_)
            | VVal::Int(_)
            | VVal::Flt(_)
            | VVal::Usr(_) => {},
        }
    }

    fn touch(&mut self, v: &VVal) -> Option<i64> {
        let id =
            if let Some(id) = v.ref_id() { id }
            else { return None; };
        if let Some(backref) = self.refs.get(&id) {
            if *backref == 0 {
                let bkr_count = self.backref_counter;
                self.backref_counter += 1;
                self.refs.insert(id, bkr_count);
                Some(bkr_count)
            } else {
                Some(*backref)
            }
        } else {
            self.refs.insert(id, 0);
            None
        }
    }

    fn backref(&mut self, v: &VVal) -> Option<(bool, String)> {
        let id =
            if let Some(id) = v.ref_id() { id }
            else { return None; };
        if let Some(backref) = self.refs.get(&id) {
            match *backref {
                br if br > 0 => {
                    self.refs.insert(id, -br);
                    Some((true, format!("$<{}=>", br)))
                },
                br if br < 0 =>
                    Some((false, format!("$<{}>", -br))),
                _ => None,
            }
        } else {
            None
        }
    }
}

pub type VValIter =
    std::iter::FromFn<Box<dyn FnMut() -> Option<(VVal, Option<VVal>)>>>;

#[allow(dead_code)]
impl VVal {
    pub fn new_str(s: &str) -> VVal {
        VVal::Str(Rc::new(RefCell::new(String::from(s))))
    }

    pub fn new_str_mv(s: String) -> VVal {
        VVal::Str(Rc::new(RefCell::new(s)))
    }

    pub fn new_sym(s: &str) -> VVal {
        VVal::Sym(Rc::new(RefCell::new(String::from(s))))
    }

    pub fn new_sym_mv(s: String) -> VVal {
        VVal::Sym(Rc::new(RefCell::new(s)))
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
             SynPos { syn: Syntax::Block, line: 0,
                      col: 0, file: FileRef::new("?"), name: None }))))
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

    pub fn compare_str(&self, b: &VVal) -> std::cmp::Ordering {
        self.with_s_ref(|a: &str| b.with_s_ref(|b: &str| a.cmp(b)))
    }

    pub fn shallow_clone(&self) -> VVal {
        match self {
            VVal::Lst(l) => {
                let out = VVal::vec();
                for v in l.borrow().iter() { out.push(v.clone()); }
                out
            },
            VVal::Map(m) => {
                let out = VVal::map();
                for (k, v) in m.borrow_mut().iter() {
                    out.set_map_key(k.to_string(), v.clone())
                }
                out
            },
            VVal::Str(s) => {
                VVal::new_str_mv(s.borrow_mut().clone())
            },
            VVal::DropFun(v) => v.v.shallow_clone(),
            VVal::Ref(v)     => v.borrow_mut().shallow_clone(),
            VVal::CRef(v)    => v.borrow_mut().shallow_clone(),
            VVal::WWRef(v)   =>
                if let Some(r) = v.upgrade() {
                    r.borrow().shallow_clone()
                } else { VVal::Nul },
            _ => self.clone()
        }
    }

    pub fn compare_num(&self, b: &VVal) -> std::cmp::Ordering {
        if self.is_float() {
            self.f().partial_cmp(&b.f())
                .unwrap_or(std::cmp::Ordering::Greater)
        } else {
            self.i().cmp(&b.i())
        }
    }

    pub fn sort<F>(&mut self, compare: F)
        where F: FnMut(&VVal, &VVal) -> std::cmp::Ordering
    {
        if let VVal::Lst(v) = self {
            v.borrow_mut().sort_by(compare);
        }
    }

    pub fn fisher_yates_shuffle<I>(&mut self, mut rand: I)
        where I: FnMut() -> i64
    {
        if let VVal::Lst(v) = self {
            let mut list = v.borrow_mut();
            if list.len() <= 1 { return; }

            for k in 1..list.len() {
                let i = list.len() - k;
                let j = rand().abs() as usize % (i + 1);
                list.swap(j, i);
            }
        }
    }

    /// This function returns you an iterator over the VVal.
    /// It will iterate over data such as VVal::Str, VVal::Sym, VVal::Lst,
    /// VVal::Map and VVal::Byt.
    ///
    /// This functionality provides the `for` keyword/function in WLambda.
    ///
    /// ```
    /// use wlambda::*;
    ///
    /// let some_vec = VVal::vec();
    /// some_vec.push(VVal::Int(10));
    /// some_vec.push(VVal::Int(22));
    /// some_vec.push(VVal::Int(36));
    ///
    /// let mut sum = 0;
    /// for (v, _) in some_vec.iter() {
    ///     sum += v.i();
    /// }
    ///
    /// assert_eq!(sum, 68);
    /// ```
    pub fn iter(&self) -> VValIter {
        match self {
            VVal::Lst(l) => {
                let l = l.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    if idx >= l.borrow().len() { return None; }
                    let r = Some((l.borrow()[idx].clone(), None));
                    idx += 1;
                    r
                }))
            },
            VVal::Map(m) => {
                let m = m.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    let r = match m.borrow().iter().nth(idx) {
                        Some((k, v)) => {
                            Some((v.clone(), Some(VVal::new_str(&k))))
                        },
                        None => None,
                    };
                    idx += 1;
                    r
                }))
            },
            VVal::Byt(b) => {
                let b = b.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    if idx >= b.borrow().len() { return None; }
                    let r = Some((VVal::new_byt(vec![b.borrow()[idx]]), None));
                    idx += 1;
                    r
                }))
            },
            VVal::Str(s) | VVal::Sym(s) => {
                let s = s.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    let r = match s.borrow().chars().nth(idx) {
                        Some(chr) => Some((VVal::new_str_mv(chr.to_string()), None)),
                        None      => None,
                    };
                    idx += 1;
                    r
                }))
            },
            VVal::DropFun(v) => v.v.iter(),
            VVal::Ref(v)     => v.borrow().iter(),
            VVal::CRef(v)    => v.borrow().iter(),
            VVal::WWRef(v)   =>
                if let Some(r) = v.upgrade() {
                    r.borrow().iter()
                } else { std::iter::from_fn(Box::new(|| { None })) },
            _ => {
                let x = self.clone();
                let mut used = false;
                std::iter::from_fn(Box::new(move || {
                    if used { return None; }
                    used = true;
                    Some((x.clone(), None))
                }))
            }
        }
    }

    /// This method will disable all arity checks of the function in the VVal.
    /// Does nothing if the VVal is not a function.
    ///
    /// It is used for disabling checks of drop functions, as they are
    /// evaluated in their own environment and there is no proper way to report
    /// errors upwards.
    pub fn disable_function_arity(&self) -> VVal {
        if let VVal::Fun(fu) = self {
            let mut new_fu = fu.as_ref().clone();
            new_fu.min_args = None;
            new_fu.max_args = None;
            VVal::Fun(Rc::new(new_fu))
        } else {
            self.clone()
        }
    }

    /// This function is an internal function that clones a function reference
    /// and assigns new upvalues to it for making a new closure instance.
    pub fn clone_and_rebind_upvalues<T>(&self, f: T) -> VVal
        where T: FnOnce(&std::vec::Vec<VarPos>, &mut std::vec::Vec<VVal>) -> ()
    {
        if let VVal::Fun(fu) = self {
            let mut new_fu = fu.as_ref().clone();
            f(&new_fu.upvalue_pos, &mut new_fu.upvalues);
            VVal::Fun(Rc::new(new_fu))
        } else {
            panic!("clone_and_rebind_upvalues does not work on non functions!");
        }
    }

    pub fn call_no_args(&self, env: &mut Env) -> Result<VVal, StackAction> {
        self.call_internal(env, 0)
    }

    pub fn call_internal(&self, env: &mut Env, argc: usize) -> Result<VVal, StackAction> {
//        env.dump_stack();
        match self {
            VVal::Nul => {
                Err(StackAction::panic_msg("Calling $none is invalid".to_string()))
            },
            VVal::Fun(fu) => {
                if let Some(i) = fu.min_args {
                    if argc < i {
                        return Err(StackAction::panic_str(
                            format!(
                                "function expects at least {} arguments, got {}",
                                i, argc),
                            fu.syn_pos.clone()));
                    }
                }

                if let Some(i) = fu.max_args {
                    if argc > i {
                        return Err(StackAction::panic_str(
                            format!(
                                "function expects at most {} arguments, got {}",
                                i, argc),
                            fu.syn_pos.clone()));
                    }
                }

                env.with_fun_info(fu.clone(), argc, |e: &mut Env| {
                    if !(*fu).err_arg_ok {
                        for i in 0..argc {
                            if let Some(VVal::Err(ev)) = e.arg_err_internal(i) {
                                return
                                    Err(StackAction::panic_str(
                                        format!("Error value in parameter list: {}",
                                                ev.borrow().0.s()),
                                        Some(ev.borrow().1.clone())));
                            }
                        }
                    }

                    ((*fu).fun.borrow())(e, argc)
                })
            },
            VVal::Bol(b) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let first = e.arg(0);
                    match first {
                        // Calling a boolean on a list converts the boolean into an integer
                        // and fetches the value at that index.
                        VVal::Lst(_) => Ok(first.at(*b as usize).unwrap_or(VVal::Nul)),
                        // Calling a boolean on a pair converts the boolean into an integer
                        // and fetches the value at that index.
                        VVal::Pair(_) => Ok(first.at(*b as usize).unwrap_or(VVal::Nul)),
                        // Calling a boolean with anything else becomes an implicit `if`,
                        // calling the first parameter if the boolean is true
                        // and the second if the boolean is false.
                        _ => {
                            let idx = if *b { 0 } else { 1 };
                            if argc > 0 {
                                let v = e.arg(idx);
                                if !v.is_none() {
                                    v.call_internal(e, 0)
                                } else {
                                    Ok(VVal::Nul)
                                }
                            } else { Ok(self.clone()) }
                        }
                    }
                })
            },
            VVal::Err(e) => {
                Err(StackAction::panic_msg(
                    format!("Called an error value: {}", e.borrow().0.s())))
            },
            VVal::Sym(sym) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    if argc > 0 {
                        let v = e.arg(0);
                        Ok(v.get_key(&*sym.borrow()).unwrap_or(VVal::Nul))
                    } else { Ok(self.clone()) }
                })
            },
            VVal::Map(m) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let f = e.arg(0);

                    let mut ret = VVal::Nul;
                    for (k, v) in m.borrow().iter() {
                        e.push(VVal::new_str(k));
                        e.push(v.clone());
                        let el = f.call_internal(e, 2);
                        e.popn(2);

                        match el {
                            Ok(v)                      => { ret = v; },
                            Err(StackAction::Break(v)) => { ret = v; break; },
                            Err(StackAction::Next)     => { },
                            Err(e)                     => { return Err(e); },
                        }
                    }
                    Ok(ret)
                })
            },
            VVal::Lst(l) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    // calling a list with any other value is an implicit map, meaning that for
                    // each value in the list the argument is called and the respective element
                    // in the map is passed in as the parameter.
                    let f = e.arg(0);

                    let mut ret = VVal::Nul;
                    for i in l.borrow().iter() {
                        e.push(i.clone());
                        let el = f.call_internal(e, 1);
                        e.popn(1);

                        match el {
                            Ok(v)                      => { ret = v; },
                            Err(StackAction::Break(v)) => { ret = v; break; },
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
                            VVal::Byt(b2) => {
                                if argc > 1 {
                                    // TODO: Fix the extra clone here:
                                    let mut accum = vval_bytes.borrow().clone();
                                    accum.extend_from_slice(&b2.borrow());
                                    for i in 2..argc {
                                        match e.arg(i) {
                                            VVal::Byt(b3) =>
                                                accum.extend_from_slice(
                                                    &b3.borrow()),
                                            _ =>
                                                accum.extend_from_slice(
                                                    &e.arg(i).as_bytes()),
                                        }
                                    }
                                    Ok(VVal::new_byt(accum))
                                } else {
                                    let mut accum = vval_bytes.borrow().clone();
                                    accum.extend_from_slice(&b2.borrow());
                                    Ok(VVal::new_byt(accum))
                                }
                            },
                            VVal::Int(arg_int) => {
                                if argc > 1 {
                                    let from = arg_int as usize;
                                    let cnt  = e.arg(1).i() as usize;
                                    let r : Vec<u8> =
                                        vval_bytes.borrow().iter().skip(from)
                                                  .take(cnt).copied().collect();
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
                            VVal::Fun(_) => {
                                let mut ret = VVal::Nul;
                                for c in vval_bytes.borrow().iter() {
                                    e.push(VVal::new_byt(vec![*c]));
                                    let el = first_arg.call_internal(e, 1);
                                    e.popn(1);
                                    match el {
                                        Ok(v)                      => { ret = v; },
                                        Err(StackAction::Break(v)) => { ret = v; break; },
                                        Err(StackAction::Next)     => { },
                                        Err(e)                     => { return Err(e); },
                                    }
                                }
                                Ok(ret)
                            },
                            VVal::Lst(_) => {
                                let from =
                                    first_arg.at(0).unwrap_or(VVal::Int(0))
                                             .i() as usize;
                                let cnt  =
                                    first_arg.at(1).unwrap_or_else(
                                        || VVal::Int((vval_bytes.borrow().len() - from) as i64))
                                    .i() as usize;
                                let r : Vec<u8> =
                                    vval_bytes.borrow().iter().skip(from)
                                              .take(cnt).copied().collect();
                                Ok(VVal::new_byt(r))
                            },
                            VVal::Map(_) => Ok(self.with_s_ref(|key: &str| first_arg.get_key(key).unwrap_or(VVal::Nul))),
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
                                        None    => Ok(VVal::new_str("")),
                                        Some(c) => {
                                            let mut b = [0; 4];
                                            Ok(VVal::new_str(c.encode_utf8(&mut b)))
                                        },
                                    }
                                }
                            },
                            VVal::Fun(_) => {
                                let mut ret = VVal::Nul;
                                for c in vval_str.borrow().chars() {
                                    e.push(VVal::new_str_mv(c.to_string()));
                                    let el = first_arg.call_internal(e, 1);
                                    e.popn(1);
                                    match el {
                                        Ok(v)                      => { ret = v; },
                                        Err(StackAction::Break(v)) => { ret = v; break; },
                                        Err(StackAction::Next)     => { },
                                        Err(e)                     => { return Err(e); },
                                    }
                                }
                                Ok(ret)
                            },
                            VVal::Lst(_) => {
                                let from = first_arg.at(0).unwrap_or(VVal::Int(0)).i() as usize;
                                let cnt  = first_arg.at(1).unwrap_or_else(|| VVal::Int((vval_str.borrow().len() - from) as i64)).i() as usize;
                                let r : String = vval_str.borrow().chars().skip(from).take(cnt).collect();
                                Ok(VVal::new_str_mv(r))
                            },
                            VVal::Str(s2) => {
                                if argc > 1 {
                                    let mut accum = vval_str.borrow().clone() + &s2.borrow();
                                    for i in 1..argc {
                                        e.arg_ref(i).unwrap().with_s_ref(|s: &str| accum += s);
                                    }
                                    Ok(VVal::new_str_mv(accum))
                                } else {
                                    Ok(VVal::new_str_mv(vval_str.borrow().clone() + &s2.borrow()))
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
                    let v = e.arg(0);
                    if argc > 0 { Ok(v.at(*i as usize).unwrap_or(VVal::Nul)) }
                    else { Ok(self.clone()) }
                })
            },
            VVal::Pair(_) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    Ok(self.at(e.arg(0).i() as usize).unwrap_or(VVal::Nul))
                })
            },
            VVal::Usr(ud) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let mut args = vec![];
                    for i in 0..argc { args.push(e.arg(i)) }
                    ud.call(&args)
                })
            },
            VVal::DropFun(v) => v.v.call_internal(env, argc),
            VVal::Ref(v)     => v.borrow().call_internal(env, argc),
            VVal::CRef(v)    => v.borrow().call_internal(env, argc),
            VVal::WWRef(v)   =>
                if let Some(r) = v.upgrade() {
                    r.borrow().call_internal(env, argc)
                } else { Ok(VVal::Nul) },
            _ => { Ok(self.clone()) },
        }
    }

    pub fn to_ref(&self) -> VVal {
        match self {
            VVal::CRef(r)    => VVal::Ref(r.clone()),
            VVal::Ref(r)     => VVal::Ref(r.clone()),
            VVal::WWRef(v)   =>
                if let Some(r) = v.upgrade() {
                    VVal::Ref(r)
                } else {
                    VVal::Ref(Rc::new(RefCell::new(VVal::Nul)))
                },
            _ => VVal::Ref(Rc::new(RefCell::new(self.clone()))),
        }
    }

    pub fn to_weakened_upvalue_ref(&self) -> VVal {
        VVal::CRef(Rc::new(RefCell::new(self.clone())))
    }

    pub fn set_ref(&self, v: VVal) -> VVal {
        match self {
            VVal::Ref(r)     => r.replace(v),
            VVal::CRef(r)    => r.replace(v),
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
            VVal::DropFun(r) => r.v.clone(),
            VVal::Ref(l)     => (*l).borrow().clone(),
            VVal::CRef(l)    => (*l).borrow().clone(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().clone(),
                    None => VVal::Nul,
                }
            },
            _ => self.clone()
        }
    }

    pub fn upgrade(self) -> VVal {
        match self {
            VVal::CRef(f) => VVal::Ref(f),
            VVal::WWRef(f) => {
                if let Some(r) = f.upgrade() {
                    VVal::Ref(r)
                } else {
                    VVal::Nul
                }
            },
            _ => self,
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
        VVal::Map(Rc::new(RefCell::new(FnvHashMap::with_capacity_and_hasher(2, Default::default()))))
    }

    pub fn sym(s: &str) -> VVal {
        VVal::Sym(std::rc::Rc::new(std::cell::RefCell::new(String::from(s))))
    }

    #[allow(clippy::cast_ptr_alignment)]
    pub fn ref_id(&self) -> Option<i64> {
        Some(match self {
            VVal::Err(r)     => { &*r.borrow() as *const (VVal, SynPos) as i64 },
            VVal::Str(s)     => { &*s.borrow() as *const String as i64 },
            VVal::Byt(s)     => { &*s.borrow() as *const Vec<u8> as i64 },
            VVal::Lst(v)     => { &*v.borrow() as *const Vec<VVal> as i64 },
            VVal::Map(v)     => { &*v.borrow() as *const FnvHashMap<String, VVal> as i64 },
            VVal::Fun(f)     => { &**f as *const VValFun as i64 },
            VVal::DropFun(f) => { &**f as *const DropVVal as i64 },
            VVal::Ref(v)     => { &*v.borrow() as *const VVal as i64 },
            VVal::CRef(v)    => { &*v.borrow() as *const VVal as i64 },
            VVal::Usr(b)     => { &**b as *const dyn VValUserData as *const usize as i64 },
            VVal::WWRef(r)   => {
                if let Some(l) = r.upgrade() {
                    &*l.borrow() as *const VVal as i64
                } else {
                    return None;
                }
            },
            _ => return None,
        })
    }

    pub fn eqv(&self, v: &VVal) -> bool {
        match self {
            VVal::Nul     => { if let VVal::Nul = v { true } else { false } },
            VVal::Bol(ia) => { if let VVal::Bol(ib) = v { ia == ib } else { false } },
            VVal::Int(ia) => { if let VVal::Int(ib) = v { ia == ib } else { false } },
            VVal::Flt(ia) => { if let VVal::Flt(ib) = v { (ia - ib).abs() < std::f64::EPSILON } else { false } },
            VVal::Sym(s)  => { if let VVal::Sym(ib) = v { *s == *ib } else { false } },
            VVal::Syn(s)  => { if let VVal::Syn(ib) = v { *s == *ib } else { false } },
            VVal::Str(s)  => { if let VVal::Str(ib) = v { *s == *ib } else { false } },
            VVal::Byt(s)  => { if let VVal::Byt(s2) = v { s.borrow()[..] == s2.borrow()[..] } else { false } },
            VVal::Pair(b)  => {
                if let VVal::Pair(b2) = v { b.0.eqv(&b2.0) && b.1.eqv(&b2.1) }
                else { false }
            },
            VVal::Lst(l)  => {
                if let VVal::Lst(l2) = v { Rc::ptr_eq(l, l2) } else { false }
            },
            VVal::Map(l)  => {
                if let VVal::Map(l2) = v { Rc::ptr_eq(l, l2) } else { false }
            },
            VVal::Fun(l)  => {
                if let VVal::Fun(l2) = v { Rc::ptr_eq(l, l2) } else { false }
            },
            VVal::IVec(_) => unimplemented!(),
            VVal::FVec(_) => unimplemented!(),
            VVal::DropFun(l)  => {
                if let VVal::DropFun(l2) = v { Rc::ptr_eq(l, l2) } else { false }
            },
            VVal::Err(l)  => {
                if let VVal::Err(l2) = v { Rc::ptr_eq(l, l2) } else { false }
            },
            VVal::Ref(l)  => {
                match v {
                    VVal::Ref(r2) => Rc::ptr_eq(l, r2),
                    VVal::CRef(r2) => Rc::ptr_eq(l, r2),
                    VVal::WWRef(r2) =>
                        match r2.upgrade() {
                            Some(v2) => Rc::ptr_eq(l, &v2),
                            None => false,
                        },
                    _ => false,
                }
            },
            VVal::CRef(l)  => {
                match v {
                    VVal::Ref(r2) => Rc::ptr_eq(l, r2),
                    VVal::CRef(r2) => Rc::ptr_eq(l, r2),
                    VVal::WWRef(r2) =>
                        match r2.upgrade() {
                            Some(v2) => Rc::ptr_eq(l, &v2),
                            None => false,
                        },
                    _ => false,
                }
            },
            VVal::WWRef(lw)  => {
                if let Some(l) = lw.upgrade() {
                    match v {
                        VVal::Ref(r2) => Rc::ptr_eq(&l, r2),
                        VVal::CRef(r2) => Rc::ptr_eq(&l, r2),
                        VVal::WWRef(r2) =>
                            match r2.upgrade() {
                                Some(v2) => Rc::ptr_eq(&l, &v2),
                                None => false,
                            },
                        _ => false,
                    }
                } else {
                    false
                }
            },
            VVal::Usr(u)  => {
                if let VVal::Usr(u2) = v {
                    u.eqv(&u2)
                } else {
                    false
                }
            }
        }
    }

    fn dump_vec_as_str(v: &Rc<RefCell<std::vec::Vec<VVal>>>, c: &mut CycleCheck) -> String {
        let mut out : Vec<String> = Vec::new();
        let mut first = true;
        out.push(String::from("$["));
        for s in v.borrow().iter().map(|v| v.s_cy(c)) {
            if !first { out.push(String::from(",")); }
            else { first = false; }
            out.push(s);
        }
        out.push(String::from("]"));
        out.concat()
    }

    fn dump_map_as_str(m: &Rc<RefCell<FnvHashMap<String,VVal>>>, c: &mut CycleCheck) -> String {
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
            out.push(v.s_cy(c));
        }
        out.push(String::from("}"));
        out.concat()
    }

    pub fn map_ok_skip<T, R>(&self, mut op: T, skip: usize) -> Vec<R>
        where T: FnMut(&VVal) -> R {

        let mut res : Vec<R> = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow().iter().skip(skip) {
                res.push(op(i));
            }
        }
        res
    }

    pub fn map_skip<R, E, T>(&self, mut op: T, skip: usize) -> Result<Vec<R>, E>
        where T: FnMut(&VVal) -> Result<R, E> {

        let mut res : Vec<R> = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow().iter().skip(skip) {
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

    pub fn set(&self, index: usize, val: VVal) {
        if let VVal::Lst(b) = &self {
            if index >= b.borrow().len() {
                b.borrow_mut().resize(index + 1, VVal::Nul);
            }
            b.borrow_mut()[index] = val;
        }
    }

    pub fn set_at(&self, index: usize, val: VVal) {
        if let VVal::Lst(b) = &self {
            b.borrow_mut()[index] = val;
        }
    }

    pub fn at(&self, index: usize) -> Option<VVal> {
        match self {
            VVal::Ref(_)   => self.deref().at(index),
            VVal::CRef(_)  => self.deref().at(index),
            VVal::WWRef(_) => self.deref().at(index),
            VVal::DropFun(f) => f.v.at(index),
            VVal::Byt(vval_bytes) => {
                let bytes = vval_bytes.borrow();
                if index as usize >= bytes.len() {
                    None
                } else {
                    Some(VVal::new_byt(vec![bytes[index as usize]]))
                }
            },
            VVal::Str(vval_str) => {
                let opt_char = vval_str.borrow().chars().nth(index as usize);
                match opt_char {
                    None    => None,
                    Some(char) => {
                        let mut buf = [0; 4];
                        Some(VVal::new_str(char.encode_utf8(&mut buf)))
                    },
                }
            },
            VVal::Pair(b) => {
                Some(if index % 2 == 0 { b.0.clone() } else { b.1.clone() })
            },
            VVal::Lst(b) => {
                if b.borrow().len() > index {
                    Some(b.borrow()[index].clone())
                } else {
                    None
                }
            },
            _ => self.get_key(&format!("{}", index)),
        }
    }

    pub fn proto_data(&self) -> VVal {
        match self {
            VVal::Ref(_)   => self.deref().proto_data(),
            VVal::CRef(_)  => self.deref().proto_data(),
            VVal::WWRef(_) => self.deref().proto_data(),
            VVal::Map(m) => m.borrow().get("_data").cloned().unwrap_or(VVal::Nul),
            VVal::Lst(l) => {
                if l.borrow().len() > 1 {
                    l.borrow()[1].clone()
                } else {
                    VVal::Nul
                }
            },
            _ => VVal::Nul,
        }
    }

    pub fn proto_lookup(&self, key: &str) -> Option<VVal> {
        match self {
            VVal::Ref(_)   => self.deref().proto_lookup(key),
            VVal::CRef(_)  => self.deref().proto_lookup(key),
            VVal::WWRef(_) => self.deref().proto_lookup(key),
            VVal::Map(m) => {
                if let Some(func) = m.borrow().get(key) {
                    Some(func.clone())
                } else if let Some(proto) = m.borrow().get("_proto") {
                    proto.proto_lookup(key)
                } else {
                    None
                }
            },
            VVal::Lst(l) => {
                let l = l.borrow();
                if l.is_empty() {
                    None
                } else {
                    l[0].proto_lookup(key)
                }
            },
            _ => None
        }
    }

    pub fn get_key(&self, key: &str) -> Option<VVal> {
        match self {
            VVal::Ref(_)   => self.deref().get_key(key),
            VVal::CRef(_)  => self.deref().get_key(key),
            VVal::WWRef(_) => self.deref().get_key(key),
            VVal::Map(m) => m.borrow().get(key).cloned(),
            VVal::Pair(_) => {
                let idx =
                    match key {
                        "0"         => 0,
                        "1"         => 1,
                        "car"       => 0,
                        "cdr"       => 1,
                        "head"      => 0,
                        "tail"      => 1,
                        "first"     => 0,
                        "second"    => 1,
                        _ => usize::from_str_radix(key, 10).unwrap_or(0),
                    };
                self.at(idx)
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

    pub fn set_map_key_fun<T>(&self, key: String, fun: T, min_args: Option<usize>, max_args: Option<usize>, err_arg_ok: bool)
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction> {
        self.set_map_key(key, VValFun::new_fun(fun, min_args, max_args, err_arg_ok));
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

    pub fn set_key_mv(&self, key: String, val: VVal) {
        match self {
            VVal::Ref(_)   => self.deref().set_key_mv(key, val),
            VVal::CRef(_)  => self.deref().set_key_mv(key, val),
            VVal::WWRef(_) => self.deref().set_key_mv(key, val),
            VVal::DropFun(f) => f.v.set_key_mv(key, val),
            VVal::Map(m) => {
                m.borrow_mut().insert(key, val);
            },
            VVal::Lst(l) => {
                let idx = key.parse::<usize>().unwrap_or(0);
                let mut v = l.borrow_mut();
                if v.len() <= idx {
                    v.resize(idx + 1, VVal::Nul);
                }
                v[idx] = val;
            },
            VVal::Usr(u) => { u.set_key(&VVal::new_str_mv(key), val).unwrap(); },
            _ => {}
        }
    }

    pub fn list_operation<O, R>(&self, mut op: O) -> Result<R, StackAction>
        where O: FnMut(&mut std::cell::RefMut<std::vec::Vec<VVal>>) -> R
    {
        match self {
            VVal::Ref(_)   => self.deref().list_operation(op),
            VVal::CRef(_)  => self.deref().list_operation(op),
            VVal::WWRef(_) => self.deref().list_operation(op),
            VVal::DropFun(f) => f.v.list_operation(op),
            VVal::Lst(l) => {
                match l.try_borrow_mut() {
                    Ok(mut v) => { Ok(op(&mut v)) },
                    Err(_) => Err(StackAction::panic_borrow(self)),
                }
            },
            _ => {
                Err(StackAction::panic_msg(format!(
                    "Can't do list operation with non list value: {}",
                    self.s())))
            }
        }
    }

    pub fn set_key(&self, key: &VVal, val: VVal) -> Result<(), StackAction> {
        match self {
            VVal::Ref(_)   => self.deref().set_key(key, val),
            VVal::CRef(_)  => self.deref().set_key(key, val),
            VVal::WWRef(_) => self.deref().set_key(key, val),
            VVal::DropFun(f) => f.v.set_key(key, val),
            VVal::Map(m) => {
                let ks = key.s_raw();
                match m.try_borrow_mut() {
                    Ok(mut r)  => { r.insert(ks, val); Ok(()) },
                    Err(_)     => Err(StackAction::panic_borrow(self)),
                }
            },
            VVal::Lst(l) => {
                let idx = key.i() as usize;
                match l.try_borrow_mut() {
                    Ok(mut v) => {
                        if v.len() <= idx {
                            v.resize(idx + 1, VVal::Nul);
                        }
                        v[idx] = val;
                        Ok(())
                    },
                    Err(_) => Err(StackAction::panic_borrow(self)),
                }
            },
            VVal::Usr(u) => u.set_key(key, val),
            _ => Ok(())
        }
    }

    pub fn pop(&self) -> VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().pop().unwrap_or(VVal::Nul)
        } else {
            VVal::Nul
        }
    }

    pub fn accum(&mut self, val: &VVal) {
        match self {
            VVal::Byt(b) => {
                let mut acc = b.borrow_mut();
                match val {
                    VVal::Int(i) => { acc.push(*i as u8); },
                    VVal::Flt(f) => { acc.push(*f as u8); },
                    VVal::Str(s) => { acc.extend_from_slice(s.borrow().as_bytes()); },
                    VVal::Sym(s) => { acc.extend_from_slice(s.borrow().as_bytes()); },
                    VVal::Byt(s) => { acc.extend_from_slice(&s.borrow()); },
                    VVal::Bol(b) => { acc.push(*b as u8); },
                    _ => { val.with_s_ref(|s: &str| acc.extend_from_slice(s.as_bytes())); }
                }
            },
            VVal::Str(a) => {
                let mut acc = a.borrow_mut();
                match val {
                    VVal::Str(s) => { acc.push_str(&s.borrow()); },
                    VVal::Sym(s) => { acc.push_str(&s.borrow()); },
                    VVal::Byt(s) => {
                        for b in s.borrow().iter() {
                            let b = *b as char;
                            acc.push(b);
                        }
                    },
                    _ => { val.with_s_ref(|s: &str| acc.push_str(s)); }
                }
            },
            VVal::Int(i) => { *i += val.i(); },
            VVal::Flt(f) => { *f += val.f(); },
            VVal::Lst(v) => { v.borrow_mut().push(val.clone()); },
            _ => (),
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
            VVal::Sym(l) => l.borrow().len(),
            _ => 0,
        }
    }

    pub fn s_len(&self) -> usize {
        match self {
            VVal::Str(s)  => s.borrow().chars().count(),
            VVal::Sym(s)  => s.borrow().chars().count(),
            VVal::Usr(s)  => s.s_raw().chars().count(),
            VVal::Byt(b)  => b.borrow().len(),
            VVal::Nul     => 0,
            _             => self.s().chars().count(),
        }
    }

    /// Returns the string data or converts the value to a string for displaying.
    /// The conversion format is not for reading the value in again via
    /// the WLambda parser, it's for accessing the data as pure as possible.
    ///
    /// Use this if you need the raw unescaped contents of VVal::Str, VVal::Sym,
    /// VVal::Byt and other VVals.
    ///
    /// As this is used usually for generating output a VVal::Nul is
    /// turned into an empty string
    ///
    /// **There is also `with_s_ref()` which allows you to work with a
    /// reference to the string, in case you don't need to own the copy!**
    ///
    /// ```
    /// use wlambda::VVal;
    ///
    /// assert_eq!(VVal::Nul.s_raw(), "");
    /// assert_eq!(VVal::new_str("Test").s_raw(), "Test");
    /// ```
    pub fn s_raw(&self) -> String {
        match self {
            VVal::Str(s)  => s.borrow().clone(),
            VVal::Sym(s)  => s.borrow().clone(),
            VVal::Usr(s)  => s.s_raw(),
            VVal::Byt(s)  => s.borrow().iter().map(|b| *b as char).collect(),
            VVal::Nul     => String::from(""),
            _             => self.s(),
        }
    }

    /// Like s_raw() but returns a reference to the string.
    ///
    /// Use this if you need the raw unescaped contents of VVal::Str, VVal::Sym,
    /// VVal::Byt and other VVals.
    ///
    /// As this is used usually for generating output a VVal::Nul is
    /// turned into an empty string
    ///
    /// ```
    /// use wlambda::VVal;
    ///
    /// VVal::Nul.with_s_ref(
    ///     |s: &str| assert_eq!(s, ""));
    ///
    /// VVal::new_str("Test").with_s_ref(
    ///     |s: &str| assert_eq!(s, "Test"));
    /// ```
    #[inline]
    pub fn with_s_ref<T, R>(&self, f: T) -> R
        where T: FnOnce(&str) -> R
    {
        match self {
            VVal::Str(s)  => f(&s.borrow()),
            VVal::Sym(s)  => f(&s.borrow()),
            VVal::Usr(s)  => f(&s.s_raw()),
            VVal::Byt(_)  => f(&self.s_raw()),
            VVal::Nul     => f(""),
            _             => f(&self.s_raw()),
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
                line: 0, col: 0,
                file: FileRef::new("?"),
                name: None,
            }
        }
    }

    pub fn set_syn(&mut self, syn: Syntax) {
        if let VVal::Syn(s) = self {
            s.syn = syn;
        }
    }

    pub fn set_syn_at(&mut self, idx: usize, syn: Syntax) {
        let mut v = self.v_(idx);
        v.set_syn(syn);
        self.set(idx, v);
    }

    pub fn get_syn(&self) -> Syntax {
        if let VVal::Syn(s) = self {
            s.syn.clone()
        } else {
            Syntax::Block
        }
    }

    pub fn compile_err(&self, msg: String) -> CompileError {
        CompileError {
            msg,
            pos: self.at(0).unwrap_or(VVal::Nul).get_syn_pos(),
        }
    }

    pub fn to_compile_err(&self, msg: String) -> Result<EvalNode, CompileError> {
        Err(self.compile_err(msg))
    }

    pub fn is_pair(&self) -> bool {
        match self { VVal::Pair(_) => true, _ => false }
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
            VVal::Err(_)     => String::from("error"),
            VVal::Bol(_)     => String::from("bool"),
            VVal::Sym(_)     => String::from("symbol"),
            VVal::Syn(_)     => String::from("syntax"),
            VVal::Int(_)     => String::from("integer"),
            VVal::Flt(_)     => String::from("float"),
            VVal::Pair(_)    => String::from("pair"),
            VVal::Lst(_)     => String::from("vector"),
            VVal::Map(_)     => String::from("map"),
            VVal::Usr(_)     => String::from("userdata"),
            VVal::Fun(_)     => String::from("function"),
            VVal::IVec(_)    => String::from("integer_vector"),
            VVal::FVec(_)    => String::from("float_vector"),
            VVal::DropFun(_) => String::from("drop_function"),
            VVal::Ref(_)     => String::from("strong"),
            VVal::CRef(_)    => String::from("weakable"),
            VVal::WWRef(_)   => String::from("weak"),
        }
    }

    /// Quick access method for retrieving the VVal at index `idx`.
    /// Returns VVal::Nul if the VVal is not a VVal::Lst or no such index exists.
    /// See also the shorthands `v_i`, `v_f`, `v_s` and `v_s_raw`.
    ///
    ///```
    /// use wlambda::VVal;
    /// let v = VVal::vec();
    /// v.push(VVal::Int(10));
    /// v.push(VVal::Int(11));
    ///
    /// assert_eq!(v.v_(1).i(), 11);
    /// assert_eq!(v.v_i(1),    11);
    ///```
    pub fn v_(&self, idx: usize) -> VVal { self.at(idx).unwrap_or(VVal::Nul) }

    /// Quick access method for retrieving the VVal at key `idx`.
    /// Returns VVal::Nul if the VVal is not a VVal::Map or no such index exists.
    /// See also the shorthands `v_ik`, `v_fk`, `v_sk` and `v_s_rawk`.
    ///
    ///```
    /// use wlambda::VVal;
    /// let v = VVal::map();
    /// v.set_map_key(String::from("aaa"), VVal::Int(12));
    /// v.set_map_key(String::from("abc"), VVal::Int(13));
    /// v.set_map_key(String::from("zyy"), VVal::Int(14));
    ///
    /// assert_eq!(v.v_k("abc").i(), 13);
    /// assert_eq!(v.v_ik("aaa"),    12);
    /// assert_eq!(v.v_ik("zyy"),    14);
    ///```
    pub fn v_k(&self, key: &str) -> VVal { self.get_key(key).unwrap_or(VVal::Nul) }
    /// Quick access of an integer at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(11));
    /// assert_eq!(v.v_i(0),    11);
    ///```
    pub fn v_i(&self, idx: usize)     -> i64 { self.v_(idx).i() }
    /// Quick access of the integer at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_map_key(String::from("aaa"), wlambda::VVal::new_str("10"));
    /// assert_eq!(v.v_ik("aaa"), 10);
    ///```
    pub fn v_ik(&self, key: &str)     -> i64 { self.v_k(key).i() }
    /// Quick access of a raw string at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(12));
    /// assert_eq!(v.v_s_raw(0), "12");
    ///```
    pub fn v_s_raw(&self, idx: usize) -> String { self.v_(idx).s_raw() }
    /// Quick access of a raw string as reference at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::new_str("12"));
    /// assert_eq!(v.v_with_s_ref(0, |s: &str| s.chars().nth(0).unwrap()), '1');
    ///```
    pub fn v_with_s_ref<T, R>(&self, idx: usize, f: T) -> R
        where T: FnOnce(&str) -> R
    {
        self.v_(idx).with_s_ref(f)
    }
    /// Quick access of the string at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_map_key(String::from("aaa"), wlambda::VVal::new_str("XYX"));
    /// assert_eq!(v.v_s_rawk("aaa"), "XYX");
    ///```
    pub fn v_s_rawk(&self, key: &str) -> String { self.v_k(key).s_raw() }
    /// Quick access of the string as reference at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_map_key(String::from("aaa"), wlambda::VVal::new_str("XYX"));
    /// assert!(v.v_with_s_refk("aaa", |s: &str| s == "XYX"));
    ///```
    pub fn v_with_s_refk<T, R>(&self, key: &str, f: T) -> R
        where T: FnOnce(&str) -> R
    {
        self.v_k(key).with_s_ref(f)
    }
    /// Quick access of the string representation at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(13));
    /// assert_eq!(v.v_s(0), "13");
    ///```
    pub fn v_s(&self, idx: usize)     -> String { self.v_(idx).s() }
    /// Quick access of the string represenation at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_map_key(String::from("aaa"), wlambda::VVal::Flt(12.2));
    /// assert_eq!(v.v_sk("aaa"), "12.2");
    ///```
    pub fn v_sk(&self, key: &str)     -> String { self.v_k(key).s() }
    /// Quick access of the float at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Flt(13.2));
    /// assert_eq!(v.v_f(0), 13.2);
    ///```
    pub fn v_f(&self, idx: usize)     -> f64 { self.v_(idx).f() }
    /// Quick access of the float at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_map_key(String::from("aaa"), wlambda::VVal::Flt(12.2));
    /// assert_eq!(v.v_fk("aaa"), 12.2);
    ///```
    pub fn v_fk(&self, key: &str)     -> f64 { self.v_k(key).f() }

    pub fn for_each<T>(&self, mut op: T)
        where T: FnMut(&VVal) -> () {
        if let VVal::Lst(b) = &self {
            for i in b.borrow().iter() { op(i); }
        }
    }

    pub fn for_eachk<T>(&self, mut op: T)
        where T: FnMut(&str, &VVal) -> () {
        if let VVal::Map(b) = &self {
            for (k, v) in b.borrow().iter() { op(&k, v); }
        }
    }

    #[allow(clippy::cast_lossless)]
    pub fn f(&self) -> f64 {
        match self {
            VVal::Str(s)     => (*s).borrow().parse::<f64>().unwrap_or(0.0),
            VVal::Sym(s)     => (*s).borrow().parse::<f64>().unwrap_or(0.0),
            VVal::Byt(s)     => if (*s).borrow().len() > 0 { (*s).borrow()[0] as f64 } else { 0.0 },
            VVal::Nul        => 0.0,
            VVal::Err(_)     => 0.0,
            VVal::Bol(b)     => if *b { 1.0 } else { 0.0 },
            VVal::Syn(s)     => (s.syn.clone() as i64) as f64,
            VVal::Int(i)     => *i as f64,
            VVal::Flt(f)     => *f,
            VVal::Pair(b)    => b.0.f(),
            VVal::Lst(l)     => l.borrow().len() as f64,
            VVal::Map(l)     => l.borrow().len() as f64,
            VVal::Usr(u)     => u.f(),
            VVal::Fun(_)     => 1.0,
            VVal::FVec(_)    => unimplemented!(),
            VVal::IVec(_)    => unimplemented!(),
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
            VVal::Sym(s)     => (*s).borrow().parse::<i64>().unwrap_or(0),
            VVal::Byt(s)     => if (*s).borrow().len() > 0 { (*s).borrow()[0] as i64 } else { 0 as i64 },
            VVal::Nul        => 0,
            VVal::Err(_)     => 0,
            VVal::Bol(b)     => if *b { 1 } else { 0 },
            VVal::Syn(s)     => s.syn.clone() as i64,
            VVal::Int(i)     => *i,
            VVal::Flt(f)     => (*f as i64),
            VVal::Pair(b)    => b.0.i(),
            VVal::Lst(l)     => l.borrow().len() as i64,
            VVal::Map(l)     => l.borrow().len() as i64,
            VVal::Usr(u)     => u.i(),
            VVal::Fun(_)     => 1,
            VVal::FVec(_)    => unimplemented!(),
            VVal::IVec(_)    => unimplemented!(),
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
            VVal::Sym(s)     => (*s).borrow().parse::<i64>().unwrap_or(0) != 0,
            VVal::Byt(s)     => (if (*s).borrow().len() > 0 { (*s).borrow()[0] as i64 } else { 0 as i64 }) != 0,
            VVal::Nul        => false,
            VVal::Err(_)     => false,
            VVal::Bol(b)     => *b,
            VVal::Syn(s)     => (s.syn.clone() as i64) != 0,
            VVal::Pair(b)    => b.0.b() || b.1.b(),
            VVal::Int(i)     => (*i) != 0,
            VVal::Flt(f)     => (*f as i64) != 0,
            VVal::Lst(l)     => (l.borrow().len() as i64) != 0,
            VVal::Map(l)     => (l.borrow().len() as i64) != 0,
            VVal::Usr(u)     => u.b(),
            VVal::Fun(_)     => true,
            VVal::FVec(_)    => unimplemented!(),
            VVal::IVec(_)    => unimplemented!(),
            VVal::DropFun(f) => f.v.b(),
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

    fn s_cy(&self, c: &mut CycleCheck) -> String {
        let br = if let Some((do_continue, backref_str)) = c.backref(self) {
            if !do_continue { return backref_str; }
            backref_str
        } else {
            String::from("")
        };
        let s = match self {
            VVal::Str(s)     => format_vval_str(&s.borrow(), false),
            VVal::Sym(s)     => format!(":\"{}\"", &s.borrow()),
            VVal::Byt(s)     => format!("$b{}", format_vval_byt(&s.borrow())),
            VVal::Nul        => "$n".to_string(),
            VVal::Err(e)     => format!("$e{} {}", (*e).borrow().1, (*e).borrow().0.s_cy(c)),
            VVal::Bol(b)     => if *b { "$true".to_string() } else { "$false".to_string() },
            VVal::Syn(s)     => format!("&{:?}", s.syn),
            VVal::Int(i)     => i.to_string(),
            VVal::Flt(f)     => f.to_string(),
            VVal::Pair(b)    => format!("$p({},{})", b.0.s_cy(c), b.1.s_cy(c)),
            VVal::Lst(l)     => VVal::dump_vec_as_str(l, c),
            VVal::Map(l)     => VVal::dump_map_as_str(l, c), // VVal::dump_map_as_str(l),
            VVal::Usr(u)     => u.s(),
            VVal::Fun(f)     => {
                let min = if f.min_args.is_none() { "any".to_string() }
                          else { format!("{}", f.min_args.unwrap()) };
                let max = if f.max_args.is_none() { "any".to_string() }
                          else { format!("{}", f.max_args.unwrap()) };
                let upvalues : String =
                    f.upvalues
                     .iter()
                     .map(|v| v.s_cy(c))
                     .collect::<Vec<String>>()
                     .join(",");
                if let Some(ref sp) = f.syn_pos {
                    format!("&F{{@{},amin={},amax={},locals={},upvalues=$[{}]}}",
                            sp, min, max, f.local_size, upvalues)
                } else {
                    format!("&F{{@[0,0:?()],amin={},amax={},locals={},upvalues=$[{}]}}",
                            min, max, f.local_size, upvalues)
                }
            },
            VVal::DropFun(f) => format!("std:to_drop[{}]", f.v.s_cy(c)),
            VVal::Ref(l)     => format!("$&&{}", (*l).borrow().s_cy(c)),
            VVal::CRef(l)    => format!("$&{}", (*l).borrow().s_cy(c)),
            VVal::FVec(nvec) => nvec.s(),
            VVal::IVec(nvec) => nvec.s(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => format!("$(&){}", v.borrow().s_cy(c)),
                    None => "$n".to_string(),
                }
            },
        };
        format!("{}{}", br, s)
    }

    pub fn as_bytes(&self) -> std::vec::Vec<u8> {
        match self {
            VVal::Byt(b)     => b.borrow().clone(),
            VVal::DropFun(f) => f.v.as_bytes(),
            VVal::Ref(l)     => (*l).borrow().as_bytes(),
            VVal::CRef(l)    => (*l).borrow().as_bytes(),
            VVal::WWRef(l)   => {
                match l.upgrade() {
                    Some(v) => v.borrow().as_bytes(),
                    None    => std::vec::Vec::new(),
                }
            },
            _ => self.with_s_ref(|s: &str| s.as_bytes().to_vec()),
        }
    }

    /// Returns a string representation of the VVal data structure.
    /// It handles cyclic data structures fine.
    /// The purpose is to return an unambigous represenation of the data
    /// structure. That means strings are quoted and VVal::Nul becomes `$n`
    /// for instance.
    ///
    /// If you need strings in pure form, use the `s_raw()` method.
    ///
    /// ```
    /// use wlambda::VVal;
    ///
    /// let v = VVal::Nul;
    /// assert_eq!(v.s(), "$n");
    ///
    /// assert_eq!(VVal::new_str("Foo").s(), "\"Foo\"");
    /// ```
    pub fn s(&self) -> String {
        let mut cc = CycleCheck::new();
        cc.touch_walk(self);
        self.s_cy(&mut cc)
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
            VVal::Str(_)     => self.with_s_ref(|s: &str| serializer.serialize_str(s)),
            VVal::Sym(_)     => self.with_s_ref(|s: &str| serializer.serialize_str(s)),
            VVal::Byt(b)     => serializer.serialize_bytes(&b.borrow()[..]),
            VVal::Nul        => serializer.serialize_none(),
            VVal::Err(_)     => serializer.serialize_str(&self.s()),
            VVal::Bol(b)     => serializer.serialize_bool(*b),
            VVal::Syn(_)     => serializer.serialize_str(&self.s()),
            VVal::Int(i)     => serializer.serialize_i64(*i),
            VVal::Flt(f)     => serializer.serialize_f64(*f),
            VVal::Pair(b)    => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&b.0)?;
                seq.serialize_element(&b.1)?;
                seq.end()
            },
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
            VVal::FVec(_)    => unimplemented!(),
            VVal::IVec(_)    => unimplemented!(),
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
            v.set_map_key(k.s_raw(), ve);
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
