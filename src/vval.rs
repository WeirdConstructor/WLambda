// This is a part of WLambda. See README.md and COPYING for details.

/*!

This module provides the core data structures used by the parser,
compiler and evaluator of WLambda.

*/

use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use std::rc::Weak;

use crate::compiler::{GlobalEnv, GlobalEnvRef};
use crate::nvec::{NVec, NVecDim};
use crate::ops::Prog;
use crate::str_int::*;

use std::sync::atomic::{AtomicUsize, Ordering};

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
    pub fn s(&self) -> &str {
        &(*self.s)
    }
}

#[derive(Clone, PartialEq)]
pub struct SynPosInfo {
    pub line: u32,
    pub col: u32,
    pub file: FileRef,
    pub name: Option<String>,
}

/// Structure for holding information about origin
/// of an AST node.
#[derive(Clone, PartialEq)]
pub struct SynPos {
    pub syn: Syntax,
    pub info: Rc<SynPosInfo>,
}

impl SynPos {
    pub fn empty() -> Self {
        Self {
            syn: Syntax::Block,
            info: Rc::new(SynPosInfo { line: 0, col: 0, file: FileRef::new("?"), name: None }),
        }
    }

    pub fn new(syn: Syntax, line: u32, col: u32, file: FileRef) -> Self {
        Self { syn, info: Rc::new(SynPosInfo { line, col, file, name: None }) }
    }

    pub fn has_info(&self) -> bool {
        self.info.line > 0
    }

    pub fn line(&self) -> u32 {
        self.info.line
    }
    pub fn col(&self) -> u32 {
        self.info.col
    }

    pub fn filename(&self) -> &str {
        self.info.file.s()
    }

    pub fn syn(&self) -> Syntax {
        self.syn
    }

    pub fn set_syn(&mut self, syn: Syntax) {
        self.syn = syn;
    }

    pub fn set_name(&mut self, name: &str) {
        let mut new_info = (*self.info).clone();
        new_info.name = Some(name.to_string());
        self.info = Rc::new(new_info);
    }

    pub fn s_short(&self) -> String {
        format!("({:?}[{}:{}])", self.syn, self.info.line, self.info.col)
    }

    pub fn s_only_pos(&self) -> String {
        format!("({}:[{}:{}])", self.info.file.s(), self.info.line, self.info.col)
    }
}

impl Display for SynPos {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.has_info() {
            if self.info.name.is_some() && !self.info.name.as_ref().unwrap().is_empty() {
                write!(
                    f,
                    "{}:{}:{} {:?}[{}]",
                    self.info.file.s(),
                    self.info.line,
                    self.info.col,
                    self.syn,
                    self.info.name.as_ref().unwrap()
                )
            } else {
                write!(
                    f,
                    "{}:{}:{} {:?}",
                    self.info.file.s(),
                    self.info.line,
                    self.info.col,
                    self.syn
                )
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
    pub pos: SynPos,
    pub msg: String,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{} Compilation Error: {}",
            self.pos.info.file, self.pos.info.line, self.pos.info.col, self.msg
        )
    }
}

/// Encodes the different types of AST nodes.
#[derive(Debug, Clone, Copy, PartialEq)]
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
    BinOpSomeOr,
    BinOpExtSomeOr,
    BinOpNoneOr,
    BinOpErrOr,
    BinOpOptOr,
    OpNewPair,
    OpCallLwR,
    OpCallRwL,
    OpCallApplyLwR,
    OpCallApplyRwL,
    OpColAddL,
    OpColAddR,
    TRoot,
    TNL,
    TComment,
    TDelim,
    TIdent,
    TBinOp,
    TOp,
    TNum,
    TLiteral,
    TValue,
    TArgList,
    TNone,
    TQ,
    T,
    Str,
    Lst,
    IVec,
    FVec,
    Opt,
    Iter,
    Map,
    Expr,
    Func,
    Block,
    Err,
    Call,
    Type,
    Apply,
    And,
    Or,
    Assign,
    Def,
    Ref,
    HRef,
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
    DumpVM,
    DebugPrint,
    MapSplice,
    VecSplice,
    Accum,
    GlobVar,
    Selector,
    Pattern,
    StructPattern,
    Formatter,
}

impl std::str::FromStr for Syntax {
    type Err = ();

    fn from_str(s: &str) -> Result<Syntax, ()> {
        match s {
            "Var" => Ok(Syntax::Var),
            "Key" => Ok(Syntax::Key),
            "SetKey" => Ok(Syntax::SetKey),
            "GetKey" => Ok(Syntax::GetKey),
            "GetKey2" => Ok(Syntax::GetKey2),
            "GetKey3" => Ok(Syntax::GetKey3),
            "GetSym" => Ok(Syntax::GetSym),
            "GetSym2" => Ok(Syntax::GetSym2),
            "GetSym3" => Ok(Syntax::GetSym3),
            "GetIdx" => Ok(Syntax::GetIdx),
            "GetIdx2" => Ok(Syntax::GetIdx2),
            "GetIdx3" => Ok(Syntax::GetIdx3),
            "BinOpAdd" => Ok(Syntax::BinOpAdd),
            "BinOpSub" => Ok(Syntax::BinOpSub),
            "BinOpMul" => Ok(Syntax::BinOpMul),
            "BinOpDiv" => Ok(Syntax::BinOpDiv),
            "BinOpMod" => Ok(Syntax::BinOpMod),
            "BinOpLe" => Ok(Syntax::BinOpLe),
            "BinOpLt" => Ok(Syntax::BinOpLt),
            "BinOpGe" => Ok(Syntax::BinOpGe),
            "BinOpGt" => Ok(Syntax::BinOpGt),
            "BinOpEq" => Ok(Syntax::BinOpEq),
            "BinOpSomeOr" => Ok(Syntax::BinOpSomeOr),
            "BinOpExtSomeOr" => Ok(Syntax::BinOpExtSomeOr),
            "BinOpNoneOr" => Ok(Syntax::BinOpNoneOr),
            "BinOpErrOr" => Ok(Syntax::BinOpErrOr),
            "BinOpOptOr" => Ok(Syntax::BinOpOptOr),
            "OpNewPair" => Ok(Syntax::OpNewPair),
            "OpCallLwR" => Ok(Syntax::OpCallLwR),
            "OpCallRwL" => Ok(Syntax::OpCallRwL),
            "OpCallApplyLwR" => Ok(Syntax::OpCallApplyLwR),
            "OpCallApplyRwL" => Ok(Syntax::OpCallApplyRwL),
            "OpColAddL" => Ok(Syntax::OpColAddL),
            "OpColAddR" => Ok(Syntax::OpColAddR),
            "TRoot" => Ok(Syntax::TRoot),
            "TBinOp" => Ok(Syntax::TBinOp),
            "TOp" => Ok(Syntax::TOp),
            "TNL" => Ok(Syntax::TNL),
            "TComment" => Ok(Syntax::TComment),
            "TDelim" => Ok(Syntax::TDelim),
            "TIdent" => Ok(Syntax::TIdent),
            "TNum" => Ok(Syntax::TNum),
            "TLiteral" => Ok(Syntax::TLiteral),
            "TValue" => Ok(Syntax::TValue),
            "TArgList" => Ok(Syntax::TArgList),
            "TNone" => Ok(Syntax::TNone),
            "T" => Ok(Syntax::T),
            "TQ" => Ok(Syntax::TQ),
            "Str" => Ok(Syntax::Str),
            "Lst" => Ok(Syntax::Lst),
            "IVec" => Ok(Syntax::IVec),
            "FVec" => Ok(Syntax::FVec),
            "Opt" => Ok(Syntax::Opt),
            "Iter" => Ok(Syntax::Iter),
            "Map" => Ok(Syntax::Map),
            "Expr" => Ok(Syntax::Expr),
            "Func" => Ok(Syntax::Func),
            "Block" => Ok(Syntax::Block),
            "Err" => Ok(Syntax::Err),
            "Call" => Ok(Syntax::Call),
            "Type" => Ok(Syntax::Type),
            "Apply" => Ok(Syntax::Apply),
            "And" => Ok(Syntax::And),
            "Or" => Ok(Syntax::Or),
            "Assign" => Ok(Syntax::Assign),
            "Def" => Ok(Syntax::Def),
            "Ref" => Ok(Syntax::Ref),
            "HRef" => Ok(Syntax::HRef),
            "WRef" => Ok(Syntax::WRef),
            "Deref" => Ok(Syntax::Deref),
            "CaptureRef" => Ok(Syntax::CaptureRef),
            "AssignRef" => Ok(Syntax::AssignRef),
            "DefGlobRef" => Ok(Syntax::DefGlobRef),
            "DefConst" => Ok(Syntax::DefConst),
            "SelfObj" => Ok(Syntax::SelfObj),
            "SelfData" => Ok(Syntax::SelfData),
            "Import" => Ok(Syntax::Import),
            "Export" => Ok(Syntax::Export),
            "DumpStack" => Ok(Syntax::DumpStack),
            "DumpVM" => Ok(Syntax::DumpVM),
            "DebugPrint" => Ok(Syntax::DebugPrint),
            "MapSplice" => Ok(Syntax::MapSplice),
            "VecSplice" => Ok(Syntax::VecSplice),
            "Accum" => Ok(Syntax::Accum),
            "GlobVar" => Ok(Syntax::GlobVar),
            "Selector" => Ok(Syntax::Selector),
            "Pattern" => Ok(Syntax::Pattern),
            "StructPattern" => Ok(Syntax::StructPattern),
            "Formatter" => Ok(Syntax::Formatter),
            _ => Err(()),
        }
    }
}

#[derive(Clone)]
pub struct Stdio {
    pub write: Rc<RefCell<dyn std::io::Write>>,
    pub read: Rc<RefCell<dyn std::io::BufRead>>,
}

impl Stdio {
    pub fn new_rust_std() -> Self {
        Self {
            write: Rc::new(RefCell::new(std::io::stdout())),
            read: Rc::new(RefCell::new(std::io::BufReader::new(std::io::stdin()))),
        }
    }

    pub fn new_from_mem(
        input: Rc<RefCell<std::io::Cursor<Vec<u8>>>>,
        output: Rc<RefCell<Vec<u8>>>,
    ) -> Self {
        Self { write: output, read: input }
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
const START_STACK_SIZE: usize = 512;

#[derive(Default, Debug, Clone, Copy)]
pub struct LoopInfo {
    pub pc: usize,
    pub uw_depth: usize,
    pub sp: usize,
    pub break_pc: usize,
}

impl LoopInfo {
    #[inline]
    pub fn new() -> Self {
        Self { pc: 0, uw_depth: 0, sp: 0, break_pc: 0 }
    }
}

/// Describes an action that needs to be done when returning from a function
/// or somehow jumps unpredictably around the VM prog.
#[derive(Debug, Clone)]
pub enum UnwindAction {
    Null,
    RestoreAccum(VVal, VVal),
    RestoreSP(usize),
    ClearLocals(usize, usize),
    RestoreSelf(VVal),
    RestoreLoopInfo(LoopInfo),
    RestoreIter(Option<Rc<RefCell<VValIter>>>),
    FunctionCall(usize, usize, usize),
}

/// The runtime environment of the evaluator.
///
/// The easiest way to get an instance of this, is to create
/// an EvalContext:
///```
/// use wlambda::{VVal, EvalContext};
///
/// let mut ctx = EvalContext::new_default();
/// let res =
///     VVal::new_str("a")
///     .call(&mut *ctx.local.borrow_mut(), &[VVal::new_str("b")])
///     .unwrap();
///
/// assert_eq!(res.s(), "\"ab\"");
///```
#[derive(Debug)]
pub struct Env {
    /// The argument stack, initialized to a size of `START_STACK_SIZE`.
    pub args: std::vec::Vec<VVal>,
    /// A stack of the currently called functions.
    ///
    /// Used for accessing the up values, backtrace
    /// and other details about the function.
    pub call_stack: std::vec::Vec<Rc<VValFun>>,
    /// A stack that holds cleanup routines that need to be handled:
    pub unwind_stack: std::vec::Vec<UnwindAction>,
    /// Holds the object of the currently called method:
    pub current_self: VVal,
    /// The basepointer to reference arguments and
    /// local variables.
    ///
    /// - `bp + n (n >= 0)` references a local variable
    /// - `bp - n (n > 0)` references an argument
    pub bp: usize,
    /// The current stack pointer.
    pub sp: usize,
    /// The argument count to the current call.
    pub argc: usize,
    /// A user defined variable that holds user context information.
    /// See also the [with_user_do](struct.Env.html#method.with_user_do) function.
    pub user: Rc<RefCell<dyn std::any::Any>>,
    /// The exported names of this module.
    pub exports: FnvHashMap<Symbol, VVal>,
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
    /// A counter that counts the nesting depth in vm() calls
    pub vm_nest: usize,
    /// Holds information to process 'next' and 'break' for loop
    /// constructs:
    pub loop_info: LoopInfo,
    /// Holds the current iterator for the 'iter' construct.
    pub iter: Option<Rc<RefCell<VValIter>>>,
}

//impl Default for Env {
//    fn default() -> Self { Self::new() }
//}
//
impl Env {
    pub fn new(global: GlobalEnvRef) -> Env {
        let mut e = Env {
            args: Vec::with_capacity(START_STACK_SIZE),
            current_self: VVal::None,
            bp: 0,
            sp: 0,
            argc: 0,
            user: Rc::new(RefCell::new(VVal::vec())),
            exports: FnvHashMap::with_capacity_and_hasher(5, Default::default()),
            stdio: Stdio::new_rust_std(),
            accum_fun: VVal::None,
            accum_val: VVal::None,
            call_stack: vec![],
            unwind_stack: vec![],
            loop_info: LoopInfo::new(),
            iter: None,
            vm_nest: 0,
            global,
        };
        e.args.resize(START_STACK_SIZE, VVal::None);
        e
    }

    pub fn new_with_user(global: GlobalEnvRef, user: Rc<RefCell<dyn std::any::Any>>) -> Env {
        let mut e = Env {
            args: Vec::with_capacity(START_STACK_SIZE),
            current_self: VVal::None,
            bp: 0,
            sp: 0,
            argc: 0,
            exports: FnvHashMap::with_capacity_and_hasher(2, Default::default()),
            stdio: Stdio::new_rust_std(),
            accum_fun: VVal::None,
            accum_val: VVal::None,
            call_stack: vec![],
            unwind_stack: std::vec::Vec::with_capacity(1000),
            loop_info: LoopInfo::new(),
            iter: None,
            vm_nest: 0,
            user,
            global,
        };
        e.args.resize(START_STACK_SIZE, VVal::None);
        e
    }

    /// Derives a new clean environment from the current one. Things like the
    /// Stdio and the global/user data will be cloned from the current environment.
    /// This is useful for cases where you want to store the Env inside a
    /// pure Rust closure to call WLambda functions later.
    pub fn derive(&self) -> Self {
        let mut e = Self::new_with_user(self.global.clone(), self.user.clone());
        e.set_stdio(self.stdio.clone());
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
    ///     Ok(VVal::None)
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
    where
        F: Fn(&mut T) -> X,
    {
        let mut any = self.user.borrow_mut();
        let ref_reg = any.downcast_mut::<T>().unwrap();
        f(ref_reg)
    }

    pub fn export_name(&mut self, name: &str, value: &VVal) {
        self.exports.insert(s2sym(name), value.clone());
    }

    #[inline]
    pub fn set_bp(&mut self, env_size: usize) -> usize {
        let new_bp = self.sp;
        self.sp += env_size;
        if self.sp >= self.args.len() {
            self.args.resize(self.sp * 2, VVal::None);
        }
        std::mem::replace(&mut self.bp, new_bp)
    }

    #[inline]
    pub fn reset_bp(&mut self, env_size: usize, oldbp: usize) {
        for i in self.bp..self.sp {
            self.args[i] = VVal::None;
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
    where
        T: Fn(&mut Env) -> Result<VVal, StackAction>,
    {
        let old_self = std::mem::replace(&mut self.current_self, object);
        let ret = f(self);
        self.current_self = old_self;
        ret
    }

    #[inline]
    pub fn with_local_call_info<T>(&mut self, argc: usize, f: T) -> Result<VVal, StackAction>
    where
        T: Fn(&mut Env) -> Result<VVal, StackAction>,
    {
        let local_size = 0;
        let old_argc = std::mem::replace(&mut self.argc, argc);
        let old_bp = self.set_bp(local_size);

        let ret = f(self);

        self.reset_bp(local_size, old_bp);
        self.argc = old_argc;

        ret
    }

    #[inline]
    pub fn with_restore_sp<T>(&mut self, f: T) -> Result<VVal, StackAction>
    where
        T: Fn(&mut Env) -> Result<VVal, StackAction>,
    {
        let old_sp = self.sp;
        let ret = f(self);
        self.popn(self.sp - old_sp);
        ret
    }

    #[inline]
    pub fn push_sp(&mut self, n: usize) {
        self.sp += n;
        if self.sp >= self.args.len() {
            self.args.resize(self.sp * 2, VVal::None);
        }
        //d// println!("PUSH_SP {} => {}", n, self.sp);
    }

    #[inline]
    pub fn push(&mut self, v: VVal) -> usize {
        if self.sp >= self.args.len() {
            self.args.resize(self.sp * 2, VVal::None);
        }
        self.args[self.sp] = v;
        self.sp += 1;
        self.sp - 1
    }

    #[inline]
    pub fn stk(&self, offs: usize) -> &VVal {
        &self.args[self.sp - offs] // implicit self.sp - 1! We must not call with 0!
    }

    #[inline]
    pub fn stk_i(&self, offs: usize) -> i64 {
        if let VVal::Int(i) = &self.args[(self.sp - 1) + offs] {
            *i
        } else {
            0
        }
    }

    #[inline]
    pub fn inc_local(&mut self, idx: usize, inc: i16) -> i64 {
        if let VVal::Int(i) = &mut self.args[self.bp + idx] {
            if inc > 0 {
                *i += 1;
            } else {
                *i -= 1;
            }

            *i
        } else {
            0
        }
    }

    #[inline]
    pub fn pop(&mut self) -> VVal {
        if self.sp < 1 {
            panic!("Stack pointer underflow {} {}", self.sp, 1);
        }
        self.sp -= 1;
        std::mem::replace(&mut self.args[self.sp], VVal::None)
    }

    #[inline]
    pub fn null_locals(&mut self, from: usize, to: usize) {
        for i in from..to {
            self.args[self.bp + i] = VVal::None;
        }
    }

    #[inline]
    pub fn popn(&mut self, n: usize) {
        if self.sp < n {
            panic!("Stack pointer underflow {} {}", self.sp, n);
        }
        if n > 0 {
            //d// println!("SP={}, N={}", self.sp, n);
            for i in (self.sp - n)..self.sp {
                //d// println!("POP[{}] {} [of {}]", i, self.args[i].s(), n);
                self.args[i] = VVal::None;
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
            if i == self.bp {
                mark = format!("{} BP", mark);
            }
            if i == self.sp {
                mark = format!("{} SP", mark);
            }
            if !mark.is_empty() {
                mark = format!("{} ->", mark);
            }

            println!("    {:9} [{:3}] = {}", mark, i, v.s());
            if i >= (1 + self.sp) {
                break;
            }
        }
        if !self.call_stack.is_empty() {
            for (i, u) in self.call_stack.last().unwrap().upvalues.iter().enumerate() {
                println!("  UP[{:3}] = {}", i, u.s());
            }
        }
    }

    #[inline]
    pub fn stk2vec(&self, count: usize) -> VVal {
        let v = VVal::vec();
        for i in 0..count {
            v.push(self.args[self.sp - (count - i)].clone());
        }
        v
    }

    #[inline]
    pub fn argv(&self) -> VVal {
        VVal::vec_from(&self.args[(self.bp - self.argc)..self.bp])
    }

    #[inline]
    pub fn argv_ref(&self) -> &[VVal] {
        &self.args[(self.bp - self.argc)..self.bp]
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
        match &self.call_stack.last().unwrap().upvalues[i] {
            VVal::HRef(hr) => hr.borrow().clone(),
            VVal::WWRef(r) => match r.upgrade() {
                Some(v) => v.borrow().clone(),
                None => VVal::None,
            },
            v => v.clone(),
        }
    }

    #[inline]
    pub fn arg_ref(&self, i: usize) -> Option<&VVal> {
        if i >= self.argc {
            return None;
        }
        Some(&self.args[(self.bp - self.argc) + i])
    }

    #[inline]
    pub fn arg_err_internal(&self, i: usize) -> Option<VVal> {
        let v = &self.args[(self.bp - self.argc) + i];
        match v {
            VVal::Err(_) => Some(v.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn arg(&self, i: usize) -> VVal {
        //d// println!("GET ARGC [{}] = {}", i, self.argc);
        if i >= self.argc {
            return VVal::None;
        }
        let v = &self.args[(self.bp - self.argc) + i];
        //d// println!("GET ARG [{}/{}] = {}", i, self.sp - (i + 1), v.s());
        v.clone()
    }

    pub fn get_local_up_promotion(&mut self, i: usize) -> VVal {
        let idx = self.bp + i;
        match &self.args[idx] {
            VVal::HRef(r) => VVal::HRef(r.clone()),
            //            VVal::Ref(r)   => VVal::Ref(r.clone()),
            VVal::WWRef(r) => VVal::WWRef(r.clone()),
            v => {
                let new_v = v.to_hidden_boxed_ref();
                self.args[idx] = new_v.clone();
                new_v
            }
        }
    }

    pub fn get_local_captured_ref(&self, i: usize) -> VVal {
        let idx = self.bp + i;
        self.args[idx].to_ref()
    }

    #[inline]
    pub fn reg(&self, i: i32) -> VVal {
        if i >= 0 {
            self.get_local(i as usize)
        } else {
            self.stk((-i) as usize).clone()
        }
    }

    #[inline]
    pub fn get_local(&self, i: usize) -> VVal {
        match &self.args[self.bp + i] {
            VVal::HRef(r) => r.borrow().clone(),
            v => v.clone(),
        }
    }

    pub fn assign_ref_up(&mut self, i: usize, value: VVal) {
        let fun = self.call_stack.last().unwrap().clone();
        let upv = &fun.upvalues[i];

        match upv {
            //            VVal::Ref(r)     => { r.replace(value); }
            VVal::HRef(r) => {
                r.borrow_mut().assign_ref(value);
            }
            VVal::WWRef(l) => {
                if let Some(r) = l.upgrade() {
                    r.borrow_mut().assign_ref(value);
                }
            }
            _ => (),
        }
    }

    pub fn assign_ref_local(&mut self, i: usize, value: VVal) {
        let idx = self.bp + i;
        self.args[idx].assign_ref(value);
    }

    pub fn set_up(&mut self, index: usize, value: VVal) {
        let fun = self.call_stack.last().unwrap().clone();
        let upv = &fun.upvalues[index];

        match upv {
            //            VVal::Ref(r)   => { r.replace(value); }
            VVal::HRef(r) => {
                r.replace(value);
            }
            VVal::WWRef(r) => {
                if let Some(r) = Weak::upgrade(r) {
                    r.replace(value);
                }
            }
            _ => {}
        }
    }

    #[inline]
    pub fn set_consume(&mut self, i: usize, value: VVal) {
        let idx = self.bp + i;
        if idx >= self.args.len() {
            self.args.resize(idx * 2, VVal::None);
        }
        match &mut self.args[idx] {
            VVal::HRef(r) => {
                r.replace(value);
            }
            v => *v = value,
        }
    }

    #[inline]
    pub fn unwind_depth(&self) -> usize {
        self.unwind_stack.len()
    }

    #[inline]
    pub fn push_unwind(&mut self, uwa: UnwindAction) {
        self.unwind_stack.push(uwa);
    }

    #[inline]
    pub fn unwind_to_depth(&mut self, depth: usize) {
        while self.unwind_stack.len() > depth {
            self.unwind_one();
        }
    }

    #[inline]
    pub fn push_fun_call(&mut self, fu: Rc<VValFun>, argc: usize) {
        let local_size = fu.local_size;
        let old_bp = self.set_bp(local_size);
        let uwa =
            UnwindAction::FunctionCall(std::mem::replace(&mut self.argc, argc), old_bp, local_size);
        self.push_unwind(uwa);
        self.call_stack.push(fu);
    }

    #[inline]
    pub fn push_clear_locals(&mut self, from: usize, to: usize) {
        self.push_unwind(UnwindAction::ClearLocals(from, to));
    }

    #[inline]
    pub fn push_unwind_self(&mut self, new_self: VVal) {
        let uwa = UnwindAction::RestoreSelf(std::mem::replace(&mut self.current_self, new_self));
        self.push_unwind(uwa);
    }

    #[inline]
    pub fn cleanup_loop(&mut self) {
        while self.sp > self.loop_info.sp {
            self.pop();
        }
        self.unwind_to_depth(self.loop_info.uw_depth);
    }

    #[inline]
    pub fn push_loop_info(&mut self, current_pc: usize, break_pc: usize, uw_depth_offs: usize) {
        let uw_depth = self.unwind_depth() + 1 + uw_depth_offs;
        let uwa = UnwindAction::RestoreLoopInfo(std::mem::replace(
            &mut self.loop_info,
            LoopInfo { pc: current_pc, sp: self.sp, uw_depth, break_pc },
        ));
        self.push_unwind(uwa);
    }

    #[inline]
    pub fn push_iter(&mut self, iter: Rc<RefCell<VValIter>>) {
        let uwa = UnwindAction::RestoreIter(std::mem::replace(&mut self.iter, Some(iter)));
        self.push_unwind(uwa);
    }

    pub fn dump_unwind_stack(&self) -> String {
        let mut s = String::new();
        for i in 0..self.unwind_stack.len() {
            let add = match &self.unwind_stack[self.unwind_stack.len() - (i + 1)] {
                UnwindAction::RestoreSP(sp) => format!("rsp({})", *sp),
                UnwindAction::ClearLocals(from, to) => format!("cl({},{})", *from, *to),
                UnwindAction::RestoreLoopInfo(li) => {
                    format!("loinf(uws:{},sp:{})", li.uw_depth, li.sp)
                }
                UnwindAction::RestoreAccum(_fun, _val) => "raccm".to_string(),
                UnwindAction::RestoreSelf(_slf) => "rslf".to_string(),
                UnwindAction::RestoreIter(_i) => "ritr".to_string(),
                UnwindAction::FunctionCall(argc, old_bp, local_size) => {
                    format!("fcal({},{},{})", argc, old_bp, local_size)
                }
                UnwindAction::Null => "nul".to_string(),
            };

            if !s.is_empty() {
                s += ";";
            }
            s += &add[..];
        }
        s
    }

    #[inline]
    pub fn unwind(&mut self, ua: UnwindAction) {
        match ua {
            UnwindAction::RestoreSP(sp) => {
                while self.sp > sp {
                    self.pop();
                }
            }
            UnwindAction::ClearLocals(from, to) => {
                self.null_locals(from, to);
            }
            UnwindAction::RestoreLoopInfo(li) => {
                self.loop_info = li;
            }
            UnwindAction::RestoreAccum(fun, val) => {
                self.accum_fun = fun;
                self.accum_val = val;
            }
            UnwindAction::RestoreSelf(slf) => {
                self.current_self = slf;
            }
            UnwindAction::RestoreIter(i) => {
                self.iter = i;
            }
            UnwindAction::FunctionCall(argc, old_bp, local_size) => {
                self.reset_bp(local_size, old_bp);
                self.call_stack.pop();
                self.argc = argc;
            }
            UnwindAction::Null => (),
        }
    }

    #[inline]
    pub fn unwind_one(&mut self) {
        let uwa = self.unwind_stack.pop().unwrap();
        self.unwind(uwa);
    }

    pub fn setup_accumulator(&mut self, v: VVal) {
        let f = match v {
            VVal::Map(_) => VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    let k = env.arg(0);
                    let v = env.arg(1);
                    env.accum_val.set_key(&k, v.clone())?;
                    Ok(v)
                },
                Some(2),
                Some(2),
                false,
            ),
            _ => VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    let v = env.arg(0);
                    env.accum_val.accum(&v);
                    Ok(v)
                },
                Some(1),
                Some(1),
                false,
            ),
        };

        let uwa = UnwindAction::RestoreAccum(
            std::mem::replace(&mut self.accum_fun, f),
            std::mem::replace(&mut self.accum_val, v),
        );
        self.push_unwind(uwa);
    }

    pub fn with_accum<T>(&mut self, v: VVal, acfun: T) -> Result<VVal, StackAction>
    where
        T: Fn(&mut Env) -> Result<VVal, StackAction>,
    {
        self.setup_accumulator(v);

        let ret = acfun(self);

        let val = self.accum_val.clone();
        self.unwind_one();

        ret?;
        Ok(val)
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
                return VVal::err(VVal::new_str_mv(s), i.syn_pos.clone().unwrap());
            }
        }

        if self.call_stack.last().is_some() {
            VVal::err(
                VVal::new_str_mv(s),
                self.call_stack
                    .last()
                    .unwrap()
                    .syn_pos
                    .clone()
                    .or_else(|| Some(SynPos::empty()))
                    .unwrap(),
            )
        } else {
            VVal::err(VVal::new_str_mv(s), SynPos::empty())
        }
    }

    pub fn new_panic(&self, val: VVal) -> StackAction {
        for i in self.call_stack.iter().rev() {
            if i.syn_pos.is_some() {
                return StackAction::panic(val, i.syn_pos.clone(), VVal::None);
            }
        }

        StackAction::panic(
            val,
            self.call_stack.last().unwrap().syn_pos.clone().or_else(|| Some(SynPos::empty())),
            VVal::None,
        )
    }

    pub fn new_panic_argv(&self, val: VVal, args: VVal) -> StackAction {
        for i in self.call_stack.iter().rev() {
            if i.syn_pos.is_some() {
                return StackAction::panic(val, i.syn_pos.clone(), args);
            }
        }

        StackAction::panic(
            val,
            self.call_stack.last().unwrap().syn_pos.clone().or_else(|| Some(SynPos::empty())),
            args,
        )
    }
}

/// Encodes all kinds of jumps up the call stack, like `break` and `next` in Loops.
///
/// As WLambda is not using a VM, it uses return values of the
/// closure call tree to handle jumping up the stack.
///
/// If you implement some loop that calls into WLambda, you can handle Break and Next accordingly!
/// You could even do other nifty stuff witht the return label.
///
/// Here is an example how to handle a `StackAction` result in the context of a simple
/// call:
///```
/// use wlambda::*;
/// let mut ctx = EvalContext::new_default();
///
/// let func = ctx.eval("{ _ + _1 }").unwrap();
/// let val = match ctx.call(&func, &[VVal::Int(1), VVal::Int(2)]) {
///     Ok(v) => v,
///     Err(StackAction::Return(val)) => {
///         // ignore the label in val.0:
///         val.1.clone()
///     }
///     Err(StackAction::Break(val)) => {
///         val.as_ref().clone()
///     }
///     Err(StackAction::Next) => {
///         VVal::None
///     }
///     Err(panic) => {
///         // Yes, you can just print the StackAction directly too:
///         panic!("{}", panic)
///     }
/// };
///
/// assert_eq!(val.i(), 3);
///```
#[derive(Clone)]
#[allow(clippy::type_complexity)]
pub enum StackAction {
    /// A panic was triggered. The first value in the pair is the panic value, usually an error message.
    /// The rest is the syntactic position information and the argument vector, if one
    /// was available at the panic site.
    Panic(Box<(VVal, Vec<(Option<SynPos>, VVal)>)>),
    /// A `return` was called. The second value of the pair is the return value.
    /// The first value is the label to return to.
    Return(Box<(VVal, VVal)>),
    /// A `break` was called with the given break value.
    Break(Box<VVal>),
    /// A `next` was called.
    Next,
}

fn fmt_shorten_ellipses(f: &mut Formatter, len: &mut usize, s: String) -> std::fmt::Result {
    if *len > 250 {
        return Ok(());
    }

    if s.len() > 35 {
        *len += 35 + 3;
        write!(f, "{}...", &s[0..35])
    } else {
        *len += s.len();
        write!(f, "{}", s)
    }
}

fn fmt_argv(f: &mut Formatter, v: &VVal) -> std::fmt::Result {
    let mut cur_len = 0;

    if !v.is_map() && v.iter_over_vvals() {
        write!(f, "[")?;
        v.with_iter(|it| {
            let mut first = true;
            for (v, _) in it {
                if cur_len > 250 {
                    break;
                }

                if first {
                    first = false;
                } else {
                    cur_len += 2;
                    write!(f, ", ")?;
                }

                fmt_shorten_ellipses(f, &mut cur_len, v.s())?;
            }

            Ok(()) as std::fmt::Result
        })?;

        if cur_len > 250 {
            write!(f, "...")?
        }

        write!(f, "]")?;
    } else {
        fmt_shorten_ellipses(f, &mut cur_len, v.s())?
    }

    Ok(())
}

impl Display for StackAction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            StackAction::Panic(panic) => {
                if panic.1.is_empty() {
                    write!(f, "Panic: {}", panic.0.s_raw())
                } else {
                    writeln!(f, "Panic: {}", panic.0.s_raw())?;

                    for t in panic.1.iter() {
                        write!(f, "    ")?;

                        if let (Some(p), v) = t {
                            write!(f, "{} ", p)?;
                            fmt_argv(f, v)?;
                        } else {
                            write!(f, "    ")?;
                            fmt_argv(f, &t.1)?;
                        }

                        writeln!(f)?;
                    }

                    Ok(())
                }
            }
            StackAction::Return(ret) => write!(f, "Return[lbl={}] {}", ret.0.s(), ret.1.s()),
            StackAction::Break(v) => write!(f, "Break: {}", v.s()),
            StackAction::Next => write!(f, "Next"),
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
        StackAction::Panic(Box::new((VVal::new_str_mv(err), vec![])))
    }

    pub fn panic_str(err: String, sp: Option<SynPos>, args: VVal) -> Self {
        StackAction::Panic(Box::new((VVal::new_str_mv(err), vec![(sp, args)])))
    }

    pub fn panic(err: VVal, sp: Option<SynPos>, args: VVal) -> Self {
        StackAction::Panic(Box::new((err, vec![(sp, args)])))
    }

    pub fn wrap_panic(self, sp: Option<SynPos>, args: VVal) -> Self {
        match self {
            StackAction::Panic(mut panic) => {
                panic.as_mut().1.push((sp, args));
                StackAction::Panic(panic)
            }
            _ => self,
        }
    }
}

impl From<VVal> for StackAction {
    fn from(v: VVal) -> StackAction {
        StackAction::panic(v, None, VVal::None)
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

pub type EvalNode = Box<dyn Fn(&mut Env) -> Result<VVal, StackAction>>;
pub type ClosNodeRef = Rc<RefCell<dyn Fn(&mut Env, usize) -> Result<VVal, StackAction>>>;

#[derive(Clone)]
pub enum FunType {
    ClosureNode(ClosNodeRef),
    VMProg(Rc<Prog>),
}

#[derive(Clone)]
/// This structure is the runtime representation of a WLambda function value.
pub struct VValFun {
    /// The closure or vm program that runs the function.
    pub fun: FunType,
    /// The positions of the upvalues that are being captured by this function.
    pub upvalue_pos: Rc<std::vec::Vec<VarPos>>,
    /// Contains any caught upvalues.
    pub upvalues: std::vec::Vec<VVal>,
    /// The number of local variables defined in this functions.
    ///
    /// This value is used to reserve stack space for storing them.
    pub local_size: usize,
    /// The function type
    pub typ: Option<Rc<Type>>,
    /// Min number of arguments this functions requires.
    pub min_args: Option<usize>,
    /// Max number of arguments this functions requires.
    pub max_args: Option<usize>,
    /// If true, then this function accepts error values without panic.
    /// Functions by default don't accept errors as argument. It needs to be
    /// explicitly enabled.
    pub err_arg_ok: bool,
    /// The location of the definition of this function.
    pub syn_pos: Option<SynPos>,
    /// The return label of the function:
    pub label: VVal,
}

impl VValFun {
    /// Creates a new VVal containing the given closure with the given minimum
    /// and maximum parameters (see also [`add_func` of GlobalEnv](compiler/struct.GlobalEnv.html#method.add_func)).
    ///
    /// There is also a new more convenient (because provided by VVal itself)
    /// function: `VVal::new_fun` which has the same parameters.
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
    pub fn new_fun<T>(
        fun: T,
        min_args: Option<usize>,
        max_args: Option<usize>,
        err_arg_ok: bool,
    ) -> VVal
    where
        T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction>,
    {
        VValFun::new_val(
            Rc::new(RefCell::new(fun)),
            Vec::new(),
            0,
            min_args,
            max_args,
            err_arg_ok,
            None,
            Rc::new(vec![]),
        )
    }

    pub fn new_fun_with_pos<T>(
        fun: T,
        min_args: Option<usize>,
        max_args: Option<usize>,
        err_arg_ok: bool,
        spos: SynPos,
    ) -> VVal
    where
        T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction>,
    {
        VValFun::new_val(
            Rc::new(RefCell::new(fun)),
            Vec::new(),
            0,
            min_args,
            max_args,
            err_arg_ok,
            Some(spos),
            Rc::new(vec![]),
        )
    }

    /// Internal utility function. Use at your own risk, API might change.
    #[allow(clippy::too_many_arguments)]
    pub fn new_val(
        fun: ClosNodeRef,
        upvalues: std::vec::Vec<VVal>,
        env_size: usize,
        min_args: Option<usize>,
        max_args: Option<usize>,
        err_arg_ok: bool,
        syn_pos: Option<SynPos>,
        upvalue_pos: Rc<std::vec::Vec<VarPos>>,
    ) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalue_pos,
            upvalues,
            fun: FunType::ClosureNode(fun),
            local_size: env_size,
            typ: None,
            min_args,
            max_args,
            err_arg_ok,
            syn_pos,
            label: VVal::None,
        }))
    }

    /// Internal utility function. Use at your own risk, API might change.
    #[allow(clippy::too_many_arguments)]
    pub fn new_prog(
        prog: Rc<Prog>,
        upvalues: std::vec::Vec<VVal>,
        env_size: usize,
        min_args: Option<usize>,
        max_args: Option<usize>,
        err_arg_ok: bool,
        syn_pos: Option<SynPos>,
        upvalue_pos: Rc<std::vec::Vec<VarPos>>,
        label: VVal,
    ) -> VVal {
        VVal::Fun(Rc::new(VValFun {
            upvalue_pos,
            upvalues,
            fun: FunType::VMProg(prog),
            local_size: env_size,
            typ: None,
            min_args,
            max_args,
            err_arg_ok,
            syn_pos,
            label,
        }))
    }

    /// Returns a dummy function that does nothing.
    pub fn new_dummy() -> Rc<VValFun> {
        Rc::new(VValFun {
            fun: FunType::ClosureNode(Rc::new(RefCell::new(|_: &mut Env, _a: usize| {
                Ok(VVal::None)
            }))),
            upvalue_pos: Rc::new(vec![]),
            upvalues: Vec::new(),
            local_size: 0,
            typ: None,
            min_args: None,
            max_args: None,
            err_arg_ok: false,
            syn_pos: None,
            label: VVal::None,
        })
    }

    /// Dumps captured up values of this function. Useful only if you want to
    /// debug functions/closures creates by WLambda code.
    pub fn dump_upvals(&self) -> VVal {
        let v = VVal::vec();
        for uv in self.upvalues.iter() {
            v.push(uv.clone());
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
///     fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
///         let args = env.argv_ref();
///         match key {
///             "test" => Ok(VVal::Int(42)),
///             _ => Ok(VVal::err_msg(&format!("Unknown method called: {}", key))),
///         }
///     }
///     fn call(&self, env: &mut Env) -> Result<VVal, StackAction> {
///         let args = env.argv_ref();
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
///         Ok(VVal::new_usr(MyType { x: Rc::new(RefCell::new((13, 42))) }))
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
///                 VVal::None
///             }
///         } else { VVal::None })
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
///     fn into(self) -> VVal { VVal::new_usr(self) }
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
    fn s(&self) -> String {
        format!("$<userdata:{:p}>", self)
    }
    /// If your data has a plain string representation,
    /// you can return the string directly from here.
    fn s_raw(&self) -> String {
        self.s()
    }
    /// Returns the i64 representation of your data.
    fn i(&self) -> i64 {
        -1
    }
    /// Returns the byte representation of your data.
    fn byte(&self) -> u8 {
        self.i() as u8
    }
    /// Returns the char representation of your data.
    fn c(&self) -> char {
        std::char::from_u32(self.i() as u32).unwrap_or('?')
    }
    /// Returns the f64 representation of your data.
    fn f(&self) -> f64 {
        self.i() as f64
    }
    /// Returns the boolean representation of your data. Can for instance
    /// be used to check if your data is _valid_ or something.
    fn b(&self) -> bool {
        true
    }
    /// Allows you to specify how two instances of your data
    /// should be compared for equivalentness.
    fn eqv(&self, _other: &Box<dyn VValUserData>) -> bool {
        false
    }
    /// Should clone your user data instance. Whether you are doing
    /// a deep clone or a shallow clone or something else is up to you.
    fn clone_ud(&self) -> Box<dyn VValUserData>;
    /// Makes your user data act like a map. This can be useful
    /// for implementing your own registries or data structures.
    /// Implement this method for setting a key to a value.
    fn set_key(&self, _key: &VVal, _val: VVal) -> Result<(), StackAction> {
        Ok(())
    }
    /// This method is called when the user wants to remove a key.
    /// Typically called when `std:delete` from the prelude is called.
    fn delete_key(&self, _key: &VVal) -> Result<VVal, StackAction> {
        Ok(VVal::None)
    }
    /// This method returns some value that your user data
    /// associates with the given key.
    fn get_key(&self, _key: &str) -> Option<VVal> {
        None
    }
    /// This method is called, when the user data object is used in a method call directly.
    /// Use this to implement convenient APIs for the user of the user data object.
    /// To quickly get the arguments you may use `env.argv_ref()`.
    fn call_method(&self, _key: &str, _env: &mut Env) -> Result<VVal, StackAction> {
        Ok(VVal::None)
    }
    /// This method is called when the user data is called.
    /// To quickly get the arguments you may use `env.argv_ref()`.
    fn call(&self, _env: &mut Env) -> Result<VVal, StackAction> {
        Ok(VVal::None)
    }
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

    /// This function is called when you try to pass a user data value
    /// between threads via the Atoms provided by the thread implementation
    /// of WLambda.
    ///
    /// You need to return something that implements the `ThreadSafeUsr` trait,
    /// that handles transformation into and from an `AVal`.
    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        None
    }
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
pub struct DropFun {
    pub fun: VVal,
}

impl Drop for DropFun {
    #[allow(unused_must_use)]
    fn drop(&mut self) {
        let global = GlobalEnv::new_default();
        let mut e = Env::new(global);
        if let Err(e) = self.fun.call_internal(&mut e, 0) {
            eprintln!("Error in drop function: {}", e);
        }
    }
}

/// This type gives the end where to add the `VVal::add` function.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CollectionAdd {
    Push,
    Unshift,
    Uniq,
}

/// A type to describe if a type is fully resolved/determined.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TypeResolve {
    UnboundVars, // Contains unbound variable types
    Named,       // Contains names of unknown types
    Resolved,    // All types are resolved, and no names or variables are present
}

/// A record type
#[derive(Debug, Clone, PartialEq)]
pub struct TypeRecord {
    typedefs: Vec<(String, Rc<Type>)>,
    fields: Vec<(String, Rc<Type>)>,
}

impl TypeRecord {
    fn s(&self) -> String {
        let mut res = String::from("");
        for v in self.typedefs.iter() {
            res += &format!(" type {}: {}", v.0, v.1.s());
        }
        for v in self.fields.iter() {
            res += &format!(" {}: {}", v.0, v.1.s());
        }
        res
    }

    fn resolve_check(&self) -> TypeResolve {
        let mut res = TypeResolve::Resolved;
        for v in self.typedefs.iter() {
            match v.1.resolve_check() {
                TypeResolve::UnboundVars => return TypeResolve::UnboundVars,
                TypeResolve::Named => res = TypeResolve::Named,
                TypeResolve::Resolved => (),
            }
        }
        for v in self.fields.iter() {
            match v.1.resolve_check() {
                TypeResolve::UnboundVars => return TypeResolve::UnboundVars,
                TypeResolve::Named => res = TypeResolve::Named,
                TypeResolve::Resolved => (),
            }
        }
        res
    }
}

static TYPE_VAR_ID: AtomicUsize = AtomicUsize::new(1);

/// The definition of a type, that can be attached to functions, variables
/// and values.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Bool,
    None,
    Str,
    Bytes,
    Sym,
    Char,
    Byte,
    Syntax,
    Type,
    Userdata,
    Int,
    Float,
    IVec2,
    IVec3,
    IVec4,
    FVec2,
    FVec3,
    FVec4,
    Opt(Rc<Type>),
    Err(Rc<Type>),
    Pair(Rc<Type>, Rc<Type>),
    Lst(Rc<Type>),
    Map(Rc<Type>),
    Ref(Rc<Type>),
    Record(Rc<TypeRecord>),
    Union(Rc<Vec<Rc<Type>>>),
    Function(Rc<Vec<(Rc<Type>, bool)>>, Rc<Type>),
    Name(Rc<String>),
    Var(Rc<String>),
}

impl Type {
    pub fn any() -> Rc<Self> {
        Rc::new(Type::Any)
    }

    pub fn rc_new_var(n: &str) -> Rc<Self> {
        Rc::new(Type::Var(Rc::new(format!("{}{}", n, TYPE_VAR_ID.fetch_add(1, Ordering::SeqCst)))))
    }

    pub fn s(&self) -> String {
        match self {
            Type::Any => "any".to_string(),
            Type::Bool => "bool".to_string(),
            Type::None => "none".to_string(),
            Type::Str => "str".to_string(),
            Type::Bytes => "bytes".to_string(),
            Type::Sym => "sym".to_string(),
            Type::Byte => "byte".to_string(),
            Type::Char => "char".to_string(),
            Type::Syntax => "syntax".to_string(),
            Type::Type => "type".to_string(),
            Type::Userdata => "userdata".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::IVec2 => "ivec2".to_string(),
            Type::IVec3 => "ivec3".to_string(),
            Type::IVec4 => "ivec4".to_string(),
            Type::FVec2 => "fvec2".to_string(),
            Type::FVec3 => "fvec3".to_string(),
            Type::FVec4 => "fvec4".to_string(),
            Type::Opt(t) => format!("optional {}", t.s()),
            Type::Err(t) => format!("error {}", t.s()),
            Type::Pair(t, t2) => format!("pair {}, {}", t.s(), t2.s()),
            Type::Lst(t) => format!("[{}]", t.s()),
            Type::Ref(t) => format!("ref {}", t.s()),
            Type::Map(t) => format!("{{{}}}", t.s()),
            Type::Record(record) => format!("record {}", record.s()),
            Type::Union(types) => {
                let mut res = String::from("");
                let mut first = true;
                for t in types.iter() {
                    if !first {
                        res += " | ";
                    }
                    res += &t.s();
                    first = false;
                }
                res
            }
            Type::Function(types, ret) => format!("({:?}) -> {}", *types, ret.s()),
            Type::Name(name) => format!("{}", *name),
            Type::Var(name) => format!("<{}>", *name),
        }
    }

    pub fn resolve_check(&self) -> TypeResolve {
        match self {
            Type::Any => return TypeResolve::Resolved,
            Type::Bool => return TypeResolve::Resolved,
            Type::None => return TypeResolve::Resolved,
            Type::Str => return TypeResolve::Resolved,
            Type::Bytes => return TypeResolve::Resolved,
            Type::Sym => return TypeResolve::Resolved,
            Type::Char => return TypeResolve::Resolved,
            Type::Byte => return TypeResolve::Resolved,
            Type::Syntax => return TypeResolve::Resolved,
            Type::Type => return TypeResolve::Resolved,
            Type::Userdata => return TypeResolve::Resolved,
            Type::Int => return TypeResolve::Resolved,
            Type::Float => return TypeResolve::Resolved,
            Type::IVec2 => return TypeResolve::Resolved,
            Type::IVec3 => return TypeResolve::Resolved,
            Type::IVec4 => return TypeResolve::Resolved,
            Type::FVec2 => return TypeResolve::Resolved,
            Type::FVec3 => return TypeResolve::Resolved,
            Type::FVec4 => return TypeResolve::Resolved,
            Type::Opt(t) => return t.resolve_check(),
            Type::Err(t) => return t.resolve_check(),
            Type::Pair(t, t2) => {
                let mut res = t.resolve_check();
                match t2.resolve_check() {
                    TypeResolve::UnboundVars => return TypeResolve::UnboundVars,
                    TypeResolve::Named => res = TypeResolve::Named,
                    TypeResolve::Resolved => (),
                }
                res
            }
            Type::Lst(t) => t.resolve_check(),
            Type::Ref(t) => t.resolve_check(),
            Type::Map(t) => t.resolve_check(),
            Type::Record(record) => record.resolve_check(),
            Type::Union(types) => {
                let mut res = TypeResolve::Resolved;
                for t in types.iter() {
                    match t.resolve_check() {
                        TypeResolve::UnboundVars => return TypeResolve::UnboundVars,
                        TypeResolve::Named => res = TypeResolve::Named,
                        TypeResolve::Resolved => (),
                    }
                }
                res
            }
            Type::Function(types, ret) => {
                let mut res = ret.resolve_check();
                for t in types.iter() {
                    match t.0.resolve_check() {
                        TypeResolve::UnboundVars => return TypeResolve::UnboundVars,
                        TypeResolve::Named => res = TypeResolve::Named,
                        TypeResolve::Resolved => (),
                    }
                }
                res
            }
            Type::Name(_name) => TypeResolve::Named,
            Type::Var(_name) => TypeResolve::UnboundVars,
        }
    }
}

/// The internal distinction between a character and a byte.
/// They share parts of the lexical represenation and also the
/// semantic purspose is similar.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum VValChr {
    Char(char),
    Byte(u8),
}

impl std::fmt::Display for VValChr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VValChr::Char(c) => write!(f, "'{}'", format_escape_char(*c, false)),
            VValChr::Byte(b) => write!(
                f,
                "$b'{}'",
                format_escape_char(std::char::from_u32(*b as u32).unwrap_or('?'), true)
            ),
        }
    }
}

impl VValChr {
    pub fn byte(&self) -> u8 {
        match self {
            VValChr::Char(c) => {
                let c = *c as u32;
                if c > 0xFF {
                    b'?'
                } else {
                    c as u8
                }
            }
            VValChr::Byte(b) => *b,
        }
    }

    pub fn c(&self) -> char {
        match self {
            VValChr::Char(c) => *c,
            VValChr::Byte(b) => std::char::from_u32(*b as u32).unwrap_or('?'),
        }
    }
}

/// VVal aka. VariantValue is a data structure to represent
/// all kinds of WLambda data structures.
///
/// It's used for the AST, for internal data and for runtime data structures.
#[derive(Debug, Clone)]
#[allow(dead_code)]
#[repr(u8)]
pub enum VVal {
    /// The none value, the default value of all non initialized data.
    None,
    /// The err value is a special sentinel value for representing any kind of
    /// application error condition. It's created using the special $e <expr> or $error <expr>
    /// syntax.
    Err(Rc<RefCell<(VVal, SynPos)>>),
    /// Representation of a boolean value.
    Bol(bool),
    /// Representation of an interned string aka symbol or key.
    Sym(Symbol),
    /// Representation of a single character (or byte)
    Chr(VValChr),
    /// Representation of a unicode/text string.
    Str(Rc<String>),
    /// Representation of a byte buffer.
    Byt(Rc<Vec<u8>>),
    /// Integer value
    Int(i64),
    /// Float value
    Flt(f64),
    /// A syntax node in the AST, records the position too.
    Syn(SynPos),
    /// A pair
    Pair(Rc<(VVal, VVal)>),
    /// An optional value. While VVal::None basically has the same meaning
    /// as "no value", an optional value provides functions a way to say
    /// that they don't even return none value but nothing. Yes, it sounds
    /// weird, and it is weird. But in case of iterator functions that
    /// iterate over an array you can find out whether the iterator is at
    /// the end or will provide more values.
    Opt(Option<Rc<VVal>>),
    /// A special internal iterator type for built in VVal data structures.
    Iter(Rc<RefCell<VValIter>>),
    /// A list (or vector) of VVals.
    Lst(Rc<RefCell<std::vec::Vec<VVal>>>),
    /// A mapping of strings to VVals.
    Map(Rc<RefCell<FnvHashMap<Symbol, VVal>>>),
    /// A function, see also [VValFun](struct.VValFun.html)
    Fun(Rc<VValFun>),
    /// A guarded VVal, that executes a given function when it is
    /// no longer referenced.
    DropFun(Rc<DropFun>),
    /// A numerical (mathematical) vector storing integers. See NVec for more information.
    FVec(Box<NVec<f64>>),
    /// A numerical (mathematical) vector storing floats. See NVec for more information.
    IVec(Box<NVec<i64>>),
    /// A (strong) reference to a VVal.
    Ref(Rc<RefCell<VVal>>),
    /// A hidden strong reference to a VVal. Generated either explicitly by `$&`
    /// or if a variable is captured by a closure.
    HRef(Rc<RefCell<VVal>>),
    /// A (weak) reference to a VVal. Might turn VVal::None anytime.
    WWRef(Weak<RefCell<VVal>>),
    /// A vval that can box some user data which can later be accessed
    /// from inside user supplied Rust functions via std::any::Any.
    Usr(Box<dyn VValUserData>),
    /// A vval that describes a type from typed WLambda. This is mostly used internally
    /// by the compiler and type checker, because our AST is basically a VVal structure.
    Type(Rc<Type>),
}

impl PartialEq for VVal {
    fn eq(&self, rhs: &Self) -> bool {
        self.eqv(rhs)
    }
}

impl std::fmt::Debug for VValFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "&VValFun")
    }
}

pub fn format_escape_char(c: char, narrow_ascii: bool) -> String {
    match c {
        '\\' => String::from("\\\\"),
        '\n' => String::from("\\n"),
        '\t' => String::from("\\t"),
        '\r' => String::from("\\r"),
        '\0' => String::from("\\0"),
        '\'' => String::from("\\'"),
        '\"' => String::from("\\\""),
        _ if narrow_ascii
            && c.is_ascii()
            && (c.is_ascii_alphanumeric()
                || c.is_ascii_graphic()
                || c.is_ascii_punctuation()
                || c == ' ') =>
        {
            format!("{}", c)
        }
        _ if narrow_ascii => {
            format!("\\x{:02X}", c as u32)
        }
        _ if !narrow_ascii && c.is_ascii_control() => {
            format!("\\x{:02X}", c as u32)
        }
        _ if !narrow_ascii && c.is_control() => c.escape_unicode().to_string(),
        _ => {
            format!("{}", c)
        }
    }
}

pub fn format_vval_str(s: &str, narrow_ascii: bool) -> String {
    // TODO: FIXME: Use writers to write into a buffered writer.
    let mut v: Vec<String> = s.chars().map(|c| format_escape_char(c, narrow_ascii)).collect();
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
        if self.touch(v).is_some() {
            return;
        }

        match v {
            VVal::Err(e) => self.touch_walk(&(*e).borrow().0),
            VVal::Pair(b) => {
                self.touch_walk(&b.0);
                self.touch_walk(&b.1);
            }
            VVal::Opt(b) => {
                if let Some(x) = b {
                    self.touch_walk(x);
                }
            }
            VVal::Lst(l) => {
                for v in l.borrow().iter() {
                    self.touch_walk(v);
                }
            }
            VVal::Map(l) => {
                for (_k, v) in l.borrow().iter() {
                    self.touch_walk(v);
                }
            }
            VVal::DropFun(f) => {
                if let VVal::Fun(f) = &f.fun {
                    for v in f.upvalues.iter() {
                        self.touch_walk(v);
                    }
                }
            }
            VVal::Fun(f) => {
                for v in f.upvalues.iter() {
                    self.touch_walk(v);
                }
            }
            VVal::Ref(l) => {
                self.touch_walk(&(*l).borrow());
            }
            VVal::HRef(l) => {
                self.touch_walk(&(*l).borrow());
            }
            VVal::WWRef(l) => {
                if let Some(v) = l.upgrade() {
                    self.touch_walk(&(*v).borrow());
                }
            }
            VVal::Str(_)
            | VVal::Byt(_)
            | VVal::None
            | VVal::Bol(_)
            | VVal::Sym(_)
            | VVal::Syn(_)
            | VVal::Iter(_)
            | VVal::FVec(_)
            | VVal::IVec(_)
            | VVal::Int(_)
            | VVal::Flt(_)
            | VVal::Chr(_)
            | VVal::Type(_)
            | VVal::Usr(_) => {}
        }
    }

    fn touch(&mut self, v: &VVal) -> Option<i64> {
        let id = v.ref_id()?;

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
        // Do not generate back refs for symbols. they are interned
        // anyways!
        if let VVal::Sym(_) = v {
            return None;
        }

        let id = v.ref_id()?;

        if let Some(backref) = self.refs.get(&id) {
            match *backref {
                br if br > 0 => {
                    self.refs.insert(id, -br);
                    Some((true, format!("$<{}=>", br)))
                }
                br if br < 0 => Some((false, format!("$<{}>", -br))),
                _ => None,
            }
        } else {
            None
        }
    }
}

pub type VValIter = std::iter::FromFn<Box<dyn FnMut() -> Option<(VVal, Option<VVal>)>>>;

macro_rules! swizzle_char2value {
    ($c: expr, $i: expr, $x: ident, $y: ident, $z: ident, $w: ident) => {
        match $c.chars().nth($i).unwrap_or(' ') {
            'r' => $x,
            'g' => $y,
            'b' => $z,
            'a' => $w,
            'h' => $x,
            's' => $y,
            'v' => $z,
            '0' => $x,
            '1' => $y,
            '2' => $z,
            '3' => $w,
            'x' => $x,
            'y' => $y,
            'z' => $z,
            'w' => $w,
            _ => return VVal::None,
        }
    };
}

#[allow(clippy::many_single_char_names)]
fn swizzle_i(s: &str, x: i64, y: i64, z: i64, w: i64) -> VVal {
    match s.len() {
        2 => VVal::IVec(Box::new(NVec::Vec2(
            swizzle_char2value!(s, 0, x, y, z, w),
            swizzle_char2value!(s, 1, x, y, z, w),
        ))),
        3 => VVal::IVec(Box::new(NVec::Vec3(
            swizzle_char2value!(s, 0, x, y, z, w),
            swizzle_char2value!(s, 1, x, y, z, w),
            swizzle_char2value!(s, 2, x, y, z, w),
        ))),
        4 => VVal::IVec(Box::new(NVec::Vec4(
            swizzle_char2value!(s, 0, x, y, z, w),
            swizzle_char2value!(s, 1, x, y, z, w),
            swizzle_char2value!(s, 2, x, y, z, w),
            swizzle_char2value!(s, 3, x, y, z, w),
        ))),
        _ => VVal::None,
    }
}

#[allow(clippy::many_single_char_names)]
fn swizzle_f(s: &str, x: f64, y: f64, z: f64, w: f64) -> VVal {
    match s.len() {
        2 => VVal::FVec(Box::new(NVec::Vec2(
            swizzle_char2value!(s, 0, x, y, z, w),
            swizzle_char2value!(s, 1, x, y, z, w),
        ))),
        3 => VVal::FVec(Box::new(NVec::Vec3(
            swizzle_char2value!(s, 0, x, y, z, w),
            swizzle_char2value!(s, 1, x, y, z, w),
            swizzle_char2value!(s, 2, x, y, z, w),
        ))),
        4 => VVal::FVec(Box::new(NVec::Vec4(
            swizzle_char2value!(s, 0, x, y, z, w),
            swizzle_char2value!(s, 1, x, y, z, w),
            swizzle_char2value!(s, 2, x, y, z, w),
            swizzle_char2value!(s, 3, x, y, z, w),
        ))),
        _ => VVal::None,
    }
}

macro_rules! iter_next {
    ($i: expr) => {
        if let Some((v, k)) = $i.next() {
            if let Some(k) = k {
                VVal::opt(VVal::pair(v, k))
            } else {
                VVal::opt(v)
            }
        } else {
            VVal::opt_none()
        }
    };
}

macro_rules! iter_next_value {
    ($i: expr, $v: ident, $conv: block, $def: expr) => {
        if let Some(($v, _)) = $i.next() {
            $conv
        } else {
            $def
        }
    };
}

macro_rules! iter_int_a_to_b {
    ($a: expr, $b: expr) => {{
        let mut i = $a;
        let b = $b;
        std::iter::from_fn(Box::new(move || {
            if i >= b {
                return None;
            }
            let ret = Some((VVal::Int(i), None));
            i += 1;
            ret
        }))
    }};
}

macro_rules! pair_key_to_iter {
    ($p: ident, $iter2: block) => {
        if let VVal::Iter(ai) = &$p.0 {
            let ai = ai.clone();

            if let VVal::Iter(bi) = &$p.1 {
                let bi = bi.clone();

                std::iter::from_fn(Box::new(move || {
                    let a = ai.borrow_mut().next();
                    if let Some((a, ak)) = a {
                        let a = if let Some(ak) = ak { VVal::pair(a, ak) } else { a };

                        let b = bi.borrow_mut().next();
                        if let Some((b, bk)) = b {
                            let b = if let Some(bk) = bk { VVal::pair(b, bk) } else { b };

                            Some((a, Some(b)))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }))
            } else {
                let mut bi = $iter2;
                std::iter::from_fn(Box::new(move || {
                    let a = ai.borrow_mut().next();
                    if let Some((a, ak)) = a {
                        let a = if let Some(ak) = ak { VVal::pair(a, ak) } else { a };

                        if let Some((b, bk)) = bi.next() {
                            let b = if let Some(bk) = bk { VVal::pair(b, bk) } else { b };

                            Some((VVal::pair(a, b), None))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }))
            }
        } else if $p.0.is_int() {
            iter_int_a_to_b!($p.0.i(), $p.1.i())
        } else {
            $p.0.with_s_ref(|key: &str| -> VValIter {
                let l = match &key[..] {
                    "keys" => $p.1.keys(),
                    "values" => $p.1.values(),
                    "enumerate" => $p.1.enumerate(),
                    _ => return std::iter::from_fn(Box::new(move || None)),
                };
                if let VVal::Lst(l) = l {
                    let l = l.clone();
                    let mut idx = 0;
                    std::iter::from_fn(Box::new(move || {
                        if idx >= l.borrow().len() {
                            return None;
                        }
                        let r = Some((l.borrow()[idx].clone(), None));
                        idx += 1;
                        r
                    }))
                } else {
                    std::iter::from_fn(Box::new(move || None))
                }
            })
        }
    };
}

#[allow(clippy::cognitive_complexity)]
#[allow(clippy::comparison_chain)]
#[allow(clippy::while_let_on_iterator)]
fn range_extract(from: i64, cnt: i64, val: &VVal) -> VVal {
    match val {
        VVal::Chr(VValChr::Char(c)) => {
            let c = *c as i64;
            VVal::Bol(from <= c && cnt >= c)
        }
        VVal::Chr(VValChr::Byte(c)) => {
            let c = *c as i64;
            VVal::Bol(from <= c && cnt >= c)
        }
        VVal::Byt(s) => {
            VVal::new_byt(s.iter().skip(from as usize).take(cnt as usize).copied().collect())
        }
        VVal::Str(s) => {
            VVal::new_str_mv(s.chars().skip(from as usize).take(cnt as usize).collect())
        }
        VVal::IVec(b) => {
            let mut out = vec![];
            match b.as_ref() {
                NVec::Vec2(a, b) => {
                    if cnt == 1 {
                        match from {
                            0 => out.push(VVal::Int(*a)),
                            1 => out.push(VVal::Int(*b)),
                            _ => (),
                        }
                    } else if cnt > 1 {
                        match from {
                            0 => {
                                out.push(VVal::Int(*a));
                                out.push(VVal::Int(*b));
                            }
                            1 => out.push(VVal::Int(*b)),
                            _ => (),
                        }
                    }
                }
                NVec::Vec3(a, b, c) => {
                    if cnt == 1 {
                        match from {
                            0 => out.push(VVal::Int(*a)),
                            1 => out.push(VVal::Int(*b)),
                            2 => out.push(VVal::Int(*c)),
                            _ => (),
                        }
                    } else if cnt == 2 {
                        match from {
                            0 => {
                                out.push(VVal::Int(*a));
                                out.push(VVal::Int(*b));
                            }
                            1 => {
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                            }
                            2 => {
                                out.push(VVal::Int(*c));
                            }
                            _ => (),
                        }
                    } else if cnt > 2 {
                        match from {
                            0 => {
                                out.push(VVal::Int(*a));
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                            }
                            1 => {
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                            }
                            2 => out.push(VVal::Int(*c)),
                            _ => (),
                        }
                    }
                }
                NVec::Vec4(a, b, c, d) => {
                    if cnt == 1 {
                        match from {
                            0 => out.push(VVal::Int(*a)),
                            1 => out.push(VVal::Int(*b)),
                            2 => out.push(VVal::Int(*c)),
                            3 => out.push(VVal::Int(*d)),
                            _ => (),
                        }
                    } else if cnt == 2 {
                        match from {
                            0 => {
                                out.push(VVal::Int(*a));
                                out.push(VVal::Int(*b));
                            }
                            1 => {
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                            }
                            2 => {
                                out.push(VVal::Int(*c));
                                out.push(VVal::Int(*d));
                            }
                            3 => {
                                out.push(VVal::Int(*d));
                            }
                            _ => (),
                        }
                    } else if cnt == 3 {
                        match from {
                            0 => {
                                out.push(VVal::Int(*a));
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                            }
                            1 => {
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                                out.push(VVal::Int(*d));
                            }
                            2 => {
                                out.push(VVal::Int(*c));
                                out.push(VVal::Int(*d));
                            }
                            3 => {
                                out.push(VVal::Int(*d));
                            }
                            _ => (),
                        }
                    } else if cnt > 2 {
                        match from {
                            0 => {
                                out.push(VVal::Int(*a));
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                                out.push(VVal::Int(*d));
                            }
                            1 => {
                                out.push(VVal::Int(*b));
                                out.push(VVal::Int(*c));
                                out.push(VVal::Int(*d));
                            }
                            2 => {
                                out.push(VVal::Int(*c));
                                out.push(VVal::Int(*d));
                            }
                            3 => {
                                out.push(VVal::Int(*d));
                            }
                            _ => (),
                        }
                    }
                }
            }

            VVal::vec_mv(out)
        }
        VVal::FVec(b) => {
            let mut out = vec![];
            match b.as_ref() {
                NVec::Vec2(a, b) => {
                    if cnt == 1 {
                        match from {
                            0 => out.push(VVal::Flt(*a)),
                            1 => out.push(VVal::Flt(*b)),
                            _ => (),
                        }
                    } else if cnt > 1 {
                        match from {
                            0 => {
                                out.push(VVal::Flt(*a));
                                out.push(VVal::Flt(*b));
                            }
                            1 => out.push(VVal::Flt(*b)),
                            _ => (),
                        }
                    }
                }
                NVec::Vec3(a, b, c) => {
                    if cnt == 1 {
                        match from {
                            0 => out.push(VVal::Flt(*a)),
                            1 => out.push(VVal::Flt(*b)),
                            2 => out.push(VVal::Flt(*c)),
                            _ => (),
                        }
                    } else if cnt == 2 {
                        match from {
                            0 => {
                                out.push(VVal::Flt(*a));
                                out.push(VVal::Flt(*b));
                            }
                            1 => {
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                            }
                            2 => {
                                out.push(VVal::Flt(*c));
                            }
                            _ => (),
                        }
                    } else if cnt > 2 {
                        match from {
                            0 => {
                                out.push(VVal::Flt(*a));
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                            }
                            1 => {
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                            }
                            2 => out.push(VVal::Flt(*c)),
                            _ => (),
                        }
                    }
                }
                NVec::Vec4(a, b, c, d) => {
                    if cnt == 1 {
                        match from {
                            0 => out.push(VVal::Flt(*a)),
                            1 => out.push(VVal::Flt(*b)),
                            2 => out.push(VVal::Flt(*c)),
                            3 => out.push(VVal::Flt(*d)),
                            _ => (),
                        }
                    } else if cnt == 2 {
                        match from {
                            0 => {
                                out.push(VVal::Flt(*a));
                                out.push(VVal::Flt(*b));
                            }
                            1 => {
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                            }
                            2 => {
                                out.push(VVal::Flt(*c));
                                out.push(VVal::Flt(*d));
                            }
                            3 => {
                                out.push(VVal::Flt(*d));
                            }
                            _ => (),
                        }
                    } else if cnt == 3 {
                        match from {
                            0 => {
                                out.push(VVal::Flt(*a));
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                            }
                            1 => {
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                                out.push(VVal::Flt(*d));
                            }
                            2 => {
                                out.push(VVal::Flt(*c));
                                out.push(VVal::Flt(*d));
                            }
                            3 => {
                                out.push(VVal::Flt(*d));
                            }
                            _ => (),
                        }
                    } else if cnt > 2 {
                        match from {
                            0 => {
                                out.push(VVal::Flt(*a));
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                                out.push(VVal::Flt(*d));
                            }
                            1 => {
                                out.push(VVal::Flt(*b));
                                out.push(VVal::Flt(*c));
                                out.push(VVal::Flt(*d));
                            }
                            2 => {
                                out.push(VVal::Flt(*c));
                                out.push(VVal::Flt(*d));
                            }
                            3 => {
                                out.push(VVal::Flt(*d));
                            }
                            _ => (),
                        }
                    }
                }
            }

            VVal::vec_mv(out)
        }
        VVal::Lst(l) => {
            let v: Vec<VVal> =
                l.borrow().iter().skip(from as usize).take(cnt as usize).cloned().collect();
            VVal::vec_mv(v)
        }
        VVal::Iter(i) => {
            let mut out = Vec::with_capacity(cnt as usize);
            let mut i = i.borrow_mut();
            let mut idx = 0;
            while let Some((v, _)) = i.next() {
                if idx >= (from as usize) && idx < ((from + cnt) as usize) {
                    out.push(v);
                }
                idx += 1;
            }
            VVal::vec_mv(out)
        }
        _ => VVal::None,
    }
}

fn pair_extract(a: &VVal, b: &VVal, val: &VVal) -> VVal {
    match val {
        VVal::Int(i) => {
            if i % 2 == 0 {
                a.clone()
            } else {
                b.clone()
            }
        }
        VVal::Byt(_) => match (a, b) {
            (VVal::Int(from), VVal::Int(cnt)) => range_extract(*from, *cnt, val),
            (VVal::Int(start_idx), b)
                if b.is_sym() || b.is_str() || b.is_byte() || b.is_char() || b.is_bytes() =>
            {
                val.find(b, *start_idx as usize, true)
            }
            (VVal::Chr(_splitstr), VVal::Int(max)) => val.split(a, *max as usize, true),
            (VVal::Str(_splitstr), VVal::Int(max)) => val.split(a, *max as usize, true),
            (VVal::Byt(_splitstr), VVal::Int(max)) => val.split(a, *max as usize, true),
            (VVal::Str(_needle), b)
                if b.is_sym() || b.is_str() || b.is_byte() || b.is_char() || b.is_bytes() =>
            {
                val.bytes_replace(a, b)
            }
            (VVal::Byt(_needle), b)
                if b.is_sym() || b.is_str() || b.is_byte() || b.is_char() || b.is_bytes() =>
            {
                val.bytes_replace(a, b)
            }
            (VVal::Chr(_needle), b)
                if b.is_sym() || b.is_str() || b.is_byte() || b.is_char() || b.is_bytes() =>
            {
                val.bytes_replace(a, b)
            }
            _ => VVal::None,
        },
        VVal::Str(s) => match (a, b) {
            (VVal::Int(from), VVal::Int(cnt)) => range_extract(*from, *cnt, val),
            (VVal::Int(start_idx), b)
                if b.is_sym() || b.is_str() || b.is_byte() || b.is_char() || b.is_bytes() =>
            {
                val.find(b, *start_idx as usize, false)
            }
            (VVal::Chr(_splitstr), VVal::Int(max)) => val.split(a, *max as usize, false),
            (VVal::Byt(_splitstr), VVal::Int(max)) => val.split(a, *max as usize, false),
            (VVal::Str(_splitstr), VVal::Int(max)) => val.split(a, *max as usize, false),
            (VVal::Chr(needle), VVal::Chr(replace)) => {
                let mut buf = [0; 6];
                let chrstr = replace.c().encode_utf8(&mut buf);
                VVal::new_str_mv(s.as_ref().replace(needle.c(), chrstr))
            }
            (VVal::Str(needle), VVal::Str(replace)) => {
                VVal::new_str_mv(s.as_ref().replace(needle.as_ref(), replace.as_ref()))
            }
            _ => VVal::None,
        },
        VVal::Lst(_) => match (a, b) {
            (VVal::Int(from), VVal::Int(cnt)) => range_extract(*from, *cnt, val),
            _ => VVal::None,
        },
        VVal::Iter(_) => match (a, b) {
            (VVal::Int(from), VVal::Int(cnt)) => range_extract(*from, *cnt, val),
            _ => VVal::None,
        },
        VVal::IVec(_) => match (a, b) {
            (VVal::Int(from), VVal::Int(cnt)) => range_extract(*from, *cnt, val),
            _ => VVal::None,
        },
        VVal::FVec(_) => match (a, b) {
            (VVal::Int(from), VVal::Int(cnt)) => range_extract(*from, *cnt, val),
            _ => VVal::None,
        },
        VVal::Chr(VValChr::Byte(c)) => match (a, b) {
            (VVal::Chr(from), VVal::Chr(to)) => {
                let a = from.c() as u32;
                let b = to.c() as u32;
                let c = *c as u32;
                VVal::Bol(a <= c && b >= c)
            }
            (VVal::Int(a), VVal::Int(b)) => {
                let c = *c as i64;
                VVal::Bol(*a <= c && *b >= c)
            }
            _ => VVal::None,
        },
        VVal::Chr(VValChr::Char(c)) => match (a, b) {
            (VVal::Chr(from), VVal::Chr(to)) => {
                let a = from.c() as u32;
                let b = to.c() as u32;
                let c = *c as u32;
                VVal::Bol(a <= c && b >= c)
            }
            (VVal::Int(a), VVal::Int(b)) => {
                let c = *c as i64;
                VVal::Bol(*a <= c && *b >= c)
            }
            _ => VVal::None,
        },
        _ => VVal::None,
    }
}

fn vval_rc_ptr_eq(v: &VVal, l: &Rc<RefCell<VVal>>) -> bool {
    match v {
        VVal::Ref(r2) => Rc::ptr_eq(l, r2),
        VVal::HRef(r2) => Rc::ptr_eq(l, r2),
        VVal::WWRef(r2) => match r2.upgrade() {
            Some(v2) => Rc::ptr_eq(l, &v2),
            None => false,
        },
        _ => false,
    }
}

fn concat_operation(
    bytes: bool,
    first: &VVal,
    argc: usize,
    env: &mut Env,
) -> Result<VVal, StackAction> {
    if bytes {
        let mut buf = first.with_bv_ref(|bv| bv.to_vec());
        for i in 0..argc {
            env.arg_ref(i).unwrap().with_bv_ref(|bv| buf.extend_from_slice(bv));
        }
        Ok(VVal::new_byt(buf))
    } else {
        let mut st = first.with_s_ref(|s: &str| String::from(s));
        for i in 0..argc {
            env.arg_ref(i).unwrap().with_s_ref(|s: &str| st += s);
        }
        Ok(VVal::new_str_mv(st))
    }
}

#[allow(dead_code)]
impl VVal {
    #[inline]
    pub fn new_str(s: &str) -> VVal {
        VVal::Str(Rc::new(String::from(s)))
    }

    #[inline]
    pub fn new_str_mv(s: String) -> VVal {
        VVal::Str(Rc::new(s))
    }

    #[inline]
    pub fn new_char(c: char) -> VVal {
        VVal::Chr(VValChr::Char(c))
    }

    #[inline]
    pub fn new_byte(b: u8) -> VVal {
        VVal::Chr(VValChr::Byte(b))
    }

    #[inline]
    pub fn new_sym(s: &str) -> VVal {
        VVal::Sym(s2sym(s))
    }

    #[inline]
    pub fn new_sym_mv(s: String) -> VVal {
        VVal::Sym(new_sym_mv(s))
    }

    #[inline]
    pub fn new_byt(v: Vec<u8>) -> VVal {
        VVal::Byt(Rc::new(v))
    }

    #[inline]
    pub fn err(v: VVal, pos: SynPos) -> VVal {
        VVal::Err(Rc::new(RefCell::new((v, pos))))
    }

    pub fn err_msg(s: &str) -> VVal {
        VVal::Err(Rc::new(RefCell::new((VVal::new_str(s), SynPos::empty()))))
    }

    pub fn new_usr<T: VValUserData + 'static>(o: T) -> VVal {
        VVal::Usr(Box::new(o))
    }

    pub fn type_any() -> VVal {
        VVal::typ_box(Type::Any)
    }

    pub fn typ_box(t: Type) -> VVal {
        VVal::Type(Rc::new(t))
    }

    pub fn typ(t: Rc<Type>) -> VVal {
        VVal::Type(t)
    }

    #[inline]
    pub fn ivec2(x: i64, y: i64) -> VVal {
        VVal::IVec(Box::new(NVec::Vec2(x, y)))
    }

    #[inline]
    pub fn ivec3(x: i64, y: i64, z: i64) -> VVal {
        VVal::IVec(Box::new(NVec::Vec3(x, y, z)))
    }

    #[inline]
    pub fn ivec4(x: i64, y: i64, z: i64, w: i64) -> VVal {
        VVal::IVec(Box::new(NVec::Vec4(x, y, z, w)))
    }

    #[inline]
    pub fn fvec2(x: f64, y: f64) -> VVal {
        VVal::FVec(Box::new(NVec::Vec2(x, y)))
    }

    #[inline]
    pub fn fvec3(x: f64, y: f64, z: f64) -> VVal {
        VVal::FVec(Box::new(NVec::Vec3(x, y, z)))
    }

    #[inline]
    pub fn fvec4(x: f64, y: f64, z: f64, w: f64) -> VVal {
        VVal::FVec(Box::new(NVec::Vec4(x, y, z, w)))
    }

    pub fn ivec_from_tpl2(tpl: (i64, i64)) -> VVal {
        if let Some(nv) = NVec::from_tpl((tpl.0, tpl.1, None, None)) {
            VVal::IVec(Box::new(nv))
        } else {
            VVal::None
        }
    }

    pub fn ivec_from_tpl3(tpl: (i64, i64, i64)) -> VVal {
        if let Some(nv) = NVec::from_tpl((tpl.0, tpl.1, Some(tpl.2), None)) {
            VVal::IVec(Box::new(nv))
        } else {
            VVal::None
        }
    }

    pub fn ivec_from_tpl4(tpl: (i64, i64, i64, i64)) -> VVal {
        if let Some(nv) = NVec::from_tpl((tpl.0, tpl.1, Some(tpl.2), Some(tpl.3))) {
            VVal::IVec(Box::new(nv))
        } else {
            VVal::None
        }
    }

    pub fn fvec_from_tpl2(tpl: (f64, f64)) -> VVal {
        if let Some(nv) = NVec::from_tpl((tpl.0, tpl.1, None, None)) {
            VVal::FVec(Box::new(nv))
        } else {
            VVal::None
        }
    }

    pub fn fvec_from_tpl3(tpl: (f64, f64, f64)) -> VVal {
        if let Some(nv) = NVec::from_tpl((tpl.0, tpl.1, Some(tpl.2), None)) {
            VVal::FVec(Box::new(nv))
        } else {
            VVal::None
        }
    }

    pub fn fvec_from_tpl4(tpl: (f64, f64, f64, f64)) -> VVal {
        if let Some(nv) = NVec::from_tpl((tpl.0, tpl.1, Some(tpl.2), Some(tpl.3))) {
            VVal::FVec(Box::new(nv))
        } else {
            VVal::None
        }
    }

    #[inline]
    pub fn vec() -> VVal {
        VVal::Lst(Rc::new(RefCell::new(Vec::new())))
    }

    #[inline]
    pub fn vec1(a: VVal) -> VVal {
        let v = Self::vec();
        v.push(a);
        v
    }

    #[inline]
    pub fn vec2(a: VVal, b: VVal) -> VVal {
        let v = Self::vec();
        v.push(a);
        v.push(b);
        v
    }

    #[inline]
    pub fn vec3(a: VVal, b: VVal, c: VVal) -> VVal {
        let v = Self::vec();
        v.push(a);
        v.push(b);
        v.push(c);
        v
    }

    #[inline]
    #[allow(clippy::many_single_char_names)]
    pub fn vec4(a: VVal, b: VVal, c: VVal, d: VVal) -> VVal {
        let v = Self::vec();
        v.push(a);
        v.push(b);
        v.push(c);
        v.push(d);
        v
    }

    #[inline]
    pub fn opt(v: VVal) -> VVal {
        VVal::Opt(Some(Rc::new(v)))
    }

    #[inline]
    pub fn opt_none() -> VVal {
        VVal::Opt(None)
    }

    #[inline]
    pub fn unwrap_opt(&self) -> VVal {
        if let VVal::Opt(Some(o)) = self {
            o.as_ref().clone()
        } else {
            VVal::None
        }
    }

    /// Creates a pair from two VVals.
    #[inline]
    pub fn pair(a: VVal, b: VVal) -> VVal {
        VVal::Pair(Rc::new((a, b)))
    }

    /// Returns an iterator of the current value.
    pub fn as_iter(&self) -> VVal {
        VVal::Iter(Rc::new(RefCell::new(self.iter())))
    }

    pub fn to_vec(&self) -> Vec<VVal> {
        if let VVal::Lst(l) = self {
            let r: Vec<VVal> = l.borrow_mut().iter().cloned().collect();
            r
        } else {
            vec![self.clone()]
        }
    }

    pub fn vec_from(vl: &[VVal]) -> VVal {
        let mut v = Vec::new();
        v.extend_from_slice(vl);
        VVal::Lst(Rc::new(RefCell::new(v)))
    }

    pub fn vec_mv(v: Vec<VVal>) -> VVal {
        VVal::Lst(Rc::new(RefCell::new(v)))
    }

    /// Creates a new WLambda function that wraps the given Rust
    /// closure. See also `VValFun::new_fun`. For a more detailed
    /// discussion of the parameters.
    ///
    ///```rust
    /// use wlambda::compiler::EvalContext;
    /// use wlambda::vval::{VVal, VValFun, Env};
    ///
    /// let mut ctx = wlambda::compiler::EvalContext::new_empty_global_env();
    ///
    /// ctx.set_global_var("xyz",
    ///     &VVal::new_fun(
    ///         move |env: &mut Env, argc: usize| {
    ///             Ok(VVal::new_str("xyz"))
    ///         }, None, None, false));
    ///
    /// assert_eq!(ctx.eval("xyz[]").unwrap().s_raw(), "xyz")
    ///```
    pub fn new_fun<T>(
        fun: T,
        min_args: Option<usize>,
        max_args: Option<usize>,
        err_arg_ok: bool,
    ) -> VVal
    where
        T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction>,
    {
        VValFun::new_val(
            Rc::new(RefCell::new(fun)),
            Vec::new(),
            0,
            min_args,
            max_args,
            err_arg_ok,
            None,
            Rc::new(vec![]),
        )
    }

    pub fn call(&self, env: &mut Env, args: &[VVal]) -> Result<VVal, StackAction> {
        let argc = args.len();
        for v in args.iter() {
            env.push(v.clone());
        }
        let ret = self.call_internal(env, argc);
        env.popn(argc);
        ret
    }

    pub fn compare_str(&self, b: &VVal) -> std::cmp::Ordering {
        self.with_s_ref(|a: &str| b.with_s_ref(|b: &str| a.cmp(b)))
    }

    pub fn shallow_clone(&self) -> VVal {
        match self {
            VVal::Lst(l) => {
                let out = VVal::vec();
                for v in l.borrow().iter() {
                    out.push(v.clone());
                }
                out
            }
            VVal::Map(m) => {
                let out = VVal::map();
                for (k, v) in m.borrow_mut().iter() {
                    if out.set_key_sym(k.clone(), v.clone()).is_err() {
                        continue;
                    }
                }
                out
            }
            VVal::Str(s) => VVal::new_str_mv(s.as_ref().clone()),
            v => v.with_deref(
                |v| v.shallow_clone(),
                |v| if let Some(v) = v { v.clone() } else { VVal::None },
            ),
        }
    }

    pub fn compare_num(&self, b: &VVal) -> std::cmp::Ordering {
        if self.is_float() {
            self.f().partial_cmp(&b.f()).unwrap_or(std::cmp::Ordering::Greater)
        } else {
            self.i().cmp(&b.i())
        }
    }

    pub fn sort<F>(&self, compare: F)
    where
        F: FnMut(&VVal, &VVal) -> std::cmp::Ordering,
    {
        if let VVal::Lst(v) = self {
            v.borrow_mut().sort_by(compare);
        }
    }

    pub fn fisher_yates_shuffle<I>(&mut self, mut rand: I)
    where
        I: FnMut() -> i64,
    {
        if let VVal::Lst(list) = self {
            let len = list.borrow().len();
            if len <= 1 {
                return;
            }

            for k in 1..len {
                let i = len - k;
                let j = rand().abs() as usize % (i + 1);
                list.borrow_mut().swap(j, i);
            }
        }
    }

    pub fn split(&self, pat: &VVal, max: usize, bytes: bool) -> VVal {
        let l = VVal::vec();

        if bytes {
            self.with_bv_ref(|inp| {
                pat.with_bv_ref(|pat| {
                    let len = inp.len();
                    let ss_len = pat.len();

                    if ss_len > len {
                        l.push(VVal::new_byt(inp[..].to_vec()));
                    } else {
                        let mut i = 0;
                        let mut last_split_i = 0;

                        while i < len {
                            if i + ss_len > len {
                                break;
                            }
                            if inp[i..(i + ss_len)] == *pat {
                                l.push(VVal::new_byt(inp[last_split_i..i].to_vec()));

                                i += ss_len;
                                last_split_i = i;

                                if max > 0 && (l.len() + 1) >= max as usize {
                                    l.push(VVal::new_byt(inp[last_split_i..len].to_vec()));
                                    i += inp[last_split_i..len].len();
                                    last_split_i = i + 1; // total end!
                                    break;
                                }
                            } else {
                                i += 1;
                            }
                        }

                        match last_split_i {
                            ls if ls < i => {
                                l.push(VVal::new_byt(inp[last_split_i..len].to_vec()));
                            }
                            ls if ls == i => {
                                l.push(VVal::new_byt(vec![]));
                            }
                            _ => (),
                        }
                    }
                })
            })
        } else {
            self.with_s_ref(|s| {
                pat.with_s_ref(|pat| {
                    if max > 0 {
                        for part in s.splitn(max as usize, pat) {
                            l.push(VVal::new_str(part));
                        }
                    } else {
                        for part in s.split(pat) {
                            l.push(VVal::new_str(part));
                        }
                    }
                })
            })
        }

        l
    }

    pub fn find(&self, pat: &VVal, start_idx: usize, bytes: bool) -> VVal {
        if bytes {
            self.with_s_ref(|s| {
                pat.with_s_ref(|pat| {
                    if start_idx >= s.len() {
                        return VVal::None;
                    }

                    match s[start_idx..].find(pat) {
                        Some(idx) => VVal::Int((start_idx + idx) as i64),
                        None => VVal::None,
                    }
                })
            })
        } else {
            self.with_bv_ref(|data| {
                pat.with_bv_ref(|pat| {
                    if pat.len() > (data.len() + start_idx) {
                        return VVal::None;
                    }

                    for i in start_idx..=(data.len() - pat.len()) {
                        if pat[..] == data[i..(i + pat.len())] {
                            return VVal::Int(i as i64);
                        }
                    }

                    VVal::None
                })
            })
        }
    }

    pub fn bytes_replace(&self, pat: &VVal, repl: &VVal) -> Self {
        use std::cmp::Ordering;

        let mut bv = self.as_bytes();
        pat.with_bv_ref(|pat| {
            repl.with_bv_ref(|repl| {
                let mut len = bv.len();
                let mut i = 0;
                let plen = pat.len();
                let rlen = repl.len();
                while i < len {
                    if bv[i..].starts_with(pat) {
                        match plen.cmp(&rlen) {
                            Ordering::Less => {
                                let len_delta = rlen - plen;
                                bv.resize(len + len_delta, 0);
                                bv.copy_within((i + plen)..len, i + rlen);
                                len += len_delta;

                                bv[i..(rlen + i)].clone_from_slice(&repl[..rlen]);

                                i += rlen;
                            }
                            Ordering::Greater => {
                                let len_delta = plen - rlen;
                                bv.copy_within((i + plen)..len, i + rlen);
                                bv.resize(len - len_delta, 0);
                                len -= len_delta;

                                bv[i..(rlen + i)].clone_from_slice(&repl[..rlen]);
                            }
                            Ordering::Equal => {
                                bv[i..(plen + i)].clone_from_slice(&repl[..plen]);
                                if plen > 0 {
                                    i += plen - 1;
                                }
                            }
                        }
                    }

                    i += 1;
                }
            })
        });

        VVal::new_byt(bv)
    }

    #[allow(clippy::while_let_on_iterator)]
    pub fn reverse(&self) -> VVal {
        match self.deref() {
            VVal::Str(s) => VVal::new_str_mv(s.chars().rev().collect::<String>()),
            VVal::Byt(b) => VVal::new_byt(b.iter().rev().copied().collect::<Vec<u8>>()),
            VVal::Lst(l) => VVal::vec_mv(l.borrow().iter().rev().cloned().collect::<Vec<VVal>>()),
            VVal::IVec(b) => match b.as_ref() {
                NVec::Vec2(a, b) => VVal::IVec(Box::new(NVec::Vec2(*b, *a))),
                NVec::Vec3(a, b, c) => VVal::IVec(Box::new(NVec::Vec3(*c, *b, *a))),
                NVec::Vec4(a, b, c, d) => VVal::IVec(Box::new(NVec::Vec4(*d, *c, *b, *a))),
            },
            VVal::FVec(b) => match b.as_ref() {
                NVec::Vec2(a, b) => VVal::FVec(Box::new(NVec::Vec2(*b, *a))),
                NVec::Vec3(a, b, c) => VVal::FVec(Box::new(NVec::Vec3(*c, *b, *a))),
                NVec::Vec4(a, b, c, d) => VVal::FVec(Box::new(NVec::Vec4(*d, *c, *b, *a))),
            },
            VVal::Iter(i) => {
                let mut out = vec![];
                let mut i = i.borrow_mut();
                while let Some((v, _)) = i.next() {
                    out.push(v);
                }
                out.reverse();
                VVal::vec_mv(out)
            }
            _ => VVal::None,
        }
    }

    #[allow(clippy::while_let_on_iterator)]
    pub fn keys(&self) -> VVal {
        match self {
            VVal::Map(m) => {
                let out = VVal::vec();
                for (k, _) in m.borrow_mut().iter() {
                    out.push(VVal::new_str_mv(k.to_string()));
                }
                out
            }
            VVal::Iter(i) => {
                let out = VVal::vec();
                let mut i = i.borrow_mut();
                while let Some((_, k)) = i.next() {
                    if let Some(k) = k {
                        out.push(k);
                    }
                }
                out
            }
            VVal::Lst(l) => {
                let out = VVal::vec();
                for (i, _) in l.borrow_mut().iter().enumerate() {
                    out.push(VVal::Int(i as i64));
                }
                out
            }
            VVal::IVec(b) => match b.as_ref() {
                NVec::Vec2(_, _) => VVal::vec2(VVal::Int(0), VVal::Int(1)),
                NVec::Vec3(_, _, _) => VVal::vec3(VVal::Int(0), VVal::Int(1), VVal::Int(2)),
                NVec::Vec4(_, _, _, _) => {
                    VVal::vec4(VVal::Int(0), VVal::Int(1), VVal::Int(2), VVal::Int(3))
                }
            },
            VVal::FVec(b) => match b.as_ref() {
                NVec::Vec2(_, _) => VVal::vec2(VVal::Int(0), VVal::Int(1)),
                NVec::Vec3(_, _, _) => VVal::vec3(VVal::Int(0), VVal::Int(1), VVal::Int(2)),
                NVec::Vec4(_, _, _, _) => {
                    VVal::vec4(VVal::Int(0), VVal::Int(1), VVal::Int(2), VVal::Int(3))
                }
            },
            v => v.with_deref(|v| v.keys(), |_| VVal::None),
        }
    }

    #[allow(clippy::while_let_on_iterator)]
    pub fn values(&self) -> VVal {
        match self {
            VVal::Map(m) => {
                let out = VVal::vec();
                for (_, v) in m.borrow_mut().iter() {
                    out.push(v.clone());
                }
                out
            }
            VVal::Lst(l) => {
                let out = VVal::vec();
                for v in l.borrow_mut().iter() {
                    out.push(v.clone());
                }
                out
            }
            VVal::Iter(i) => {
                let out = VVal::vec();
                let mut i = i.borrow_mut();
                while let Some((v, _)) = i.next() {
                    out.push(v);
                }
                out
            }
            VVal::IVec(b) => {
                let out = VVal::vec();
                match b.as_ref() {
                    NVec::Vec2(a, b) => {
                        out.push(VVal::Int(*a));
                        out.push(VVal::Int(*b));
                    }
                    NVec::Vec3(a, b, c) => {
                        out.push(VVal::Int(*a));
                        out.push(VVal::Int(*b));
                        out.push(VVal::Int(*c));
                    }
                    NVec::Vec4(a, b, c, d) => {
                        out.push(VVal::Int(*a));
                        out.push(VVal::Int(*b));
                        out.push(VVal::Int(*c));
                        out.push(VVal::Int(*d));
                    }
                }
                out
            }
            VVal::FVec(b) => {
                let out = VVal::vec();
                match b.as_ref() {
                    NVec::Vec2(a, b) => {
                        out.push(VVal::Flt(*a));
                        out.push(VVal::Flt(*b));
                    }
                    NVec::Vec3(a, b, c) => {
                        out.push(VVal::Flt(*a));
                        out.push(VVal::Flt(*b));
                        out.push(VVal::Flt(*c));
                    }
                    NVec::Vec4(a, b, c, d) => {
                        out.push(VVal::Flt(*a));
                        out.push(VVal::Flt(*b));
                        out.push(VVal::Flt(*c));
                        out.push(VVal::Flt(*d));
                    }
                }
                out
            }
            v => v.with_deref(|v| v.values(), |_| VVal::None),
        }
    }

    #[allow(clippy::while_let_on_iterator)]
    pub fn enumerate(&self) -> VVal {
        match self {
            VVal::Map(m) => {
                let out = VVal::vec();
                for (i, _) in m.borrow_mut().iter().enumerate() {
                    out.push(VVal::Int(i as i64));
                }
                out
            }
            VVal::Lst(l) => {
                let out = VVal::vec();
                for (i, _) in l.borrow_mut().iter().enumerate() {
                    out.push(VVal::Int(i as i64));
                }
                out
            }
            VVal::Iter(i) => {
                let out = VVal::vec();
                let mut i = i.borrow_mut();
                let mut c = 0;
                while let Some(_) = i.next() {
                    out.push(VVal::Int(c));
                    c += 1;
                }
                out
            }
            v => v.with_deref(|v| v.enumerate(), |_| VVal::None),
        }
    }

    /// Returns true if this vval is containing (multiple) other VVals.
    /// Usually true for: Maps, Vectors and Opt.
    ///
    /// Helpful if you want to iterate through a data structure by recursively
    /// iterating over a vval. Without getting iterations over individual
    /// string characters.
    pub fn iter_over_vvals(&self) -> bool {
        self.with_deref(
            |v| matches!(v, VVal::Lst(_) | VVal::Map(_) | VVal::Opt(_)),
            |v| matches!(v, Some(VVal::Lst(_)) | Some(VVal::Map(_)) | Some(VVal::Opt(_))),
        )
    }

    /// This function returns you an iterator over the VVal.
    /// It will iterate over data such as VVal::Str, VVal::Sym, VVal::Lst,
    /// VVal::Map and VVal::Byt.
    ///
    /// **See also:** [VVal::with_iter] which is the better option for iterating
    /// over a VVal, because it catches the special case when the VVal itself
    /// is an iterator already.
    /// This function will not return the same iterator again when called
    /// on an VVal iterator value!
    ///
    /// **Attention:** Iterators from WLambda functions can only be
    /// created with [VVal::iter_env]. This is because we need a proper
    /// WLambda [Env] environment to derive the Rust iterator from.
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
                    if idx >= l.borrow().len() {
                        return None;
                    }
                    let r = Some((l.borrow()[idx].clone(), None));
                    idx += 1;
                    r
                }))
            }
            VVal::Map(m) => {
                let m = m.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    let r = m
                        .borrow()
                        .iter()
                        .nth(idx)
                        .map(|(k, v)| (v.clone(), Some(VVal::new_str(k.as_ref()))));
                    idx += 1;
                    r
                }))
            }
            VVal::Byt(b) => {
                let b = b.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    if idx >= b.len() {
                        return None;
                    }
                    let r = Some((VVal::new_byte(b[idx]), None));
                    idx += 1;
                    r
                }))
            }
            VVal::Sym(s) => {
                let s = s.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || {
                    let r = s.chars().nth(idx).map(|chr| (VVal::new_char(chr), None));
                    idx += 1;
                    r
                }))
            }
            VVal::Str(s) => {
                let s = s.clone();
                let mut idx = 0;
                std::iter::from_fn(Box::new(move || match s[idx..].chars().next() {
                    Some(chr) => {
                        idx += chr.len_utf8();
                        Some((VVal::new_char(chr), None))
                    }
                    None => None,
                }))
            }
            VVal::None => std::iter::from_fn(Box::new(move || None)),
            VVal::Pair(p) => {
                pair_key_to_iter!(p, { p.1.iter() })
            }
            VVal::IVec(b) => match b.as_ref() {
                NVec::Vec2(a, b) => {
                    iter_int_a_to_b!(*a, *b)
                }
                NVec::Vec3(a, b, skip) => {
                    let mut i = *a;
                    let b = *b;
                    let skip = *skip;
                    std::iter::from_fn(Box::new(move || {
                        if i >= b {
                            return None;
                        }
                        let ret = Some((VVal::Int(i), None));
                        i += skip;
                        ret
                    }))
                }
                _ => std::iter::from_fn(Box::new(move || None)),
            },
            VVal::FVec(b) => match b.as_ref() {
                NVec::Vec2(a, b) => {
                    let mut i = *a;
                    let b = *b;
                    std::iter::from_fn(Box::new(move || {
                        if i >= b {
                            return None;
                        }
                        let r = Some((VVal::Flt(i), None));
                        i += 1.0;
                        r
                    }))
                }
                NVec::Vec3(a, b, s) => {
                    let mut i = *a;
                    let b = *b;
                    let s = *s;
                    std::iter::from_fn(Box::new(move || {
                        if i >= b {
                            return None;
                        }
                        let ret = Some((VVal::Flt(i), None));
                        i += s;
                        ret
                    }))
                }
                _ => std::iter::from_fn(Box::new(move || None)),
            },
            VVal::Opt(Some(v)) => {
                let x = v.as_ref().clone();
                let mut used = false;
                std::iter::from_fn(Box::new(move || {
                    if used {
                        return None;
                    }
                    used = true;
                    Some((x.clone(), None))
                }))
            }
            VVal::Opt(None) => std::iter::from_fn(Box::new(move || None)),
            _ => self.with_deref(
                |v| v.iter(),
                |v| {
                    if let Some(v) = v {
                        let x = v.clone();
                        let mut used = false;
                        std::iter::from_fn(Box::new(move || {
                            if used {
                                return None;
                            }
                            used = true;
                            Some((x.clone(), None))
                        }))
                    } else {
                        std::iter::from_fn(Box::new(move || None))
                    }
                },
            ),
        }
    }

    /// This function is like [VVal::iter] but it also can create an iterator
    /// from a WLambda function. Where the function is interpreted as generator
    /// and called until it yields `$o()`.
    pub fn iter_env(&self, env: &mut Env) -> VValIter {
        match self {
            VVal::Pair(p) => {
                pair_key_to_iter!(p, { p.1.iter_env(env) })
            }
            VVal::Fun(_) => {
                let fu = self.clone();
                let mut e = env.derive();
                std::iter::from_fn(Box::new(move || match fu.call(&mut e, &[]) {
                    Err(err) => {
                        let _ =
                            write!(*e.stdio.write.borrow_mut(), "Error in iter function: {}", err);
                        None
                    }
                    Ok(v) => match v {
                        VVal::Opt(None) => None,
                        VVal::Opt(Some(v)) => Some((v.as_ref().clone(), None)),
                        _ => Some((v, None)),
                    },
                }))
            }
            _ => self.iter(),
        }
    }

    /// Calls the given callback with an iterator through
    /// the vval. It catches the special case when the VVal itself
    /// is an iterator.
    ///
    /// ```
    /// use wlambda::VVal;
    /// use std::rc::Rc;
    /// use std::cell::RefCell;
    ///
    /// let x = VVal::vec();
    /// x.push(VVal::Int(10));
    /// x.push(VVal::Int(20));
    ///
    /// let it = x.as_iter();
    ///
    /// let mut sum = 0;
    /// it.with_iter(|iter| {
    ///     for (v, _) in iter {
    ///         sum += v.i();
    ///     }
    /// });
    ///
    /// assert_eq!(sum, 30);
    /// ```
    pub fn with_iter<F, R>(&self, mut f: F) -> R
    where
        F: FnMut(&mut VValIter) -> R,
    {
        if let VVal::Iter(i) = self {
            f(&mut *i.borrow_mut())
        } else {
            let mut iter = self.iter();
            f(&mut iter)
        }
    }

    pub fn with_value_or_iter_values<T>(self, mut f: T)
    where
        T: FnMut(VVal, Option<VVal>) -> bool,
    {
        if let VVal::Iter(i) = self {
            while let Some((v, k)) = i.borrow_mut().next() {
                if !f(v, k) {
                    break;
                }
            }
        } else {
            f(self, None);
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
    where
        T: FnOnce(&std::vec::Vec<VarPos>, &mut std::vec::Vec<VVal>),
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

    pub(crate) fn call_internal(&self, env: &mut Env, argc: usize) -> Result<VVal, StackAction> {
        match self {
            VVal::None => Err(StackAction::panic_str(
                "Calling $none is invalid".to_string(),
                None,
                env.stk2vec(argc),
            )),
            VVal::Fun(fu) => {
                if let Some(i) = fu.min_args {
                    if argc < i {
                        return Err(StackAction::panic_str(
                            format!("function expects at least {} arguments, got {}", i, argc),
                            fu.syn_pos.clone(),
                            env.stk2vec(argc),
                        ));
                    }
                }

                if let Some(i) = fu.max_args {
                    if argc > i {
                        return Err(StackAction::panic_str(
                            format!("function expects at most {} arguments, got {}", i, argc),
                            fu.syn_pos.clone(),
                            env.stk2vec(argc),
                        ));
                    }
                }

                env.push_fun_call(fu.clone(), argc);
                if !(*fu).err_arg_ok {
                    for i in 0..argc {
                        if let Some(VVal::Err(ev)) = env.arg_err_internal(i) {
                            return Err(StackAction::panic_str(
                                format!("Error value in parameter list: {}", ev.borrow().0.s()),
                                Some(ev.borrow().1.clone()),
                                env.argv(),
                            ));
                        }
                    }
                }

                let ret = match &(*fu).fun {
                    FunType::ClosureNode(cn) => (cn.borrow())(env, argc),
                    FunType::VMProg(prog) => match crate::vm::vm(&*prog, env) {
                        Ok(v) => Ok(v),
                        Err(StackAction::Return(ret)) => {
                            if ret.0.eqv(&fu.label) {
                                Ok(ret.1)
                            } else {
                                Err(StackAction::Return(ret))
                            }
                        }
                        Err(e) => Err(e.wrap_panic(fu.syn_pos.clone(), env.argv())),
                    },
                };
                env.unwind_one();
                ret
            }
            VVal::Bol(b) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    let first = e.arg(0);
                    match first {
                        // Calling a boolean on a list converts the boolean into an integer
                        // and fetches the value at that index.
                        VVal::Lst(_) => Ok(first.at(*b as usize).unwrap_or(VVal::None)),
                        // Calling a boolean on a pair converts the boolean into an integer
                        // and fetches the value at that index.
                        VVal::Pair(_) => Ok(first.at(*b as usize).unwrap_or(VVal::None)),
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
                                    Ok(VVal::None)
                                }
                            } else {
                                Ok(self.clone())
                            }
                        }
                    }
                })
            }
            VVal::Err(e) => {
                Err(StackAction::panic_msg(format!("Called an error value: {}", e.borrow().0.s())))
            }
            VVal::Sym(sym) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc > 0 {
                    let v = e.arg(0);
                    Ok(v.get_key(&*sym).unwrap_or(VVal::None))
                } else {
                    Ok(self.clone())
                }
            }),
            VVal::Map(m) => env.with_local_call_info(argc, |e: &mut Env| {
                let f = e.arg(0);

                let mut ret = VVal::None;
                for (k, v) in m.borrow().iter() {
                    e.push(v.clone());
                    e.push(VVal::new_str(k));
                    let el = f.call_internal(e, 2);
                    e.popn(2);

                    match el {
                        Ok(v) => {
                            ret = v;
                        }
                        Err(StackAction::Break(v)) => {
                            ret = *v;
                            break;
                        }
                        Err(StackAction::Next) => {}
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(ret)
            }),
            VVal::Lst(l) => {
                env.with_local_call_info(argc, |e: &mut Env| {
                    // calling a list with any other value is an implicit map, meaning that for
                    // each value in the list the argument is called and the respective element
                    // in the map is passed in as the parameter.
                    let f = e.arg(0);

                    let mut ret = VVal::None;
                    for i in l.borrow().iter() {
                        e.push(i.clone());
                        let el = f.call_internal(e, 1);
                        e.popn(1);

                        match el {
                            Ok(v) => {
                                ret = v;
                            }
                            Err(StackAction::Break(v)) => {
                                ret = *v;
                                break;
                            }
                            Err(StackAction::Next) => {}
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                    Ok(ret)
                })
            }
            VVal::Chr(VValChr::Char(_)) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc > 0 {
                    let first_arg = e.arg(0);
                    match first_arg {
                        VVal::Pair(p) => Ok(pair_extract(&p.0, &p.1, self)),
                        _ => concat_operation(false, self, argc, e),
                    }
                } else {
                    Ok(self.clone())
                }
            }),
            VVal::Chr(VValChr::Byte(_)) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc > 0 {
                    let first_arg = e.arg(0);
                    match first_arg {
                        VVal::Pair(p) => Ok(pair_extract(&p.0, &p.1, self)),
                        _ => concat_operation(true, self, argc, e),
                    }
                } else {
                    Ok(self.clone())
                }
            }),
            VVal::Byt(vval_bytes) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc > 0 {
                    let first_arg = e.arg(0);
                    match first_arg {
                        VVal::Chr(_) | VVal::Byt(_) | VVal::Str(_) => {
                            concat_operation(true, self, argc, e)
                        }
                        VVal::Int(arg_int) => {
                            if argc > 1 {
                                let from = arg_int as usize;
                                let cnt = e.arg(1).i() as usize;
                                let r: Vec<u8> =
                                    vval_bytes.iter().skip(from).take(cnt).copied().collect();
                                Ok(VVal::new_byt(r))
                            } else if arg_int as usize >= vval_bytes.len() {
                                Ok(VVal::None)
                            } else {
                                Ok(VVal::new_byte(vval_bytes[arg_int as usize]))
                            }
                        }
                        VVal::Fun(_) => {
                            let mut ret = VVal::None;
                            for c in vval_bytes.iter() {
                                e.push(VVal::new_byt(vec![*c]));
                                let el = first_arg.call_internal(e, 1);
                                e.popn(1);
                                match el {
                                    Ok(v) => {
                                        ret = v;
                                    }
                                    Err(StackAction::Break(v)) => {
                                        ret = *v;
                                        break;
                                    }
                                    Err(StackAction::Next) => {}
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            }
                            Ok(ret)
                        }
                        VVal::Lst(_) => {
                            let from = first_arg.at(0).unwrap_or(VVal::Int(0)).i() as usize;
                            let cnt = first_arg
                                .at(1)
                                .unwrap_or_else(|| VVal::Int((vval_bytes.len() - from) as i64))
                                .i() as usize;
                            let r: Vec<u8> =
                                vval_bytes.iter().skip(from).take(cnt).copied().collect();
                            Ok(VVal::new_byt(r))
                        }
                        VVal::Pair(p) => Ok(pair_extract(&p.0, &p.1, self)),
                        VVal::IVec(iv) => Ok(range_extract(iv.x_raw(), iv.y_raw(), self)),
                        VVal::Map(_) => Ok(self
                            .with_s_ref(|key: &str| first_arg.get_key(key).unwrap_or(VVal::None))),
                        _ => Ok(VVal::None),
                    }
                } else {
                    Ok(self.clone())
                }
            }),
            VVal::Str(vval_str) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc > 0 {
                    let first_arg = e.arg(0);
                    match first_arg {
                        VVal::Int(arg_int) => {
                            if argc > 1 {
                                let from = arg_int as usize;
                                let cnt = e.arg(1).i() as usize;
                                Ok(VVal::new_str_mv(
                                    vval_str.chars().skip(from).take(cnt).collect(),
                                ))
                            } else {
                                let r = vval_str.chars().nth(arg_int as usize);
                                match r {
                                    None => Ok(VVal::None),
                                    Some(c) => Ok(VVal::new_char(c)),
                                }
                            }
                        }
                        VVal::Fun(_) => {
                            let mut ret = VVal::None;
                            for c in vval_str.chars() {
                                e.push(VVal::new_str_mv(c.to_string()));
                                let el = first_arg.call_internal(e, 1);
                                e.popn(1);
                                match el {
                                    Ok(v) => {
                                        ret = v;
                                    }
                                    Err(StackAction::Break(v)) => {
                                        ret = *v;
                                        break;
                                    }
                                    Err(StackAction::Next) => {}
                                    Err(e) => {
                                        return Err(e);
                                    }
                                }
                            }
                            Ok(ret)
                        }
                        VVal::Lst(_) => {
                            let from = first_arg.at(0).unwrap_or(VVal::Int(0)).i() as usize;
                            let cnt = first_arg
                                .at(1)
                                .unwrap_or_else(|| VVal::Int((vval_str.len() - from) as i64))
                                .i() as usize;
                            let r: String = vval_str.chars().skip(from).take(cnt).collect();
                            Ok(VVal::new_str_mv(r))
                        }
                        VVal::Chr(_) | VVal::Byt(_) | VVal::Str(_) => {
                            concat_operation(false, self, argc, e)
                        }
                        VVal::Map(_) => {
                            Ok(first_arg.get_key(vval_str.as_ref()).unwrap_or(VVal::None))
                        }
                        VVal::Pair(p) => Ok(pair_extract(&p.0, &p.1, self)),
                        VVal::IVec(iv) => Ok(range_extract(iv.x_raw(), iv.y_raw(), self)),
                        _ => Ok(VVal::None),
                    }
                } else {
                    Ok(self.clone())
                }
            }),
            VVal::Int(i) => env.with_local_call_info(argc, |e: &mut Env| {
                let v = e.arg(0);
                if argc > 0 {
                    Ok(v.at(*i as usize).unwrap_or(VVal::None))
                } else {
                    Ok(self.clone())
                }
            }),
            VVal::Pair(p) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc == 0 {
                    match (&p.0, &p.1) {
                        (VVal::Chr(c), VVal::Int(cnt)) | (VVal::Int(cnt), VVal::Chr(c)) => {
                            let cnt = *cnt as usize;
                            match c {
                                VValChr::Char(c) => {
                                    let mut s = String::with_capacity(cnt);
                                    for _ in 0..cnt {
                                        s.push(*c);
                                    }
                                    return Ok(VVal::new_str_mv(s));
                                }
                                VValChr::Byte(b) => {
                                    let mut v = Vec::with_capacity(cnt);
                                    v.resize(cnt, *b);
                                    return Ok(VVal::new_byt(v));
                                }
                            }
                        }
                        _ => {
                            return Ok(self.clone());
                        }
                    }
                }
                if argc != 1 {
                    return Ok(self.clone());
                }
                Ok(pair_extract(&p.0, &p.1, e.arg_ref(0).unwrap()))
            }),
            VVal::IVec(iv) => env.with_local_call_info(argc, |e: &mut Env| {
                if argc != 1 {
                    return Ok(self.clone());
                }
                Ok(range_extract(iv.x_raw(), iv.y_raw(), e.arg_ref(0).unwrap()))
            }),
            VVal::Iter(i) => {
                if argc == 1 {
                    env.with_local_call_info(argc, |e: &mut Env| {
                        let f = e.arg(0);

                        let mut ret = VVal::None;
                        #[allow(clippy::while_let_loop)]
                        loop {
                            let v = if let Some((v, k)) = i.borrow_mut().next() {
                                if let Some(k) = k {
                                    VVal::pair(v, k)
                                } else {
                                    v
                                }
                            } else {
                                break;
                            };

                            e.push(v);
                            let el = f.call_internal(e, 1);
                            e.popn(1);

                            match el {
                                Ok(v) => {
                                    ret = v;
                                }
                                Err(StackAction::Break(v)) => {
                                    ret = *v;
                                    break;
                                }
                                Err(StackAction::Next) => {}
                                Err(e) => {
                                    return Err(e);
                                }
                            }
                        }
                        Ok(ret)
                    })
                } else {
                    Ok(iter_next!(i.borrow_mut()))
                }
            }
            VVal::Opt(v) => {
                if argc == 0 {
                    if let Some(v) = v {
                        Ok(v.as_ref().clone())
                    } else {
                        Ok(VVal::None)
                    }
                } else {
                    let v = if let Some(v) = v { v.as_ref().clone() } else { VVal::None };

                    v.call_internal(env, argc)
                }
            }
            VVal::Usr(ud) => env.with_local_call_info(argc, |e: &mut Env| ud.call(e)),
            VVal::Ref(v) => v.borrow().call_internal(env, argc),
            VVal::HRef(v) => v.borrow().call_internal(env, argc),
            VVal::WWRef(v) => {
                if let Some(r) = v.upgrade() {
                    r.borrow().call_internal(env, argc)
                } else {
                    Ok(VVal::None)
                }
            }
            _ => Ok(self.clone()),
        }
    }

    pub fn to_ref(&self) -> VVal {
        match self {
            VVal::HRef(r) => VVal::Ref(r.clone()),
            VVal::Ref(r) => VVal::Ref(r.clone()),
            VVal::WWRef(v) => {
                if let Some(r) = v.upgrade() {
                    VVal::Ref(r)
                } else {
                    VVal::Ref(Rc::new(RefCell::new(VVal::None)))
                }
            }
            _ => VVal::Ref(Rc::new(RefCell::new(self.clone()))),
        }
    }

    pub fn to_hidden_boxed_ref(&self) -> VVal {
        VVal::HRef(Rc::new(RefCell::new(self.clone())))
    }

    pub fn assign_ref(&mut self, value: VVal) {
        match self {
            VVal::Ref(_) => {
                self.set_ref(value);
            }
            VVal::HRef(_) => {
                self.set_ref(value);
            }
            VVal::WWRef(_) => {
                self.set_ref(value);
            }
            v => {
                *v = value;
            }
        }
    }

    pub fn set_ref(&self, v: VVal) -> VVal {
        match self {
            VVal::Ref(r) => r.replace(v),
            VVal::HRef(r) => r.replace(v),
            VVal::WWRef(l) => {
                if let Some(r) = l.upgrade() {
                    r.replace(v)
                } else {
                    VVal::None
                }
            }
            _ => VVal::None,
        }
    }

    pub fn hide_ref(self) -> VVal {
        match self {
            VVal::HRef(f) => VVal::HRef(f),
            VVal::Ref(f) => VVal::HRef(f),
            VVal::WWRef(f) => {
                if let Some(r) = f.upgrade() {
                    VVal::HRef(r)
                } else {
                    VVal::None.to_ref().hide_ref()
                }
            }
            _ => self.to_ref().hide_ref(),
        }
    }

    pub fn upgrade(self) -> VVal {
        match self {
            VVal::HRef(f) => VVal::Ref(f),
            VVal::WWRef(f) => {
                if let Some(r) = f.upgrade() {
                    VVal::Ref(r)
                } else {
                    VVal::None.to_ref()
                }
            }
            _ => self.to_ref(),
        }
    }

    pub fn downgrade(self) -> VVal {
        match self {
            VVal::Ref(f) => VVal::WWRef(Rc::downgrade(&f)),
            VVal::HRef(f) => VVal::WWRef(Rc::downgrade(&f)),
            _ => self,
        }
    }

    pub fn map() -> VVal {
        VVal::Map(Rc::new(RefCell::new(FnvHashMap::with_capacity_and_hasher(
            2,
            Default::default(),
        ))))
    }

    pub fn map1(k: &str, v: VVal) -> VVal {
        let m = VVal::map();
        m.set_key_str(k, v).expect("single use");
        m
    }

    pub fn map2(k: &str, v: VVal, k2: &str, v2: VVal) -> VVal {
        let m = VVal::map();
        m.set_key_str(k, v).expect("single use");
        m.set_key_str(k2, v2).expect("single use");
        m
    }

    pub fn map3(k: &str, v: VVal, k2: &str, v2: VVal, k3: &str, v3: VVal) -> VVal {
        let m = VVal::map();
        m.set_key_str(k, v).expect("single use");
        m.set_key_str(k2, v2).expect("single use");
        m.set_key_str(k3, v3).expect("single use");
        m
    }

    pub fn map4(
        k: &str,
        v: VVal,
        k2: &str,
        v2: VVal,
        k3: &str,
        v3: VVal,
        k4: &str,
        v4: VVal,
    ) -> VVal {
        let m = VVal::map();
        m.set_key_str(k, v).expect("single use");
        m.set_key_str(k2, v2).expect("single use");
        m.set_key_str(k3, v3).expect("single use");
        m.set_key_str(k4, v4).expect("single use");
        m
    }

    pub fn map5(
        k: &str,
        v: VVal,
        k2: &str,
        v2: VVal,
        k3: &str,
        v3: VVal,
        k4: &str,
        v4: VVal,
        k5: &str,
        v5: VVal,
    ) -> VVal {
        let m = VVal::map();
        m.set_key_str(k, v).expect("single use");
        m.set_key_str(k2, v2).expect("single use");
        m.set_key_str(k3, v3).expect("single use");
        m.set_key_str(k4, v4).expect("single use");
        m.set_key_str(k5, v5).expect("single use");
        m
    }

    pub fn sym(s: &str) -> VVal {
        VVal::Sym(s2sym(s))
    }

    pub fn to_sym(&self) -> Symbol {
        if let VVal::Sym(s) = self {
            s.clone()
        } else {
            self.with_s_ref(s2sym)
        }
    }

    #[allow(clippy::cast_ptr_alignment)]
    pub fn ref_id(&self) -> Option<i64> {
        Some(match self {
            VVal::Err(r) => &*r.borrow() as *const (VVal, SynPos) as i64,
            VVal::Str(s) => &*s.as_ref() as *const String as i64,
            VVal::Byt(s) => &*s.as_ref() as *const Vec<u8> as i64,
            VVal::Sym(s) => s.ref_id(),
            VVal::Lst(v) => &*v.borrow() as *const Vec<VVal> as i64,
            VVal::Map(v) => &*v.borrow() as *const FnvHashMap<Symbol, VVal> as i64,
            VVal::Iter(v) => &*v.borrow() as *const VValIter as i64,
            VVal::Opt(p) => {
                if let Some(p) = p {
                    &*p.as_ref() as *const VVal as i64
                } else {
                    0
                }
            }
            VVal::Fun(f) => &**f as *const VValFun as i64,
            VVal::DropFun(f) => &**f as *const DropFun as i64,
            VVal::Pair(v) => &**v as *const (VVal, VVal) as i64,
            VVal::Ref(v) => &*v.borrow() as *const VVal as i64,
            VVal::HRef(v) => &*v.borrow() as *const VVal as i64,
            VVal::Usr(b) => &**b as *const dyn VValUserData as *const usize as i64,
            VVal::WWRef(r) => {
                if let Some(l) = r.upgrade() {
                    &*l.borrow() as *const VVal as i64
                } else {
                    return None;
                }
            }
            VVal::Type(v) => &**v as *const Type as i64,
            _ => return None,
        })
    }

    pub fn eqv(&self, v: &VVal) -> bool {
        match self {
            VVal::None => {
                matches!(v, VVal::None)
            }
            VVal::Bol(ia) => {
                if let VVal::Bol(ib) = v {
                    ia == ib
                } else {
                    false
                }
            }
            VVal::Int(ia) => {
                if let VVal::Int(ib) = v {
                    ia == ib
                } else {
                    false
                }
            }
            VVal::Chr(ca) => {
                if let VVal::Chr(cb) = v {
                    ca == cb
                } else {
                    false
                }
            }
            VVal::Flt(ia) => {
                if let VVal::Flt(ib) = v {
                    (ia - ib).abs() < std::f64::EPSILON
                } else {
                    false
                }
            }
            VVal::Sym(s) => {
                if let VVal::Sym(ib) = v {
                    s == ib
                } else {
                    false
                }
            }
            VVal::Syn(s) => {
                if let VVal::Syn(ib) = v {
                    s.syn == ib.syn
                } else {
                    false
                }
            }
            VVal::Str(s) => {
                if let VVal::Str(ib) = v {
                    *s == *ib
                } else {
                    false
                }
            }
            VVal::Byt(s) => {
                if let VVal::Byt(s2) = v {
                    s[..] == s2[..]
                } else {
                    false
                }
            }
            VVal::Pair(b) => {
                if let VVal::Pair(b2) = v {
                    b.0.eqv(&b2.0) && b.1.eqv(&b2.1)
                } else {
                    false
                }
            }
            VVal::Opt(a) => {
                if let VVal::Opt(b) = v {
                    if let Some(a) = a {
                        if let Some(b) = b {
                            a.eqv(b)
                        } else {
                            false
                        }
                    } else {
                        b.is_none()
                    }
                } else {
                    false
                }
            }
            VVal::Lst(l) => {
                if let VVal::Lst(l2) = v {
                    Rc::ptr_eq(l, l2)
                } else {
                    false
                }
            }
            VVal::Map(l) => {
                if let VVal::Map(l2) = v {
                    Rc::ptr_eq(l, l2)
                } else {
                    false
                }
            }
            VVal::Fun(l) => {
                if let VVal::Fun(l2) = v {
                    Rc::ptr_eq(l, l2)
                } else {
                    false
                }
            }
            VVal::IVec(iv) => match v {
                VVal::IVec(v) => *v == *iv,
                _ => false,
            },
            VVal::FVec(fv) => match v {
                VVal::FVec(v) => *v == *fv,
                _ => false,
            },
            VVal::DropFun(l) => {
                if let VVal::DropFun(l2) = v {
                    Rc::ptr_eq(l, l2)
                } else {
                    false
                }
            }
            VVal::Err(l) => {
                if let VVal::Err(l2) = v {
                    Rc::ptr_eq(l, l2)
                } else {
                    false
                }
            }
            VVal::Iter(i) => {
                if let VVal::Iter(i2) = v {
                    Rc::ptr_eq(i, i2)
                } else {
                    false
                }
            }
            VVal::Ref(l) => vval_rc_ptr_eq(v, l),
            VVal::HRef(l) => vval_rc_ptr_eq(v, l),
            VVal::WWRef(lw) => {
                if let Some(l) = lw.upgrade() {
                    vval_rc_ptr_eq(v, &l)
                } else {
                    false
                }
            }
            VVal::Usr(u) => {
                if let VVal::Usr(u2) = v {
                    u.eqv(u2)
                } else {
                    false
                }
            }
            VVal::Type(t) => {
                if let VVal::Type(t2) = v {
                    t == t2
                } else {
                    false
                }
            }
        }
    }

    fn dump_vec_as_str(v: &Rc<RefCell<std::vec::Vec<VVal>>>, c: &mut CycleCheck) -> String {
        let mut out: Vec<String> = Vec::new();
        let mut first = true;
        out.push(String::from("$["));
        for s in v.borrow().iter().map(|v| v.s_cy(c)) {
            if !first {
                out.push(String::from(","));
            } else {
                first = false;
            }
            out.push(s);
        }
        out.push(String::from("]"));
        out.concat()
    }

    fn dump_sym(s: &str) -> String {
        if s.chars().all(|c| {
            c.is_alphanumeric() || c == '_' || c == '-' || c == '+' || c == '&' || c == '@'
        }) {
            format!(":{}", s)
        } else {
            format!(":\"{}\"", s)
        }
    }

    fn dump_map_as_str(m: &Rc<RefCell<FnvHashMap<Symbol, VVal>>>, c: &mut CycleCheck) -> String {
        let mut out: Vec<String> = Vec::new();
        let mut first = true;
        out.push(String::from("${"));
        let hm = m.borrow();

        let mut keys: Vec<&Symbol> = hm.keys().collect();
        keys.sort();
        for k in keys {
            let v = hm.get(k).unwrap();
            if !first {
                out.push(String::from(","));
            } else {
                first = false;
            }
            if k.as_ref().chars().any(char::is_whitespace) {
                out.push(format!("\"{}\"", k.as_ref()));
            } else {
                out.push(k.as_ref().to_string());
            }
            out.push(String::from("="));
            out.push(v.s_cy(c));
        }
        out.push(String::from("}"));
        out.concat()
    }

    pub fn map_ok_skip<T, R>(&self, mut op: T, skip: usize) -> Vec<R>
    where
        T: FnMut(&VVal) -> R,
    {
        let mut res: Vec<R> = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow().iter().skip(skip) {
                res.push(op(i));
            }
        }
        res
    }

    pub fn map_skip<R, E, T>(&self, mut op: T, skip: usize) -> Result<Vec<R>, E>
    where
        T: FnMut(&VVal) -> Result<R, E>,
    {
        let mut res: Vec<R> = Vec::new();
        if let VVal::Lst(b) = &self {
            for i in b.borrow().iter().skip(skip) {
                res.push(op(i)?);
            }
        }
        Ok(res)
    }

    pub fn map_skip_vval<E, T>(&self, mut op: T, skip: usize) -> Result<VVal, E>
    where
        T: FnMut(&VVal, bool) -> Result<VVal, E>,
    {
        let res = VVal::vec();
        if let VVal::Lst(b) = &self {
            let len = b.borrow().len();
            let len = if len > skip { len - skip } else { 0 };

            for (i, v) in b.borrow().iter().enumerate().skip(skip) {
                let is_last = (i + 1) == len;
                res.push(op(v, is_last)?);
            }
        }
        Ok(res)
    }

    pub fn unshift(&self, val: VVal) -> &VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().insert(0, val);
            self
        } else {
            self.with_deref(
                move |v| {
                    v.unshift(val);
                },
                |_| (),
            );
            self
        }
    }

    pub fn has(&self, elem: &VVal) -> bool {
        let collection = if self.is_ref() { self.deref() } else { self.clone() };

        if collection.is_map() {
            return elem.with_s_ref(|key| {
                if let Some(_) = collection.get_key(key) {
                    return true;
                }
                return false;
            });
        } else {
            return collection.with_iter(|it| {
                for (v, _k) in it {
                    if v.eqv(elem) {
                        return true;
                    }
                }
                return false;
            });
        }
    }

    pub fn add(&self, vals: &[VVal], list_add: CollectionAdd) -> VVal {
        let mut collection = if self.is_ref() { self.deref() } else { self.clone() };
        //d// println!("ADD: {:?}", vals);

        match &collection {
            VVal::Lst(lst) => {
                for v in vals.iter() {
                    v.clone().with_value_or_iter_values(|v, _k| {
                        match list_add {
                            CollectionAdd::Push => {
                                lst.borrow_mut().push(v);
                            }
                            CollectionAdd::Unshift => {
                                lst.borrow_mut().insert(0, v);
                            }
                            CollectionAdd::Uniq => {
                                if !collection.has(&v) {
                                    lst.borrow_mut().push(v);
                                }
                            }
                        }
                        true
                    });
                }
            }
            VVal::Map(map) => {
                for v in vals.iter() {
                    v.clone().with_value_or_iter_values(|v, k| {
                        match list_add {
                            CollectionAdd::Uniq => {
                                if !collection.has(&v) {
                                    v.with_s_ref(|s| {
                                        map.borrow_mut().insert(s2sym(s), VVal::Bol(true));
                                    })
                                }
                            }
                            _ => match v {
                                VVal::Pair(_) => {
                                    v.at(0).unwrap().with_s_ref(|k| {
                                        map.borrow_mut().insert(s2sym(k), v.at(1).unwrap())
                                    });
                                }
                                VVal::Lst(_) => {
                                    v.v_(0).with_s_ref(|k| {
                                        map.borrow_mut().insert(s2sym(k), v.clone())
                                    });
                                }
                                VVal::Map(_) => {
                                    for (vm, km) in v.iter() {
                                        if let Some(k) = km {
                                            k.with_s_ref(|ks| {
                                                map.borrow_mut().insert(s2sym(ks), vm.clone())
                                            });
                                        }
                                    }
                                }
                                _ => {
                                    if let Some(k) = k {
                                        k.with_s_ref(|kv| {
                                            map.borrow_mut().insert(s2sym(kv), v.clone())
                                        });
                                    } else {
                                        v.with_s_ref(|kv| {
                                            map.borrow_mut().insert(s2sym(kv), v.clone())
                                        });
                                    }
                                }
                            },
                        }
                        true
                    })
                }
            }
            VVal::Str(_) => {
                let mut out = collection.with_s_ref(|s| s.to_string());
                for v in vals.iter() {
                    v.clone().with_value_or_iter_values(|v, _k| {
                        match list_add {
                            CollectionAdd::Uniq => {
                                if !collection.has(&v) {
                                    v.with_s_ref(|s| {
                                        out.push_str(s);
                                    });
                                }
                            }
                            _ => {
                                v.with_s_ref(|s| out.push_str(s));
                            }
                        }
                        true
                    });
                }
                collection = VVal::new_str_mv(out);
            }
            VVal::Byt(_) => {
                let mut out = collection.with_bv_ref(|b| b.to_vec());
                for v in vals.iter() {
                    v.clone().with_value_or_iter_values(|v, _k| {
                        match list_add {
                            CollectionAdd::Uniq => {
                                if !collection.has(&v) {
                                    v.with_bv_ref(|b| out.extend_from_slice(b));
                                }
                            }
                            _ => {
                                v.with_bv_ref(|b| out.extend_from_slice(b));
                            }
                        }
                        true
                    })
                }
                collection = VVal::new_byt(out);
            }
            v => {
                return VVal::err_msg(&format!(
                    "Can't add to non collection, got '{}' (type {})",
                    v.s(),
                    v.type_name()
                ));
            }
        }

        collection
        //        let collection =
        //            if (b.is_ref() && b.deref().is_vec() {
        //               b.deref()
        //            } else if !b.is_vec() {
        //               VVal::vec1(b)
        //            } else {
        //                b
        //            };
        //
        //        if !b.is_vec() {
        //        }
        //
        //        match list_add {
        //            CollectionAdd::Push => {
        //                if let VVal::Iter(_) = a {
        //                    a.for_each(|v| b.push(v));
        //
        //                } else if a.is_ref() {
        //                    if let VVal::Iter(_) = a.deref() {
        //                        a.deref().for_each(|v| b.push(v));
        //                    } else {
        //                        b.push(a);
        //                    }
        //                } else {
        //                    b.push(a);
        //                }
        //            },
        //            CollectionAdd::Unshift => {
        //                if let VVal::Iter(_) = a {
        //                    a.for_each(|v| b.unshift(v));
        //
        //                } else if a.is_ref() {
        //                    if let VVal::Iter(_) = a.deref() {
        //                        a.deref().for_each(|v| b.unshift(v));
        //                    } else {
        //                        b.unshift(a);
        //                    }
        //                } else {
        //                    b.unshift(a);
        //                }
        //            },
        //        }
        //
        //        self
    }

    pub fn insert_at(&self, index: usize, val: VVal) {
        if let VVal::Lst(b) = &self {
            if index > b.borrow().len() {
                b.borrow_mut().resize_with(index, || VVal::None);
            }

            b.borrow_mut().insert(index, val);
        } else {
            self.with_deref(|v| v.insert_at(index, val), |_| ())
        }
    }

    pub fn remove_at(&self, index: usize) -> VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().remove(index)
        } else {
            self.with_deref(|v| v.remove_at(index), |_| VVal::None)
        }
    }

    pub fn set(&self, index: usize, val: VVal) {
        if let VVal::Lst(b) = &self {
            if index >= b.borrow().len() {
                b.borrow_mut().resize(index + 1, VVal::None);
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
            VVal::Byt(vval_bytes) => {
                if index as usize >= vval_bytes.len() {
                    None
                } else {
                    Some(VVal::new_byte(vval_bytes[index as usize]))
                }
            }
            VVal::Str(vval_str) => vval_str.chars().nth(index as usize).map(VVal::new_char),
            VVal::Pair(b) => Some(if index % 2 == 0 { b.0.clone() } else { b.1.clone() }),
            VVal::Iter(i) => {
                let mut i = i.borrow_mut();
                for _ in 0..index {
                    i.next();
                }
                iter_next_value!(i, v, { Some(v) }, None)
            }
            VVal::Opt(ref b) => {
                if let Some(b) = b {
                    b.as_ref().at(index)
                } else {
                    None
                }
            }
            VVal::IVec(b) => Some(match index {
                0 => b.x(),
                1 => b.y(),
                2 => b.z().unwrap_or(VVal::None),
                3 => b.w().unwrap_or(VVal::None),
                _ => VVal::None,
            }),
            VVal::FVec(b) => Some(match index {
                0 => b.x(),
                1 => b.y(),
                2 => b.z().unwrap_or(VVal::None),
                3 => b.w().unwrap_or(VVal::None),
                _ => VVal::None,
            }),
            VVal::Lst(b) => {
                if b.borrow().len() > index {
                    Some(b.borrow()[index].clone())
                } else {
                    None
                }
            }
            v => v.with_deref(
                |v| v.at(index),
                |v| if let Some(v) = v { v.get_key(&format!("{}", index)) } else { None },
            ),
        }
    }

    pub fn proto_data(&self) -> VVal {
        match self {
            VVal::Map(m) => m.borrow().get(&s2sym("_data")).cloned().unwrap_or(VVal::None),
            VVal::Lst(l) => {
                if l.borrow().len() > 1 {
                    l.borrow()[1].clone()
                } else {
                    VVal::None
                }
            }
            v => v.with_deref(|v| v.proto_data(), |_| VVal::None),
        }
    }

    pub fn proto_lookup(&self, key: &str) -> Option<VVal> {
        match self {
            VVal::Map(m) => {
                if let Some(func) = m.borrow().get(&s2sym(key)) {
                    Some(func.clone())
                } else if let Some(proto) = m.borrow().get(&s2sym("_proto")) {
                    proto.proto_lookup(key)
                } else {
                    None
                }
            }
            VVal::Lst(l) => {
                let l = l.borrow();
                if l.is_empty() {
                    None
                } else {
                    l[0].proto_lookup(key)
                }
            }
            v => v.with_deref(|v| v.proto_lookup(key), |_| None),
        }
    }

    pub fn get_key_sym(&self, key: &Symbol) -> Option<VVal> {
        match self {
            VVal::Map(m) => m.borrow().get(key).cloned(),
            _ => self.get_key(key.as_ref()),
        }
    }

    pub fn get_key(&self, key: &str) -> Option<VVal> {
        match self {
            VVal::Map(m) => m.borrow().get(&s2sym(key)).cloned(),
            VVal::IVec(b) => Some(match key {
                "0" | "first" | "x" | "r" | "h" => b.x(),
                "1" | "second" | "y" | "g" | "s" => b.y(),
                "2" | "third" | "z" | "b" | "v" => b.z().unwrap_or(VVal::None),
                "3" | "fourth" | "w" | "a" => b.w().unwrap_or(VVal::None),
                _ => swizzle_i(
                    key,
                    b.x_raw(),
                    b.y_raw(),
                    b.z_raw().unwrap_or(0),
                    b.w_raw().unwrap_or(0),
                ),
            }),
            VVal::FVec(b) => Some(match key {
                "0" | "first" | "x" | "r" | "h" => b.x(),
                "1" | "second" | "y" | "g" | "s" => b.y(),
                "2" | "third" | "z" | "b" | "v" => b.z().unwrap_or(VVal::None),
                "3" | "fourth" | "w" | "a" => b.w().unwrap_or(VVal::None),
                _ => swizzle_f(
                    key,
                    b.x_raw(),
                    b.y_raw(),
                    b.z_raw().unwrap_or(0.0),
                    b.w_raw().unwrap_or(0.0),
                ),
            }),
            VVal::Pair(_) => self.at(match key {
                "0" | "value" | "v" | "car" | "head" | "first" => 0,
                "1" | "key" | "k" | "cdr" | "tail" | "second" => 1,
                _ => {
                    if !key.chars().all(|c| c.is_digit(10)) {
                        return None;
                    }
                    key.parse::<usize>().unwrap_or(0)
                }
            }),
            VVal::Iter(i) => {
                if !key.chars().all(|c| c.is_digit(10)) {
                    return None;
                }

                let idx = key.parse::<usize>().unwrap_or(0);
                let mut i = i.borrow_mut();
                for _ in 0..idx {
                    i.next();
                }
                iter_next_value!(i, v, { Some(v) }, None)
            }
            VVal::Lst(l) => {
                if !key.chars().all(|c| c.is_digit(10)) {
                    return None;
                }

                let idx = key.parse::<usize>().unwrap_or(0);
                if idx < l.borrow().len() {
                    Some(l.borrow()[idx].clone())
                } else {
                    Some(VVal::None)
                }
            }
            VVal::Usr(u) => u.get_key(key),
            v => v.with_deref(|v| v.get_key(key), |_| None),
        }
    }

    pub fn set_map_key_fun<T>(
        &self,
        key: &str,
        fun: T,
        min_args: Option<usize>,
        max_args: Option<usize>,
        err_arg_ok: bool,
    ) where
        T: 'static + Fn(&mut Env, usize) -> Result<VVal, StackAction>,
    {
        self.set_key_sym(s2sym(key), VValFun::new_fun(fun, min_args, max_args, err_arg_ok))
            .expect("Map not borrowed when using set_map_key_fun");
    }

    pub fn deref(&self) -> VVal {
        self.with_deref(|v| v.clone(), |v| v.map_or(VVal::None, |v| v.clone()))
    }

    #[inline]
    pub fn with_deref<O, D, R>(&self, op: O, default: D) -> R
    where
        O: FnOnce(&VVal) -> R,
        D: FnOnce(Option<&VVal>) -> R,
    {
        match self {
            VVal::Opt(Some(v)) => op(v.as_ref()),
            VVal::Opt(None) => op(&VVal::None),
            VVal::Ref(l) => op(&(*l).borrow()),
            VVal::HRef(l) => op(&(*l).borrow()),
            VVal::WWRef(l) => match l.upgrade() {
                Some(v) => op(&v.borrow()),
                None => default(None),
            },
            _ => default(Some(self)),
        }
    }

    pub fn list_operation<O, R>(&self, mut op: O) -> Result<R, StackAction>
    where
        O: FnMut(&mut std::cell::RefMut<std::vec::Vec<VVal>>) -> R,
    {
        match self {
            VVal::Lst(l) => match l.try_borrow_mut() {
                Ok(mut v) => Ok(op(&mut v)),
                Err(_) => Err(StackAction::panic_borrow(self)),
            },
            v => v.with_deref(
                |v| v.list_operation(op),
                |v| {
                    Err(StackAction::panic_msg(format!(
                        "Can't do list operation with non list value: {}",
                        v.map_or("".to_string(), |v| v.s())
                    )))
                },
            ),
        }
    }

    pub fn delete_key(&self, key: &VVal) -> Result<VVal, StackAction> {
        match self {
            VVal::Map(m) => {
                let ks = key.to_sym();
                match m.try_borrow_mut() {
                    Ok(mut r) => Ok(r.remove(&ks).unwrap_or(VVal::None)),
                    Err(_) => Err(StackAction::panic_borrow(self)),
                }
            }
            VVal::Lst(l) => {
                if key.i() < 0 {
                    return Ok(VVal::None);
                }

                let idx = key.i() as usize;
                match l.try_borrow_mut() {
                    Ok(mut v) => {
                        if idx < v.len() {
                            Ok(v.remove(idx))
                        } else {
                            Ok(VVal::None)
                        }
                    }
                    Err(_) => Err(StackAction::panic_borrow(self)),
                }
            }
            VVal::Usr(u) => u.delete_key(key),
            v => v.with_deref(|v| v.delete_key(key), |_| Ok(VVal::None)),
        }
    }

    pub fn set_key_str(&self, key: &str, val: VVal) -> Result<(), StackAction> {
        self.set_key_sym(s2sym(key), val)
    }

    pub fn set_key_sym(&self, key: Symbol, val: VVal) -> Result<(), StackAction> {
        match self {
            VVal::Map(m) => match m.try_borrow_mut() {
                Ok(mut r) => {
                    r.insert(key, val);
                    Ok(())
                }
                Err(_) => Err(StackAction::panic_borrow(self)),
            },
            _ => self.set_key(&VVal::Sym(key), val),
        }
    }

    pub fn set_key(&self, key: &VVal, val: VVal) -> Result<(), StackAction> {
        match self {
            VVal::Map(m) => {
                let ks = key.to_sym();
                match m.try_borrow_mut() {
                    Ok(mut r) => {
                        r.insert(ks, val);
                        Ok(())
                    }
                    Err(_) => Err(StackAction::panic_borrow(self)),
                }
            }
            VVal::Lst(l) => {
                if key.i() < 0 {
                    return Ok(());
                }

                let idx = key.i() as usize;
                match l.try_borrow_mut() {
                    Ok(mut v) => {
                        if v.len() <= idx {
                            v.resize(idx + 1, VVal::None);
                        }
                        v[idx] = val;
                        Ok(())
                    }
                    Err(_) => Err(StackAction::panic_borrow(self)),
                }
            }
            VVal::FVec(_) => Err(StackAction::panic_msg("Can't mutate float vector".to_string())),
            VVal::IVec(_) => Err(StackAction::panic_msg("Can't mutate integer vector".to_string())),
            VVal::Pair(_) => Err(StackAction::panic_msg("Can't mutate pair".to_string())),
            VVal::Usr(u) => u.set_key(key, val),
            v => v.with_deref(|v| v.set_key(key, val), |_| Ok(())),
        }
    }

    pub fn pop(&self) -> VVal {
        if let VVal::Lst(b) = &self {
            b.borrow_mut().pop().unwrap_or(VVal::None)
        } else {
            self.with_deref(|v| v.pop(), |_| VVal::None)
        }
    }

    pub fn accum(&mut self, val: &VVal) {
        match self {
            VVal::Byt(ref mut b) => match val {
                VVal::Int(i) => {
                    Rc::make_mut(b).push(*i as u8);
                }
                VVal::Flt(f) => {
                    Rc::make_mut(b).push(*f as u8);
                }
                VVal::Str(s) => {
                    Rc::make_mut(b).extend_from_slice(s.as_bytes());
                }
                VVal::Sym(s) => {
                    Rc::make_mut(b).extend_from_slice(s.as_bytes());
                }
                VVal::Byt(s) => {
                    Rc::make_mut(b).extend_from_slice(s.as_ref());
                }
                VVal::Bol(o) => {
                    Rc::make_mut(b).push(*o as u8);
                }
                _ => {
                    val.with_bv_ref(|s: &[u8]| Rc::make_mut(b).extend_from_slice(s));
                }
            },
            VVal::Str(ref mut a) => match val {
                VVal::Str(s) => {
                    Rc::make_mut(a).push_str(s.as_ref());
                }
                VVal::Sym(s) => {
                    Rc::make_mut(a).push_str(&*s);
                }
                VVal::Byt(s) => {
                    for b in s.as_ref().iter() {
                        let b = *b as char;
                        Rc::make_mut(a).push(b);
                    }
                }
                _ => {
                    val.with_s_ref(|s: &str| Rc::make_mut(a).push_str(s));
                }
            },
            VVal::Int(i) => {
                *i += val.i();
            }
            VVal::Flt(f) => {
                *f += val.f();
            }
            VVal::Lst(v) => {
                v.borrow_mut().push(val.clone());
            }
            _ => (),
        }
    }

    pub fn push(&self, val: VVal) -> &VVal {
        //d// println!("FN PUSH {} v {}", self.s(), val.s());
        if let VVal::Lst(b) = &self {
            //d// println!("FN ! PUSH {} v {}", self.s(), val.s());
            b.borrow_mut().push(val);
        } else {
            self.with_deref(
                |v| {
                    v.push(val);
                },
                |_| (),
            )
        }
        self
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            VVal::Chr(_) => 1,
            VVal::Lst(l) => l.borrow().len(),
            VVal::Map(l) => l.borrow().len(),
            VVal::Byt(l) => l.len(),
            VVal::Str(l) => l.len(),
            VVal::Sym(l) => l.len(),
            v => v.with_deref(|v| v.len(), |_| 0),
        }
    }

    pub fn s_len(&self) -> usize {
        match self {
            VVal::Chr(_) => 1,
            VVal::Str(s) => s.chars().count(),
            VVal::Sym(s) => s.chars().count(),
            VVal::Usr(s) => s.s_raw().chars().count(),
            VVal::Byt(b) => b.len(),
            VVal::None => 0,
            _ => self.s().chars().count(),
        }
    }

    /// Returns the string data or converts the value to a string for displaying.
    /// The conversion format is not for reading the value in again via
    /// the WLambda parser, it's for accessing the data as pure as possible.
    ///
    /// Use this if you need the raw unescaped contents of VVal::Str, VVal::Sym,
    /// VVal::Byt and other VVals.
    ///
    /// As this is used usually for generating output a VVal::None is
    /// turned into an empty string
    ///
    /// **There is also `with_s_ref()` which allows you to work with a
    /// reference to the string, in case you don't need to own the copy!**
    ///
    /// ```
    /// use wlambda::VVal;
    ///
    /// assert_eq!(VVal::None.s_raw(), "");
    /// assert_eq!(VVal::new_str("Test").s_raw(), "Test");
    /// ```
    pub fn s_raw(&self) -> String {
        match self {
            VVal::Chr(c) => {
                let mut buf = [0; 6];
                let chrstr = c.c().encode_utf8(&mut buf);
                chrstr.to_string()
            }
            VVal::Str(s) => s.as_ref().clone(),
            VVal::Sym(s) => String::from(s.as_ref()),
            VVal::Usr(s) => s.s_raw(),
            VVal::Byt(s) => s.iter().map(|b| *b as char).collect(),
            VVal::None => String::from(""),
            v => v.with_deref(
                |v| {
                    if v.is_none() {
                        "".to_string()
                    } else {
                        v.s_raw()
                    }
                },
                |v| {
                    v.map_or_else(
                        || "".to_string(),
                        |v| if v.is_none() { "".to_string() } else { v.s() },
                    )
                },
            ),
        }
    }

    /// Like s_raw() but returns a reference to the string.
    ///
    /// Use this if you need the raw unescaped contents of VVal::Str, VVal::Sym,
    /// VVal::Byt and other VVals.
    ///
    /// As this is used usually for generating output a VVal::None is
    /// turned into an empty string
    ///
    /// ```
    /// use wlambda::VVal;
    ///
    /// VVal::None.with_s_ref(
    ///     |s: &str| assert_eq!(s, ""));
    ///
    /// VVal::new_str("Test").with_s_ref(
    ///     |s: &str| assert_eq!(s, "Test"));
    /// ```
    #[inline]
    pub fn with_s_ref<T, R>(&self, f: T) -> R
    where
        T: FnOnce(&str) -> R,
    {
        match self {
            VVal::Chr(c) => {
                let mut buf = [0; 6];
                let chrstr = c.c().encode_utf8(&mut buf);
                f(chrstr)
            }
            VVal::Str(s) => f(s.as_ref()),
            VVal::Sym(s) => f(&*s),
            VVal::Usr(s) => f(&s.s_raw()),
            VVal::Byt(_) => f(&self.s_raw()),
            VVal::None => f(""),
            _ => f(&self.s_raw()),
        }
    }

    #[inline]
    pub fn with_bv_ref<T, R>(&self, f: T) -> R
    where
        T: FnOnce(&[u8]) -> R,
    {
        match self {
            VVal::Chr(c) => f(&[c.byte()]),
            VVal::Byt(v) => f(&v.as_ref()[..]),
            VVal::Str(_) => self.with_s_ref(|s| f(s.as_bytes())),
            _ => {
                let vec = self.as_bytes();
                f(&vec[..])
            }
        }
    }

    pub fn is_float(&self) -> bool {
        matches!(self, VVal::Flt(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, VVal::Int(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, VVal::Chr(VValChr::Char(_)))
    }

    pub fn is_byte(&self) -> bool {
        matches!(self, VVal::Chr(VValChr::Byte(_)))
    }

    pub fn is_sym(&self) -> bool {
        matches!(self, VVal::Sym(_))
    }

    pub fn is_syn(&self) -> bool {
        matches!(self, VVal::Syn(_))
    }

    pub fn get_syn_pos(&self) -> SynPos {
        if let VVal::Syn(s) = self {
            s.clone()
        } else {
            SynPos::empty()
        }
    }

    pub fn syn(&self) -> Option<Syntax> {
        if let VVal::Syn(s) = self {
            Some(s.syn())
        } else {
            None
        }
    }

    pub fn set_syn(&mut self, syn: Syntax) {
        if let VVal::Syn(s) = self {
            s.set_syn(syn);
        }
    }

    pub fn set_syn_at(&self, idx: usize, syn: Syntax) {
        let mut v = self.v_(idx);
        v.set_syn(syn);
        self.set(idx, v);
    }

    pub fn get_syn(&self) -> Syntax {
        if let VVal::Syn(s) = self {
            s.syn()
        } else {
            Syntax::Block
        }
    }

    pub fn compile_err(&self, msg: String) -> CompileError {
        CompileError { msg, pos: self.at(0).unwrap_or(VVal::None).get_syn_pos() }
    }

    pub fn to_compile_err(&self, msg: String) -> Result<EvalNode, CompileError> {
        Err(self.compile_err(msg))
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, VVal::Pair(_))
    }

    pub fn is_iter(&self) -> bool {
        matches!(self, VVal::Iter(_))
    }

    pub fn is_optional(&self) -> bool {
        matches!(self, VVal::Opt(_))
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, VVal::Ref(_) | VVal::HRef(_) | VVal::WWRef(_))
    }

    pub fn is_wref(&self) -> bool {
        matches!(self, VVal::WWRef(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, VVal::Bol(_))
    }

    pub fn is_bytes(&self) -> bool {
        matches!(self, VVal::Byt(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, VVal::Str(_))
    }

    /// Returns true if the VVal is a string, symbol, byte vector, byte or character.
    pub fn is_kind_of_string(&self) -> bool {
        matches!(self, VVal::Chr(_) | VVal::Sym(_) | VVal::Str(_) | VVal::Byt(_))
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, VVal::Fun(_))
    }

    pub fn is_vec(&self) -> bool {
        matches!(self, VVal::Lst(_))
    }

    pub fn is_nvec(&self) -> bool {
        matches!(self, VVal::FVec(_) | VVal::IVec(_))
    }

    pub fn is_ivec(&self) -> bool {
        matches!(self, VVal::IVec(_))
    }

    pub fn is_fvec(&self) -> bool {
        matches!(self, VVal::FVec(_))
    }

    pub fn nvec_len(&self) -> usize {
        match self {
            VVal::IVec(nv) => nv.dims().len(),
            VVal::FVec(nv) => nv.dims().len(),
            _ => 0,
        }
    }

    pub fn is_map(&self) -> bool {
        matches!(self, VVal::Map(_))
    }

    pub fn is_some(&self) -> bool {
        !matches!(self, VVal::Opt(None) | VVal::None)
    }

    pub fn map_some<T, R>(&self, r: R, f: T) -> R
    where
        T: FnOnce(R, &VVal) -> R,
    {
        match self {
            VVal::None => r,
            VVal::Opt(Some(v)) => f(r, &*v),
            _ => f(r, self),
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, VVal::Opt(None) | VVal::None)
    }

    pub fn is_err(&self) -> bool {
        matches!(self, VVal::Err(_))
    }

    pub fn syntax_type(&self) -> &str {
        if let VVal::Syn(sp) = self {
            match sp.syn {
                Syntax::Var => "Var",
                Syntax::Key => "Key",
                Syntax::SetKey => "SetKey",
                Syntax::GetKey => "GetKey",
                Syntax::GetKey2 => "GetKey2",
                Syntax::GetKey3 => "GetKey3",
                Syntax::GetSym => "GetSym",
                Syntax::GetSym2 => "GetSym2",
                Syntax::GetSym3 => "GetSym3",
                Syntax::GetIdx => "GetIdx",
                Syntax::GetIdx2 => "GetIdx2",
                Syntax::GetIdx3 => "GetIdx3",
                Syntax::BinOpAdd => "BinOpAdd",
                Syntax::BinOpSub => "BinOpSub",
                Syntax::BinOpMul => "BinOpMul",
                Syntax::BinOpDiv => "BinOpDiv",
                Syntax::BinOpMod => "BinOpMod",
                Syntax::BinOpLe => "BinOpLe",
                Syntax::BinOpLt => "BinOpLt",
                Syntax::BinOpGe => "BinOpGe",
                Syntax::BinOpGt => "BinOpGt",
                Syntax::BinOpEq => "BinOpEq",
                Syntax::BinOpSomeOr => "BinOpSomeOr",
                Syntax::BinOpExtSomeOr => "BinOpExtSomeOr",
                Syntax::BinOpNoneOr => "BinOpNoneOr",
                Syntax::BinOpErrOr => "BinOpErrOr",
                Syntax::BinOpOptOr => "BinOpOptOr",
                Syntax::OpNewPair => "OpNewPair",
                Syntax::OpCallLwR => "OpCallLwR",
                Syntax::OpCallRwL => "OpCallRwL",
                Syntax::OpCallApplyLwR => "OpCallApplyLwR",
                Syntax::OpCallApplyRwL => "OpCallApplyRwL",
                Syntax::OpColAddL => "OpColAddL",
                Syntax::OpColAddR => "OpColAddR",
                Syntax::TRoot => "TRoot",
                Syntax::TBinOp => "TBinOp",
                Syntax::TOp => "TOp",
                Syntax::TNL => "TNL",
                Syntax::TComment => "TComment",
                Syntax::TDelim => "TDelim",
                Syntax::TIdent => "TIdent",
                Syntax::TNum => "TNum",
                Syntax::TLiteral => "TLiteral",
                Syntax::TValue => "TValue",
                Syntax::TArgList => "TArgList",
                Syntax::TNone => "TNone",
                Syntax::T => "T",
                Syntax::TQ => "TQ",
                Syntax::Str => "Str",
                Syntax::Lst => "Lst",
                Syntax::IVec => "IVec",
                Syntax::FVec => "FVec",
                Syntax::Opt => "Opt",
                Syntax::Iter => "Iter",
                Syntax::Map => "Map",
                Syntax::Expr => "Expr",
                Syntax::Func => "Func",
                Syntax::Block => "Block",
                Syntax::Err => "Err",
                Syntax::Call => "Call",
                Syntax::Type => "Type",
                Syntax::Apply => "Apply",
                Syntax::And => "And",
                Syntax::Or => "Or",
                Syntax::Assign => "Assign",
                Syntax::Def => "Def",
                Syntax::Ref => "Ref",
                Syntax::HRef => "HRef",
                Syntax::WRef => "WRef",
                Syntax::Deref => "Deref",
                Syntax::CaptureRef => "CaptureRef",
                Syntax::AssignRef => "AssignRef",
                Syntax::DefGlobRef => "DefGlobRef",
                Syntax::DefConst => "DefConst",
                Syntax::SelfObj => "SelfObj",
                Syntax::SelfData => "SelfData",
                Syntax::Import => "Import",
                Syntax::Export => "Export",
                Syntax::DumpStack => "DumpStack",
                Syntax::DumpVM => "DumpVM",
                Syntax::DebugPrint => "DebugPrint",
                Syntax::MapSplice => "MapSplice",
                Syntax::VecSplice => "VecSplice",
                Syntax::Accum => "Accum",
                Syntax::GlobVar => "GlobVar",
                Syntax::Selector => "Selector",
                Syntax::Pattern => "Pattern",
                Syntax::StructPattern => "StructPattern",
                Syntax::Formatter => "Formatter",
            }
        } else {
            "VVal"
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            VVal::Str(_) => "string",
            VVal::Byt(_) => "bytes",
            VVal::Chr(VValChr::Char(_)) => "char",
            VVal::Chr(VValChr::Byte(_)) => "byte",
            VVal::None => "none",
            VVal::Err(_) => "error",
            VVal::Bol(_) => "bool",
            VVal::Sym(_) => "symbol",
            VVal::Syn(_) => "syntax",
            VVal::Int(_) => "integer",
            VVal::Flt(_) => "float",
            VVal::Pair(_) => "pair",
            VVal::Iter(_) => "iter",
            VVal::Opt(_) => "optional",
            VVal::Lst(_) => "vector",
            VVal::Map(_) => "map",
            VVal::Usr(_) => "userdata",
            VVal::Fun(_) => "function",
            VVal::IVec(_) => "integer_vector",
            VVal::FVec(_) => "float_vector",
            VVal::DropFun(_) => "drop_function",
            VVal::Ref(_) => "ref_strong",
            VVal::HRef(_) => "ref_hidden",
            VVal::WWRef(_) => "ref_weak",
            VVal::Type(_) => "type",
        }
    }

    /// Returns true if this value is a user value of type T.
    pub fn is_usr<T: 'static>(&mut self) -> bool {
        if let VVal::Usr(bx) = self {
            bx.as_any().is::<T>()
        } else {
            false
        }
    }

    /// Accesses the user data of type T directly.
    /// If the user data is not of that type it returns `None`.
    pub fn with_usr_ref<T: 'static, F, X>(&mut self, f: F) -> Option<X>
    where
        F: FnOnce(&mut T) -> X,
    {
        if let VVal::Usr(bx) = self {
            bx.as_any().downcast_mut::<T>().map(f)
        } else {
            None
        }
    }

    /// Quick access method for retrieving the VVal at index `idx`.
    /// Returns VVal::None if the VVal is not a VVal::Lst or no such index exists.
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
    pub fn v_(&self, idx: usize) -> VVal {
        self.at(idx).unwrap_or(VVal::None)
    }

    /// Quick access method for retrieving the VVal at key `idx`.
    /// Returns VVal::None if the VVal is not a VVal::Map or no such index exists.
    /// See also the shorthands `v_ik`, `v_fk`, `v_sk`, `v_bk` and `v_s_rawk`.
    ///
    ///```
    /// use wlambda::VVal;
    /// let v = VVal::map();
    /// v.set_key_str("aaa", VVal::Int(12));
    /// v.set_key_str("abc", VVal::Int(13));
    /// v.set_key_str("zyy", VVal::Int(14));
    ///
    /// assert_eq!(v.v_k("abc").i(), 13);
    /// assert_eq!(v.v_ik("aaa"),    12);
    /// assert_eq!(v.v_ik("zyy"),    14);
    ///```
    pub fn v_k(&self, key: &str) -> VVal {
        self.get_key(key).unwrap_or(VVal::None)
    }
    /// Quick access of an integer at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(11));
    /// assert_eq!(v.v_i(0),    11);
    ///```
    pub fn v_i(&self, idx: usize) -> i64 {
        self.v_(idx).i()
    }
    /// Quick access of the integer at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::new_str("10"));
    /// assert_eq!(v.v_ik("aaa"), 10);
    ///```
    pub fn v_ik(&self, key: &str) -> i64 {
        self.v_k(key).i()
    }
    /// Quick access of an bool at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Bol(true));
    /// assert_eq!(v.v_b(0),    true);
    ///```
    pub fn v_b(&self, idx: usize) -> bool {
        self.v_(idx).b()
    }
    /// Quick access of the integer at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::Bol(true));
    /// assert_eq!(v.v_bk("aaa"), true);
    ///```
    pub fn v_bk(&self, key: &str) -> bool {
        self.v_k(key).b()
    }
    /// Quick access of a character at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(11));
    /// assert_eq!(v.v_c(0),    '\u{0b}');
    ///```
    pub fn v_c(&self, idx: usize) -> char {
        self.v_(idx).c()
    }
    /// Quick access of the character at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::new_char('@'));
    /// assert_eq!(v.v_ck("aaa"), '@');
    ///```
    pub fn v_ck(&self, key: &str) -> char {
        self.v_k(key).c()
    }
    /// Quick access of a byte at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(11));
    /// assert_eq!(v.v_byte(0), b'\x0b');
    ///```
    pub fn v_byte(&self, idx: usize) -> u8 {
        self.v_(idx).byte()
    }
    /// Quick access of the byte at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::new_byte(b'@'));
    /// assert_eq!(v.v_bytek("aaa"), b'@');
    ///```
    pub fn v_bytek(&self, key: &str) -> u8 {
        self.v_k(key).byte()
    }
    /// Quick access of a raw string at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Int(12));
    /// assert_eq!(v.v_s_raw(0), "12");
    ///```
    pub fn v_s_raw(&self, idx: usize) -> String {
        self.v_(idx).s_raw()
    }
    /// Quick access of a raw string as reference at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::new_str("12"));
    /// assert_eq!(v.v_with_s_ref(0, |s: &str| s.chars().nth(0).unwrap()), '1');
    ///```
    pub fn v_with_s_ref<T, R>(&self, idx: usize, f: T) -> R
    where
        T: FnOnce(&str) -> R,
    {
        self.v_(idx).with_s_ref(f)
    }
    /// Quick access of the string at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::new_str("XYX"));
    /// assert_eq!(v.v_s_rawk("aaa"), "XYX");
    ///```
    pub fn v_s_rawk(&self, key: &str) -> String {
        self.v_k(key).s_raw()
    }
    /// Quick access of the string as reference at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::new_str("XYX"));
    /// assert!(v.v_with_s_refk("aaa", |s: &str| s == "XYX"));
    ///```
    pub fn v_with_s_refk<T, R>(&self, key: &str, f: T) -> R
    where
        T: FnOnce(&str) -> R,
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
    pub fn v_s(&self, idx: usize) -> String {
        self.v_(idx).s()
    }
    /// Quick access of the string represenation at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::Flt(12.2));
    /// assert_eq!(v.v_sk("aaa"), "12.2");
    ///```
    pub fn v_sk(&self, key: &str) -> String {
        self.v_k(key).s()
    }
    /// Quick access of the float at the given `idx`.
    /// See also `v_`.
    ///
    ///```
    /// let v = wlambda::VVal::vec();
    /// v.push(wlambda::VVal::Flt(13.2));
    /// assert_eq!(v.v_f(0), 13.2);
    ///```
    pub fn v_f(&self, idx: usize) -> f64 {
        self.v_(idx).f()
    }
    /// Quick access of the float at the given `key`.
    /// See also `v_k`.
    ///
    ///```
    /// let v = wlambda::VVal::map();
    /// v.set_key_str("aaa", wlambda::VVal::Flt(12.2));
    /// assert_eq!(v.v_fk("aaa"), 12.2);
    ///```
    pub fn v_fk(&self, key: &str) -> f64 {
        self.v_k(key).f()
    }

    pub fn for_each<T>(&self, mut op: T)
    where
        T: FnMut(&VVal),
    {
        if let VVal::Lst(b) = &self {
            for i in b.borrow().iter() {
                op(i);
            }
        }
    }

    pub fn for_eachk<T>(&self, mut op: T)
    where
        T: FnMut(&str, &VVal),
    {
        if let VVal::Map(b) = &self {
            for (k, v) in b.borrow().iter() {
                op(k, v);
            }
        }
    }

    pub fn shallow_merge_from(&self, other: &VVal) {
        other.with_iter(|it| {
            for (v, k) in it {
                if let Some(k) = k {
                    k.with_s_ref(|sk| {
                        let _ = self.set_key_str(sk, v);
                    })
                }
            }
        });
    }

    pub fn t(&self) -> Rc<Type> {
        let typ = match self {
            VVal::Type(t) => return t.clone(),
            VVal::None => Type::None,
            VVal::Err(e) => Type::Err(e.borrow().0.t()),
            VVal::Bol(_) => Type::Bool,
            VVal::Sym(_) => Type::Sym,
            VVal::Chr(VValChr::Char(_)) => Type::Char,
            VVal::Chr(VValChr::Byte(_)) => Type::Byte,
            VVal::Str(_) => Type::Str,
            VVal::Byt(_) => Type::Bytes,
            VVal::Int(_) => Type::Int,
            VVal::Flt(_) => Type::Float,
            VVal::Syn(_) => Type::Syntax,
            VVal::Pair(p) => Type::Pair(p.0.t(), p.1.t()),
            VVal::Opt(None) => Type::Opt(Type::any()),
            VVal::Opt(Some(v)) => Type::Opt(v.t()),
            VVal::Iter(_) => Type::Any, // TODO: Iterator type encoded in VValIter???
            VVal::Lst(_) => Type::Lst(Type::any()),
            VVal::Map(_) => Type::Map(Type::any()),
            // TODO: Function type should be stored in VValFun!!!!
            VVal::Fun(fun) => match &fun.typ {
                Some(t) => return t.clone(),
                None => return Type::rc_new_var("_FUN"),
            },
            VVal::DropFun(_) => Type::Function(Rc::new(vec![]), Rc::new(Type::None)),
            VVal::FVec(nv) => match nv.dims() {
                NVecDim::Two => Type::FVec2,
                NVecDim::Three => Type::FVec3,
                NVecDim::Four => Type::FVec4,
            },
            VVal::IVec(nv) => match nv.dims() {
                NVecDim::Two => Type::IVec2,
                NVecDim::Three => Type::IVec3,
                NVecDim::Four => Type::IVec4,
            },
            VVal::Ref(v) => Type::Ref(v.borrow().t()),
            VVal::HRef(v) => Type::Ref(v.borrow().t()),
            VVal::WWRef(l) => match l.upgrade() {
                Some(v) => Type::Ref(v.borrow().t()),
                None => Type::Ref(Type::rc_new_var("_T")),
            },
            VVal::Usr(_) => Type::Userdata,
        };
        Rc::new(typ)
    }

    #[allow(clippy::cast_lossless)]
    pub fn f(&self) -> f64 {
        match self {
            VVal::Str(s) => (*s).parse::<f64>().unwrap_or(0.0),
            VVal::Sym(s) => (*s).parse::<f64>().unwrap_or(0.0),
            VVal::Byt(s) => {
                if s.len() > 0 {
                    s[0] as f64
                } else {
                    0.0
                }
            }
            VVal::Chr(c) => c.c() as u32 as f64,
            VVal::None => 0.0,
            VVal::Err(_) => 0.0,
            VVal::Bol(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            VVal::Syn(s) => (s.syn() as i64) as f64,
            VVal::Int(i) => *i as f64,
            VVal::Flt(f) => *f,
            VVal::Pair(b) => b.0.f(),
            VVal::Lst(l) => l.borrow().len() as f64,
            VVal::Map(l) => l.borrow().len() as f64,
            VVal::Usr(u) => u.f(),
            VVal::Fun(_) => 1.0,
            VVal::IVec(iv) => iv.x_raw() as f64,
            VVal::FVec(fv) => fv.x_raw(),
            VVal::Iter(i) => iter_next_value!(i.borrow_mut(), v, { v.f() }, 0.0),
            v => v.with_deref(|v| v.f(), |_| 0.0),
        }
    }

    #[allow(clippy::cast_lossless)]
    pub fn i(&self) -> i64 {
        match self {
            VVal::Str(s) => (*s).parse::<i64>().unwrap_or(0),
            VVal::Sym(s) => (*s).parse::<i64>().unwrap_or(0),
            VVal::Chr(c) => c.c() as u32 as i64,
            VVal::Byt(s) => {
                if s.len() > 0 {
                    s[0] as i64
                } else {
                    0
                }
            }
            VVal::None => 0,
            VVal::Err(_) => 0,
            VVal::Bol(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            VVal::Syn(s) => s.syn() as i64,
            VVal::Int(i) => *i,
            VVal::Flt(f) => *f as i64,
            VVal::Pair(b) => b.0.i(),
            VVal::Lst(l) => l.borrow().len() as i64,
            VVal::Map(l) => l.borrow().len() as i64,
            VVal::Usr(u) => u.i(),
            VVal::Fun(_) => 1,
            VVal::IVec(iv) => iv.x_raw(),
            VVal::FVec(fv) => fv.x_raw() as i64,
            VVal::Iter(i) => iter_next_value!(i.borrow_mut(), v, { v.i() }, 0),
            v => v.with_deref(|v| v.i(), |_| 0),
        }
    }

    #[allow(clippy::cast_lossless)]
    pub fn byte(&self) -> u8 {
        match self {
            VVal::Str(s) => {
                if (*s).len() > 0 {
                    (*s).as_bytes()[0]
                } else {
                    0
                }
            }
            VVal::Sym(s) => {
                if (*s).len() > 0 {
                    (*s).as_bytes()[0]
                } else {
                    0
                }
            }
            VVal::Chr(c) => c.byte(),
            VVal::Byt(s) => {
                if s.len() > 0 {
                    s[0]
                } else {
                    b'0'
                }
            }
            VVal::None => b'\0',
            VVal::Err(_) => b'\0',
            VVal::Bol(b) => {
                if *b {
                    b'\x01'
                } else {
                    b'\0'
                }
            }
            VVal::Syn(s) => s.syn() as i64 as u32 as u8,
            VVal::Int(i) => {
                if *i <= 255 {
                    *i as u8
                } else {
                    b'?'
                }
            }
            VVal::Flt(f) => {
                if (*f as u32) <= 255 {
                    *f as u8
                } else {
                    b'?'
                }
            }
            VVal::Pair(b) => b.0.byte(),
            VVal::Lst(l) => l.borrow().len() as u8,
            VVal::Map(l) => l.borrow().len() as u8,
            VVal::Usr(u) => u.byte(),
            VVal::Fun(_) => b'\x01',
            VVal::IVec(iv) => iv.x_raw() as u8,
            VVal::FVec(fv) => fv.x_raw() as u8,
            VVal::Iter(i) => iter_next_value!(i.borrow_mut(), v, { v.byte() }, b'\0'),
            v => v.with_deref(|v| v.byte(), |_| b'\0'),
        }
    }

    #[allow(clippy::cast_lossless)]
    pub fn c(&self) -> char {
        match self {
            VVal::Str(s) => (*s).chars().next().unwrap_or('\0'),
            VVal::Sym(s) => (*s).chars().next().unwrap_or('\0'),
            VVal::Chr(c) => c.c(),
            VVal::Byt(s) => {
                if s.len() > 0 {
                    s[0] as char
                } else {
                    '\0'
                }
            }
            VVal::None => '\0',
            VVal::Err(_) => '\0',
            VVal::Bol(b) => {
                if *b {
                    '\u{01}'
                } else {
                    '\0'
                }
            }
            VVal::Syn(s) => std::char::from_u32(s.syn() as i64 as u32).unwrap_or('\0'),
            VVal::Int(i) => std::char::from_u32(*i as u32).unwrap_or('\0'),
            VVal::Flt(f) => std::char::from_u32(*f as u32).unwrap_or('\0'),
            VVal::Pair(b) => b.0.c(),
            VVal::Lst(l) => std::char::from_u32(l.borrow().len() as u32).unwrap_or('\0'),
            VVal::Map(l) => std::char::from_u32(l.borrow().len() as u32).unwrap_or('\0'),
            VVal::Usr(u) => u.c(),
            VVal::Fun(_) => '\u{01}',
            VVal::IVec(iv) => std::char::from_u32(iv.x_raw() as u32).unwrap_or('\0'),
            VVal::FVec(fv) => std::char::from_u32(fv.x_raw() as u32).unwrap_or('\0'),
            VVal::Iter(i) => iter_next_value!(i.borrow_mut(), v, { v.c() }, '\0'),
            v => v.with_deref(|v| v.c(), |_| '\0'),
        }
    }

    #[allow(clippy::cast_lossless)]
    pub fn b(&self) -> bool {
        match self {
            VVal::Str(s) => (*s).parse::<i64>().unwrap_or(0) != 0,
            VVal::Sym(s) => (*s).parse::<i64>().unwrap_or(0) != 0,
            VVal::Chr(c) => (c.c() as u32) > 0,
            VVal::Byt(s) => (if s.len() > 0 { s[0] as i64 } else { 0 }) != 0,
            VVal::None => false,
            VVal::Err(_) => false,
            VVal::Bol(b) => *b,
            VVal::Syn(s) => (s.syn() as i64) != 0,
            VVal::Pair(b) => b.0.b() || b.1.b(),
            VVal::Int(i) => (*i) != 0,
            VVal::Flt(f) => (*f as i64) != 0,
            VVal::Lst(l) => (l.borrow().len() as i64) != 0,
            VVal::Map(l) => (l.borrow().len() as i64) != 0,
            VVal::Usr(u) => u.b(),
            VVal::Fun(_) => true,
            VVal::Opt(None) => false,
            VVal::Opt(Some(_)) => true,
            VVal::IVec(iv) => iv.x().b(),
            VVal::FVec(fv) => fv.x().b(),
            VVal::Iter(i) => iter_next_value!(i.borrow_mut(), v, { v.b() }, false),
            v => v.with_deref(|v| v.b(), |_| false),
        }
    }

    pub fn nvec<N: crate::nvec::NVecNum>(&self) -> NVec<N> {
        use NVec::*;
        match self {
            VVal::IVec(i) => N::from_ivec(**i),
            VVal::FVec(f) => N::from_fvec(**f),
            VVal::Map(map) => {
                let m = map.borrow();
                let o = N::zero().into_vval();
                NVec::from_vval_tpl((
                    m.get(&s2sym("x")).unwrap_or(&o),
                    m.get(&s2sym("y")).unwrap_or(&o),
                    m.get(&s2sym("z")),
                    m.get(&s2sym("w")),
                ))
                .unwrap_or_else(|| {
                    // The only way from_vval_tpl can fail is if the fourth
                    // parameter is Some(_) but the third is None.
                    // That means that the following will always succeed
                    // (even if the above did not):
                    NVec::from_vval_tpl((
                        m.get(&s2sym("x")).unwrap_or(&o),
                        m.get(&s2sym("y")).unwrap_or(&o),
                        Some(&o),
                        m.get(&s2sym("w")),
                    ))
                    .unwrap()
                })
            }
            VVal::Lst(lst) => {
                let list = lst.borrow();
                let mut lst = list.iter();
                let zero = N::zero().into_vval();
                let (x, y, z, w) = (lst.next(), lst.next(), lst.next(), lst.next());
                // The only way from_vval_tpl can fail is if the fourth
                // parameter is Some(_) but the third is None.
                // That means that the following will always succeed,
                // because lists can't have holes.
                NVec::from_vval_tpl((x.unwrap_or(&zero), y.unwrap_or(&zero), z, w)).unwrap()
            }
            VVal::Pair(p) => NVec::from_vval_tpl((p.0.clone(), p.1.clone(), None, None)).unwrap(),
            VVal::Iter(i) => {
                iter_next_value!(
                    i.borrow_mut(),
                    v,
                    { v.nvec::<N>() },
                    NVec::from_vval_tpl((VVal::Int(0), VVal::Int(0), None, None)).unwrap()
                )
            }
            _ => Vec2(N::from_vval(self), N::zero()),
        }
    }

    fn s_cy(&self, c: &mut CycleCheck) -> String {
        let br = if let Some((do_continue, backref_str)) = c.backref(self) {
            if !do_continue {
                return backref_str;
            }
            backref_str
        } else {
            String::from("")
        };
        let s = match self {
            VVal::Str(_) => self.with_s_ref(|s| format_vval_str(s, false)),
            VVal::Sym(s) => VVal::dump_sym(&*s),
            VVal::Byt(s) => format!("$b{}", format_vval_byt(s.as_ref())),
            VVal::None => "$n".to_string(),
            VVal::Err(e) => {
                if (*e).borrow().1.has_info() {
                    format!("$e {} [@ {}]", (*e).borrow().0.s_cy(c), (*e).borrow().1)
                } else {
                    format!("$e {}", (*e).borrow().0.s_cy(c))
                }
            }
            VVal::Bol(b) => {
                if *b {
                    "$true".to_string()
                } else {
                    "$false".to_string()
                }
            }
            VVal::Syn(s) => format!("$%:{:?}", s.syn()),
            VVal::Chr(c) => c.to_string(),
            VVal::Int(i) => i.to_string(),
            VVal::Flt(f) => f.to_string(),
            VVal::Pair(b) => format!("$p({},{})", b.0.s_cy(c), b.1.s_cy(c)),
            VVal::Iter(_) => "$iter(&)".to_string(),
            VVal::Opt(b) => {
                if let Some(b) = b {
                    format!("$o({})", b.s_cy(c))
                } else {
                    "$o()".to_string()
                }
            }
            VVal::Lst(l) => VVal::dump_vec_as_str(l, c),
            VVal::Map(l) => VVal::dump_map_as_str(l, c), // VVal::dump_map_as_str(l),
            VVal::Usr(u) => u.s(),
            VVal::Fun(f) => {
                let min = if f.min_args.is_none() {
                    "any".to_string()
                } else {
                    format!("{}", f.min_args.unwrap())
                };
                let max = if f.max_args.is_none() {
                    "any".to_string()
                } else {
                    format!("{}", f.max_args.unwrap())
                };
                let upvalues: String =
                    f.upvalues.iter().map(|v| v.s_cy(c)).collect::<Vec<String>>().join(",");
                if let Some(ref sp) = f.syn_pos {
                    format!(
                        "&F{{@{},amin={},amax={},locals={},upvalues=$[{}]}}",
                        sp, min, max, f.local_size, upvalues
                    )
                } else {
                    format!(
                        "&F{{@[0,0:?()],amin={},amax={},locals={},upvalues=$[{}]}}",
                        min, max, f.local_size, upvalues
                    )
                }
            }
            VVal::DropFun(f) => format!("std:to_drop[{}]", f.fun.s_cy(c)),
            VVal::Ref(l) => format!("$&&{}", (*l).borrow().s_cy(c)),
            VVal::HRef(l) => format!("$&{}", (*l).borrow().s_cy(c)),
            VVal::Type(t) => format!("type[{}]", (*t).s()),
            VVal::FVec(nvec) => nvec.s(),
            VVal::IVec(nvec) => nvec.s(),
            VVal::WWRef(l) => match l.upgrade() {
                Some(v) => format!("$w&{}", v.borrow().s_cy(c)),
                None => "$n".to_string(),
            },
        };
        format!("{}{}", br, s)
    }

    pub fn as_bytes(&self) -> std::vec::Vec<u8> {
        match self {
            VVal::Chr(c) => vec![c.byte()],
            VVal::Byt(b) => b.as_ref().clone(),
            v => v.with_deref(
                |v| v.as_bytes(),
                |v| v.map_or_else(Vec::new, |v| v.with_s_ref(|s: &str| s.as_bytes().to_vec())),
            ),
        }
    }

    pub fn to_duration(&self) -> Result<std::time::Duration, VVal> {
        match self {
            VVal::Int(i) => Ok(std::time::Duration::from_millis(*i as u64)),
            VVal::Pair(b) => {
                let a = &b.0;
                let b = &b.1;

                a.with_s_ref(|astr| match astr {
                    "s" => Ok(std::time::Duration::from_secs(b.i() as u64)),
                    "ms" => Ok(std::time::Duration::from_millis(b.i() as u64)),
                    "us" => Ok(std::time::Duration::from_micros(b.i() as u64)),
                    "ns" => Ok(std::time::Duration::from_nanos(b.i() as u64)),
                    _ => Err(VVal::err_msg(&format!("Bad duration: {}", self.s()))),
                })
            }
            _ => Err(VVal::err_msg(&format!("Bad duration: {}", self.s()))),
        }
    }

    /// Returns a string representation of the VVal data structure.
    /// It handles cyclic data structures fine.
    /// The purpose is to return an unambigous represenation of the data
    /// structure. That means strings are quoted and VVal::None becomes `$n`
    /// for instance.
    ///
    /// If you need strings in pure form, use the `s_raw()` method.
    ///
    /// ```
    /// use wlambda::VVal;
    ///
    /// let v = VVal::None;
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
    #[cfg(feature = "rmp-serde")]
    pub fn to_msgpack(&self) -> Result<Vec<u8>, String> {
        match rmp_serde::to_vec(self) {
            Ok(s) => Ok(s),
            Err(e) => Err(format!("to_msgpack failed: {}", e)),
        }
    }

    /// Creates a VVal structure from a msgpack byte vector.
    #[cfg(feature = "rmp-serde")]
    pub fn from_msgpack(s: &[u8]) -> Result<VVal, String> {
        match rmp_serde::from_slice(s) {
            Ok(v) => Ok(v),
            Err(e) => Err(format!("from_msgpack failed: {}", e)),
        }
    }

    /// Serializes the VVal (non cyclic) structure to a JSON string.
    #[cfg(feature = "serde_json")]
    pub fn to_json(&self, not_pretty: bool) -> Result<String, String> {
        if not_pretty {
            match serde_json::to_string(self) {
                Ok(s) => Ok(s),
                Err(e) => Err(format!("to_json failed: {}", e)),
            }
        } else {
            match serde_json::to_string_pretty(self) {
                Ok(s) => Ok(s),
                Err(e) => Err(format!("to_json failed: {}", e)),
            }
        }
    }

    /// Creates a VVal structure from a JSON string.
    #[cfg(feature = "serde_json")]
    pub fn from_json(s: &str) -> Result<VVal, String> {
        match serde_json::from_str(s) {
            Ok(v) => Ok(v),
            Err(e) => Err(format!("from_json failed: {}", e)),
        }
    }

    /// Serializes the VVal (non cyclic) structure to a JSON string.
    #[cfg(feature = "toml")]
    pub fn to_toml(&self, not_pretty: bool) -> Result<String, String> {
        match toml::value::Value::try_from(self) {
            Ok(v) => {
                if not_pretty {
                    match toml::to_string(&v) {
                        Ok(s) => Ok(s),
                        Err(e) => Err(format!("to_toml failed: {}", e)),
                    }
                } else {
                    match toml::to_string_pretty(&v) {
                        Ok(s) => Ok(s),
                        Err(e) => Err(format!("to_toml failed: {}", e)),
                    }
                }
            }
            Err(e) => Err(format!("to_toml value failed: {}", e)),
        }
    }

    /// Creates a VVal structure from a JSON string.
    #[cfg(feature = "toml")]
    pub fn from_toml(s: &str) -> Result<VVal, String> {
        match toml::from_str(s) {
            Ok(v) => Ok(v),
            Err(e) => Err(format!("from_toml failed: {}", e)),
        }
    }
}

#[cfg(feature = "serde")]
impl serde::ser::Serialize for VVal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        use serde::ser::{SerializeMap, SerializeSeq};

        match self {
            VVal::Str(_) => self.with_s_ref(|s: &str| serializer.serialize_str(s)),
            VVal::Sym(_) => self.with_s_ref(|s: &str| serializer.serialize_str(s)),
            VVal::Byt(b) => serializer.serialize_bytes(&b[..]),
            VVal::Chr(b) => match b {
                VValChr::Char(c) => {
                    let mut buf = [0; 6];
                    serializer.serialize_str(c.encode_utf8(&mut buf))
                }
                VValChr::Byte(b) => serializer.serialize_u8(*b),
            },
            VVal::None => serializer.serialize_none(),
            VVal::Iter(_) => serializer.serialize_none(),
            VVal::Err(_) => serializer.serialize_str(&self.s()),
            VVal::Bol(b) => serializer.serialize_bool(*b),
            VVal::Syn(_) => serializer.serialize_str(&self.s()),
            VVal::Int(i) => serializer.serialize_i64(*i),
            VVal::Flt(f) => serializer.serialize_f64(*f),
            VVal::Pair(b) => {
                let mut seq = serializer.serialize_seq(Some(2))?;
                seq.serialize_element(&b.0)?;
                seq.serialize_element(&b.1)?;
                seq.end()
            }
            VVal::Opt(b) => {
                if let Some(b) = b {
                    let mut seq = serializer.serialize_seq(Some(1))?;
                    seq.serialize_element(b.as_ref())?;
                    seq.end()
                } else {
                    let seq = serializer.serialize_seq(Some(0))?;
                    seq.end()
                }
            }
            VVal::Lst(l) => {
                let mut seq = serializer.serialize_seq(Some(l.borrow().len()))?;
                for v in l.borrow().iter() {
                    seq.serialize_element(v)?;
                }
                seq.end()
            }
            VVal::Map(l) => {
                let hm = l.borrow();

                let mut map = serializer.serialize_map(Some(l.borrow().len()))?;
                for (k, v) in hm.iter() {
                    map.serialize_entry(k.as_ref(), v)?;
                }
                map.end()
            }
            VVal::Usr(_) => serializer.serialize_str(&self.s()),
            VVal::Fun(_) => serializer.serialize_str(&self.s()),
            VVal::FVec(fv) => match fv.as_ref() {
                NVec::Vec2(x, y) => {
                    let mut seq = serializer.serialize_seq(Some(2))?;
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                    seq.end()
                }
                NVec::Vec3(x, y, z) => {
                    let mut seq = serializer.serialize_seq(Some(3))?;
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                    seq.serialize_element(z)?;
                    seq.end()
                }
                NVec::Vec4(x, y, z, w) => {
                    let mut seq = serializer.serialize_seq(Some(4))?;
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                    seq.serialize_element(z)?;
                    seq.serialize_element(w)?;
                    seq.end()
                }
            },
            VVal::IVec(iv) => match iv.as_ref() {
                NVec::Vec2(x, y) => {
                    let mut seq = serializer.serialize_seq(Some(2))?;
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                    seq.end()
                }
                NVec::Vec3(x, y, z) => {
                    let mut seq = serializer.serialize_seq(Some(3))?;
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                    seq.serialize_element(z)?;
                    seq.end()
                }
                NVec::Vec4(x, y, z, w) => {
                    let mut seq = serializer.serialize_seq(Some(4))?;
                    seq.serialize_element(x)?;
                    seq.serialize_element(y)?;
                    seq.serialize_element(z)?;
                    seq.serialize_element(w)?;
                    seq.end()
                }
            },
            VVal::DropFun(_) => serializer.serialize_str(&self.s()),
            VVal::Ref(_) => self.deref().serialize(serializer),
            VVal::HRef(_) => self.deref().serialize(serializer),
            VVal::WWRef(_) => self.deref().serialize(serializer),
        }
    }
}

#[cfg(feature = "serde")]
struct VValVisitor;

#[cfg(feature = "serde")]
impl<'de> serde::de::Visitor<'de> for VValVisitor {
    type Value = VVal;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a VVal")
    }

    fn visit_i128<E>(self, value: i128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(value as i64))
    }
    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(value))
    }
    fn visit_i32<E>(self, value: i32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(i64::from(value)))
    }
    fn visit_i16<E>(self, value: i16) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(i64::from(value)))
    }
    fn visit_i8<E>(self, value: i8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(i64::from(value)))
    }
    fn visit_u128<E>(self, value: u128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(value as i64))
    }
    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(value as i64))
    }
    fn visit_u32<E>(self, value: u32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(i64::from(value)))
    }
    fn visit_u16<E>(self, value: u16) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(i64::from(value)))
    }
    fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Int(i64::from(value)))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Flt(value))
    }
    fn visit_f32<E>(self, value: f32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Flt(f64::from(value)))
    }

    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::Bol(value))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::new_str(value))
    }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::new_byt(value.to_vec()))
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::None)
    }
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(VVal::None)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let v = VVal::vec();

        while let Some(ve) = seq.next_element()? {
            v.push(ve);
        }

        Ok(v)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let v = VVal::map();

        while let Some((ke, ve)) = map.next_entry()? {
            let k: VVal = ke;
            v.set_key(&k, ve).expect("Deserialized map not used more than once");
        }

        Ok(v)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::de::Deserialize<'de> for VVal {
    fn deserialize<D>(deserializer: D) -> Result<VVal, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_any(VValVisitor)
    }
}
