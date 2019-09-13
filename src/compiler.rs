// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::parser::{self};
use crate::prelude::*;
use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::Env;
use crate::vval::VValFun;
use crate::vval::EvalNode;
use crate::vval::StackAction;
use crate::vval::CompileError;
use std::rc::Rc;
use std::cell::RefCell;
use std::time::Instant;
use std::fmt::{Display, Formatter};

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
struct CompileLocal {
    is_upvalue: bool,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum ModuleLoadError {
    NoSuchModule,
    ModuleEvalError(EvalError),
    Other(String),
}

/// This trait is responsible for loading modules
/// and returning a collection of name->value mappings for a module
/// name.
///
/// See also GlobalEnv::set_resolver() about how to configure the
/// global environment to use you trait implementation.
///
/// There is a default implementation named LocalFileModuleResolver,
/// which loads the modules from files.
pub trait ModuleResolver {
    /// Resolves the path to a HashMap of names -> VVal.
    /// Where you obtain this mapping from is completely up to you.
    /// You can statically define these, load them from a JSON file,
    /// load them by executing another WLambda script or whatever you fancy.
    ///
    /// See LocalFileModuleResolver as example on how to implement this.
    fn resolve(&mut self, global: GlobalEnvRef, path: &[String]) -> Result<std::collections::HashMap<String, VVal>, ModuleLoadError>;
}

/// This structure implements the ModuleResolver trait and is
/// responsible for loading modules on `!@import` for WLambda.
#[derive(Debug, Clone, Default)]
pub struct LocalFileModuleResolver {
    loaded_modules: std::collections::HashMap<String, std::rc::Rc<std::collections::HashMap<String, VVal>>>,
}

#[allow(dead_code)]
impl LocalFileModuleResolver {
    pub fn new() -> LocalFileModuleResolver {
        LocalFileModuleResolver {
            loaded_modules: std::collections::HashMap::new(),
        }
    }
}

type SymbolTable = std::collections::HashMap<String, VVal>;

impl ModuleResolver for LocalFileModuleResolver {
    fn resolve(&mut self, _global: GlobalEnvRef, path: &[String]) -> Result<SymbolTable, ModuleLoadError> {
        let mut ctx = EvalContext::new(GlobalEnv::new_empty_default());
        let pth = path.join("/");
        match ctx.eval_file(&(pth.clone() + ".wl")) {
            Err(e) => Err(ModuleLoadError::ModuleEvalError(e)),
            Ok(_v) => Ok(ctx.get_exports()),
        }
    }
}

/// Holds global environment variables.
///
/// This data structure is part of the API. It's there
/// to make functions or values available to a WLambda program.
///
/// This environment structure is usually wrapped inside an [EvalContext](struct.EvalContext.html)
/// which augments it for calling the compiler and allows evaluation
/// of the code.
///
/// See also [GlobalEnv::add_func()](#method.add_func) of how to create a function and put it
/// into the global variable.
/// And [GlobalEnv::set_var()](#method.set_var).
/// And [GlobalEnv::get_var()](#method.get_var).
#[derive(Clone)]
pub struct GlobalEnv {
    env: std::collections::HashMap<String, VVal>,
    mem_modules: std::collections::HashMap<String, SymbolTable>,
    resolver: Option<Rc<RefCell<dyn ModuleResolver>>>,
}

impl std::fmt::Debug for GlobalEnv {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "<<GlobalEnv>>")
    }
}

/// Reference type of `GlobalEnv`.
pub type GlobalEnvRef = Rc<RefCell<GlobalEnv>>;

impl GlobalEnv {
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=7011ec14ebade14fe62e438a0db52c98
    // 'static Lifetime != program length: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=74450a3fbb85f3baa6e667b85621152a
    //  simplified: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=45f376a6ef06d81ebbc11f08ec55d918
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=94811153fadd511effa306e5369e5b19
    // FnMut: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=ef42feadce57ac61b63ec7c6dc274b2b
    /// Adds a function to a `GlobalEnv`.
    ///
    /// This is an example of how to add a function:
    ///
    /// ```
    /// use wlambda::compiler::GlobalEnv;
    /// use wlambda::vval::{Env, VVal};
    ///
    /// let g = GlobalEnv::new();
    /// g.borrow_mut().add_func(
    ///     "push",
    ///     |env: &mut Env, argc: usize| {
    ///         if argc < 2 { return Ok(VVal::Nul); }
    ///         let v = env.arg(0);
    ///         v.push(env.arg(1).clone());
    ///         Ok(v.clone())
    ///     }, Some(2), Some(2));
    ///
    /// 
    /// ```
    pub fn add_func<T>(&mut self, fnname: &str, fun: T, min_args: Option<usize>, max_args: Option<usize>)
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal,StackAction> {
        self.env.insert(
            String::from(fnname),
            VValFun::new_fun(fun, min_args, max_args));
    }

    /// Sets a global variable to a value.
    ///
    /// See also [EvalContext::set_global_var()](struct.EvalContext.html#method.set_global_var)
    #[allow(dead_code)]
    pub fn set_var(&mut self, var: &str, val: &VVal) {
        self.env.insert(String::from(var), val.to_ref());
    }

    /// Returns the value of a global variable.
    ///
    /// See also [EvalContext::get_global_var()](struct.EvalContext.html#method.get_global_var)
    #[allow(dead_code)]
    pub fn get_var(&mut self, var: &str) -> Option<VVal> {
        match self.env.get(var) {
            Some(v) => Some(v.deref()),
            None    => None,
        }
    }

    /// Sets a symbol table for a module before a module asks for it.
    /// Modules set via this function have precedence over resolved modules
    /// via set_resolver().
    #[allow(dead_code)]
    pub fn set_module(&mut self, mod_name: &str, symtbl: SymbolTable) {
        self.mem_modules.insert(mod_name.to_string(), symtbl);
    }

    /// Sets the module resolver. There is a LocalFileModuleResolver available
    /// which loads the modules relative to the current working directory.
    ///
    /// Please note that modules made available using `set_module` have priority
    /// over modules that are provided by the resolver.
    ///
    ///```
    /// use std::rc::Rc;
    /// use std::cell::RefCell;
    ///
    /// let global = wlambda::compiler::GlobalEnv::new_default();
    ///
    /// let lfmr = Rc::new(RefCell::new(
    ///     wlambda::compiler::LocalFileModuleResolver::new()));
    ///
    /// global.borrow_mut().set_resolver(lfmr);
    ///```
    pub fn set_resolver(&mut self, res: Rc<RefCell<dyn ModuleResolver>>) {
        self.resolver = Some(res.clone());
    }

    /// Creates a new completely empty GlobalEnv.
    ///
    /// There is no core language, no std lib. You have
    /// to add all that on your own via `set_var` and `set_module`.
    pub fn new() -> GlobalEnvRef {
        Rc::new(RefCell::new(GlobalEnv {
            env: std::collections::HashMap::new(),
            mem_modules: std::collections::HashMap::new(),
            resolver: None,
        }))
    }

    /// Returns a default global environment. Where default means what
    /// the author of WLambda decided what is default at the moment.
    /// For more precise global environment, that is not a completely
    /// moving target consider the alternate constructor variants.
    ///
    /// Global environments constructed with this typically contain:
    ///
    /// - `set_module("wlambda", wlambda::prelude::core_symbol_table())`
    /// - `set_module("std",     wlambda::prelude::std_symbol_table())`
    /// - `set_resolver(Rc::new(RefCell::new(wlambda::compiler::LocalFileModuleResolver::new()))`
    /// - `import_module("wlambda", "")`
    /// - `import_module("std", "std")`
    ///
    /// On top of that, the `WLambda` module has been imported without a prefix
    /// and the `std` module has been loaded with an `std:` prefix.
    /// This means you can load and eval scripts you see all over this documentation.
    pub fn new_default() -> GlobalEnvRef {
        create_wlamba_prelude()
    }

    /// This is like `new_default` but does not import anything, neither the
    /// core language nor the std module.
    pub fn new_empty_default() -> GlobalEnvRef {
        create_wlamba_prelude()
    }
}

/// Position of a variable represented in the `CompileEnv`.
#[derive(Debug, Clone)]
enum VarPos {
    /// No position of the variable. Mostly placeholder value for non existing variable.
    NoPos,
    /// Variable is stored in upvalue at the specified position.
    UpValue(usize),
    /// Variable is stored in local variables on the stack at the specified position.
    Local(usize),
    /// Variable is stored in the global variables with the given value.
    Global(VVal),
}

#[derive(Debug, Clone)]
pub enum EvalError {
    IOError(String),
    ParseError(parser::ParseError),
    CompileError(CompileError),
    ExecError(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            EvalError::IOError(e)      => { write!(f, "IO error: {}", e) },
            EvalError::ParseError(e)   => { write!(f, "Parse error: {}", e) },
            EvalError::CompileError(e) => { write!(f, "Compile error: {}", e) },
            EvalError::ExecError(s)    => { write!(f, "Execution error: {}", s) },
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvalContext {
    pub global:        GlobalEnvRef,
    local_compile:     Rc<RefCell<CompileEnv>>,
    pub local:         Rc<RefCell<Env>>,
}

impl EvalContext {
    pub fn new(global: GlobalEnvRef) -> EvalContext {
        (Self::new_with_user_impl(global, Rc::new(RefCell::new(VVal::vec()))))
        .register_self_eval()
    }

    #[allow(dead_code)]
    pub fn new_empty_global_env() -> EvalContext {
        Self::new_with_user_impl(
            GlobalEnv::new(),
            Rc::new(RefCell::new(VVal::vec())))
    }

    /// A shortcut: This creates a new EvalContext with a GlobalEnv::new_default()
    /// global environment.
    ///
    /// This is a shorthand for:
    ///```
    /// wlambda::compiler::EvalContext::new(
    ///     wlambda::compiler::GlobalEnv::new_default());
    ///```
    #[allow(dead_code)]
    pub fn new_default() -> EvalContext {
        Self::new(GlobalEnv::new_default())
    }

    fn register_self_eval(self) -> Self {
        let ctx_clone =
            Self::new_with_user_impl(
                self.global.clone(),
                self.local.borrow().get_user());

        self.global.borrow_mut().add_func("wl:eval", move |env: &mut Env, _argc: usize| {
            let code    = env.arg(0).s_raw();
            let ctx     = ctx_clone.clone();
            let mut ctx = ctx.register_self_eval();
            match ctx.eval(&code) {
                Ok(v)  => Ok(v),
                Err(e) => Ok(VVal::err_msg(&format!("{}", e))),
            }
        }, Some(1), Some(2));

        self
    }

    pub fn get_exports(&self) -> std::collections::HashMap<String, VVal> {
        self.local.borrow_mut().exports.clone()
    }

    #[allow(dead_code)]
    fn new_with_user_impl(global: GlobalEnvRef, user: Rc<RefCell<std::any::Any>>) -> EvalContext {
        EvalContext {
            global: global.clone(),
            local_compile: Rc::new(RefCell::new(CompileEnv {
                parent:    None,
                global:    global.clone(),
                local_map: std::collections::HashMap::new(),
                locals:    Vec::new(),
                upvals:    Vec::new(),
                implicit_arity: (ArityParam::Undefined, ArityParam::Undefined),
                explicit_arity: (ArityParam::Undefined, ArityParam::Undefined),
            })),
            local: Rc::new(RefCell::new(Env::new_with_user(user))),
        }
    }

    #[allow(dead_code)]
    pub fn new_with_user(global: GlobalEnvRef, user: Rc<RefCell<std::any::Any>>) -> EvalContext {
        (Self::new_with_user_impl(global, user)).register_self_eval()
    }

    /// Evaluates an AST of WLambda code and executes it with the given `EvalContext`.
    ///
    /// ```
    /// use wlambda::parser;
    /// let mut ctx = wlambda::EvalContext::new_default();
    ///
    /// let s = "$[1,2,3]";
    /// let ast = parser::parse(s, 0).unwrap();
    /// let r = &mut ctx.eval_ast(&ast).unwrap();
    ///
    /// println!("Res: {}", r.s());
    /// ```
    pub fn eval_ast(&mut self, ast: &VVal) -> Result<VVal, EvalError>  {
        let prog = compile(ast, &mut self.local_compile);
        let local_env_size = CompileEnv::local_env_size(&self.local_compile);

        let env = self.local.borrow_mut();
        let mut res = Ok(VVal::Nul);

        std::cell::RefMut::map(env, |l_env| {
            res = match prog {
                Ok(prog_closures) => {
                    l_env.sp = 0;
                    l_env.set_bp(local_env_size);
                    match prog_closures(l_env) {
                        Ok(v)   => Ok(v.clone()),
                        Err(je) =>
                            Err(EvalError::ExecError(
                                format!("Jumped out of execution: {:?}", je))),
                    }
                },
                Err(e) => { Err(EvalError::CompileError(e)) },
            };
            l_env
        });

        res
    }

    /// Evaluates a piece of WLambda code with the given `EvalContext`.
    ///
    /// ```
    /// let mut ctx = wlambda::EvalContext::new_default();
    ///
    /// let r = &mut ctx.eval("$[1,2,3]").unwrap();
    /// println!("Res: {}", r.s());
    /// ```
    #[allow(dead_code)]
    pub fn eval(&mut self, s: &str) -> Result<VVal, EvalError>  {
        match parser::parse(s, 0) {
            Ok(ast) => { self.eval_ast(&ast) },
            Err(e) => { Err(EvalError::ParseError(e)) },
        }
    }

    /// Evaluates a WLambda code in a file with the given `EvalContext`.
    ///
    /// ```
    /// let mut ctx = wlambda::EvalContext::new_default();
    ///
    /// let r = &mut ctx.eval_file("examples/read_test.wl").unwrap();
    /// assert_eq!(r.i(), 403, "matches contents!");
    /// ```
    #[allow(dead_code)]
    pub fn eval_file(&mut self, filename: &str) -> Result<VVal, EvalError> {
        let contents = std::fs::read_to_string(filename);
        if contents.is_err() {
            return Err(EvalError::IOError(format!("file '{}': {}", filename, contents.unwrap_err())));
        }
        let contents = contents.unwrap();
        match parser::parse(&contents, 0) {
            Ok(ast) => { self.eval_ast(&ast) },
            Err(e) => { Err(EvalError::ParseError(e)) },
        }
    }

    /// Calls a wlambda function with the given `EvalContext`.
    ///
    /// ```
    /// use wlambda::{VVal, EvalContext};
    /// let mut ctx = EvalContext::new_default();
    ///
    /// let returned_func = &mut ctx.eval("{ _ + _1 }").unwrap();
    /// assert_eq!(
    ///     ctx.call(returned_func,
    ///              &vec![VVal::Int(10), VVal::Int(11)]).unwrap().i(),
    ///     21);
    /// ```
    #[allow(dead_code)]
    pub fn call(&mut self, f: &VVal, args: &[VVal]) -> Result<VVal, StackAction>  {
        let mut env = self.local.borrow_mut();
        f.call(&mut env, args)
    }

    /// Sets a global variable for the scripts to access.
    ///
    /// ```
    /// use wlambda::{VVal, EvalContext};
    /// let mut ctx = EvalContext::new_default();
    ///
    /// ctx.set_global_var("XXX", &VVal::Int(200));
    ///
    /// assert_eq!(ctx.eval("XXX * 2").unwrap().i(), 400);
    /// ```
    #[allow(dead_code)]
    pub fn set_global_var(&mut self, var: &str, val: &VVal) {
        self.global.borrow_mut().set_var(var, val);
    }

    /// Gets the value of a global variable from the script:
    ///
    /// ```
    /// use wlambda::{VVal, EvalContext};
    /// let mut ctx = EvalContext::new_default();
    ///
    /// assert_eq!(ctx.eval("!:global XXX = 22 * 2; XXX").unwrap().i(), 44);
    /// ```
    #[allow(dead_code)]
    pub fn get_global_var(&mut self, var: &str) -> Option<VVal> {
        self.global.borrow_mut().get_var(var)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArityParam {
    Undefined,
    Limit(usize),
    Infinite,
}

/// Compile time environment for allocating and
/// storing variables inside a function scope.
///
/// Also handles upvalues. Upvalues in WLambda are copied to every
/// scope that they are passed through until they are needed.
#[allow(dead_code)]
#[derive(Debug, Clone)]
struct CompileEnv {
    /// Reference to the global environment
    global:    GlobalEnvRef,
    /// Reference to the environment of the _parent_ function.
    parent:    Option<Rc<RefCell<CompileEnv>>>,
    /// Mapping of strings to where they can be found.
    local_map: std::collections::HashMap<String, VarPos>,
    /// List of local variables of this function.
    locals:    std::vec::Vec<CompileLocal>,
    /// Stores position of the upvalues for copying the upvalues at runtime.
    upvals:    std::vec::Vec<VarPos>,
    /// Stores the implicitly calculated arity of this function.
    implicit_arity: (ArityParam, ArityParam),
    /// Stores the explicitly defined arity of this function.
    explicit_arity: (ArityParam, ArityParam),
}

/// Reference type to a `CompileEnv`.
type CompileEnvRef = Rc<RefCell<CompileEnv>>;

impl CompileEnv {
    fn create_env(parent: Option<CompileEnvRef>) -> Rc<RefCell<CompileEnv>> {
        let global = if let Some(p) = &parent {
            p.borrow_mut().global.clone()
        } else {
            GlobalEnv::new()
        };
        Rc::new(RefCell::new(CompileEnv {
            parent,
            global,
            local_map: std::collections::HashMap::new(),
            locals:    Vec::new(),
            upvals:    Vec::new(),
            implicit_arity: (ArityParam::Undefined, ArityParam::Undefined),
            explicit_arity: (ArityParam::Undefined, ArityParam::Undefined),
        }))
    }

    fn def_up(&mut self, s: &str, pos: VarPos) -> usize {
        let next_index = self.upvals.len();
        self.upvals.push(pos);
        self.local_map.insert(String::from(s), VarPos::UpValue(next_index));
        next_index
    }

    fn def(&mut self, s: &str, is_global: bool) -> VarPos {
        //d// println!("DEF: {} global={}", s, is_global);
        let pos = self.local_map.get(s);
        match pos {
            None => {
                if is_global {
                    let v = VVal::Nul;
                    let r = v.to_ref();
                    //d// println!("GLOBAL: {} => {}", s, r.s());
                    self.global.borrow_mut().env.insert(String::from(s), r.clone());
                    return VarPos::Global(r);
                }
            },
            Some(p) => {
                match p {
                    VarPos::NoPos => {
                        if is_global {
                            let v = VVal::Nul;
                            let r = v.to_ref();
                            //d// println!("GLOBAL: {} => {}", s, r.s());
                            self.global.borrow_mut().env.insert(String::from(s), r.clone());
                            return VarPos::Global(r);
                        }
                    },
                    VarPos::UpValue(_)  => {},
                    VarPos::Global(_)   => {},
                    VarPos::Local(_i)   => return p.clone(),
                }
            },
        }

        let next_index = self.locals.len();
        self.locals.push(CompileLocal {
            is_upvalue: false,
        });
        self.local_map.insert(String::from(s), VarPos::Local(next_index));
        VarPos::Local(next_index)
    }

    fn copy_upvals(&self, e: &mut Env, upvalues: &mut std::vec::Vec<VVal>) {
        //d// println!("COPY UPVALS: {:?}", self.upvals);
        for p in self.upvals.iter() {
            match p {
                VarPos::UpValue(i) => upvalues.push(e.get_up_raw(*i)),
                VarPos::Local(i) => {
                    let u = e.get_local(*i);
                    upvalues.push(
                        match u {
                            VVal::CRef(_) => u.downgrade(),
                            _             => u.to_ref(),
                        });
                },
                VarPos::Global(v) => {
                    // Will probably be never used, as upvalues are
                    // always defined in relation to local variables.
                    upvalues.push(v.clone().to_ref());
                },
                VarPos::NoPos => upvalues.push(VVal::Nul.to_ref()),
            }
        }
    }

    fn local_env_size(ce: &CompileEnvRef) -> usize {
        ce.borrow().locals.len()
    }

    fn mark_upvalue(&mut self, idx: usize) {
        self.locals[idx].is_upvalue = true;
    }

    fn get(&mut self, s: &str) -> VarPos {
        let pos = self.local_map.get(s);
        match pos {
            None => {
                let opt_p = self.parent.as_mut();
                if opt_p.is_none() {
                    if let Some(v) = self.global.borrow().env.get(s){
                        return VarPos::Global(v.clone());
                    } else {
                        return VarPos::NoPos;
                    }
                }
                let parent = opt_p.unwrap().clone();
                let mut par_mut = parent.borrow_mut();

                let pos = par_mut.local_map.get(s).cloned();
                let par_var_pos = if let Some(pp) = pos {
                    pp
                } else {
                    par_mut.get(s)
                };
                match par_var_pos {
                    VarPos::Local(i) => {
                        par_mut.mark_upvalue(i);
                        VarPos::UpValue(self.def_up(s, par_var_pos))
                    },
                    VarPos::UpValue(_) => VarPos::UpValue(self.def_up(s, par_var_pos)),
                    VarPos::Global(g)  => VarPos::Global(g.clone()),
                    VarPos::NoPos      => VarPos::NoPos
                }
            }
            Some(p) => { p.clone() },
        }
    }
}

fn compile_block(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, CompileError> {
    let exprs : Vec<EvalNode> =
        ast.map_skip(|e| compile(e, ce), 1)?;

    #[allow(unused_assignments)]
    Ok(Box::new(move |e: &mut Env| {
        let mut res = VVal::Nul;
        for x in exprs.iter() {
            if let VVal::Err(ev) = res {
                let err_msg =
                    format!("Error value '{}'(@{},{}:{}) dropped.",
                            ev.borrow().0.s(),
                            ev.borrow().1.line,
                            ev.borrow().1.col,
                            ev.borrow().1.file);
                return
                    Err(StackAction::Panic(
                        VVal::new_str(&err_msg), None));
            }

            res = VVal::Nul;

            match x(e) {
                Ok(v)  => { res = v; },
                Err(e) => { return Err(e); },
            }
        }
        Ok(res)
    }))
}

fn set_impl_arity(i: usize, ce: &mut Rc<RefCell<CompileEnv>>) {
    let min = ce.borrow().implicit_arity.0.clone();
    match min {
        ArityParam::Undefined => { ce.borrow_mut().implicit_arity.0 = ArityParam::Limit(i); },
        ArityParam::Limit(j) => { if j < i { ce.borrow_mut().implicit_arity.0 = ArityParam::Limit(i); }; },
        _ => (),
    }

    let max = ce.borrow_mut().implicit_arity.1.clone();
    match max {
        ArityParam::Undefined => {
            ce.borrow_mut().implicit_arity.1 = ArityParam::Limit(i);
        },
        ArityParam::Limit(j) => {
            if j < i { ce.borrow_mut().implicit_arity.1 = ArityParam::Limit(i); }
        },
        _ => (),
    }
}

fn compile_var(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, CompileError> {
    let var = ast.at(1).unwrap();

    let s = var.s_raw();
    match &s[..] {
        "_"  => { set_impl_arity(1,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(0).clone()) })) },
        "_1" => { set_impl_arity(2,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(1).clone()) })) },
        "_2" => { set_impl_arity(3,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(2).clone()) })) },
        "_3" => { set_impl_arity(4,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(3).clone()) })) },
        "_4" => { set_impl_arity(5,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(4).clone()) })) },
        "_5" => { set_impl_arity(6,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(5).clone()) })) },
        "_6" => { set_impl_arity(7,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(6).clone()) })) },
        "_7" => { set_impl_arity(8,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(7).clone()) })) },
        "_8" => { set_impl_arity(9,  ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(8).clone()) })) },
        "_9" => { set_impl_arity(10, ce); Ok(Box::new(move |e: &mut Env| { Ok(e.arg(9).clone()) })) },
        "@"  => {
            ce.borrow_mut().implicit_arity.1 = ArityParam::Infinite;
            Ok(Box::new(move |e: &mut Env| { Ok(e.argv()) }))
        },
        _ => {
            let pos = ce.borrow_mut().get(&s);
            match pos {
                VarPos::UpValue(i) =>
                    Ok(Box::new(move |e: &mut Env| { Ok(e.get_up(i)) })),
                VarPos::Local(i) =>
                    Ok(Box::new(move |e: &mut Env| { Ok(e.get_local(i)) })),
                VarPos::Global(v) =>
                    Ok(Box::new(move |_e: &mut Env| { Ok(v.deref()) })),
                VarPos::NoPos => {
                    ast.to_compile_err(
                        format!("Variable '{}' undefined", var.s_raw()))
                }
            }
        }
    }
}

fn check_for_at_arity(prev_arity: (ArityParam, ArityParam), ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, vars: &VVal) {
    // If we have an destructuring assignment directly from "@", then we conclude
    // the implicit max arity to be minimum of number of vars:
    if ast.at(2).unwrap_or(VVal::Nul).at(0).unwrap_or(VVal::Nul).get_syn() == Syntax::Var {
        if let VVal::Lst(l) = vars {
            let llen = l.borrow().len();

            let var = ast.at(2).unwrap().at(1).unwrap();
            if var.s_raw() == "@" {
                ce.borrow_mut().implicit_arity = prev_arity;
                set_impl_arity(llen, ce);
            }
        }
    }
}

fn compile_def(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_global: bool) -> Result<EvalNode, CompileError> {
    let prev_max_arity = ce.borrow().implicit_arity.clone();

    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let destr   = ast.at(3).unwrap_or(VVal::Nul);

    //d// println!("COMP DEF: {:?} global={}, destr={}", vars, is_global, destr.b());

    let cv = compile(&value, ce)?;
    if destr.b() {
        check_for_at_arity(prev_max_arity, ast, ce, &vars);

        let poses =
            vars.map_ok_skip(
                |v| ce.borrow_mut().def(&v.s_raw(), is_global),
                0);

        Ok(Box::new(move |e: &mut Env| {
            let v = cv(e)?;
            match v {
                VVal::Lst(l) => {
                    for (i, vi) in poses.iter().enumerate() {
                        if l.borrow().len() <= i {
                            if let VarPos::Local(vip) = vi {
                                e.set_consume(*vip, VVal::Nul);
                            }
                        } else {
                            let val = &mut l.borrow_mut()[i];

                            let set_val = val.clone();
                            match vi {
                                VarPos::Local(vip) => {
                                    e.set_local(*vip, &set_val);
                                },
                                VarPos::Global(r) => {
                                    if let VVal::Ref(gr) = r {
                                        gr.replace(set_val);
                                    }
                                },
                                _ => {}
                            }
                        }
                    }
                },
                VVal::Map(m) => {
                    for (i, vi) in poses.iter().enumerate() {
                        let vname = vars.at(i).unwrap().s_raw();
                        let val = m.borrow().get(&vname).cloned().unwrap_or(VVal::Nul);

                        match vi {
                            VarPos::Local(vip) => e.set_consume(*vip, val),
                            VarPos::Global(r) => {
                                if let VVal::Ref(gr) = r {
                                    gr.replace(val);
                                }
                            },
                            _ => {}
                        }
                    }
                },
                _ => {
                    for vi in poses.iter() {
                        match vi {
                            VarPos::Local(vip) => e.set_local(*vip, &v),
                            VarPos::Global(r) => {
                                if let VVal::Ref(gr) = r {
                                    gr.replace(v.clone());
                                }
                            },
                            _ => {},
                        }
                    }
                }
            }

            Ok(VVal::Nul)
        }))
    } else {
        let pos = ce.borrow_mut().def(&vars.at(0).unwrap().s_raw(), is_global);

        match pos {
            VarPos::Local(vip) => {
                Ok(Box::new(move |e: &mut Env| {
                    let v = cv(e)?;
                    e.set_consume(vip, v);
                    Ok(VVal::Nul)
                }))
            },
            VarPos::Global(r) => {
                let gref = r.clone();
                Ok(Box::new(move |e: &mut Env| {
                    let v = cv(e)?;
                    gref.set_ref(v.clone());
                    Ok(VVal::Nul)
                }))
            },
            _ => ast.to_compile_err(
                    "Can't define badly positioned variable!".to_string()),
        }
    }
}

fn set_ref_at_varpos(e: &mut Env, pos: &VarPos, v: &VVal) -> Option<String> {
    match pos {
        VarPos::UpValue(d) => { e.get_up(*d).set_ref(v.clone()); None },
        VarPos::Local(d)   => { e.get_local(*d).set_ref(v.clone()); None },
        VarPos::Global(d)  => {
            if let VVal::Ref(r) = d {
                r.borrow().set_ref(v.clone());
                None
            } else {
                Some("Can't assign to global read only variable!".to_string())
            }
        },
        VarPos::NoPos => {
            Some(format!("Unknown pos to assign value '{}' to!", v.s()))
        }
    }
}

fn set_env_at_varpos(e: &mut Env, pos: &VarPos, v: &VVal) -> Option<String> {
    match pos {
        VarPos::UpValue(d) => { e.set_up(*d, v); None },
        VarPos::Local(d)   => { e.set_local(*d, v); None },
        VarPos::Global(d)  => {
            if let VVal::Ref(r) = d {
                r.replace(v.clone());
                None
            } else {
                Some("Can't assign to global read only variable!".to_string())
            }
        },
        VarPos::NoPos => {
            Some(format!("Unknown pos to assign value '{}' to!", v.s()))
        }
    }
}

fn compile_assign(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_ref: bool) -> Result<EvalNode, CompileError> {
    let prev_max_arity = ce.borrow().implicit_arity.clone();

    let syn  = ast.at(0).unwrap_or(VVal::Nul);
    let spos = syn.get_syn_pos();

    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let destr   = ast.at(3).unwrap_or(VVal::Nul);
    let cv      = compile(&value, ce)?;

    if destr.b() {
        check_for_at_arity(prev_max_arity, ast, ce, &vars);

        let poses = vars.map_ok_skip(|v| ce.borrow_mut().get(&v.s_raw()), 0);

        for (i, pos) in poses.iter().enumerate() {
            if let VarPos::NoPos = pos {
                return 
                    ast.to_compile_err(
                        format!("Can't assign to undefined local variable '{}'",
                                vars.at(i).unwrap_or(VVal::Nul).s_raw()));
            }
        }

        if is_ref {
            Ok(Box::new(move |e: &mut Env| {
                let v = cv(e)?;
                match v {
                    VVal::Lst(l) => {
                        for (i, pos) in poses.iter().enumerate() {
                            let val = &mut l.borrow_mut()[i];
                            match set_ref_at_varpos(e, pos, val) {
                                None => (),
                                Some(err) => {
                                    return
                                        Err(StackAction::Panic(
                                            VVal::new_str_mv(err),
                                            Some(vec![spos.clone()])));
                                }
                            }
                        }
                    },
                    VVal::Map(m) => {
                        for (i, pos) in poses.iter().enumerate() {
                            let vname = vars.at(i).unwrap().s_raw();
                            let val = m.borrow().get(&vname).cloned().unwrap_or(VVal::Nul);
                            set_ref_at_varpos(e, pos, &val);
                        }
                    },
                    _ => {
                        for pos in poses.iter() {
                            set_ref_at_varpos(e, pos, &v);
                        }
                    }
                }

                Ok(VVal::Nul)
            }))
        } else {
            Ok(Box::new(move |e: &mut Env| {
                let v = cv(e)?;
                match v {
                    VVal::Lst(l) => {
                        for (i, pos) in poses.iter().enumerate() {
                            let val = &mut l.borrow_mut()[i];
                            match set_env_at_varpos(e, pos, val) {
                                None => (),
                                Some(err) => {
                                    return
                                        Err(StackAction::Panic(
                                            VVal::new_str_mv(err),
                                            Some(vec![spos.clone()])));
                                }
                            }
                        }
                    },
                    VVal::Map(m) => {
                        for (i, pos) in poses.iter().enumerate() {
                            let vname = vars.at(i).unwrap().s_raw();
                            let val = m.borrow().get(&vname).cloned().unwrap_or(VVal::Nul);
                            set_env_at_varpos(e, pos, &val);
                        }
                    },
                    _ => {
                        for pos in poses.iter() {
                            set_env_at_varpos(e, pos, &v);
                        }
                    }
                }

                Ok(VVal::Nul)
            }))
        }

    } else {
        let s   = &vars.at(0).unwrap().s_raw();
        let pos = ce.borrow_mut().get(s);

        if is_ref {
            match pos {
                VarPos::UpValue(i) => {
                    Ok(Box::new(move |e: &mut Env| {
                        let v = cv(e)?;
                        e.get_up(i).set_ref(v);
                        Ok(VVal::Nul)
                    }))
                },
                VarPos::Local(i) => {
                    Ok(Box::new(move |e: &mut Env| {
                        let v = cv(e)?;
                        e.get_local(i).set_ref(v);
                        Ok(VVal::Nul)
                    }))
                },
                VarPos::Global(glob_v) => {
                    if let VVal::Ref(glob_r) = glob_v {
                        Ok(Box::new(move |e: &mut Env| {
                            let v = cv(e)?;
                            glob_r.borrow().set_ref(v);
                            Ok(VVal::Nul)
                        }))
                    } else {
                        ast.to_compile_err(
                            format!("Can't assign to read only global variable '{}'",
                                    s))
                    }
                },
                VarPos::NoPos =>
                    ast.to_compile_err(
                        format!("Can't assign to undefined local variable '{}'", s)),
            }
        } else {
            match pos {
                VarPos::UpValue(i) => {
                    Ok(Box::new(move |e: &mut Env| {
                        let v = cv(e)?;
                        e.set_up(i, &v);
                        Ok(VVal::Nul)
                    }))
                },
                VarPos::Local(i) => {
                    Ok(Box::new(move |e: &mut Env| {
                        let v = cv(e)?;
                        e.set_local(i, &v);
                        Ok(VVal::Nul)
                    }))
                },
                VarPos::Global(glob_v) => {
                    if let VVal::Ref(glob_r) = glob_v {
                        Ok(Box::new(move |e: &mut Env| {
                            let v = cv(e)?;
                            glob_r.replace(v.clone());
                            Ok(VVal::Nul)
                        }))
                    } else {
                        ast.to_compile_err(
                            format!("Can't assign to read only global variable '{}'",
                                    s))
                    }
                },
                VarPos::NoPos =>
                    ast.to_compile_err(
                        format!("Can't assign to undefined local variable '{}'", s)),
            }
        }
    }
}

fn compile(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, CompileError> {
    match ast {
        VVal::Lst(_l) => {
            let syn  = ast.at(0).unwrap_or(VVal::Nul);
            let spos = syn.get_syn_pos();
            let syn  = syn.get_syn();

            match syn {
                Syntax::Block       => { compile_block(ast, ce)       },
                Syntax::Var         => { compile_var(ast, ce)         },
                Syntax::Def         => { compile_def(ast, ce, false)  },
                Syntax::DefGlobRef  => { compile_def(ast, ce, true)   },
                Syntax::Assign      => { compile_assign(ast, ce, false) },
                Syntax::AssignRef   => { compile_assign(ast, ce, true) },
                Syntax::Import => {
                    let prefix = ast.at(1).unwrap();
                    let name   = ast.at(2).unwrap();
                    let s_prefix = if prefix.is_none() { String::from("") }
                                   else { prefix.s_raw() + ":" };

                    let glob_ref = ce.borrow_mut().global.clone();
                    if let Some(stbl) = glob_ref.borrow().mem_modules.get(&name.s_raw()) {
                        for (k, v) in stbl {
                            glob_ref.borrow_mut().env.insert(
                                s_prefix.clone() + &k, v.clone());
                        }
                    }

                    let resolver : Option<Rc<RefCell<dyn ModuleResolver>>> =
                        glob_ref.borrow_mut().resolver.clone();

                    let path : Vec<String> =
                        (&name.s_raw())
                            .split(':')
                            .map(String::from)
                            .collect();

                    if resolver.is_some() {
                        let exports = resolver.unwrap().borrow_mut().resolve(glob_ref.clone(), &path);
                        match exports {
                            Err(ModuleLoadError::NoSuchModule) => {
                                ast.to_compile_err(
                                    format!("Couldn't find module '{}'", name.s_raw()))
                            },
                            Err(ModuleLoadError::ModuleEvalError(e)) => {
                                ast.to_compile_err(
                                    format!("Error on evaluating module '{}': {}", name.s_raw(), e))
                            },
                            Err(ModuleLoadError::Other(s)) => {
                                ast.to_compile_err(
                                    format!("Error on resolving module '{}': {}", name.s_raw(), s))
                            },
                            Ok(map) => {
                                for (k, v) in map {
                                    glob_ref.borrow_mut().env.insert(
                                        s_prefix.clone() + &k, v.clone());
                                }

                                Ok(Box::new(move |_e: &mut Env| { Ok(VVal::Nul) }))
                            },
                        }
                    } else {
                        ast.to_compile_err(
                            format!("Couldn't resolve module '{}'", name.s_raw()))
                    }
                },
                Syntax::Export => {
                    let name = ast.at(1).unwrap();
                    let val = compile(&ast.at(2).unwrap(), ce)?;

                    Ok(Box::new(move |e: &mut Env| {
                        let value = val(e)?;
                        e.export_name(&name.s_raw(), &value);
                        Ok(value)
                    }))
                },
                Syntax::Err => {
                    let err_val = compile(&ast.at(1).unwrap(), ce)?;
                    Ok(Box::new(move |e: &mut Env|
                                    Ok(VVal::err(err_val(e)?.clone(), spos.clone()))))
                },
                Syntax::Key => {
                    let sym = ast.at(1).unwrap();
                    Ok(Box::new(move |_: &mut Env| Ok(sym.clone())))
                },
                Syntax::Str => {
                    let str = ast.at(1).unwrap();
                    Ok(Box::new(move |_: &mut Env| Ok(str.clone())))
                },
                Syntax::SetKey => {
                    let map = compile(&ast.at(1).unwrap(), ce)?;
                    let sym = compile(&ast.at(2).unwrap(), ce)?;
                    let val = compile(&ast.at(3).unwrap(), ce)?;
                    Ok(Box::new(move |e: &mut Env| {
                        let m = map(e)?;
                        let s = sym(e)?;
                        let v = val(e)?;
                        m.set_key(&s, v.clone());
                        Ok(v)
                    }))
                },
                Syntax::Ref => {
                    let val = compile(&ast.at(1).unwrap(), ce)?;
                    Ok(Box::new(move |e: &mut Env| {
                        Ok(val(e)?.to_ref())
                    }))
                },
                Syntax::WRef => {
                    let val = compile(&ast.at(1).unwrap(), ce)?;
                    Ok(Box::new(move |e: &mut Env| {
                        Ok(val(e)?.to_wref())
                    }))
                },
                Syntax::Deref => {
                    let val = compile(&ast.at(1).unwrap(), ce)?;
                    Ok(Box::new(move |e: &mut Env| {
                        Ok(val(e)?.deref())
                    }))
                },
                Syntax::Lst => {
                    let list_elems : Vec<EvalNode> =
                        ast.map_skip(|e| compile(e, ce), 1)?; 

                    Ok(Box::new(move |e: &mut Env| {
                        let v = VVal::vec();
                        for x in list_elems.iter() { v.push(x(e)?); }
                        Ok(v)
                    }))
                },
                Syntax::Map    => {
                    let map_elems : Result<Vec<(EvalNode,EvalNode)>, CompileError> =
                        ast.map_skip(|e| {
                                let k = e.at(0).unwrap();
                                let v = e.at(1).unwrap();
                                let kc = compile(&k, ce)?;
                                let vc = compile(&v, ce)?;
                                Ok((kc, vc))
                            }, 1);
                    if let Err(e) = map_elems { return Err(e); }
                    let map_elems = map_elems.unwrap();

                    Ok(Box::new(move |e: &mut Env| {
                        let v = VVal::map();
                        for x in map_elems.iter() {
                            let ke = x.0(e)?;
                            v.set_key(&ke, x.1(e)?);
                        }
                        Ok(v)
                    }))
                },
                Syntax::Or => {
                    let exprs : Vec<EvalNode> =
                        ast.map_skip(|e| compile(e, ce), 1)?;

                    Ok(Box::new(move |e: &mut Env| {
                        for x in exprs.iter() {
                            let ret = x(e)?;
                            if ret.b() {
                                return Ok(ret);
                            }
                        }
                        Ok(VVal::Bol(false))
                    }))
                },
                Syntax::And => {
                    let exprs : Vec<EvalNode> =
                        ast.map_skip(|e| compile(e, ce), 1)?;

                    Ok(Box::new(move |e: &mut Env| {
                        let mut ret = VVal::Nul;
                        for x in exprs.iter() {
                            ret = x(e)?;
                            if !ret.b() {
                                return Ok(VVal::Bol(false));
                            }
                        }
                        Ok(ret)
                    }))
                },
                Syntax::Call => {
                    let mut call_args : Vec<EvalNode> =
                        ast.map_skip(|e| compile(e, ce), 1)?;
                    call_args.reverse();
                    let func = call_args.pop().expect("function in evaluation args list");

                    Ok(Box::new(move |e: &mut Env| {
                        let f    = func(e)?;
                        let argc = call_args.len();
                        e.with_pushed_sp(argc, |e: &mut Env| {
                            for (i, x) in call_args.iter().enumerate() {
                                let v = x(e)?;
                                //d// println!("SETARGC: {} => {:?}", i, v);
                                e.set_arg(argc - (i + 1), v);
                            }
                            let ret = f.call_internal(e, argc);
                            if let Err(StackAction::Panic(msg, synpos)) = ret {
                                if synpos.is_none() {
                                    return Err(StackAction::Panic(msg, Some(vec![spos.clone()])));
                                } else {
                                    return Err(StackAction::Panic(msg, synpos));
                                }
                            } else {
                                ret
                            }
                        })
                    }))
                },
                Syntax::Func => {
                    let mut ce_sub = CompileEnv::create_env(Some(ce.clone()));

                    let label          = ast.at(1).unwrap();
                    let explicit_arity = ast.at(2).unwrap();
                    let stmts : Vec<EvalNode> =
                        ast.map_skip(|e| compile(e, &mut ce_sub), 3)?;

                    #[allow(unused_assignments)]
                    let fun_ref = Rc::new(RefCell::new(move |env: &mut Env, _argc: usize| {
                        let mut res = VVal::Nul;
                        for s in stmts.iter() {
                            if let VVal::Err(ev) = res {
                                let err_msg =
                                    format!("Error value '{}'@({},{}:{}) dropped.",
                                            ev.borrow().0.s(),
                                            ev.borrow().1.line,
                                            ev.borrow().1.col,
                                            ev.borrow().1.file);
                                return
                                    Err(StackAction::Panic(
                                        VVal::new_str(&err_msg), None));
                            }

                            res = VVal::Nul;
                            match s(env) {
                                Ok(v)  => { res = v; },
                                Err(StackAction::Return((v_lbl, v))) => {
                                    //d// println!("RETTETE {} {} {}", v_lbl.s(), label.s(), v.s());
                                    return
                                        if v_lbl.eqv(&label) { Ok(v) }
                                        else { Err(StackAction::Return((v_lbl, v))) }
                                },
                                Err(e) => { return Err(e); }
                            }
                        }
                        Ok(res)
                    }));

                    ce_sub.borrow_mut().explicit_arity.0 =
                        match explicit_arity.at(0).unwrap_or(VVal::Nul) {
                            VVal::Int(i) => ArityParam::Limit(i as usize),
                            VVal::Bol(true) => ArityParam::Limit(0),
                            _ => ArityParam::Undefined,
                        };

                    ce_sub.borrow_mut().explicit_arity.1 =
                        match explicit_arity.at(1).unwrap_or(VVal::Nul) {
                            VVal::Int(i) => ArityParam::Limit(i as usize),
                            VVal::Bol(true) => ArityParam::Infinite,
                            _ => ArityParam::Undefined,
                        };

                    let deciding_min_arity = if ce_sub.borrow().explicit_arity.0 != ArityParam::Undefined {
                        ce_sub.borrow().explicit_arity.0.clone()
                    } else {
                        ce_sub.borrow().implicit_arity.0.clone()
                    };

                    let deciding_max_arity = if ce_sub.borrow().explicit_arity.1 != ArityParam::Undefined {
                        ce_sub.borrow().explicit_arity.1.clone()
                    } else {
                        ce_sub.borrow().implicit_arity.1.clone()
                    };

                    let min_args : Option<usize> = match deciding_min_arity {
                        ArityParam::Infinite  => None,
                        ArityParam::Undefined => Some(0),
                        ArityParam::Limit(i)  => Some(i),
                    };

                    let max_args : Option<usize> = match deciding_max_arity {
                        ArityParam::Infinite  => None,
                        ArityParam::Undefined => Some(0),
                        ArityParam::Limit(i)  => Some(i),
                    };

                    let env_size = CompileEnv::local_env_size(&ce_sub);
                    Ok(Box::new(move |e: &mut Env| {
                        let mut v = Vec::new();
                        ce_sub.borrow_mut().copy_upvals(e, &mut v);
                        Ok(VValFun::new_val(
                            fun_ref.clone(),
                            v, env_size, min_args, max_args,
                            Some(spos.clone())))
                    }))
                },
                _ => { ast.to_compile_err(format!("bad input: {}", ast.s())) }
            }
        },
        _ => {
            let am = ast.clone();
            Ok(Box::new(move |_e: &mut Env| Ok(am.clone())))
        }
    }
}

/// Evaluates a parsed AST a number of times and prints out
/// some benchmarking information.
#[allow(clippy::cast_lossless)]
pub fn bench_eval_ast(v: VVal, g: GlobalEnvRef, runs: u32) -> VVal {
    let mut ce = Rc::new(RefCell::new(CompileEnv {
        parent:    None,
        global:    g,
        local_map: std::collections::HashMap::new(),
        locals:    Vec::new(),
        upvals:    Vec::new(),
        implicit_arity: (ArityParam::Undefined, ArityParam::Undefined),
        explicit_arity: (ArityParam::Undefined, ArityParam::Undefined),
    }));

    let prog = compile(&v, &mut ce);
    match prog {
        Ok(r) => {
            let mut e = Env::new();
            e.push(VVal::Flt(42.42)); // 2nd arg
            e.push(VVal::Int(13));    // 1st arg
            e.argc = 2;
            e.set_bp(CompileEnv::local_env_size(&ce));

            if runs > 1 {
                let mut ret = VVal::Nul;
                let mut rts = 0.0;
                let mut cnt = 0;
                for _ in 0..runs {
                    let now = Instant::now();
                    match r(&mut e) {
                        Ok(v)   => { ret = v },
                        Err(je) => { ret = VVal::err(VVal::new_str(&format!("EXEC ERR: Caught {:?}", je)), v.get_syn_pos()) }
                    }
                    rts += now.elapsed().as_millis() as f64;
                    cnt += 1;
                }
                println!("*** runtime: {} ({} runs)", rts / (cnt as f64), cnt);
                ret
            } else {
                match r(&mut e) {
                    Ok(v)   => { v },
                    Err(je) => { VVal::err(VVal::new_str(&format!("EXEC ERR: Caught {:?}", je)), v.get_syn_pos()) }
                }
            }
        },
        Err(re) => { panic!(format!("COMPILE ERROR: {}", re)); },
    }
}

/// Evaluates a string of WLambda code, executes it and returns a string representation of the VVal.
///
/// This functions is mainly existing for testing purposes.
#[allow(dead_code)]
pub fn s_eval(s: &str) -> String {
    let global = GlobalEnv::new_default();
    match parser::parse(s, 0) {
        Ok(ast) => bench_eval_ast(ast, global, 1).s(),
        Err(e)  => { panic!(format!("EVAL ERROR: {}", e)); },
    }
}


/// Evaluates a string of WLambda code, executes it and returns a string representation of the VVal.
/// Any critical error (parse error for instance) is not panic!'ed, but
/// returned as informal string.
///
/// This functions is mainly existing for testing purposes.
#[allow(dead_code)]
pub fn s_eval_no_panic(s: &str) -> String {
    let global = GlobalEnv::new_default();
    match parser::parse(s, 0) {
        Ok(ast) => bench_eval_ast(ast, global, 1).s(),
        Err(e)  => { format!("EVAL ERROR: {}", e) },
    }
}

/// Evaluates a piece of WLambda code in a default global environment.
///
/// ```
/// println!("> {}", wlambda::eval("${a = 10, b = 20}").unwrap().s());
/// ```
#[allow(dead_code)]
pub fn eval(s: &str) -> Result<VVal, EvalError>  {
    let mut ctx = EvalContext::new_default();
    ctx.eval(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_trivial() {
        assert_eq!(s_eval("_"),                       "13");          // XXX: in test env
        assert_eq!(s_eval("_1"),                      "42.42");       // XXX: in test env
        assert_eq!(s_eval("@"),                       "$[13,42.42]");  // XXX: in test env

        assert_eq!(s_eval("$n"), "$n");
        assert_eq!(s_eval("10"), "10");
        assert_eq!(s_eval("10; 20; 30"),              "30");
        assert_eq!(s_eval("!x = 10; x"),              "10");
        assert_eq!(s_eval("!x = $true; x"),           "$true");
        assert_eq!(s_eval("{ 10 }"),                  "&VValFun");
        assert_eq!(s_eval("{ 10 }[]"),                "10");
        assert_eq!(s_eval("{ 10; 20 }[]"),            "20");
        assert_eq!(s_eval("!x = $&11; { 12; x }[]"),                    "11");
        assert_eq!(s_eval("!x = 11; { 12; x }[]"),                        "11");
        assert_eq!(s_eval("!x = 13; { .x = 12 }[]; { x }[] "),            "13");
        assert_eq!(s_eval("!x = $&13; { .x = 12 }[]; $[{ x }[], $*x]"),  "$[12,12]");
        assert_eq!(s_eval("!x = $&13; { .x = 12; x }[]; $[{ x }[], { .x = 15; x }[], $*x]"), "$[12,15,15]");
        assert_eq!(s_eval("{ _ } 10"),                        "10");
        assert_eq!(s_eval("!y = $&0; { .y = _ } 10; $*y"),   "10");
        assert_eq!(s_eval("${:a = 10, :b = 20}"),             "${a=10,b=20}");
        assert_eq!(s_eval("${:b = 20, :a = 10}"),             "${a=10,b=20}");
        assert_eq!(s_eval("${a = 10, b = 20}"),               "${a=10,b=20}");
        assert_eq!(s_eval("${b = 20, a = 10}"),               "${a=10,b=20}");
        assert_eq!(s_eval("${(:a) = 10, b = 20}"),            "${a=10,b=20}");
        assert_eq!(s_eval("${(:b) = 20, a = 10}"),            "${a=10,b=20}");
        assert_eq!(s_eval("!x = ${:b = 20, :a = 10}; x"),     "${a=10,b=20}");
        assert_eq!(s_eval("!x = ${:b = 20, :a = 10}; x.a"),   "10");
        assert_eq!(s_eval("!x = ${:b = 20, :a = 11}; :a x"),  "11");
        assert_eq!(s_eval("!x = ${}; x.a = 12; x.a"),         "12");
        assert_eq!(s_eval("!x = ${}; x.a = 12; x"),           "${a=12}");

        assert_eq!(s_eval("$[33,44,55].2"), "55");
        assert_eq!(s_eval("$[33,44,55].0"), "33");
        assert_eq!(s_eval("$[33,44,55].3"), "$n");
        assert_eq!(s_eval("1 $[33,44,55]"), "44");
    }

    #[test]
    fn check_ref_closures() {
        assert_eq!(s_eval("!c1 = { !a = $&& 1.2; { $*a } }; c1[][]"),            "1.2");
        assert_eq!(s_eval("!c1 = { !a = $& 1.2; { a } }; c1[][]"),           "$n");
        assert_eq!(s_eval("!c1 = { !a = $& 1.2; { a }[] }; c1[]"),           "1.2");
        assert_eq!(s_eval("!c1 = { !a = $& 1.2; !a = $n; { a }[] }; c1[]"),  "$n");
        assert_eq!(s_eval("!outer_a = $&2.3; !c1 = { !a = $&&1.2; { $*a + outer_a } }; c1[][]"), "3.5");
        assert_eq!(s_eval("!outer_a = $&2.3; !c1 = { !a = $&1.2; { outer_a + a } }; c1[][]"), "2.3");
        assert_eq!(s_eval("!outer_a = $&2.3; !c1 = { !a = $&1.2; { outer_a + a } }; !outer_a = $n; c1[][]"), "0");
    }

    #[test]
    fn check_arithmetics() {
        assert_eq!(s_eval("12 + 23"),         "35");
        assert_eq!(s_eval("+[12, 23]"),       "35");
        assert_eq!(s_eval("+ 12 23"),         "35");
        assert_eq!(s_eval("+ 12 ~ - 24 23"),  "13");
        assert_eq!(s_eval("(+ 12 ~ - 24 23) + 1"),    "14");
        assert_eq!(s_eval("(12 + 1) == 13"),          "$true");
        assert_eq!(s_eval("(+ 12 ~ - 24 23) == 13"),  "$true");
        assert_eq!(s_eval("(+ 12 ~ - 24 23) == 14"),  "$false");
        assert_eq!(s_eval("12.12 + 23.23"),           "35.35");

        // coertion of strings and keys to numbers:
        assert_eq!(s_eval(":10 + :20"),       "30");
        assert_eq!(s_eval(":-10 + :20"),      "10");

        assert_eq!(s_eval("12 - 23"),         "-11");
        assert_eq!(s_eval("5 * 4"),           "20");
        assert_eq!(s_eval("20 / 5"),          "4");

        assert_eq!(s_eval("6 - 3 * 2"),       "0");
        assert_eq!(s_eval("12 / 6 - 3 * 2"),  "-4");
    }

    #[test]
    fn check_compile_env() {
//        let ce = CompileEnv::create_env(None);
//
//        assert_eq!(ce.borrow_mut().def("x", false), 0);
//        assert_eq!(ce.borrow_mut().def("y", false), 1);
//        assert_eq!(ce.borrow_mut().def("z", false), 2);
//
//        let ce2 = CompileEnv::create_env(Some(ce.clone()));
//        assert_eq!(ce2.borrow_mut().def("a", false), 0);
//        assert_eq!(ce2.borrow_mut().get("y"), VarPos::UpValue(0));
//        assert_eq!(ce2.borrow_mut().get("z"), VarPos::UpValue(1));
//        assert_eq!(ce2.borrow_mut().get("a"), VarPos::Local(0));
//
//        let ce3 = CompileEnv::create_env(Some(ce2.clone()));
//        assert_eq!(ce3.borrow_mut().get("a"), VarPos::UpValue(0));
//        assert_eq!(ce3.borrow_mut().get("z"), VarPos::UpValue(1));
//
//        assert_eq!(ce2.borrow_mut().get("x"), VarPos::UpValue(2));
    }

    #[test]
    fn check_bool() {
        assert_eq!(s_eval("!a = $&0; $t { .a = 1 } { .a = 2 }; $*a"), "1");
        assert_eq!(s_eval("!a = $&0; $f { .a = 1 } { .a = 2 }; $*a"), "2");
    }

    #[test]
    fn check_range() {
        assert_eq!(s_eval("!x = $& 10; { .x = x + _; x } 5"),                    "15");
        assert_eq!(s_eval("!x = $& 10; { .x = { x + 11 + _ }(2) + _; x } 5"),    "28");
        assert_eq!(s_eval("!x = $& 10;   range 1 3 1     { .x = x + _; x }; $*x"), "16");
        assert_eq!(s_eval("!x = $& 10.0; range 1.0 3 0.5 { .x = x + _; x }; $*x"), "20");
    }

    #[test]
    fn check_push() {
        assert_eq!(s_eval("!a = 10; !x = $[1]; !y = 20; x"), "$[1]");
        assert_eq!(s_eval("!x = $&&$[]; push $*x 12; $*x"), "$[12]");
        assert_eq!(s_eval("!a = 10; !x = $[]; !y = 20; push x 10; push x 30; x"), "$[10,30]");
        assert_eq!(s_eval("!x = $&&$[]; push $*x 10; push $*x 20; $*x"), "$[10,20]");
    }

    #[test]
    fn check_range_break() {
        assert_eq!(s_eval("4 == 4"), "$true");
        assert_eq!(s_eval("range 0 10 1 {|1| break 14 }"), "14");
        assert_eq!(s_eval("range 0 10 1 { !i = _; (i == 4) { break ~ i + 10 } }"), "14");
    }

    #[test]
    fn check_range_next() {
        assert_eq!(s_eval("!x = $&&0; range 0 10 1 { (_ == 4) { next[] }; .*x = $*x + _; }; $*x"), "51");
        assert_eq!(s_eval("!x = $&&0; range 0 10 1 { next[]; .*x = $*x + _; }; $*x"), "0");
        assert_eq!(s_eval("!x = $&0; range 0 10 1 { (_ == 4) { next[] }; .x = x + _; }; $*x"), "51");
        assert_eq!(s_eval("!x = $&0; range 0 10 1 { next[]; .x = x + _; }; $*x"), "0");
    }

    #[test]
    fn check_while() {
        assert_eq!(s_eval(r#"
            !x = $& 0;
            while { x == 0 } {
                .x = x + 1;
            };
            $*x
        "#),
        "1");

        assert_eq!(s_eval(r#"
            !x = $&0;
            while { x == 0 } {
                break 10;
                .x = x + 1;
            }
        "#),
        "10");

        assert_eq!(s_eval(r#"
            !x = $&0;
            while { x == 0 } {
                next;
                .x = x + 1;
            };
            $*x
        "#),
        "1");

        assert_eq!(s_eval(r#"
            !x = 0;
            !a = while { x == 0 } {
                break 20;
            };
            a
        "#),
        "20");
    }

    #[test]
    fn check_args() {
        assert_eq!(s_eval("{ $[_, _1, _2] }[1, 2, 3]"),       "$[1,2,3]");
        assert_eq!(s_eval("{ @ }[1, 2, 3]"),                  "$[1,2,3]");
        assert_eq!(s_eval("{|3<4| $[_, _1, _2, _3] }[1, 2, 3]"),   "$[1,2,3,$n]");
    }

    #[test]
    fn check_to_drop() {
        assert_eq!(s_eval("!x = $&1; { .x = 2; }[]; $*x"), "2");
        assert_eq!(s_eval("!x = $&1; { !d = { .x = 2; }; d }[][]; $*x"), "2");
        assert_eq!(s_eval(r#"
            !x = $&0;
            { !d = to_drop 10 {|| .x = 17; } }[];
            $*x
        "#),
        "17");
    }

    #[test]
    fn check_call_primitives() {
        assert_eq!(s_eval("13[]"), "13");
        assert_eq!(s_eval("$t[]"), "$true");
        assert_eq!(s_eval(":foo"), ":\"foo\"");
    }

    #[test]
    fn check_global_vars() {
        assert_eq!(s_eval("
            !:global x = 120;
            !f = { x };
            !l = f[];
            .x = 30;
            !l2 = f[];
            $[l, l2, x]
        "), "$[120,30,30]");
        assert_eq!(s_eval("
            !:global x = 120;
            !f = {
                !(x, b) = @;
                .x = x + 2;
                $[x, b]
            };
            !l = f[1, 2];
            .x = 30;
            !l2 = f[3, 4];
            $[l, l2, x]
        "), "$[$[3,2],$[5,4],30]");
        assert_eq!(s_eval("
            !:global x = 120;
            !f = {
                !(y, b) = @;
                .x = x + 2;
                $[y, x, b]
            };
            !l = f[1, 2];
            .x = x + 30;
            !l2 = f[3, 4];
            $[l, l2, x]
        "), "$[$[1,122,2],$[3,154,4],154]");
        assert_eq!(s_eval("
            !f = { !:global (a, b, c) = @; };
            .b = 20;
            !x1 = $[a, b, c];
            f[13, 17, 43];
            !x2 = $[a, b, c];
            $[x1, x2]
        "), "$[$[$n,20,$n],$[13,17,43]]");
    }

    #[test]
    fn check_ops() {
        assert_eq!(s_eval("10 < 20"),     "$true");
        assert_eq!(s_eval("11 < 10"),     "$false");
        assert_eq!(s_eval("10 < 10"),     "$false");
        assert_eq!(s_eval("10 > 20"),     "$false");
        assert_eq!(s_eval("11 > 10"),     "$true");
        assert_eq!(s_eval("10 > 10"),     "$false");
        assert_eq!(s_eval("10 <= 20"),    "$true");
        assert_eq!(s_eval("11 <= 10"),    "$false");
        assert_eq!(s_eval("10 <= 10"),    "$true");
        assert_eq!(s_eval("10 >= 20"),    "$false");
        assert_eq!(s_eval("11 >= 10"),    "$true");
        assert_eq!(s_eval("10 >= 10"),    "$true");
        assert_eq!(s_eval("10.1 < 20.4"), "$true");
        assert_eq!(s_eval("11.2 < 10.2"), "$false");
        assert_eq!(s_eval("10.3 < 10.4"), "$true");
        assert_eq!(s_eval("22 == 22"),    "$true");
        assert_eq!(s_eval("22 == 23"),    "$false");
        assert_eq!(s_eval("22 != 22"),    "$false");
        assert_eq!(s_eval("21 != 22"),    "$true");

        assert_eq!(s_eval("$t &and $t"),    "$true");
        assert_eq!(s_eval("$f &and $t"),    "$false");
        assert_eq!(s_eval("$t &or  $t"),    "$true");
        assert_eq!(s_eval("$f &or  $t"),    "$true");
        assert_eq!(s_eval("$f &or  $f"),    "$false");

        assert_eq!(s_eval("2 ^ 2"),       "4");
        assert_eq!(s_eval("2 ^ 3"),       "8");
        assert_eq!(s_eval("2.1 ^ 2"),     "4.41");
        assert_eq!(s_eval("4 ^ 0.5"),     "1");
        assert_eq!(s_eval("4.0 ^ 0.5"),   "2");

        assert_eq!(s_eval("4 % 5"),       "4");
        assert_eq!(s_eval("6 % 5"),       "1");
        assert_eq!(s_eval("4.4 % 5.5"),   "4.4");
        assert_eq!(s_eval("5.5 % 5.5"),   "0");

        assert_eq!(s_eval("neg 0xFF"),    "-256");
        assert_eq!(s_eval("uneg 0xFF"),   "4294967040");
        assert_eq!(s_eval("uneg 0x1"),    "4294967294");
        assert_eq!(s_eval("uneg 0x0"),    "4294967295");

        assert_eq!(s_eval("(0x10 &| 0x01) == 0x11"), "$true");
        assert_eq!(s_eval("(0x0f &  0x33) == 0x3"),  "$true");
        assert_eq!(s_eval("(0x11 &^ 0x01) == 0x10"), "$true");
        assert_eq!(s_eval("(0b1 << 1) == 0b10"),     "$true");
        assert_eq!(s_eval("(0b1 << 2) == 0b100"),    "$true");
        assert_eq!(s_eval("(0b1 >> 1) == 0x0"),      "$true");

        assert_eq!(s_eval("!x = $&0; !b = { $t }[] &and { .x = 1; 10 }[]; $[$*x, b]"), "$[1,10]");
        assert_eq!(s_eval("!x = $&0; !b = { $f }[] &and { .x = 1; 10 }[]; $[$*x, b]"), "$[0,$false]");
        assert_eq!(s_eval("!x = $&0; !b = { $f }[] &or { .x = 1; 10 }[]; $[$*x, b]"),  "$[1,10]");
        assert_eq!(s_eval("!x = $&0; !b = { 12 }[] &or { .x = 1; 10 }[]; $[$*x, b]"),  "$[0,12]");
        assert_eq!(s_eval(r#"
            !x = $&0;
            !f = { yay[x]; .x = x + 1; x };
            !b = f[]
                &and f[]
                &and f[]
                &and f[]
                &and f[];
            $[$*x, b]
        "#),  "$[5,5]");

        assert_eq!(s_eval(r#"
            !c = $&0;
            !x = $&10;
            while { x > 0 } { .c = c + 1; .x = x - 1; };
            .*x = 20;
            while { x > 0 } { .c = c + 1; .x = x - 1; };
            $*c
        "#), "30");
    }

    #[test]
    fn check_destructure() {
        assert_eq!(s_eval("!(a, b) = $[10, 20]; $[a, b]"),        "$[10,20]");
        assert_eq!(s_eval("!(a, b) = $[10, 20, 30]; $[a, b]"),    "$[10,20]");
        assert_eq!(s_eval("!(a, b) = $[10]; $[a, b]"),            "$[10,$n]");
        assert_eq!(s_eval(
            "!(a, b) = ${a = 10, b= 20, c=30}; $[a, b]"),
            "$[10,20]");
        assert_eq!(s_eval(
            "!(a, b) = ${a = 10}; $[a, b]"),
            "$[10,$n]");
        assert_eq!(s_eval(
            "!(a, b) = ${b = 20, c = 30}; $[a, b]"),
            "$[$n,20]");

        assert_eq!(s_eval("!(a, b) = $[$&10, $&20]; { .a = 33; }[]; $[$*a, $*b]"), "$[33,20]");
        assert_eq!(s_eval(r#"
            !fun = {
                !(a, b) = $[$&&10, $&&20];
                $[{$*a}, { .*a = 33; }];
            }[];
            (1 fun)[];
            (0 fun)[]
        "#),
        "33");
        assert_eq!(s_eval(r#"
            !fun = {
                !(a, b) = $[$&10, $&20];
                $[{$[a, b]}, { .a = 33; }];
            }[];
            (1 fun)[];
            (0 fun)[]
        "#),
        "$[$n,$n]");
        assert_eq!(s_eval(r#"
            !fun = {
                !(a, b) = $[$&10, $&20];
                !(wa, wb) = $[wl:weaken a, wl:weaken b];
                $[{$[wa, wb]}, { .wa = 33; }];
            }[];
            (1 fun)[];
            (0 fun)[]
        "#),
        "$[$n,$n]");
        assert_eq!(s_eval(r#"
            !fun = {
                !(a, b) = $[$&&10, $&&20];
                $[{$[$*a, $*b]}, { .*a = 33; }];
            }[];
            (1 fun)[];
            (0 fun)[]
        "#),
        "$[33,20]");

        assert_eq!(
            s_eval("!a = 0; !b = 0; .(a, b) = $[10, 20]; $[a, b]"),
            "$[10,20]");

        assert_eq!(
            s_eval("!a = 0; !b = 0; .(a, b) = 40; $[a, b]"),
            "$[40,40]");
    }

    #[test]
    fn check_field() {
        assert_eq!(s_eval("!v = $[]; v.0 = 10; v"), "$[10]");
        assert_eq!(s_eval("!v = $[]; v.2 = 10; v"), "$[$n,$n,10]");
        assert_eq!(s_eval("!i = 2; !v = $[]; v.(i) = 10; v"), "$[$n,$n,10]");
    }

    #[test]
    fn check_type() {
        assert_eq!(s_eval("type type"), "\"function\"");
        assert_eq!(s_eval("type 12"),   "\"int\"");
        assert_eq!(s_eval("type 12.2"), "\"float\"");
        assert_eq!(s_eval("type $n"),   "\"none\"");
        assert_eq!(s_eval("type $[]"),  "\"vector\"");
        assert_eq!(s_eval("type ${}"),  "\"map\"");
    }

    #[test]
    fn check_string() {
        assert_eq!(s_eval("\"foo\""),   "\"foo\"");
        assert_eq!(s_eval("$q#foo#"),   "\"foo\"");
        assert_eq!(s_eval("$b\"foo\""), "$b\"foo\"");

        assert_eq!(s_eval("\"foo\"(0)"),                       "\"f\"");
        assert_eq!(s_eval("\"foo\" 0"),                        "\"f\"");
        assert_eq!(s_eval("\"foobar\" 1 3"),                   "\"oob\"");
        assert_eq!(s_eval("\"foobar\"[1, 3]"),                 "\"oob\"");
        assert_eq!(s_eval("\"foobar\" $[1, 3]"),               "\"oob\"");
        assert_eq!(s_eval("\"foobar\" $q/xyz/"),               "\"foobarxyz\"");
        assert_eq!(s_eval("\"foobar\" ${ foobar = 12 }"),       "12");
        assert_eq!(s_eval("\"foobar\" ${ (\"foobar\") = 12 }"), "12");
    }

    #[test]
    fn check_match() {
        assert_eq!(s_eval("match 10 :?t :int {|| 13 } {|| 14 }"), "13");
        assert_eq!(s_eval("match 10 :?t :str {|| 13 } {|| 14 }"), "14");
        assert_eq!(s_eval("match 10 :?t :str {|| 13 }"),        "$n");
        assert_eq!(s_eval("match $q xx :?s :xx      {|| 15 }"),    "15");
        assert_eq!(s_eval("match $q xx :?s :yx :xx  {|| 16 }"),    "16");
        assert_eq!(s_eval("match $q zx :?s :yx :xx  {|| 16 } {|| 17 }"), "17");
        assert_eq!(s_eval("match $q xx :?p { _ == $q|xx| } {|| 18 }"),    "18");
        assert_eq!(s_eval("match $q x9 :?p { yay _; _ == $q|xx| } {|| 181 } {|| 19 }"), "19");
        assert_eq!(s_eval("match 10"),                           "$n");
        assert_eq!(s_eval("
            match ($e $[:foo, 1, 2, 3])
                :?e :foo {|| 19 }
                :?e :bar {|| 19.2 }
                :?e :sna :snu {|| 19.4 }
                { :nothin }
        "), "19");
        assert_eq!(s_eval("
            match (
                $e $[:bar, 1, 2, 3] )
                :?e :foo {|| 19 }
                :?e :bar {|4| $[(2 _) + 19.2, _1] }
                :?e :sna :snu {|| 19.4 }
                { :nothin }
        "), "$[21,3]");
        assert_eq!(s_eval("
            match ($e $[:snu, 1, 2, 3])
                :?e :foo {|| 19 }
                :?e :bar {|| 19.2 }
                :?e :sna :snu {|| 19.4 + _.3 }
                { :nothin }
        "), "22.4");
    }

    #[test]
    fn check_callbacks() {
        let global = GlobalEnv::new_default();
        global.borrow_mut().add_func("reg", |env: &mut Env, _argc: usize| {
            let fun = env.arg(0);
            env.with_user_do(|v: &mut Vec<VVal>| v.push(fun.clone()));
            Ok(VVal::Nul)
        }, Some(1), Some(1));

        let reg : Rc<RefCell<Vec<VVal>>> = Rc::new(RefCell::new(Vec::new()));

        let mut ctx = EvalContext::new_with_user(global, reg.clone());
        ctx.eval("reg { _ + 10 }").unwrap();
        let n = reg.borrow_mut()[0].clone();
        let ret = ctx.call(&n, &vec![VVal::Int(11)]).unwrap();
        assert_eq!(ret.i(), 21);
    }

    #[test]
    fn check_returned_functions() {
        let mut ctx = EvalContext::new_default();
        let n = ctx.eval("{ _ + 11 }").unwrap();
        let ret = ctx.call(&n, &vec![VVal::Int(11)]).unwrap();
        assert_eq!(ret.i(), 22);
    }

    #[test]
    fn check_global_var_api() {
        let mut ctx = EvalContext::new_default();
        ctx.set_global_var("XXX", &VVal::Int(210));
        ctx.set_global_var("YYY", &VVal::Nul);
        let n = ctx.eval("{ .YYY = _ + 11; XXX + _ } 20").unwrap();
        assert_eq!(ctx.get_global_var("YYY").unwrap().i(), 31);
        assert_eq!(n.i(), 230);
    }

    #[test]
    fn check_return() {
        assert_eq!(s_eval("block {
            !x = { return 11; 20 }[];
            .x = x + 20;
            x
        }"), "31");
        assert_eq!(s_eval("block {
            !x = { 13 }[];
            .x = 20;
            x
        }"), "20");
        assert_eq!(s_eval("block :x {
            !x = { return :x 10; 20 }[];
            .x = x + 20;
            x
        }"), "10");
        assert_eq!(s_eval("\\:x {
                !x = { return :x 10; 20 }[];
                .x = x + 20;
                x
            }[]
        "), "10");
        assert_eq!(s_eval("{ 10; 20 }[]"), "20");
        assert_eq!(s_eval("!g = { _1 }; g :x 10"), "10");
        assert_eq!(s_eval("block {
            !x = { block :x { return :x 13; 20 } }[];
            yay x;
            .x = x + 12;
            x
        }"), "25");
    }

    #[test]
    fn check_arity() {
        assert_eq!(s_eval_no_panic("{}[1,2,3]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 0 arguments, got 3\\\" }), Some([SynPos { syn: Call, line: 1, col: 3, file: 0 }]))\"");
        assert_eq!(s_eval("{|3| _1 }[1,2,3]"), "2");
        assert_eq!(s_eval_no_panic("{|3| _1 }[2,3]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at least 3 arguments, got 2\\\" }), Some([SynPos { syn: Call, line: 1, col: 10, file: 0 }]))\"");
        assert_eq!(s_eval_no_panic("{|3| _1 }[2,3,4,5]"),  "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 3 arguments, got 4\\\" }), Some([SynPos { syn: Call, line: 1, col: 10, file: 0 }]))\"");
        assert_eq!(s_eval("{|0<4| _1 }[]"), "$n");
        assert_eq!(s_eval("{|0<4| _1 }[1]"), "$n");
        assert_eq!(s_eval("{|0<4| _1 }[1,2]"), "2");
        assert_eq!(s_eval("{|0<4| _1 }[1,2,3]"), "2");
        assert_eq!(s_eval("(\\|0<4| _1)[1,2,3]"), "2");
        assert_eq!(s_eval("{|0<4| _1 }[1,2,3,4]"), "2");
        assert_eq!(s_eval_no_panic("{|0<4| _1 }[1,2,3,4,5]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 4 arguments, got 5\\\" }), Some([SynPos { syn: Call, line: 1, col: 12, file: 0 }]))\"");
        assert_eq!(s_eval("{ @ }[1,2,3,4,5]"), "$[1,2,3,4,5]");
        assert_eq!(s_eval("{|2| @ }[1,2]"), "$[1,2]");
        assert_eq!(s_eval_no_panic("{|2| @ }[1]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at least 2 arguments, got 1\\\" }), Some([SynPos { syn: Call, line: 1, col: 9, file: 0 }]))\"");
        assert_eq!(s_eval_no_panic("{|2| @ }[1,2,3]"),  "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 2 arguments, got 3\\\" }), Some([SynPos { syn: Call, line: 1, col: 9, file: 0 }]))\"");

        assert_eq!(s_eval_no_panic("{!(a,b,c) = @;}[1,2,3,4]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 3 arguments, got 4\\\" }), Some([SynPos { syn: Call, line: 1, col: 16, file: 0 }]))\"");
        assert_eq!(s_eval_no_panic("{_3; !(a,b,c) = @; }[1,2,3,4,5]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 4 arguments, got 5\\\" }), Some([SynPos { syn: Call, line: 1, col: 21, file: 0 }]))\"");
        assert_eq!(s_eval_no_panic("{!(a,b,c) = @; _3 }[1,2,3,4,5]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 4 arguments, got 5\\\" }), Some([SynPos { syn: Call, line: 1, col: 20, file: 0 }]))\"");
        assert_eq!(s_eval("{!(a,b,c) = @; b }[1,2,3]"), "2");
        assert_eq!(s_eval("{!(a,b,c) = @; _3 }[1,2,3,5]"), "5");
        assert_eq!(s_eval("{!:global (a,b,c) = @; _3 }[1,2,3,5]"), "5");
        assert_eq!(s_eval_no_panic("{!:global (a,b,c) = @; _3 }[1,2,3,4,5]"), "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[1:1/0] expects at most 4 arguments, got 5\\\" }), Some([SynPos { syn: Call, line: 1, col: 28, file: 0 }]))\"");
    }

    #[test]
    fn check_error_fn_pos() {
        assert_eq!(s_eval_no_panic(r#"

            !x = {

            };

            !l = { x 10 };
            l[];
        "#),
        "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[3:18/0] expects at most 0 arguments, got 1\\\" }), Some([SynPos { syn: Call, line: 7, col: 22, file: 0 }]))\"");
    }

    #[test]
    fn check_error() {
        assert_eq!(s_eval_no_panic("$e 10; 14"),
                   "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"Error value \\\\\\\'10\\\\\\\'(@1,4:0) dropped.\\\" }), None)\"");
        assert_eq!(s_eval_no_panic("{ { { { $e 10; 14 }[]; 3 }[]; 9 }[]; 10 }[]"),
                   "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"Error value \\\\\\\'10\\\\\\\'@(1,12:0) dropped.\\\" }), Some([SynPos { syn: Call, line: 1, col: 20, file: 0 }]))\"");
        assert_eq!(s_eval_no_panic("_? $e 10"),
                   "$e \"EXEC ERR: Caught Return((Nul, Err(RefCell { value: (Int(10), SynPos { syn: Err, line: 1, col: 7, file: 0 }) })))\"");
        assert_eq!(s_eval_no_panic("_? { return $e 10; 10 }[]"),
                   "$e \"EXEC ERR: Caught Return((Nul, Err(RefCell { value: (Int(10), SynPos { syn: Err, line: 1, col: 16, file: 0 }) })))\"");
        assert_eq!(s_eval_no_panic("unwrap $e 1"),
                   "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"unwrap error: 1@(1,11:0)\\\" }), Some([SynPos { syn: Call, line: 1, col: 8, file: 0 }]))\"");
        assert_eq!(s_eval_no_panic("unwrap 1.1"), "1.1");
        assert_eq!(s_eval_no_panic("on_error {|4| _ + 20 } $e 19.9"), "39.9");

        assert_eq!(s_eval_no_panic("{ { { panic 102 }[]; 20 }[]; return 20 }[]; 49"),
                   "$e \"EXEC ERR: Caught Panic(Int(102), Some([SynPos { syn: Call, line: 1, col: 13, file: 0 }]))\"");

        assert_eq!(s_eval_no_panic("
            !x = $&10;
            {
                .x = x + 1;
                block :outer { .x = x + 1; };
                .x = x + 1;
            }[];
            $*x
        "), "13");
        assert_eq!(s_eval_no_panic("
            !gen_err = { $e $q$something_failed!$ };
            !x = $&10;
            !msg = $&$q'all ok';
            {
                .x = x + 1;
                on_error {|4| .x = x * 2; .msg = _; }
                    (block :outer { _? :outer gen_err[]; .x = x + 1; });
                .x = x + 1;
            }[];
            $[$*x, $*msg]
        "), "$[23,\"something_failed!\"]");
        assert_eq!(s_eval_no_panic("
            !gen_ok = { 99 };
            !x = $&10;
            !msg = $&$q'all ok';
            {
                .x = x + 1;
                on_error { .x = x * 2; .msg = _; }
                    ~ block :outer { _? :outer gen_ok[]; .x = x + 1; };
                .x = x + 1;
            }[];
            $[$*x, $*msg]
        "), "$[13,\"all ok\"]");

        assert_eq!(s_eval_no_panic("{ $e 23 }[] | on_error {|4| _ + 21 }"), "44");
    }

    #[test]
    fn check_prelude() {
        assert_eq!(s_eval("bool $n"),           "$false");
        assert_eq!(s_eval("int $n"),            "0");
        assert_eq!(s_eval("float $q$10.2$"),    "10.2");
        assert_eq!(s_eval("str 10.3"),          "\"10.3\"");
        assert_eq!(s_eval("sym \"foo\""),       ":\"foo\"");
        assert_eq!(s_eval("sym 10.4"),          ":\"10.4\"");
        assert_eq!(s_eval("(bool $e :fail) { 10 } { 20 }"), "20");
        assert_eq!(s_eval("fold 1 { _ + _1 } $[1,2,3,4]"), "11");
        assert_eq!(s_eval("take 2 $[1,2,3,4,5,6]"), "$[1,2]");
        assert_eq!(s_eval("drop 2 $[1,2,3,4,5,6]"), "$[3,4,5,6]");
    }

    #[test]
    fn check_oop() {
        assert_eq!(s_eval(r#"
            !new = {
                !obj = $&${};
                obj.add = { obj.b + 10 };
                obj.get = { type obj };
                obj.set = { obj.b = _; };
                obj
            };
            !v = $[];
            !o = new[];
            !ext_add = o.add;
            !ext_get = o.get;
            o.set 10;
            push v o.add[];
            push v o.get[];
            push v ext_add[];
            push v ext_get[];
            .o = $n;
            push v ext_add[];
            push v ext_get[];
            v
        "#),
        "$[20,\"map\",20,\"map\",10,\"none\"]");
        assert_eq!(s_eval(r#"
            !obj = ${};
            !x = $&&0;
            obj.a = { .*x = 10 };
            obj.a[];
            $*x
        "#),
        "10");
    }

    #[test]
    fn check_test_funs() {
        assert_eq!(s_eval("is_none $n"),        "$true");
        assert_eq!(s_eval("is_err $e $n"),      "$true");
        assert_eq!(s_eval("is_map ${}"),        "$true");
        assert_eq!(s_eval("is_vec $[]"),        "$true");
        assert_eq!(s_eval("is_sym :f"),         "$true");
        assert_eq!(s_eval("is_bool $n"),        "$false");
        assert_eq!(s_eval("is_bool $f"),        "$true");
        assert_eq!(s_eval("is_bytes $f"),       "$false");
        assert_eq!(s_eval("is_bytes \"f\""),    "$false");
        assert_eq!(s_eval("is_bytes $b\"f\""),  "$true");
        assert_eq!(s_eval("is_bytes $Q'f'"),    "$true");
        assert_eq!(s_eval("is_str \"f\""),      "$true");
        assert_eq!(s_eval("is_int 1"),          "$true");
        assert_eq!(s_eval("is_float 1.2"),      "$true");
        assert_eq!(s_eval("is_fun {}"),         "$true");
    }

    #[test]
    fn check_len_fun() {
        assert_eq!(s_eval("len $[]"),            "0");
        assert_eq!(s_eval("len $[1,2,3]"),       "3");
        assert_eq!(s_eval("len ${a=1,b=20}"),    "2");
        assert_eq!(s_eval("len ${}"),            "0");
        assert_eq!(s_eval("str:len ${}"),        "3");
        assert_eq!(s_eval("len $q abcdef "),     "6");
        assert_eq!(s_eval("len $q abcüdef "),    "8");
        assert_eq!(s_eval("str:len $q abcüdef "),"7");
        assert_eq!(s_eval("len $Q abcdef "),     "6");
        assert_eq!(s_eval("len $Q abcüdef "),    "8");
        assert_eq!(s_eval("str:len $Q abcüdef "),"8");
    }

    #[test]
    fn check_lst_map() {
        assert_eq!(s_eval("$[12,1,30] \\_ * 2"),        "$[24,2,60]");
        assert_eq!(s_eval("$[12,1,304] str:len"),       "$[2,1,3]");
        assert_eq!(s_eval("$[123,22,4304] str:len"),    "$[3,2,4]");
        assert_eq!(s_eval("$[123,22,4304] str:len | fold 1 \\_ * _1"), "24");
    }

    #[test]
    fn check_prelude_assert() {
        assert_eq!(s_eval("wl:assert ~ (type \"2019\".(int)) == $q int "), "$true");
    }

    #[test]
    fn check_prelude_str() {
        assert_eq!(s_eval("str:to_uppercase $q foo "), "\"FOO\"");
        assert_eq!(s_eval("str:to_lowercase $q FOO "), "\"foo\"");
        assert_eq!(s_eval("str:join \",\" $[1,2,3,${a=:x}]"), "\"1,2,3,${a=:\\\"x\\\"}\"");
        assert_eq!(s_eval("str:cat $[1,2,3,${a=:x}]"), "\"123${a=:\\\"x\\\"}\"");
    }

    #[test]
    fn check_prelude_chrono() {
        if cfg!(feature="chrono") {
            assert_eq!(s_eval("chrono:timestamp $q$%Y$ | int"), "2019");
        }
    }

    #[test]
    fn check_prelude_regex() {
        if cfg!(feature="regex") {
            assert_eq!(s_eval("$q$fofoaaaaofefoeaafefeoaaaa$ | re:map $q{(a+)} { _.1 } | str:join $q$,$"),
                       "\"aaaa,aa,aaaa\"");
            assert_eq!(s_eval("
                $q$fofoaaaofefoeaaaaafefeoaaaaaaa$
                | re:map $q{(a+)} { str:len _.1 }
                | fold 1 \\_ * _1"),
                "105");

            assert_eq!(s_eval_no_panic("
                re:replace_all $q/ar/ { \"mak\" } $q/foobarbarfoobararar/
            "),
            "$e \"EXEC ERR: Caught Panic(Str(RefCell { value: \\\"function[2:39/0] expects at most 0 arguments, got 1\\\" }), Some([SynPos { syn: Call, line: 2, col: 32, file: 0 }]))\"");
            assert_eq!(s_eval("
                re:replace_all $q/a+r/ { str:cat \"mak\" ~ str:len _.0 } $q/foobarbaaaarfoobaararar/
            "),
            "\"foobmak2bmak5foobmak3mak2mak2\"");
            assert_eq!(s_eval("
                re:replace_all $q/a+r/
                    {
                        (str:len[_.0] == 3) {
                            break \"XX\"
                        };
                        (str:cat \"<\" _.0 \">\")
                    }
                    $q/foobarbaaaarfoobaararar/
            "),
            "\"foob<ar>b<aaaar>foobXXarar\"");
            assert_eq!(s_eval("
                re:replace_all $q/a+r/
                    {
                        (str:len[_.0] == 3) { next[] };
                        (str:cat \"<\" _.0 \">\")
                    }
                    $q/foobarbaaaarfoobaararar/
            "),
            "\"foob<ar>b<aaaar>foobaar<ar><ar>\"");
        }
    }

    #[test]
    fn check_json() {
        if cfg!(feature="serde_json") {
            assert_eq!(s_eval("ser:json $[1,1.2,$f,$t,$n,${a=1}]"), "\"[\\n  1,\\n  1.2,\\n  false,\\n  true,\\n  null,\\n  {\\n    \\\"a\\\": 1\\n  }\\n]\"");
            assert_eq!(s_eval("ser:json $[1,1.2,$f,$t,$n,${a=1}] $t"), "\"[1,1.2,false,true,null,{\\\"a\\\":1}]\"");
            assert_eq!(s_eval("deser:json $q$[1,2.3,true,null,{\"a\":10}]$"), "$[1,2.3,$true,$n,${a=10}]");
        }
    }

    #[test]
    fn check_msgpack() {
        if cfg!(feature="rmp-serde") {
            assert_eq!(s_eval("deser:msgpack ~ ser:msgpack $[1,1.2,$f,$t,$n,${a=1},\"abcä\",$b\"abcä\"]"),
                       "$[1,1.2,$false,$true,$n,${a=1},\"abcä\",$b\"abc\\xC3\\xA4\"]");
            assert_eq!(s_eval("ser:msgpack $b\"abc\""), "$b\"\\xC4\\x03abc\"");
            assert_eq!(s_eval("ser:msgpack $[1,$n,16.22]"), "$b\"\\x93\\x01\\xC0\\xCB@08Q\\xEB\\x85\\x1E\\xB8\"");
            assert_eq!(s_eval("deser:msgpack $b\"\\xC4\\x03abc\""), "$b\"abc\"");
            assert_eq!(s_eval("deser:msgpack $b\"\\x93\\x01\\xC0\\xCB@08Q\\xEB\\x85\\x1E\\xB8\""), "$[1,$n,16.22]");
        }
    }

    #[test]
    fn check_eval() {
        let mut ctx = EvalContext::new_default();

        assert_eq!(ctx.eval("wl:eval $q$1 + 2$").unwrap().s(), "3");

        ctx.set_global_var("XXX", &VVal::Int(1337));
        assert_eq!(ctx.eval("wl:eval $q$XXX + 2$").unwrap().s(), "1339");

        assert_eq!(ctx.eval("wl:eval $q/wl:eval $q$XXX + 2$/").unwrap().s(), "1339");
    }

    #[test]
    fn check_userdata() {
        use std::rc::Rc;
        use std::cell::RefCell;
        let global_env = GlobalEnv::new_default();

        #[derive(Clone, Debug)]
        struct MyType {
            x: Rc<RefCell<(i64, i64)>>,
        }

        impl crate::vval::VValUserData for MyType {
            fn s(&self) -> String { format!("$<MyType({:?})>", self.x.borrow()) }
            fn i(&self) -> i64    { self.x.borrow_mut().1 }
            fn as_any(&mut self) -> &mut dyn std::any::Any { self }
            fn clone_ud(&self) -> Box<dyn crate::vval::VValUserData> {
                Box::new(self.clone())
            }
        }

        global_env.borrow_mut().add_func(
            "new_mytype",
            |_env: &mut Env, _argc: usize| {
                Ok(VVal::Usr(Box::new(MyType { x: Rc::new(RefCell::new((13, 42))) })))
            }, Some(0), Some(0));

        global_env.borrow_mut().add_func(
            "modify_mytype",
            |env: &mut Env, _argc: usize| {
                Ok(if let VVal::Usr(mut u) = env.arg(0) {
                    if let Some(ud) = u.as_any().downcast_mut::<MyType>() {
                        ud.x.borrow_mut().0 += 1;
                        ud.x.borrow_mut().1 *= 2;
                        VVal::Int(ud.x.borrow().0 + ud.x.borrow().1)
                    } else {
                        VVal::Nul
                    }
                } else { VVal::Nul })
            }, Some(1), Some(1));

        let mut ctx = crate::compiler::EvalContext::new(global_env);

        let r = &mut ctx.eval(r#"
            !x = new_mytype[];
            !i = modify_mytype x;
            $[i, x]
        "#).unwrap();

        assert_eq!(
            r.s(), "$[98,$<MyType((14, 84))>]", "Userdata implementation works");
    }

    #[test]
    fn check_bytes_impl() {
        assert_eq!(s_eval("ser:json $b\"abc\""),                         "\"[\\n  97,\\n  98,\\n  99\\n]\"", "JSON serializer for bytes ok");
        assert_eq!(s_eval("str $b\"abc\""),                              "\"abc\"", "Bytes to String by 1:1 Byte to Unicode Char mapping");
        assert_eq!(s_eval("str $b\"äbcß\""),                             "\"Ã¤bcÃ\\u{9f}\"", "Bytes to String by 1:1 Byte to Unicode Char mapping");
        assert_eq!(s_eval("str:from_utf8 $b\"äbcß\""),                   "\"äbcß\"", "Bytes to String from UTF8");
        assert_eq!(s_eval("str:from_utf8 $b\"\\xC4\\xC3\""),             "$e \"str:from_utf8 decoding error: invalid utf-8 sequence of 1 bytes from index 0\"", "Bytes to String from invalid UTF8");
        assert_eq!(s_eval("str:from_utf8_lossy $b\"\\xC4\\xC3\""),       "\"��\"", "Bytes to String from invalid UTF8 lossy");
        assert_eq!(s_eval("str:to_bytes \"aäß\""),                       "$b\"a\\xC3\\xA4\\xC3\\x9F\"", "Bytes from String as UTF8");
        assert_eq!(s_eval("str:from_utf8 ~ str:to_bytes \"aäß\""),       "\"aäß\"", "Bytes from String as UTF8 into String again");
        assert_eq!(s_eval("$b\"abc\" 1"),                                "$b\"b\"", "Get single byte from bytes");
        assert_eq!(s_eval("$b\"abcdef\" 0 2"),                           "$b\"ab\"", "Substring bytes operation");
        assert_eq!(s_eval("$b\"abcdef\" 3 3"),                           "$b\"def\"", "Substring bytes operation");
        assert_eq!(s_eval("$b\"abcdef\" $[3, 3]"),                       "$b\"def\"", "Substring bytes operation");
        assert_eq!(s_eval("$b\"abcdef\" $[3]"),                          "$b\"def\"", "Substring bytes operation");
        assert_eq!(s_eval("$b\"abcdef\" ${abcdef = 10}"),                "10", "Bytes as map key");
        assert_eq!(s_eval("bytes:to_vec $b\"abcdef\""),                  "$[97,98,99,100,101,102]", "bytes:to_vec");
        assert_eq!(s_eval("bytes:from_vec ~ bytes:to_vec $b\"abcdef\""), "$b\"abcdef\"", "bytes:from_vec");
        assert_eq!(s_eval("bytes:from_vec $[]"),                         "$b\"\"", "bytes:from_vec");
        assert_eq!(s_eval("bytes:from_vec $[1,2,3]"),                    "$b\"\\x01\\x02\\x03\"", "bytes:from_vec");

        assert_eq!(s_eval("bytes:to_hex $b\"abc\\xFF\""),                  "\"616263FF\"");
        assert_eq!(s_eval("bytes:to_hex $b\"abc\\xFF\" 6"),                "\"616263 FF\"");
        assert_eq!(s_eval("bytes:to_hex $b\"abc\\xFF\" 6 \":\""),          "\"616263:FF\"");
        assert_eq!(s_eval("bytes:to_hex $b\"abc\\xFF\" 1 \":\""),          "\"6:1:6:2:6:3:F:F\"");

        assert_eq!(s_eval("bytes:from_hex ~ bytes:to_hex $b\"abc\\xFF\""),         "$b\"abc\\xFF\"");
        assert_eq!(s_eval("bytes:from_hex ~ bytes:to_hex $b\"abc\\xFF\" 6"),       "$b\"abc\\xFF\"");
        assert_eq!(s_eval("bytes:from_hex ~ bytes:to_hex $b\"abc\\xFF\" 6 \":\""), "$b\"abc\\xFF\"");
        assert_eq!(s_eval("bytes:from_hex ~ bytes:to_hex $b\"abc\\xFF\" 1 \":\""), "$b\"abc\\xFF\"");
        assert_eq!(s_eval("bytes:from_hex ~ bytes:to_hex $b\"\\x00abc\\xFF\" 1 \":\""), "$b\"\\0abc\\xFF\"");
    }

    #[test]
    fn check_ref() {
        assert_eq!(s_eval("!x = $&&1; $*x"),             "1");
        assert_eq!(s_eval("!x = $&&1; .*x = 2; $*x"),    "2");
        assert_eq!(s_eval("!(x, y) = $[$&&1, $&&2]; $[$*x, $*y]"), "$[1,2]");
        assert_eq!(s_eval("!(x, y) = $[$&&1, $&&2]; $[x, y]"), "$[$&&1,$&&2]");
        assert_eq!(s_eval("!(x, y) = $[$&&1, $&&2]; .*(x, y) = $[33, 34]; $[x, y]"), "$[$&&33,$&&34]");
        assert_eq!(s_eval(r#"
                !:global (x, y) = $[$&&1, $&&2];
                .*(x, y) = $[33, 34];
                $[x, y]
            "#), "$[$&&33,$&&34]");
        assert_eq!(s_eval(r#"
                !(x, y) = $[$&&1, $&&2];
                !z = wl:weaken y;
                !f = { .*x = $*x + 1; .*z = $*z + 2; $[x, z] };
                !r = $[];
                push r ~ str f[];
                .x = $n;
                .y = $n;
                push r ~ str f[];
                $[r, z]
            "#), "$[$[\"$[$&&2,$(&)4]\",\"$[$&&3,$n]\"],$n]");
        assert_eq!(s_eval(r#"
            !self = $&&${};
            !wself = wl:weaken self;
            self.x = { wself.g = 10; wself.g };
            self.y = { wself.g * 10 };
            !r = $[];
            push r self.x[];
            push r self.y[];
            !f = self.y;
            .self = $n;
            push r f[];
            push r wself;
            r
        "#), "$[10,100,0,$n]");
    }
}
