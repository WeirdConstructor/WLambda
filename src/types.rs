// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Implements a type checker for WLambda, which is an extra compile pass over the AST.
The AST as it comes directly from the Parser. The type checker will also augment the AST with
type information of course.
*/

use std::cell::RefCell;
use std::rc::Rc;

use crate::compiler::{fetch_object_key_access, CompileEnv};
use crate::ops::BinOp;
use crate::vval::{
    bind_free_vars, resolve_type, CompileError, Syntax, Type, TypeResolveResult, VVal, VarPos,
};

#[derive(Debug, Clone)]
pub(crate) enum TypeHint {
    DontCare,
    Expect(Rc<Type>),
}

impl TypeHint {
    fn from_type(ty: &Rc<Type>) -> Self {
        if ty.is_none() {
            TypeHint::DontCare
        } else {
            TypeHint::Expect(ty.clone())
        }
    }

    fn expect(&self) -> Option<&Type> {
        if let TypeHint::Expect(ty) = self {
            Some(ty.as_ref())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TypedVVal {
    pub vval: VVal,
    pub typ: Rc<Type>,
}

impl TypedVVal {
    pub fn new(typ: Rc<Type>, vval: VVal) -> Self {
        Self { vval, typ }
    }
}

impl std::fmt::Display for TypedVVal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypedVVal vval={}, typ={}", self.vval, self.typ)
    }
}

fn type_var(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    capt_ref: bool,
) -> Result<TypedVVal, CompileError> {
    let var = ast.at(1).unwrap();
    let cur_func_type =
        ce.borrow().current_function().cloned().unwrap_or_else(|| Type::fun_ret(Type::None));
    let mut typ_val = var.with_s_ref(|var_s: &str| -> Result<TypedVVal, CompileError> {
        match var_s {
            "_" => {
                ce.borrow_mut().set_impl_arity(1);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(0), ast.clone()));
            }
            "_1" => {
                ce.borrow_mut().set_impl_arity(2);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(1), ast.clone()));
            }
            "_2" => {
                ce.borrow_mut().set_impl_arity(3);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(2), ast.clone()));
            }
            "_3" => {
                ce.borrow_mut().set_impl_arity(4);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(3), ast.clone()));
            }
            "_4" => {
                ce.borrow_mut().set_impl_arity(5);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(4), ast.clone()));
            }
            "_5" => {
                ce.borrow_mut().set_impl_arity(6);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(5), ast.clone()));
            }
            "_6" => {
                ce.borrow_mut().set_impl_arity(7);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(6), ast.clone()));
            }
            "_7" => {
                ce.borrow_mut().set_impl_arity(8);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(7), ast.clone()));
            }
            "_8" => {
                ce.borrow_mut().set_impl_arity(9);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(8), ast.clone()));
            }
            "_9" => {
                ce.borrow_mut().set_impl_arity(10);
                return Ok(TypedVVal::new(cur_func_type.get_parameter_type(9), ast.clone()));
            }
            "@" => {
                ce.borrow_mut().set_impl_arity_infinite();
                return Ok(TypedVVal::new(cur_func_type.get_parameter_tuple(), ast.clone()));
                // TODO: @ type is a record/typed array, that is determined by the
                //       function this code resides in. We can set the arity here,
                //       implicitly, but to determine the type we need to check the function.
                // XXX: It might not be possible to do this without inference. We don't want to give
                //      an explicit function type to everything.
            }
            _ => {
                let (pos, typ) = ce.borrow_mut().get(var_s);
                if let VarPos::NoPos = pos {
                    return Err(ast.compile_err(format!("Variable '{}' undefined", var_s)));
                }
                println!("GET VAR TYPE {} => {}", var_s, typ.s());
                if capt_ref {
                    return Ok(TypedVVal::new(Type::ref_type(typ), ast.clone()));
                } else {
                    return Ok(TypedVVal::new(typ, ast.clone()));
                }
            }
        }
    })?;
    if capt_ref {
        typ_val.typ = Rc::new(Type::Ref(typ_val.typ));
    }
    Ok(typ_val)
}

fn type_binop(
    ast: &VVal,
    op: BinOp,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let (syn, a, b) = (ast.v_(0), ast.v_(1), ast.v_(2));
    let op_type = match ce.borrow_mut().get_type(op.token()) {
        Some(t) => t,
        None => return Err(ast.compile_err(format!("Unknown type of operator: {}", ast.s()))),
    };
    println!("BINOP: {} => {:?}", ast, op_type);

    let a_type = type_pass(&a, TypeHint::DontCare, ce)?;
    let b_type = type_pass(&b, TypeHint::DontCare, ce)?;

    let new_ast = VVal::vec3(syn, a_type.vval, b_type.vval);

    let (op_args, op_ret) = op_type.fun_split_ret();

    // Now synthesize the arguments and check their type against the operation's arguments:
    let synth_args = Type::fun_2("", (*a_type.typ).clone(), "", (*b_type.typ).clone());

    if let TypeHint::Expect(ret_type) = type_hint {
        // In this case, we can bind the return type before the argument types.
        // That will bind the variables in the function arguments by the return type.
        let mut bound_vars = vec![];
        println!("OOOOOOOOOOOBOUNDBOUND: {} - {}", ret_type, op_ret);
        // TODO: The following only works properly if the ret_type=Any (or generally more generic?)
        //       it does not work if ret_type=int and op_ret=<N is @Num> - because of the
        //       union subtype matching done here.
        //
        //       Maybe we need to run it twice, in both directions?
        println!("11111111111111111111111111111111111111111111");
        if let Ok(ret_type) =
            resolve_and_check_bound(&ret_type, &op_ret, &mut bound_vars, ast, || {
                format!("operator call '{}' with expected return type {}", op.token(), ret_type)
            })
        {
        println!("§BODUD2: {:?}", bound_vars);
            if let Ok(synth_type) =
                resolve_and_check_bound(&op_args, &synth_args, &mut bound_vars, ast, || {
                    format!("operator call '{}' with return type {}", op.token(), ret_type)
                })
            {
                println!("Result type: {}", synth_args);
                return Ok(TypedVVal::new(ret_type, new_ast));
            }
        }

        bound_vars = vec![];
        println!("222222222222222222222222222222222222222222222222222");
        let ret_type = resolve_and_check_bound(&op_ret, &ret_type, &mut bound_vars, ast, || {
            format!("operator call '{}' with expected return type {}", op.token(), ret_type)
        })?;
        println!("§BODUD: {:?}", bound_vars);
        let synth_type =
            resolve_and_check_bound(&op_args, &synth_args, &mut bound_vars, ast, || {
                format!("operator call '{}' with return type {}", op.token(), ret_type)
            })?;
        println!("Result2 type: {}", synth_args);
        return Ok(TypedVVal::new(ret_type, new_ast));

        return Err(ast.compile_err(format!("Can't infer return type at: {}", ast.s())));
    } else {
        // We have no clue what the return type should be.
        // So we try to infer it by synthesizing a function with the same return type as the operation.
        // And then let the variable binding take care of the rest.
        let synth_fun = Type::fun_2_ret(
            "",
            (*a_type.typ).clone(),
            "",
            (*b_type.typ).clone(),
            Type::unknown_typ("ret_type"),
        );
        let op_fun_type = resolve_and_check(&op_type, &synth_fun, ast, || {
            format!("operator call '{}'", op.token())
        })?;

        Ok(TypedVVal::new(
            op_fun_type.get_return_type().clone().unwrap_or_else(|| Type::unknown("opret")),
            new_ast,
        ))
    }
}

fn type_func(
    ast: &VVal,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let (label, arity_type) = (ast.v_(1), ast.v_(2));

    let fun_type = if arity_type.is_type() {
        arity_type.t()
    } else {
        match &type_hint {
            TypeHint::Expect(typ) => typ.clone(),
            _ => Type::unknown("fun"),
        }
    };

    let ret_type = fun_type.get_return_type().unwrap_or_else(|| Type::unknown("funret"));

    // TODO: Setup a function environment, holding the positional parameter types
    //       and their names maybe?
    println!("FUN_TYPE: {:?}, RET_TYPE: {:?}", fun_type, ret_type);

    ce.borrow_mut().push_function(fun_type.clone());
    let mut last_type = (VVal::None, Type::none());
    println!("AST: {}", ast.s());
    let stmts = ast.map_skip_vval(
        |e, is_last| {
            let type_hint =
                if is_last { TypeHint::from_type(&ret_type) } else { TypeHint::DontCare };
            println!("COMP {}: {}", is_last, e);

            let cur_func_type = ce
                .borrow()
                .current_function()
                .cloned()
                .unwrap_or_else(|| Type::fun_ret(Type::None));
            println!("CUR FUNCTIONI: {}", cur_func_type);
            let mut tv = type_pass(e, type_hint.clone(), ce)?;
            println!("COMP {}: {} => {}", is_last, e, tv);

            if is_last {
                if let TypeHint::Expect(ty) = &type_hint {
                    println!("CHECK RET: {}", e);
                    let res_ty = resolve_and_check(&ty, &tv.typ, e, || {
                        format!("last statement of function block")
                    })?;
                    tv.typ = res_ty;
                }
            }

            last_type = (e.clone(), tv.typ);
            Ok(tv.vval)
        },
        3,
    )?;
    ce.borrow_mut().pop_function();

    println!("LAST TYPE: {}, FUN_TYPE: {}, RET_TYPE: {}", last_type.1, fun_type, ret_type);

    stmts.unshift(arity_type.clone());
    stmts.unshift(label.clone());
    stmts.unshift(ast.v_(0));
    Ok(TypedVVal::new(fun_type, stmts))

    // let mut fun_spos = spos.clone();
    // fun_spos.set_name(&ce.borrow().recent_var);
    //
    // let mut func_ce = CompileEnv::create_env(Some(ce.clone()));
    // let ce_sub = func_ce.clone();
    //
    // let label = ast.at(1).unwrap();
    // let explicit_arity = ast.at(2).unwrap();
    //
    // let mut func_prog = Prog::new();
    //
    // let func_pw = compile_stmts(ast, 3, &mut func_ce)?;
    // func_pw.eval_to(&mut func_prog, ResPos::Value(ResValue::Ret));
    // func_prog.op_end();
    //
    // let func_prog = Rc::new(func_prog);
    //
    // let func_typ = if explicit_arity.is_type() {
    //     ce_sub.borrow_mut().explicit_arity.0 = ArityParam::Undefined;
    //     ce_sub.borrow_mut().explicit_arity.1 = ArityParam::Undefined;
    //     explicit_arity.t()

    //    Ok(TypedVVal::new(Type::none(), ast.clone()))
}

fn type_call(
    ast: &VVal,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    if let Some((_syntax, _object, _key)) = fetch_object_key_access(&ast.at(1).unwrap()) {
        panic!("Unimplemented GetKey/GetSym! {}", ast.s());
    }

    let symbol = if let Syntax::Var =
        ast.at(1).unwrap_or(VVal::None).at(0).unwrap_or(VVal::None).get_syn()
    {
        let var = ast.at(1).unwrap().at(1).unwrap();
        Some(var.s_raw())
    } else {
        None
    };

    if let Some(sym) = symbol.as_ref() {
        match &sym[..] {
            "?" => panic!("unimpl"),     // return compile_if(ast, ce),
            "if" => panic!("unimpl"),    // return compile_if(ast, ce),
            "while" => panic!("unimpl"), // return compile_while(ast, ce),
            "iter" => panic!("unimpl"),  // return compile_iter(ast, ce),
            "next" => panic!("unimpl"),  // return compile_next(ast, ce),
            "break" => panic!("unimpl"), // return compile_break(ast, ce),
            "match" => panic!("unimpl"), // return compile_match(ast, ce),
            "jump" => panic!("unimpl"),  // return compile_jump(ast, ce),
            _ => (),
        }
    }

    let ret_type = match type_hint {
        TypeHint::Expect(rt) => rt.clone(),
        _ => Type::unknown("callret"),
    };

    let mut args = vec![];
    let mut fun_arg_types = vec![];
    for (i, (e, _)) in ast.iter().skip(2).enumerate() {
        args.push(e);
        fun_arg_types.push((Type::unknown(&format!("callarg{}", i)), None));
    }
    let synth_fun_type =
        bind_free_vars(&Rc::new(Type::Function(Rc::new(fun_arg_types), ret_type.clone(), None)))
            .expect("properly synthesized function");

    // Synthesize the function type (including return tyep) and pass it as
    // type hint for the return type of the function?
    // If we don't get a function type back, we need to synthesize it though...
    // Maybe do two passes?
    let called_thing = type_pass(&ast.v_(1), TypeHint::from_type(&synth_fun_type), ce)?;
    println!("CALLED THING {}: {}", symbol.unwrap_or(String::from("?")), called_thing.typ);
    let real_fun_type = called_thing.typ.clone();
    let ret_type = real_fun_type.get_return_type().unwrap_or_else(|| Type::unknown("rlfunret"));

    let mut type_checked_args = vec![];
    for (i, e) in args.iter().enumerate() {
        let param_type = real_fun_type.get_parameter_type(i);
        let ty = type_pass(e, TypeHint::from_type(&param_type), ce)?;
        println!("ARG: {:?} has type: {}", e.s(), ty);
        type_checked_args.push(ty);
    }
    // Synthesize the function from the argument types:
    let chk_func = Rc::new(Type::Function(
        Rc::new(type_checked_args.iter().map(|arg| (arg.typ.clone(), None)).collect()),
        ret_type.clone(),
        None,
    ));
    println!("CHK_FUNC: {}", chk_func);
    println!("REAL_FUN: {}", real_fun_type);
    let res_typ = resolve_and_check(&real_fun_type, &chk_func, ast, || format!("function call"))?;
    println!("RESULTING FUNCTION CALL TYPE: {}", res_typ);

    let fun_ast_node = called_thing.vval;
    let new_ast = VVal::vec();
    new_ast.push(ast.v_(0));
    new_ast.push(fun_ast_node);
    //    new_ast.push(VVal::Type(res_typ));
    for arg_node in type_checked_args.iter() {
        new_ast.push(arg_node.vval.clone());
    }

    Ok(TypedVVal::new(ret_type, new_ast))
}

fn type_block(
    ast: &VVal,
    skip_cnt: usize,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let mut last_type = Type::unknown("blk");
    let stmts = ast.map_skip_vval(
        |e, is_last| {
            let mut tv =
                type_pass(e, if is_last { type_hint.clone() } else { TypeHint::DontCare }, ce)?;

            if is_last {
                if let TypeHint::Expect(ty) = &type_hint {
                    let res_ty = resolve_and_check(&ty, &tv.typ, ast, || {
                        format!("last statement of block")
                    })?;
                    tv.typ = res_ty;
                }
            }

            last_type = tv.typ;
            Ok(tv.vval)
        },
        skip_cnt,
    )?;
    Ok(TypedVVal::new(last_type, stmts))
}

fn type_def(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_global: bool,
) -> Result<TypedVVal, CompileError> {
    let (vars, value, destr, types) = (ast.v_(1), ast.v_(2), ast.v_(3), ast.v_(4));

    if destr.b() {
        // TODO // check_for_at_arity(prev_max_arity, ast, ce, &vars);

        if let VVal::Lst(b, _) = vars.clone() {
            for (i, v) in b.borrow().iter().enumerate() {
                ce.borrow_mut().def(&v.s_raw(), is_global, types.v_(i).t());
            }
        }
    } else {
        let varname = vars.v_s_raw(0);
        ce.borrow_mut().recent_var = varname.clone();

        let var_typ = types.v_(0).t();
        println!("VAR TYPE DEF: {} = {}", varname, var_typ.s());

        if is_global {
            let tv = type_pass(&value, TypeHint::from_type(&var_typ), ce)?;
            let _typ = tv.typ;

            // If typ == None, but var_typ != None         => Error
            // If var_typ != None and not(var_typ ISA typ) => Error
            // If var_typ == None and typ == None          => Error
            // If var_typ != None and var_typ ISA typ      => Ok

            if let VarPos::Global(_r) = ce.borrow_mut().def(&varname, true, var_typ) {
                // everything is fine!
            } else {
                panic!("Defining global did not return a global!");
            }
        } else {
            let next_local = ce.borrow_mut().next_local();
            let tv = type_pass(&value, TypeHint::from_type(&var_typ), ce)?;
            println!("TPPPP: {} <=> {}", tv, var_typ);
            let typ = tv.typ;

            let real_var_typ = resolve_and_check(&var_typ, &typ, ast, || {
                format!("variable definition '{}'", &varname)
            })?;
            ce.borrow_mut().def_local(&varname, next_local, real_var_typ);
        }
    }

    println!("Vars: {}, Types: {}", vars.s(), types.s());

    Ok(TypedVVal::new(Type::none(), ast.clone()))
}

fn type_deftype(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_global: bool,
) -> Result<TypedVVal, CompileError> {
    let (typename, typ) = (ast.v_s_raw(1), ast.v_(2));

    let new_typ = if Some('@') == typename.chars().nth(0) {
        Type::aliased(&typename)
    } else {
        Type::named(&typename)
    };

    if is_global {
        if let VarPos::Global(r) = ce.borrow_mut().def(&typename, true, new_typ) {
            r.set_ref(typ);
        } else {
            panic!("Defining global did not return a global!");
        }
    } else {
        let next_local = ce.borrow_mut().next_local();
        ce.borrow_mut().def_local(&typename, next_local, new_typ);
        println!("DEFTYPE {} => {}", typename, typ);
        ce.borrow_mut().set_compiletime_value(&typename, &typ);
    }

    Ok(TypedVVal::new(Type::typ(), ast.clone()))
}

//fn bind_names<F>(
//)
//where
//    F: Fn() -> String
//{
//
//        &mut |name| {
//            let (value, vartype) = ce.borrow_mut().get_compiletime_value(name)?;
//            eprintln!("get comptime {}: {} => {}", name, value, vartype.s());
//            if vartype.is_alias() {
//                Some(value.t())
//            } else if vartype.is_type() {
//                Some(value.t())
//            } else {
//                None
//            }
//        },
//}

fn resolve_and_check_bound<F>(
    typ: &Rc<Type>,
    chk_typ: &Rc<Type>,
    bound_vars: &mut Vec<(usize, Rc<Type>)>,
    ast: &VVal,
    err_cb: F,
) -> Result<Rc<Type>, CompileError>
where
    F: Fn() -> String,
{
    let res = resolve_type(typ, chk_typ, bound_vars, 0);

    match res {
        TypeResolveResult::Match { typ } => Ok(typ),
        TypeResolveResult::Conflict { expected, got, reason } => {
            return Err(ast.compile_err(format!(
                "Type error, expected ({}), but got ({}); in {}\n=> reason: {}",
                expected.s(),
                got.s(),
                err_cb(),
                reason
            )));
        }
        TypeResolveResult::Error { reason } => {
            return Err(ast.compile_err(format!(
                "Error in resolve_type. {}; in {}",
                reason,
                err_cb()
            )));
        }
    }
}

fn resolve_and_check<F>(
    typ: &Rc<Type>,
    chk_typ: &Rc<Type>,
    ast: &VVal,
    err_cb: F,
) -> Result<Rc<Type>, CompileError>
where
    F: Fn() -> String,
{
    let mut bound_vars = vec![];
    resolve_and_check_bound(typ, chk_typ, &mut bound_vars, ast, err_cb)
}

fn type_assign(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    _is_ref: bool, // TODO: Handle is_ref?!?!?
) -> Result<TypedVVal, CompileError> {
    let (vars, value, destr) = (ast.v_(1), ast.v_(2), ast.v_(3));

    if destr.b() {
        // let val_pw = compile(&value, ce)?;

        // TODO // let prev_max_arity = ce.borrow().implicit_arity.clone();
        // TODO // check_for_at_arity(prev_max_arity, ast, ce, &vars);

        // 1. get types for vars
        // 2. get types for values
        // 3. check value types against var types.
        // let poses = vars.map_ok_skip(|v| ce.borrow_mut().get(&v.s_raw()).0, 0);
        Ok(TypedVVal::new(Type::none(), ast.clone()))
    } else {
        let varname = &vars.v_s_raw(0);
        let (pos, var_type) = ce.borrow_mut().get(varname);

        match pos {
            VarPos::Const(_) => {
                return Err(ast.compile_err(format!("Can't assign to constant '{}'", varname)))
            }
            VarPos::NoPos => {
                return Err(ast.compile_err(format!(
                    "Can't assign to undefined local variable '{}'",
                    varname
                )))
            }
            _ => (),
        }

        let value_type = type_pass(&value, TypeHint::from_type(&var_type), ce)?;
        let new_ast = VVal::vec4(ast.v_(0), vars, value_type.vval, destr);

        resolve_and_check(&var_type, &value_type.typ, ast, || {
            format!("assignment to '{}'", &varname)
        })?;

        Ok(TypedVVal::new(Type::none(), new_ast))
    }
}

/// Runs the type checker pass over the AST.
/// TODO: Do we do preparative steps for the compiler/code generator?
#[allow(clippy::cognitive_complexity)]
pub(crate) fn type_pass(
    ast: &VVal,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let tv = match ast {
        VVal::Lst(_, _) => {
            let syn = ast.at(0).unwrap_or(VVal::None).get_syn();
            let v = match syn {
                Syntax::Key => TypedVVal::new(Type::sym(), ast.clone()),
                Syntax::Str => TypedVVal::new(Type::str(), ast.clone()),
                Syntax::Block => {
                    let tv = type_block(ast, 1, type_hint, ce)?;
                    tv.vval.unshift(ast.v_(0));
                    tv
                }
                Syntax::BinOpAdd => type_binop(ast, BinOp::Add, type_hint, ce)?,
                Syntax::BinOpSub => type_binop(ast, BinOp::Sub, type_hint, ce)?,
                Syntax::BinOpDiv => type_binop(ast, BinOp::Div, type_hint, ce)?,
                Syntax::BinOpMod => type_binop(ast, BinOp::Mod, type_hint, ce)?,
                Syntax::BinOpMul => type_binop(ast, BinOp::Mul, type_hint, ce)?,
                Syntax::BinOpGe => type_binop(ast, BinOp::Ge, type_hint, ce)?,
                Syntax::BinOpGt => type_binop(ast, BinOp::Gt, type_hint, ce)?,
                Syntax::BinOpLe => type_binop(ast, BinOp::Le, type_hint, ce)?,
                Syntax::BinOpLt => type_binop(ast, BinOp::Lt, type_hint, ce)?,
                Syntax::BinOpEq => type_binop(ast, BinOp::Eq, type_hint, ce)?,
                Syntax::Var => type_var(ast, ce, false)?,
                Syntax::Def => type_def(ast, ce, false)?,
                Syntax::DefType => type_deftype(ast, ce, false)?,
                Syntax::DefGlobType => type_deftype(ast, ce, true)?,
                Syntax::Assign => type_assign(ast, ce, false)?,
                Syntax::AssignRef => type_assign(ast, ce, true)?,
                Syntax::DumpVM => TypedVVal::new(Type::none(), ast.clone()),
                Syntax::DumpStack => TypedVVal::new(Type::none(), ast.clone()),
                Syntax::Func => type_func(ast, type_hint, ce)?,
                Syntax::Call => type_call(ast, type_hint, ce)?,
                Syntax::GetIdx => {
                    let value_type = type_pass(&ast.v_(1), TypeHint::DontCare, ce)?;
                    let idx = ast.v_i(2);
                    let idx = if idx < 0 { 0 as usize } else { idx as usize };
                    match value_type.typ.type_at(idx) {
                        Some(t) => {
                            TypedVVal::new(t, VVal::vec3(ast.v_(0), value_type.vval, ast.v_(2)))
                        }
                        None => {
                            return Err(ast.compile_err(format!(
                                "Type error, ({}) has no index {}",
                                value_type.typ, idx
                            )))
                        }
                    }
                }
                Syntax::TypeOf => {
                    let expr = ast.v_(1);
                    let res_type = type_pass(&expr, type_hint, ce)?;
                    let new_ast =
                        VVal::vec3(ast.v_(0), res_type.vval, VVal::typ(res_type.typ.clone()));
                    TypedVVal::new(Type::pair(Type::typ(), res_type.typ), new_ast)
                }
                //                Syntax::DefGlobRef => compile_def(ast, ce, true),
                _ => {
                    return Err(
                        ast.compile_err(format!("type checker got unknown input: {}", ast.s()))
                    )
                }
            };
            v
        }
        VVal::Pair(pair) => {
            // TODO: If type_hint is hinting a Pair, then deconstruct that!
            let (th_a, th_b) = if let Some(&Type::Pair(ref atype, ref btype)) = type_hint.expect() {
                (TypeHint::from_type(&atype), TypeHint::from_type(&btype))
            } else {
                (TypeHint::DontCare, TypeHint::DontCare)
            };

            let a = type_pass(&pair.0, th_a, ce)?;
            let b = type_pass(&pair.1, th_b, ce)?;

            TypedVVal::new(Type::pair(a.typ, b.typ), VVal::pair(a.vval, b.vval))
        }
        _ => TypedVVal::new(ast.t(), ast.clone()),
    };

    // if let TypeHint::Expect(expected_typ) = th {
    //     println!("TESTINGXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX {} {}", expected_typ, tv);
    //     let res_ty = resolve_and_check(&expected_typ, &tv.typ, ce, ast, || {
    //         format!("expression: {}", ast.s())
    //     })?;
    //     tv.typ = res_ty;
    // }

    Ok(tv)
}

pub(crate) fn type_check(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    types_enabled: bool,
) -> Result<VVal, CompileError> {
    match type_pass(ast, TypeHint::Expect(Type::any()), ce) {
        Ok(tv) => Ok(tv.vval.clone()),
        Err(e) => {
            if types_enabled {
                Err(e)
            } else {
                eprintln!("TYPE ERROR: {}", e);
                Ok(ast.clone())
            }
        }
    }
}
