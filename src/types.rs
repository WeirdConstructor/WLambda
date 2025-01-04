// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Implements a type checker for WLambda, which is an extra compile pass over the AST.
The AST as it comes directly from the Parser. The type checker will also augment the AST with
type information of course.
*/

use std::cell::RefCell;
use std::rc::Rc;

use crate::compiler::CompileEnv;
use crate::ops::BinOp;
use crate::vval::{
    resolve_type, CompileError, Syntax, Type, TypeConflictReason, TypeResolveResult, VVal, VarPos,
};
//
//pub(crate) fn type_pass(
//    ast: &VVal,
//    type_hint: Rc<Type>,
//    ce: &mut Rc<RefCell<CompileEnv>>,
//) -> Result<VVal, CompileError>;

#[derive(Debug, Clone)]
pub struct TypedVVal {
    pub vval: VVal,
    pub typ: Rc<Type>,
}

impl TypedVVal {
    pub fn new(typ: Rc<Type>, vval: VVal) -> Self {
        Self { vval, typ }
    }
}

fn type_var(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    capt_ref: bool,
) -> Result<TypedVVal, CompileError> {
    let syn = ast.at(0).unwrap_or(VVal::None);

    let var = ast.at(1).unwrap();
    let mut typ_val = var.with_s_ref(|var_s: &str| -> Result<TypedVVal, CompileError> {
        match var_s {
            "_" => {
                ce.borrow_mut().set_impl_arity(1);
            }
            "_1" => {
                ce.borrow_mut().set_impl_arity(2);
            }
            "_2" => {
                ce.borrow_mut().set_impl_arity(3);
            }
            "_3" => {
                ce.borrow_mut().set_impl_arity(4);
            }
            "_4" => {
                ce.borrow_mut().set_impl_arity(5);
            }
            "_5" => {
                ce.borrow_mut().set_impl_arity(6);
            }
            "_6" => {
                ce.borrow_mut().set_impl_arity(7);
            }
            "_7" => {
                ce.borrow_mut().set_impl_arity(8);
            }
            "_8" => {
                ce.borrow_mut().set_impl_arity(9);
            }
            "_9" => {
                ce.borrow_mut().set_impl_arity(10);
            }
            "@" => {
                ce.borrow_mut().set_impl_arity_infinite();
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
                if capt_ref {
                    return Ok(TypedVVal::new(Type::ref_type(typ), ast.clone()));
                } else {
                    return Ok(TypedVVal::new(typ, ast.clone()));
                }
            }
        }
        Ok(TypedVVal::new(Type::rc_new_var(&format!("V{}_", var_s)), ast.clone()))
    })?;
    if capt_ref {
        typ_val.typ = Rc::new(Type::Ref(typ_val.typ));
    }
    Ok(typ_val)
}

fn type_binop(
    ast: &VVal,
    op: BinOp,
    type_hint: Rc<Type>,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let (syn, a, b) = (ast.v_(0), ast.v_(1), ast.v_(2));
    //    println!("SYN: {:?}", ce.borrow_mut().get("+"));
    // TODO: Get Type of BinOp from Environment by `syn`.
    let op_type = match ce.borrow_mut().get_type("+") {
        Some(t) => t,
        None => return Err(ast.compile_err(format!("Unknown type of operator: {}", ast.s()))),
    };
    //    if !op_type.is_resolved() {
    //        return Err(ast.compile_err(format!("Operator has unknown type: {}", op_type.s(),)));
    //    }
    //d// println!("TYPE: {:?}", op_type);

    // TODO: Get union type from first argument!
    let a_type = type_pass(&a, Type::any(), ce)?;

    // TODO: Get union type from second argument!
    let b_type = type_pass(&b, Type::any(), ce)?;

    // TODO: Maybe we should also pass a second type hint down, one which limits the
    //       possible return types?
    let chk_typ = Type::fun_2_ret((*a_type.typ).clone(), (*b_type.typ).clone(), Type::Any);

    let mut bound_vars = vec![];
    let res = resolve_type(&op_type, &chk_typ, &mut bound_vars, &mut |name| {
        ce.borrow_mut().get_type(name)
    });
    let operation_typ = match res {
        TypeResolveResult::Match { typ } => typ,
        TypeResolveResult::Conflict { expected, got, reason } => {
            return Err(ast.compile_err(format!(
                "Type error, expected:\n {}\ngot: {}\n=> reason: {}",
                expected.s(),
                got.s(),
                reason
            )));
        }
    };

    if let Type::Function(_args, ret_type, _bound_vars) = operation_typ.as_ref() {
        eprintln!("RetType: {}", ret_type.s());
        Ok(TypedVVal::new(ret_type.clone(), ast.clone()))
    } else {
        Err(ast.compile_err(format!(
            "Expected function type from operator {}, got: {}",
            syn.s(),
            operation_typ.s()
        )))
    }
}

fn type_block(
    ast: &VVal,
    skip_cnt: usize,
    type_hint: Rc<Type>,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let mut last_type = type_hint.clone();
    let stmts = ast.map_skip_vval(
        |e, is_last| {
            let tv = type_pass(e, if is_last { type_hint.clone() } else { Type::any() }, ce)?;
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
        if let VVal::Lst(b, _) = vars.clone() {
            for (i, v) in b.borrow().iter().enumerate() {
                ce.borrow_mut().def(&v.s_raw(), is_global, types.v_(i).t());
            }
        }
    } else {
        let varname = vars.v_s_raw(0);
        ce.borrow_mut().recent_var = varname.clone();

        let var_typ = types.v_(0).t();

        if !var_typ.is_resolved() {
            return Err(ast.compile_err(format!(
                "Can't define variable with unresolved type, variable '{}', unresolved type: {}",
                varname,
                var_typ.s(),
            )));
        }

        if is_global {
            let tv = type_pass(&value, var_typ.clone(), ce)?;
            let typ = tv.typ;

            if !typ.is_resolved() {
                return Err(ast.compile_err(format!(
                    "Can't assign value with unresolved type to variable '{}', unresolved value type: {}",
                    varname,
                    typ.s(),
                )));
            }

            if !typ.isa_resolved(&var_typ) {
                return Err(ast.compile_err(format!(
                    "Can't assign type {} to variable {} of type {}",
                    typ.s(),
                    varname,
                    var_typ.s(),
                )));
            }

            if let VarPos::Global(r) = ce.borrow_mut().def(&varname, true, var_typ) {
                // everything is fine!
            } else {
                panic!("Defining global did not return a global!");
            }
        } else {
            let next_local = ce.borrow_mut().next_local();
            let tv = type_pass(&value, var_typ.clone(), ce)?;
            let typ = tv.typ;

            if !typ.is_resolved() {
                return Err(ast.compile_err(format!(
                    "Can't assign value with unresolved type to variable '{}', unresolved value type: {}",
                    varname,
                    typ.s(),
                )));
            }

            if !typ.isa_resolved(&var_typ) {
                return Err(ast.compile_err(format!(
                    "Can't assign type {} to variable {} of type {}",
                    typ.s(),
                    varname,
                    var_typ.s(),
                )));
            }

            ce.borrow_mut().def_local(&varname, next_local, typ);
        }
    }

    println!("Vars: {}, Types: {}", vars.s(), types.s());

    Ok(TypedVVal::new(Type::any(), ast.clone()))
}

fn type_deftype(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_global: bool,
) -> Result<TypedVVal, CompileError> {
    let (typename, typ) = (ast.v_s_raw(1), ast.v_(2));

    if is_global {
        if let VarPos::Global(r) = ce.borrow_mut().def(&typename, true, typ.t()) {
            r.set_ref(typ);
        } else {
            panic!("Defining global did not return a global!");
        }
    } else {
        let next_local = ce.borrow_mut().next_local();
        ce.borrow_mut().def_local(&typename, next_local, typ.t());
    }

    Ok(TypedVVal::new(Type::typ(), ast.clone()))
}

/// Runs the type checker pass over the AST.
/// TODO: Do we do preparative steps for the compiler/code generator?
#[allow(clippy::cognitive_complexity)]
pub(crate) fn type_pass(
    ast: &VVal,
    type_hint: Rc<Type>,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    match ast {
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
                //                Syntax::DefGlobRef => compile_def(ast, ce, true),
                _ => {
                    return Err(
                        ast.compile_err(format!("type checker got unknown input: {}", ast.s()))
                    )
                }
            };
            Ok(v)
        }
        _ => Ok(TypedVVal::new(ast.t(), ast.clone())),
    }
}

pub(crate) fn type_check(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    types_enabled: bool,
) -> Result<VVal, CompileError> {
    match type_pass(ast, Type::any(), ce) {
        Ok(tv) => Ok(ast.clone()),
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
