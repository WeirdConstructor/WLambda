// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Implements a type checker for WLambda, which is an extra compile pass over the AST.
The AST as it comes directly from the Parser. The type checker will also augment the AST with
type information of course.
*/

use crate::vval::CompileError;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ops::BinOp;
use crate::compiler::CompileEnv;
use crate::vval::Syntax;
use crate::vval::Type;
use crate::vval::VVal;
//
//pub(crate) fn type_pass(
//    ast: &VVal,
//    type_hint: Rc<Type>,
//    ce: &mut Rc<RefCell<CompileEnv>>,
//) -> Result<VVal, CompileError>;

fn type_var(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    capt_ref: bool,
) -> Result<VVal, CompileError> {
    let syn = ast.at(0).unwrap_or(VVal::None);
    let spos = syn.get_syn_pos();

    let var = ast.at(1).unwrap();
    var.with_s_ref(|var_s: &str| -> Result<VVal, CompileError> {
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
                let pos = ce.borrow_mut().get(var_s);
            }
        }
        Ok(VVal::None)
    })?;

    Ok(ast.clone())
}


fn type_binop(
    ast: &VVal,
    op: BinOp,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<VVal, CompileError> {
    let (syn, a, b) = (ast.v_(0), ast.v_(1), ast.v_(2));

    Ok(ast.clone())
}

fn type_block(
    ast: &VVal,
    skip_cnt: usize,
    type_hint: Rc<Type>,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<VVal, CompileError> {
    let stmts = ast.map_skip_vval(
        |e, is_last| type_pass(e, if is_last { type_hint.clone() } else { Type::any() }, ce),
        skip_cnt,
    )?;
    Ok(stmts)
}

fn type_def(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_global: bool,
) -> Result<VVal, CompileError> {
    let (vars, value, destr, types) = (ast.v_(1), ast.v_(2), ast.v_(3), ast.v_(4));

    if destr.b() {
        panic!("CANT DO THIS!");
    } else {
        let varname = vars.v_s_raw(0);
        ce.borrow_mut().recent_var = varname.clone();

        type_pass(&value, types.v_(0).t(), ce)?;

//        let val_pw = compile(&value, ce)?;
    }

    println!("Vars: {}, Types: {}", vars.s(), types.s());

    Ok(ast.clone())
}

/// Runs the type checker pass over the AST.
/// TODO: Do we do preparative steps for the compiler/code generator?
#[allow(clippy::cognitive_complexity)]
pub(crate) fn type_pass(
    ast: &VVal,
    type_hint: Rc<Type>,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<VVal, CompileError> {
    match ast {
        VVal::Lst(_) => {
            let syn = ast.at(0).unwrap_or(VVal::None).get_syn();
            let v = match syn {
                Syntax::Block => {
                    let node = type_block(ast, 1, type_hint, ce)?;
                    node.unshift(ast.v_(0));
                    node
                }
                Syntax::BinOpAdd => type_binop(ast, BinOp::Add, ce)?,
                Syntax::BinOpSub => type_binop(ast, BinOp::Sub, ce)?,
                Syntax::BinOpDiv => type_binop(ast, BinOp::Div, ce)?,
                Syntax::BinOpMod => type_binop(ast, BinOp::Mod, ce)?,
                Syntax::BinOpMul => type_binop(ast, BinOp::Mul, ce)?,
                Syntax::BinOpGe => type_binop(ast, BinOp::Ge, ce)?,
                Syntax::BinOpGt => type_binop(ast, BinOp::Gt, ce)?,
                Syntax::BinOpLe => type_binop(ast, BinOp::Le, ce)?,
                Syntax::BinOpLt => type_binop(ast, BinOp::Lt, ce)?,
                Syntax::BinOpEq => type_binop(ast, BinOp::Eq, ce)?,
                Syntax::Var => type_var(ast, ce, false)?,
                Syntax::Def => type_def(ast, ce, false)?,
                //                Syntax::DefGlobRef => compile_def(ast, ce, true),
                _ => return Err(ast.compile_err(format!("type checker got unknown input: {}", ast.s()))),
            };
            Ok(v)
        }
        _ => {
            println!("AST IN {:?}", ast.s());
            Ok(ast.clone())
        }
    }
}