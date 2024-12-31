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

    let vars = ast.at(1).unwrap();
    let value = ast.at(2).unwrap();
    let destr = ast.at(3).unwrap_or(VVal::None);
    let types = ast.at(4).unwrap_or(VVal::None);

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
                },
                Syntax::Def => type_def(ast, ce, false)?,
//                Syntax::DefGlobRef => compile_def(ast, ce, true),
                _ => return Err(ast.compile_err(format!("bad input: {}", ast.s()))),
            };
            Ok(v)
        }
        _ => {
            println!("AST IN {:?}", ast.s());
            Ok(ast.clone())
        }
    }
}
