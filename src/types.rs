// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Implements a type checker for WLambda, which is an extra compile pass over the AST.
The AST as it comes directly from the Parser. The type checker will also augment the AST with
type information of course.
*/

use std::rc::Rc;
use std::cell::RefCell;
use crate::vval::CompileError;

//use crate::vval::Syntax;
use crate::vval::VVal;
use crate::vval::Type;
use crate::compiler::CompileEnv;

/// Runs the type checker pass over the AST.
/// TODO: Do we do preparative steps for the compiler/code generator?
#[allow(clippy::cognitive_complexity)]
pub(crate) fn type_pass(ast: &VVal, type_hint: Rc<Type>, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<VVal, CompileError> {
    match ast {
        _ => {
            println!("AST IN {:?}", ast.s());
            Ok(ast.clone())
        }
    }
}
