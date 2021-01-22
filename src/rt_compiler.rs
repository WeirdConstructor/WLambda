use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::SynPos;
use crate::vval::CompileError;
use crate::compiler::EvalError;
use crate::parser::{self};

enum RtType {
    Num,
    Pair,
    V2,
    V3,
    V4,
    Arr,
    Str,
}

#[derive(Debug, Clone)]
pub struct RtProg {
}

impl RtProg {
    fn call(args: &[f64]) -> f64 {
    }
}

pub(crate) fn compile(ast: &VVal)
    -> Result<RtProg, CompileError>
{
    match ast {
        VVal::Lst(_) => {
            let syn  = ast.at(0).unwrap_or_else(|| VVal::None);
            let spos = syn.get_syn_pos();
            let syn  = syn.get_syn();

            match syn {
//                Syntax::Block => compile_block(ast, 1, ce),
                _ => { Err(ast.compile_err(format!("bad input: {}", ast.s()))) },
            }
        },
        _ => { Err(ast.compile_err(format!("bad input: {}", ast.s()))) },
    }
}

pub fn compile_str(source: &str, name: &str) -> Result<RtProg, EvalError> {
    match parser::parse(source, name) {
        Ok(ast) => {
            match compile(&ast) {
                Ok(rt) => Ok(rt),
                Err(e) => Err(EvalError::CompileError(e)),
            }
        },
        Err(e)  => Err(EvalError::ParseError(e)),
    }
}

//                            Err(ast.compile_err(
//                                format!("Variable '{}' undefined", var_s)))
