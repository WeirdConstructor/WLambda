use crate::vval::VVal;
use crate::vval::VValChr;
use crate::vval::Syntax;
use crate::vval::SynPos;
use crate::vval::CompileError;
use crate::compiler::EvalError;
use crate::parser::{self};

#[derive(Debug, Clone)]
pub enum RtVal {
    Void,
    Num(f64),
    Chr(VValChr),
    Bol(bool),
    Opt(usize),
    Arr(u32, u32),
    NVec(u32, u32),
    Str(u32, u32),
    Byt(u32, u32),
    Bltin(usize),
    Fun(u32, u32),
}

#[derive(Debug, Clone)]
enum RtType {
    Void,
    Num,
    Chr,
    Bol,
    Opt,
    Pair,
    Arr,
    NVec,
    Str,
    Byt,
}

#[derive(Debug, Clone)]
pub struct RtProg {
    result: RtVal,
}

impl RtProg {
    fn call(& mut self, args: &[RtVal]) -> &RtVal {
        &self.result
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
