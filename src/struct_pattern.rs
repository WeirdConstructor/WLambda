use crate::vval::*;
use crate::str_int::*;

pub enum StructPatternError {
}

pub type FnVarAssign = dyn Fn(&Symbol, &VVal);
pub type StructNode  = Box<dyn Fn(&VVal, &FnVarAssign) -> bool>;

pub fn compile_struct_pattern(v: &VVal) -> Result<StructNode, StructPatternError> {
    Ok(Box::new(|v: &VVal, f: &FnVarAssign| {
        true
    }))
}
