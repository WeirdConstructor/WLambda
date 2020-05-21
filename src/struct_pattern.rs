use crate::vval::*;
use crate::str_int::*;

pub type FnVarAssign = dyn Fn(&Symbol, &VVal);
pub type StructNode  = Box<dyn Fn(&VVal, &FnVarAssign) -> bool>;

pub fn compile_struct_pattern(ast: &VVal, var_map: &VVal)
    -> Result<StructNode, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::None);
    let spos = syn.get_syn_pos();

    println!("COMP STRUCT PAT: {}", syn.s());

    Ok(Box::new(|v: &VVal, f: &FnVarAssign| {
        true
    }))
}

pub fn create_struct_pattern_function(ast: &VVal, var_map: &VVal) -> Result<VVal, CompileError> {
    let struct_pat = compile_struct_pattern(ast, var_map)?;

    Ok(VValFun::new_fun(
        move |env: &mut Env, _argc: usize| {
            let m = VVal::map();
            let m2 = m.clone();
            if (*struct_pat)(
                env.arg_ref(0).unwrap(),
                &move |key: &Symbol, val: &VVal| {
                    m2.set_key_sym(key.clone(), val.clone()).unwrap();
                })
            {
                Ok(m)
            }
            else
            {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), false))
}
