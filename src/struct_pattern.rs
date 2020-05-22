use crate::vval::*;
use crate::str_int::*;
use crate::ops::DirectFun;

pub type FnVarAssign = dyn Fn(&Symbol, &VVal);
pub type FnVarReset  = dyn Fn();
pub type StructNode  = Box<dyn Fn(&VVal, &FnVarAssign) -> bool>;

pub fn compile_struct_pattern(ast: &VVal, var_map: &VVal)
    -> Result<StructNode, CompileError>
{
    let syn  = ast.v_(0);
    let spos = syn.get_syn_pos();

    println!("COMP STRUCT PAT: {}", ast.s());

    match syn.get_syn() {
        Syntax::Var => {
            let sym = ast.v_(1);
            var_map.set_key(&sym, VVal::Bol(true));
            let sym = sym.to_sym();

            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                f(&sym, v);
                true
            }))
        },
        Syntax::Key => {
            let sym = ast.v_(1);
            Ok(Box::new(move |v: &VVal, _f: &FnVarAssign| {
                sym.eqv(&v.deref())
            }))
        },
        Syntax::IVec => {
            match ast.len() {
                3 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = v {
                            if nv.dims().len() == 2 {
                                return
                                       x(&VVal::Int(nv.x_raw()), f)
                                    && y(&VVal::Int(nv.y_raw()), f)
                            }
                        }

                        false
                    }))
                },
                4 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map)?;
                    let z = compile_struct_pattern(&ast.v_(3), var_map)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = v {
                            if nv.dims().len() == 3 {
                                return
                                       x(&VVal::Int(nv.x_raw()), f)
                                    && y(&VVal::Int(nv.y_raw()), f)
                                    && z(&VVal::Int(nv.z_raw().unwrap()), f)
                            }
                        }

                        false
                    }))
                },
                5 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map)?;
                    let z = compile_struct_pattern(&ast.v_(3), var_map)?;
                    let w = compile_struct_pattern(&ast.v_(4), var_map)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = v {
                            if nv.dims().len() == 4 {
                                return
                                       x(&VVal::Int(nv.x_raw()), f)
                                    && y(&VVal::Int(nv.y_raw()), f)
                                    && z(&VVal::Int(nv.z_raw().unwrap()), f)
                                    && w(&VVal::Int(nv.w_raw().unwrap()), f)
                            }
                        }

                        false
                    }))
                },
                _ => {
                    Err(ast.compile_err(
                        format!("invalid structure pattern: {}", ast.s())))
                }
            }
        },
        _ => {
            if ast.is_pair() {
                let p1 = compile_struct_pattern(&ast.at(0).unwrap(), var_map)?;
                let p2 = compile_struct_pattern(&ast.at(1).unwrap(), var_map)?;

                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    let v = v.deref();
                    if !v.is_pair() {
                        return false;
                    }

                       p1(&v.v_(0), f)
                    && p2(&v.v_(1), f)
                }))
            } else if ast.is_int() {
                let ast = ast.clone();
                Ok(Box::new(move |v: &VVal, _f: &FnVarAssign| {
                    ast.eqv(&v.deref())
                }))

            } else {
                Err(ast.compile_err(
                    format!("invalid structure pattern: {}", ast.s())))
            }
        },
    }
}

pub fn create_struct_patterns_direct_fun(patterns: &Vec<VVal>, var_map: &VVal)
    -> Result<Box<dyn Fn(Box<FnVarAssign>, Box<FnVarReset>) -> DirectFun>, CompileError>
{
    let mut pat_funs = Vec::new();
    for p in patterns.iter() {
        pat_funs.push(compile_struct_pattern(p, var_map)?);
    }

    let pat_funs = std::rc::Rc::new(pat_funs);

    Ok(Box::new(move |var_assign: Box<FnVarAssign>, var_reset: Box<FnVarReset>| {
        let pat_funs = pat_funs.clone();
        DirectFun { fun: std::rc::Rc::new(move |v: VVal, env: &mut Env| {
            for (i, p) in pat_funs.iter().enumerate() {
                (var_reset)();
                if p(&v, &*var_assign) {
                    return VVal::Int(i as i64);
                }
            }

            VVal::Int(-1)
        }) }
    }))
}

pub fn create_struct_pattern_function(ast: &VVal, var_map: &VVal) -> Result<VVal, CompileError> {
    let struct_pat = vec![ast.clone()];
    let fun_constructor =
        create_struct_patterns_direct_fun(&struct_pat, var_map)?;

    Ok(VValFun::new_fun(
        move |env: &mut Env, _argc: usize| {
            let m = VVal::map();
            let m2 = m.clone();
            let dfun =
                fun_constructor(Box::new(move |key: &Symbol, val: &VVal| {
                    m2.set_key_sym(key.clone(), val.clone()).unwrap();
                }), Box::new(|| ()));

            let res = (dfun.fun)(env.arg(0), env);

            if res.i() >= 0 { Ok(m) }
            else { Ok(VVal::None) }
        }, Some(1), Some(1), false))
}
