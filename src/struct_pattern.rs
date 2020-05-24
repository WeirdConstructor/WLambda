use crate::vval::*;
use crate::str_int::*;
use crate::ops::DirectFun;
use crate::selector;

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
        Syntax::Call => {
            if ast.v_(1).v_(0).get_syn() != Syntax::Var {
                return Err(ast.compile_err(
                    format!("invalid variable binding in structure pattern: {}",
                            ast.v_(1).s())))
            }
            let var_bind = compile_struct_pattern(&ast.v_(1), var_map)?;

            let mut or_terms = vec![];
            for i in 2..ast.len() {
                or_terms.push(compile_struct_pattern(&ast.v_(i), var_map)?);
            }

            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                for o in or_terms.iter() {
                    if o(v, f) {
                        return var_bind(v, f);
                    }
                }
                return false;
            }))
        },
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
        Syntax::FVec => {
            match ast.len() {
                3 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = v {
                            if nv.dims().len() == 2 {
                                return
                                       x(&VVal::Flt(nv.x_raw()), f)
                                    && y(&VVal::Flt(nv.y_raw()), f)
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
                        if let VVal::FVec(nv) = v {
                            if nv.dims().len() == 3 {
                                return
                                       x(&VVal::Flt(nv.x_raw()), f)
                                    && y(&VVal::Flt(nv.y_raw()), f)
                                    && z(&VVal::Flt(nv.z_raw().unwrap()), f)
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
                        if let VVal::FVec(nv) = v {
                            if nv.dims().len() == 4 {
                                return
                                       x(&VVal::Flt(nv.x_raw()), f)
                                    && y(&VVal::Flt(nv.y_raw()), f)
                                    && z(&VVal::Flt(nv.z_raw().unwrap()), f)
                                    && w(&VVal::Flt(nv.w_raw().unwrap()), f)
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
        Syntax::Str => {
            let s = ast.v_(1).clone();
            Ok(Box::new(move |v: &VVal, _f: &FnVarAssign| {
                s.eqv(&v.deref())
            }))
        },
        Syntax::Pattern => {
            let res_ref = (VVal::None).to_ref();
            let rvar = s2sym("_");

            match ast.at(1).unwrap().with_s_ref(|pat_src|
                    selector::create_regex_find(pat_src, res_ref))
            {
                Ok(fun) => {
                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let r = fun(&v.deref());
                        if r.is_some() {
                            f(&rvar, &r);
                            true
                        } else {
                            false
                        }
                    }))
                },
                Err(e) => {
                    return Err(ast.compile_err(format!("bad pattern: {}", e)))
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
            } else if ast.is_int() || ast.is_float() {
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
