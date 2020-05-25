use crate::vval::*;
use crate::str_int::*;
use crate::ops::DirectFun;
use crate::selector;

pub type FnVarAssign    = dyn Fn(&Symbol, &VVal);
pub type FnVarReset     = dyn Fn();
pub type StructNode     = Box<dyn Fn(&VVal, &FnVarAssign) -> bool>;
pub type StructListNode = Box<dyn Fn(&VVal, usize, &FnVarAssign) -> bool>;

fn store_var_if(b: bool, v: &VVal, var: &Option<Symbol>, f: &FnVarAssign) -> bool {
    if b { store_var(v, var, f); }
    b
}

fn store_var(v: &VVal, var: &Option<Symbol>, f: &FnVarAssign) {
    if let Some(sym) = var {
        f(&sym, v);
    }
}

pub fn compile_struct_list_pattern(ast: &VVal, var_map: &VVal, var: Option<Symbol>, next: StructListNode)
    -> Result<StructNode, CompileError>
{
    let syn  = ast.v_(0);
    let spos = syn.get_syn_pos();

    match syn.get_syn() {
        _ => {
            let pat = compile_struct_pattern(ast, var_map, None),
            Some(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                if idx >= lst.len() { return false; }
                let v = lst.v_(idx);

                if pat(v, f) && next(lst, idx + 1, f) {
                    store_var(v, var, f);
                    return true;
                }

                false
            }));
        },
    }
}

pub fn compile_struct_pattern(ast: &VVal, var_map: &VVal, var: Option<Symbol>)
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
            let var_sym = ast.v_(1).v_(1).to_sym();
            let var_sym_store =
                if var_sym.to_string() != "?" {
                    var_map.set_key_sym(var_sym.clone(), VVal::Bol(true));
                    Some(var_sym)
                } else {
                    None
                };

            let mut or_terms = vec![];
            for i in 2..ast.len() {
                or_terms.push(
                    compile_struct_pattern(
                        &ast.v_(i), var_map, var_sym_store.clone())?);
            }

            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                for o in or_terms.iter() {
                    if o(v, f) {
                        store_var(v, &var, f);
                        return true;
                    }
                }
                return false;
            }))
        },
        Syntax::Var => {
            let sym = ast.v_(1);
            var_map.set_key(&sym, VVal::Bol(true));
            let sym = sym.to_sym();

            if sym.to_string() == "?" {
                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    store_var(v, &var, f);
                    true
                }))
            } else {
                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    store_var(v, &var, f);
                    store_var(v, &Some(sym.clone()), f);
                    true
                }))
            }
        },
        Syntax::Key => {
            let sym = ast.v_(1);
            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                store_var_if(sym.eqv(&v.deref()), v, &var, f)
            }))
        },
        Syntax::IVec => {
            match ast.len() {
                3 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = v {
                            if nv.dims().len() == 2 {
                                return store_var_if(
                                       x(&VVal::Int(nv.x_raw()), f)
                                    && y(&VVal::Int(nv.y_raw()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                4 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z = compile_struct_pattern(&ast.v_(3), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = v {
                            if nv.dims().len() == 3 {
                                return store_var_if(
                                       x(&VVal::Int(nv.x_raw()), f)
                                    && y(&VVal::Int(nv.y_raw()), f)
                                    && z(&VVal::Int(nv.z_raw().unwrap()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                5 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z = compile_struct_pattern(&ast.v_(3), var_map, None)?;
                    let w = compile_struct_pattern(&ast.v_(4), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = v {
                            if nv.dims().len() == 4 {
                                return store_var_if(
                                       x(&VVal::Int(nv.x_raw()), f)
                                    && y(&VVal::Int(nv.y_raw()), f)
                                    && z(&VVal::Int(nv.z_raw().unwrap()), f)
                                    && w(&VVal::Int(nv.w_raw().unwrap()), f),
                                    &v, &var, f);
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
                    let x = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = v {
                            if nv.dims().len() == 2 {
                                return store_var_if(
                                       x(&VVal::Flt(nv.x_raw()), f)
                                    && y(&VVal::Flt(nv.y_raw()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                4 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z = compile_struct_pattern(&ast.v_(3), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = v {
                            if nv.dims().len() == 3 {
                                return store_var_if(
                                       x(&VVal::Flt(nv.x_raw()), f)
                                    && y(&VVal::Flt(nv.y_raw()), f)
                                    && z(&VVal::Flt(nv.z_raw().unwrap()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                5 => {
                    let x = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z = compile_struct_pattern(&ast.v_(3), var_map, None)?;
                    let w = compile_struct_pattern(&ast.v_(4), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = v {
                            if nv.dims().len() == 4 {
                                return store_var_if(
                                       x(&VVal::Flt(nv.x_raw()), f)
                                    && y(&VVal::Flt(nv.y_raw()), f)
                                    && z(&VVal::Flt(nv.z_raw().unwrap()), f)
                                    && w(&VVal::Flt(nv.w_raw().unwrap()), f),
                                    &v, &var, f);
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
        Syntax::Lst => {
            let next : Option<StructListNode> =
                Some(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                    if idx != lst.len() { return false; }
                    true
                }));
            for i in 1..ast.len() {
                let n = next.take();
                next = Some(
                    compile_struct_list_pattern(
                        &ast.v_(ast.len() - i), var_map, None, n));
            }

            let n = next.take();
            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                let v = v.deref();
                if !v.is_list() { return false; }
                store_var_if(n(&v, 0, f), v, &var, f)
            }))
        },
        Syntax::Str => {
            let s = ast.v_(1).clone();
            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                store_var_if(s.eqv(&v.deref()), v, &var, f)
            }))
        },
        Syntax::Pattern => {
            let res_ref = (VVal::None).to_ref();

            match ast.at(1).unwrap().with_s_ref(|pat_src|
                    selector::create_regex_find(pat_src, res_ref))
            {
                Ok(fun) => {
                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let r = fun(&v.deref());
                        if r.is_some() {
                            store_var(&r, &var, f);
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
        Syntax::Selector => {
            let res_ref = (VVal::None).to_ref();

            match ast.at(1).unwrap().with_s_ref(|sel_src|
                    selector::create_selector(sel_src, res_ref))
            {
                Ok(fun) => {
                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let r = fun(&v.deref());
                        println!("STORE V: {:?} = {}", var, r.s());
                        if r.is_some() {
                            store_var(&r, &var, f);
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
                let p1 = compile_struct_pattern(&ast.at(0).unwrap(), var_map, None)?;
                let p2 = compile_struct_pattern(&ast.at(1).unwrap(), var_map, None)?;

                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    let v = v.deref();
                    if !v.is_pair() {
                        return false;
                    }

                    store_var_if(
                           p1(&v.v_(0), f)
                        && p2(&v.v_(1), f),
                        &v, &var, f)
                }))
            } else if ast.is_int() || ast.is_float() {
                let ast = ast.clone();
                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    store_var_if(ast.eqv(&v.deref()), v, &var, f)
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
        pat_funs.push(compile_struct_pattern(p, var_map, None)?);
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

pub fn create_struct_pattern_function(ast: &VVal, var_map: &VVal, result_ref: VVal) -> Result<VVal, CompileError> {
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

            if res.i() >= 0 {
                result_ref.set_ref(m.clone());
                Ok(m)
            } else { Ok(VVal::None) }
        }, Some(1), Some(1), false))
}
