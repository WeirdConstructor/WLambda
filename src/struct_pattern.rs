use crate::vval::*;
use crate::str_int::*;
use crate::ops::DirectFun;
use crate::selector;
use std::rc::Rc;

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

macro_rules! gen_glob {
    ($var: ident, $next: ident, $f: ident, $lst: ident, $idx: ident, $remaining: ident) => {
        {
            for i in 0..$remaining {

                let n_idx = $lst.len() - i;
                if $next($lst, n_idx, $f) {

                    if $var.is_some() {
                        let sublist = VVal::vec();
                        for j in $idx..n_idx {
                            sublist.push($lst.v_(j));
                        }
                        store_var(&sublist, &$var, $f);
                    }

                    return true;
                }
            }

            false
        }
    }
}

pub fn compile_struct_list_pattern(ast: &VVal, var_map: &VVal, var: Option<Symbol>, next: StructListNode)
    -> Result<StructListNode, CompileError>
{
    let syn  = ast.v_(0);

    //d// println!("LIST PAT: {}", ast.s());

    match syn.get_syn() {
        Syntax::Var if ast.v_(1).to_sym().to_string() == "_*" => {
            Ok(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                if idx >= lst.len() {
                    store_var(&VVal::vec(), &var, f);
                    return true;
                }
                let remaining = (lst.len() - idx) + 1;
                gen_glob!(var, next, f, lst, idx, remaining)
            }))
        },
        Syntax::Var if ast.v_(1).to_sym().to_string() == "_+" => {
            Ok(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                if idx >= lst.len() { return false; }
                let remaining = (lst.len() - (idx + 1)) + 1;
                gen_glob!(var, next, f, lst, idx, remaining)
            }))
        },
        Syntax::Var if ast.v_(1).to_sym().to_string() == "_?" => {
            Ok(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                if next(lst, idx + 1, f) {
                    store_var(&VVal::opt(lst.v_(idx)), &var, f);
                    true
                } else {
                    store_var_if(next(lst, idx, f), &VVal::opt_none(), &var, f)
                }
            }))
        },
        Syntax::Call
            if    ast.v_(1).v_(0).get_syn() == Syntax::Var
               && (   ast.v_(1).v_(1).to_sym().to_string() == "_*"
                   || ast.v_(1).v_(1).to_sym().to_string() == "_+") => {

            if ast.len() > 3 {
                return Err(ast.compile_err(
                    format!("_* takes only 1 argument in list structure pattern: {}",
                            ast.s())));
            }

            let pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;

            let n0 = ast.v_(1).v_(1).to_sym().to_string() == "_*";

            Ok(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                let mut valid_idx : Option<usize> = None;
                let mut capture_matches = false;

                let match_start_idx =
                    if n0 {
                        if next(lst, idx, f) {
                            valid_idx = Some(idx);
                        }
                        idx
                    } else {
//                        if idx >= lst.len() {
//                            return false;
//                        }

                        if pat(&lst.v_(idx), f) {
                            capture_matches = true;
                            if next(lst, idx + 1, f) {
                                valid_idx = Some(idx);
                            }

                            idx + 1
                        } else {
                            return false;
                        }
                    };

                for i in match_start_idx..lst.len() {
                    if pat(&lst.v_(i), f) {
                        capture_matches = true;

                        if next(lst, i + 1, f) {
                            valid_idx = Some(i);
                        }
                    } else {
                        break;
                    }
                }

                if let Some(valid_idx) = valid_idx {
                    let v = VVal::vec();
                    if capture_matches {
                        for i in idx..(valid_idx + 1) {
                            v.push(lst.v_(i));
                        }
                    }

                    store_var(&v, &var, f);

                    true
                } else {
                    false
                }
            }))
        },
        Syntax::Call
            if    ast.v_(1).v_(0).get_syn() == Syntax::Var
               && ast.v_(1).v_(1).to_sym().to_string() == "_?" => {

            if ast.len() > 3 {
                return Err(ast.compile_err(
                    format!("_* takes only 1 argument in list structure pattern: {}",
                            ast.s())));
            }

            let pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;

            Ok(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                if idx >= lst.len() {
                    return next(lst, idx, f);
                }
                if pat(&lst.v_(idx), f) && next(lst, idx + 1, f) {
                    store_var(&lst.v_(idx), &var, f);
                    true
                } else {
                    next(lst, idx, f)
                }
            }))
        },
        Syntax::Call
            if    ast.v_(1).v_(0).get_syn() == Syntax::Var
               && ast.v_(1).v_(1).to_sym().chars().next().unwrap_or('_') != '_' => {
            let var_sym = ast.v_(1).v_(1).to_sym();
            let var_str = var_sym.to_string();
            let var_sym_store =
                if var_str != "?" {
                    var_map.set_key_sym(var_sym.clone(), VVal::Bol(true))
                           .expect("no double mut");
                    Some(var_sym)
                } else {
                    None
                };

            let mut or_terms = vec![];
            let next_rc = Rc::new(next);
            for i in 2..ast.len() {
                let n_next_rc = next_rc.clone();
                let n_next =
                    Box::new(move |v: &VVal, idx: usize, f: &FnVarAssign| {
                        (*n_next_rc)(v, idx, f)
                    });
                or_terms.push(
                    compile_struct_list_pattern(
                        &ast.v_(i), var_map, var_sym_store.clone(), n_next)?);
            }

            Ok(Box::new(move |v: &VVal, idx: usize, f: &FnVarAssign| {
                for o in or_terms.iter() {
                    if o(v, idx, f) {
                        store_var(v, &var, f);
                        return true;
                    }
                }
                false
            }))
        },
        _ => {
            let pat = compile_struct_pattern(ast, var_map, var)?;
            Ok(Box::new(move |lst: &VVal, idx: usize, f: &FnVarAssign| -> bool {
                if idx >= lst.len() { return false; }
                let v = lst.v_(idx);

                if pat(&v, f) && next(lst, idx + 1, f) {
                    return true;
                }

                false
            }))
        },
    }
}

pub fn compile_struct_pattern(ast: &VVal, var_map: &VVal, var: Option<Symbol>)
    -> Result<StructNode, CompileError>
{
    let syn  = ast.v_(0);

    //d// println!("COMP STRUCT PAT: {}", ast.s());

    match syn.get_syn() {
        Syntax::Call => {
            if ast.v_(1).v_(0).get_syn() != Syntax::Var {
                return Err(ast.compile_err(
                    format!("invalid variable binding in structure pattern: {}",
                            ast.v_(1).s())))
            }
            let var_sym = ast.v_(1).v_(1).to_sym();
            let var_str = var_sym.to_string();
            let var_sym_store =
                if var_sym.to_string() != "?" {
                    var_map.set_key_sym(var_sym.clone(), VVal::Bol(true))
                           .expect("no double mut");
                    Some(var_sym)
                } else {
                    None
                };

            let mut terms : Vec<StructNode> = vec![];

            if var_str.len() >= 2 && var_str.starts_with('_') {
                match &var_str[..] {
                    "_type?" => {
                        for i in 2..ast.len() {
                            if ast.v_(i).v_(0).get_syn() != Syntax::Key {
                                return Err(ast.compile_err(
                                    format!("invalid type test in structure pattern, must be a symbol: {}",
                                            ast.v_(i).s())));
                            }
                            let sym = ast.v_(i).v_(1);
                            terms.push(Box::new(move |v: &VVal, _f: &FnVarAssign| {
                                sym.with_s_ref(|s| s[..] == v.type_name()[..])
                            }));
                        }
                    },
                    _ => {
                        return Err(ast.compile_err(
                            format!("invalid test function in structure pattern: {}",
                                    var_str)));
                    },
                }
            } else {
                for i in 2..ast.len() {
                    terms.push(
                        compile_struct_pattern(
                            &ast.v_(i), var_map, var_sym_store.clone())?);
                }
            }

            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                for o in terms.iter() {
                    if o(v, f) {
                        store_var(v, &var, f);
                        return true;
                    }
                }
                false
            }))
        },
        Syntax::Var => {
            let sym = ast.v_(1);
            var_map.set_key(&sym, VVal::Bol(true)).expect("no double mut");
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
                    let x_pat = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y_pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = &v {
                            let nv = nv.as_ref();
                            if nv.dims().len() == 2 {
                                return store_var_if(
                                       x_pat(&VVal::Int(nv.x_raw()), f)
                                    && y_pat(&VVal::Int(nv.y_raw()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                4 => {
                    let x_pat = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y_pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z_pat = compile_struct_pattern(&ast.v_(3), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = &v {
                            let nv = nv.as_ref();
                            if nv.dims().len() == 3 {
                                return store_var_if(
                                       x_pat(&VVal::Int(nv.x_raw()), f)
                                    && y_pat(&VVal::Int(nv.y_raw()), f)
                                    && z_pat(&VVal::Int(nv.z_raw().unwrap()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                5 => {
                    let x_pat = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y_pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z_pat = compile_struct_pattern(&ast.v_(3), var_map, None)?;
                    let w_pat = compile_struct_pattern(&ast.v_(4), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::IVec(nv) = &v {
                            let nv = nv.as_ref();
                            if nv.dims().len() == 4 {
                                return store_var_if(
                                       x_pat(&VVal::Int(nv.x_raw()), f)
                                    && y_pat(&VVal::Int(nv.y_raw()), f)
                                    && z_pat(&VVal::Int(nv.z_raw().unwrap()), f)
                                    && w_pat(&VVal::Int(nv.w_raw().unwrap()), f),
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
                    let x_pat = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y_pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = &v {
                            let nv = nv.as_ref();
                            if nv.dims().len() == 2 {
                                return store_var_if(
                                       x_pat(&VVal::Flt(nv.x_raw()), f)
                                    && y_pat(&VVal::Flt(nv.y_raw()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                4 => {
                    let x_pat = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y_pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z_pat = compile_struct_pattern(&ast.v_(3), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = &v {
                            let nv = nv.as_ref();
                            if nv.dims().len() == 3 {
                                return store_var_if(
                                       x_pat(&VVal::Flt(nv.x_raw()), f)
                                    && y_pat(&VVal::Flt(nv.y_raw()), f)
                                    && z_pat(&VVal::Flt(nv.z_raw().unwrap()), f),
                                    &v, &var, f);
                            }
                        }

                        false
                    }))
                },
                5 => {
                    let x_pat = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                    let y_pat = compile_struct_pattern(&ast.v_(2), var_map, None)?;
                    let z_pat = compile_struct_pattern(&ast.v_(3), var_map, None)?;
                    let w_pat = compile_struct_pattern(&ast.v_(4), var_map, None)?;

                    Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                        let v = v.deref();
                        if let VVal::FVec(nv) = &v {
                            let nv = nv.as_ref();
                            if nv.dims().len() == 4 {
                                return store_var_if(
                                       x_pat(&VVal::Flt(nv.x_raw()), f)
                                    && y_pat(&VVal::Flt(nv.y_raw()), f)
                                    && z_pat(&VVal::Flt(nv.z_raw().unwrap()), f)
                                    && w_pat(&VVal::Flt(nv.w_raw().unwrap()), f),
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
            let mut next : Option<StructListNode> =
                Some(Box::new(move |lst: &VVal, idx: usize, _f: &FnVarAssign| -> bool {
                    if idx != lst.len() { return false; }
                    true
                }));

            for i in 1..ast.len() {
                let list_match_cont = next.take();
                next = Some(
                    compile_struct_list_pattern(
                        &ast.v_(ast.len() - i),
                        var_map,
                        None,
                        list_match_cont.unwrap())?);
            }

            let list_matcher = next.take().unwrap();
            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                let v = v.deref();
                if !v.is_vec() { return false; }
                store_var_if(list_matcher(&v, 0, f), &v, &var, f)
            }))
        },
        Syntax::Map => {
            let mut all_matches_are_gets = true;
            for (kv, _) in ast.iter().skip(1) {
                if !kv.v_(0).is_sym() {
                    all_matches_are_gets = false;
                }
            }

            if all_matches_are_gets {
                let mut key_matches : Vec<(Symbol, StructNode)> = vec![];
                for (kv, _) in ast.iter().skip(1) {
                    let kvmatch = (
                        kv.v_(0).to_sym(),
                        compile_struct_pattern(&kv.v_(1), var_map, None)?
                    );
                    key_matches.push(kvmatch);
                }

                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    let v = v.deref();
                    if !v.is_map() { return false; }

                    for kv_match in key_matches.iter() {
                        let v = v.get_key_sym(&kv_match.0);
                        if let Some(v) = v {
                            if !(kv_match.1)(&v, f) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }

                    store_var(&v, &var, f);
                    true
                }))

            } else {
                let mut key_matches : Vec<(StructNode, StructNode)> = vec![];
                for (kv, _) in ast.iter().skip(1) {
                    let kvmatch = (
                        compile_struct_pattern(&kv.v_(0), var_map, None)?,
                        compile_struct_pattern(&kv.v_(1), var_map, None)?
                    );
                    key_matches.push(kvmatch);
                }

                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    let v = v.deref();
                    if let VVal::Map(m) = &v {
                        for kv_match in key_matches.iter() {
                            let mut matched_kv = false;
                            for (k, v) in m.borrow().iter() {
                                let k = VVal::Sym(k.clone());
                                if    (kv_match.0)(&k, f)
                                   && (kv_match.1)(&v, f)
                                {
                                    matched_kv = true;
                                    break;
                                }
                            }

                            if !matched_kv {
                                return false;
                            }
                        }
                    } else {
                        return false;
                    }

                    store_var(&v, &var, f);
                    true
                }))
            }
        },
        Syntax::Opt => {
            if ast.len() == 1 {
                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    if let VVal::Opt(None) = v {
                        store_var(&v, &var, f);
                        true
                    } else {
                        false
                    }
                }))
            } else {
                let cond = compile_struct_pattern(&ast.v_(1), var_map, None)?;
                Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                    let matched =
                        if let VVal::Opt(Some(ov)) = &v { cond(&*ov, f) }
                        else                            { false };
                    store_var_if(matched, &v, &var, f)
                }))
            }
        },
        Syntax::Err => {
            let cond = compile_struct_pattern(&ast.v_(1), var_map, None)?;
            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                let v = v.deref();
                let matched =
                    if let VVal::Err(ov) = &v { cond(&ov.borrow().0, f) }
                    else                      { false };
                store_var_if(matched, &v, &var, f)
            }))
        },
        Syntax::Str => {
            let s = ast.v_(1);
            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                store_var_if(s.eqv(&v.deref()), v, &var, f)
            }))
        },
        Syntax::Pattern => {
            let res_ref = (VVal::None).to_ref();

            let mode = ast.at(2).unwrap().with_s_ref(|s| selector::RegexMode::from_str(s));

            if mode == selector::RegexMode::FindAll {
                match ast.at(1).unwrap().with_s_ref(|pat_src|
                        selector::create_regex_find_all(pat_src, res_ref))
                {
                    Ok(fun) => {
                        Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                            let matches = VVal::vec();
                            let matches_o = matches.clone();
                            fun(&v.deref(), Box::new(move |v, _pos| {
                                matches.push(v);
                            }));
                            if matches_o.len() > 0 {
                                store_var(&matches_o, &var, f);
                                true
                            } else {
                                false
                            }
                        }))
                    },
                    Err(e) => {
                        Err(ast.compile_err(format!("bad pattern: {}", e)))
                    }
                }

            } else {
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
                        Err(ast.compile_err(format!("bad pattern: {}", e)))
                    }
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
                        //d// println!("STORE V: {:?} = {}", var, r.s());
                        if r.is_some() {
                            store_var(&r, &var, f);
                            true
                        } else {
                            false
                        }
                    }))
                },
                Err(e) => {
                    Err(ast.compile_err(format!("bad pattern: {}", e)))
                }
            }
        },
        Syntax::And => {
            let variant_a = compile_struct_pattern(&ast.v_(1), var_map, None)?;
            let variant_b = compile_struct_pattern(&ast.v_(2), var_map, None)?;

            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                store_var_if(
                       variant_a(&v, f)
                    && variant_b(&v, f),
                    &v, &var, f)
            }))
        },
        Syntax::Or => {
            let variant_a = compile_struct_pattern(&ast.v_(1), var_map, None)?;
            let variant_b = compile_struct_pattern(&ast.v_(2), var_map, None)?;

            Ok(Box::new(move |v: &VVal, f: &FnVarAssign| {
                store_var_if(
                       variant_a(&v, f)
                    || variant_b(&v, f),
                    &v, &var, f)
            }))
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
            } else if ast.is_int() || ast.is_float() || ast.is_none() || ast.is_sym() || ast.is_bool() || ast.is_byte() || ast.is_char() {
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

pub type StructPatBuildFun = Box<dyn Fn(Box<FnVarAssign>, Box<FnVarReset>) -> DirectFun>;

pub fn create_struct_patterns_direct_fun(patterns: &[VVal], var_map: &VVal)
    -> Result<StructPatBuildFun, CompileError>
{
    let mut pat_funs = Vec::new();
    for p in patterns.iter() {
        pat_funs.push(compile_struct_pattern(p, var_map, None)?);
    }

    let pat_funs = std::rc::Rc::new(pat_funs);

    Ok(Box::new(move |var_assign: Box<FnVarAssign>, var_reset: Box<FnVarReset>| {
        let pat_funs = pat_funs.clone();
        DirectFun::new(Rc::new(move |v: VVal, _env: &mut Env| {
            for (i, p) in pat_funs.iter().enumerate() {
                (var_reset)();
                if p(&v, &*var_assign) {
                    return VVal::Int(i as i64);
                }
            }

            VVal::Int(-1)
        }))
    }))
}

pub fn create_struct_pattern_function(ast: &VVal, var_map: &VVal, result_ref: VVal) -> Result<VVal, CompileError> {
    let struct_pat = vec![ast.clone()];
    let fun_constructor =
        create_struct_patterns_direct_fun(&struct_pat[..], var_map)?;

    Ok(VValFun::new_fun(
        move |env: &mut Env, _argc: usize| {
            let m = VVal::map();
            let m2 = m.clone();
            let dfun =
                fun_constructor(Box::new(move |key: &Symbol, val: &VVal| {
                    m2.set_key_sym(key.clone(), val.clone())
                      .expect("no double mut");
                }), Box::new(|| ()));

            let res = (dfun.fun)(env.arg(0), env);

            if res.i() >= 0 {
                result_ref.set_ref(m.clone());
                Ok(m)
            } else { Ok(VVal::None) }
        }, Some(1), Some(1), true))
}
