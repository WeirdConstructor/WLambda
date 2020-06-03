// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This module defines some default functions and operations
available in the WLambda language.

You there are two WLambda modules provided by this module:

- [core_symbol_table()](fn.core_symbol_table.html)
- [std_symbol_table()](fn.std_symbol_table.html)

[]: ---- REFERENCE DOC START ----

[]: ---- REFERENCE DOC END ----

*/

const VERSION: &str = env!("CARGO_PKG_VERSION");

use crate::compiler::*;
use crate::vval::*;
use crate::nvec::*;
use crate::util;
use std::rc::Rc;
use crate::threads::*;
use crate::selector::*;

macro_rules! func {
    ($g: ident, $name: expr, $cb: expr, $min: expr, $max: expr, $err_arg_ok: expr) => {
        $g.fun($name, $cb, $min, $max, $err_arg_ok);
    }
}

macro_rules! add_func {
    ($g: ident, $op: tt, $env: ident, $argc: ident, $b: block, $min: expr, $max: expr, $err_arg_ok: expr) => {
        $g.fun(stringify!($op), |$env: &mut Env, $argc: usize| $b, $min, $max, $err_arg_ok);
    }
}

macro_rules! add_multi_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc <= 0 { return Ok(VVal::None); }
            if let VVal::Flt(f) = env.arg(0) {
                let mut accum = f;
                for i in 1..argc { accum = accum $op env.arg(i).f() }
                Ok(VVal::Flt(accum))
            } else {
                let mut accum = env.arg(0).i();
                for i in 1..argc { accum = accum $op env.arg(i).i() }
                Ok(VVal::Int(accum))
            }
        }, Some(2), None, false)
    }
}

macro_rules! add_bool_bin_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::None); }
            let a = env.arg(0);
            if let VVal::Flt(af) = a { Ok(VVal::Bol(af $op env.arg(1).f())) }
            else { Ok(VVal::Bol(a.i() $op env.arg(1).i())) }
        }, Some(2), Some(2), false)
    }
}

macro_rules! add_fi_bin_op {
    ($g: ident, $op: tt, $a: ident, $b: ident, $ef: expr, $ei: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::None); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            if let VVal::Flt(_) = $a { $ef }
            else { $ei }
        }, Some(2), Some(2), false)
    }
}

macro_rules! add_bin_op_err_ok {
    ($g: ident, $op: tt, $a: ident, $b: ident, $e: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::None); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            $e
        }, Some(2), Some(2), true)
    }
}

macro_rules! add_sbin_op {
    ($g: ident, $op: literal, $a: ident, $b: ident, $e: expr) => {
        func!($g,
                $op, |env: &mut Env, argc: usize| {
                if argc < 2 { return Ok(VVal::None); }
                let $a = env.arg(0);
                let $b = env.arg(1);
                $e
                }, Some(2), Some(2), false);
    }
}

macro_rules! add_num_fun_flt {
    ($g: ident, $op: literal, $e: tt) => {
        func!($g, $op,
            |env: &mut Env, _argc: usize| {
                Ok(VVal::Flt(env.arg(0).f().$e()))
            }, Some(1), Some(1), false);
    }
}

macro_rules! add_num_fun_flt2 {
    ($g: ident, $op: literal, $e: tt) => {
        func!($g, $op,
            |env: &mut Env, _argc: usize| {
                Ok(VVal::Flt(env.arg(0).f().$e(env.arg(1).f())))
            }, Some(2), Some(2), false);
    }
}

macro_rules! sizeof_writeln {
    ($write: ident, $type: ty) => {
        writeln!($write, "sizeof {:40}:  {:2} bytes",
                 stringify!($type),
                 std::mem::size_of::<$type>()).expect("stdout access works");
    }
}

/// Returns a SymbolTable with all WLambda core language symbols.
#[allow(clippy::cast_lossless,clippy::assign_op_pattern)]
pub fn core_symbol_table() -> SymbolTable {
    let mut st = SymbolTable::new();

    // The implementations for +/- are essentially just like the `add_multi_op`
    // implementations, except for how they accept down to 1 parameter for
    // unary +/-.
    add_func!(st, +, env, argc, {
        Ok(match (argc, env.arg(0)) {
            (0, _) => VVal::None,
            (1, VVal::Flt(f)) => VVal::Flt(f),
            (1, VVal::Int(i)) => VVal::Int(i),
            (1, VVal::FVec(fv)) => VVal::FVec(fv),
            (1, VVal::IVec(iv)) => VVal::IVec(iv),
            (a, VVal::Flt(f)) => {
                let mut accum = f;
                for i in 1..a { accum = accum + env.arg(i).f() }
                VVal::Flt(accum)
            }
            (a, v) => {
                let mut accum = v.i();
                for i in 1..a { accum = accum + env.arg(i).i() }
                VVal::Int(accum)
            }
        })
    }, Some(1), None, false);
    add_func!(st, -, env, argc, {
        Ok(match (argc, env.arg(0)) {
            (0, _) => VVal::None,
            (1, VVal::Int(i))   => VVal::Int(-i),
            (1, VVal::Flt(f))   => VVal::Flt(-f),
            (1, VVal::FVec(fv)) => VVal::FVec(Box::new(-*fv)),
            (1, VVal::IVec(iv)) => VVal::IVec(Box::new(-*iv)),
            (a, VVal::Flt(f))   => {
                let mut accum = f;
                for i in 1..a { accum = accum - env.arg(i).f() }
                VVal::Flt(accum)
            }
            (a, v) => {
                let mut accum = v.i();
                for i in 1..a { accum = accum - env.arg(i).i() }
                VVal::Int(accum)
            }
        })
    }, Some(1), None, false);

    add_multi_op!(st, *);
    add_multi_op!(st, /);
    add_multi_op!(st, %);

    add_bool_bin_op!(st, <);
    add_bool_bin_op!(st, >);
    add_bool_bin_op!(st, <=);
    add_bool_bin_op!(st, >=);

    add_bin_op_err_ok!(st, ==, a, b, Ok(VVal::Bol(a.eqv(&b))));
    add_bin_op_err_ok!(st, !=, a, b, Ok(VVal::Bol(!a.eqv(&b))));

    add_sbin_op!(st, "&|", a, b,
        Ok(VVal::Int(((a.i() as u32) | (b.i() as u32)) as i64)));
    add_sbin_op!(st, "&", a, b,
        Ok(VVal::Int(((a.i() as u32) & (b.i() as u32)) as i64)));
    add_sbin_op!(st, "&^", a, b,
        Ok(VVal::Int(((a.i() as u32) ^ (b.i() as u32)) as i64)));
    add_sbin_op!(st, "<<", a, b,
        Ok(VVal::Int(((a.i() as u32) << (b.i() as u32)) as i64)));
    add_sbin_op!(st, ">>", a, b,
        Ok(VVal::Int(((a.i() as u32) >> (b.i() as u32)) as i64)));

    add_fi_bin_op!(st, ^, a, b,
        Ok(VVal::Flt(a.f().powf(b.f()))),
        Ok(VVal::Int(a.i().pow(b.i() as u32))));

    func!(st, "not",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Bol(!env.arg(0).b()))
        }, Some(1), Some(1), false);

    func!(st, "panic",
        |env: &mut Env, _argc: usize| {
            Err(StackAction::panic(env.arg(0), None))
        }, Some(1), Some(1), true);

    func!(st, "block",
        |env: &mut Env, argc: usize| {
            let mut label = VVal::None;
            let fn_arg_idx = if argc <= 1 { 0 } else { label = env.arg(0); 1 };
            match env.arg(fn_arg_idx).call_no_args(env) {
                Ok(v)   => Ok(v),
                Err(StackAction::Return(ret)) => {
                    if ret.0.eqv(&label) { Ok(ret.1) }
                    else { Err(StackAction::Return(ret)) }
                },
                Err(e)  => Err(e),
            }
        }, Some(1), Some(2), false);

    func!(st, "_?",
        |env: &mut Env, argc: usize| {
            let mut lbl = VVal::None;
            let err_val = if argc > 1 {
                lbl = env.arg(0);
                env.arg(1)
            } else { env.arg(0) };

            match err_val {
                VVal::Err(e) => Err(StackAction::Return(Box::new((lbl, VVal::Err(e))))),
                v            => Ok(v),
            }
        }, Some(1), Some(2), true);

    func!(st, "error_to_str",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(_) => {
                    Ok(VVal::new_str_mv(env.arg(0).s()))
                },
                v => {
                    Err(StackAction::panic_msg(
                        format!("error_to_str on non error value: {}", v.s())))
                },
            }
        }, Some(1), Some(1), true);

    func!(st, "unwrap_err",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(err_v) => {
                    Ok(err_v.borrow().0.clone())
                },
                v => {
                    Err(StackAction::panic_msg(
                        format!("unwrap_err on non error value: {}", v.s())))
                },
            }
        }, Some(1), Some(1), true);

    func!(st, "unwrap",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(err_v) => {
                    Err(StackAction::panic_str(
                        format!("unwrap error: {}", err_v.borrow().0.s()),
                        Some(err_v.borrow().1.clone())))
                },
                VVal::Opt(None) => {
                    Err(StackAction::panic_str(
                        "unwrap empty option!".to_string(), None))
                },
                VVal::Opt(Some(v)) => Ok((*v).clone()),
                v => Ok(v)
            }
        }, Some(1), Some(1), true);

    func!(st, "on_error",
        |env: &mut Env, _argc: usize| {
            let err_fn = env.arg(0);
            match env.arg(1) {
                VVal::Err(err_v) => {
                    env.with_restore_sp(|e: &mut Env| {
                        e.push(err_v.borrow().0.clone());
                        e.push(VVal::Int(err_v.borrow().1.line as i64));
                        e.push(VVal::Int(err_v.borrow().1.col as i64));
                        e.push(VVal::new_str(err_v.borrow().1.file.s()));
                        err_fn.call_internal(e, 4)
                    })
                },
                e => Ok(e)
            }
        }, Some(2), Some(2), true);

    func!(st, "return",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Return(Box::new((VVal::None, VVal::None)))); }
            if argc < 2 { return Err(StackAction::Return(Box::new((VVal::None, env.arg(0))))); }
            Err(StackAction::Return(Box::new((env.arg(0), env.arg(1)))))
        }, Some(1), Some(2), true);

    func!(st, "break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(Box::new(VVal::None))); }
            Err(StackAction::Break(Box::new(env.arg(0))))
        }, Some(0), Some(1), true);

    func!(st, "next",
        |_env: &mut Env, _argc: usize| {
            Err(StackAction::Next)
        }, Some(0), Some(0), false);

    func!(st, "pick",
        |env: &mut Env, _argc: usize| Ok(if env.arg(0).b() { env.arg(1) } else { env.arg(2) }),
        Some(3), Some(3), false);

    func!(st, "ivec",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec()))),
        Some(1), Some(1), false);
    func!(st, "ivec2",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec().vec2()))),
        Some(1), Some(1), false);
    func!(st, "ivec3",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec().vec3()))),
        Some(1), Some(1), false);
    func!(st, "ivec4",
        |env: &mut Env, _argc: usize| Ok(VVal::IVec(Box::new(env.arg(0).nvec().vec4()))),
        Some(1), Some(1), false);
    func!(st, "fvec",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec()))),
        Some(1), Some(1), false);
    func!(st, "fvec2",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec().vec2()))),
        Some(1), Some(1), false);
    func!(st, "fvec3",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec().vec3()))),
        Some(1), Some(1), false);
    func!(st, "fvec4",
        |env: &mut Env, _argc: usize| Ok(VVal::FVec(Box::new(env.arg(0).nvec().vec4()))),
        Some(1), Some(1), false);
    func!(st, "is_nvec",
        |env: &mut Env, _argc: usize| Ok(VVal::Bol(env.arg(0).is_nvec())),
        Some(1), Some(1), false);
    func!(st, "is_ivec",
        |env: &mut Env, _argc: usize| Ok(VVal::Bol(env.arg(0).is_ivec())),
        Some(1), Some(1), false);
    func!(st, "is_fvec",
        |env: &mut Env, _argc: usize| Ok(VVal::Bol(env.arg(0).is_fvec())),
        Some(1), Some(1), false);
    func!(st, "nvec_len",
        |env: &mut Env, _argc: usize| Ok(VVal::Int(env.arg(0).nvec_len() as i64)),
        Some(1), Some(1), false);

    func!(st, "bool",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).b())) },
        Some(1), Some(1), true);
    func!(st, "float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Flt(env.arg(0).f())) },
        Some(1), Some(1), false);
    func!(st, "int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).i())) },
        Some(1), Some(1), false);
    func!(st, "str",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw())) },
        Some(1), Some(1), false);
    func!(st, "sym",
        |env: &mut Env, _argc: usize|
            env.arg(0).with_s_ref(|s: &str| { Ok(VVal::new_sym(s)) }),
        Some(1), Some(1), false);
    func!(st, "is_some",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_some())) },
        Some(1), Some(1), true);
    func!(st, "is_none",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_none())) },
        Some(1), Some(1), true);
    func!(st, "is_err",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_err())) },
        Some(1), Some(1), true);
    func!(st, "is_map",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_map())) },
        Some(1), Some(1), true);
    func!(st, "is_vec",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_vec())) },
        Some(1), Some(1), true);
    func!(st, "is_fun",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_fun())) },
        Some(1), Some(1), true);
    func!(st, "is_str",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_str())) },
        Some(1), Some(1), true);
    func!(st, "is_wref",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_wref())) },
        Some(1), Some(1), true);
    func!(st, "is_ref",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_ref())) },
        Some(1), Some(1), true);
    func!(st, "is_bool",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_bool())) },
        Some(1), Some(1), true);
    func!(st, "is_bytes",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_bytes())) },
        Some(1), Some(1), true);
    func!(st, "is_sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_sym())) },
        Some(1), Some(1), true);
    func!(st, "is_float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_float())) },
        Some(1), Some(1), true);
    func!(st, "is_pair",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_pair())) },
        Some(1), Some(1), true);
    func!(st, "is_optional",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_optional())) },
        Some(1), Some(1), true);
    func!(st, "is_iter",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_iter())) },
        Some(1), Some(1), true);
    func!(st, "is_int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_int())) },
        Some(1), Some(1), true);

    func!(st, "len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).len() as i64)) },
        Some(1), Some(1), false);

    func!(st, "type",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str(env.arg(0).type_name()))
        }, Some(1), Some(1), true);

    func!(st, "cons",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::pair(env.arg(0), env.arg(1)))
        }, Some(2), Some(2), true);

    func!(st, "for",
        |env: &mut Env, _argc: usize| {
            let val = env.arg(0);
            let f   = env.arg(1);

            let mut ret = VVal::None;
            for (v, k) in val.iter() {
                env.push(v);
                let n =
                    if let Some(k) = k { env.push(k); 2 }
                    else               { 1 };
                match f.call_internal(env, n) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { env.popn(n); return Ok(*v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { env.popn(n); return Err(e); }
                }
                env.popn(n);
            }

            Ok(ret)
        }, Some(2), Some(2), false);

    func!(st, "range",
        |env: &mut Env, _argc: usize| {
            let from     = env.arg(0);
            let to       = env.arg(1);
            let step     = env.arg(2);
            let f        = env.arg(3);
            //println!("RAGEN from={} to={} f={}", from.s(), to.s(), f.s());

            if let VVal::Flt(_) = from {
                let mut from = from.f();
                let to       = to.f();
                let step     = step.f();

                let mut ret = VVal::None;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::None;
                    env.push(VVal::Flt(from));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(1); return Err(e); }
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            } else {
                let mut from = from.i();
                let to       = to.i();
                let step     = step.i();

                let mut ret = VVal::None;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::None;
                    env.push(VVal::Int(from));
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { env.popn(1); return Err(e); }
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            }
        }, Some(4), Some(4), false);

    st
}

fn print_value(env: &mut Env, argc: usize, raw: bool) -> Result<VVal, StackAction> {
    let mut write = env.stdio.write.borrow_mut();

    for i in 0..argc {
        if raw {
            env.arg_ref(i).unwrap().with_s_ref(|s: &str| {
                if i == (argc - 1) {
                    if i > 0 { write!(write, " ").ok(); }
                    writeln!(write, "{}", s).ok();
                } else {
                    if i > 0 { write!(write, " ").ok(); }
                    write!(write, "{}", s).ok();
                }
            });
        } else {
            let s = env.arg_ref(i).unwrap().s();

            if i == (argc - 1) {
                if i > 0 { write!(write, " ").ok(); }
                writeln!(write, "{}", s).ok();
            } else {
                if i > 0 { write!(write, " ").ok(); }
                write!(write, "{}", s).ok();
            }
        }
    }
    if argc == 0 {
        writeln!(write).ok();
    }
    if argc > 0 {
        Ok(env.arg(argc - 1))
    } else {
        Ok(VVal::None)
    }
}

/// Returns a SymbolTable with all WLambda standard library language symbols.
pub fn std_symbol_table() -> SymbolTable {
    let mut st = SymbolTable::new();

    func!(st, "not_i64",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(!env.arg(0).i()))
        }, Some(1), Some(1), false);
    func!(st, "neg_i64",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(env.arg(0).i().wrapping_neg()))
        }, Some(1), Some(1), false);
    func!(st, "not_u32",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(i64::from(!(env.arg(0).i() as u32))))
        }, Some(1), Some(1), false);
    func!(st, "neg_u32",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(i64::from((env.arg(0).i() as u32).wrapping_neg())))
        }, Some(1), Some(1), false);

    func!(st, "unshift",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            v.unshift(env.arg(1));
            Ok(v)
        }, Some(2), Some(2), false);

    func!(st, "push",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            v.push(env.arg(1));
            Ok(v)
        }, Some(2), Some(2), false);

    func!(st, "keys",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.keys())
        }, Some(1), Some(1), false);

    func!(st, "values",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.values())
        }, Some(1), Some(1), false);

    func!(st, "accum",
        |env: &mut Env, argc: usize| {
            let mut v = env.arg(0);
            for i in 1..argc {
                v.accum(&env.arg(i));
            }
            Ok(v)
        }, Some(2), None, false);

    func!(st, "delete",
        |env: &mut Env, _argc: usize| {
            let v   = env.arg(0);
            let key = env.arg(1);
            v.delete_key(&key)
        }, Some(2), Some(2), false);

    func!(st, "prepend",
        |env: &mut Env, argc: usize| {
            let v = env.arg(0);
            let v =
                if v.is_vec() { v }
                else {
                    let r = VVal::vec();
                    r.push(v);
                    r
                };

            for i in 1..argc {
                match env.arg(i) {
                    VVal::Lst(b) => {
                        for item in b.borrow().iter() {
                            v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                                r.insert(0, item.clone());
                            })?;
                        }
                    },
                    item => {
                        v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                            r.insert(0, item.clone());
                        })?;
                    },
                }
            }

            Ok(v)
        }, Some(1), None, false);

    func!(st, "append",
        |env: &mut Env, argc: usize| {
            let v = env.arg(0);
            let v =
                if v.is_vec() { v }
                else {
                    let r = VVal::vec();
                    r.push(v);
                    r
                };

            for i in 1..argc {
                match env.arg(i) {
                    VVal::Lst(b) => {
                        for item in b.borrow().iter() {
                            v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                                r.push(item.clone());
                            })?;
                        }
                    },
                    item => {
                        v.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                            r.push(item.clone());
                        })?;
                    }
                }
            }

            Ok(v)
        }, Some(1), None, false);

    func!(st, "pop",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.pop())
        }, Some(1), Some(1), false);

    func!(st, "take",
        |env: &mut Env, _argc: usize| {
            let cnt = env.arg(0).i() as usize;
            let lst = env.arg(1);

            lst.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                let svec : Vec<VVal> =
                    r.iter().take(cnt).cloned().collect();
                VVal::vec_mv(svec)
            })
        }, Some(2), Some(2), false);

    func!(st, "drop",
        |env: &mut Env, _argc: usize| {
            let cnt = env.arg(0).i() as usize;
            let lst = env.arg(1);

            lst.list_operation(|r: &mut std::cell::RefMut<Vec<VVal>>| {
                let svec : Vec<VVal> =
                    r.iter().skip(cnt).cloned().collect();
                VVal::vec_mv(svec)
            })
        }, Some(2), Some(2), false);

    func!(st, "ser:wlambda",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s())) },
        Some(1), Some(1), true);
    func!(st, "str:len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).s_len() as i64)) },
        Some(1), Some(1), false);
    func!(st, "str:to_lowercase",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.to_lowercase()))) },
        Some(1), Some(1), false);
    func!(st, "str:to_uppercase",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.to_uppercase()))) },
        Some(1), Some(1), false);
    func!(st, "str:trim",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.trim().to_string()))) },
        Some(1), Some(1), false);
    func!(st, "str:trim_start",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.trim_start().to_string()))) },
        Some(1), Some(1), false);
    func!(st, "str:trim_end",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(
                env.arg_ref(0).unwrap().with_s_ref(|s: &str| s.trim_end().to_string()))) },
        Some(1), Some(1), false);
    func!(st, "str:pad_start",
        |env: &mut Env, _argc: usize| {
            let len   = env.arg(0).i() as usize;
            let pads  = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            let mut src_len = s.chars().count();
            let pad_len     = pads.chars().count();

            if pad_len == 0 {
                return Ok(VVal::new_str_mv(s));
            }

            while src_len < len {
                if len - src_len < pad_len {
                    for c in pads.chars().rev() {
                        s.insert(0, c);
                        src_len += 1;
                        if src_len >= len {
                            break;
                        }
                    }
                } else {
                    s.insert_str(0, &pads);
                }
                src_len += pad_len;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3), false);

    func!(st, "str:pad_end",
        |env: &mut Env, _argc: usize| {
            let len   = env.arg(0).i() as usize;
            let pads  = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            let mut src_len = s.chars().count();
            let pad_len     = pads.chars().count();

            if pad_len == 0 {
                return Ok(VVal::new_str_mv(s));
            }

            while src_len < len {
                if len - src_len < pad_len {
                    for c in pads.chars() {
                        s.push(c);
                        src_len += 1;
                        if src_len >= len {
                            break;
                        }
                    }
                } else {
                    s.push_str(&pads);
                }
                src_len += pad_len;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3), false);
    func!(st, "str:cat",
        |env: &mut Env, argc: usize| {
            let mut s = String::from("");
            for i in 0..argc {
                let aref = env.arg_ref(i).unwrap();
                if let VVal::Lst(l) = aref {
                    for v in l.borrow().iter() {
                        v.with_s_ref(|vs: &str| s.push_str(vs));
                    }
                } else {
                    env.arg_ref(i).unwrap()
                       .with_s_ref(|vs: &str| s.push_str(vs))
                }
            }
            Ok(VVal::new_str_mv(s))
        }, None, None, false);
    func!(st, "str:replace_n",
        |env: &mut Env, _argc: usize| {
            let cnt  = env.arg(2).i() as usize;
            env.arg_ref(0).unwrap().with_s_ref(|pat: &str|
                env.arg_ref(1).unwrap().with_s_ref(|to: &str|
                    env.arg_ref(3).unwrap().with_s_ref(|data: &str|
                        Ok(VVal::new_str_mv(data.replacen(pat, to, cnt))))))
        }, Some(4), Some(4), false);
    func!(st, "str:replace",
        |env: &mut Env, _argc: usize| {
            env.arg_ref(0).unwrap().with_s_ref(|pat: &str|
                env.arg_ref(1).unwrap().with_s_ref(|to: &str|
                    env.arg_ref(2).unwrap().with_s_ref(|data: &str|
                        Ok(VVal::new_str_mv(data.replace(pat, to))))))
        }, Some(3), Some(3), false);
    func!(st, "str:join",
        |env: &mut Env, _argc: usize| {
            let sep = env.arg(0);
            let lst = env.arg(1);
            if let VVal::Lst(l) = lst {
                let mut s = VVal::new_str("");
                let mut first = true;
                for item in l.borrow().iter() {
                    if !first {
                        s.accum(&sep);
                    } else {
                        first = false;
                    }
                    s.accum(item);
                }
                Ok(s)

            } else {
                Ok(env.new_err(
                    format!(
                        "str:join only works with lists as second argument, got '{}'",
                        lst.s())))
            }
        }, Some(2), Some(2), false);
    func!(st, "str:from_utf8_lossy",
        |env: &mut Env, _argc: usize| {
            let b = env.arg(0);
            Ok(
                if let VVal::Byt(u) = b {
                    VVal::new_str_mv(String::from_utf8_lossy(u.as_ref()).to_string())
                } else {
                    VVal::None
                })
        }, Some(1), Some(1), false);
    func!(st, "str:from_utf8",
        |env: &mut Env, _argc: usize| {
            let b = env.arg(0);
            if let VVal::Byt(u) = b {
                match String::from_utf8(u.to_vec()) {
                    Ok(s) => Ok(VVal::new_str_mv(s)),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("str:from_utf8 decoding error: {}", e)))
                    }
                }
            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), false);

    func!(st, "str:to_char_vec",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::vec_mv(
                env.arg_ref(0).unwrap().with_s_ref(|arg: &str|
                    arg.chars()
                    .map(|c| VVal::Int(i64::from(c as u32)))
                    .collect())))
        }, Some(1), Some(1), false);

    func!(st, "str:from_char_vec",
        |env: &mut Env, _argc: usize| {
            let mut s = String::new();
            for (vc, _) in env.arg(0).iter() {
                s.push(std::char::from_u32(vc.i() as u32).unwrap_or('?'));
            }
            Ok(VVal::new_str_mv(s))
        }, Some(1), Some(1), false);

    func!(st, "str:to_bytes",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_byt(env.arg(0).as_bytes()))
        }, Some(1), Some(1), false);

    func!(st, "bytes:from_vec",
        |env: &mut Env, _argc: usize| {
            if let VVal::Lst(u) = env.arg(0) {
                Ok(VVal::new_byt(u.borrow().iter().map(|v| v.i() as u8).collect()))

            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), false);

    func!(st, "bytes:to_vec",
        |env: &mut Env, _argc: usize| {
            if let VVal::Byt(u) = env.arg(0) {
                Ok(VVal::vec_mv(
                    u.iter()
                     .map(|u| VVal::Int(i64::from(*u)))
                     .collect()))
            } else {
                Ok(VVal::vec_mv(
                    env.arg(0).as_bytes().iter()
                        .map(|u| VVal::Int(i64::from(*u)))
                        .collect()))
            }
        }, Some(1), Some(1), false);

    func!(st, "bytes:from_hex",
        |env: &mut Env, _argc: usize| {
            env.arg_ref(0).unwrap().with_s_ref(|s: &str| {
                let out : Vec<u8> = Vec::with_capacity((s.len() + 1) / 2);
                Ok(VVal::new_byt(
                    s.chars()
                     .map(|c|
                         match c { '0'..='9' => i16::from( 9 - (b'9' - (c as u8))),
                                   'a'..='f' => i16::from(15 - (b'f' - (c as u8))),
                                   'A'..='F' => i16::from(15 - (b'F' - (c as u8))),
                                   _ => -1 })
                     .fold((256, out), |(last, mut out), c: i16|
                         if c == -1 { (last, out) }
                         else if last == 256 { (c, out) }
                         else {
                             out.push((((last << 4) | (c & 0x0F)) & 0xFF) as u8);
                             (256, out)
                         }).1))
            })
        }, Some(1), Some(1), false);

    func!(st, "bytes:to_hex",
        |env: &mut Env, argc: usize| {
            static HEXCHARS : &[char] =
                &['0', '1', '2', '3', '4', '5', '6', '7',
                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

            if let VVal::Byt(u) = env.arg(0) {
                let mut out : String =
                    String::with_capacity(u.len() * 2);

                if argc == 1 {
                    for (a, b) in u.iter().map(|u|
                                        (HEXCHARS[(u >> 4) as usize],
                                         HEXCHARS[(u & 0x0F) as usize])) {
                        out.push(a);
                        out.push(b);
                    }
                } else {
                    let group_len = env.arg(1).i();
                    let group_sep =
                        if env.arg(2).is_none() { String::from(" ") }
                        else { env.arg(2).s_raw() };

                    let mut len_counter = 0;
                    for (a, b) in u.iter().map(|u|
                                        (HEXCHARS[(u >> 4) as usize],
                                         HEXCHARS[(u & 0x0F) as usize])) {
                        if len_counter >= group_len { out.push_str(&group_sep); len_counter = 0; }
                        out.push(a);
                        len_counter += 1;
                        if len_counter >= group_len { out.push_str(&group_sep); len_counter = 0; }
                        out.push(b);
                        len_counter += 1;
                    }
                }

                Ok(VVal::new_str_mv(out))

            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(3), false);

    func!(st, "to_no_arity",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            Ok(v.disable_function_arity())
        }, Some(1), Some(1), false);

    func!(st, "to_drop",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(0).disable_function_arity();

            Ok(VVal::DropFun(Rc::new(DropFun { fun })))
        }, Some(1), Some(1), false);

    func!(st, "fold",
        |env: &mut Env, _argc: usize| {
            let mut acc = env.arg(0);
            let f       = env.arg(1);
            let lst     = env.arg(2);

            for (i, _) in lst.iter() {
                env.push(i);
                env.push(acc.clone());
                let rv = f.call_internal(env, 2);
                env.popn(2);

                match rv {
                    Ok(v)                      => { acc = v;  },
                    Err(StackAction::Break(v)) => { acc = *v; break; },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { return Err(e); },
                }
            }

            Ok(acc)
        }, Some(3), Some(3), false);

    func!(st, "enumerate",
        |env: &mut Env, _argc: usize| {
            let f = env.arg(0);
            let i = Rc::new(std::cell::RefCell::new(0));

            Ok(VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    env.push(VVal::Int(*i.borrow() as i64));
                    let r = f.call_internal(env, 1 + argc);
                    *i.borrow_mut() += 1;
                    env.popn(1);
                    r
                }, None, None, false))
        }, Some(1), Some(1), false);

    func!(st, "zip",
        |env: &mut Env, _argc: usize| {
            let o = env.arg(0);
            let f = env.arg(1);
            let i = Rc::new(std::cell::RefCell::new(0));

            Ok(VValFun::new_fun(
                move |env: &mut Env, argc: usize| {
                    env.push(o.at(*i.borrow()).unwrap_or(VVal::None));
                    let r = f.call_internal(env, 1 + argc);
                    *i.borrow_mut() += 1;
                    env.popn(1);
                    r
                }, None, None, false))
        }, Some(2), Some(2), false);

    add_num_fun_flt!(st, "num:ceil",       ceil);
    add_num_fun_flt!(st, "num:sqrt",       sqrt);
    add_num_fun_flt!(st, "num:cbrt",       cbrt);
    add_num_fun_flt!(st, "num:floor",      floor);
    add_num_fun_flt!(st, "num:round",      round);
    add_num_fun_flt!(st, "num:signum",     signum);
    add_num_fun_flt!(st, "num:abs",        abs);
    add_num_fun_flt!(st, "num:trunc",      trunc);
    add_num_fun_flt!(st, "num:to_degrees", to_degrees);
    add_num_fun_flt!(st, "num:to_radians", to_radians);
    add_num_fun_flt!(st, "num:tan",        tan);
    add_num_fun_flt!(st, "num:tanh",       tanh);
    add_num_fun_flt!(st, "num:sin",        sin);
    add_num_fun_flt!(st, "num:sinh",       sinh);
    add_num_fun_flt!(st, "num:cos",        cos);
    add_num_fun_flt!(st, "num:cosh",       cosh);
    add_num_fun_flt!(st, "num:asin",       asin);
    add_num_fun_flt!(st, "num:asinh",      asinh);
    add_num_fun_flt!(st, "num:acos",       acos);
    add_num_fun_flt!(st, "num:acosh",      acosh);
    add_num_fun_flt!(st, "num:recip",      recip);
    add_num_fun_flt!(st, "num:log2",       log2);
    add_num_fun_flt!(st, "num:log10",      log10);
    add_num_fun_flt!(st, "num:ln",         ln);
    add_num_fun_flt!(st, "num:exp_m1",     exp_m1);
    add_num_fun_flt!(st, "num:exp",        exp);
    add_num_fun_flt!(st, "num:exp2",       exp2);
    add_num_fun_flt!(st, "num:atan",       atan);
    add_num_fun_flt!(st, "num:atanh",      atanh);

    add_num_fun_flt2!(st, "num:log",        log);
    add_num_fun_flt2!(st, "num:atan2",      atan2);
    add_num_fun_flt2!(st, "num:hypot",      hypot);
    add_num_fun_flt2!(st, "num:pow",        powf);

    func!(st, "num:abs",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::Int(i) => VVal::Int(i.abs()),
                VVal::Flt(i) => VVal::Flt(i.abs()),
                _ => VVal::Int(env.arg(0).i().abs())
            })
        }, Some(1), Some(1), false);

    func!(st, "num:lerp",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0).f();
            let b = env.arg(1).f();
            let x = env.arg(2).f();
            Ok(VVal::Flt(a * (1.0 - x) + b * x))
        }, Some(3), Some(3), false);

    func!(st, "num:smoothstep",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0).f();
            let b = env.arg(1).f();
            let x = env.arg(2).f();
            let x = (x - a) / (b - a);
            let x = x.max(0.0).min(1.0);
            Ok(VVal::Flt(x * x * (3.0 - (2.0 * x))))
        }, Some(3), Some(3), false);

    func!(st, "fs:rename",
        |env: &mut Env, _argc: usize| {
            let from = env.arg(0);
            let to   = env.arg(1);
            from.with_s_ref(|from| to.with_s_ref(|to| {
                if let Err(e) = std::fs::rename(&from, &to) {
                    return Ok(env.new_err(
                        format!(
                            "Couldn't rename file '{}' to file '{}': {}",
                            from, to, e)));
                }

                Ok(VVal::Bol(true))
            }))
        }, Some(2), Some(2), false);

    func!(st, "io:lines",
        |env: &mut Env, _argc: usize| {
            let f = env.arg(0);
            let mut ret = VVal::None;
            loop {
                let mut line = String::new();
                {
                    let mut read = env.stdio.read.borrow_mut();
                    match read.read_line(&mut line) {
                        Ok(n) => { if n == 0 { break; } },
                        Err(e) => {
                            return Ok(env.new_err(
                                format!("IO-Error on std:io:lines: {}", e)))
                        },
                    }
                }

                env.push(VVal::new_str_mv(line));
                match f.call_internal(env, 1) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { env.popn(1); return Ok(*v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { env.popn(1); return Err(e); }
                }
                env.popn(1);
            }

            Ok(ret)
        }, Some(1), Some(1), false);

    func!(st, "io:stdout:flush",
        |env: &mut Env, _argc: usize| {
            if let Err(e) = env.stdio.write.borrow_mut().flush() {
                Ok(env.new_err(
                    format!("IO-Error on std:io:stdout:flush: {}", e)))
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(0), Some(0), false);

    func!(st, "io:stdout:newline",
        |env: &mut Env, _argc: usize| {
            if let Err(e) = writeln!(*env.stdio.write.borrow_mut(), "") {
                Ok(env.new_err(
                    format!("IO-Error on std:io:stdout:newline: {}", e)))
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(0), Some(0), false);

    func!(st, "io:stdout:write",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            if let Err(e) = write!(*env.stdio.write.borrow_mut(), "{}", v.s()) {
                Ok(env.new_err(
                    format!("IO-Error on std:io:stdout:write: {}", e)))
            } else {
                Ok(v)
            }
        }, Some(1), Some(1), false);

    func!(st, "io:stdout:print",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            env.arg_ref(0).unwrap().with_s_ref(|vs: &str| {
                if let Err(e) = write!(*env.stdio.write.borrow_mut(), "{}", vs) {
                    Ok(env.new_err(
                       format!("IO-Error on std:io:stdout:print: {}", e)))
                } else {
                    Ok(v)
                }
            })
        }, Some(1), Some(1), false);

    func!(st, "io:file:read_text",
        |env: &mut Env, _argc: usize| {
            let filename = env.arg(0).s_raw();

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .write(false)
                .create(false)
                .read(true)
                .open(&filename);

            match file {
                Err(e) => {
                    Ok(env.new_err(
                        format!("Couldn't open file '{}': {}", filename, e)))
                },
                Ok(mut f) => {
                    let mut contents = String::new();
                    if let Err(e) = f.read_to_string(&mut contents) {
                        Ok(env.new_err(
                            format!(
                                "Couldn't read text from file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::new_str_mv(contents))
                    }
                },
            }
        }, Some(1), Some(1), false);

    func!(st, "io:file:read",
        |env: &mut Env, _argc: usize| {
            let filename = env.arg(0).s_raw();

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .write(false)
                .create(false)
                .read(true)
                .open(&filename);

            match file {
                Err(e) => {
                    Ok(env.new_err(
                        format!("Couldn't open file '{}': {}", filename, e)))
                },
                Ok(mut f) => {
                    let mut contents : Vec<u8> = Vec::new();
                    if let Err(e) = f.read_to_end(&mut contents) {
                        Ok(env.new_err(
                            format!(
                                "Couldn't read text from file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::new_byt(contents))
                    }
                },
            }
        }, Some(1), Some(1), false);

    func!(st, "io:file:copy",
        |env: &mut Env, _argc: usize| {
            let from = env.arg(0).s_raw();
            let to   = env.arg(1).s_raw();

            use std::path::Path;

            match std::fs::copy(Path::new(&from), Path::new(&to)) {
                Ok(_) => Ok(VVal::Bol(true)),
                Err(e) => {
                    Ok(env.new_err(
                        format!(
                            "Couldn't copy file '{}' to file '{}': {}",
                            from, to, e)))
                },
            }
        }, Some(2), Some(2), false);

    func!(st, "io:file:write_safe",
        |env: &mut Env, _argc: usize| {
            let filename     = env.arg(0).s_raw();
            let tmp_filename = format!("{}~", filename);
            let contents     = env.arg(1);
            let buf = match contents {
                VVal::Byt(b) => b.as_ref().clone(), // TODO: Remove clone
                v            => v.with_s_ref(|v: &str| v.as_bytes().to_vec()),
            };

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(&tmp_filename);

            match file {
                Err(e) => {
                    Ok(env.new_err(
                        format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    if let Err(e) = f.write_all(&buf) {
                        return Ok(env.new_err(
                            format!(
                                "Couldn't write to file '{}': {}",
                                tmp_filename, e)));
                    }

                    if let Err(e) = std::fs::rename(&tmp_filename, &filename) {
                        return Ok(env.new_err(
                            format!(
                                "Couldn't rename file '{}' to file '{}': {}",
                                tmp_filename, filename, e)));
                    }

                    Ok(VVal::Bol(true))
                },
            }
        }, Some(2), Some(2), false);

    func!(st, "io:file:append",
        |env: &mut Env, _argc: usize| {
            let filename     = env.arg(0).s_raw();
            let contents     = env.arg(1);
            let buf = match contents {
                VVal::Byt(b) => b.as_ref().clone(), // TODO: Remove clone
                v            => v.with_s_ref(|v: &str| v.as_bytes().to_vec()),
            };

            use std::io::prelude::*;
            use std::fs::OpenOptions;

            let file =
                OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open(&filename);

            match file {
                Err(e) => {
                    Ok(env.new_err(
                        format!(
                            "Couldn't open file '{}': {}",
                            filename, e)))
                },
                Ok(mut f) => {
                    if let Err(e) = f.write_all(&buf) {
                        Ok(env.new_err(
                            format!(
                                "Couldn't write to file '{}': {}",
                                filename, e)))
                    } else {
                        Ok(VVal::Bol(true))
                    }
                },
            }
        }, Some(2), Some(2), false);

    func!(st, "writeln",
        |env: &mut Env, argc: usize| {
            print_value(env, argc, false)
        }, None, None, false);

    func!(st, "displayln",
        |env: &mut Env, argc: usize| {
            print_value(env, argc, true)
        }, None, None, false);

    func!(st, "dump_upvals",
        |env: &mut Env, _argc: usize| {
            if let VVal::Fun(f) = env.arg(0).deref() {
                return Ok(f.dump_upvals());
            }
            Ok(VVal::None)
        }, Some(1), Some(1), false);

    func!(st, "wlambda:sizes",
        |env: &mut Env, _argc: usize| {
            let mut write = env.stdio.write.borrow_mut();
            use crate::compiler::*;
            use crate::ops::*;
            use crate::vval::*;
            use crate::str_int::*;
            sizeof_writeln!(write, VVal);
            sizeof_writeln!(write, Op);
            sizeof_writeln!(write, Builtin);
            sizeof_writeln!(write, NVecPos);
            sizeof_writeln!(write, Prog);
            sizeof_writeln!(write, ResPos);
            sizeof_writeln!(write, (ResPos, Box<Symbol>, ResPos));
            sizeof_writeln!(write, (ResPos, Box<String>, u16, ResPos));
            sizeof_writeln!(write, (ResPos, ResPos, u16, ResPos));
            sizeof_writeln!(write, (ResPos, ResPos, ResPos, ResPos));
            sizeof_writeln!(write, (ResPos, DirectFun, ResPos));
            sizeof_writeln!(write, (DirectFun, ResPos, ResPos));
            sizeof_writeln!(write, (Box<String>, ResPos, ResPos, u16));
            sizeof_writeln!(write, SynPos);
            sizeof_writeln!(write, Symbol);
            sizeof_writeln!(write, Option<Rc<VVal>>);
            sizeof_writeln!(write, crate::nvec::NVec<f64>);
            sizeof_writeln!(write, Result<VVal, StackAction>);
            sizeof_writeln!(write, StackAction);
            sizeof_writeln!(write, Box<String>);
            sizeof_writeln!(write, Box<Vec<VVal>>);
            sizeof_writeln!(write, Vec<VVal>);
            Ok(VVal::None)
        }, Some(0), Some(0), false);

////    println!("sizeof OP:{} bytes", std::mem::size_of::<(ResPos, Box<String>, Box<String>, Box<String>, ResPos)>());

    func!(st, "wlambda:version",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::new_str(VERSION))
        }, Some(0), Some(0), false);

    func!(st, "measure_time",
        |env: &mut Env, _argc: usize| {
            use std::convert::TryFrom;
            let t = std::time::Instant::now();
            let unit = env.arg(0).s_raw();
            match env.arg(1).call_no_args(env) {
                Ok(v) => {
                    let ret = VVal::vec();
                    match &unit[..] {
                        "s"  => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_secs())  .unwrap_or(0))); },
                        "ms" => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_millis()).unwrap_or(0))); },
                        "us" => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_micros()).unwrap_or(0))); },
                        "ns" => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_nanos()) .unwrap_or(0))); },
                        _    => { ret.push(VVal::Int(i64::try_from(t.elapsed().as_millis()).unwrap_or(0))); },
                    }
                    ret.push(v);
                    Ok(ret)
                },
                Err(e) => Err(e),
            }
        }, Some(2), Some(2), false);

    func!(st, "assert_eq",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);
            if !a.eqv(&b) {
                if env.arg(2).is_none() {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion failed: expected: '{}', got: '{}'",
                            b.s(), a.s())))
                } else {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion '{}' failed: expected: '{}', got: '{}'",
                            env.arg(2).s_raw(), b.s(), a.s())))
                }
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(2), Some(3), true);

    func!(st, "assert_str_eq",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0).s();
            let b = env.arg(1).s();

            if a != b {
                if env.arg(2).is_none() {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion failed: expected: '{}', got: '{}'",
                            b, a)))
                } else {
                    Err(StackAction::panic_msg(
                        format!(
                            "assertion '{}' failed: expected: '{}', got: '{}'",
                            env.arg(2).s_raw(), b, a)))
                }
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(2), Some(3), true);

    func!(st, "assert_rel_eq",
        |env: &mut Env, _argc: usize| {
            let l = env.arg(0);
            let r = env.arg(1);
            let epsilon = env.arg(2).f();
            let delta = (l.f() - r.f()).abs();

            if delta < epsilon {
                Ok(VVal::Bol(true))
            } else {
                Err(StackAction::panic_msg(format!(
                    "assertion{}failed: delta[{}] was more than epsilon[{}],\
                        left['{}', f:'{}'], right['{}', f:'{}']",
                    if env.arg(3).is_none() {
                        " ".to_string()
                    } else {
                        format!(" '{}' ", env.arg(3).s_raw())
                    },
                    delta, epsilon, l.s(), l.f(), r.s(), r.f()
                )))
            }
        }, Some(3), Some(4), true);

    func!(st, "to_ref",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).to_ref())
        }, Some(1), Some(1), true);

    func!(st, "ref_id",
        |env: &mut Env, _argc: usize| {
            if let Some(id) = env.arg_ref(0).unwrap_or(&VVal::None).ref_id() {
                Ok(VVal::Int(id))
            } else {
                Ok(VVal::None)
            }
        }, Some(1), Some(1), true);

    func!(st, "ref:set",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).set_ref(env.arg(1)))
        }, Some(2), Some(2), false);

    func!(st, "ref:strengthen",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).upgrade())
        }, Some(1), Some(1), false);

    func!(st, "ref:hide",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).hide_ref())
        }, Some(1), Some(1), false);

    func!(st, "ref:weaken",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).downgrade())
        }, Some(1), Some(1), false);

    func!(st, "assert",
        |env: &mut Env, _argc: usize| {
            if !env.arg(0).b() {
                if env.arg(1).is_none() {
                    Err(StackAction::panic_msg("assertion failed".to_string()))
                } else {
                    Err(StackAction::panic_msg(format!("assertion failed '{}'", env.arg(1).s_raw())))
                }
            } else {
                Ok(env.arg(0))
            }
        }, Some(1), Some(2), true);

    func!(st, "pattern",
        |env: &mut Env, _argc: usize| {
            let pat_src = env.arg_ref(0).cloned().unwrap_or_else(|| VVal::None);
            let res_ref =
                env.global.borrow_mut()
                   .get_var_ref("\\")
                   .unwrap_or_else(|| VVal::None);
            pat_src.with_s_ref(|pat_src|
                match create_regex_find_function(pat_src, res_ref) {
                    Ok(fun) => Ok(fun),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("bad pattern: {}, pattern was: /{}/",
                                    e, pat_src)))
                    }
                })
        }, Some(1), Some(1), false);

    func!(st, "selector",
        |env: &mut Env, _argc: usize| {
            let pat_src = env.arg_ref(0).cloned().unwrap_or_else(|| VVal::None);
            let res_ref =
                env.global.borrow_mut()
                   .get_var_ref("\\")
                   .unwrap_or_else(|| VVal::None);
            pat_src.with_s_ref(|sel_src|
                match create_selector_function(sel_src, res_ref) {
                    Ok(fun) => Ok(fun),
                    Err(e) => {
                        Ok(env.new_err(
                            format!("bad selector: {}, selector was: /{}/",
                                    e, sel_src)))
                    }
                })
        }, Some(1), Some(1), false);

//    func!(st, "tree_select",
//        |env: &mut Env, _argc: usize| {
//            let slct = env.arg(0);
//            let tree = env.arg(1);
//            Ok(util::tree_select(&slct, &tree))
//        }, Some(2), Some(2), false);

    func!(st, "ser:csv",
        |env: &mut Env, _argc: usize| {
            use crate::csv;
            let delim =
                if env.arg(0).is_none() {
                    ",".to_string()
                } else {
                    env.arg(0).s_raw()
                };
            let row_sep =
                if env.arg(1).is_none() {
                    "\r\n".to_string()
                } else {
                    env.arg(1).s_raw()
                };
            let escape_all = env.arg(2).b();
            let val = env.arg(3);

            Ok(VVal::new_str_mv(csv::to_csv(
                delim.chars().next().unwrap_or(','),
                &row_sep,
                escape_all,
                val)))
        }, Some(4), Some(4), false);

    func!(st, "deser:csv",
        |env: &mut Env, _argc: usize| {
            use crate::csv;
            let delim =
                if env.arg(0).is_none() {
                    ",".to_string()
                } else {
                    env.arg(0).s_raw()
                };
            let row_sep =
                if env.arg(1).is_none() {
                    "\r\n".to_string()
                } else {
                    env.arg(1).s_raw()
                };

            env.arg_ref(2).unwrap().with_s_ref(|data: &str| {
                match csv::parse_csv(
                        delim.chars().next().unwrap_or(','),
                        &row_sep,
                        data)
                {
                    Ok(v) => Ok(v),
                    Err(e) => Ok(env.new_err(e)),
                }
            })
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:replace_all",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);
            let f    = env.arg(1);
            let text = env.arg(2);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            let mut finished = false;
            let mut ret = Ok(VVal::None);
            let ret_str = text.with_s_ref(|text: &str| {
                VVal::new_str_mv(String::from(
                    rx.replace_all(&text, |capts: &regex::Captures| {
                        let captures = VVal::vec();
                        for cap in capts.iter() {
                            match cap {
                                None    => { captures.push(VVal::None); },
                                Some(c) => {
                                    captures.push(VVal::new_str(c.as_str()));
                                }
                            }
                        }

                        let repl = captures.at(0).unwrap_or(VVal::None).s_raw();
                        if finished { return repl; }

                        if f.is_fun() {
                            env.push(captures);
                            let rv = f.call_internal(env, 1);
                            env.popn(1);

                            match rv {
                                Ok(v)                      => v.s_raw(),
                                Err(StackAction::Break(v)) => { finished = true; v.s_raw() },
                                Err(StackAction::Next)     => { repl },
                                Err(e)                     => { finished = true; ret = Err(e); repl },
                            }
                        } else {
                            f.s_raw()
                        }
                    })))
            });
            if ret.is_err() { return ret; }
            Ok(ret_str)
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:match_compile",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            Ok(VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    let text = env.arg(0);
                    let f    = env.arg(1);

                    text.with_s_ref(|text: &str| {
                        match rx.captures(text) {
                            Some(c) => {
                                let captures = VVal::vec();
                                for cap in c.iter() {
                                    match cap {
                                        None    => { captures.push(VVal::None); },
                                        Some(c) => {
                                            captures.push(VVal::new_str(c.as_str()));
                                        }
                                    }
                                }
                                env.push(captures);
                                let rv = f.call_internal(env, 1);
                                env.popn(1);
                                rv
                            },
                            None => {
                                Ok(VVal::None)
                            }
                        }
                    })
                }, Some(2), Some(2), false))
        }, Some(1), Some(1), false);

    #[cfg(feature="regex")]
    func!(st, "re:match",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);
            let text = env.arg(1);
            let f    = env.arg(2);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            text.with_s_ref(|text: &str| {
                match rx.captures(text) {
                    Some(c) => {
                        let captures = VVal::vec();
                        for cap in c.iter() {
                            match cap {
                                None    => { captures.push(VVal::None); },
                                Some(c) => {
                                    captures.push(VVal::new_str(c.as_str()));
                                }
                            }
                        }
                        env.push(captures);
                        let rv = f.call_internal(env, 1);
                        env.popn(1);
                        rv
                    },
                    None => {
                        Ok(VVal::None)
                    }
                }
            })
        }, Some(3), Some(3), false);

    #[cfg(feature="regex")]
    func!(st, "re:map",
        |env: &mut Env, _argc: usize| {
            use regex::Regex;
            let re   = env.arg(0);
            let f    = env.arg(1);
            let text = env.arg(2);

            let rx = re.with_s_ref(|re: &str| Regex::new(&re));
            if let Err(e) = rx {
                return Ok(env.new_err(
                    format!("Regex '{}' did not compile: {}", re.s_raw(), e)));
            }
            let rx = rx.unwrap();

            let mut ret = VVal::None;
            text.with_s_ref(|text: &str| {
                for capts in rx.captures_iter(text) {
                    let captures = VVal::vec();
                    for cap in capts.iter() {
                        match cap {
                            None    => { captures.push(VVal::None); },
                            Some(c) => {
                                captures.push(VVal::new_str(c.as_str()));
                            }
                        }
                    }
                    env.push(captures);
                    let rv = f.call_internal(env, 1);
                    env.popn(1);

                    match rv {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { ret = *v; break; },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); },
                    }
                }
                Ok(ret)
            })
        }, Some(3), Some(3), false);

    #[cfg(feature="chrono")]
    func!(st, "chrono:timestamp",
        |env: &mut Env, _argc: usize| {
            use chrono::prelude::*;
            let dt = Local::now();

            let fmt = env.arg(0);
            if fmt.is_str() {
                fmt.with_s_ref(|fmt: &str|
                    Ok(VVal::new_str_mv(dt.format(fmt).to_string())))
            } else {
                Ok(VVal::new_str_mv(dt.format("%Y-%m-%d %H:%M:%S.%f").to_string()))
            }

        }, Some(0), Some(1), false);

    #[cfg(feature="serde_json")]
    func!(st, "ser:json",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            let pp = env.arg(1).b();

            match v.to_json(pp) {
                Ok(s) => Ok(VVal::new_str_mv(s)),
                Err(e) => Ok(env.new_err(e)),
            }
        }, Some(1), Some(2), false);

    #[cfg(feature="serde_json")]
    func!(st, "deser:json",
        |env: &mut Env, _argc: usize| {
            env.arg_ref(0).unwrap().with_s_ref(
                |json_txt: &str|
                    match VVal::from_json(json_txt) {
                        Ok(v) => Ok(v),
                        Err(e) => Ok(env.new_err(e)),
                    })
        }, Some(1), Some(1), false);

    #[cfg(feature="rmp-serde")]
    func!(st, "ser:msgpack",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            match v.to_msgpack() {
                Ok(s) => Ok(VVal::new_byt(s)),
                Err(e) => Ok(env.new_err(e)),
            }
        }, Some(1), Some(1), false);

    #[cfg(feature="rmp-serde")]
    func!(st, "deser:msgpack",
        |env: &mut Env, _argc: usize| {
            if let VVal::Byt(u) = env.arg(0) {
                match VVal::from_msgpack(&u[..]) {
                    Ok(v) => Ok(v),
                    Err(e) => Ok(env.new_err(e)),
                }
            } else {
                Ok(env.new_err("deser:msgpack expects bytes".to_string()))
            }
        }, Some(1), Some(1), false);

    func!(st, "copy",
        |env: &mut Env, _argc: usize| {
            Ok(env.arg(0).shallow_clone())
        }, Some(1), Some(1), false);

    func!(st, "cmp:num:asc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_num(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "cmp:num:desc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_num(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "cmp:str:asc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_str(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);

    func!(st, "cmp:str:desc",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).compare_str(&env.arg(1)) {
                std::cmp::Ordering::Greater => Ok(VVal::Int(1)),
                std::cmp::Ordering::Less    => Ok(VVal::Int(-1)),
                std::cmp::Ordering::Equal   => Ok(VVal::Int(0)),
            }
        }, Some(2), Some(2), false);


    func!(st, "v:dims",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(match env.arg(0) {
                VVal::FVec(fv) => fv.dims(),
                v => v.nvec::<i64>().dims(),
            } as i64))
        }, Some(1), Some(1), false);

    func!(st, "v:mag2",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(match env.arg(0) {
                VVal::FVec(fv) => fv.mag2(),
                v => v.nvec::<i64>().mag2(),
            }))
        }, Some(1), Some(1), false);

    func!(st, "v:mag",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(match env.arg(0) {
                VVal::FVec(fv) => fv.mag(),
                v => v.nvec::<i64>().mag(),
            }))
        }, Some(1), Some(1), false);

    func!(st, "v:norm",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.norm())),
                v => VVal::IVec(Box::new(v.nvec::<i64>().norm())),
            })
        }, Some(1), Some(1), false);

    func!(st, "v:dot",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::Flt(fv.dot(env.arg(1).nvec())),
                v => VVal::Int(v.nvec::<i64>().dot(env.arg(1).nvec())),
            })
        }, Some(2), Some(2), false);

    func!(st, "v:cross",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.cross(env.arg(1).nvec()))),
                v => VVal::IVec(Box::new(v.nvec::<i64>().cross(env.arg(1).nvec()))),
            })
        }, Some(2), Some(2), false);

    func!(st, "v:lerp",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.lerp(env.arg(1).nvec(), env.arg(2).f()))),
                v => VVal::IVec(Box::new(v.nvec::<i64>().lerp(env.arg(1).nvec(), env.arg(2).f()))),
            })
        }, Some(3), Some(3), false);

    func!(st, "v:slerp",
        |env: &mut Env, _argc: usize| {
            Ok(match env.arg(0) {
                VVal::FVec(fv) => VVal::FVec(Box::new(fv.slerp(env.arg(1).nvec(), env.arg(2).f()))),
                v => VVal::IVec(Box::new(v.nvec::<i64>().slerp(env.arg(1).nvec(), env.arg(2).f()))),
            })
        }, Some(3), Some(3), false);

    func!(st, "v:vec2rad",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Flt(match env.arg(0) {
                VVal::FVec(fv) => fv.vec2rad(),
                v => v.nvec::<i64>().vec2rad(),
            }))
        }, Some(1), Some(1), false);

    func!(st, "v:rad2vec",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::FVec(Box::new(crate::nvec::NVec::rad2vec(env.arg(0).f()))))
        }, Some(1), Some(1), false);

//    func!(st, "v:hex2hsva_i",
//        |env: &mut Env, _argc: usize| {
//        }, Some(1), Some(1), false);
//
//    func!(st, "v:hex2hsva_f",
//        |env: &mut Env, _argc: usize| {
//        }, Some(1), Some(1), false);

    func!(st, "v:hex2rgba_i",
        |env: &mut Env, _argc: usize| {
            let (r, g, b, a) =
                env.arg(0).with_s_ref(|s| util::hex2rgba(s));
            Ok(VVal::IVec(Box::new(NVec::from_tpl(
                (r as i64, g as i64, Some(b as i64), Some(a as i64))).unwrap())))
        }, Some(1), Some(1), false);

    func!(st, "v:hex2rgba_f",
        |env: &mut Env, _argc: usize| {
            let (r, g, b, a) =
                env.arg(0).with_s_ref(|s| util::hex2rgbaf(s));
            Ok(VVal::FVec(Box::new(NVec::from_tpl((r, g, Some(b), Some(a))).unwrap())))
        }, Some(1), Some(1), false);

//    func!(st, "v:rgba2hex",
//        |env: &mut Env, _argc: usize| {
//        }, Some(1), Some(1), false);
//
//    func!(st, "v:hsv2rgb",
//        |env: &mut Env, _argc: usize| {
//        }, Some(1), Some(1), false);

//    func!(st, "v:rgb2hsv",
//        |env: &mut Env, _argc: usize| {
//        }, Some(1), Some(1), false);

    func!(st, "sort",
        |env: &mut Env, argc: usize| {
            if argc == 1 {
                let list = env.arg(0);
                list.sort(|a: &VVal, b: &VVal| {
                    if a.is_int() || a.is_float() {
                        a.compare_num(b)
                    } else {
                        a.compare_str(b)
                    }
                });
                Ok(list)
            } else {
                let fun = env.arg(0);
                let list = env.arg(1);
                let mut ret = Ok(VVal::None);
                list.sort(|a: &VVal, b: &VVal| {
                    env.push(b.clone());
                    env.push(a.clone());
                    let i =
                        match fun.call_internal(env, 2) {
                            Ok(v)  => { v.i() },
                            Err(e) => { ret = Err(e); 1 },
                        };
                    env.popn(2);
                    match i {
                        _ if i == 0 => std::cmp::Ordering::Equal,
                        _ if i >  0 => std::cmp::Ordering::Greater,
                        _           => std::cmp::Ordering::Less,
                    }
                });
                if ret.is_ok() { ret = Ok(list); }
                ret
            }
        }, Some(1), Some(2), false);

    func!(st, "shuffle",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(0);
            let mut list = env.arg(1);
            list.fisher_yates_shuffle(|| {
                fun.call_no_args(env).unwrap_or(VVal::None).i()
            });
            Ok(list)
        }, Some(2), Some(2), false);

    func!(st, "hash:fnv1a",
        |env: &mut Env, argc: usize| {
            let mut hash = util::FnvHasher::default();
            for i in 0..argc {
                match env.arg(i) {
                    VVal::Int(i) => hash.write_i64(i),
                    VVal::Flt(f) => hash.write_f64(f),
                    _ => {
                        env.arg(i).with_s_ref(|s: &str|
                            hash.write(&s.as_bytes()[..]));
                    }
                }
            }
            Ok(VVal::Int(hash.finish_i64()))
        }, Some(1), None, false);

    func!(st, "rand:split_mix64_new",
        |_env: &mut Env, _argc: usize| {
            let v = VVal::vec();
            v.push(VVal::Int(util::now_timestamp() as i64));
            Ok(v)
        }, Some(0), Some(0), false);

    func!(st, "rand:split_mix64_new_from",
        |env: &mut Env, _argc: usize| {
            let v = VVal::vec();
            v.push(VVal::Int(env.arg(0).i()));
            Ok(v)
        }, Some(1), Some(1), false);

    func!(st, "rand:split_mix64_next",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());

            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Int(sm.next_i64()));
                    }
                    v
                } else {
                    VVal::Int(sm.next_i64())
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "num:int_to_open01", |env: &mut Env, _argc: usize| {
        Ok(VVal::Flt(util::u64_to_open01(env.arg(0).i() as u64)))
    }, Some(1), Some(1), false);

    func!(st, "num:int_to_open_closed01", |env: &mut Env, _argc: usize| {
        Ok(VVal::Flt(util::u64_to_open_closed01(env.arg(0).i() as u64)))
    }, Some(1), Some(1), false);

    func!(st, "num:int_to_closed_open01", |env: &mut Env, _argc: usize| {
        Ok(VVal::Flt(util::u64_to_closed_open01(env.arg(0).i() as u64)))
    }, Some(1), Some(1), false);

    func!(st, "symbols:collect", |_env: &mut Env, _argc: usize| {
        Ok(VVal::Int(crate::str_int::string_interner_collect()))
    }, Some(0), Some(0), false);

    func!(st, "rand:split_mix64_next_closed_open01",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());
            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Flt(util::u64_to_closed_open01(sm.next_u64())));
                    }
                    v
                } else {
                    VVal::Flt(util::u64_to_closed_open01(sm.next_u64()))
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "rand:split_mix64_next_open_closed01",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());
            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Flt(util::u64_to_open_closed01(sm.next_u64())));
                    }
                    v
                } else {
                    VVal::Flt(util::u64_to_open_closed01(sm.next_u64()))
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "rand:split_mix64_next_open01",
        |env: &mut Env, argc: usize| {
            let mut sm =
                util::SplitMix64::new_from_i64(
                    env.arg(0).at(0).unwrap_or(VVal::Int(0)).i());
            let ret =
                if argc == 2 {
                    let v = VVal::vec();
                    for _i in 0..env.arg(1).i() {
                        v.push(VVal::Flt(util::u64_to_open01(sm.next_u64())));
                    }
                    v
                } else {
                    VVal::Flt(util::u64_to_open01(sm.next_u64()))
                };
            env.arg(0).set_at(0,
                VVal::Int(i64::from_be_bytes(sm.0.to_be_bytes())));
            Ok(ret)
        }, Some(1), Some(2), false);

    func!(st, "sync:atom:new",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            let av = AtomicAVal::new();
            av.write(&v);
            Ok(VVal::Usr(Box::new(av)))
        }, Some(1), Some(1), false);

    func!(st, "sync:mpsc:new",
        |_env: &mut Env, _argc: usize| {
            Ok(AValChannel::new_vval())
        }, Some(0), Some(0), false);

    func!(st, "sync:slot:new",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::Usr(Box::new(AtomicAValSlot::new())))
        }, Some(0), Some(0), false);

    func!(st, "thread:sleep",
        |env: &mut Env, _argc: usize| {
            match env.arg(0).to_duration() {
                Ok(dur) => std::thread::sleep(dur),
                Err(v)  => { return Ok(v); },
            }
            Ok(VVal::Bol(true))
        }, Some(1), Some(1), false);

    func!(st, "thread:spawn",
        move |env: &mut Env, argc: usize| {
            let avs =
                if argc > 1 {
                    let mut avs = vec![];
                    for (i, (v, k)) in env.arg(1).iter().enumerate() {
                        let av = AtomicAVal::new();
                        av.write(&v);

                        if let Some(k) = k {
                            avs.push((k.s_raw(), av));
                        } else {
                            avs.push((format!("THREAD_ARG{}", i), av));
                        }
                    }
                    Some(avs)
                } else {
                    None
                };

            let tc = env.global.borrow().get_thread_creator();
            if let Some(tc) = &tc {
                let ntc = tc.clone();
                match tc.lock() {
                    Ok(mut tcg) => {
                        env.arg(0).with_s_ref(|code: &str|
                            Ok(tcg.spawn(ntc, code.to_string(), avs)))
                    },
                    Err(e) => {
                        Err(StackAction::panic_str(
                            format!("Couldn't create thread: {}", e),
                            None))
                    },
                }

            } else {
                Err(StackAction::panic_str(
                    "This global environment does not provide threads.".to_string(),
                    None))
            }
        }, Some(1), Some(2), false);

    st
}
