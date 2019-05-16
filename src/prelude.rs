// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This module defines some default functions and operations
available in the WLambda language.

For an example, refer to [create_wlamba_prelude](fn.create_wlamba_prelude.html).
*/

use crate::compiler::*;
use crate::vval::*;
use std::rc::Rc;
//use std::cell::RefCell;

macro_rules! add_func {
    ($g: ident, $op: tt, $env: ident, $argc: ident, $b: block) => {
        $g.borrow_mut().add_func(
            stringify!($op), |$env: &mut Env, $argc: usize| $b);
    }
}

macro_rules! add_multi_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(f) = env.arg(0) {
                let mut accum = f;
                for i in 1..argc { accum = accum $op env.arg(i).f() }
                Ok(VVal::Flt(accum))
            } else {
                let mut accum = env.arg(0).i();
                for i in 1..argc { accum = accum $op env.arg(i).i() }
                Ok(VVal::Int(accum))
            }
        })
    }
}

macro_rules! add_bool_bin_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let a = env.arg(0);
            if let VVal::Flt(af) = a { Ok(VVal::Bol(af $op env.arg(1).f())) }
            else { Ok(VVal::Bol(a.i() $op env.arg(1).i())) }
        })
    }
}

macro_rules! add_fi_bin_op {
    ($g: ident, $op: tt, $a: ident, $b: ident, $ef: expr, $ei: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            if let VVal::Flt(_) = $a { $ef }
            else { $ei }
        })
    }
}

macro_rules! add_bin_op {
    ($g: ident, $op: tt, $a: ident, $b: ident, $e: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            $e
        })
    }
}

macro_rules! add_sbin_op {
    ($g: ident, $op: literal, $a: ident, $b: ident, $e: expr) => {
        $g.borrow_mut().add_func(
            $op, |env: &mut Env, argc: usize| {
                if argc < 2 { return Ok(VVal::Nul); }
                let $a = env.arg(0);
                let $b = env.arg(1);
                $e
            });
    }
}

/// Defines a new global Environment for running the `compiler`.
///
/// The global Environment `GlobalEnvRef` can be reused in different
/// compilations. Keep in mind, that the compiler might add/remove/change
/// global definitions.
///
/// For an example see also [compiler::eval](../compiler/fn.eval.html)
pub fn create_wlamba_prelude() -> GlobalEnvRef {
    let g = GlobalEnv::new();

    add_multi_op!(g, +);
    add_multi_op!(g, -);
    add_multi_op!(g, *);
    add_multi_op!(g, /);
    add_multi_op!(g, %);

    add_bool_bin_op!(g, <);
    add_bool_bin_op!(g, >);
    add_bool_bin_op!(g, <=);
    add_bool_bin_op!(g, >=);

    add_bin_op!(g, ==, a, b, Ok(VVal::Bol(a.eqv(&b))));
    add_bin_op!(g, !=, a, b, Ok(VVal::Bol(!a.eqv(&b))));

    add_sbin_op!(g, "&|", a, b,
        Ok(VVal::Int(((a.i() as u32) | (b.i() as u32)) as i64)));
    add_sbin_op!(g, "&", a, b,
        Ok(VVal::Int(((a.i() as u32) & (b.i() as u32)) as i64)));
    add_sbin_op!(g, "&^", a, b,
        Ok(VVal::Int(((a.i() as u32) ^ (b.i() as u32)) as i64)));
    add_sbin_op!(g, "<<", a, b,
        Ok(VVal::Int(((a.i() as u32) << (b.i() as u32)) as i64)));
    add_sbin_op!(g, ">>", a, b,
        Ok(VVal::Int(((a.i() as u32) >> (b.i() as u32)) as i64)));

    add_fi_bin_op!(g, ^, a, b,
        Ok(VVal::Flt(a.f().powf(b.f()))),
        Ok(VVal::Int(a.i().pow(b.i() as u32))));

    g.borrow_mut().add_func(
        "neg",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            Ok(VVal::Int(!env.arg(0).i()))
        });

    g.borrow_mut().add_func(
        "uneg",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            Ok(VVal::Int((!(env.arg(0).i() as u32)) as i64))
        });

    g.borrow_mut().add_func(
        "break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(VVal::Nul)); }
            Err(StackAction::Break(env.arg(0).clone()))
        });

    g.borrow_mut().add_func(
        "next",
        |_env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Next); }
            Err(StackAction::Next)
        });

    g.borrow_mut().add_func(
        "push",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let v = env.arg(0);
            v.push(env.arg(1).clone());
            Ok(v.clone())
        });

    g.borrow_mut().add_func(
        "type",
        |env: &mut Env, argc: usize| {
            if argc < 1 { println!("YOOOY"); return Ok(VVal::Nul); }
            if argc > 1 {
                let vec = VVal::vec();
                for i in 0..argc {
                    vec.push(VVal::new_str(&env.arg(i).type_name()));
                }
                Ok(vec)
            } else {
                Ok(VVal::new_str(&env.arg(0).type_name()))
            }
        });

    g.borrow_mut().add_func(
        "yay",
        |env: &mut Env, argc: usize| {
            if argc < 1 { println!("YOOOY"); return Ok(VVal::Nul); }
            println!("YAAAAY {}", env.arg(0).s());
            env.dump_stack();
            Ok(VVal::Nul)
        });

    g.borrow_mut().add_func(
        "to_drop",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let fun = env.arg(1);
            let v   = env.arg(0);

            Ok(VVal::DropFun(Rc::new(DropVVal { v, fun })))
        });

    g.borrow_mut().add_func(
        "while",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let test = env.arg(0);
            let f    = env.arg(1);

            let mut ret = VVal::Nul;
            loop {
                match test.call(env, 0) {
                    Ok(v)                      => { if !v.b() { return Ok(ret); } },
                    Err(StackAction::Break(v)) => { return Ok(v); },
                    Err(StackAction::Next)     => { continue; },
                }

                match f.call(env, 0) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { return Ok(v); },
                    Err(StackAction::Next)     => { },
                }
            }
        });

    g.borrow_mut().add_func(
        "range",
        |env: &mut Env, argc: usize| {
            if argc <= 3 { return Ok(VVal::Nul); }
            let from     = env.arg(0);
            let to       = env.arg(1);
            let step     = env.arg(2);
            let f        = env.arg(3);
            //println!("RAGEN from={} to={} f={}", from.s(), to.s(), f.s());

            if let VVal::Flt(_) = from {
                let mut from = from.f();
                let to       = to.f();
                let step     = step.f();

                let mut ret = VVal::Nul;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::Nul;
                    env.push(VVal::Flt(from));
                    match f.call(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        //e                          => { return e; },
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            } else {
                let mut from = from.i();
                let to       = to.i();
                let step     = step.i();

                let mut ret = VVal::Nul;
                #[allow(unused_must_use)]
                while from <= to {
                    ret = VVal::Nul;
                    env.push(VVal::Int(from));
                    match f.call(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); println!("BREAK {}", v.s()); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        // e                          => { return e; },
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            }
        });

    g
}
