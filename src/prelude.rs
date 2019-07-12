// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This module defines some default functions and operations
available in the WLambda language.

For an example, refer to [create_wlamba_prelude](fn.create_wlamba_prelude.html).

# WLambda Reference

WLambda is a functional programming language. The syntax gravitates around the
concept that everything is callable like a function. There is special syntax
for composing arguments of functions, to give the programmer the ability to
express his thoughts as they see fit.

## Syntax

A more formal introduction to the syntax can be found in the [parser API documentation](../parser/index.html).

### Data Types

- nul
- bool
- int
- floats
- string
- bytes
- symbols
- lists/vectors
- maps

### Functions

- {... } syntax and statements
- \ ... syntax
- Implicit @ and _, _1, _2 argument variables

### Function call composition

- chaining
- traditional () call syntax
- ~ syntax
- | syntax
- || syntax
- [...] syntax

#### Control Flow - Returning

- \:lbl { ... } syntax and returning

WLambda uses labelled blocks for control flow, as returning from the current function would not be
very helpful for the control flow in wlambda in case of conditional execution.

```wlambda
!some_func = \:outer {
    # does stuff

    [x == 10] {
        return :outer 20
    }

    # more stuff that is not executed if x == 10.
}
```

### Conditional Execution - if / then / else

WLambda has no `if`. Conditional execution is provided by the bool
data type. As in WLambda everything can be called like a function, you
can just pass other functions as arguments to `$true` and `$false`.
If you pass a function as first argument to `$true`, it will
be executed. If you pass a function as second argument to `$false` then that
will be executed.

```wlambda
[10 == 10] { displayln "10 is 10" }         #=> prints "10 is 10"
[10 != 10] { displayln "10 is not 10" }     #=> doesn't print anything

!x = 20;

[x == 20] {
    displayln "x is 20";
} {
    displayln "x is 20";
}; // Do not forget the ";"!
```

Actually, as the values `$true` and `$false` can be called like any other
function you may write it also like this, which is not the recommended
syntax, but still works:

```wlambda
[10 == 10]({ display "10 is 10" })

[x == 20]({ displayln "x is 20" }, { displayln "x is 20" })
```

### Lexical Scope and Variable assignment

- !x = y        variable definition
- .x = y        assignments
- !:ref x = y   upvalue references
- !:wref x = y  weak upvalue references
- !(x, y) = list / map    destructuring assignments
- (x, y) = list / map    destructuring assignments

### Arithmetics

- operator precedence syntax
- prefix operator syntax

## Prelude


### wl:assert _bool_ \[_message_]

Just a simple assertion function that panics if the first argument is not true.
Returns the passed value if it is a true value.
You can pass an optional message as second parameter.

```wlambda
wl:assert $false #=> Panic
wl:assert 120    #=> 120
```

## Optional Prelude

### regex


### chrono

#### chrono:timestamp \[_format_]

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = chrono:timestamp "%Y";
wl:assert [year_str | int] == 2019

!now_str = chrono:timestamp();
```

*/

use crate::compiler::*;
use crate::vval::*;
use std::rc::Rc;
use regex::Regex;
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

fn match_next(env: &mut Env, val: &VVal, mut arg_idx: usize, argc: usize) -> Result<VVal, StackAction> {
    while arg_idx < argc {
        if env.arg(arg_idx).is_fun() {
            return env.with_restore_sp(|e: &mut Env| {
                e.push(val.clone());
                e.arg(arg_idx).call_internal(e, 1)
            });
        }

        let mut match_vals = vec![arg_idx];
        arg_idx += 1;
        while arg_idx < argc && !env.arg(arg_idx).is_fun() {
            match_vals.push(arg_idx);
            arg_idx += 1;
        }

        if arg_idx >= argc { return Ok(val.clone()); }

        let fun_idx = arg_idx;

        if    env.arg(match_vals[0]).is_sym()
           && env.arg(match_vals[0]).s_raw().chars().nth(0).unwrap_or('_') == '?' {

            match &env.arg(match_vals[0]).s_raw()[..] {
                "?t" => {
                    let val_type_name = val.type_name();
                    for i in match_vals.iter().skip(1) {
                        if env.arg(*i).s_raw() == val_type_name {
                            return env.arg(fun_idx).call(env, &vec![val.clone()]);
                        }
                    }
                },
                "?s" => {
                    let val_s = val.s_raw();
                    for i in match_vals.iter().skip(1) {
                        if env.arg(*i).s_raw() == val_s {
                            return env.arg(fun_idx).call(env, &vec![val.clone()]);
                        }
                    }
                },
                "?e" => {
                    if let VVal::Err(e) = val {
                        let err_val = e.borrow().0.at(0).unwrap_or(e.borrow().0.clone());

                        for i in match_vals.iter().skip(1) {
                            if env.arg(*i).eqv(&err_val) {
                                let args = vec![
                                    e.borrow().0.clone(),
                                    VVal::Int(e.borrow().1.line as i64),
                                    VVal::Int(e.borrow().1.col as i64),
                                    VVal::Int(e.borrow().1.file as i64),
                                ];
                                return env.arg(fun_idx).call(env, &args);
                            }
                        }
                    }
                },
                "?p" => {
                    if fun_idx + 1 >= argc { return Ok(VVal::Nul); }
                    let fun_idx = fun_idx + 1;

                    let pred_res = env.arg(arg_idx).call(env, &vec![val.clone()]);
                    match pred_res {
                        Ok(v) => {
                            arg_idx += 1;
                            if v.b() {
                                return env.arg(fun_idx).call(env, &vec![val.clone()]);
                            }
                        },
                        Err(sa) => { return Err(sa); }
                    }
                },
                _ => {
                    // TODO: Usually we should bail out with an error here.
                }
            }
        } else {
            for i in match_vals.iter() {
                if env.arg(*i).eqv(val) {
                    return env.arg(fun_idx).call(env, &vec![val.clone()]);
                }
            }
        }

        arg_idx += 1;
    }

    Ok(VVal::Nul)
}

/// Defines a new global Environment for running the `compiler`.
///
/// The global Environment `GlobalEnvRef` can be reused in different
/// compilations. Keep in mind, that the compiler might add/remove/change
/// global definitions.
///
/// For an example see also [compiler::eval](../compiler/fn.eval.html)
#[allow(clippy::cast_lossless,clippy::assign_op_pattern)]
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

    g.borrow_mut().add_func("not",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            Ok(VVal::Bol(!env.arg(0).b()))
        });

    g.borrow_mut().add_func("neg",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            Ok(VVal::Int(!env.arg(0).i()))
        });

    g.borrow_mut().add_func("uneg",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            Ok(VVal::Int((!(env.arg(0).i() as u32)) as i64))
        });

    g.borrow_mut().add_func("panic",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Panic(VVal::Nul, None)); }
            Err(StackAction::Panic(env.arg(0).clone(), None))
        });

    g.borrow_mut().add_func("block",
        |env: &mut Env, argc: usize| {
            let mut label = VVal::Nul;
            let fn_arg_idx = if argc <= 1 { 0 } else { label = env.arg(0); 1 };
            match env.arg(fn_arg_idx).call_no_args(env) {
                Ok(v)   => Ok(v),
                Err(StackAction::Return((v_lbl, v))) => {
                    if v_lbl.eqv(&label) { Ok(v) }
                    else { Err(StackAction::Return((v_lbl, v))) }
                },
                Err(e)  => Err(e),
            }
        });

    g.borrow_mut().add_func("_?",
        |env: &mut Env, argc: usize| {
            let mut lbl = VVal::Nul;
            let err_val = if argc > 1 {
                lbl = env.arg(0);
                env.arg(1)
            } else { env.arg(0) };

            match err_val {
                VVal::Err(e) => Err(StackAction::Return((lbl, VVal::Err(e)))),
                v            => Ok(v),
            }
        });

    g.borrow_mut().add_func("unwrap",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(err_v) => {
                    return Err(StackAction::Panic(
                        VVal::new_str_mv(format!(
                            "unwrap error: {}@({},{}:{})",
                            err_v.borrow().0.s(),
                            err_v.borrow().1.line,
                            err_v.borrow().1.col,
                            err_v.borrow().1.file)), None));
                },
                v => Ok(v)
            }
        });

    g.borrow_mut().add_func("on_error",
        |env: &mut Env, _argc: usize| {
            let err_fn = env.arg(0).clone();
            match env.arg(1) {
                VVal::Err(err_v) => {
                    return env.with_restore_sp(|e: &mut Env| {
                        e.push(VVal::Int(err_v.borrow().1.file as i64));
                        e.push(VVal::Int(err_v.borrow().1.col as i64));
                        e.push(VVal::Int(err_v.borrow().1.line as i64));
                        e.push(err_v.borrow().0.clone());
                        err_fn.call_internal(e, 4)
                    });
                },
                e => Ok(e)
            }
        });

    g.borrow_mut().add_func("return",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Return((VVal::Nul, VVal::Nul))); }
            if argc < 2 { return Err(StackAction::Return((VVal::Nul, env.arg(0).clone()))); }
            Err(StackAction::Return((env.arg(0).clone(), env.arg(1).clone())))
        });

    g.borrow_mut().add_func("break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(VVal::Nul)); }
            Err(StackAction::Break(env.arg(0).clone()))
        });

    g.borrow_mut().add_func("next",
        |_env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Next); }
            Err(StackAction::Next)
        });

    g.borrow_mut().add_func("push",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let v = env.arg(0);
            v.push(env.arg(1).clone());
            Ok(v.clone())
        });

    g.borrow_mut().add_func("bool",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).b())) });
    g.borrow_mut().add_func("float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Flt(env.arg(0).f())) });
    g.borrow_mut().add_func("int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).i())) });
    g.borrow_mut().add_func("str",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw())) });
    g.borrow_mut().add_func("sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_sym(&env.arg(0).s_raw())) });
    g.borrow_mut().add_func("is_nul",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_nul())) });
    g.borrow_mut().add_func("is_err",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_err())) });
    g.borrow_mut().add_func("is_map",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_map())) });
    g.borrow_mut().add_func("is_vec",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_vec())) });
    g.borrow_mut().add_func("is_fun",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_fun())) });
    g.borrow_mut().add_func("is_str",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_str())) });
    g.borrow_mut().add_func("is_sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_sym())) });
    g.borrow_mut().add_func("is_float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_float())) });
    g.borrow_mut().add_func("is_int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_int())) });
    g.borrow_mut().add_func("str:len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).s_raw().len() as i64)) });
    g.borrow_mut().add_func("str:join",
        |env: &mut Env, _argc: usize| {
            let sep = env.arg(0).s_raw();
            let lst = env.arg(1);
            if let VVal::Lst(l) = lst {
                let svec : Vec<String> = l.borrow_mut().iter().map(|v| v.s_raw()).collect();
                Ok(VVal::new_str_mv((&svec).join(&sep)))

            } else {
                Ok(VVal::err_msg(
                    &format!(
                        "str:join only works with lists as second argument, got '{}'",
                        lst.s())))
            }
        });

    g.borrow_mut().add_func("type",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            if argc > 1 {
                let vec = VVal::vec();
                for i in 0..argc {
                    vec.push(VVal::new_str_mv(env.arg(i).type_name()));
                }
                Ok(vec)
            } else {
                Ok(VVal::new_str_mv(env.arg(0).type_name()))
            }
        });

    g.borrow_mut().add_func("yay",
        |env: &mut Env, argc: usize| {
            if argc < 1 { println!("YOOOY"); return Ok(VVal::Nul); }
            println!("YAAAAY {}", env.arg(0).s());
            env.dump_stack();
            Ok(VVal::Nul)
        });

    g.borrow_mut().add_func("to_drop",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let fun = env.arg(1);
            let v   = env.arg(0);

            Ok(VVal::DropFun(Rc::new(DropVVal { v, fun })))
        });


    g.borrow_mut().add_func("to_drop",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let fun = env.arg(1);
            let v   = env.arg(0);

            Ok(VVal::DropFun(Rc::new(DropVVal { v, fun })))
        });

    g.borrow_mut().add_func("match",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            if argc == 1 { return Ok(VVal::Nul) }
            return match_next(env, &env.arg(0), 1, argc);
        });

    g.borrow_mut().add_func("while",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let test = env.arg(0);
            let f    = env.arg(1);

            let mut ret = VVal::Nul;
            loop {
                match test.call_no_args(env) {
                    Ok(v)                      => { if !v.b() { return Ok(ret); } },
                    Err(StackAction::Break(v)) => { return Ok(v); },
                    Err(StackAction::Next)     => { continue; },
                    Err(e)                     => { return Err(e); }
                }

                match f.call_no_args(env) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { return Ok(v); },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { return Err(e); }
                }
            }
        });

    g.borrow_mut().add_func("fold",
        |env: &mut Env, _argc: usize| {
            let mut acc = env.arg(0);
            let f       = env.arg(1);
            let lst     = env.arg(2);

            if let VVal::Lst(l) = lst {
                for i in l.borrow_mut().iter() {
                    env.push(acc.clone());
                    env.push(i.clone());
                    let rv = f.call_internal(env, 2);
                    env.popn(2);

                    match rv {
                        Ok(v)                      => { acc = v;  },
                        Err(StackAction::Break(v)) => { acc = v; break; },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); },
                    }
                }
            } else {
                return Ok(VVal::err_msg(
                    &format!(
                        "fold only works with lists as argument, got '{}'",
                        lst.s())));
            }

            Ok(acc)
        });

    g.borrow_mut().add_func("range",
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
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); }
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
                    match f.call_internal(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); println!("BREAK {}", v.s()); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); }
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            }
        });

    g.borrow_mut().add_func("displayln",
        |env: &mut Env, argc: usize| {
            for i in 0..argc {
                if i == (argc - 1) {
                    println!("{}", env.arg(i).s_raw());
                } else if i > 0 {
                    print!(" {}", env.arg(i).s_raw());
                } else {
                    print!("{}", env.arg(i).s_raw());
                }
            }
            if argc > 0 {
                Ok(env.arg(argc - 1).clone())
            } else {
                Ok(VVal::Nul)
            }
        });

    g.borrow_mut().add_func("wl:assert",
        |env: &mut Env, _argc: usize| {
            if !env.arg(0).b() {
                if env.arg(1).is_nul() {
                    Err(StackAction::Panic(VVal::new_str("assertion failed"), None))
                } else {
                    Err(StackAction::Panic(VVal::new_str_mv(format!("assertion failed '{}'", env.arg(1).s_raw())), None))
                }
            } else {
                Ok(env.arg(0).clone())
            }
        });

    if cfg!(feature="regex") {
        g.borrow_mut().add_func("re:map",
            |env: &mut Env, _argc: usize| {
                let re   = env.arg(0).s_raw();
                let f    = env.arg(1);
                let text = env.arg(2).s_raw();

                let rx = Regex::new(&re);
                if let Err(e) = rx {
                    return Ok(VVal::err_msg(
                        &format!("Regex '{}' did not compile: {}", re, e)));
                }
                let rx = rx.unwrap();

                let ret = VVal::vec();
                for capts in rx.captures_iter(&text) {
                    let captures = VVal::vec();
                    for cap in capts.iter() {
                        match cap {
                            None    => { captures.push(VVal::Nul); },
                            Some(c) => {
                                captures.push(VVal::new_str(c.as_str()));
                            }
                        }
                    }

                    env.push(captures);
                    let rv = f.call_internal(env, 1);
                    env.popn(1);

                    match rv {
                        Ok(v)                      => { ret.push(v); },
                        Err(StackAction::Break(v)) => { ret.push(v); break; },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); },
                    }
                }
                Ok(ret)
            });
    }

    if cfg!(feature="chrono") {
        g.borrow_mut().add_func("chrono:timestamp",
            |env: &mut Env, _argc: usize| {
                use chrono::prelude::*;
                let dt = Local::now();

                let fmt = env.arg(0);
                let fmt = if fmt.is_str() {
                    fmt.s_raw()
                } else {
                    String::from("%Y-%m-%d %H:%M:%S.%f")

                };

                Ok(VVal::new_str_mv(dt.format(&fmt).to_string()))
            });
    }

    g
}
