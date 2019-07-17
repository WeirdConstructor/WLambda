// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This module defines some default functions and operations
available in the WLambda language.

For an example, refer to [create_wlamba_prelude](fn.create_wlamba_prelude.html).

# WLambda Reference

WLambda is a functional programming language. The main goal of this
implementation is the extension of Rust applications with dynamic scripting.
The syntax gravitates around the concept that everything is callable like a
function. There is special syntax for composing arguments of functions, to give
the programmer the ability to express his thoughts as they see fit.

You can use this document as reference or as cover to cover lecture. It starts
out with functions and the base data types of WLambda, where I also explain
some semantics of the language.

Please note: I expect you to know how to program and be familiar with at least
one other dynamic language like _JavaScript_, _Perl_ or at least _Python_. The
syntax and semantics of WLambda are different from what you might know. Think
of it more like a LISP without parenthesis. The syntax is loosely inspired from
Smalltalk, LISP and Perl.

## Syntax

A more formal introduction to the syntax can be found in the [parser API documentation](../parser/index.html).

## Functions (part 1/2)

A function can be defined using the `{ ... }` syntax and the `\ _statement_`
syntax: To give functions a name, you need to assign them to a variable with
the `!_name_ = _expr_` syntax.

To call functions, you have at least 2 alternatives. First is the bare
`_expr_ arg1 arg2 arg3 arg4` syntax. And the second is the delimiter
full variant: `_expr_ (arg1, arg2, arg3, ...)`. You can always delimit the first
variant using the `[ ... ]` brackets.

The arguments passed to the function are accessible using the `_`, `_1`, `_2`, ..., `_9`
variables. If you need to access more arguments the `@` variable holds a list of all
arguments.

```wlambda
!twoify = { _ * 2 };

wl:assert_eq twoify(2) 4;

!twoify2 = \_ * 2;

wl:assert_eq twoify2(2) 4;

# You may also call them directly, notice the bracket [ ... ] syntax
# for delimiting the inner function call:
wl:assert_eq [{ _ * 2 } 2] 4;
```

If you want to name arguments, you can use the destructuring assignment
syntax:

```wlamdba
!add = {!(a, b) = @;
    a + b
};

wl:assert_eq add(1, 2) 3;
```

### Function arity checks

Functions check the number of arguments passed to them. The compiler tries to
infer the number of arguments the function requires by looking at the parameter
variables `_` to `_9` and `@`. If the compiler gets it wrong, you can:

* Define minimum and maximum number of arguments with: `{|min < max| ... }`
* Define exact number of arguments with: `{|num_of_args| ... }`
* Accept any number of arguments: `{|| ... }`

For the shortened function syntax there is:

* `\|min < max| ...`
* `\|num_of_args| ...`
* `\|| ...`

Here an example:

```wlambda
!dosomething = {|2 < 4|
    !a = _;
    !b = _1;
    !c = _2;
    !d = _3;

    # Please note: We have to assign _ to _3 here, because
    # the arms of the conditional below have
    # their own set of arguments.

    [is_none c] { a + b } { a * b + c * d }
};

wl:assert_eq dosomething(1, 2)         3;
wl:assert_eq dosomething(2, 2, 3, 4)  16;
```

## Data Types

### None sentinel value: `$n` or `$none`

This is a special sentinel value that is returned by functions and
when a non existing field of a datastructure is accessed. It's semantic
meaning is that there is no value.

Please note for API design: In case of errornous states you should not
return a `$none` but an `$error` value.

```wlambda
wl:assert ~ $n        == $none;
wl:assert ~ int($n)   == 0;
wl:assert ~ float($n) == 0.0;
wl:assert ~ str($n)   == "$n";
wl:assert ~ is_none($n);
```

### Error values: `$e expr` or `$error expr`

There are no exceptions in WLambda, except the panic, that
halts all execution of the currently running WLambda
program. To signal errors, you return an `$error` value.

These error values, if not handled, will cause a panic of your
program. This means, you need to handle returned error values
one way or the other.

The error value wraps any value you pass to the `$error` or `$e`
constructor syntax.

```wlambda
wl:assert ~ is_err ~ $e "something went wrong!"
```

There are more routines except `is_err` to handle an error.
`_?` will return from the currently executed function
up until some given label. `on_error` executes a function
if the second argument was an error value. Otherwise it
just passes through the value. `unwrap` will explicitly cause
an panic if an error value was passed to it. All other values
will be passed through. And `unwrap_err` unwraps an error value, it's
the opposite of `unwrap` because it will cause a panic if you don't pass
an error value.

#### Return on error with `_?`

```wlambda
!func = { $e "this failed!" };

!other = {
    # some code ...

    _? func(); # If you would not catch the error value here,
               # the program would panic, as an error value
               # must not be ignored!

    # other code ...

    panic "this will never be reached!";

    # something here...
};

wl:assert ~ [unwrap_err other()] == "this failed!";
```

`_?` can take up to 2 arguments. If so, the first argument is interpreted
as jump label. That is handy if you want to jump up multiple call frames:

```wlambda
!failing_func = { $e :FAIL };

!func = \:some_unique_label {
    [ _ == 42 ] {
        displayln "We got 42!";

        # The `then` branch we are currently in is a call frame.
        # To jump further up the call stack, we need the label
        # we defined for the function above.
        !val = _? :some_unique_label failing_func();

        displayln "Returned:" val;
    }
};

wl:assert_eq [unwrap_err ~ func 42] :FAIL;
```

#### Handle errors with `on_error`

```wlambda
!func = {
    [_ == 13] {
        $e "this failed!"
    } {
        "all ok!"
    }
};

!:ref x = $n;

# The first function of on_error will be called with the unwrapped
# error if an error occured.
on_error {|4| .x = _; } ~ func 13;
wl:assert_eq x "this failed!";

!ret = on_error {|4| .x = _; } ~ func 1;
wl:assert_eq ret "all ok!";
```

- bool
- int
- floats
- string
- bytes
- symbols
- lists/vectors
- maps

## Operators

### Arithmetics

- \+
- \-
- \*
- /
- %
- ^

### Comparison

- ==
- !=
- \<
- \>
- \<=
- \>=

### Bitwise

- &|
- &
- &^
- \<<
- \>>

## Functions (part 2/2)

### Function call composition

- chaining
- traditional () call syntax
- ~ syntax
- | syntax
- || syntax
- [...] syntax

### Control Flow - Returning

- \:lbl { ... } syntax and returning

WLambda uses labelled blocks for control flow, as returning from the current function would not be
very helpful for the control flow in wlambda in case of conditional execution.

```wlambda
!some_func = \:outer {
    !x = 10;
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
[10 == 10] { displayln "10 is 10" };         #=> prints "10 is 10"
[10 != 10] { displayln "10 is not 10" };     #=> doesn't print anything

!x = 20;

[x == 20] {
    displayln "x is 20";
} {
    displayln "x is 20";
}; # Do not forget the ";"!
```

Actually, as the values `$true` and `$false` can be called like any other
function you may write it also like this, which is not the recommended
syntax, but still works:

```wlambda
[10 == 10]({ displayln "10 is 10" });

!x = 21;
[x == 20]({ displayln "x is 20" }, { displayln "x isn't 20" }); #=> print "x isn't 20"
```

## Lexical Scope and Variable assignment

- !x = y        variable definition
- .x = y        assignments
- !:ref x = y   upvalue references
- !:wref x = y  weak upvalue references
- !(x, y) = list / map    destructuring assignments
- (x, y) = list / map    destructuring assignments

## Arithmetics

- operator precedence syntax
- prefix operator syntax

## Modules

### export

```wlambda

!expr = { _ + 30 };

!@export symbol expr; # exports symbol with value of expr (a function)

```

### import

```wlambda

!@import x tests:test_mod; # prefixes everything from modixes with x:

wl:assert ~ [x:symbol 10] == 40;

```

## Prelude

### wl:assert _bool_ \[_message_]

Just a simple assertion function that panics if the first argument is not true.
Returns the passed value if it is a true value.
You can pass an optional message as second parameter.

```norun_wlambda
wl:assert $false; #=> Panic
wl:assert 120;    #=> 120
```

### wl:assert_eq _actual_ _expected_ \[_message_]

This function check if the _actual_ value is equal to the
_expected_ value and panics if not. The optional _message_ is
passed in the panic for reference.

```wlambda
!x = 30 * 2;
wl:assert_eq x 60 "30 * 2 == 60";
```

## Optional Prelude

### regex


### chrono

#### chrono:timestamp \[_format_]

For the documentation of _format_ please consule the
chrono Rust crate documentation: [chrono crate strftime format](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).

```wlambda
!year_str = chrono:timestamp "%Y";
displayln :XXXX ~ [year_str | int] == 2019;
wl:assert ~ [year_str | int] == 2019;

!now_str = chrono:timestamp();
```

*/

use crate::compiler::*;
use crate::vval::*;
use std::rc::Rc;
//use std::cell::RefCell;

macro_rules! add_func {
    ($g: ident, $op: tt, $env: ident, $argc: ident, $b: block, $min: expr, $max: expr) => {
        $g.borrow_mut().add_func(
            stringify!($op), |$env: &mut Env, $argc: usize| $b, $min, $max);
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
        }, Some(2), None)
    }
}

macro_rules! add_bool_bin_op {
    ($g: ident, $op: tt) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let a = env.arg(0);
            if let VVal::Flt(af) = a { Ok(VVal::Bol(af $op env.arg(1).f())) }
            else { Ok(VVal::Bol(a.i() $op env.arg(1).i())) }
        }, Some(2), Some(2))
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
        }, Some(2), Some(2))
    }
}

macro_rules! add_bin_op {
    ($g: ident, $op: tt, $a: ident, $b: ident, $e: expr) => {
        add_func!($g, $op, env, argc, {
            if argc < 2 { return Ok(VVal::Nul); }
            let $a = env.arg(0);
            let $b = env.arg(1);
            $e
        }, Some(2), Some(2))
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
            }, Some(2), Some(2));
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
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Bol(!env.arg(0).b()))
        }, Some(1), Some(1));

    g.borrow_mut().add_func("neg",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int(!env.arg(0).i()))
        }, Some(1), Some(1));

    g.borrow_mut().add_func("uneg",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::Int((!(env.arg(0).i() as u32)) as i64))
        }, Some(1), Some(1));

    g.borrow_mut().add_func("panic",
        |env: &mut Env, _argc: usize| {
            Err(StackAction::Panic(env.arg(0).clone(), None))
        }, Some(1), Some(1));

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
        }, Some(1), Some(2));

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
        }, Some(1), Some(2));

    g.borrow_mut().add_func("unwrap_err",
        |env: &mut Env, _argc: usize| {
            match env.arg(0) {
                VVal::Err(err_v) => {
                    Ok(err_v.borrow().0.clone())
                },
                v => {
                    return Err(StackAction::Panic(
                        VVal::new_str_mv(format!(
                            "unwrap_err on non error value: {}", v.s())), None));
                },
            }
        }, Some(1), Some(1));

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
        }, Some(1), Some(1));

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
        }, Some(2), Some(2));

    g.borrow_mut().add_func("return",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Return((VVal::Nul, VVal::Nul))); }
            if argc < 2 { return Err(StackAction::Return((VVal::Nul, env.arg(0).clone()))); }
            Err(StackAction::Return((env.arg(0).clone(), env.arg(1).clone())))
        }, Some(1), Some(2));

    g.borrow_mut().add_func("break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(VVal::Nul)); }
            Err(StackAction::Break(env.arg(0).clone()))
        }, Some(0), Some(1));

    g.borrow_mut().add_func("next",
        |_env: &mut Env, _argc: usize| {
            Err(StackAction::Next)
        }, Some(0), Some(0));

    g.borrow_mut().add_func("push",
        |env: &mut Env, _argc: usize| {
            let v = env.arg(0);
            v.push(env.arg(1).clone());
            Ok(v.clone())
        }, Some(2), Some(2));

    g.borrow_mut().add_func("take",
        |env: &mut Env, _argc: usize| {
            let cnt = env.arg(0).i() as usize;
            let lst = env.arg(1);
            if let VVal::Lst(l) = lst {
                let svec : Vec<VVal> =
                    l.borrow_mut().iter().take(cnt).map(|v| v.clone()).collect();
                Ok(VVal::vec_mv(svec))
            } else {
                Ok(VVal::err_msg(
                    &format!(
                        "drop only works with a list as second argument, got '{}'",
                        lst.s())))
            }
        }, Some(2), Some(2));

    g.borrow_mut().add_func("drop",
        |env: &mut Env, _argc: usize| {
            let cnt = env.arg(0).i() as usize;
            let lst = env.arg(1);
            if let VVal::Lst(l) = lst {
                let svec : Vec<VVal> =
                    l.borrow_mut().iter().skip(cnt).map(|v| v.clone()).collect();
                Ok(VVal::vec_mv(svec))
            } else {
                Ok(VVal::err_msg(
                    &format!(
                        "drop only works with a list as second argument, got '{}'",
                        lst.s())))
            }
        }, Some(2), Some(2));

    g.borrow_mut().add_func("bool",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).b())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Flt(env.arg(0).f())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).i())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("str",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_sym(&env.arg(0).s_raw())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_none",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_none())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_err",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_err())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_map",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_map())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_vec",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_vec())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_fun",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_fun())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_str",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_str())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_sym",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_sym())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_float",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_float())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("is_int",
        |env: &mut Env, _argc: usize| { Ok(VVal::Bol(env.arg(0).is_int())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("str:len",
        |env: &mut Env, _argc: usize| { Ok(VVal::Int(env.arg(0).s_raw().len() as i64)) },
        Some(1), Some(1));
    g.borrow_mut().add_func("str:to_lowercase",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().to_lowercase())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("str:to_uppercase",
        |env: &mut Env, _argc: usize| { Ok(VVal::new_str_mv(env.arg(0).s_raw().to_uppercase())) },
        Some(1), Some(1));
    g.borrow_mut().add_func("str:padl",
        |env: &mut Env, _argc: usize| {
            let len = env.arg(0).i() as usize;
            let pads = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            while s.len() < len {
                s = pads.to_string() + &s;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3));

    g.borrow_mut().add_func("str:padr",
        |env: &mut Env, _argc: usize| {
            let len = env.arg(0).i() as usize;
            let pads = env.arg(1).s_raw();
            let mut s = env.arg(2).s_raw();

            while s.len() < len {
                s += &pads;
            }

            Ok(VVal::new_str_mv(s))
        }, Some(3), Some(3));
    g.borrow_mut().add_func("str:cat",
        |env: &mut Env, argc: usize| {
            let lst = env.arg(0);
            if let VVal::Lst(l) = lst {
                let svec : Vec<String> = l.borrow_mut().iter().map(|v| v.s_raw()).collect();
                Ok(VVal::new_str_mv((&svec).concat()))

            } else {
                let mut s = String::from("");
                for i in 0..argc {
                    s += &env.arg(i).s_raw();
                }
                Ok(VVal::new_str_mv(s))
            }
        }, None, None);
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
        }, Some(2), Some(2));

    g.borrow_mut().add_func("type",
        |env: &mut Env, _argc: usize| {
            Ok(VVal::new_str_mv(env.arg(0).type_name()))
        }, Some(1), Some(1));

    g.borrow_mut().add_func("yay",
        |env: &mut Env, _argc: usize| {
            println!("YAAAAY {}", env.arg(0).s());
            env.dump_stack();
            Ok(VVal::Nul)
        }, None, None);

    g.borrow_mut().add_func("to_drop",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(1);
            let v   = env.arg(0);

            Ok(VVal::DropFun(Rc::new(DropVVal { v, fun })))
        }, Some(2), Some(2));


    g.borrow_mut().add_func("match",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Ok(VVal::Nul); }
            if argc == 1 { return Ok(VVal::Nul) }
            return match_next(env, &env.arg(0), 1, argc);
        }, Some(1), None);

    g.borrow_mut().add_func("while",
        |env: &mut Env, _argc: usize| {
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
        }, Some(2), Some(2));

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
        }, Some(3), Some(3));

    g.borrow_mut().add_func("range",
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
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                        Err(StackAction::Next)     => { },
                        Err(e)                     => { return Err(e); }
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            }
        }, Some(4), Some(4));

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
        }, None, None);

    g.borrow_mut().add_func("wl:dump_func",
        |env: &mut Env, _argc: usize| {
            if let VVal::Fun(f) = env.arg(0) {
                return Ok(f.dump_upvals());
            }
            Ok(VVal::Nul)
        }, Some(1), Some(1));

    g.borrow_mut().add_func("wl:assert_eq",
        |env: &mut Env, _argc: usize| {
            let a = env.arg(0);
            let b = env.arg(1);
            if !a.eqv(&b) {
                if env.arg(2).is_none() {
                    Err(
                        StackAction::Panic(
                            VVal::new_str_mv(
                                format!(
                                    "assertion failed: expected: '{}', got: '{}'",
                                    b.s(), a.s())),
                            None))
                } else {
                    Err(
                        StackAction::Panic(
                            VVal::new_str_mv(
                                format!(
                                    "assertion '{}' failed: expected: '{}', got: '{}'",
                                    env.arg(2).s_raw(), b.s(), a.s())),
                            None))
                }
            } else {
                Ok(VVal::Bol(true))
            }
        }, Some(2), Some(3));

    g.borrow_mut().add_func("wl:assert",
        |env: &mut Env, _argc: usize| {
            if !env.arg(0).b() {
                if env.arg(1).is_none() {
                    Err(StackAction::Panic(VVal::new_str("assertion failed"), None))
                } else {
                    Err(StackAction::Panic(VVal::new_str_mv(format!("assertion failed '{}'", env.arg(1).s_raw())), None))
                }
            } else {
                Ok(env.arg(0).clone())
            }
        }, Some(1), Some(2));

    if cfg!(feature="regex") {
        use regex::Regex;
        g.borrow_mut().add_func("re:replace_all",
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

                let mut finished = false;
                let mut ret = Ok(VVal::Nul);
                let ret_str = VVal::new_str_mv(String::from(rx.replace_all(&text, |capts: &regex::Captures| {
                    let captures = VVal::vec();
                    for cap in capts.iter() {
                        match cap {
                            None    => { captures.push(VVal::Nul); },
                            Some(c) => {
                                captures.push(VVal::new_str(c.as_str()));
                            }
                        }
                    }

                    let repl = captures.at(0).unwrap_or(VVal::Nul).s_raw();
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
                })));
                if ret.is_err() { return ret; }
                Ok(ret_str)
            }, Some(3), Some(3));

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
            }, Some(3), Some(3));
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
            }, Some(0), Some(1));
    }

    g
}
