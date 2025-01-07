// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use wlambda::*;

pub fn v(s: &str) -> String {
    let mut ctx = EvalContext::new_default();
    ctx.types_enabled = true;
    match ctx.eval(s) {
        Ok(v) => v.s(),
        Err(e) => {
            eprintln!("ERROR: {}", e);
            format!("{}", e)
        }
    }
}

pub fn v2s(s: &str) -> String {
    let mut ctx = EvalContext::new_default();
    ctx.types_enabled = true;
    match ctx.eval(s) {
        Ok(v) => v.with_s_ref(|s| String::from(s)),
        Err(e) => format!("{}", e),
    }
}

#[test]
fn chk_types_parse_type_values() {
    assert_eq!(v("$type any"), "$type(any)");
    assert_eq!(v("$type bool"), "$type(bool)");
    assert_eq!(v("$type none"), "$type(none)");
    assert_eq!(v("$type str"), "$type(str)");
    assert_eq!(v("$type int"), "$type(int)");
    assert_eq!(v("$type float"), "$type(float)");
    assert_eq!(v("$type bytes"), "$type(bytes)");
    assert_eq!(v("$type sym"), "$type(sym)");
    assert_eq!(v("$type char"), "$type(char)");
    assert_eq!(v("$type byte"), "$type(byte)");
    assert_eq!(v("$type syntax"), "$type(syntax)");
    assert_eq!(v("$type type"), "$type(type)");

    assert_eq!(v("$type ref int"), "$type(ref int)");
    assert_eq!(v("$type ref ref int"), "$type(ref ref int)");
    assert_eq!(v("$type optional int"), "$type(optional int)");
    assert_eq!(v("$type pair(int, int)"), "$type(pair(int, int))");
    assert_eq!(
        v("$type pair(ref pair(int, str), float)"),
        "$type(pair(ref pair(int, str), float))"
    );
    assert_eq!(
        v("$type (pair(ref pair(int, (pair(str, float))), ivec2))"),
        "$type(pair(ref pair(int, pair(str, float)), ivec2))"
    );
    assert_eq!(v("$type Animals.Mammal"), "$type(Animals.Mammal)");
    assert_eq!(v("$type Main . Animal<Mammal,   XXX>"), "$type(Main.Animal<Mammal, XXX>)");
}

#[test]
fn chk_types_infer_var_type() {
    assert_eq!(v("!x = 5 + 2; .x = 10.3; x"), "Err"); // Expecting error on assigning 10.3!
}

#[test]
fn chk_types_add() {
    assert_eq!(v("!x: int = 5 + 2; x"), "7"); // Expecting all ok
    assert!(v("!x: float = 5 + 3; x").find("int to variable x of type float").is_some());
    assert_eq!(v("!x: float = 1 + \"2\"; x"), "10"); // Expecting a type error!
}

#[test]
fn chk_types_function() {
    assert_eq!(v("!x: fn <O is (Num),X>(a: O, b: float) -> X = $n"), "");
}

#[test]
fn chk_types_named() {
    assert_eq!(v("!o: Num = 10; o"), "");
    assert_eq!(v("!o: Num = 10.12; o"), "");
}

#[test]
fn chk_types_def() {
    assert_eq!(
        v("!:type OneDimPoint int; !x: OneDimPoint = 120; ($COMPILE_TYPE x) => OneDimPoint"),
        "$p($p(120,$type(OneDimPoint)),$type(int))"
    );
}
