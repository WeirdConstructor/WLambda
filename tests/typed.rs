// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use wlambda::*;

pub fn v(s: &str) -> String {
    let mut ctx = EvalContext::new_default();
    ctx.types_enabled = true;
    match ctx.eval(s) {
        Ok(v) => v.s(),
        Err(e) => {
            eprintln!("Error: {}", e);
            let err = format!("{}", e);
            if let Some(idx) = err.find("Type error, ") {
                if let Some(spart) = err.get(idx..) {
                    if let Some(idx2) = spart.find("\n") {
                        return err[(idx + 12)..(idx + idx2)].to_string();
                    }
                }
            }
            err
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
fn chk_types_index() {
    assert_eq!(v("!x = \"foo\"; x.1"), "'o'");
    assert_eq!(v("!x = $b\"foo\"; x.1"), "$b'o'");

    assert_eq!(v("std:types:type_at $type(str) 10"), "$type(char?)");
    assert_eq!(v("std:types:type_at $type(bytes) 10"), "$type(byte?)");
    assert_eq!(v("std:types:type_at $type(none) 10"), "$type(none)");
    assert_eq!(v("std:types:type_at $type(bytes?) 10"), "$type(byte?)");
    assert_eq!(v("std:types:type_at $type(str?) 10"), "$type(char?)");
    assert_eq!(v("std:types:type_at $type(optional str) 10"), "$type(char?)");
    assert_eq!(v("std:types:type_at $type(optional pair(int, str)) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(optional pair(int, str)) 0"), "$type(int?)");
    assert_eq!(v("std:types:type_at $type(optional pair(int, str)) 1"), "$type(str?)");
    assert_eq!(v("std:types:type_at $type(pair(int, str)?) 1"), "$type(str?)");
    assert_eq!(v("std:types:type_at $type(pair(int, str)?) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(ivec2) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(ivec2) 0"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec2) 1"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec3) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(ivec3) 0"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec3) 1"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec3) 2"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec4) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(ivec4) 0"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec4) 1"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec4) 2"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(ivec4) 3"), "$type(int)");
    assert_eq!(v("std:types:type_at $type(fvec2) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(fvec2) 0"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec2) 1"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec3) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(fvec3) 0"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec3) 1"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec3) 2"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec4) 10"), "$n");
    assert_eq!(v("std:types:type_at $type(fvec4) 0"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec4) 1"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec4) 2"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(fvec4) 3"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(iter int) 3"), "$type(int?)");
    assert_eq!(v("std:types:type_at $type(ref fvec3) 1"), "$type(float)");
    assert_eq!(v("std:types:type_at $type(ref fvec3) 4"), "$n");
    assert_eq!(v("std:types:type_at $type(ivec2 | ivec3 | ivec4) 3"), "$n");
    assert_eq!(v("std:types:type_at $type(ivec2 | ivec3 | ivec4 | fvec2) 1"), "$type(int | int | int | float)");
    assert_eq!(v("std:types:type_at $type([int]) 1"), "$type(int?)");
    assert_eq!(v("std:types:type_at $type([int, str, int]) 3"), "$n");
    assert_eq!(v("std:types:type_at $type([int, str, int]) 1"), "$type(str)");
    assert_eq!(v("std:types:type_at $type({int}) 1"), "$type(int?)");
    assert_eq!(v("std:types:type_at $type({int} | [int]) 1"), "$type(int? | int?)");
    assert_eq!(v("std:types:type_at $type({int} | [int,str]) 1"), "$type(int? | str)");
    assert_eq!(v("std:types:type_at $type({int} | [int,str]?) 1"), "$type((int? | str)?)");
}

#[test]
fn chk_types_function() {
    assert_eq!(v("!x: fn <O is (Num),X>(a: O, b: float) -> X = $n"), "");
}

#[test]
fn chk_types_aliasing() {
    assert_eq!(v("!:type @Num int | float; !x: @Num = 10; x"), "10");
    assert_eq!(v("!:type @Num int | float; !x: @Num = 10.3; x"), "10.3");
    assert_eq!(
        v("!:type @Num int | float; !x: @Num = \"test\"; $typeof x"),
        "expected (int | float), but got (str); in variable definition 'x'"
    );

    assert_eq!(
        v("!:type @Num int | float; !v: int = 10; !x: @Num = 0; .x = v; $typeof x"),
        "$p($type(int),10)"
    );
    assert_eq!(
        v("!:type @Num int | float; !v: float = 10.4; !x: @Num = 0; .x = v; $typeof x"),
        "expected (int), but got (float); in assignment to 'x'"
    );
    assert_eq!(
        v("!:type @Num int | float; !v: float = 10.4; !x: @Num = 0.0; .x = v; $typeof x"),
        "$p($type(float),10.4)"
    );
    assert_eq!(
        v("!:type @Num int | float; !v: str = \"test\"; !x: @Num = 0; .x = v; $typeof x"),
        "expected (int), but got (str); in assignment to 'x'"
    );
    assert_eq!(
        v(r#"!f = {|@Num, @Num -> int| int[_ + _1] };
             f 10 11.2
        "#),
        "21"
    );
    assert_eq!(
        v(r#"!:type @X (int | float);
            {|@X -> int| _ } 10.1
        "#),
        "expected (int), but got (float); in last statement of function block"
    );
    assert_eq!(
        v(r#"!:type @X (int | float);
            {|@X, @X -> int| int _1 } 10 12.6
        "#),
        "12"
    );
    assert_eq!(
        v(r#"!f1: fn <N is @Num>(N) -> N = { _ };
             !f2: fn <X is int>(X) -> X = { _ };
             .f1 = f2;
            "#),
        "expected (fn (<N is @Num>) -> <N is @Num>), but got (fn (<X is int>) -> <X is int>); in assignment to 'f1'"
    );
    assert_eq!(
        v(r#"std:types:is_typeof
                ($type fn <X is int>(X) -> X)
                ($type fn <N is @Num>(N) -> N);
        "#),
        "$type(fn (int) -> int)"
    );
    assert_eq!(
        v(r#"panic ~ unwrap_err ~ std:types:is_typeof
                ($type fn <N is @Num>(N) -> N)
                ($type fn <X is int>(X) -> X);
        "#),
        "expected (fn <N is (@Num)> (<N is @Num>) -> <N is @Num>), but got (fn <X is (int)> (<X is int>) -> <X is int>); reason: Wrong argument 1, reason: Type variable <N is int> does not accept @Num"
    );
    assert_eq!(
        v(r#"!f1: fn <N is @Num>(N) -> N = { _ };
             !f2: fn <X is int>(X) -> X = { _ };
             .f2 = f1;
             ($typeof f2).0
            "#),
        "$type(fn <X is int>(X) -> X)"
    );
    assert_eq!(
        v(r#"!:type @X (int | float);
            !f: fn <N is @Num> (N) -> N = { _ };
            f 12.6
        "#),
        "22"
    );
    assert_eq!(
        v(r#"!:type @X (int | float);
            {|@X, @X -> int|
                int[_ + (int _1)] }
                10
                12.6
        "#),
        "22"
    );
    assert_eq!(
        v("!:type @Num (int | float); !f = {|@Num, @Num -> str| int[_ + (int _1)] }; f 10 11.2"),
        "errror about int returned, but str expected!"
    );
    assert_eq!(
        v("!:type @Num (int | float); !f = {|@Num, str -> int| int[_ + _1] }; f 10 11.2"),
        "errror"
    );
    assert_eq!(
        v("!:type @Num (int | float); !f = {|float, float -> int| int[_ + _1] }; f 10 11.2"),
        "errror"
    );
    //    assert_eq!(
    //        v("!:type @Num (int | float); !f = {|a: @Num, b: @Num -> int| int[_ + _1] }; f 10 11.2"),
    //        "21"
    //    );
    //    assert_eq!(
    //        v("!:type @Num (int | float); !f: fn(@Num, @Num) -> int = { int[_ + _1] }; f 10 11.2"),
    //        "21"
    //    );
    //    assert_eq!(
    //        v("!:type Numi (int | float); !f = {|Numi, Numi -> int| int[_ + _1] }; f 10 11.2"),
    //        "error 10 or 11.2 is not \"Numi\"."
    //    );
}

#[test]
fn chk_types_named() {
    assert_eq!(v("!o: Num = 10; $typeof o"), "$p(10,$type(Num))");
    assert!(
        v("!o: Num = 10.12; .o = 10; o").find("Mismatch Num <=> int").is_some(),
        "Nominally typed stuff can't be assigned."
    );
    assert_eq!(v("!o: Num = 10.12; !o2: Num = 10; .o = o2; o"), "02");
    assert_eq!(v("!o: int | float = 10.12; .o = 10; o"), "");
    //    assert!(r.find("Wrong argument 2").is_some(), "Catch bad argument.");
}

#[test]
fn chk_types_def() {
    assert_eq!(
        v("!:type OneDimPoint int; !x: OneDimPoint = 120; ($typeof x) => OneDimPoint"),
        "$p($p(120,$type(OneDimPoint)),$type(int))"
    );
}
