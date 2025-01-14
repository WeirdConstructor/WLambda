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
fn chk_types_function() {
    assert_eq!(v("!x: fn <O is (Num),X>(a: O, b: float) -> X = $n"), "");
}

#[test]
fn chk_types_aliasing() {
    //    assert_eq!(v("!:type @Num int | float; !x: @Num = 10; x"), "10");
    //    assert_eq!(v("!:type @Num int | float; !x: @Num = 10.3; x"), "10.3");
    //    assert_eq!(
    //        v("!:type @Num int | float; !x: @Num = \"test\"; $typeof x"),
    //        "expected (int | float), but got (str); in variable definition 'x'"
    //    );

//    assert_eq!(
//        v("!:type @Num int | float; !v: int = 10; !x: @Num = 0; .x = v; $typeof x"),
//        "$p($type(int),10)"
//    );
//    assert_eq!(
//        v("!:type @Num int | float; !v: float = 10.4; !x: @Num = 0; .x = v; $typeof x"),
//        "expected (int), but got (float); in assignment to 'x'"
//    );
//    assert_eq!(
//        v("!:type @Num int | float; !v: float = 10.4; !x: @Num = 0.0; .x = v; $typeof x"),
//        "$p($type(float),10.4)"
//    );
//    assert_eq!(
//        v("!:type @Num int | float; !v: str = \"test\"; !x: @Num = 0; .x = v; $typeof x"),
//        "expected (int), but got (str); in assignment to 'x'"
//    );
//    assert_eq!(
//        v("!:type @Num (int | float); !f = {|@Num, @Num -> int| int[_ + _1] }; f 10 11.2"),
//        "21"
//    );
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
