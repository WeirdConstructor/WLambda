use wlambda::*;
use wlambda::compiler::{CompileEnv, ResPos, ResValue};
use wlambda::vm::*;
use wlambda::ops::Prog;
use std::rc::Rc;
use std::cell::RefCell;

pub fn v(s: &str) -> String {
    let mut ctx = EvalContext::new_default();
    match ctx.eval(s) {
        Ok(v) => v.s(),
        Err(e) => format!("{}", e),
    }
}

pub fn ve(s: &str) -> String {
    let global = GlobalEnv::new_default();
    match parser::parse(s, "<compiler:s_eval>") {
        Ok(ast) => {
            let mut ce = CompileEnv::new(global.clone());
            match vm_compile2(&ast, &mut ce) {
                Ok(prog) => {
                    let local_space = ce.borrow().get_local_space();

                    let mut p = Prog::new();
                    prog.eval_to(&mut p, ResPos::Value(ResValue::Ret));
                    p.op_end();

                    let mut e = Env::new(global);
                    e.push(VVal::Int(10));
                    e.push(VVal::Flt(14.4));
                    e.argc = 2;
                    e.set_bp(0);
                    e.push_sp(local_space);

                    match vm(&p, &mut e) {
                        Ok(v) => v.s(),
                        Err(je) => {
                            format!("EXEC ERR: Caught {:?}", je)
                        }
                    }
                },
                Err(re) => format!("COMPILE ERROR: {}", re),
            }
        }
        Err(e)  => format!("PARSE ERROR: {}", e),
    }
}



#[test]
fn check_function_string_rep() {
    assert_eq!(ve("!upv1 = \"lol!\"; str {|1<3| !x = 1; !g = 2; upv1 }"),
               "\"&F{@[1,21:<compiler:s_eval>(Func)@upv1],amin=1,amax=3,locals=2,upvalues=$[$(&)\\\"lol!\\\"]}\"");
    assert_eq!(ve("!upv1 = $&& \"lol!\"; str {|1<3| !x = 1; !g = 2; upv1 }"),
               "\"&F{@[1,23:<compiler:s_eval>(Func)@upv1],amin=1,amax=3,locals=2,upvalues=$[$&&\\\"lol!\\\"]}\"");
    assert_eq!(ve("!upv1 = \"lol!\"; {|1<3| !x = 1; !g = 2; upv1 }"),
               "&F{@[1,17:<compiler:s_eval>(Func)@upv1],amin=1,amax=3,locals=2,upvalues=$[$n]}");
    assert_eq!(ve("!upv1 = $&& \"lol!\"; {|1<3| !x = 1; !g = 2; upv1 }"),
               "&F{@[1,19:<compiler:s_eval>(Func)@upv1],amin=1,amax=3,locals=2,upvalues=$[$&&\"lol!\"]}");
}

#[test]
fn check_pick() {
    assert_eq!(ve("pick $t :v :h"), ":v");
    assert_eq!(ve("pick $f :v :h"), ":h");
}

#[test]
fn check_simple_indexing() {
    assert_eq!(ve("$[1,2].0"),               "1");
    assert_eq!(ve("$[$[3,2],2].0.0"),        "3");
    assert_eq!(ve("$[$[$[5,6],2],2].0.0.0"), "5");
    assert_eq!(ve("${a=30}.a"),              "30");
    assert_eq!(ve("${a=${b=31}}.a.b"),       "31");
    assert_eq!(ve("${a=${b=${c=32}}}.a.b.c"),"32");
    assert_eq!(ve("${a=30}.(\"\" \"a\")"),              "30");
    assert_eq!(ve("${a=${b=31}}.(\"\" \"a\").b"),       "31");
    assert_eq!(ve("${a=${b=${c=32}}}.(\"\" \"a\").b.c"),"32");
    assert_eq!(ve(r"
        !o = ${ b = 10 };
        !o = $[10];
        !o2 = ${ b = 10 };
        o2"), "${b=10}");
}

#[test]
fn check_list_boolean_indexing() {
    assert_eq!(ve("$[\"hi\", \"there\"].$t"),
               ve("pick $f \"hi\" \"there\""));
    assert_eq!(ve("$[94, 38].(is_vec $[])"),                                        "38");
    assert_eq!(ve("{ !l = $[]; range 10 20 5 { std:push l _ }; l }[].$t"),          "15");
    assert_eq!(ve("{ !l = $[]; range 10 20 5 { std:push l _ }; l }[].(is_none 1)"), "10");
    assert_eq!(ve("$[$[1, 2], $[3, 4]] $t"),                                        "4");
    assert_eq!(ve("$[$[1, 2], $[3, 4]] $f"),                                        "3");
    assert_eq!(ve("$@v$[$[1, 2], $[3, 4]] \\_|$t|$+"),                              "$[2,4]");
    assert_eq!(ve("$@v$[$[1, 2], $[3, 4]] \\_|$f|$+"),                              "$[1,3]");
    assert_eq!(ve("$f $[:a, :b]"),                                                  ":a");
    assert_eq!(ve("$t $[:a, :b]"),                                                  ":b");
}

#[test]
fn check_trivial() {
    assert_eq!(ve("_"),                       "10");          // XXX: in test env
    assert_eq!(ve("_1"),                      "14.4");        // XXX: in test env
    assert_eq!(ve("@"),                       "$[10,14.4]");  // XXX: in test env

    assert_eq!(ve("$n"), "$n");
    assert_eq!(ve("10"), "10");
    assert_eq!(ve("10; 20; 30"),              "30");
    assert_eq!(ve("!x = 10; x"),              "10");
    assert_eq!(ve("!x = $true; x"),           "$true");
    assert_eq!(ve("{ 10 }"),                  "&F{@[1,1:<compiler:s_eval>(Func)],amin=0,amax=0,locals=0,upvalues=$[]}");
    assert_eq!(ve("{ 10 }[]"),                "10");
    assert_eq!(ve("{ 10; 20 }[]"),            "20");
    assert_eq!(ve("!x = $&11; { 12; x }[]"),                   "11");
    assert_eq!(ve("!x = 11; { 12; x }[]"),                     "11");
    assert_eq!(ve("!x = 13; { .x = 12 }[]; { x }[] "),         "12");
    assert_eq!(ve("!x = 13; { .x = 12 }[]; $[{ x }[], x]"),  "$[12,12]");
    assert_eq!(ve("!x = 13; { .x = 12; x }[]; $[{ x }[], { .x = 15; x }[], x]"), "$[12,15,15]");
    assert_eq!(ve("{ _ } 10"),                   "10");
    assert_eq!(ve("!y = 0; { .y = _ } 10; y"),   "10");
    assert_eq!(ve("${:a = 10, :b = 20}"),             "${a=10,b=20}");
    assert_eq!(ve("${:b = 20, :a = 10}"),             "${a=10,b=20}");
    assert_eq!(ve("${a = 10, b = 20}"),               "${a=10,b=20}");
    assert_eq!(ve("${b = 20, a = 10}"),               "${a=10,b=20}");
    assert_eq!(ve("${(:a) = 10, b = 20}"),            "${a=10,b=20}");
    assert_eq!(ve("${(:b) = 20, a = 10}"),            "${a=10,b=20}");
    assert_eq!(ve("!x = ${:b = 20, :a = 10}; x"),     "${a=10,b=20}");
    assert_eq!(ve("!x = ${:b = 20, :a = 10}; x.a"),   "10");
    assert_eq!(ve("!x = ${:b = 20, :a = 11}; :a x"),  "11");
    assert_eq!(ve("!x = ${}; x.a = 12; x.a"),         "12");
    assert_eq!(ve("!x = ${}; x.a = 12; x"),           "${a=12}");

    assert_eq!(ve("$[33,44,55].2"), "55");
    assert_eq!(ve("$[33,44,55].0"), "33");
    assert_eq!(ve("$[33,44,55].3"), "$n");
    assert_eq!(ve("1 $[33,44,55]"), "44");
}

#[test]
fn check_ref_closures() {
    assert_eq!(ve("!c1 = { !a = $&& 1.2; { $*a } }; c1[][]"),            "1.2");
    assert_eq!(ve("!c1 = { !a = $& 1.2; { a } }; c1[][]"),           "$n");
    assert_eq!(ve("!c1 = { !a = $& 1.2; { a }[] }; c1[]"),           "1.2");
    assert_eq!(ve("!c1 = { !a = $& 1.2; !a = $n; { a }[] }; c1[]"),  "$n");
    assert_eq!(ve("!outer_a = $&2.3; !c1 = { !a = $&&1.2; { $*a + outer_a } }; c1[][]"), "3.5");
    assert_eq!(ve("!outer_a = $&2.3; !c1 = { !a = $&1.2; { outer_a + a } }; c1[][]"), "2.3");
    assert_eq!(ve("!outer_a = $&2.3; !c1 = { !a = $&1.2; { outer_a + a } }; .outer_a = $n; c1[][]"), "0");
    assert_eq!(ve(r"
        !x = $&$[1,2,3];
        !y = $&&$[1,2,3];
        !z = std:weaken y;
        $[$@v x \$+ _ * 2, $@i y \$+ _ * 2, z \_ * 2]
    "), "$[$[2,4,6],12,6]");
}

#[test]
fn check_arithmetics() {
    assert_eq!(ve("12 + 23"),         "35");
    assert_eq!(ve("+[12, 23]"),       "35");
    assert_eq!(ve("+ 12 23"),         "35");
    assert_eq!(ve("+ 12 ~ - 24 23"),  "13");
    assert_eq!(ve("(+ 12 ~ - 24 23) + 1"),    "14");
    assert_eq!(ve("(12 + 1) == 13"),          "$true");
    assert_eq!(ve("(+ 12 ~ - 24 23) == 13"),  "$true");
    assert_eq!(ve("(+ 12 ~ - 24 23) == 14"),  "$false");
    assert_eq!(ve("12.12 + 23.23"),           "35.35");

    // coertion of strings and keys to numbers:
    assert_eq!(ve(":10 + :20"),       "30");
    assert_eq!(ve(":-10 + :20"),      "10");

    assert_eq!(ve("12 - 23"),         "-11");
    assert_eq!(ve("5 * 4"),           "20");
    assert_eq!(ve("20 / 5"),          "4");

    assert_eq!(ve("6 - 3 * 2"),       "0");
    assert_eq!(ve("12 / 6 - 3 * 2"),  "-4");
}

#[test]
fn check_unary_plus_minus() {
    assert_eq!(ve("!x = 10; -x"), "-10");
    assert_eq!(ve("!x = 10; +x"), "10");
    assert_eq!(ve("- (+ 0xF)"),   "-15");
}

#[test]
fn check_compile_env() {
//        let ce = CompileEnv::create_env(None);
//
//        assert_eq!(ce.borrow_mut().def("x", false), 0);
//        assert_eq!(ce.borrow_mut().def("y", false), 1);
//        assert_eq!(ce.borrow_mut().def("z", false), 2);
//
//        let ce2 = CompileEnv::create_env(Some(ce.clone()));
//        assert_eq!(ce2.borrow_mut().def("a", false), 0);
//        assert_eq!(ce2.borrow_mut().get("y"), VarPos::UpValue(0));
//        assert_eq!(ce2.borrow_mut().get("z"), VarPos::UpValue(1));
//        assert_eq!(ce2.borrow_mut().get("a"), VarPos::Local(0));
//
//        let ce3 = CompileEnv::create_env(Some(ce2.clone()));
//        assert_eq!(ce3.borrow_mut().get("a"), VarPos::UpValue(0));
//        assert_eq!(ce3.borrow_mut().get("z"), VarPos::UpValue(1));
//
//        assert_eq!(ce2.borrow_mut().get("x"), VarPos::UpValue(2));
}

#[test]
fn check_bool() {
    assert_eq!(ve("!a = $&0; $t { .a = 1 } { .a = 2 }; $*a"), "1");
    assert_eq!(ve("!a = $&0; $f { .a = 1 } { .a = 2 }; $*a"), "2");
}

#[test]
fn check_range() {
    assert_eq!(ve("!x = $& 10; { .x = x + _; x } 5"),                    "15");
    assert_eq!(ve("!x = $& 10; { .x = { x + 11 + _ }(2) + _; x } 5"),    "28");
    assert_eq!(ve("!x = $& 10;   range 1 3 1     { .x = x + _; x }; $*x"), "16");
    assert_eq!(ve("!x = $& 10.0; range 1.0 3 0.5 { .x = x + _; x }; $*x"), "20");
}

#[test]
fn check_push() {
    assert_eq!(ve("!a = 10; !x = $[1]; !y = 20; x"), "$[1]");
    assert_eq!(ve("!x = $&&$[]; std:push $*x 12; $*x"), "$[12]");
    assert_eq!(ve("!a = 10; !x = $[]; !y = 20; std:push x 10; std:push x 30; x"), "$[10,30]");
    assert_eq!(ve("!x = $&&$[]; std:push $*x 10; std:push $*x 20; $*x"), "$[10,20]");
}

#[test]
fn check_range_break() {
    assert_eq!(ve("4 == 4"), "$true");
    assert_eq!(ve("range 0 10 1 {|1| break 14 }"), "14");
    assert_eq!(ve("range 0 10 1 { !i = _; (i == 4) { break ~ i + 10 } }"), "14");
}

#[test]
fn check_range_next() {
    assert_eq!(ve("!x = $&&0; range 0 10 1 { (_ == 4) { next[] }; .*x = $*x + _; }; $*x"), "51");
    assert_eq!(ve("!x = $&&0; range 0 10 1 { next[]; .*x = $*x + _; }; $*x"), "0");
    assert_eq!(ve("!x = $&0; range 0 10 1 { (_ == 4) { next[] }; .x = x + _; }; $*x"), "51");
    assert_eq!(ve("!x = $&0; range 0 10 1 { next[]; .x = x + _; }; $*x"), "0");
}

#[test]
fn check_loop_info_unwind() {
    assert_eq!(ve(r"
        !y = 0;
        range 0 10 1 {
            .y = y + _;
            next[];
            .y = y + 100;
        };
        y
    "), "55");

    assert_eq!(ve(r"
        !x = 0;
        is_none ~ while { x < 10 } {
            .x = x + 1;
            1 2 3 4 next[];
        };
    "), "$true");

    assert_eq!(ve(r"
        !x = 0;
        !y = 0;
        while { x < 10 } {
            .y = 0;
            range 0 10 1 {
                .y = y + _;
                next[];
                .y = y + 100;
            };
            .x = x + 1;
            next[];
            .x = x + 100;
        };
        $p(x, y)
    "), "$p(10,55)");
}

#[test]
fn check_while() {
    assert_eq!(ve(r#"
        !x = $& 0;
        while { x == 0 } {
            .x = x + 1;
        };
        $*x
    "#),
    "1");

    assert_eq!(ve(r#"
        !x = $&0;
        while { x < 2 } {
            .x = x + 1;
            next[];
            .x = x + 100;
        };
        $*x
    "#),
    "2");

    assert_eq!(ve(r#"
        !x = $&0;
        while { x < 2 } {
            !k = std:to_drop 10 {|| .x = 99; };
            .x = x + 1;
            next[];
        };
        $*x
    "#),
    "99");

    assert_eq!(ve(r#"
        !x = $&0;
        while { x < 2 } {
            !k = std:to_drop 10 {|| .x = 89; };
            .x = x + 1;
        };
        $*x
    "#),
    "89");

    assert_eq!(ve(r#"
        !x = $&0;
        while { x == 0 } {
            .x = 20;
            break[];
            .x = x + 1;
        };
        x
    "#),
    "20");

    assert_eq!(ve(r#"
        !i = 0;

        while $true {
            (i >= 4) break;
            .i = i + 1;
        };

        i
    "#),
    "4");

    assert_eq!(ve(r#"
        !x = 0;
        $@v
            while x < 4 {
                !y = 4;
                while y > 0 {
                    .y = y - 1;
                    ? x % 2 == 0 { !kk = 3; next[]; };
                    ? y < 2 { !kk = 3; break[]; };
                    $+ $p(x, y);
                };
                .x = x + 1;
            };
    "#), "$[$p(1,3),$p(1,2),$p(3,3),$p(3,2)]");

//    assert_eq!(ve("while 1"),
//        "COMPILE ERROR: [1,7:<compiler:s_eval>] Compilation Error: while takes exactly 2 arguments (condition and expression)");
//    assert_eq!(ve("while 1 2 3"),
//        "COMPILE ERROR: [1,7:<compiler:s_eval>] Compilation Error: while takes exactly 2 arguments (condition and expression)");
}

#[test]
fn check_args() {
    assert_eq!(ve("{ $[_, _1, _2] }[1, 2, 3]"),       "$[1,2,3]");
    assert_eq!(ve("{ @ }[1, 2, 3]"),                  "$[1,2,3]");
    assert_eq!(ve("{|3<4| $[_, _1, _2, _3] }[1, 2, 3]"),   "$[1,2,3,$n]");
}

#[test]
fn check_to_drop() {
    assert_eq!(ve("!x = $&1; { .x = 2; }[]; $*x"), "2");
    assert_eq!(ve("!x = $&1; { !d = { .x = 2; }; d }[][]; $*x"), "2");
    assert_eq!(ve(r#"
        !x = $&0;
        { !d = std:to_drop 10 {|| .x = 17; } }[];
        $*x
    "#),
    "17");
    assert_eq!(ve(r#"
        !k = 10;
        !j = 20;
        !x = $&0;
        !f = std:to_drop 33 {|| .x = 18; };
        f + 20;
        .f = $n;
        $*x
    "#),
    "18");
    assert_eq!(ve(r"
        !l = $&0;
        !x = std:to_drop $[1,2,3] {|| .l = 18; };
        .x = $@v x { $+ _ * 2 };
        $[$*l, x]
    "), "$[18,$[2,4,6]]");
    assert_eq!(ve("!x = std:to_drop $[1,2] {||}; x.1 = 3; x"),
               "std:to_drop[$[1,3]]");
    assert_eq!(ve("!x = std:to_drop ${a=2} {||}; x.a = 3; x"),
               "std:to_drop[${a=3}]");
    assert_eq!(ve("
        { !k = $&0;
          { .k = 20; }[];
          std:to_drop $[] {
            # XXX: only works, because to_drop disables arity checks!
            .k = 10;
            std:displayln :foo;
          };
          $*k }[]
    "), "10");
}

#[test]
fn check_to_no_arity() {
    assert_eq!(
        ve("(std:to_no_arity {!(x) = @; $[x, x + 1] })[]"),
        "$[$n,1]");
}

#[test]
fn check_strengthen() {
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = ${ del = std:to_drop 1 { .dropper = 1; } };
            !f = { .k = $n; };
            .k = $n;
            dropper
        "#), "1");
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = ${};
            k.d = ${ k = { k }, del = std:to_drop 1 { .dropper = 1; } };
            .k = $n;
            dropper
        "#), "1");
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = $&${};
            k.d = ${ k = { k }, del = std:to_drop 1 { .dropper = 1; } };
            .k = $n;
            dropper
        "#), "1");
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = std:strengthen $&${};
            k.d = ${ k = { k }, del = std:to_drop 1 { .dropper = 1; } };
            .k = $n;
            dropper
        "#), "0");
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = $&&${};
            k.d = ${ k = $*k, del = std:to_drop 1 { .dropper = 1; } };
            .k = $n;
            dropper
        "#), "0");
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = std:strengthen $&${};
            k.d = ${ k = k, del = std:to_drop 1 { .dropper = 1; } };
            k.d.k = $n;
            .k = $n;
            dropper
        "#), "1");
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = std:strengthen $&${ del = std:to_drop 1 { .dropper = 1; } };
            !f = { .k = $n; };
            !r = dropper;
            f[];
            .r = r + dropper;
            r
        "#), "1");
}

#[test]
fn check_call_primitives() {
    assert_eq!(ve("13[]"), "13");
    assert_eq!(ve("$t[]"), "$true");
    assert_eq!(ve(":foo"), ":foo");
    assert_eq!(ve("$n[]"),
        "EXEC ERR: Caught [1,3:<compiler:s_eval>(Call)] SA::Panic(\"Calling $none is invalid\")");
}

#[test]
fn check_global_vars() {
    assert_eq!(ve("
        !:global x = 120;
        !f = { x };
        !l = f[];
        .x = 30;
        !l2 = f[];
        $[l, l2, x]
    "), "$[120,30,30]");
    assert_eq!(ve("
        !:global x = 120;
        !f = {
            !(x, b) = @;
            .x = x + 2;
            $[x, b]
        };
        !l = f[1, 2];
        .x = 30;
        !l2 = f[3, 4];
        $[l, l2, x]
    "), "$[$[3,2],$[5,4],30]");
    assert_eq!(ve("
        !:global x = 120;
        !f = {
            !(y, b) = @;
            .x = x + 2;
            $[y, x, b]
        };
        !l = f[1, 2];
        .x = x + 30;
        !l2 = f[3, 4];
        $[l, l2, x]
    "), "$[$[1,122,2],$[3,154,4],154]");
    assert_eq!(ve("
        !f = { !:global (a, b, c) = @; };
        .b = 20;
        !x1 = $[a, b, c];
        f[13, 17, 43];
        !x2 = $[a, b, c];
        $[x1, x2]
    "), "$[$[$n,20,$n],$[13,17,43]]");
}

#[test]
fn check_and_or() {
    assert_eq!(ve("$t &and $t"),                "$true");
    assert_eq!(ve("$f &and $t"),                "$false");
    assert_eq!(ve("$t &and 10 &and (1 + 3)"),   "4");
    assert_eq!(ve("$t &and 0 &and (1 + 3)"),    "0");
    assert_eq!(ve("\"\" &and (4+3) &and (1 + 3)"), "\"\"");
    assert_eq!(ve(r"
        !x = 0;
        !inc = { .x = x + 1; x };
        !a = inc[] &and inc[] &and inc[] &and inc[];
        !b = x;
        $p(a,b)
    "), "$p(4,4)");

    assert_eq!(ve("$t &or  $t"),    "$true");
    assert_eq!(ve("$f &or  $t"),    "$true");
    assert_eq!(ve("$f &or  $f"),    "$false");
    assert_eq!(ve("$f\n&or\n$f\n&or\n0\n&or\n10"), "10");
    assert_eq!(ve("11\n&or\n$f\n&or\n0\n&or\n10"), "11");
    assert_eq!(ve("$f\n&or\n12\n&or\n0\n&or\n10"), "12");
    assert_eq!(ve("$f\n&or\n\"\"\n&or\n13\n&or\n10"), "13");
    assert_eq!(ve("$f\n&or\n(1 + 2 * 4)\n&or\n13\n&or\n10"), "9");
    assert_eq!(ve("(8 - 2 * 4)\n&or\n(1 + 2 * 4)\n&or\n13\n&or\n10"), "9");

    assert_eq!(ve(r"
        !x = 0;
        !inc = { .x = x + 1; x };
        !a = inc[] &or inc[] &or inc[] &or inc[];
        !b = x;
        $p(a,b)
    "), "$p(1,1)");
}

#[test]
fn check_ops() {
    assert_eq!(ve("10 < 20"),     "$true");
    assert_eq!(ve("11 < 10"),     "$false");
    assert_eq!(ve("10 < 10"),     "$false");
    assert_eq!(ve("10 > 20"),     "$false");
    assert_eq!(ve("11 > 10"),     "$true");
    assert_eq!(ve("10 > 10"),     "$false");
    assert_eq!(ve("10 <= 20"),    "$true");
    assert_eq!(ve("11 <= 10"),    "$false");
    assert_eq!(ve("10 <= 10"),    "$true");
    assert_eq!(ve("10 >= 20"),    "$false");
    assert_eq!(ve("11 >= 10"),    "$true");
    assert_eq!(ve("10 >= 10"),    "$true");
    assert_eq!(ve("10.1 < 20.4"), "$true");
    assert_eq!(ve("11.2 < 10.2"), "$false");
    assert_eq!(ve("10.3 < 10.4"), "$true");
    assert_eq!(ve("22 == 22"),    "$true");
    assert_eq!(ve("22 == 23"),    "$false");
    assert_eq!(ve("22 != 22"),    "$false");
    assert_eq!(ve("21 != 22"),    "$true");

    assert_eq!(ve("2 ^ 2"),       "4");
    assert_eq!(ve("2 ^ 3"),       "8");
    assert_eq!(ve("2.1 ^ 2"),     "4.41");
    assert_eq!(ve("4 ^ 0.5"),     "1");
    assert_eq!(ve("4.0 ^ 0.5"),   "2");

    assert_eq!(ve("4 % 5"),       "4");
    assert_eq!(ve("6 % 5"),       "1");
    assert_eq!(ve("4.4 % 5.5"),   "4.4");
    assert_eq!(ve("5.5 % 5.5"),   "0");

    assert_eq!(ve("std:neg_i64 0xFF"),    "-255");
    assert_eq!(ve("std:not_i64 0xFF"),    "-256");
    assert_eq!(ve("std:neg_u32 0xFF"),   "4294967041");
    assert_eq!(ve("std:neg_u32 0x1"),    "4294967295");
    assert_eq!(ve("std:neg_u32 0x0"),    "0");
    assert_eq!(ve("std:not_u32 0xFF"),   "4294967040");
    assert_eq!(ve("std:not_u32 0x1"),    "4294967294");
    assert_eq!(ve("std:not_u32 0x0"),    "4294967295");

    assert_eq!(ve("(0x10 &| 0x01) == 0x11"), "$true");
    assert_eq!(ve("(0x0f &  0x33) == 0x3"),  "$true");
    assert_eq!(ve("(0x11 &^ 0x01) == 0x10"), "$true");
    assert_eq!(ve("(0b1 << 1) == 0b10"),     "$true");
    assert_eq!(ve("(0b1 << 2) == 0b100"),    "$true");
    assert_eq!(ve("(0b1 >> 1) == 0x0"),      "$true");

    assert_eq!(ve("!x = 0; !b = { $t }[] &and { .x = 1; 10 }[]; $[x, b]"), "$[1,10]");
    assert_eq!(ve("!x = 0; !b = { $f }[] &and { .x = 1; 10 }[]; $[x, b]"), "$[0,$false]");
    assert_eq!(ve("!x = 0; !b = { $f }[] &or { .x = 1; 10 }[]; $[x, b]"),  "$[1,10]");
    assert_eq!(ve("!x = 0; !b = { 12 }[] &or { .x = 1; 10 }[]; $[x, b]"),  "$[0,12]");
    assert_eq!(ve(r#"
        !x = 0;
        !f = { std:displayln[x]; .x = x + 1; x };
        !b = f[]
            &and f[]
            &and f[]
            &and f[]
            &and f[];
        $[x, b]
    "#),  "$[5,5]");

    assert_eq!(ve(r#"
        !c = 0;
        !x = 10;
        while { x > 0 } { .c = c + 1; .x = x - 1; };
        .x = 20;
        while { x > 0 } { .c = c + 1; .x = x - 1; };
        c
    "#), "30");

    assert_eq!(ve(r"(7 & 15) >= 10"),   "$false");
    assert_eq!(ve(r"7 & (15 >= 10)"),   "1");
    assert_eq!(ve(r"7 & 15 >= 10"),     "1");
}

#[test]
fn check_destructure_pair() {
    assert_eq!(ve("!(e, m) = $p(1, $f); $p(m, e)"),       "$p($false,1)");
    assert_eq!(ve("!(e, m) = $p(1); $p(m, e)"),           "$p($n,1)");
    assert_eq!(ve("!(e, m) = $p($n,1); $p(m, m)"),        "$p(1,1)");
    assert_eq!(ve("!(e, m) = $p($n,1); $p(e, m)"),        "$p($n,1)");
    assert_eq!(ve("!(a, b) = $p($&10, $&20); { .a = 33; }[]; $p(a, b)"), "$p(33,20)");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $p($&&10, $&&20);
            $p({a}, { .a = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "33");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $p($&10, $&20);
            $p({$p(a, b)}, { .a = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "$p($n,$n)");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $p($&10, $&20);
            !(wa, wb) = $p(std:weaken a, std:weaken b);
            $p({$p(wa, wb)}, { .wa = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "$p($n,$n)");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $p($&&10, $&&20);
            $p({$p(a, b)}, { .a = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "$p(33,20)");

    assert_eq!(
        ve("!a = 0; !b = 0; .(a, b) = $p(10, 20); $p(a, b)"),
        "$p(10,20)");

    assert_eq!(
        ve("!a = 0; !b = 0; .(a, b) = 40; $p(a, b)"),
        "$p(40,40)");
}

#[test]
fn check_destructure() {
    assert_eq!(ve("!(a, b) = $[10, 20]; $[a, b]"),        "$[10,20]");
    assert_eq!(ve("!(a, b) = $[10, 20, 30]; $[a, b]"),    "$[10,20]");
    assert_eq!(ve("!(a, b) = $[10]; $[a, b]"),            "$[10,$n]");
    assert_eq!(ve(
        "!(a, b) = ${a = 10, b= 20, c=30}; $[a, b]"),
        "$[10,20]");
    assert_eq!(ve(
        "!(a, b) = ${a = 10}; $[a, b]"),
        "$[10,$n]");
    assert_eq!(ve(
        "!(a, b) = ${b = 20, c = 30}; $[a, b]"),
        "$[$n,20]");

    assert_eq!(ve("!(a, b) = $[$&10, $&20]; { .a = 33; }[]; $[a, b]"), "$[33,20]");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $[$&&10, $&&20];
            $[{a}, { .a = 33; }];
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "33");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $[$&10, $&20];
            $[{$[a, b]}, { .a = 33; }];
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "$[$n,$n]");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $[$&10, $&20];
            !(wa, wb) = $[std:weaken a, std:weaken b];
            $[{$[wa, wb]}, { .wa = 33; }];
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "$[$n,$n]");
    assert_eq!(ve(r#"
        !fun = {
            !(a, b) = $[$&&10, $&&20];
            $[{$[a, b]}, { .a = 33; }];
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
    "$[33,20]");

    assert_eq!(
        ve("!a = 0; !b = 0; .(a, b) = $[10, 20]; $[a, b]"),
        "$[10,20]");

    assert_eq!(
        ve("!a = 0; !b = 0; .(a, b) = 40; $[a, b]"),
        "$[40,40]");

    assert_eq!(ve("!(a, b, c, d) = $i(1, 2);        $[a, b, c, d]"), "$[1,2,$n,$n]");
    assert_eq!(ve("!(a, b, c, d) = $i(1, 2, 3);     $[a, b, c, d]"), "$[1,2,3,$n]");
    assert_eq!(ve("!(a, b, c, d) = $i(1, 2, 3, 4);  $[a, b, c, d]"), "$[1,2,3,4]");

    assert_eq!(ve("!(a, b, c, d) = $f(1, 2);        $[a, b, c, d]"), "$[1,2,$n,$n]");
    assert_eq!(ve("!(a, b, c, d) = $f(1, 2, 3);     $[a, b, c, d]"), "$[1,2,3,$n]");
    assert_eq!(ve("!(a, b, c, d) = $f(1, 2, 3, 4);  $[a, b, c, d]"), "$[1,2,3,4]");
}

#[test]
fn check_field() {
    assert_eq!(ve("!v = $[]; v.0 = 10; v"), "$[10]");
    assert_eq!(ve("!v = $[]; v.2 = 10; v"), "$[$n,$n,10]");
    assert_eq!(ve("!i = 2; !v = $[]; v.(i) = 10; v"), "$[$n,$n,10]");
}

#[test]
fn check_type() {
    assert_eq!(ve("type type"), "\"function\"");
    assert_eq!(ve("type 12"),   "\"integer\"");
    assert_eq!(ve("type 12.2"), "\"float\"");
    assert_eq!(ve("type $n"),   "\"none\"");
    assert_eq!(ve("type $[]"),  "\"vector\"");
    assert_eq!(ve("type ${}"),  "\"map\"");
}

#[test]
fn check_eqv() {
    assert_eq!(ve("1 == 1"),            "$true");
    assert_eq!(ve("1 == 2"),            "$false");
    assert_eq!(ve("1.1 == 1.1"),        "$true");
    assert_eq!(ve("1.1 == 1.2"),        "$false");
    assert_eq!(ve("1.0 == 1"),          "$false");
    assert_eq!(ve("1.0 == \"1\""),      "$false");
    assert_eq!(ve("$true == $true"),    "$true");
    assert_eq!(ve("$false == $true"),   "$false");
    assert_eq!(ve("$none == $n"),       "$true");
    assert_eq!(ve("$none == $false"),   "$false");
    assert_eq!(ve("0 == $false"),       "$false");
    assert_eq!(ve("\"abc\" == (std:str:cat \"a\" \"bc\")"),       "$true");
    assert_eq!(ve("\"abc\" == (std:str:cat \"b\" \"bc\")"),       "$false");
    assert_eq!(ve("$[] == $[]"),                    "$false");
    assert_eq!(ve("$[1,2] == $[1,2]"),              "$false");
    assert_eq!(ve("!a = $[1,2]; !b = a; a == b"),   "$true");
    assert_eq!(ve("!a = $[1,2]; a == a"),           "$true");
    assert_eq!(ve("!a = $[1,2]; a == $[1,2]"),      "$false");
    assert_eq!(ve("!a = ${l=3}; !b = a; a == b"),   "$true");
    assert_eq!(ve("!a = ${l=2}; a == a"),           "$true");
    assert_eq!(ve("!a = ${l=2}; a == a"),           "$true");
    assert_eq!(ve("!a = ${l=2}; a == ${l=2}"),      "$false");
    assert_eq!(ve(":a == :b"),                      "$false");
    assert_eq!(ve(":a == :a"),                      "$true");
    assert_eq!(ve("\"a\" == :a"),                   "$false");
    assert_eq!(ve("sym[\"a\"] == :a"),              "$true");
    assert_eq!(ve("std:str:to_bytes[\"a\"] == $b\"a\""), "$true");
    assert_eq!(ve("$b\"a\" == $b\"a\""),            "$true");
    assert_eq!(ve("$b\"b\" == $b\"a\""),            "$false");
    assert_eq!(ve("!f = {}; f == {}"),              "$false");
    assert_eq!(ve("!f = {}; f == f"),               "$true");
    assert_eq!(ve("($e :a) == ($e :b)"),            "$false");
    assert_eq!(ve("!e = $e :a; e == e"),            "$true");
    assert_eq!(ve(r#"
        !r  = $&& $&&0;
        !b  = $&& $&&0;
        !c  = $&& $&&$*r;
        !r2 = $&& r;
        $[r == b, r == c, r2 == r, std:weaken[r] == r, r == std:weaken[r]]
    "#),
    "$[$false,$false,$true,$true,$true]");
    assert_eq!(ve(r#"
        !r  = $& $&1;
        !r2 = $&& r;
        r == r2
    "#),
    "$true");
    assert_eq!(ve(r#"
        !r  = $& $&1;
        !r2 = $&& r;
        r == r2
    "#),
    "$true");
    assert_eq!(ve(r#"
        !r  = $& $&0;
        !b  = $& $&0;
        !c  = $& $&r;
        !r2 = $&& r;
        $[r == b, r == c, r2 == r, std:weaken[r] == r, r == std:weaken[r]]
    "#),
    "$[$false,$false,$true,$true,$true]");
}

#[test]
fn check_string() {
    assert_eq!(ve("\"foo\""),   "\"foo\"");
    assert_eq!(ve("$q#foo#"),   "\"foo\"");
    assert_eq!(ve("$b\"foo\""), "$b\"foo\"");

    assert_eq!(ve("\"foo\"(0)"),                       "\"f\"");
    assert_eq!(ve("\"foo\" 0"),                        "\"f\"");
    assert_eq!(ve("\"foobar\" 1 3"),                   "\"oob\"");
    assert_eq!(ve("\"foobar\"[1, 3]"),                 "\"oob\"");
    assert_eq!(ve("\"foobar\" $[1, 3]"),               "\"oob\"");
    assert_eq!(ve("\"foobar\" $q/xyz/"),               "\"foobarxyz\"");
    assert_eq!(ve("\"foobar\" ${ foobar = 12 }"),       "12");
    assert_eq!(ve("\"foobar\" ${ (\"foobar\") = 12 }"), "12");
    assert_eq!(ve("\"foobar\" 2 -1"),                  "\"obar\"");
    assert_eq!(ve("\"\" 2 -1"),                        "\"\"");
    assert_eq!(ve("\"foobar\" 6 -1"),                  "\"\"");
    assert_eq!(ve("\"foobar\" 6"),                     "\"\"");

    assert_eq!(ve("\"foo\" \"a\" :b $b\"c\""),         "\"fooabc\"");
}

#[test]
fn check_match() {
//    assert_eq!(ve("match 10 :?t :integer {|| 13 } {|| 14 }"), "13");
//    assert_eq!(ve("match 10 :?t :string {|| 13 } {|| 14 }"), "14");
//    assert_eq!(ve("match 10 :?t :string {|| 13 }"),        "$n");
//    assert_eq!(ve("match $q xx :?s :xx      {|| 15 }"),    "15");
//    assert_eq!(ve("match $q xx :?s :yx :xx  {|| 16 }"),    "16");
//    assert_eq!(ve("match $q zx :?s :yx :xx  {|| 16 } {|| 17 }"), "17");
//    assert_eq!(ve("match $q xx :?p { _ == $q|xx| } {|| 18 }"),    "18");
//    assert_eq!(ve("match $q x9 :?p { _ == $q|xx| } {|| 181 } {|| 19 }"), "19");
//    assert_eq!(ve("match 10"),                           "$n");
//    assert_eq!(ve("
//        match ($e $[:foo, 1, 2, 3])
//            :?e :foo {|| 19 }
//            :?e :bar {|| 19.2 }
//            :?e :sna :snu {|| 19.4 }
//            { :nothin }
//    "), "19");
//    assert_eq!(ve("
//        match (
//            $e $[:bar, 1, 2, 3] )
//            :?e :foo {|| 19 }
//            :?e :bar {|4| $[(2 _) + 19.2, _1] }
//            :?e :sna :snu {|| 19.4 }
//            { :nothin }
//    "), "$[21,3]");
//    assert_eq!(ve("
//        match ($e $[:snu, 1, 2, 3])
//            :?e :foo {|| 19 }
//            :?e :bar {|| 19.2 }
//            :?e :sna :snu {|| 19.4 + _.3 }
//            { :nothin }
//    "), "22.4");
}

#[test]
fn check_callbacks() {
    let global = GlobalEnv::new_default();
    global.borrow_mut().add_func("reg", |env: &mut Env, _argc: usize| {
        let fun = env.arg(0);
        env.with_user_do(|v: &mut Vec<VVal>| v.push(fun.clone()));
        Ok(VVal::None)
    }, Some(1), Some(1));

    let reg : Rc<RefCell<Vec<VVal>>> = Rc::new(RefCell::new(Vec::new()));

    let mut ctx = EvalContext::new_with_user(global, reg.clone());
    ctx.eval("reg { _ + 10 }").unwrap();
    let n = reg.borrow_mut()[0].clone();
    let ret = ctx.call(&n, &vec![VVal::Int(11)]).unwrap();
    assert_eq!(ret.i(), 21);
}

#[test]
fn check_returned_functions() {
    let mut ctx = EvalContext::new_default();
    let n = ctx.eval("{ _ + 11 }").unwrap();
    let ret = ctx.call(&n, &vec![VVal::Int(11)]).unwrap();
    assert_eq!(ret.i(), 22);
}

#[test]
fn check_global_var_api() {
    let mut ctx = EvalContext::new_default();
    ctx.set_global_var("XXX", &VVal::Int(210));
    ctx.set_global_var("YYY", &VVal::None);
    let n = ctx.eval("{ .YYY = _ + 11; XXX + _ } 20").unwrap();
    assert_eq!(ctx.get_global_var("YYY").unwrap().i(), 31);
    assert_eq!(n.i(), 230);
}

#[test]
fn check_return() {
    assert_eq!(ve("block {
        !x = { return 11; 20 }[];
        .x = x + 20;
        x
    }"), "31");
    assert_eq!(ve("block {
        !x = { 13 }[];
        .x = 20;
        x
    }"), "20");
    assert_eq!(ve("block :x {
        !x = { return :x 10; 20 }[];
        .x = x + 20;
        x
    }"), "10");
    assert_eq!(ve("\\:x {
            !x = { return :x 10; 20 }[];
            .x = x + 20;
            x
        }[]
    "), "10");
    assert_eq!(ve("{ 10; 20 }[]"), "20");
    assert_eq!(ve("!g = { _1 }; g :x 10"), "10");
    assert_eq!(ve("block {
        !x = { block :x { return :x 13; 20 } }[];
        .x = x + 12;
        x
    }"), "25");
}

#[test]
fn check_arity() {
    assert_eq!(ve("{}[1,2,3]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,3:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 3\")");
    assert_eq!(ve("{|3| _1 }[1,2,3]"), "2");
    assert_eq!(ve("{|3| _1 }[2,3]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,10:<compiler:s_eval>(Call)] SA::Panic(\"function expects at least 3 arguments, got 2\")");
    assert_eq!(ve("{|3| _1 }[2,3,4,5]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,10:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 3 arguments, got 4\")");
    assert_eq!(ve("{|0<4| _1 }[]"), "$n");
    assert_eq!(ve("{|0<4| _1 }[1]"), "$n");
    assert_eq!(ve("{|0<4| _1 }[1,2]"), "2");
    assert_eq!(ve("{|0<4| _1 }[1,2,3]"), "2");
    assert_eq!(ve("(\\|0<4| _1)[1,2,3]"), "2");
    assert_eq!(ve("{|0<4| _1 }[1,2,3,4]"), "2");
    assert_eq!(ve("{|0<4| _1 }[1,2,3,4,5]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,12:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 4 arguments, got 5\")");
    assert_eq!(ve("{ @ }[1,2,3,4,5]"), "$[1,2,3,4,5]");
    assert_eq!(ve("{|2| @ }[1,2]"), "$[1,2]");
    assert_eq!(ve("{|2| @ }[1]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,9:<compiler:s_eval>(Call)] SA::Panic(\"function expects at least 2 arguments, got 1\")");
    assert_eq!(ve("{|2| @ }[1,2,3]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,9:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 2 arguments, got 3\")");

    assert_eq!(ve("{!(a,b,c) = @;}[1,2,3,4]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,16:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 3 arguments, got 4\")");
    assert_eq!(ve("{_3; !(a,b,c) = @; }[1,2,3,4,5]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,21:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 4 arguments, got 5\")");
    assert_eq!(ve("{!(a,b,c) = @; _3 }[1,2,3,4,5]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,20:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 4 arguments, got 5\")");
    assert_eq!(ve("{!(a,b,c) = @; b }[1,2,3]"), "2");
    assert_eq!(ve("{!(a,b,c) = @; _3 }[1,2,3,5]"), "5");
    assert_eq!(ve("{!:global (a,b,c) = @; _3 }[1,2,3,5]"), "5");
    assert_eq!(ve("{!:global (a,b,c) = @; _3 }[1,2,3,4,5]"),
        "EXEC ERR: Caught [1,1:<compiler:s_eval>(Func)]=>\
        [1,28:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 4 arguments, got 5\")");
}

#[test]
fn check_error_fn_pos() {
    assert_eq!(ve(r#"

        !x = {

        };

        !l = { x 10 };
        l[];
    "#),
    "EXEC ERR: Caught [3,14:<compiler:s_eval>(Func)@x]=>\
     [7,18:<compiler:s_eval>(Call)]=>\
     [7,14:<compiler:s_eval>(Func)@l]=>\
     [8,10:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 1\")");
}

#[test]
fn check_error() {
    assert_eq!(ve("$e 10; 14"),
        "EXEC ERR: Caught [1,4:<compiler:s_eval>(Err)] SA::Panic(\"Dropped error value: 10\")");
    assert_eq!(ve("{ { { { $e 10; 14 }[]; 3 }[]; 9 }[]; 10 }[]"),
        "EXEC ERR: Caught [1,12:<compiler:s_eval>(Err)]=>\
        [1,7:<compiler:s_eval>(Func)]=>\
        [1,20:<compiler:s_eval>(Call)]=>\
        [1,5:<compiler:s_eval>(Func)]=>\
        [1,27:<compiler:s_eval>(Call)]=>\
        [1,3:<compiler:s_eval>(Func)]=>\
        [1,34:<compiler:s_eval>(Call)]=>\
        [1,1:<compiler:s_eval>(Func)]=>\
        [1,42:<compiler:s_eval>(Call)] SA::Panic(\"Dropped error value: 10\")");
    assert_eq!(ve("_? $e 10"),
        "EXEC ERR: Caught SA::Return(lbl=$n,$e[1,7:<compiler:s_eval>(Err)] 10)");
    assert_eq!(ve("_? { return $e 10; 10 }[]"),
        "EXEC ERR: Caught SA::Return(lbl=$n,$e[1,16:<compiler:s_eval>(Err)] 10)");
    assert_eq!(ve("unwrap $e 1"),
        "EXEC ERR: Caught [1,11:<compiler:s_eval>(Err)]=>\
        [1,8:<compiler:s_eval>(Call)] SA::Panic(\"unwrap error: 1\")");
    assert_eq!(ve("unwrap 1.1"), "1.1");
    assert_eq!(ve("on_error {|4| _ + 20 } $e 19.9"), "39.9");

    assert_eq!(ve("{ { { panic 102 }[]; 20 }[]; return 20 }[]; 49"),
        "EXEC ERR: Caught [?]=>\
        [1,13:<compiler:s_eval>(Call)]=>\
        [1,5:<compiler:s_eval>(Func)]=>\
        [1,18:<compiler:s_eval>(Call)]=>\
        [1,3:<compiler:s_eval>(Func)]=>\
        [1,26:<compiler:s_eval>(Call)]=>\
        [1,1:<compiler:s_eval>(Func)]=>\
        [1,41:<compiler:s_eval>(Call)] SA::Panic(102)");

    assert_eq!(ve("
        !x = $&10;
        {
            .x = x + 1;
            block :outer { .x = x + 1; };
            .x = x + 1;
        }[];
        $*x
    "), "13");
    assert_eq!(ve("
        !gen_err = { $e $q$something_failed!$ };
        !x = $&10;
        !msg = $&$q'all ok';
        {
            .x = x + 1;
            on_error {|4| .x = x * 2; .msg = _; }
                (block :outer { _? :outer gen_err[]; .x = x + 1; });
            .x = x + 1;
        }[];
        $[$*x, $*msg]
    "), "$[23,\"something_failed!\"]");
    assert_eq!(ve("
        !gen_ok = { 99 };
        !x = $&10;
        !msg = $&$q'all ok';
        {
            .x = x + 1;
            on_error { .x = x * 2; .msg = _; }
                ~ block :outer { _? :outer gen_ok[]; .x = x + 1; };
            .x = x + 1;
        }[];
        $[$*x, $*msg]
    "), "$[13,\"all ok\"]");

    assert_eq!(ve("{ $e 23 }[] | on_error {|4| _ + 21 }"), "44");

    assert_eq!(ve("!x = $[$e 181];"),
        "EXEC ERR: Caught [1,11:<compiler:s_eval>(Err)] SA::Panic(\"Error value in list element: 181\")");
    assert_eq!(ve("!x = ${a=$e 182};"),
        "EXEC ERR: Caught [1,13:<compiler:s_eval>(Err)] SA::Panic(\"Error value in map value: 182\")");
    assert_eq!(ve("!x = $[]; x.0 = $e 183;"),
        "EXEC ERR: Caught [1,20:<compiler:s_eval>(Err)] SA::Panic(\"Error value in map value: 183\")");
    assert_eq!(ve("!x = ${}; x.a = $e 184;"),
        "EXEC ERR: Caught [1,20:<compiler:s_eval>(Err)] SA::Panic(\"Error value in map value: 184\")");
    assert_eq!(ve("!x = $[]; x.($e 185) = 5;"),
        "EXEC ERR: Caught [1,17:<compiler:s_eval>(Err)] SA::Panic(\"Error value in map key: 185\")");
    assert_eq!(ve("!x = ${}; x.($e 186) = 4;"),
        "EXEC ERR: Caught [1,17:<compiler:s_eval>(Err)] SA::Panic(\"Error value in map key: 186\")");
}

#[test]
fn check_prelude() {
    assert_eq!(ve("bool $n"),           "$false");
    assert_eq!(ve("int $n"),            "0");
    assert_eq!(ve("float $q$10.2$"),    "10.2");
    assert_eq!(ve("str 10.3"),          "\"10.3\"");
    assert_eq!(ve("sym \"foo\""),       ":foo");
    assert_eq!(ve("sym 10.4"),          ":\"10.4\"");
    assert_eq!(ve("(bool $e :fail) { 10 } { 20 }"), "20");
    assert_eq!(ve("std:fold 1 { _ + _1 } $[1,2,3,4]"), "11");
    assert_eq!(ve("std:take 2 $[1,2,3,4,5,6]"), "$[1,2]");
    assert_eq!(ve("std:drop 2 $[1,2,3,4,5,6]"), "$[3,4,5,6]");
}

#[test]
fn check_oop() {
    assert_eq!(ve(r#"
        !oo = 0;
        !new = {
            !self = ${};
            self.x = { self.y[] };
            self.y = { 10 };
            self.k = std:to_drop 20 {|| .oo = 20; }; 
            $:self
        };

        !c = new[];
        std:displayln "XXXXXXXXXXXXXXXXXXXXXXX";
        c.x[];
        std:displayln "XXXXXXXXXXXXXXXXXXXXXXX";
        !k = $*oo + 1;
        .*c = $n;
        .k = k + $*oo + 1;

        k
    "#), "22");
    assert_eq!(ve(r#"
        !new = {
            !obj = $&${};
            obj.add = { obj.b + 10 };
            obj.get = { type obj };
            obj.set = { obj.b = _; };
            $:obj
        };
        !v = $[];
        !o = new[];
        !ext_add = o.add;
        !ext_get = o.get;
        o.set 10;
        std:push v o.add[];
        std:push v o.get[];
        std:push v ext_add[];
        std:push v ext_get[];
        .o = $n;
        std:push v ext_add[];
        std:push v ext_get[];
        v
    "#),
    "$[20,\"map\",20,\"map\",10,\"none\"]");
    assert_eq!(ve(r#"
        !obj = ${};
        !x = $&&0;
        obj.a = { .x = 10 };
        obj.a[];
        $*x
    "#),
    "10");
}

#[test]
fn check_test_funs() {
    assert_eq!(ve("is_none $n"),        "$true");
    assert_eq!(ve("is_err $e $n"),      "$true");
    assert_eq!(ve("is_map ${}"),        "$true");
    assert_eq!(ve("is_vec $[]"),        "$true");
    assert_eq!(ve("is_sym :f"),         "$true");
    assert_eq!(ve("is_bool $n"),        "$false");
    assert_eq!(ve("is_bool $f"),        "$true");
    assert_eq!(ve("is_bytes $f"),       "$false");
    assert_eq!(ve("is_bytes \"f\""),    "$false");
    assert_eq!(ve("is_bytes $b\"f\""),  "$true");
    assert_eq!(ve("is_bytes $Q'f'"),    "$true");
    assert_eq!(ve("is_str \"f\""),      "$true");
    assert_eq!(ve("is_int 1"),          "$true");
    assert_eq!(ve("is_float 1.2"),      "$true");
    assert_eq!(ve("is_fun {}"),         "$true");
}

#[test]
fn check_len_fun() {
    assert_eq!(ve("len $[]"),            "0");
    assert_eq!(ve("len $[1,2,3]"),       "3");
    assert_eq!(ve("len ${a=1,b=20}"),    "2");
    assert_eq!(ve("len ${}"),            "0");
    assert_eq!(ve("std:str:len ${}"),        "3");
    assert_eq!(ve("len $q abcdef "),     "6");
    assert_eq!(ve("len $q abcüdef "),    "8");
    assert_eq!(ve("std:str:len $q abcüdef "),"7");
    assert_eq!(ve("len $Q abcdef "),     "6");
    assert_eq!(ve("len $Q abcüdef "),    "8");
    assert_eq!(ve("std:str:len $Q abcüdef "),"8");
}

#[test]
fn check_lst_map() {
    assert_eq!(ve("$[12,1,30] \\_ * 2"),                         "60");
    assert_eq!(ve("$@v $[12,1,30] \\$+ _ * 2"),                  "$[24,2,60]");
    assert_eq!(ve("$@v $[12,1,304] \\std:str:len _ | $+"),       "$[2,1,3]");
    assert_eq!(ve("$@v $[123,22,4304] \\std:str:len _ | $+"),    "$[3,2,4]");
    assert_eq!(ve("($@v $[123,22,4304] \\std:str:len _ | $+) | std:fold 1 \\_ * _1"), "24");
}

#[test]
fn check_prelude_assert() {
    assert_eq!(ve("std:assert ~ (type \"2019\".(int)) == $q integer "), "$true");
}

#[test]
fn check_prelude_str() {
    assert_eq!(ve("std:str:to_uppercase $q foo "), "\"FOO\"");
    assert_eq!(ve("std:str:to_lowercase $q FOO "), "\"foo\"");
    assert_eq!(ve("std:str:join \",\" $[1,2,3,${a=:x}]"), "\"1,2,3,${a=:x}\"");
    assert_eq!(ve("std:str:cat $[1,2,3,${a=:x}]"), "\"123${a=:x}\"");
}

#[test]
fn check_prelude_chrono() {
    if cfg!(feature="chrono") {
        assert_eq!(ve("std:chrono:timestamp $q$%Y$ | int"), "2020");
    }
}

#[test]
fn check_prelude_regex() {
    if cfg!(feature="regex") {
        assert_eq!(ve("($@v $q$fofoaaaaofefoeaafefeoaaaa$ | std:re:map $q{(a+)} { $+ _.1 }) | std:str:join $q$,$"),
                   "\"aaaa,aa,aaaa\"");
        assert_eq!(ve("
            ($@v $q$fofoaaaofefoeaaaaafefeoaaaaaaa$
                 | std:re:map $q{(a+)} { $+ ~ std:str:len _.1 })
            | std:fold 1 \\_ * _1"),
            "105");

        assert_eq!(ve("
            !x = $&$n;
            std:re:match $q/(a)\\s+(b)/ $q$a     b$ {
                .x = @;
            };
            $*x"),
            "$[$[\"a     b\",\"a\",\"b\"]]");

        assert_eq!(ve("
            std:re:replace_all $q/ar/ { \"mak\" } $q/foobarbarfoobararar/
        "),
        "EXEC ERR: Caught [2,39:<compiler:s_eval>(Func)]=>\
        [2,32:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 1\")");
        assert_eq!(ve("
            std:re:replace_all $q/a+r/ { std:str:cat \"mak\" ~ std:str:len _.0 } $q/foobarbaaaarfoobaararar/
        "),
        "\"foobmak2bmak5foobmak3mak2mak2\"");
        assert_eq!(ve("
            std:re:replace_all $q/a+r/
                {
                    (std:str:len[_.0] == 3) {
                        break \"XX\"
                    };
                    (std:str:cat \"<\" _.0 \">\")
                }
                $q/foobarbaaaarfoobaararar/
        "),
        "\"foob<ar>b<aaaar>foobXXarar\"");
        assert_eq!(ve("
            std:re:replace_all $q/a+r/
                {
                    (std:str:len[_.0] == 3) { next[] };
                    (std:str:cat \"<\" _.0 \">\")
                }
                $q/foobarbaaaarfoobaararar/
        "),
        "\"foob<ar>b<aaaar>foobaar<ar><ar>\"");
    }
}

#[test]
#[cfg(feature="serde_json")]
fn check_json() {
    assert_eq!(ve("std:ser:json $[1,1.2,$f,$t,$n,${a=1}]"), "\"[\\n  1,\\n  1.2,\\n  false,\\n  true,\\n  null,\\n  {\\n    \\\"a\\\": 1\\n  }\\n]\"");
    assert_eq!(ve("std:ser:json $[1,1.2,$f,$t,$n,${a=1}] $t"), "\"[1,1.2,false,true,null,{\\\"a\\\":1}]\"");
    assert_eq!(ve("std:deser:json $q$[1,2.3,true,null,{\"a\":10}]$"), "$[1,2.3,$true,$n,${a=10}]");
}

#[test]
#[cfg(feature="rmp-serde")]
fn check_msgpack() {
    assert_eq!(ve("std:deser:msgpack ~ std:ser:msgpack $[1,1.2,$f,$t,$n,${a=1},\"abcä\",$b\"abcä\"]"),
               "$[1,1.2,$false,$true,$n,${a=1},\"abcä\",$b\"abc\\xC3\\xA4\"]");
    assert_eq!(ve("std:ser:msgpack $b\"abc\""), "$b\"\\xC4\\x03abc\"");
    assert_eq!(ve("std:ser:msgpack $[1,$n,16.22]"), "$b\"\\x93\\x01\\xC0\\xCB@08Q\\xEB\\x85\\x1E\\xB8\"");
    assert_eq!(ve("std:deser:msgpack $b\"\\xC4\\x03abc\""), "$b\"abc\"");
    assert_eq!(ve("std:deser:msgpack $b\"\\x93\\x01\\xC0\\xCB@08Q\\xEB\\x85\\x1E\\xB8\""), "$[1,$n,16.22]");
}

#[test]
fn check_eval() {
    let mut ctx = EvalContext::new_default();

    assert_eq!(ctx.eval("std:eval $q$1 + 2$").unwrap().s(), "3");

    ctx.set_global_var("XXX", &VVal::Int(1337));
    assert_eq!(ctx.eval("std:eval $q$XXX + 2$").unwrap().s(), "1339");

    assert_eq!(ctx.eval("std:eval $q/std:eval $q$XXX + 2$/").unwrap().s(), "1339");
}

#[test]
fn check_eval_ctx() {
    let mut ctx = wlambda::EvalContext::new_default();
    ctx.eval("!toplevel_var = 20").unwrap();
    let ret = ctx.eval("toplevel_var").unwrap().i();
    assert_eq!(ret, 20);

    let mut ctx = wlambda::EvalContext::new_default();
    ctx.eval("!toplevel_var = { _ + 20 }").unwrap();
    let ret = ctx.eval("toplevel_var 11").unwrap().i();
    assert_eq!(ret, 31);
}

#[test]
fn check_userdata() {
    use std::rc::Rc;
    use std::cell::RefCell;
    let global_env = GlobalEnv::new_default();

    #[derive(Clone, Debug)]
    struct MyType {
        x: Rc<RefCell<(i64, i64)>>,
    }

    impl crate::vval::VValUserData for MyType {
        fn s(&self) -> String { format!("$<MyType({:?})>", self.x.borrow()) }
        fn i(&self) -> i64    { self.x.borrow_mut().1 }
        fn as_any(&mut self) -> &mut dyn std::any::Any { self }
        fn get_key(&self, key: &str) -> Option<VVal> {
            Some(VVal::new_str(key))
        }
        fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
            let args = env.argv_ref();
            Ok(VVal::pair(VVal::new_str(key), args[0].clone()))
        }
        fn call(&self, env: &mut Env) -> Result<VVal, StackAction> {
            let args = env.argv_ref();
            Ok(args[0].clone())
        }
        fn clone_ud(&self) -> Box<dyn crate::vval::VValUserData> {
            Box::new(self.clone())
        }
    }

    global_env.borrow_mut().add_func(
        "new_mytype",
        |_env: &mut Env, _argc: usize| {
            Ok(VVal::Usr(Box::new(MyType { x: Rc::new(RefCell::new((13, 42))) })))
        }, Some(0), Some(0));

    global_env.borrow_mut().add_func(
        "modify_mytype",
        |env: &mut Env, _argc: usize| {
            Ok(if let VVal::Usr(mut u) = env.arg(0) {
                if let Some(ud) = u.as_any().downcast_mut::<MyType>() {
                    ud.x.borrow_mut().0 += 1;
                    ud.x.borrow_mut().1 *= 2;
                    VVal::Int(ud.x.borrow().0 + ud.x.borrow().1)
                } else {
                    VVal::None
                }
            } else { VVal::None })
        }, Some(1), Some(1));

    let mut ctx = crate::compiler::EvalContext::new(global_env);

    let r = &mut ctx.eval(r#"
        !x = new_mytype[];
        !i = modify_mytype x;
        $[i, x, x.foo, x :foo2, x.foo[99], x[:kkk]]
    "#).unwrap();

    assert_eq!(
        r.s(),
        "$[98,$<MyType((14, 84))>,\"foo\",:foo2,$p(\"foo\",99),:kkk]",
        "Userdata implementation works");
}

#[test]
fn check_bytes_impl() {
    #[cfg(feature="serde_json")]
    assert_eq!(ve("std:ser:json $b\"abc\""),                         "\"[\\n  97,\\n  98,\\n  99\\n]\"", "JSON serializer for bytes ok");

    assert_eq!(ve("str $b\"abc\""),                              "\"abc\"", "Bytes to String by 1:1 Byte to Unicode Char mapping");
    assert_eq!(ve("str $b\"äbcß\""),                             "\"Ã¤bcÃ\\u{9f}\"", "Bytes to String by 1:1 Byte to Unicode Char mapping");
    assert_eq!(ve("std:str:from_utf8 $b\"äbcß\""),                   "\"äbcß\"", "Bytes to String from UTF8");
    assert_eq!(ve("std:str:from_utf8 $b\"\\xC4\\xC3\""),             "$e \"str:from_utf8 decoding error: invalid utf-8 sequence of 1 bytes from index 0\"", "Bytes to String from invalid UTF8");
    assert_eq!(ve("std:str:from_utf8_lossy $b\"\\xC4\\xC3\""),       "\"��\"", "Bytes to String from invalid UTF8 lossy");
    assert_eq!(ve("std:str:to_bytes \"aäß\""),                       "$b\"a\\xC3\\xA4\\xC3\\x9F\"", "Bytes from String as UTF8");
    assert_eq!(ve("std:str:from_utf8 ~ std:str:to_bytes \"aäß\""),       "\"aäß\"", "Bytes from String as UTF8 into String again");
    assert_eq!(ve("$b\"abc\" 1"),                                "$b\"b\"", "Get single byte from bytes");
    assert_eq!(ve("$b\"abcdef\" 0 2"),                           "$b\"ab\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" 3 3"),                           "$b\"def\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" $[3, 3]"),                       "$b\"def\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" $[3]"),                          "$b\"def\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" ${abcdef = 10}"),                "10", "Bytes as map key");
    assert_eq!(ve("std:bytes:to_vec $b\"abcdef\""),                  "$[97,98,99,100,101,102]", "bytes:to_vec");
    assert_eq!(ve("std:bytes:from_vec ~ std:bytes:to_vec $b\"abcdef\""), "$b\"abcdef\"", "bytes:from_vec");
    assert_eq!(ve("std:bytes:from_vec $[]"),                         "$b\"\"", "bytes:from_vec");
    assert_eq!(ve("std:bytes:from_vec $[1,2,3]"),                    "$b\"\\x01\\x02\\x03\"", "bytes:from_vec");

    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\""),                  "\"616263FF\"");
    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\" 6"),                "\"616263 FF\"");
    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\" 6 \":\""),          "\"616263:FF\"");
    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\" 1 \":\""),          "\"6:1:6:2:6:3:F:F\"");

    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\""),         "$b\"abc\\xFF\"");
    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 6"),       "$b\"abc\\xFF\"");
    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 6 \":\""), "$b\"abc\\xFF\"");
    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 1 \":\""), "$b\"abc\\xFF\"");
    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"\\x00abc\\xFF\" 1 \":\""), "$b\"\\0abc\\xFF\"");

    assert_eq!(ve("std:str:to_char_vec $q ABC "), "$[65,66,67]");
    assert_eq!(ve("$q ABC | std:str:to_char_vec | std:str:from_char_vec"), "\"ABC\"");
}

#[test]
fn check_ref() {
    assert_eq!(ve("!:global x = 3; x"),                  "3");

    assert_eq!(ve("!x = $&&1; $*x"),                     "1");
    assert_eq!(ve("!x = $&&1; .*x = 2; $*x"),            "2");
    assert_eq!(ve("!:global x = $&&1; x"),               "$&&1");
    assert_eq!(ve("!:global x = $&&1; $*x"),             "1");
    assert_eq!(ve("!:global x = $&&1; .*x = 2; $*x"),    "2");
    assert_eq!(ve("!(x, y) = $[$&&1, $&&2]; $[$*x, $*y]"), "$[1,2]");
    assert_eq!(ve("!(x, y) = $[$&&1, $&&2]; $[x, y]"), "$[$&&1,$&&2]");
    assert_eq!(ve("!(x, y) = $[$&&1, $&&2]; .*(x, y) = $[33, 34]; $[x, y]"), "$[$&&33,$&&34]");
    assert_eq!(ve(r#"
            !:global (x, y) = $[$&&1, $&&2];
            .*(x, y) = $[33, 34];
            $[x, y]
        "#), "$[$&&33,$&&34]");
    assert_eq!(ve(r#"
            !(x, y) = $[$&&1, $&&2];
            !z = std:weaken y;
            !f = { .x = x + 1; .z = z + 2; $[$:x, $:z] };
            !r = $[];
            std:push r ~ str f[];
            .x = $n;
            .y = $n;
            std:push r ~ str f[];
            $[r, z]
        "#), "$[$[\"$[$&&2,$&&4]\",\"$[$&&3,$&&$n]\"],$n]");
    assert_eq!(ve(r#"
        !self = $&&${};
        !wself = std:weaken self;
        self.x = { wself.g = 10; wself.g };
        self.y = { wself.g * 10 };
        !r = $[];
        std:push r self.x[];
        std:push r self.y[];
        !f = self.y;
        .self = $n;
        std:push r f[];
        std:push r wself;
        r
    "#), "$[10,100,0,$n]");
}

#[test]
fn check_set_key() {
    assert_eq!(ve(r#"
        !self = ${};
        self.x = 22;
        self.x;
    "#), "22");
    assert_eq!(ve(r#"
        !self = $&&${};
        self.x = { 23 };
        self.x[];
    "#), "23");
}

#[test]
fn check_append_prepend() {
    assert_eq!(ve("std:append 1 2"),                    "$[1,2]");
    assert_eq!(ve("std:append $[1, 2] 3"),              "$[1,2,3]");
    assert_eq!(ve("std:append $[1, 2] 3 4 $[5]"),       "$[1,2,3,4,5]");
    assert_eq!(ve("std:append 1 2 3 4 $[5, 6, 7, 8]"),  "$[1,2,3,4,5,6,7,8]");
    assert_eq!(ve("std:append 1"),                      "$[1]");

    assert_eq!(ve("std:prepend 1 2"),                   "$[2,1]");
    assert_eq!(ve("std:prepend $[1, 2] 3"),             "$[3,1,2]");
    assert_eq!(ve("std:prepend $[1, 2] 3 4 $[5]"),      "$[5,4,3,1,2]");
    assert_eq!(ve("std:prepend 1 2 3 4 $[5, 6, 7, 8]"), "$[8,7,6,5,4,3,2,1]");
    assert_eq!(ve("std:prepend 1"),                     "$[1]");
}

#[test]
fn check_apply() {
    assert_eq!(ve("std:str:cat[[$[1,2,3]]]"), "\"123\"");
    assert_eq!(ve("std:assert_eq std:str:cat[[$[1,2,3]]] \"123\""), "$true");
    assert_eq!(ve("!a = $[4,5,6]; std:str:cat[[a]]"), "\"456\"");
}

#[test]
fn check_call_order() {
    assert_eq!(ve(r#"
        !v = $[];
        std:push v ~ ({
            std:push v 1;
            { std:push v $[3, _]; 4 }
        }[])[[$[{ std:push v 2; 3.5 }[]]]];
        v
    "#),
    "$[1,2,$[3,3.5],4]");
    assert_eq!(ve(r#"
        !v = $[];
        !get_func = {
            std:push v 1;
            {
                std:push v _;
                std:push v _1;
                std:push v _2;
                std:push v 5;
                6
            }
        };
        std:push v ~
            get_func[]
                {std:push v 2; 2.5}[]
                {std:push v 3; 3.5}[]
                {std:push v 4; 4.5}[];
        v
    "#),
    "$[1,2,3,4,2.5,3.5,4.5,5,6]");
}

#[test]
fn check_cyclic_str_write() {
    assert_eq!(ve(r#"!x = $&&0; .*x = x; x"#), "$<1=>$&&$<1>");
    assert_eq!(ve(r#"!x = $&1;  .x = $:x; x"#), "$<1=>$&&$<1>");
    assert_eq!(ve(r#"!x = $&0;  .*x = $:x; !y = std:weaken x; y"#), "$<1=>$(&)$<1>");
    assert_eq!(ve(r#"
        !x = $[1,2];
        !y = ${};
        y.x = x;
        std:push x y;
        x
    "#),
    "$<1=>$[1,2,${x=$<1>}]");
    assert_eq!(ve(r#"
        !x = $[1,2];
        !y = ${};
        !f = $[];
        std:push f f;
        std:push f x;
        y.x = x;
        std:push x y;
        std:push x x;
        std:push x f;
        x
    "#),
    "$<1=>$[1,2,${x=$<1>},$<1>,$<2=>$[$<2>,$<1>]]");

    assert_eq!(ve(r#"!x = $[]; std:push x $&&x; $[x.0, x]"#), "$[$<1=>$&&$<2=>$[$<1>],$<2>]");
    assert_eq!(ve(r#"
        !x = ${};
        x.f = { x.b };
        $[x.f, $:x]
    "#),
    "$[$<1=>&F{@[3,15:<compiler:s_eval>(Func)@f],amin=0,amax=0,locals=0,upvalues=$[$<2=>$(&)${f=$<1>}]},$<2>]");

    assert_eq!(ve(r#"
        !x = $[];
        std:push x ~ $&$e x;
        x
    "#),
    "$<1=>$[$&$e[3,27:<compiler:s_eval>(Err)] $<1>]");
}

#[test]
fn check_byte_str_index() {
    assert_eq!(ve("$q$abc$ 0"), "\"a\"");
    assert_eq!(ve("$q$abc$ 2"), "\"c\"");
    assert_eq!(ve("0 $q$abc$"), "\"a\"");
    assert_eq!(ve("2 $q$abc$"), "\"c\"");
    assert_eq!(ve("$q$abc$.0"), "\"a\"");
    assert_eq!(ve("$q$abc$.2"), "\"c\"");
    assert_eq!(ve("$Q$abc$ 0"), "$b\"a\"");
    assert_eq!(ve("$Q$abc$ 2"), "$b\"c\"");
    assert_eq!(ve("0 $Q$abc$"), "$b\"a\"");
    assert_eq!(ve("2 $Q$abc$"), "$b\"c\"");
    assert_eq!(ve("$Q$abc$.0"), "$b\"a\"");
    assert_eq!(ve("$Q$abc$.2"), "$b\"c\"");
}

#[test]
fn check_num_funs() {
    assert_eq!(ve("std:num:ceil  1.0"), "1");
    assert_eq!(ve("std:num:ceil  1.1"), "2");
    assert_eq!(ve("std:num:ceil  1.5"), "2");
    assert_eq!(ve("std:num:ceil  1.9"), "2");
    assert_eq!(ve("std:num:floor  1.0"), "1");
    assert_eq!(ve("std:num:floor  1.1"), "1");
    assert_eq!(ve("std:num:floor  1.5"), "1");
    assert_eq!(ve("std:num:floor  1.9"), "1");
    assert_eq!(ve("std:num:round  1.0"), "1");
    assert_eq!(ve("std:num:round  1.1"), "1");
    assert_eq!(ve("std:num:round  1.5"), "2");
    assert_eq!(ve("std:num:round  1.9"), "2");
    assert_eq!(ve(
        "std:num:sqrt   2       | (\\_ * 10000000) | std:num:round"),
        "14142136");
    assert_eq!(ve(
        "std:num:cbrt   2       | (\\_ * 10000000) | std:num:round"),
        "12599210");
    assert_eq!(ve(
        "std:num:to_degrees   2 | (\\_ * 10000000) | std:num:round"),
        "1145915590");
    assert_eq!(ve(
        "std:num:to_radians   2 | (\\_ * 10000000) | std:num:round"),
        "349066");
    assert_eq!(ve(
        "std:num:tan   2        | (\\_ * 10000000) | std:num:round"),
        "-21850399");
    assert_eq!(ve(
        "std:num:tanh  2        | (\\_ * 10000000) | std:num:round"),
        "9640276");
    assert_eq!(ve(
        "std:num:sin   2        | (\\_ * 10000000) | std:num:round"),
        "9092974");
    assert_eq!(ve(
        "std:num:sinh  2        | (\\_ * 10000000) | std:num:round"),
        "36268604");
    assert_eq!(ve(
        "std:num:cos   2        | (\\_ * 10000000) | std:num:round"),
        "-4161468");
    assert_eq!(ve(
        "std:num:cosh  2        | (\\_ * 10000000) | std:num:round"),
        "37621957");
    assert_eq!(ve(
        "std:num:atan   2       | (\\_ * 10000000) | std:num:round"),
        "11071487");
    assert_eq!(ve(
        "std:num:atanh  0.5     | (\\_ * 10000000) | std:num:round"),
        "5493061");
    assert_eq!(ve(
        "std:num:asin   0.5     | (\\_ * 10000000) | std:num:round"),
        "5235988");
    assert_eq!(ve(
        "std:num:asinh  0.5     | (\\_ * 10000000) | std:num:round"),
        "4812118");
    assert_eq!(ve(
        "std:num:acos   0.5     | (\\_ * 10000000) | std:num:round"),
        "10471976");
    assert_eq!(ve(
        "std:num:acosh  2.1     | (\\_ * 10000000) | std:num:round"),
        "13728591");
    assert_eq!(ve(
        "std:num:ln     200     | (\\_ * 10000000) | std:num:round"),
        "52983174");
    assert_eq!(ve(
        "std:num:log2   200     | (\\_ * 10000000) | std:num:round"),
        "76438562");
    assert_eq!(ve(
        "std:num:log10  200     | (\\_ * 10000000) | std:num:round"),
        "23010300");
    assert_eq!(ve(
        "std:num:exp_m1   2     | (\\_ * 10000000) | std:num:round"),
        "63890561");
    assert_eq!(ve(
        "std:num:exp      2     | (\\_ * 10000000) | std:num:round"),
        "73890561");
    assert_eq!(ve(
        "std:num:exp2   10"),
        "1024");
    assert_eq!(ve(
        "std:num:log   100 3    | (\\_ * 10000000) | std:num:round"),
        "41918065");
    assert_eq!(ve("std:num:pow   100 3"), "1000000");
    assert_eq!(ve(
        "std:num:pow   100 3.1  | (\\_ * 10000000) | std:num:round"),
        "15848931924611");
    assert_eq!(ve(
        "std:num:abs   -1.2"),
        "1.2");
    assert_eq!(ve("(std:num:abs  -1) * 2"), "2");
}

#[test]
fn check_hash_fnv1() {
    assert_eq!(ve("std:hash:fnv1a 123123123"), "3905796366342510356");
    assert_eq!(ve("std:hash:fnv1a 231.2"), "-1882808912766899311");
    assert_eq!(ve("std:hash:fnv1a 231.1"), "4682567979461110603");
    assert_eq!(ve("std:hash:fnv1a :123123123"), "-7308283643128320213");
    assert_eq!(ve("(std:hash:fnv1a \"a\") - 0xaf63dc4c8601ec8c"), "0");
    assert_eq!(ve("(std:hash:fnv1a \"foo\") - 0xdcb27518fed9d577"), "0");
    assert_eq!(ve("(std:hash:fnv1a \"fo\" :o) - 0xdcb27518fed9d577"), "0");
    assert_eq!(ve("(std:hash:fnv1a \"f\" :o :o) - 0xdcb27518fed9d577"), "0");
    assert_eq!(ve("(std:hash:fnv1a \"\")  - 0xcbf29ce484222325"), "0");
    assert_eq!(ve("(std:hash:fnv1a \"http://www.isthe.com/chongo/tech/math/prime/mersenne.html#largest\")  - 0x8e87d7e7472b3883"), "0");
}

#[test]
fn string_map_with_function() {
    assert_eq!(ve("$@v $q$abcdef$ $+"), "$[\"a\",\"b\",\"c\",\"d\",\"e\",\"f\"]");
    assert_eq!(ve("$@v $b\"abcdef\" $+"), "$[$b\"a\",$b\"b\",$b\"c\",$b\"d\",$b\"e\",$b\"f\"]");
}

#[test]
fn map_over_map() {
    assert_eq!(ve(
        "!sum = $&0; ${a=10, b=20, c=30} {|2| .sum = sum + _; }; $*sum"),
        "60");
}

#[test]
fn check_int_float_conversion() {
    assert_eq!(ve("std:num:int_to_open01 0"),         "0.00000000000000011102230246251565");
    assert_eq!(ve("std:num:int_to_open01 -1"),        "0.9999999999999999");
    assert_eq!(ve("std:num:int_to_open_closed01 0"),  "0.00000000000000011102230246251565");
    assert_eq!(ve("std:num:int_to_open_closed01 -1"), "1");
    assert_eq!(ve("std:num:int_to_closed_open01 0"),  "0");
    assert_eq!(ve("std:num:int_to_closed_open01 -1"), "0.9999999999999999");
}

#[test]
fn check_splitmix64() {
    assert_eq!(ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next s
    "), "4473449133009263371");
    assert_eq!(ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next s 4
    "), "$[4473449133009263371,-9009341174627168353,7739434774028954414,-453282142843114385]");
    assert_eq!(ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_open01 s 4
    "), "$[0.24250616342560194,0.5116026362903058,0.41955559979060253,0.9754275257990305]");
    assert_eq!(ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_open_closed01 s 4
    "), "$[0.24250616342560205,0.5116026362903059,0.41955559979060264,0.9754275257990306]");
    assert_eq!(ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_closed_open01 s 4
    "), "$[0.24250616342560194,0.5116026362903058,0.41955559979060253,0.9754275257990305]");
    assert_eq!(ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_open01 s
    "), "0.24250616342560194");
}

#[test]
fn check_user_obj_macro() {
    use crate::set_vval_method;

    let o = VVal::map();
    let oo = VVal::vec();
    oo.push(VVal::Int(10));
    set_vval_method!(o, oo, get_it,   None, None, _env, _argc, { Ok(oo.at(0).unwrap_or(VVal::Int(99))) });
    set_vval_method!(o, oo, get_it2x, None, None, _env, _argc, { Ok(VVal::Int(oo.at(0).unwrap_or(VVal::Int(99)).i() * 2)) });

    let mut ctx = EvalContext::new_default();
    ctx.set_global_var("O", &o);
    assert_eq!(ctx.eval("O.get_it[]").unwrap().s(), "10");
    oo.set_at(0, VVal::Int(11));
    assert_eq!(ctx.eval("O.get_it2x[]").unwrap().s(), "22");
}

#[test]
fn check_for() {
    assert_eq!(
        ve("!o = $&$q 1 ; for :XYZ \\.o = _ o; $*o"),
        "\"ZYX1\"");
    assert_eq!(
        ve("!r = $&:XYZ; !o = $&$q 1 ; for (std:weaken r) \\.o = _ o; $*o"),
        "\"ZYX1\"");
    assert_eq!(
        ve("!o = $&$q 1 ; for $&:XYZ \\.o = _ o; $*o"),
        "\"ZYX1\"");
    assert_eq!(
        ve("!o = $&$q 1 ; for $&&:XYZ \\.o = _ o; $*o"),
        "\"ZYX1\"");
    assert_eq!(
        ve("!o = $&$q 1 ; for (std:to_drop :XYZ {||}) \\.o = _ o; $*o"),
        "\"ZYX1\"");
    assert_eq!(
        ve("!o = $&$q 1 ; for \"XYZ\" \\.o = _ o; $*o"),
        "\"ZYX1\"");
    assert_eq!(
        ve("!o = $&$b\"L\"; for $b\"@XZ\" \\.o = _ o; $*o"),
        "$b\"ZX@L\"");
    assert_eq!(
        ve("!o = $&0; for $[1,2,3] \\.o = o + _; $*o"), "6");
    assert_eq!(
        ve(r"
            !x = $&0;
            !o = $&0;
            for ${a=3, b=2, c=4} {
                .x = x + (@ | 1 | std:bytes:to_vec | 0);
                .o = _ + o;
            };
            $[x, o]
        "), "$[294,9]");
}

#[test]
fn check_splices() {
    assert_eq!(ve("${ a = 10, *${ b = 2 }}.b"), "2");
    assert_eq!(ve("${*10}"), "${=10}");
    assert_eq!(ve("!it = $iter ${a=33}; ${*it}"), "${a=33}");
    assert_eq!(ve("!a = ${a=10}; !b = ${b = 3}; ${*a, *b}.b"), "3");
    assert_eq!(ve("$[1,2,*$[3,4]].2"), "3");
    assert_eq!(ve("!a = $[1,2]; !b = $[3,4]; $[*a, *b].2"), "3");
}

#[test]
fn check_shuffle() {
    assert_eq!(ve(r"
        !sm = std:rand:split_mix64_new_from 1234;
        std:shuffle { std:rand:split_mix64_next sm }
            $[1,2,3,4,5,6,7,8];
    "), "$[2,1,7,4,8,5,3,6]");
}

#[test]
fn check_sort() {
    assert_eq!(ve("std:sort std:cmp:str:asc  $[:c, :x, :a, :b]"),    "$[:a,:b,:c,:x]");
    assert_eq!(ve("std:sort std:cmp:str:desc $[:c, :x, :a, :b]"),    "$[:x,:c,:b,:a]");
    assert_eq!(ve("std:sort std:cmp:str:asc  $[3, 2, 5, 9, 0, -1]"), "$[-1,0,2,3,5,9]");
    assert_eq!(ve("std:sort std:cmp:str:desc $[3, 2, 5, 9, 0, -1]"), "$[9,5,3,2,0,-1]");
    assert_eq!(ve("std:sort std:cmp:num:asc  $[3, 2, 5, 9, 0, -1]"), "$[-1,0,2,3,5,9]");
    assert_eq!(ve("std:sort std:cmp:num:desc $[3, 2, 5, 9, 0, -1]"), "$[9,5,3,2,0,-1]");
}

#[test]
fn check_copy() {
    assert_eq!(ve("!a = $[1,2,3]; a.0 = 10; !b = std:copy a; b.1 = 20; $[a,b]"),
               "$[$[10,2,3],$[10,20,3]]");
    assert_eq!(ve("!a = ${a=1}; a.a = 10; !b = std:copy a; b.a = 20; $[a,b]"),
               "$[${a=10},${a=20}]");
}

#[test]
fn check_borrow_error() {
    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { x.1 = _; }
    "),
    "EXEC ERR: Caught [3,11:<compiler:s_eval>(Func)@x]=>\
    [3,11:<compiler:s_eval>(Call)] SA::Panic(\"Can\\\'t mutate borrowed value: $[1,2,3]\")");

    assert_eq!(ve(r"
        !x = ${a=1};
        x { x.a = $[_, _1]; }
    "),
    "EXEC ERR: Caught [3,11:<compiler:s_eval>(Func)@a]=>\
    [3,11:<compiler:s_eval>(Call)] SA::Panic(\"Can\\\'t mutate borrowed value: ${a=1}\")");

    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { std:prepend x $[_] }
    "),
    "EXEC ERR: Caught [3,25:<compiler:s_eval>(Call)]=>\
    [3,11:<compiler:s_eval>(Func)@x]=>\
    [3,11:<compiler:s_eval>(Call)] SA::Panic(\"Can\\\'t mutate borrowed value: $[1,2,3]\")");

    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { std:append x $[_] }
    "),
    "EXEC ERR: Caught [3,24:<compiler:s_eval>(Call)]=>\
    [3,11:<compiler:s_eval>(Func)@x]=>\
    [3,11:<compiler:s_eval>(Call)] SA::Panic(\"Can\\\'t mutate borrowed value: $[1,2,3]\")");

    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { std:take 2 x; _ }
    "),
    "EXEC ERR: Caught [3,22:<compiler:s_eval>(Call)]=>\
    [3,11:<compiler:s_eval>(Func)@x]=>\
    [3,11:<compiler:s_eval>(Call)] SA::Panic(\"Can\\\'t mutate borrowed value: $[1,2,3]\")");
}

#[test]
fn check_test_import() {
    assert_eq!(ve(r"
        !@import x tests:test_mod_r1;
        x:f[10]
    "), "40");
    assert_eq!(ve(r"
        !@import x tests:test_paths_mod;
        x:xxx[]
    "), "123");
}

#[test]
fn check_field_access() {
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1"),                                 "$[1,2,$[1,2,3]]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1.2"),                               "$[1,2,3]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1.2.3"),                             "$n");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1.2.2"),                             "3");
    assert_eq!(ve("2 ~ 2 ~ 1 $[1,$[1,2,$[1,2,3]]]"),                         "3");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].(0 + 1)"),                           "$[1,2,$[1,2,3]]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].(0 + 1).(1 + 1)"),                   "$[1,2,3]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].(0 + 1).(1 + 1).(1 + 1)"),           "3");
    assert_eq!(ve("${a=${b=${c=9}}}.a"),                                      "${b=${c=9}}");
    assert_eq!(ve("${a=${b=${c=9}}}.a.b"),                                    "${c=9}");
    assert_eq!(ve("${a=${b=${c=9}}}.a.b.c"),                                  "9");
    assert_eq!(ve("${a=${b=${c=9}}}.\"a\".\"b\".c"),                          "9");
    assert_eq!(ve("${a=${b=${c=9}}}.a.\"b\".c"),                              "9");
    assert_eq!(ve("${a=${b=${c=9}}}.\"a\".\"b\".\"c\""),                      "9");
    assert_eq!(ve("${a=${b=${c=9}}}.(\"a\" \"\").\"b\".\"c\""),               "9");
    assert_eq!(ve("${a=${b=${c=9}}}.(\"a\" \"\").(\"b\" \"\").(\"c\" \"\")"), "9");
}

#[test]
fn check_method_calls() {
    // Idiomatic class making:
    assert_eq!(ve(r"
        !class = ${
            new = {!(x) = @;
                ${
                    _proto = $self,
                    x = x,
                }
            },
            meth_a = { $self.x * _ },
        };
        !instance = class.new 20;
        instance.meth_a 22;
    "), "440");
    assert_eq!(ve("!v = $&${ _proto = ${ a = { 11 } } }; v.(\"\" \"a\")[]"), "11");
    assert_eq!(ve("!v = $&${ _proto = ${ a = { 10 } } }; v.a[]"), "10");

    // Simple vector call table access still works as usual:
    assert_eq!(ve("!v = $[{ _ }]; v.0 20"),                       "20");
    assert_eq!(ve("!v = $[1,2,$[10,{ _ }]]; v.2.1 20"),           "20");
    assert_eq!(ve("!v = $[$[1,2,$[10,{ _ }]]]; v.0.2.1 20"),      "20");
    assert_eq!(ve("!v = $[$[$[1,2,$[10,{ _ }]]]]; v.0.0.2.1 20"), "20");

    // Does it work on references?
    assert_eq!(ve(r"
        !class = ${ a = { 10 }};
        !v = $&${_proto = class};
        v.a[]
    "), "10");

    // Does it work with arrays?
    assert_eq!(ve(r"
        !class = ${ a = { 11 }};
        !v = $[class];
        v.a[]
    "), "11");

    // Does it work with arrays and $data
    assert_eq!(ve(r"
        !class = ${ a = { $s.b[] * $d.x }, b = { 10 } };
        !v = $[class, ${ x = 10 }];
        v.a[]
    "), "100");

    // Does it work with $data?
    assert_eq!(ve(r"
        !class = ${ a = { $s.b[] * $d.x }, b = { 10 } };
        !v = ${ _proto = class, _data = ${ x = 11 } };
        v.a[]
    "), "110");

    // Idiomatic class making:
    assert_eq!(ve(r"
        !class = ${
            new = {!(x) = @;
                ${
                    _proto = $self,
                    x = x,
                }
            },
            meth_a = { $self.x * _ },
        };
        !instance = class.new 20;
        instance.meth_a 22;
    "), "440");

    // Idiomatic class making and recursive $self access:
    assert_eq!(ve(r"
        !class = ${
            new = {!(x) = @;
                ${
                    _proto = $self,
                    x = x,
                }
            },
            meth_a = { $self.meth_b _ },
            meth_b = { $self.x * _ }
        };
        !instance = class.new 20;
        instance.meth_a 23;
    "), "460");

    assert_eq!(ve(r"
        !class = ${
            new = {!(x) = @;
                ${
                    _proto = $self,
                    x = x,
                }
            },
            # works because $self is still set when woop is called:
            meth_a = { !woop = $self._proto.meth_b; woop _ },
            meth_b = { $self.x * _ + 1 }
        };
        !instance = class.new 20;
        instance.meth_a 23;
    "), "461");

    assert_eq!(ve(r"
        !class_b = ${
            new = { ${ _proto = $self, l = 11 } },
            ggg = { $self.l * 100 },
        };
        !class = ${
            new = {!(x) = @;
                ${
                    _proto = $self,
                    x = x,
                }
            },
            # works because $self is still set when woop is called:
            meth_a = { $self.meth_b _ },
            meth_b = { $self.x * _.ggg[] + 2 }
        };
        !instance = class.new 20;
        !instance_b = class_b.new[];
        instance.meth_a instance_b;
    "), "22002");

    // Access by symbol/string
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ y = 99, a = { .x = $[$self.y, _] } };
        o.a 10;
        $*x
    "), "$[99,10]");
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } };
        o.b.a 10;
        $*x
    "), "$[99,10]");
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } };
        o.c.b.a 10;
        $*x
    "), "$[99,10]");

    // Access by string
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ d = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } } };
        o.d.c.b.($q$a$) 10;
        $*x
    "), "$[99,10]");

    // Access by Key/Call
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ y = 99, a = { .x = $[$self.y, _] } };
        o.($q$a$ $q$$) 11;
        $*x
    "), "$[99,11]");
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ y = 99, a = { .x = $[$self.y, _] } };
        !mkkey = { :a };
        o.(mkkey[]) 11;
        $*x
    "), "$[99,11]");
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } };
        o.b.($q$a$ $q$$) 11;
        $*x
    "), "$[99,11]");
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } };
        o.c.b.($q$a$ $q$$) 11;
        $*x
    "), "$[99,11]");
    assert_eq!(ve(r"
        !x = $&$n;
        !o = ${ d = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } } };
        o.d.c.b.($q$a$ $q$$) 11;
        $*x
    "), "$[99,11]");

    // Prototyped inheritance:
    assert_eq!(ve(r"
        !x = $&$n;
        !class = ${ a = { .x = $[$self.y, _] } };
        !o = ${ y = 99, _proto = class };
        o.a 10;
        $*x
    "), "$[99,10]");

    // Prototyped multi level inheritance:
    assert_eq!(ve(r"
        !x = $&$n;
        !super = ${ a = { .x = $[$self.y, _, $self.b[]] } };
        !class = ${ _proto = super, b = { 13 } };
        !o = ${ y = 99, _proto = class };
        o.a 14;
        $*x
    "), "$[99,14,13]");
}

#[test]
fn capture_ref_semantics() {
    assert_eq!(ve(r" !x = $& 10;   x "),            "10");
    assert_eq!(ve(r" !x = $& 10; $*x "),            "10");
    assert_eq!(ve(r" !x = $& 10; $:x "),            "$&&10");
    assert_eq!(ve(r" !x = 10;     !y = { x }[]; $[x,  y] "),         "$[10,10]");
    assert_eq!(ve(r" !x = 10;     !y = { x }[]; $[$*x, y] "),        "$[10,10]");
    assert_eq!(ve(r" !x = 10;     !y = { x }[]; $[$:x, y] "),        "$[$&&10,10]");
    assert_eq!(ve(r" !x = $& 10;  !y = { x }[]; $[x,  y] "),         "$[10,10]");
    assert_eq!(ve(r" !x = $& 10;  !y = { x }[]; $[$*x, y] "),        "$[10,10]");
    assert_eq!(ve(r" !x = $& 10;  !y = { x }[]; $[$:x, y] "),        "$[$&&10,10]");
    assert_eq!(ve(r" !x = $&& 10; !y = { x }[]; $[x, y] "),          "$[$&&10,10]");
    assert_eq!(ve(r" !x = $&& 10; !y = { x }[]; $[$*x, y] "),        "$[10,10]");
    assert_eq!(ve(r" !x = $&& 10; !y = { x }[]; $[$:x, y] "),        "$[$&&10,10]");

    assert_eq!(ve(r" !x = $& 10;  !y = std:weaken $:x;   y "),       "$n");
    assert_eq!(ve(r" !x = $& 10;  !y = std:weaken $:x; $*y "),       "10");
    assert_eq!(ve(r" !x = $& 10;  !y = std:weaken $:x; $:y "),       "$&&10");
    assert_eq!(ve(r" !x = $&& 10; !y = std:weaken $:x;   y "),       "$n");
    assert_eq!(ve(r" !x = $&& 10; !y = std:weaken $:x; $*y "),       "10");
    assert_eq!(ve(r" !x = $&& 10; !y = std:weaken $:x; $:y "),       "$&&10");

    assert_eq!(ve("!x = 10; !y = { $:x }[]; .x = 20; y"), "$&&20");
    assert_eq!(ve("!x = 10; !y = { $:x }[]; .x = 20; .y = 11; $[x, y]"),        "$[20,11]");
    assert_eq!(ve("!x = 10; !y = { $:x }[]; .x = 20; .*y = 11; $[x, $*y, y]"),  "$[11,11,$&&11]");

    assert_eq!(ve("!x =     10; !y = $:x; .x  = 11; $[x,y]"), "$[11,$&&10]");
    assert_eq!(ve("!x = $&  10; !y = $:x; .x  = 11; $[x,y]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; !y = $:x; .x  = 11; $[x,y]"), "$[11,$&&10]");
    assert_eq!(ve("!x =     10; !y = $:x; .*x = 11; $[x,y]"), "$[11,$&&10]");
    assert_eq!(ve("!x = $&  10; !y = $:x; .*x = 11; $[x,y]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; !y = $:x; .*x = 11; $[x,y]"), "$[$<1=>$&&11,$<1>]");

    assert_eq!(ve("!x =     10; { !y = $:x; .x  = 11; $[x,y] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&  10; { !y = $:x; .x  = 11; $[x,y] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; { !y = $:x; .x  = 11; $[x,y] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x =     10; { !y = $:x; .*x = 11; $[x,y] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&  10; { !y = $:x; .*x = 11; $[x,y] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; { !y = $:x; .*x = 11; $[x,y] }[]"), "$[11,$&&11]"); // yes, upvalue refs are implicit

    assert_eq!(ve("!x =     10; { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&  10; { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x =     10; { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&  10; { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"), "$[11,$&&11]"); // yes, upvalue refs are implicit
}

#[test]
fn closure_generator() {
    assert_eq!(ve(r"
        !mk_gen = {
            !x = $& 0; # weakly captured does not keep alive!
            { .x = x + 1; x }
        };

        !g1 = mk_gen[];
        !g2 = mk_gen[];
        $[g1[], g2[]]
    "), "$[$n,$n]");

    assert_eq!(ve(r"
        !mk_gen = {
            !x = $&& 0; # strongly captured keeps alive!
            { .x = x + 1; x }
        };

        !g1 = mk_gen[];
        !g2 = mk_gen[];
        g1[];
        g1[];
        g2[];
        g1[];
        g2[];
        $[g1[], g2[]]
    "), "$[4,3]");
}

#[test]
fn ref_semantics() {
    assert_eq!(ve(r"
        !dropped = $false;
        !self = ${};
        self.drop = std:to_drop $n {|| .dropped = $true; };
        self.foo = { self.x = self.x + 1; self.x };
        self.x = 10;
        !ret = self.foo[];
        !ret2 = self.x;
        .self = $n;
        $[ret, ret2, dropped]
    "), "$[11,11,$true]");

    assert_eq!(ve(r"
        !dropped = $false;
        !self = $n;
        .self = ${
            drop = std:to_drop $n {|| .dropped = $true; },
            foo  = { self.x = self.x + 1; self.x },
            x    = 10,
        };
        !ret = self.foo[];
        !ret2 = self.x;
        .self = $n;
        $[ret, ret2, dropped]
    "), "$[11,11,$true]");

    assert_eq!(ve(r"
        !x = 10;
        { .x = 20 }[];
        x
    "), "20");
    assert_eq!(ve(r"
        !x = $& 10;
        { .x = 20 }[];
        x
    "), "20");
    assert_eq!(ve(r"
        !x = $&& 10;
        { .x = 20 }[];
        $*x
    "), "20");

    assert_eq!(ve(r"
        !x = 10;
        { .x = 20 }[];
        .x = x + 1;
        x
    "), "21");
    assert_eq!(ve(r"
        !x = $& 10;
        { .x = x + 1 }[];
        .x = x + 1;
        x
    "), "12");
    assert_eq!(ve(r"
        !x = $&& 10;
        { .x = 20 }[];
        .x = $*x + 1;
        $*x
    "), "21");

    assert_eq!(ve(r"
        !x = 10;
        !f = { .x = x + 1 };
        f[];
        .x = x + 1;
        f[];
        x
    "), "13");

    assert_eq!(ve(r"
        !x = $& 10;
        !f = { .x = x + 1 };
        f[];
        .x = x + 1;
        f[];
        x
    "), "13");

    assert_eq!(ve(r"
        !x = $&& 10;
        !f = { .x = x + 1 };
        f[];
        .*x = $*x + 1;
        f[];
        $*x
    "), "13");

    assert_eq!(ve(r"
        !x = $&& 19;
        !y = std:weaken x;
        !f = { y }; # weak refs are captured and stay weak
        !a = str f[];
        .x = $n;
        !b = str f[];
        std:str:join $q$,$ $[a, b];
    "), "\"19,\"");
}

#[test]
fn check_trim() {
    assert_eq!(ve("$qX foo X | std:str:trim_start"), "\"foo \"");
    assert_eq!(ve("$qX foo X | std:str:trim_end"),   "\" foo\"");
    assert_eq!(ve("$qX foo X | std:str:trim"),       "\"foo\"");
    assert_eq!(ve("$qX foo \n X | std:str:trim"),    "\"foo\"");
}

#[test]
fn check_accumulator() {
    assert_eq!(ve(r"$@v   $[1,2,3]\$+2*_"),         "$[2,4,6]");
    assert_eq!(ve(r"$@vec $[1,2,3]\$+2*_"),         "$[2,4,6]");

    assert_eq!(ve(r"$@i   $[1,2,3]\$+_"),           "6");
    assert_eq!(ve(r"$@int $[1,2,3]\$+_"),           "6");

    assert_eq!(ve(r"$@s      $[1,2,3]\$+_"),        "\"123\"");
    assert_eq!(ve(r"$@string $[1,2,3]\$+_"),        "\"123\"");

    assert_eq!(ve(r"$@b      $[1,2,3]\$+_"),        "$b\"\\x01\\x02\\x03\"");
    assert_eq!(ve(r"$@bytes  $[1,2,3]\$+_"),        "$b\"\\x01\\x02\\x03\"");

    assert_eq!(ve(r"std:num:round ~ 10.0 * $@f     $[1.1,2.1,3.1]\$+_"), "63");
    assert_eq!(ve(r"std:num:round ~ 10.0 * $@float $[1.1,2.1,3.1]\$+_"), "63");

    assert_eq!(ve(r"($@m   $[1,2,3]\$+_ 2*_).2"),   "4");
    assert_eq!(ve(r"($@map $[1,2,3]\$+_ 2*_).2"),   "4");

    assert_eq!(ve("$@s $+10"),        "\"10\"");
    assert_eq!(ve("$@s $+10.1"),      "\"10.1\"");
    assert_eq!(ve("$@s $+$b\"ABC\""), "\"ABC\"");
    assert_eq!(ve("$@s $+$t"),        "\"$true\"");
    assert_eq!(ve("$@s $+$f"),        "\"$false\"");
    assert_eq!(ve("$@s $+\"ABC\""),   "\"ABC\"");

    assert_eq!(ve("$@b $+10"),        "$b\"\\n\"");
    assert_eq!(ve("$@b $+10.1"),      "$b\"\\n\"");
    assert_eq!(ve("$@b $+$b\"ABC\""), "$b\"ABC\"");
    assert_eq!(ve("$@b $+$t"),        "$b\"\\x01\"");
    assert_eq!(ve("$@b $+$f"),        "$b\"\\0\"");
    assert_eq!(ve("$@b $+\"ABC\""),   "$b\"ABC\"");

    assert_eq!(ve(r"
        $@v { !s = $@s { $+ :a; $+ :b }[]; $+ s; $+ s }[]
    "), "$[$<1=>\"ab\",$<1>]");
    assert_eq!(ve(r"
        $@v $+ ($@s $[1,2,3] \$+ _)
    "), "$[\"123\"]");
    assert_eq!(ve(r"
        !f = {
            $@v $+ ($@s $[1,2,3] \$+ _)
        };
        $@m $+ :j f[]
    "), "${j=$[\"123\"]}");
    assert_eq!(ve(r"
        !f = \:x{
            $@v $+ ($@s
                (return :x 10))
        };
        $@m $+ :l f[]
    "), "${l=10}");

    assert_eq!(ve(r"
        !res = $[];
        std:push res $@s iter i $i(1, 3) {
            !x = $@@;
            $+ ~ str i;
            std:push res $p(x, $@@);
        };
    "), "$[$p(\"\",$<1=>\"1\"),$p($<1>,$<2=>\"12\"),$<2>]");

    assert_eq!(ve(r#"$@i $p(";", 0) "1;3;44;2;4" |> $+"#), "54");
}

#[test]
fn check_return_sp() {
    assert_eq!(ve(r"
        !:global g = {|| 11 };
        !:global f = { g (return 10) };
        f[];
    "), "10");
    assert_eq!(ve("{|| std:num:log2 (return 12) }[4,5]"), "12");
    assert_eq!(ve(r"
        {|| std:num:log2 (+ 5 6) (return 13) (+ 1 2) (+ 3 4)}[4,5]
    "), "13");
}

#[test]
fn check_accum() {
    assert_eq!(ve("std:accum $b\"a\" 1"),           "$b\"a\\x01\"");
    assert_eq!(ve("std:accum $b\"a\" 2.2"),         "$b\"a\\x02\"");
    assert_eq!(ve("std:accum $b\"a\" \"abcde\""),   "$b\"aabcde\"");
    assert_eq!(ve("std:accum $b\"a\" $b\"abcde\""), "$b\"aabcde\"");
    assert_eq!(ve("std:accum $b\"a\" $t"),          "$b\"a\\x01\"");

    assert_eq!(ve("std:accum \"a\" 1"),             "\"a1\"");
    assert_eq!(ve("std:accum \"a\" 2.2"),           "\"a2.2\"");
    assert_eq!(ve("std:accum \"a\" \"abcde\""),     "\"aabcde\"");
    assert_eq!(ve("std:accum \"a\" $b\"abcde\""),   "\"aabcde\"");
    assert_eq!(ve("std:accum \"a\" $t"),            "\"a$true\"");

    assert_eq!(ve("std:accum 10 1"),                "11");
    assert_eq!(ve("std:accum 11 2.2"),              "13");
    assert_eq!(ve("std:accum 12 \"3\""),            "15");

    assert_eq!(ve("std:accum 10.2 1"),              "11.2");
    assert_eq!(ve("std:num:round 10.0 * (std:accum 11.2 2.1)"),
               "133");
    assert_eq!(ve("std:accum 12.2 \"3\""),          "15.2");

    assert_eq!(ve("std:accum $[1] \"3\""),          "$[1,\"3\"]");
    assert_eq!(ve("std:accum $[1] $[2,3]"),         "$[1,$[2,3]]");
}

#[test]
fn check_error_reporting_func() {
    assert_eq!(ve("!f = {}; f 10;"),
        "EXEC ERR: Caught [1,6:<compiler:s_eval>(Func)@f]=>\
        [1,12:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 1\")");
    assert_eq!(ve("!x = ${}; x.foo = {}; x.foo 10;"),
        "EXEC ERR: Caught [1,19:<compiler:s_eval>(Func)@foo]=>\
        [1,29:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 1\")");
    assert_eq!(ve("!x = ${(\"foo\") = {}}; x.foo 10;"),
        "EXEC ERR: Caught [1,18:<compiler:s_eval>(Func)@foo]=>\
        [1,29:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 1\")");
    assert_eq!(ve("!x = ${foo = {}}; x.foo 10;"),
        "EXEC ERR: Caught [1,14:<compiler:s_eval>(Func)@foo]=>\
        [1,25:<compiler:s_eval>(Call)] SA::Panic(\"function expects at most 0 arguments, got 1\")");
}

#[test]
fn check_const() {
    assert_eq!(ve("!:const X = 32; X"),                     "32");
    assert_eq!(ve("!:const X = 32.4; X"),                   "32.4");
    assert_eq!(ve("!:const X = :XX; X"),                    ":XX");
    assert_eq!(ve("!:const X = $[1,2]; X.1"),               "2");
    assert_eq!(ve("!:const X = ${A=32}; X.A"),              "32");
    assert_eq!(ve("!:const X = \"fo\"; X"),                 "\"fo\"");
    assert_eq!(ve("!:const (A,B,X) = $[1,3,4]; $[X,B,A]"),  "$[4,3,1]");
    assert_eq!(ve("!:const (A,B,X) = ${A=1,B=3,X=4}; $[X,B,A]"),  "$[4,3,1]");

    assert_eq!(ve(r"
        !@import c tests:test_mod_const;
        c:XX
    "), "32");

    assert_eq!(ve(r"
        !@import c tests:test_mod_const;
        c:X2
    "), "$[1,2]");

    assert_eq!(ve(r"
        !:const K = $i(1, 2, 3);
        K
    "), "$i(1,2,3)");

    assert_eq!(ve(r"
        !:const K = $i(1, 2, 3, 5);
        K
    "), "$i(1,2,3,5)");

    assert_eq!(ve(r"
        !:const K = $i(1, 2);
        K
    "), "$i(1,2)");

    assert_eq!(ve(r"
        !:const K = $f(1, 2);
        K
    "), "$f(1,2)");

    assert_eq!(ve(r"
        !:const K = $f(1, 2, 3);
        K
    "), "$f(1,2,3)");

    assert_eq!(ve(r"
        !:const K = $f(1, 2, 3, 4);
        K
    "), "$f(1,2,3,4)");

    assert_eq!(ve(r"
        !:const P = $p(:a, :b);
        $[P.v, P.k]
    "), "$[:a,:b]");
}

#[test]
fn check_threads() {
    assert_eq!(ve("
        (std:thread:spawn $q{ $b\"abc\" }).join[];
    "), "$b\"abc\"");
    assert_eq!(ve("
        (std:thread:spawn $q{ $o(49) }).join[];
    "), "$o(49)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $p(:foo,2) }).join[];
    "), "$p(:foo,2)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $f(1,2) }).join[];
    "), "$f(1,2)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $f(1,2,3) }).join[];
    "), "$f(1,2,3)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $f(1,2,3,4) }).join[];
    "), "$f(1,2,3,4)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $i(1,2) }).join[];
    "), "$i(1,2)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $i(1,2,3) }).join[];
    "), "$i(1,2,3)");
    assert_eq!(ve("
        (std:thread:spawn $q{ $i(1,2,3,4) }).join[];
    "), "$i(1,2,3,4)");
    assert_eq!(ve("
        !h = std:thread:spawn $q( $[1,2,${a=20},:x] );
        h.join[];
    "), "$[1,2,${a=20},:x]");
    assert_eq!(ve("
        !at = std:sync:atom:new 99;
        !h = std:thread:spawn $q{
            !@wlambda;
            !@import std;

            !a = THREAD_ARG0;
            !b = a.7.read[];
            a.7.write a;
            $[a.0, a.1, a.2, a.3, a.4, a.5, a.6, b]
        } $[$[1,2,${a=20},:x,\"oo\",$o(33),$p(1,$p(2,$i(3,4))), at]];
        $[
            h.join[],
            std:take 7 ~ at.read[]
        ];
    "),
    "$[$[1,2,${a=20},:x,\"oo\",$o(33),$p(1,$p(2,$i(3,4))),99],$[1,2,${a=20},:x,\"oo\",$o(33),$p(1,$p(2,$i(3,4)))]]");

    assert_eq!(ve(r#"
        !chan = std:sync:mpsc:new[];
        !h = std:thread:spawn ($code {
            !@import std;
            !@wlambda;

            chan.send 20;
            99
        }[]) ${ chan = chan };

        !msg = chan.recv_timeout $p(:ms, 1000);
        std:assert_eq (type msg) "optional";

        $[h.join[], msg[]];
    "#), "$[99,20]");

    assert_eq!(ve(r#"
        !chan = std:sync:mpsc:new[];
        !chan2 = std:sync:mpsc:new[];
        !h = std:thread:spawn ($code {
            !@import std;
            !@wlambda;

            !m = chan2.recv[];
            chan.send m;
            99
        }[]) ${ chan = chan, chan2 = chan2 };

        !msg = chan.try_recv[];
        std:assert_eq (type msg) "optional";
        std:assert ~ not msg;

        chan2.send "test";
        !msg = $n;
        while not[msg] {
            .msg = chan.try_recv[];
            std:thread:sleep $p(:ms, 10);
        };

        $[h.join[], msg[]];
    "#), "$[99,\"test\"]");
}

#[test]
fn check_thread_valslot() {
    assert_eq!(ve(r#"
        !slt1 = std:sync:slot:new[];
        !slt2 = std:sync:slot:new[];

        std:assert slt1.check_empty[];

        std:assert_eq slt1.try_recv[] $o();

        !h = std:thread:spawn ($code {
            !@import std;
            _READY.send 11;

            slt1.send $p(:x, $[1,2,3]);
            slt2.recv[];
        }[]) ${ slt1 = slt1, slt2 = slt2 };

        std:assert_eq h.recv_ready[] 11;

        !r1 = slt1.recv[];

        std:assert_eq (str r1) (str $p(:x, $[1,2,3]));
        slt2.send $true;
        h.join[]
    "#), "$true");

    assert_eq!(ve(r#"
        !slt1 = std:sync:slot:new[];
        !slt2 = std:sync:slot:new[];

        !h = std:thread:spawn ($code {
            !@import std;
            _READY.send $true;

            slt2.send 45;
            std:thread:sleep $p(:ms, 500);
            slt1.send 44;

            100
        }[]) ${ slt1 = slt1, slt2 = slt2 };

        std:assert ~ unwrap h.recv_ready[];

        !r1 = slt1.recv_timeout $p(:ms, 100);
        std:assert_eq r1 $o();

        std:assert_eq slt2.try_recv[] $o(45);

        .r1 = slt1.recv_timeout $p(:ms, 100);
        std:assert_eq r1 $o();
        .r1 = slt1.recv_timeout $p(:ms, 500);
        std:assert_eq r1 44;

        h.join[]
    "#), "100");


    assert_eq!(ve(r#"
        !slt1 = std:sync:slot:new[];

        !h = std:thread:spawn ($code {
            !@import std;
            _READY.send $true;

            std:assert_eq _READY.check_empty[] $false;

            slt1.send :ok;

            !ret = _READY.wait_empty[];
            std:assert_eq ret $true;

            100
        }[]) ${ slt1 = slt1 };

        slt1.recv_timeout $p(:ms, 500);

        std:assert ~ unwrap h.recv_ready[];

        h.join[]
    "#), "100");
}

#[test]
fn check_nvec() {
    assert_eq!(ve("$i(1, 2)"),                  "$i(1,2)");
    assert_eq!(ve("$i(1, 2) * 2"),              "$i(2,4)");
    assert_eq!(ve("$f(1, 2) / 2"),              "$f(0.5,1)");
    assert_eq!(ve("$f(2, 0) - $f(2, 0)"),       "$f(0,0)");
    assert_eq!(ve("$i(2, 0) - $f(2, 0)"),       "$i(0,0)");
    assert_eq!(ve("$f(2, 0) + $f(0, 2)"),       "$f(2,2)");
    assert_eq!(ve("$f(2, 0) + $i(1, 2)"),       "$f(3,2)");
    assert_eq!(ve("$i(2, 0) + $f(1, 2)"),       "$i(3,2)");
    assert_eq!(ve("$f(2, 0) + $f(2, 2)"),       "$f(4,2)");
    assert_eq!(ve("$i(2, 0) + ${y=2,x=1,z=0}"), "$i(3,2,0)");
    assert_eq!(ve("$i(2, 0) + $[2,1,3]"),       "$i(4,1,3)");
    assert_eq!(ve("$f(2, 0) == ${x=2,y=0}"),    "$false");
    assert_eq!(ve("$i(0, 0) == ${}"),           "$false");
    assert_eq!(ve("$i(0, 0) == ${}"),           "$false");
    assert_eq!(ve("$i(0, 0) == $f(0, 0)"),      "$false");

    assert_eq!(ve("$i(2, 0) + $p(1, 2)"),       "$i(3,2)");

    assert_eq!(ve("$i(2, 3).x"),        "2");
    assert_eq!(ve("$i(2, 3).y"),        "3");
    assert_eq!(ve("$i(2, 3, 4).x"),     "2");
    assert_eq!(ve("$i(2, 3, 4).y"),     "3");
    assert_eq!(ve("$i(2, 3, 4).z"),     "4");
    assert_eq!(ve("$i(2, 3, 4, 5).x"),  "2");
    assert_eq!(ve("$i(2, 3, 4, 5).y"),  "3");
    assert_eq!(ve("$i(2, 3, 4, 5).z"),  "4");
    assert_eq!(ve("$i(2, 3, 4, 5).w"),  "5");

    assert_eq!(ve("$f(2.1, 3.1).x"),            "2.1");
    assert_eq!(ve("$f(2.1, 3.1).y"),            "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).x"),       "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).y"),       "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).z"),       "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).x"),  "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).y"),  "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).z"),  "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).w"),  "5.3");

    assert_eq!(ve("$i(2, 3).0"),        "2");
    assert_eq!(ve("$i(2, 3).1"),        "3");
    assert_eq!(ve("$i(2, 3, 4).0"),     "2");
    assert_eq!(ve("$i(2, 3, 4).1"),     "3");
    assert_eq!(ve("$i(2, 3, 4).2"),     "4");
    assert_eq!(ve("$i(2, 3, 4, 5).0"),  "2");
    assert_eq!(ve("$i(2, 3, 4, 5).1"),  "3");
    assert_eq!(ve("$i(2, 3, 4, 5).2"),  "4");
    assert_eq!(ve("$i(2, 3, 4, 5).3"),  "5");

    assert_eq!(ve("$f(2.1, 3.1).0"),            "2.1");
    assert_eq!(ve("$f(2.1, 3.1).1"),            "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).0"),       "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).1"),       "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).2"),       "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).0"),  "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).1"),  "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).2"),  "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).3"),  "5.3");

    assert_eq!(ve("$i(2, 3).r"),        "2");
    assert_eq!(ve("$i(2, 3).g"),        "3");
    assert_eq!(ve("$i(2, 3, 4).r"),     "2");
    assert_eq!(ve("$i(2, 3, 4).g"),     "3");
    assert_eq!(ve("$i(2, 3, 4).b"),     "4");
    assert_eq!(ve("$i(2, 3, 4, 5).r"),  "2");
    assert_eq!(ve("$i(2, 3, 4, 5).g"),  "3");
    assert_eq!(ve("$i(2, 3, 4, 5).b"),  "4");
    assert_eq!(ve("$i(2, 3, 4, 5).a"),  "5");

    assert_eq!(ve("$i(2, 3).h"),        "2");
    assert_eq!(ve("$i(2, 3).s"),        "3");
    assert_eq!(ve("$i(2, 3, 4).h"),     "2");
    assert_eq!(ve("$i(2, 3, 4).s"),     "3");
    assert_eq!(ve("$i(2, 3, 4).v"),     "4");
    assert_eq!(ve("$i(2, 3, 4, 5).h"),  "2");
    assert_eq!(ve("$i(2, 3, 4, 5).s"),  "3");
    assert_eq!(ve("$i(2, 3, 4, 5).v"),  "4");
    assert_eq!(ve("$i(2, 3, 4, 5).a"),  "5");

    assert_eq!(ve("$i(1, 2).xx"),       "$i(1,1)");
    assert_eq!(ve("$i(1, 2).xxx"),      "$i(1,1,1)");
    assert_eq!(ve("$i(1, 2).xyxy"),     "$i(1,2,1,2)");
    assert_eq!(ve("$i(1, 2, 3).zx"),    "$i(3,1)");
    assert_eq!(ve("$i(1, 2, 3).zxy"),   "$i(3,1,2)");
    assert_eq!(ve("$i(1, 2, 3).zx"),    "$i(3,1)");
    assert_eq!(ve("$i(1, 2, 3).zx"),    "$i(3,1)");

    assert_eq!(ve("$i(255, 128, 64).rrg"),       "$i(255,255,128)");
    assert_eq!(ve("$i(255, 128).rgba"),          "$i(255,128,0,0)");
    assert_eq!(ve("$i(255, 128, 64).rgba"),      "$i(255,128,64,0)");
    assert_eq!(ve("$i(255, 128, 64, 255).rgba"), "$i(255,128,64,255)");
    assert_eq!(ve("$i(255, 128, 64).hhs"),       "$i(255,255,128)");
    assert_eq!(ve("$i(255, 128).hsva"),          "$i(255,128,0,0)");
    assert_eq!(ve("$i(255, 128, 64).hsva"),      "$i(255,128,64,0)");
    assert_eq!(ve("$i(255, 128, 64, 255).hsva"), "$i(255,128,64,255)");

    assert_eq!(ve("ivec $p(4.3, 5.4)"),  "$i(4,5)");
    assert_eq!(ve("ivec2 $p(4.3, 5.4)"), "$i(4,5)");
    assert_eq!(ve("ivec3 $p(4.3, 5.4)"), "$i(4,5,0)");
    assert_eq!(ve("ivec4 $p(4.3, 5.4)"), "$i(4,5,0,0)");

    assert_eq!(ve("fvec $p(4.3, 5.4)"),  "$f(4.3,5.4)");
    assert_eq!(ve("fvec2 $p(4.3, 5.4)"), "$f(4.3,5.4)");
    assert_eq!(ve("fvec3 $p(4.3, 5.4)"), "$f(4.3,5.4,0)");
    assert_eq!(ve("fvec4 $p(4.3, 5.4)"), "$f(4.3,5.4,0,0)");

    assert_eq!(ve("is_nvec $p(0, 0)"), "$false");
    assert_eq!(ve("is_ivec $p(0, 0)"), "$false");
    assert_eq!(ve("is_fvec $p(0, 0)"), "$false");

    assert_eq!(ve("is_nvec $i(0, 0)"), "$true");
    assert_eq!(ve("is_ivec $i(0, 0)"), "$true");
    assert_eq!(ve("is_fvec $i(0, 0)"), "$false");

    assert_eq!(ve("is_nvec $f(0, 0)"), "$true");
    assert_eq!(ve("is_ivec $f(0, 0)"), "$false");
    assert_eq!(ve("is_fvec $f(0, 0)"), "$true");

    assert_eq!(ve("nvec_len $i(0,0)"),     "2");
    assert_eq!(ve("nvec_len $i(0,0,0)"),   "3");
    assert_eq!(ve("nvec_len $i(0,0,0,0)"), "4");
    assert_eq!(ve("nvec_len $f(0,0)"),     "2");
    assert_eq!(ve("nvec_len $f(0,0,0)"),   "3");
    assert_eq!(ve("nvec_len $f(0,0,0,0)"), "4");
    assert_eq!(ve("nvec_len $p(0,0)"),     "0");
}

#[test]
fn check_pairs() {
    assert_eq!(ve("$p(1 + 2, 3 + 4)"),  "$p(3,7)");
    assert_eq!(ve("$p(:a, :f).0"),      ":a");
    assert_eq!(ve("$p(:a, :f).1"),      ":f");
    assert_eq!(ve("$p(:a, :f).car"),    ":a");
    assert_eq!(ve("$p(:a, :f).cdr"),    ":f");
    assert_eq!(ve("$p(:a, :f).first"),  ":a");
    assert_eq!(ve("$p(:a, :f).second"), ":f");
    assert_eq!(ve("$p(:a, :f).head"),   ":a");
    assert_eq!(ve("$p(:a, :f).tail"),   ":f");
    assert_eq!(ve("$true $p(:a, :f)"),  ":f");
    assert_eq!(ve("$false $p(:a, :f)"), ":a");
    assert_eq!(ve("cons :a :f"),        "$p(:a,:f)");
    assert_eq!(ve("cons :a :f |> 0"),   ":a");
    assert_eq!(ve("cons :a :f |> 1"),   ":f");

    assert_eq!(ve("(cons :a :f) == $p(:a,:f)"),   "$true");
    assert_eq!(ve("(cons :b :f) == $p(:a,:f)"),   "$false");

    assert_eq!(ve("bool $p($t,$t)"),   "$true");
    assert_eq!(ve("bool $p($f,$t)"),   "$true");
    assert_eq!(ve("bool $p($t,$f)"),   "$true");
    assert_eq!(ve("bool $p($f,$f)"),   "$false");

    assert_eq!(ve("int $p(3.3,4.4)"),   "3");
    assert_eq!(ve("float $p(3.3,4.4)"), "3.3");

    assert_eq!(ve(r"
        !p1 = $p(1, 2);
        !p2 = $p(1, 2);
        $[std:ref_id[p1] == std:ref_id[p2],
          std:ref_id[p1] == std:ref_id[p2]]
    "), "$[$false,$false]");

    assert_eq!(ve("$p(0, 3)             $q ABCDEFG "),      "\"ABC\"");
    assert_eq!(ve("$p(\";\", 2)         $q AB.C.DE.FG "),   "$[\"AB.C.DE.FG\"]");
    assert_eq!(ve("$p(\";\", 2)         $q AB;C;DE;FG "),   "$[\"AB\",\"C;DE;FG\"]");
    assert_eq!(ve("$p(\";\", 2)         $q ;C;DE;FG "),     "$[\"\",\"C;DE;FG\"]");
    assert_eq!(ve("$p(\";\", 3)         $q ;C; "),          "$[\"\",\"C\",\"\"]");
    assert_eq!(ve("$p(\";\", 0)         $q AB;C;DE;FG "),   "$[\"AB\",\"C\",\"DE\",\"FG\"]");
    assert_eq!(ve("$p(\";\", 0)         $q ;C;DE; "),       "$[\"\",\"C\",\"DE\",\"\"]");
    assert_eq!(ve("$p(\";\", \"_\")     $q AB;C;DE;FG "),   "\"AB_C_DE_FG\"");

    assert_eq!(ve("$p(0, 3)               $Q ABCDEFG "),      "$b\"ABC\"");
    assert_eq!(ve("$p($b\";\", 2)         $Q AB;C;DE;FG "),   "$[$b\"AB\",$b\"C;DE;FG\"]");
    assert_eq!(ve("$p($b\";\", 2)         $Q ;C;DE;FG "),     "$[$b\"\",$b\"C;DE;FG\"]");
    assert_eq!(ve("$p($b\";\", 3)         $Q ;C; "),          "$[$b\"\",$b\"C\",$b\"\"]");
    assert_eq!(ve("$p($b\";\", 0)         $Q AB;C;DE;FG "),   "$[$b\"AB\",$b\"C\",$b\"DE\",$b\"FG\"]");
    assert_eq!(ve("$p($b\";\", 0)         $Q ;C;DE; "),       "$[$b\"\",$b\"C\",$b\"DE\",$b\"\"]");

    assert_eq!(ve("$p($b\";;\", 2)        $Q AB;C;DE;FG "),   "$[$b\"AB;C;DE;FG\"]");
    assert_eq!(ve("$p($b\";;\", 2)        $Q AB;;C;;DE;;FG "),"$[$b\"AB\",$b\"C;;DE;;FG\"]");
    assert_eq!(ve("$p($b\";;\", 2)        $Q ;;C;;DE;;FG "),  "$[$b\"\",$b\"C;;DE;;FG\"]");
    assert_eq!(ve("$p($b\";;\", 3)        $Q ;;C; "),         "$[$b\"\",$b\"C;\"]");
    assert_eq!(ve("$p($b\";;\", 3)        $Q ;;C;; "),        "$[$b\"\",$b\"C\",$b\"\"]");
    assert_eq!(ve("$p($b\";;\", 0)        $Q AB;;C;;DE;;FG "),"$[$b\"AB\",$b\"C\",$b\"DE\",$b\"FG\"]");
    assert_eq!(ve("$p($b\";;\", 0)        $Q ;;C;;DE;; "),    "$[$b\"\",$b\"C\",$b\"DE\",$b\"\"]");
    assert_eq!(ve("$p($b\";;\", 0)        $Q ;;C;;DE; "),     "$[$b\"\",$b\"C\",$b\"DE;\"]");

    assert_eq!(ve("$p($b\";\", $b\"_\")     $Q AB;C;DE;FG "),               "$b\"AB_C_DE_FG\"");
    assert_eq!(ve("$p($b\";;\", $b\"_\")    $Q AB;;C;;DE;;FG "),            "$b\"AB_C_DE_FG\"");
    assert_eq!(ve("$p($b\";;;\", $b\"_\")   $Q ;;;AB;;;C;;;DE;;;FG;;; "),   "$b\"_AB_C_DE_FG_\"");
    assert_eq!(ve("$p($b\";;\", $b\"____\") $Q AB;;C;;DE;;FG "),            "$b\"AB____C____DE____FG\"");

    assert_eq!(ve("$q AB;;C;;DE;;FG  $p(\";;\", \"____\")"),                "\"AB____C____DE____FG\"");
    assert_eq!(ve("$Q AB;;C;;DE;;FG  $p($b\";;\", $b\"____\")"),            "$b\"AB____C____DE____FG\"");

    assert_eq!(ve("$q ABCDEF  $i(2,3)"), "\"CDE\"");
    assert_eq!(ve("$Q ABCDEF  $i(2,3)"), "$b\"CDE\"");

    assert_eq!(ve("1 => 2 => 3 => 4"), "$p(1,$p(2,$p(3,4)))");
}

#[test]
fn check_if() {
//assert_eq!(ve(r"
//!res =
//    ? { !x = 2; x > 1 } 39;
//
//std:assert_eq res 39;
//"), "");
    assert_eq!(v("? { !x = 10; x > 1 } 99 49"), "99");
    assert_eq!(v("if { !x = 10; x > 1 } 99 49"), "99");
    assert_eq!(v("? { !x = 10; x > 1 } 99"),    "99");
    assert_eq!(v("if { !x = 10; x > 1 } 99"),    "99");
    assert_eq!(v("!res = ? { !x = 10; x > 1 } 99 49; res"), "99");
    assert_eq!(v("!res = ? { !x = 10; x > 1 } 99; res"),    "99");
    assert_eq!(ve("? 2 > 3 { 1 } { 2 }"), "2");
    assert_eq!(ve("? 2 < 3 { 1 } { 2 }"), "1");
    assert_eq!(ve("if 2 > 3 { 1 } { 2 }"), "2");
    assert_eq!(ve("if 2 < 3 { 1 } { 2 }"), "1");
    assert_eq!(ve(r"
        !y = ? 2 < 3 { !x = 11; 1 } { 2 };
        y
    "), "1");
    assert_eq!(ve(r"
        !x = 11;
        !k = $n;
        !y = ? 2 < 3 {
            !x = $&& 21;
            .k = { x };
            1
        } {
            2
        };
        $[x, y, k[]]
    "), "$[11,1,21]");
    assert_eq!(ve(r"
        !x = 10;
        !k = $n;
        !y = ? 2 < 3 {
            !x = $&& 20;
            ? $t {
                .k = { x };
            };
            1
        };
        $[x, y, k[]]
    "), "$[10,1,20]");
    assert_eq!(ve(r"
        !x = 10;
        !k = $n;
        !y = ? 2 < 3 {
            !x = $&& 20;
            ? $t {
                !x = 30;
                .k = { x };
            };
            1
        };
        $[x, y, k[]]
    "), "$[10,1,$n]");
    assert_eq!(ve("? 1"),
        "COMPILE ERROR: [1,3:<compiler:s_eval>] Compilation Error: ?/if takes 1 or 2 arguments (condition and expression)");
    assert_eq!(ve("? 1 2 3 4"),
        "COMPILE ERROR: [1,3:<compiler:s_eval>] Compilation Error: ?/if takes 1 or 2 arguments (condition and expression)");
}

#[test]
fn check_backslash_function() {
    assert_eq!(ve("!x = $[\\1,\\2,\\3 + _,5]; $[x.1[],x.2 4,x.3[]]"), "$[2,7,5]");
    assert_eq!(ve("!x = ${a = \\1, b = \\2 * _, c = 9}; $[x.a[],x.b 4,x.c[]]"), "$[1,8,9]");
}

#[test]
fn check_iter() {
    assert_eq!(ve("!x = 0; for $n {|| .x = 20 }; x"), "0");
    assert_eq!(ve(r"
        !x = $[1,2,3,4,5,6,7];
        !r = 0;
        iter i x {
            .r = r + i;
        };
        r
    "), "28");
    assert_eq!(ve(r"
        !x = $[1,2,3,4];
        !r = 0;
        iter i x {
            .r = r + i;
            ? r >= 6 break[];
        };
        r
    "), "6");
    assert_eq!(v(r"
        !sum = 0;
        iter i $[1, 2, 3, 4] { .sum = sum + i; };
        sum
    "), "10");

    assert_eq!(v(r"
        !r = $[];
        iter i ${a=10} { !(v, k) = i; std:push r i; std:push r v; std:push r k; };
        r
    "), "$[$p(10,$<1=>\"a\"),10,$<1>]");

    assert_eq!(v(r"
        !r = $[];
        iter i $i(1, 10) { std:push r i; };
        r
    "), "$[1,2,3,4,5,6,7,8,9]");

    assert_eq!(v(r"
        !r = $[];
        iter i $p(1, 10) { std:push r i; };
        r
    "), "$[1,2,3,4,5,6,7,8,9]");

    assert_eq!(v(r"
        !r = $[];
        iter i 1 => 10 { std:push r i; };
        r
    "), "$[1,2,3,4,5,6,7,8,9]");

    assert_eq!(v(r"
        !r = $[];
        iter i $i(1, 10, 2) { std:push r i; };
        r
    "), "$[1,3,5,7,9]");

    assert_eq!(v(r"
        !r = $[];
        iter i $f(0.0, 10.0, 3.2) { std:push r int[i * 10]; };
        r
    "), "$[0,32,64,96]");

    assert_eq!(v(r"
        !r = $[];
        iter i $f(0.0, 10.0) { std:push r int[i * 10]; };
        r
    "), "$[0,10,20,30,40,50,60,70,80,90]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[90,80,70];
        !v = $[];

        $@v
            iter ai a
            ~ iter bi b {
                $+ $p(ai, bi);
            };
    "), "$[$p(1,90),$p(1,80),$p(1,70),$p(2,90),$p(2,80),$p(2,70),$p(3,90),$p(3,80),$p(3,70),$p(4,90),$p(4,80),$p(4,70)]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[90,80,70];
        !v = $[];

        $@v
            iter ai a {
                ? (ai % 2) == 0 {
                    iter bi b {
                        $+ $p(ai, bi);
                    };
                };
            }
    "), "$[$p(2,90),$p(2,80),$p(2,70),$p(4,90),$p(4,80),$p(4,70)]");

    assert_eq!(v(r"
        $@v iter i $i(1,4) {
            ? i == 2 next[];
            $+ i
        }
    "), "$[1,3]");

    assert_eq!(v(r"
        !x = 0;
        iter i $i(1,4) {
            ? i == 2 next[];
            .x = x + i;
        };
        x
    "), "4");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[90,80,70];
        !v = $[];

        $@v
            iter ai a {
                ? (ai % 2) == 0 next[];
                $+ $p(ai, ai);
            }
    "), "$[$p(1,1),$p(3,3)]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[90,80];

        $@v
            iter ai a {
                iter bi b {
                    ? (ai % 2) == 0 next[];
                    $+ $p(ai, bi);
                }
            }
    "), "$[$p(1,90),$p(1,80),$p(3,90),$p(3,80)]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[99,100,200,300];

        $@v
            iter ai a {
                iter bi b {
                    ? (ai % 2) == 0 next[];
                    ? bi > 100 break[];
                    $+ $p(ai, bi);
                }
            }
    "), "$[$p(1,99),$p(1,100),$p(3,99),$p(3,100)]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[99,100,200,300];

        $@v
            iter ai a {
                iter bi b {
                    ? (ai % 2) == 0 next[];
                    $+ $p(ai, bi);
                    ? bi > 100 break[];
                }
            }
    "), "$[$p(1,99),$p(1,100),$p(1,200),$p(3,99),$p(3,100),$p(3,200)]");


    assert_eq!(v(r"
        $@v
            iter ai $i(0,2) {
                $+ $@i
                    iter bi $i(1,2) {
                        $+ bi;
                        break[];
                    };
            }
    "), "$[1,1]");

    assert_eq!(v(r"
        $@v
            iter ai $i(3,5) {
                $+ $@i
                    iter bi $i(ai,ai + 1) {
                        $+ bi;
                        break[];
                    };
            }
    "), "$[3,4]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        $@v
            iter ai a {
                $+ $@i
                    iter bi $i(0,1) {
                        $+ bi + ai;
                        break[];
                    };
            }
    "), "$[1,2,3,4]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[99,100,200,300];

        $@v
            iter ai a {
                $+ $@i
                    iter bi b {
                        ? (ai % 2) == 0 next[];
                        $+ bi;
                        ? bi > 100 break[];
                    };
            }
    "), "$[399,0,399,0]");

    assert_eq!(v(r"
        !a = $[1,2,3,4];
        !b = $[99,100,200,300];

        $@v
            iter ai a {
                !x = $@i
                    iter bi b {
                        ? (ai % 2) == 0 next[];
                        $+ bi;
                        ? bi > 100 break[];
                    };
                $+ x;
            }
    "), "$[399,0,399,0]");

    assert_eq!(v(r"
        !it = $iter $i(0,10);

        $@v it \$+ _;
    "), "$[0,1,2,3,4,5,6,7,8,9]");

    assert_eq!(v(r"
        !it = $iter $i(0,3);

        $@v it $+;
    "), "$[0,1,2]");

    assert_eq!(v(r"
        !it = $iter $i(0,4);
        $@v it \$+ $p(_, unwrap it[]);
    "), "$[$p(0,1),$p(2,3)]");

    assert_eq!(v(r"
        !it = $iter $i(0,5);
        $@v it \$+ $p(_, unwrap it[]);
    "), "Execution error: Jumped out of execution: [?]=>[3,31:<wlambda::eval>(Call)]=>[3,15:<wlambda::eval>(Func)@it]=>[3,14:<wlambda::eval>(Call)] SA::Panic(\"unwrap empty option!\")");
}

#[test]
fn check_function_local_space() {
    assert_eq!(v(r"
        { len ~ ? $true { !x = 4; :abc }; }[]
    "), "3");
    assert_eq!(v(r"
        {
            len $@s
                iter i $i(0,1) {
                    $+ :abcd;
                    break[];
                };
        }[]
    "), "4");
    assert_eq!(v(r"
        !x = 0;
        iter ai $i(0,1) {
            .x = len $@s
                iter i $i(0,1) {
                    $+ :kkkkk;
                    break[];
                };
        };
        x
    "), "5");

}

#[test]
fn check_iter_data() {
    assert_eq!(ve(r"
        !it = $iter $[1,2,3,4];
        $[int it, it[], ${a=float[it] + 0.2}, ${b=it[]}]
    "), "$[1,$o(2),${a=3.2},${b=$o(4)}]");

    assert_eq!(ve(r"
        !it = $iter $q abc ;
        it[];
        it[][];
    "), "\"b\"");

    assert_eq!(ve(r"
        !it = $iter $Q abc ;
        it[];
        it[][];
    "), "$b\"b\"");

    assert_eq!(ve(r"
        !it = $iter $i(0, 10);
        it[];
        $[
            it[][],
            it[][],
            it[][],
        ]
    "), "$[1,2,3]");

    assert_eq!(ve(r"
        !it = $iter $i(0, 10, 2);
        it[];
        $[
            it[][],
            it[][],
            it[][],
        ]
    "), "$[2,4,6]");

    assert_eq!(ve(r"
        !it = $iter $true;
        $[
            it[][],
            it[][],
        ]
    "), "$[$true,$n]");

    // splicing!
    assert_eq!(ve(r"
        !it = $iter $i(5,8);
        $[:a, *it, :b]
    "), "$[:a,5,6,7,:b]");

    // zip!
    assert_eq!(ve(r"
        !it_a = $iter $[1, 2, 3, 4];
        !it_b = $iter $[21, 22, 23];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "), "$[$p(1,21),$p(2,22),$p(3,23),$o()]");

    assert_eq!(ve(r"
        !it_a = $iter $[1, 2, 3];
        !it_b = $iter $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "), "$[$p(1,21),$p(2,22),$p(3,23),$o()]");

    assert_eq!(ve(r"
        !it_a = $iter $[1, 2, 3];
        !it_b = $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "), "$[$p(1,21),$p(2,22),$p(3,23),$o()]");

    assert_eq!(ve(r"
        !it_a = $iter $[1, 2, 3, $none];
        !it_b = $iter $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "), "$[$p(1,21),$p(2,22),$p(3,23),$o($p($n,24))]");

    assert_eq!(ve(r"
        !it_a = $iter ${a = 10};
        !it_b = $iter $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][]]
    "), "$[$p($p(10,\"a\"),21),$n]");

    assert_eq!(ve(r"
        !it_a = $iter ${a = 10};
        !it_b = ${b = 20};
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][]]
    "), "$[$p($p(10,\"a\"),$p(20,\"b\")),$n]");

    assert_eq!(ve(r"
        !it_a = $iter $[1, 11, 2, 12, 3, 13, 4, 14];
        !it = $iter $p(it_a, it_a);

        $[it[][], it[][], it[][], it[]]
    "), "$[$p(1,11),$p(2,12),$p(3,13),$o($p(4,14))]");
}

#[test]
fn check_kve_funcs() {
    assert_eq!(ve("std:sort ~ std:keys      ${a=10,b=20,c=30}"),       "$[\"a\",\"b\",\"c\"]");
    assert_eq!(ve("std:sort ~ std:values    ${a=10,b=20,c=30}"),       "$[10,20,30]");
    assert_eq!(ve("!it = $iter ${a=10,b=20,c=30}; std:sort $[*it]"),   "$[10,20,30]");
    assert_eq!(ve("std:keys      $[4,5,6,7,8]"),                       "$[0,1,2,3,4]");
    assert_eq!(ve("std:values    $[4,5,6,7,8]"),                       "$[4,5,6,7,8]");
    assert_eq!(ve("!it = $iter $p(:enumerate, $[4,5,6,7,8]); $[*it]"), "$[0,1,2,3,4]");
    assert_eq!(ve("!it = $iter $i(9,30,4); std:keys      it"),         "$[]");
    assert_eq!(ve("!it = $iter ${a=10};    std:keys      it"),         "$[\"a\"]");
    assert_eq!(ve("!it = $iter $i(9,30,4); std:values    it"),         "$[9,13,17,21,25,29]");
    assert_eq!(ve("!it = $iter $i(9,30,4); $[*$iter $p(:enumerate, it)]"), "$[0,1,2,3,4,5]");

}

#[test]
fn check_delete() {
    assert_eq!(ve("!v = $[1,2,3];     std:delete v 1; v"),          "$[1,3]");
    assert_eq!(ve("!v = ${a=10,b=20}; std:delete v :a; v"),     "${b=20}");
}

#[test]
fn check_lerp_smoothstep() {
    assert_eq!(ve("std:num:lerp       0.0 1000.0 0.1 | int"), "100");
    assert_eq!(ve("std:num:lerp       0.0 1000.0 0.9 | int"), "900");
    assert_eq!(ve("std:num:lerp       0.0 1000.0 0.5 | int"), "500");

    assert_eq!(ve("10000.0 * (std:num:smoothstep 0.0 1000.0 50.0 )  | int"), "72");
    assert_eq!(ve("10000.0 * (std:num:smoothstep 0.0 1000.0 900.0)  | int"), "9720");
    assert_eq!(ve("10000.0 * (std:num:smoothstep 0.0 1000.0 500.0 ) | int"), "5000");
}

#[test]
fn check_optionals() {
    assert_eq!(ve("$o(10)"),      "$o(10)");
    assert_eq!(ve("$o()"),        "$o()");

    assert_eq!(ve("is_optional $o(10)"),      "$true");
    assert_eq!(ve("is_optional $o()"),        "$true");
    assert_eq!(ve("is_optional $none"),       "$false");

    assert_eq!(ve("is_some $o(10)"),      "$true");
    assert_eq!(ve("is_some $o()"),        "$false");

    assert_eq!(ve("is_none $o(10)"),      "$false");
    assert_eq!(ve("is_none $o()"),        "$true");

    assert_eq!(ve("$o(10) == $o(10)"),  "$true");
    assert_eq!(ve("$o(11) == $o(10)"),  "$false");
    assert_eq!(ve("$o() == $o()"),      "$true");
    assert_eq!(ve("$o() == $o(10)"),    "$false");
    assert_eq!(ve("$o(10) == $o()"),    "$false");

    assert_eq!(ve("bool $o()"),         "$false");
    assert_eq!(ve("bool $o(30)"),       "$true");
    assert_eq!(ve("bool $o(\"test\")"), "$true");
    assert_eq!(ve("not $o(30)"),        "$false");
    assert_eq!(ve("not $o()"),          "$true");
    assert_eq!(ve("not $o(\"xx\")"),    "$false");

    assert_eq!(ve("$o(10)[]"),              "10");
    assert_eq!(ve("$o()[]"),                "$n");

    assert_eq!(ve("$o() 340"),                   "EXEC ERR: Caught [1,6:<compiler:s_eval>(Call)] SA::Panic(\"Calling $none is invalid\")");
    assert_eq!(ve("$o(\"f\") \"a\" 340 4 :vvv"), "\"fa3404vvv\"");

    assert_eq!(ve(r"
        !x = $o(10);
        !y = $o(11);
        !x1 = std:ref_id x;
        !y1 = std:ref_id y;
        !x2 = std:ref_id x;
        !y2 = std:ref_id y;
        $[std:ref_id[x] != std:ref_id[y],
          std:ref_id[x] == x1,
          std:ref_id[x] == x2,
          std:ref_id[y] == y1,
          std:ref_id[y] == y2]
        "),
        "$[$true,$true,$true,$true,$true]");
}

#[test]
fn check_code_string_literals() {
    assert_eq!(ve(r#"
        !code = $code 1 + 2;
        code
    "#), "\"1 + 2\"");
    assert_eq!(ve(r#"
        !code = $c {
            !a = 302;
            !b = $i(1, 2, 3);
        };
        code
    "#), "\"{\\n            !a = 302;\\n            !b = $i(1, 2, 3);\\n        }\"");

    assert_eq!(ve(r#"
        !code = $c {
            !a = 302;
            !b = $i(1, 2, 3);
            !x = $c (a b c "fff");
        };
        code
    "#), "\"{\\n            !a = 302;\\n            !b = $i(1, 2, 3);\\n            !x = $c (a b c \\\"fff\\\");\\n        }\"");

    assert_eq!(ve(r#"
        !code = $c {
            !a = 302;
            !b = $i(1, 2, 3);
            !x = 
        };
        code
    "#), "PARSE ERROR: error[6,9:<compiler:s_eval>] Expected literal value, sub expression, block, key or identifier at code \'};\n        code\n    \'");
}

#[test]
fn check_quote() {
    assert_eq!(ve(r#"
        $[$q{fewof wefewop
            fwe []
            []
        }, 121]
    "#), "$[\"fewof wefewop\\n            fwe []\\n            []\\n        \",121]");
    assert_eq!(ve(r#"
        $q#fewof wefewop
            fwe { feofwef [ XX }(]})]w}
        #
    "#), "\"fewof wefewop\\n            fwe { feofwef [ XX }(]})]w}\\n        \"");
}

#[test]
fn check_color_functions() {
    assert_eq!(ve("ivec ~ (std:v:hex2rgba_f :33C0CC80) * 100"), "$i(20,75,80,50)");
    assert_eq!(ve("std:v:hex2rgba_i :33C0CC80"),                "$i(51,192,204,128)");
}

#[test]
fn check_regex_patterns() {
    assert_eq!(ve("type $r(a)"),   "\"function\"");
    assert_eq!(ve("$r(a\\)"), "COMPILE ERROR: [1,7:<compiler:s_eval>] Compilation Error: bad pattern: error[1,3:<pattern>] EOF while parsing: Unexpected EOF at code \'\'");

    assert_eq!(ve("$r((^$+a)rba) $q foobaaarba "),             "$[\"aaarba\",\"aaa\"]");
    assert_eq!(ve("$r($+a)       $q foobaaarba "),             "$[\"aaa\"]");
    assert_eq!(ve("$r($+a)       $q foobaaarba ; `\\\\`"),     "$[\"aaa\"]");
    assert_eq!(ve("$r($+a)       $q foobaaarba ; $\\"),        "$[\"aaa\"]");

    assert_eq!(ve(r#"
        !rx = $r/ (^$+[0123456789]) $*$s + $*$s (^$+[0123456789]) /;
        ? rx["4943 + 32"] \"got:" $\.1 "+" $\.2;
    "#), "\"got:4943+32\"");

    assert_eq!(ve(r#"
        !rx = std:pattern ~ $q/(^$+[0123456789]) $*$s +/ $q/$*$s (^$+[0123456789])/;
        ? rx["4943 + 32"] \"got:" $\.1 "+" $\.2;
    "#), "\"got:4943+32\"");

    assert_eq!(ve(r#"
        !rx = $r/$^foo/;
        rx "foobar"
    "#), "$[\"foo\"]");

    assert_eq!(ve(r#"
        !rx = $r/($^foo)/;
        rx "foobar"
    "#), "$[\"foo\"]");

    assert_eq!(ve(r#"
        !rx = $r/(^$^foo)bar/;
        rx "foobarx"
    "#), "$[\"foobar\",\"foo\"]");

    assert_eq!(ve(r#"
        !res =
            ? ($r< $<*? (^$+[\\/]) * > "foo//\\/foo") {
                std:assert_eq $\.0 "foo//\\/foo";
                $\.1
            };
        res
    "#), "\"//\\\\/\"");
}

#[test]
fn check_tree_match() {
    assert_eq!(ve("type $S(a/b)"), "\"function\"");

    assert_eq!(ve("$S(a/b)  ${a = ${b = 20}}"),               "$[20]");
    assert_eq!(ve("$S(a/b]) ${a = ${b = 20}}"),               "COMPILE ERROR: [1,10:<compiler:s_eval>] Compilation Error: bad selector: error[1,4:<selector>] Unexpected token \']\'. At end of selector at code \']\'");
    assert_eq!(ve("unwrap_err ~ std:selector $q a/b] "),      "\"bad selector: error[1,4:<selector>] Unexpected token \\\']\\\'. At end of selector at code \\\']\\\', selector was: /a/b]/\"");
    assert_eq!(ve("(std:selector $q a/b ) ${a = ${b = 20}}"), "$[20]");
    assert_eq!(ve(r#"
        $S(*/a/^*/^c) $[
            ${ a = $[ ${ c = 10 }, ${ c = 20 }, ${ c = 30 } ] },
            ${ a = $[ ${ c = 11 }, ${ c = 22 }, ${ c = 34 } ] },
            ${ a = $[ ${ c = 12 }, ${ c = 23 }, ${ c = 35 } ] },
            ${ a = $[ ${ c = 13 }, ${ c = 24 }, ${ c = 36 } ] },
        ];
        $\
    "#), "$[$[${c=10},10],$[${c=20},20],$[${c=30},30],$[${c=11},11],$[${c=22},22],$[${c=34},34],$[${c=12},12],$[${c=23},23],$[${c=35},35],$[${c=13},13],$[${c=24},24],$[${c=36},36]]");
    assert_eq!(ve(r#"
        $S(*/a/^*/^d) $[
            ${ a = $[ ${ c = 10 }, ${ c = 20 }, ${ c = 30 } ] },
            ${ a = $[ ${ c = 11 }, ${ c = 22 }, ${ c = 34 } ] },
            ${ a = $[ ${ c = 12 }, ${ c = 23 }, ${ c = 35 } ] },
            ${ a = $[ ${ c = 13 }, ${ c = 24 }, ${ c = 36 } ] },
        ];
        $\
    "#), "$n");
}

#[test]
fn check_iter_var_closure() {
    assert_eq!(ve(r#"
        !sum = 0;
        !v = $[];
        iter i $i(0, 10) {
            std:push v { i };
        };
        v { .sum = sum + _[]; };
        sum
    "#), "0");

    assert_eq!(ve(r#"
        !sum = 0;
        !v = $[];
        !sum2 = $@i
            iter $&&i $i(0, 10) {
                $+ $*i;
                std:push v { i };
            };
        v { .sum = sum + _[]; };
        $p(sum, sum2)
    "#), "$p(45,45)");
}

#[test]
fn check_struct_patterns() {
    assert_eq!(ve("($P x)               10"),           "${x=10}");
    assert_eq!(ve("($P 1.0)             1"),            "$n");
    assert_eq!(ve("($P 1)               1"),            "${}");
    assert_eq!(ve("($P 1)               1.0"),          "$n");
    assert_eq!(ve("($P 1.0)             1.0"),          "${}");
    assert_eq!(ve("($P $p(x, y))        20 => 30"),     "${x=20,y=30}");
    assert_eq!(ve("($P $p(x, 30))       20 => 30"),     "${x=20}");
    assert_eq!(ve("($P $p(x, 40))       20 => 30"),     "$n");
    assert_eq!(ve("($P $p(20, y))       20 => 30"),     "${y=30}");
    assert_eq!(ve("($P $p(:x, y))       :x => 30"),     "${y=30}");

    assert_eq!(ve("($P $i(x, y))        $i(1,2)"),      "${x=1,y=2}");
    assert_eq!(ve("($P $i(x, y, z))     $i(1,2,3)"),    "${x=1,y=2,z=3}");
    assert_eq!(ve("($P $i(x, y, z, w))  $i(1,2,3,4)"),  "${w=4,x=1,y=2,z=3}");

    assert_eq!(ve("($P $i(:x, y, z, w)) $i(1,2,3,4)"),  "$n");
    assert_eq!(ve("($P $i(1, 2, z, w))  $i(1,2,3,4)"),  "${w=4,z=3}");

    assert_eq!(ve("($P $f(1.1, y))          $f(1.1,2)"),      "${y=2}");
    assert_eq!(ve("($P $f(1.1, 2.0, z))     $f(1.1,2,3)"),    "${z=3}");
    assert_eq!(ve("($P $f(1.1, 2, z))       $f(1.1,2,3)"),    "$n");
    assert_eq!(ve("($P $f(1.1, 2.0, z))     $f(1.1,2,3,4)"),  "$n");
    assert_eq!(ve("($P $f(1.1, 2.0, z, w))  $f(1.1,2,3,4)"),  "${w=4,z=3}");
}

#[test]
fn check_struct_match() {
    assert_eq!(ve("match $i(1,2,3,4)"),                             "COMPILE ERROR: [1,7:<compiler:s_eval>] Compilation Error: match takes at least 2 arguments");
    assert_eq!(ve("match $i(1,2,3,4) 20 30"),                       "COMPILE ERROR: [1,7:<compiler:s_eval>] Compilation Error: match argument 2 is not a pair: 20");
    assert_eq!(ve("match $i(1,2,3,4) $i(1,2,z,w) { $[z,w] };"),     "COMPILE ERROR: [1,7:<compiler:s_eval>] Compilation Error: match argument 2 is not a pair: $[&IVec,1,2,$[&Var,:z],$[&Var,:w]]");

    assert_eq!(ve("match $i(1,2,3,4) 30"),                                  "30");
    assert_eq!(ve("match $i(1,2,3,4) $i(1,2,z,w) => { $[$\\.z,$\\.w] };"),  "$[3,4]");
    assert_eq!(ve("match $i(1,2)     $i(1,:s)    => 11 42;"),               "42");
//    assert_eq!(ve("match $i(1,2)     $i(1,:s)    => 11 0 => 42;"),          "$n");
}

#[test]
fn check_jump_table() {
    assert_eq!(ve("jump -1 10 30"), "30");
    assert_eq!(ve("jump 2  10 30"), "30");
    assert_eq!(ve("jump 0  10 30"), "10");
    assert_eq!(ve("jump 1  10 30"), "30");
    assert_eq!(ve("(jump 1  10 20 30) + 11"), "31");
    assert_eq!(ve("$@v iter i -1 => 4 { $+ ~ jump i { !x = 10; str x } { 20 } 30 }"),
               "$[30,\"10\",20,30,30]");
}
