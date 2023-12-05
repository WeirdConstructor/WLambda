// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use std::cell::RefCell;
use std::rc::Rc;
use wlambda::*;

pub fn v(s: &str) -> String {
    let mut ctx = EvalContext::new_default();
    match ctx.eval(s) {
        Ok(v) => v.s(),
        Err(e) => format!("{}", e),
    }
}

pub fn v2s(s: &str) -> String {
    let mut ctx = EvalContext::new_default();
    match ctx.eval(s) {
        Ok(v) => v.with_s_ref(|s| String::from(s)),
        Err(e) => format!("{}", e),
    }
}

fn ve(s: &str) -> String {
    wlambda::compiler::test_eval_to_string(s)
}

#[test]
fn check_function_string_rep() {
    assert_eq!(ve("!upv1 = \"lol!\"; str {|1<3| !x = 1; !g = 2; upv1 }"),
               "\"&F{@<compiler:s_eval>:1:21 Func[upv1],amin=1,amax=3,locals=2,upvalues=$[$&\\\"lol!\\\"]}\"");
    assert_eq!(ve("!upv1 = $&& \"lol!\"; str {|1<3| !x = 1; !g = 2; upv1 }"),
               "\"&F{@<compiler:s_eval>:1:23 Func[upv1],amin=1,amax=3,locals=2,upvalues=$[$&$&&\\\"lol!\\\"]}\"");
    assert_eq!(
        ve("!upv1 = \"lol!\"; {|1<3| !x = 1; !g = 2; upv1 }"),
        "&F{@<compiler:s_eval>:1:17 Func[upv1],amin=1,amax=3,locals=2,upvalues=$[$&\"lol!\"]}"
    );
    assert_eq!(
        ve("!upv1 = $&& \"lol!\"; {|1<3| !x = 1; !g = 2; upv1 }"),
        "&F{@<compiler:s_eval>:1:19 Func[upv1],amin=1,amax=3,locals=2,upvalues=$[$&$&&\"lol!\"]}"
    );
}

#[test]
fn check_pick() {
    assert_eq!(ve("pick $t :v :h"), ":v");
    assert_eq!(ve("pick $f :v :h"), ":h");
}

#[test]
fn check_simple_indexing() {
    assert_eq!(ve("$[1,2].0"), "1");
    assert_eq!(ve("$[$[3,2],2].0.0"), "3");
    assert_eq!(ve("$[$[$[5,6],2],2].0.0.0"), "5");
    assert_eq!(ve("${a=30}.a"), "30");
    assert_eq!(ve("${a=${b=31}}.a.b"), "31");
    assert_eq!(ve("${a=${b=${c=32}}}.a.b.c"), "32");
    assert_eq!(ve("${a=30}.(\"\" \"a\")"), "30");
    assert_eq!(ve("${a=${b=31}}.(\"\" \"a\").b"), "31");
    assert_eq!(ve("${a=${b=${c=32}}}.(\"\" \"a\").b.c"), "32");
    assert_eq!(
        ve(r"
        !o = ${ b = 10 };
        !o = $[10];
        !o2 = ${ b = 10 };
        o2"),
        "${b=10}"
    );
}

#[test]
fn check_list_boolean_indexing() {
    assert_eq!(ve("$[\"hi\", \"there\"].$t"), ve("pick $f \"hi\" \"there\""));
    assert_eq!(ve("$[94, 38].(is_vec $[])"), "38");
    assert_eq!(ve("{ !l = $[]; range 10 20 5 { std:push l _ }; l }[].$t"), "15");
    assert_eq!(ve("{ !l = $[]; range 10 20 5 { std:push l _ }; l }[].(is_none 1)"), "10");
    assert_eq!(ve("$[$[1, 2], $[3, 4]] $t"), "4");
    assert_eq!(ve("$[$[1, 2], $[3, 4]] $f"), "3");
    assert_eq!(ve("$@v$[$[1, 2], $[3, 4]] \\_|$t|$+"), "$[2,4]");
    assert_eq!(ve("$@v$[$[1, 2], $[3, 4]] \\_|$f|$+"), "$[1,3]");
    assert_eq!(ve("$f $[:a, :b]"), ":a");
    assert_eq!(ve("$t $[:a, :b]"), ":b");
}

#[test]
fn check_trivial() {
    assert_eq!(ve("_"), "10"); // XXX: in test env
    assert_eq!(ve("_1"), "14.4"); // XXX: in test env
    assert_eq!(ve("@"), "$[10,14.4]"); // XXX: in test env

    assert_eq!(ve("$n"), "$n");
    assert_eq!(ve("10"), "10");
    assert_eq!(ve("10; 20; 30"), "30");
    assert_eq!(ve("!x = 10; x"), "10");
    assert_eq!(ve("!x = $true; x"), "$true");
    assert_eq!(ve("{ 10 }"), "&F{@<compiler:s_eval>:1:1 Func,amin=0,amax=0,locals=0,upvalues=$[]}");
    assert_eq!(ve("{ 10 }[]"), "10");
    assert_eq!(ve("{ 10; 20 }[]"), "20");
    assert_eq!(ve("!x = $&11; { 12; x }[]"), "11");
    assert_eq!(ve("!x = 11; { 12; x }[]"), "11");
    assert_eq!(ve("!x = 13; { .x = 12 }[]; { x }[] "), "12");
    assert_eq!(ve("!x = 13; { .x = 12 }[]; $[{ x }[], x]"), "$[12,12]");
    assert_eq!(ve("!x = 13; { .x = 12; x }[]; $[{ x }[], { .x = 15; x }[], x]"), "$[12,15,15]");
    assert_eq!(ve("{ _ } 10"), "10");
    assert_eq!(ve("!y = 0; { .y = _ } 10; y"), "10");
    assert_eq!(ve("${:a = 10, :b = 20}"), "${a=10,b=20}");
    assert_eq!(ve("${:b = 20, :a = 10}"), "${a=10,b=20}");
    assert_eq!(ve("${a = 10, b = 20}"), "${a=10,b=20}");
    assert_eq!(ve("${b = 20, a = 10}"), "${a=10,b=20}");
    assert_eq!(ve("${(:a) = 10, b = 20}"), "${a=10,b=20}");
    assert_eq!(ve("${(:b) = 20, a = 10}"), "${a=10,b=20}");
    assert_eq!(ve("!x = ${:b = 20, :a = 10}; x"), "${a=10,b=20}");
    assert_eq!(ve("!x = ${:b = 20, :a = 10}; x.a"), "10");
    assert_eq!(ve("!x = ${:b = 20, :a = 11}; :a x"), "11");
    assert_eq!(ve("!x = ${}; x.a = 12; x.a"), "12");
    assert_eq!(ve("!x = ${}; x.a = 12; x"), "${a=12}");

    assert_eq!(ve("$[33,44,55].2"), "55");
    assert_eq!(ve("$[33,44,55].0"), "33");
    assert_eq!(ve("$[33,44,55].3"), "$n");
    assert_eq!(ve("1 $[33,44,55]"), "44");
}

#[test]
fn check_ref_closures() {
    assert_eq!(ve("!c1 = { !a = $&& 1.2; { $*a } }; c1[][]"), "1.2");
    assert_eq!(ve("!c1 = { !a = $& 1.2; { a } }; c1[][]"), "1.2");
    assert_eq!(ve("!c1 = { !a = $& 1.2; { a }[] }; c1[]"), "1.2");
    assert_eq!(ve("!c1 = { !a = $& 1.2; !a = $n; { a }[] }; c1[]"), "$n");
    assert_eq!(ve("!outer_a = $&2.3; !c1 = { !a = $&&1.2; { $*a + outer_a } }; c1[][]"), "3.5");
    assert_eq!(ve("!outer_a = $&2.3; !c1 = { !a = $&1.2; !a_weak = std:ref:weaken $:a; { outer_a + a_weak } }; c1[][]"), "2.3");
    assert_eq!(ve("!o_a = $&2.3; !outer_a = std:ref:weaken $:o_a; !c1 = { !a = $&1.2; !a_weak = std:ref:weaken $:a; { outer_a + a_weak } }; .o_a = $n; c1[][]"), "0");
    assert_eq!(
        ve(r"
        !x = $&$[1,2,3];
        !y = $&&$[1,2,3];
        !z = std:ref:weaken y;
        $[$@v x \$+ _ * 2, $@i y \$+ _ * 2, z \_ * 2]
    "),
        "$[$[2,4,6],12,6]"
    );
}

#[test]
fn check_arithmetics() {
    assert_eq!(ve("12 + 23"), "35");
    assert_eq!(ve("+[12, 23]"), "35");
    assert_eq!(ve("+ 12 23"), "35");
    assert_eq!(ve("+ 12 ~ - 24 23"), "13");
    assert_eq!(ve("(+ 12 ~ - 24 23) + 1"), "14");
    assert_eq!(ve("(12 + 1) == 13"), "$true");
    assert_eq!(ve("(+ 12 ~ - 24 23) == 13"), "$true");
    assert_eq!(ve("(+ 12 ~ - 24 23) == 14"), "$false");
    assert_eq!(ve("12.12 + 23.23"), "35.35");

    // coertion of strings and keys to numbers:
    assert_eq!(ve(":10 + :20"), "30");
    assert_eq!(ve(":-10 + :20"), "10");

    assert_eq!(ve("12 - 23"), "-11");
    assert_eq!(ve("5 * 4"), "20");
    assert_eq!(ve("20 / 5"), "4");

    assert_eq!(ve("6 - 3 * 2"), "0");
    assert_eq!(ve("12 / 6 - 3 * 2"), "-4");
}

#[test]
fn check_unary_plus_minus() {
    assert_eq!(ve("!x = 10; -x"), "-10");
    assert_eq!(ve("!x = 10; +x"), "10");
    assert_eq!(ve("- (+ 0xF)"), "-15");
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
    assert_eq!(ve("!x = $& 10; { .x = x + _; x } 5"), "15");
    assert_eq!(ve("!x = $& 10; { .x = { x + 11 + _ }(2) + _; x } 5"), "28");
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
    assert_eq!(
        ve(r"
        !y = 0;
        range 0 10 1 {
            .y = y + _;
            next[];
            .y = y + 100;
        };
        y
    "),
        "55"
    );

    assert_eq!(
        ve(r"
        !x = 0;
        is_none ~ while { x < 10 } {
            .x = x + 1;
            1 2 3 4 next[];
        };
    "),
        "$true"
    );

    assert_eq!(
        ve(r"
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
    "),
        "$p(10,55)"
    );
}

#[test]
fn check_while() {
    assert_eq!(
        ve(r#"
        !x = $& 0;
        while { x == 0 } {
            .x = x + 1;
        };
        $*x
    "#),
        "1"
    );

    assert_eq!(
        ve(r#"
        !x = $&0;
        while { x < 2 } {
            .x = x + 1;
            next[];
            .x = x + 100;
        };
        $*x
    "#),
        "2"
    );

    assert_eq!(
        ve(r#"
        !x = $&0;
        while { x < 2 } {
            !k = std:to_drop {|| .x = 99; };
            .x = x + 1;
            next[];
        };
        $*x
    "#),
        "99"
    );

    assert_eq!(
        ve(r#"
        !x = $&0;
        while { x < 2 } {
            !k = std:to_drop {|| .x = 89; };
            .x = x + 1;
        };
        $*x
    "#),
        "89"
    );

    assert_eq!(
        ve(r#"
        !x = $&0;
        while { x == 0 } {
            .x = 20;
            break[];
            .x = x + 1;
        };
        x
    "#),
        "20"
    );

    assert_eq!(
        ve(r#"
        !i = 0;

        while $true {
            (i >= 4) break;
            .i = i + 1;
        };

        i
    "#),
        "4"
    );

    assert_eq!(
        ve(r#"
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
    "#),
        "$[$p(1,3),$p(1,2),$p(3,3),$p(3,2)]"
    );

    //    assert_eq!(ve("while 1"),
    //        "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: while takes exactly 2 arguments (condition and expression)");
    //    assert_eq!(ve("while 1 2 3"),
    //        "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: while takes exactly 2 arguments (condition and expression)");
}

#[test]
fn check_args() {
    assert_eq!(ve("{ $[_, _1, _2] }[1, 2, 3]"), "$[1,2,3]");
    assert_eq!(ve("{ @ }[1, 2, 3]"), "$[1,2,3]");
    assert_eq!(ve("{|3<4| $[_, _1, _2, _3] }[1, 2, 3]"), "$[1,2,3,$n]");
}

#[test]
fn check_to_drop() {
    assert_eq!(ve("!x = $&1; { .x = 2; }[]; $*x"), "2");
    assert_eq!(ve("!x = $&1; { !d = { .x = 2; }; d }[][]; $*x"), "2");
    assert_eq!(
        ve(r#"
        !x = $&0;
        { !d = std:to_drop {|| .x = 17; } }[];
        $*x
    "#),
        "17"
    );
    assert_eq!(
        ve(r#"
        !k = 10;
        !j = 20;
        !x = $&0;
        !f = std:to_drop {|| .x = 18; };
        .f = $n;
        $*x
    "#),
        "18"
    );
    assert_eq!(
        ve(r"
        !l = $&0;
        !x = std:to_drop {|| .l = 18; };
        .x = $n;
        $*l
    "),
        "18"
    );
    assert_eq!(
        ve("
        {
            !k = $&0;
            { .k = 20; }[];
            std:to_drop {
              # XXX: only works, because to_drop disables arity checks!
              .k = 10;
              std:displayln :foo;
            };
            $*k
        }[]
    "),
        "10"
    );
    assert_eq!(
        ve("
        !destroyed = $false;

        !MyClass = ${
            new = {
                $&& ${
                    _proto = $self,
                    _data  = $&& ${
                        x = 1
                    },
                    dropper = std:to_drop { .destroyed = $t; },
                }
            },
            inc_x = { $data.x += 1 },
            install_on = {!(callchain) = @;
                !self = $w& $self;
                std:push callchain { self.inc_x[]; };
            },
            install_getter = {!(callchain) = @;
                !data = $w& $data;
                std:push callchain { data.x };
            },
        };

        ## Create instance:
        !my_obj = MyClass.new[];

        my_obj.inc_x[];

        !chain = $[];
        my_obj.install_on     chain;
        my_obj.install_getter chain;

        ## There are now 3 references to 'my_obj':
        ## - my_obj variable
        ## - first callback in chain
        ## - second callback in chain

        std:assert_eq my_obj._data.x 2;
        chain.0[]; # calls my_ocj.inc_x[];
        std:assert_eq my_obj._data.x 3;

        ## Second callback gets x:
        std:assert_eq chain.1[] 3;

        .my_obj = $n; # destroy only strong reference
        destroyed
    "),
        "$true"
    );
}

#[test]
fn check_to_no_arity() {
    assert_eq!(ve("(std:to_no_arity {!(x) = @; $[x, x + 1] })[]"), "$[$n,1]");
}

#[test]
fn check_strengthen() {
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = ${ del = std:to_drop { .dropper = 1; } };
            !f = { .k = $n; };
            .k = $n;
            dropper
        "#),
        "1"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = ${};
            k.d = ${ k = { k }, del = std:to_drop { .dropper = 1; } };
            .k = $n;
            dropper
        "#),
        "1"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = $&${};
            k.d = ${ k = { k }, del = std:to_drop { .dropper = 1; } };
            .k = $n;
            dropper
        "#),
        "1"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = $&&${};
            k.d = ${ k = { k }, del = std:to_drop { .dropper = 1; } };
            .k = $n;
            dropper
        "#),
        "1"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = std:ref:strengthen $&${};
            k.d = ${ k = { k }, del = std:to_drop { .dropper = 1; } };
            .k = $n;
            dropper
        "#),
        "1"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = $&&${};
            k.d = ${ k = $*k, del = std:to_drop { .dropper = 1; } };
            .k = $n;
            dropper
        "#),
        "0"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = std:ref:strengthen $&${};
            k.d = ${ k = k, del = std:to_drop { .dropper = 1; } };
            k.d.k = $n;
            .k = $n;
            dropper
        "#),
        "1"
    );
    assert_eq!(
        ve(r#"
            !dropper = 0;
            !k = std:ref:strengthen $&${ del = std:to_drop { .dropper = 1; } };
            !f = { .k = $n; };
            !r = dropper;
            f[];
            .r = r + dropper;
            r
        "#),
        "1"
    );
}

#[test]
fn check_call_primitives() {
    assert_eq!(ve("13[]"), "13");
    assert_eq!(ve("$t[]"), "$true");
    assert_eq!(ve(":foo"), ":foo");
    assert_eq!(ve("$n[]"),
        "EXEC ERR: Caught Panic: Calling $none is invalid\n        []\n    <compiler:s_eval>:1:3 Call []\n");
}

#[test]
fn check_global_vars() {
    assert_eq!(
        ve("
        !:global x = 120;
        !f = { x };
        !l = f[];
        .x = 30;
        !l2 = f[];
        $[l, l2, x]
    "),
        "$[120,30,30]"
    );
    assert_eq!(
        ve("
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
    "),
        "$[$[3,2],$[5,4],30]"
    );
    assert_eq!(
        ve("
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
    "),
        "$[$[1,122,2],$[3,154,4],154]"
    );
    assert_eq!(
        ve("
        !f = { !:global (a, b, c) = @; };
        .b = 20;
        !x1 = $[a, b, c];
        f[13, 17, 43];
        !x2 = $[a, b, c];
        $[x1, x2]
    "),
        "$[$[$n,20,$n],$[13,17,43]]"
    );
}

#[test]
fn check_and_or() {
    assert_eq!(ve("$t &and $t"), "$true");
    assert_eq!(ve("$f &and $t"), "$false");
    assert_eq!(ve("$t &and 10 &and (1 + 3)"), "4");
    assert_eq!(ve("$t &and 0 &and (1 + 3)"), "0");
    assert_eq!(ve("\"\" &and (4+3) &and (1 + 3)"), "\"\"");
    assert_eq!(
        ve(r"
        !x = 0;
        !inc = { .x = x + 1; x };
        !a = inc[] &and inc[] &and inc[] &and inc[];
        !b = x;
        $p(a,b)
    "),
        "$p(4,4)"
    );

    assert_eq!(ve("$t &or  $t"), "$true");
    assert_eq!(ve("$f &or  $t"), "$true");
    assert_eq!(ve("$f &or  $f"), "$false");
    assert_eq!(ve("$f\n&or\n$f\n&or\n0\n&or\n10"), "10");
    assert_eq!(ve("11\n&or\n$f\n&or\n0\n&or\n10"), "11");
    assert_eq!(ve("$f\n&or\n12\n&or\n0\n&or\n10"), "12");
    assert_eq!(ve("$f\n&or\n\"\"\n&or\n13\n&or\n10"), "13");
    assert_eq!(ve("$f\n&or\n(1 + 2 * 4)\n&or\n13\n&or\n10"), "9");
    assert_eq!(ve("(8 - 2 * 4)\n&or\n(1 + 2 * 4)\n&or\n13\n&or\n10"), "9");

    assert_eq!(
        ve(r"
        !x = 0;
        !inc = { .x = x + 1; x };
        !a = inc[] &or inc[] &or inc[] &or inc[];
        !b = x;
        $p(a,b)
    "),
        "$p(1,1)"
    );
}

#[test]
fn check_ops() {
    assert_eq!(ve("10 < 20"), "$true");
    assert_eq!(ve("11 < 10"), "$false");
    assert_eq!(ve("10 < 10"), "$false");
    assert_eq!(ve("10 > 20"), "$false");
    assert_eq!(ve("11 > 10"), "$true");
    assert_eq!(ve("10 > 10"), "$false");
    assert_eq!(ve("10 <= 20"), "$true");
    assert_eq!(ve("11 <= 10"), "$false");
    assert_eq!(ve("10 <= 10"), "$true");
    assert_eq!(ve("10 >= 20"), "$false");
    assert_eq!(ve("11 >= 10"), "$true");
    assert_eq!(ve("10 >= 10"), "$true");
    assert_eq!(ve("10.1 < 20.4"), "$true");
    assert_eq!(ve("11.2 < 10.2"), "$false");
    assert_eq!(ve("10.3 < 10.4"), "$true");
    assert_eq!(ve("22 == 22"), "$true");
    assert_eq!(ve("22 == 23"), "$false");
    assert_eq!(ve("22 != 22"), "$false");
    assert_eq!(ve("21 != 22"), "$true");

    assert_eq!(ve("2 ^ 2"), "4");
    assert_eq!(ve("2 ^ 3"), "8");
    assert_eq!(ve("2.1 ^ 2"), "4.41");
    assert_eq!(ve("4 ^ 0.5"), "1");
    assert_eq!(ve("4.0 ^ 0.5"), "2");

    assert_eq!(ve("4 % 5"), "4");
    assert_eq!(ve("6 % 5"), "1");
    assert_eq!(ve("4.4 % 5.5"), "4.4");
    assert_eq!(ve("5.5 % 5.5"), "0");

    assert_eq!(ve("std:neg_i64 0xFF"), "-255");
    assert_eq!(ve("std:not_i64 0xFF"), "-256");
    assert_eq!(ve("std:neg_u32 0xFF"), "4294967041");
    assert_eq!(ve("std:neg_u32 0x1"), "4294967295");
    assert_eq!(ve("std:neg_u32 0x0"), "0");
    assert_eq!(ve("std:not_u32 0xFF"), "4294967040");
    assert_eq!(ve("std:not_u32 0x1"), "4294967294");
    assert_eq!(ve("std:not_u32 0x0"), "4294967295");

    assert_eq!(ve("(0x10 &| 0x01) == 0x11"), "$true");
    assert_eq!(ve("(0x0f &  0x33) == 0x3"), "$true");
    assert_eq!(ve("(0x11 &^ 0x01) == 0x10"), "$true");
    assert_eq!(ve("(0b1 << 1) == 0b10"), "$true");
    assert_eq!(ve("(0b1 << 2) == 0b100"), "$true");
    assert_eq!(ve("(0b1 >> 1) == 0x0"), "$true");

    assert_eq!(ve("!x = 0; !b = { $t }[] &and { .x = 1; 10 }[]; $[x, b]"), "$[1,10]");
    assert_eq!(ve("!x = 0; !b = { $f }[] &and { .x = 1; 10 }[]; $[x, b]"), "$[0,$false]");
    assert_eq!(ve("!x = 0; !b = { $f }[] &or { .x = 1; 10 }[]; $[x, b]"), "$[1,10]");
    assert_eq!(ve("!x = 0; !b = { 12 }[] &or { .x = 1; 10 }[]; $[x, b]"), "$[0,12]");
    assert_eq!(
        ve(r#"
        !x = 0;
        !f = { std:displayln[x]; .x = x + 1; x };
        !b = f[]
            &and f[]
            &and f[]
            &and f[]
            &and f[];
        $[x, b]
    "#),
        "$[5,5]"
    );

    assert_eq!(
        ve(r#"
        !c = 0;
        !x = 10;
        while { x > 0 } { .c = c + 1; .x = x - 1; };
        .x = 20;
        while { x > 0 } { .c = c + 1; .x = x - 1; };
        c
    "#),
        "30"
    );

    assert_eq!(ve(r"(7 & 15) >= 10"), "$false");
    assert_eq!(ve(r"7 & (15 >= 10)"), "1");
    assert_eq!(ve(r"7 & 15 >= 10"), "1");
}

#[test]
fn check_destructure_pair() {
    assert_eq!(ve("!(e, m) = $p(1, $f); $p(m, e)"), "$p($false,1)");
    assert_eq!(ve("!(e, m) = $p(1); $p(m, e)"), "$p($n,1)");
    assert_eq!(ve("!(e, m) = $p($n,1); $p(m, m)"), "$p(1,1)");
    assert_eq!(ve("!(e, m) = $p($n,1); $p(e, m)"), "$p($n,1)");
    assert_eq!(ve("!(a, b) = $p($&10, $&20); { .a = 33; }[]; $p(a, b)"), "$p(33,20)");
    assert_eq!(
        ve(r#"
        !x = $&&10;
        !fun = {
            !(a, b) = $p(x, $&&20);
            !ret = $p({ a }, {
                .a = a + $*b;
                std:assert_eq type <& a "integer";
                std:assert_eq type <& b "ref_strong";
            });
            std:assert_eq (type <& a) "ref_strong";
            ret
        }[];
        (1 fun)[];
        x => (0 fun)[]
    "#),
        "$p($&&10,30)"
    );
    assert_eq!(
        ve(r#"
        !x = $&&10;
        !fun = {
            !(a, b) = $p(x, $&&20);
            !ret = $p({ a }, {
                .*a = a + $*b;
                std:assert_eq type <& a "ref_strong";
                std:assert_eq type <& b "ref_strong";
            });
            std:assert_eq (type <& a) "ref_strong";
            ret
        }[];
        (1 fun)[];
        x => (0 fun)[]
    "#),
        "$p($<1=>$&&30,$<1>)"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $p($&10, $&20);
            $p({ $p(a, b) }, { .a = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
        "$p(33,20)"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $p($&10, $&20);
            !(wa, wb) = $p($w& a, $w& b);
            $p({$p(wa, wb)}, { .wa = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
        "$p(33,20)"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $p($&&10, $&&20);
            $p({$p(a, b)}, { .a = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
        "$p(33,$&&20)"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $p($&&10, $&&20);
            $p({$p(a, b)}, { .*a = 33; });
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
        "$p($&&33,$&&20)"
    );

    assert_eq!(ve("!a = 0; !b = 0; .(a, b) = $p(10, 20); $p(a, b)"), "$p(10,20)");

    assert_eq!(ve("!a = 0; !b = 0; .(a, b) = 40; $p(a, b)"), "$p(40,40)");
}

#[test]
fn check_destructure() {
    assert_eq!(ve("!(a, b) = $[10, 20]; $[a, b]"), "$[10,20]");
    assert_eq!(ve("!(a, b) = $[10, 20, 30]; $[a, b]"), "$[10,20]");
    assert_eq!(ve("!(a, b) = $[10]; $[a, b]"), "$[10,$n]");
    assert_eq!(ve("!(a, b) = ${a = 10, b= 20, c=30}; $[a, b]"), "$[10,20]");
    assert_eq!(ve("!(a, b) = ${a = 10}; $[a, b]"), "$[10,$n]");
    assert_eq!(ve("!(a, b) = ${b = 20, c = 30}; $[a, b]"), "$[$n,20]");

    assert_eq!(ve("!(a, b) = $[$&10, $&20]; { .a = 33; }[]; $[a, b]"), "$[33,20]");
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $[$&&10, $&&20];
            $[{ a }, { .a = 33; }];
        }[];
        (1 fun)[];
        !r = (0 fun)[];
        std:assert_eq r &> type "integer";
        $*r
    "#),
        "33"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $[$&&10, $&&20];
            $[{ a }, { .*a = 33; }];
        }[];
        (1 fun)[];
        !r = (0 fun)[];
        std:assert_eq r &> type "ref_strong";
        $*r
    "#),
        "33"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $[$&10, $&20];
            $[{$[a, b]}, { .a = 33; }];
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
        "$[33,20]"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $[$&10, $&20];
            !(wa, wb) = $[$w& a, $w& b];
            $[{ $[wa, wb] }, { .wa = 33; }];
        }[];
        (1 fun)[];
        (0 fun)[]
    "#),
        "$[33,20]"
    );
    assert_eq!(
        ve(r#"
        !fun = {
            !(a, b) = $[$&&10, $&&20];
            $[{$[a, b]}, { .a = 33; }];
        }[];
        (1 fun)[];
        $@v (0 fun)[] \$+ $* _;
    "#),
        "$[33,20]"
    );

    assert_eq!(ve("!a = 0; !b = 0; .(a, b) = $[10, 20]; $[a, b]"), "$[10,20]");

    assert_eq!(ve("!a = 0; !b = 0; .(a, b) = 40; $[a, b]"), "$[40,40]");

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
    assert_eq!(ve("type 12"), "\"integer\"");
    assert_eq!(ve("type 12.2"), "\"float\"");
    assert_eq!(ve("type $n"), "\"none\"");
    assert_eq!(ve("type $[]"), "\"vector\"");
    assert_eq!(ve("type ${}"), "\"map\"");
}

#[test]
fn check_eqv() {
    assert_eq!(ve("1 == 1"), "$true");
    assert_eq!(ve("1 == 2"), "$false");
    assert_eq!(ve("1.1 == 1.1"), "$true");
    assert_eq!(ve("1.1 == 1.2"), "$false");
    assert_eq!(ve("1.0 == 1"), "$false");
    assert_eq!(ve("1.0 == \"1\""), "$false");
    assert_eq!(ve("$true == $true"), "$true");
    assert_eq!(ve("$false == $true"), "$false");
    assert_eq!(ve("$none == $n"), "$true");
    assert_eq!(ve("$none == $false"), "$false");
    assert_eq!(ve("0 == $false"), "$false");
    assert_eq!(ve("\"abc\" == (std:str:cat \"a\" \"bc\")"), "$true");
    assert_eq!(ve("\"abc\" == (std:str:cat \"b\" \"bc\")"), "$false");
    assert_eq!(ve("$[] == $[]"), "$false");
    assert_eq!(ve("$[1,2] == $[1,2]"), "$false");
    assert_eq!(ve("!a = $[1,2]; !b = a; a == b"), "$true");
    assert_eq!(ve("!a = $[1,2]; a == a"), "$true");
    assert_eq!(ve("!a = $[1,2]; a == $[1,2]"), "$false");
    assert_eq!(ve("!a = ${l=3}; !b = a; a == b"), "$true");
    assert_eq!(ve("!a = ${l=2}; a == a"), "$true");
    assert_eq!(ve("!a = ${l=2}; a == a"), "$true");
    assert_eq!(ve("!a = ${l=2}; a == ${l=2}"), "$false");
    assert_eq!(ve(":a == :b"), "$false");
    assert_eq!(ve(":a == :a"), "$true");
    assert_eq!(ve("\"a\" == :a"), "$false");
    assert_eq!(ve("sym[\"a\"] == :a"), "$true");
    assert_eq!(ve("std:str:to_bytes[\"a\"] == $b\"a\""), "$true");
    assert_eq!(ve("$b\"a\" == $b\"a\""), "$true");
    assert_eq!(ve("$b\"b\" == $b\"a\""), "$false");
    assert_eq!(ve("!f = {}; f == {}"), "$false");
    assert_eq!(ve("!f = {}; f == f"), "$true");
    assert_eq!(ve("($e :a) == ($e :b)"), "$false");
    assert_eq!(ve("!e = $e :a; e == e"), "$true");
    assert_eq!(
        ve(r#"
        !r  = $&& $&&0;
        !b  = $&& $&&0;
        !c  = $&& $&&$*r;
        !r2 = $&& r;
        $[r == b, r == c, r2 == r, std:ref:weaken[r] == r, r == std:ref:weaken[r]]
    "#),
        "$[$false,$false,$true,$true,$true]"
    );
    assert_eq!(
        ve(r#"
        !r  = $& $&1;
        !r2 = $&& r;
        r == r2
    "#),
        "$true"
    );
    assert_eq!(
        ve(r#"
        !r  = $& $&1;
        !r2 = $&& r;
        r == r2
    "#),
        "$true"
    );
    assert_eq!(
        ve(r#"
        !r  = $& $&0;
        !b  = $& $&0;
        !c  = $& $&r;
        !r2 = $&& r;
        $[r == b, r == c, r2 == r, std:ref:weaken[r] == r, r == std:ref:weaken[r]]
    "#),
        "$[$false,$false,$true,$true,$true]"
    );
}

#[test]
fn check_string() {
    assert_eq!(ve("\"foo\""), "\"foo\"");
    assert_eq!(ve("$q#foo#"), "\"foo\"");
    assert_eq!(ve("$b\"foo\""), "$b\"foo\"");

    assert_eq!(ve("\"foo\"(0)"), "'f'");
    assert_eq!(ve("\"foo\" 0"), "'f'");
    assert_eq!(ve("\"foobar\" 1 3"), "\"oob\"");
    assert_eq!(ve("\"foobar\"[1, 3]"), "\"oob\"");
    assert_eq!(ve("\"foobar\" $[1, 3]"), "\"oob\"");
    assert_eq!(ve("\"foobar\" $q/xyz/"), "\"foobarxyz\"");
    assert_eq!(ve("\"foobar\" ${ foobar = 12 }"), "12");
    assert_eq!(ve("\"foobar\" ${ (\"foobar\") = 12 }"), "12");
    assert_eq!(ve("\"foobar\" 2 -1"), "\"obar\"");
    assert_eq!(ve("\"\" 2 -1"), "\"\"");
    assert_eq!(ve("\"foobar\" 6 -1"), "\"\"");
    assert_eq!(ve("\"foobar\" 6"), "$n");

    assert_eq!(ve("\"foo\" \"a\" :b $b\"c\""), "\"fooabc\"");
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
    global.borrow_mut().add_func(
        "reg",
        |env: &mut Env, _argc: usize| {
            let fun = env.arg(0);
            env.with_user_do(|v: &mut Vec<VVal>| v.push(fun.clone()));
            Ok(VVal::None)
        },
        Some(1),
        Some(1),
    );

    let reg: Rc<RefCell<Vec<VVal>>> = Rc::new(RefCell::new(Vec::new()));

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
    assert_eq!(
        ve("block {
        !x = { return 11; 20 }[];
        .x = x + 20;
        x
    }"),
        "31"
    );
    assert_eq!(
        ve("block {
        !x = { 13 }[];
        .x = 20;
        x
    }"),
        "20"
    );
    assert_eq!(
        ve("block :x {
        !x = { return :x 10; 20 }[];
        .x = x + 20;
        x
    }"),
        "10"
    );
    assert_eq!(
        ve("\\:x {
            !x = { return :x 10; 20 }[];
            .x = x + 20;
            x
        }[]
    "),
        "10"
    );
    assert_eq!(ve("{ 10; 20 }[]"), "20");
    assert_eq!(ve("!g = { _1 }; g :x 10"), "10");
    assert_eq!(
        ve("block {
        !x = { block :x { return :x 13; 20 } }[];
        .x = x + 12;
        x
    }"),
        "25"
    );
}

#[test]
fn check_arity() {
    assert_eq!(ve("{}[1,2,3]"),
        "EXEC ERR: Caught Panic: function expects at most 0 arguments, got 3\n    <compiler:s_eval>:1:1 Func [1, 2, 3]\n    <compiler:s_eval>:1:3 Call [1, 2, 3]\n");
    assert_eq!(ve("{|3| _1 }[1,2,3]"), "2");
    assert_eq!(ve("{|3| _1 }[2,3]"),
        "EXEC ERR: Caught Panic: function expects at least 3 arguments, got 2\n    <compiler:s_eval>:1:1 Func [2, 3]\n    <compiler:s_eval>:1:10 Call [2, 3]\n");
    assert_eq!(ve("{|3| _1 }[2,3,4,5]"),
        "EXEC ERR: Caught Panic: function expects at most 3 arguments, got 4\n    <compiler:s_eval>:1:1 Func [2, 3, 4, 5]\n    <compiler:s_eval>:1:10 Call [2, 3, 4, 5]\n");
    assert_eq!(ve("{|0<4| _1 }[]"), "$n");
    assert_eq!(ve("{|0<4| _1 }[1]"), "$n");
    assert_eq!(ve("{|0<4| _1 }[1,2]"), "2");
    assert_eq!(ve("{|0<4| _1 }[1,2,3]"), "2");
    assert_eq!(ve("(\\|0<4| _1)[1,2,3]"), "2");
    assert_eq!(ve("{|0<4| _1 }[1,2,3,4]"), "2");
    assert_eq!(ve("{|0<4| _1 }[1,2,3,4,5]"),
        "EXEC ERR: Caught Panic: function expects at most 4 arguments, got 5\n    <compiler:s_eval>:1:1 Func [1, 2, 3, 4, 5]\n    <compiler:s_eval>:1:12 Call [1, 2, 3, 4, 5]\n");
    assert_eq!(ve("{ @ }[1,2,3,4,5]"), "$[1,2,3,4,5]");
    assert_eq!(ve("{|2| @ }[1,2]"), "$[1,2]");
    assert_eq!(ve("{|2| @ }[1]"),
        "EXEC ERR: Caught Panic: function expects at least 2 arguments, got 1\n    <compiler:s_eval>:1:1 Func [1]\n    <compiler:s_eval>:1:9 Call [1]\n");
    assert_eq!(ve("{|2| @ }[1,2,3]"),
        "EXEC ERR: Caught Panic: function expects at most 2 arguments, got 3\n    <compiler:s_eval>:1:1 Func [1, 2, 3]\n    <compiler:s_eval>:1:9 Call [1, 2, 3]\n");

    assert_eq!(ve("{!(a,b,c) = @;}[1,2,3,4]"),
        "EXEC ERR: Caught Panic: function expects at most 3 arguments, got 4\n    <compiler:s_eval>:1:1 Func [1, 2, 3, 4]\n    <compiler:s_eval>:1:16 Call [1, 2, 3, 4]\n");
    assert_eq!(ve("{_3; !(a,b,c) = @; }[1,2,3,4,5]"),
        "EXEC ERR: Caught Panic: function expects at most 4 arguments, got 5\n    <compiler:s_eval>:1:1 Func [1, 2, 3, 4, 5]\n    <compiler:s_eval>:1:21 Call [1, 2, 3, 4, 5]\n");
    assert_eq!(ve("{!(a,b,c) = @; _3 }[1,2,3,4,5]"),
        "EXEC ERR: Caught Panic: function expects at most 4 arguments, got 5\n    <compiler:s_eval>:1:1 Func [1, 2, 3, 4, 5]\n    <compiler:s_eval>:1:20 Call [1, 2, 3, 4, 5]\n");
    assert_eq!(ve("{!(a,b,c) = @; b }[1,2,3]"), "2");
    assert_eq!(ve("{!(a,b,c) = @; _3 }[1,2,3,5]"), "5");
    assert_eq!(ve("{!:global (a,b,c) = @; _3 }[1,2,3,5]"), "5");
    assert_eq!(ve("{!:global (a,b,c) = @; _3 }[1,2,3,4,5]"),
        "EXEC ERR: Caught Panic: function expects at most 4 arguments, got 5\n    <compiler:s_eval>:1:1 Func [1, 2, 3, 4, 5]\n    <compiler:s_eval>:1:28 Call [1, 2, 3, 4, 5]\n");
}

#[test]
fn check_error_fn_pos() {
    assert_eq!(ve(r#"

        !x = {

        };

        !l = { x 10 };
        l[];
    "#),
    "EXEC ERR: Caught Panic: function expects at most 0 arguments, got 1\n    <compiler:s_eval>:3:14 Func[x] [10]\n    <compiler:s_eval>:7:18 Call [10]\n    <compiler:s_eval>:7:14 Func[l] []\n    <compiler:s_eval>:8:10 Call []\n");
}

#[test]
fn check_error() {
    assert_eq!(
        ve("$e 10; 14"),
        "EXEC ERR: Caught Panic: Dropped error value: 10\n    <compiler:s_eval>:1:4 Err 10\n"
    );
    assert_eq!(ve("{ { { { $e 10; 14 }[]; 3 }[]; 9 }[]; 10 }[]"),
        "EXEC ERR: Caught Panic: Dropped error value: 10\n    <compiler:s_eval>:1:12 Err 10\n    <compiler:s_eval>:1:7 Func []\n    <compiler:s_eval>:1:20 Call []\n    <compiler:s_eval>:1:5 Func []\n    <compiler:s_eval>:1:27 Call []\n    <compiler:s_eval>:1:3 Func []\n    <compiler:s_eval>:1:34 Call []\n    <compiler:s_eval>:1:1 Func []\n    <compiler:s_eval>:1:42 Call []\n");
    assert_eq!(
        ve("_? $e 10"),
        "EXEC ERR: Caught Return[lbl=$n] $e 10 [@ <compiler:s_eval>:1:7 Err]"
    );
    assert_eq!(
        ve("_? { return $e 10; 10 }[]"),
        "EXEC ERR: Caught Return[lbl=$n] $e 10 [@ <compiler:s_eval>:1:16 Err]"
    );
    assert_eq!(ve("unwrap $e 1"),
        "EXEC ERR: Caught Panic: unwrap error: 1\n    <compiler:s_eval>:1:11 Err 1\n    <compiler:s_eval>:1:8 Call [$e 1 [@ <compiler:s_eval>:1:11 Err]]\n");
    assert_eq!(ve("unwrap 1.1"), "1.1");
    assert_eq!(ve("on_error {|4| _ + 20 } $e 19.9"), "39.9");

    assert_eq!(ve("{ { { panic 102 }[]; 20 }[]; return 20 }[]; 49"),
        "EXEC ERR: Caught Panic: 102\n    <compiler:s_eval>:1:5 Func $n\n    <compiler:s_eval>:1:13 Call [102]\n    <compiler:s_eval>:1:5 Func []\n    <compiler:s_eval>:1:18 Call []\n    <compiler:s_eval>:1:3 Func []\n    <compiler:s_eval>:1:26 Call []\n    <compiler:s_eval>:1:1 Func []\n    <compiler:s_eval>:1:41 Call []\n");

    assert_eq!(
        ve("
        !x = $&10;
        {
            .x = x + 1;
            block :outer { .x = x + 1; };
            .x = x + 1;
        }[];
        $*x
    "),
        "13"
    );
    assert_eq!(
        ve("
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
    "),
        "$[23,\"something_failed!\"]"
    );
    assert_eq!(
        ve("
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
    "),
        "$[13,\"all ok\"]"
    );

    assert_eq!(ve("{ $e 23 }[] | on_error {|4| _ + 21 }"), "44");

    assert_eq!(ve("!x = $[$e 181];"),
        "EXEC ERR: Caught Panic: Error value in list element: 181\n    <compiler:s_eval>:1:11 Err 181\n");
    assert_eq!(ve("!x = ${a=$e 182};"),
        "EXEC ERR: Caught Panic: Error value in map value: 182\n    <compiler:s_eval>:1:13 Err 182\n");
    assert_eq!(ve("!x = $[]; x.0 = $e 183;"),
        "EXEC ERR: Caught Panic: Error value in map value: 183\n    <compiler:s_eval>:1:20 Err 183\n");
    assert_eq!(ve("!x = ${}; x.a = $e 184;"),
        "EXEC ERR: Caught Panic: Error value in map value: 184\n    <compiler:s_eval>:1:20 Err 184\n");
    assert_eq!(
        ve("!x = $[]; x.($e 185) = 5;"),
        "EXEC ERR: Caught Panic: Error value in map key: 185\n    <compiler:s_eval>:1:17 Err 185\n"
    );
    assert_eq!(
        ve("!x = ${}; x.($e 186) = 4;"),
        "EXEC ERR: Caught Panic: Error value in map key: 186\n    <compiler:s_eval>:1:17 Err 186\n"
    );
}

#[test]
fn check_prelude() {
    assert_eq!(ve("bool $n"), "$false");
    assert_eq!(ve("int $n"), "0");
    assert_eq!(ve("float $q$10.2$"), "10.2");
    assert_eq!(ve("str 10.3"), "\"10.3\"");
    assert_eq!(ve("sym \"foo\""), ":foo");
    assert_eq!(ve("sym 10.4"), ":\"10.4\"");
    assert_eq!(ve("(bool $e :fail) { 10 } { 20 }"), "20");
    assert_eq!(ve("std:fold 1 { _ + _1 } $[1,2,3,4]"), "11");
    assert_eq!(ve("std:take 2 $[1,2,3,4,5,6]"), "$[1,2]");
    assert_eq!(ve("std:drop 2 $[1,2,3,4,5,6]"), "$[3,4,5,6]");
}

#[test]
fn check_oop() {
    assert_eq!(
        ve(r#"
        !oo = 0;
        !new = {
            !self = ${};
            self.x = { self.y[] };
            self.y = { 10 };
            self.k = std:to_drop {|| .oo = 20; }; 
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
    "#),
        "22"
    );
    assert_eq!(
        ve(r#"
        !new = {
            !obj = $&${};
            !self = $w& $:obj;
            obj.add = { self.b + 10 };
            obj.get = { type self };
            obj.set = { self.b = _; };
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
        "$[20,\"map\",20,\"map\",10,\"none\"]"
    );
    assert_eq!(
        ve(r#"
        !new = {
            !obj = $&&${};
            !self = $w& obj;
            obj.add = { self.b + 10 };
            obj.get = { type self };
            obj.set = { self.b = _; };
            obj
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
        "$[20,\"map\",20,\"map\",10,\"none\"]"
    );
    assert_eq!(
        ve(r#"
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
        "$[20,\"map\",20,\"map\",20,\"map\"]"
    );
    assert_eq!(
        ve(r#"
        !obj = ${};
        !x = $&&0;
        obj.a = { .x = 10 };
        obj.a[];
        $*x
    "#),
        "10"
    );
}

#[test]
fn check_test_funs() {
    assert_eq!(ve("is_none $n"), "$true");
    assert_eq!(ve("is_err $e $n"), "$true");
    assert_eq!(ve("is_map ${}"), "$true");
    assert_eq!(ve("is_vec $[]"), "$true");
    assert_eq!(ve("is_sym :f"), "$true");
    assert_eq!(ve("is_bool $n"), "$false");
    assert_eq!(ve("is_bool $f"), "$true");
    assert_eq!(ve("is_bytes $f"), "$false");
    assert_eq!(ve("is_bytes \"f\""), "$false");
    assert_eq!(ve("is_bytes $b\"f\""), "$true");
    assert_eq!(ve("is_bytes $Q'f'"), "$true");
    assert_eq!(ve("is_byte $b'f'"), "$true");
    assert_eq!(ve("is_char 'f'"), "$true");
    assert_eq!(ve("is_str \"f\""), "$true");
    assert_eq!(ve("is_int 1"), "$true");
    assert_eq!(ve("is_float 1.2"), "$true");
    assert_eq!(ve("is_fun {}"), "$true");
}

#[test]
fn check_len_fun() {
    assert_eq!(ve("len $[]"), "0");
    assert_eq!(ve("len $[1,2,3]"), "3");
    assert_eq!(ve("len ${a=1,b=20}"), "2");
    assert_eq!(ve("len ${}"), "0");
    assert_eq!(ve("len 'x'"), "1");
    assert_eq!(ve("len $b'x'"), "1");
    assert_eq!(ve("std:str:len ${}"), "3");
    assert_eq!(ve("len $q abcdef "), "6");
    assert_eq!(ve("len $q abcüdef "), "8");
    assert_eq!(ve("std:str:len $q abcüdef "), "7");
    assert_eq!(ve("len $Q abcdef "), "6");
    assert_eq!(ve("len $Q abcüdef "), "8");
    assert_eq!(ve("std:str:len $Q abcüdef "), "8");
}

#[test]
fn check_lst_map() {
    assert_eq!(ve("$[12,1,30] \\_ * 2"), "60");
    assert_eq!(ve("$@v $[12,1,30] \\$+ _ * 2"), "$[24,2,60]");
    assert_eq!(ve("$@v $[12,1,304] \\std:str:len _ | $+"), "$[2,1,3]");
    assert_eq!(ve("$@v $[123,22,4304] \\std:str:len _ | $+"), "$[3,2,4]");
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
    if cfg!(feature = "chrono") {
        assert_eq!(ve("std:chrono:timestamp $q$%Y$ | int"), "2023");
    }
}

#[test]
fn check_prelude_xml() {
    if cfg!(feature = "quick-xml") {
        assert_eq!(
            ve(r#"
                $@v std:xml:read_sax $q$
                    <?xml version="1.0" encoding="UTF-8"?>
                    <!DOCTYPE test>
                    <x>
                        foo
                        <!-- foo bar -->
                        <![CDATA[
                            Some Stuff Bla
                        ]]>
                        <?do this foobar?>
                        <img/>
                        xxx
                    </x>
                $ {!event = _;
                    $+ ~ match event.0
                        (? :start :end :text :comment :cdata) => {
                            event.0 => std:str:trim <& event.1
                        }
                        event.0;
                };
            "#),
            "$[:decl,:doctype,$p(:start,\"x\"),$p(:text,\"foo\"),$p(:comment,\"foo bar\"),$p(:cdata,\"Some Stuff Bla\"),:pi,:empty,$p(:text,\"xxx\"),$p(:end,\"x\")]");

        assert_eq!(
            ve(r#"
                $@v std:xml:read_sax $q$
                    <x a="10" b="&gt;20">&gt;foo<i xxx="foo"/></x>
                $ $+;
            "#),
            "$[$[:start,\"x\",${a=\"10\",b=\">20\"}],$[:text,\">foo\"],$[:empty,\"i\",${xxx=\"foo\"}],$[:end,\"x\"]]");

        assert_eq!(
            ve(r#" 0 ~ $@v std:xml:read_sax $q$ <?xml version="1.0"?> $ $+; "#),
            "$[:decl,\"1.0\",$n,$n]"
        );
        assert_eq!(
            ve(r#" 0 ~ $@v std:xml:read_sax $q$ <?xml version="1.0" standalone="yes"?> $ $+; "#),
            "$[:decl,\"1.0\",$n,\"yes\"]"
        );
        assert_eq!(
            ve(r#" 0 ~ $@v std:xml:read_sax $q$ <?xml version="1.0" standalone="no"?> $ $+; "#),
            "$[:decl,\"1.0\",$n,\"no\"]"
        );
        assert_eq!(
            ve(
                r#" 0 ~ $@v std:xml:read_sax $q$ <?xml version="1.0" encoding="UTF-8" standalone="yes"?> $ $+; "#
            ),
            "$[:decl,\"1.0\",\"UTF-8\",\"yes\"]"
        );

        assert_eq!(
            v2s(r#"
                !w = std:xml:create_sax_writer[];
                w :start => "test";
                w :start => "test-inner";
                w :end   => "test-inner";
                w :end   => "test";
                w[]
            "#),
            "<test><test-inner></test-inner></test>"
        );

        assert_eq!(
            v2s(r#"
                !w = std:xml:create_sax_writer 2;
                w :start => "test";
                w :start => "test-inner";
                w :end   => "test-inner";
                w :end   => "test";
                w[]
            "#),
            "<test>\n  <test-inner>\n  </test-inner>\n</test>"
        );

        assert_eq!(
            v2s(r#"
                !w = std:xml:create_sax_writer[];
                w $[:start, "test", ${ a = 12.12 }];
                w $[:end, "test"];
                w $[:start, "test", ${ a = "a & b" }];
                w $[:empty, "test", ${ a = "a & b" }];
                w $[:end, "test"];
                w[]
            "#),
            "<test a=\"12.12\"></test><test a=\"a &amp; b\"><test a=\"a &amp; b\"/></test>"
        );

        assert_eq!(
            v2s(r#"
                !w = std:xml:create_sax_writer[];
                w $[:comment, "Wurst <!-- foefoeo --> feofoe"];
                w[]
            "#),
            "<!--Wurst &lt;!-- foefoeo --&gt; feofoe-->"
        );

        assert_eq!(
            v2s(r#"
                !xml = $q$
                    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                    <!DOCTYPE wurst>
                    <h>
                        Hello!
                        <xx:img xx:src="foobar"/>
                        Here: <!-- comment here -->
                        And PI: <?pi something here?>
                        And CDATA: <![CDATA[FOO foe ofe o]]>
                    </h>
                $;
                !writer = std:xml:create_sax_writer[];
                std:xml:read_sax xml writer;
                writer[]
            "#),
            "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><!DOCTYPE wurst><h>Hello!<xx:img xx:src=\"foobar\"/>Here:<!-- comment here -->And PI:<?pi something here?>And CDATA:<![CDATA[FOO foe ofe o]]></h>");

        assert_eq!(
            v2s(r#"
                !xml = $q$
                    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                    <!DOCTYPE wurst>
                    <h>
                        Hello!
                        <xx:img xx:src="foobar"/>
                        Here: <!-- comment here -->
                        And PI: <?pi something here?>
                        And CDATA: <![CDATA[FOO foe ofe o]]>
                    </h>
                $;
                !writer = std:xml:create_sax_writer[];
                std:xml:read_sax xml writer $t;
                writer[]
            "#),
            r#"
                    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                    <!DOCTYPE wurst>
                    <h>
                        Hello!
                        <xx:img xx:src="foobar"/>
                        Here: <!-- comment here -->
                        And PI: <?pi something here?>
                        And CDATA: <![CDATA[FOO foe ofe o]]>
                    </h>
                "#
        );
    }
}

#[test]
fn check_prelude_regex() {
    if cfg!(feature = "regex") {
        assert_eq!(ve("($@v $q$fofoaaaaofefoeaafefeoaaaa$ | std:re:map $q{(a+)} { $+ _.1 }) | std:str:join $q$,$"),
                   "\"aaaa,aa,aaaa\"");
        assert_eq!(
            ve("
            ($@v $q$fofoaaaofefoeaaaaafefeoaaaaaaa$
                 | std:re:map $q{(a+)} { $+ ~ std:str:len _.1 })
            | std:fold 1 \\_ * _1"),
            "105"
        );

        assert_eq!(
            ve("
            !x = $&$n;
            std:re:match $q/(a)\\s+(b)/ $q$a     b$ {
                .x = @;
            };
            $*x"),
            "$[$[\"a     b\",\"a\",\"b\"]]"
        );

        assert_eq!(ve("
            std:re:replace_all $q/ar/ { \"mak\" } $q/foobarbarfoobararar/
        "),
        "EXEC ERR: Caught Panic: function expects at most 0 arguments, got 1\n    <compiler:s_eval>:2:39 Func [$[\"ar\"]]\n    <compiler:s_eval>:2:32 Call [\"ar\", &F{@<compiler:s_eval>:2:39 Func,ami..., \"foobarbarfoobararar\"]\n");
        assert_eq!(ve("
            std:re:replace_all $q/a+r/ { std:str:cat \"mak\" ~ std:str:len _.0 } $q/foobarbaaaarfoobaararar/
        "),
        "\"foobmak2bmak5foobmak3mak2mak2\"");
        assert_eq!(
            ve("
            std:re:replace_all $q/a+r/
                {
                    (std:str:len[_.0] == 3) {
                        break \"XX\"
                    };
                    (std:str:cat \"<\" _.0 \">\")
                }
                $q/foobarbaaaarfoobaararar/
        "),
            "\"foob<ar>b<aaaar>foobXXarar\""
        );
        assert_eq!(
            ve("
            std:re:replace_all $q/a+r/
                {
                    (std:str:len[_.0] == 3) { next[] };
                    (std:str:cat \"<\" _.0 \">\")
                }
                $q/foobarbaaaarfoobaararar/
        "),
            "\"foob<ar>b<aaaar>foobaar<ar><ar>\""
        );
    }
}

#[test]
#[cfg(feature = "serde_json")]
fn check_json() {
    assert_eq!(
        ve("std:ser:json $[1,1.2,$f,$t,$n,${a=1}]"),
        "\"[\\n  1,\\n  1.2,\\n  false,\\n  true,\\n  null,\\n  {\\n    \\\"a\\\": 1\\n  }\\n]\""
    );
    assert_eq!(
        ve("std:ser:json $[1,1.2,$f,$t,$n,${a=1}] $t"),
        "\"[1,1.2,false,true,null,{\\\"a\\\":1}]\""
    );
    assert_eq!(ve("std:deser:json $q$[1,2.3,true,null,{\"a\":10}]$"), "$[1,2.3,$true,$n,${a=10}]");
}

#[test]
#[cfg(feature = "rmp-serde")]
fn check_msgpack() {
    assert_eq!(
        ve("std:deser:msgpack ~ std:ser:msgpack $[1,1.2,$f,$t,$n,${a=1},\"abcä\",$b\"abcä\"]"),
        "$[1,1.2,$false,$true,$n,${a=1},\"abcä\",$b\"abc\\xC3\\xA4\"]"
    );
    assert_eq!(ve("std:ser:msgpack $b\"abc\""), "$b\"\\xC4\\x03abc\"");
    assert_eq!(
        ve("std:ser:msgpack $[1,$n,16.22]"),
        "$b\"\\x93\\x01\\xC0\\xCB@08Q\\xEB\\x85\\x1E\\xB8\""
    );
    assert_eq!(ve("std:deser:msgpack $b\"\\xC4\\x03abc\""), "$b\"abc\"");
    assert_eq!(
        ve("std:deser:msgpack $b\"\\x93\\x01\\xC0\\xCB@08Q\\xEB\\x85\\x1E\\xB8\""),
        "$[1,$n,16.22]"
    );
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
fn check_recursive_eval_ctx() {
    let mut ctx = wlambda::EvalContext::new_default();
    let ctx2 = RefCell::new(ctx.clone());

    let called = Rc::new(RefCell::new(false));
    let called_ = called.clone();
    ctx.set_global_var(
        "recall",
        &VVal::new_fun(
            move |_env, _argc| {
                *called_.borrow_mut() = true;
                let a = ctx2.borrow_mut().eval("!old = x; .x = 333; x").unwrap();
                let b = ctx2.borrow_mut().eval("old").unwrap();
                Ok(VVal::pair(a, b))
            },
            Some(0),
            Some(0),
            false,
        ),
    );

    let r1 = ctx.eval("!:global x = 32; !r = recall[]; !y = x; r").unwrap();
    assert_eq!(r1.v_(0).i(), 333);
    assert_eq!(r1.v_(1).i(), 32);
    let ret = ctx.eval("x").unwrap().i();
    assert_eq!(*called.borrow(), true);
    assert_eq!(ret, 333);

    let ret = ctx.eval("y").unwrap().i();
    assert_eq!(ret, 333);
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
    use std::cell::RefCell;
    use std::rc::Rc;
    let global_env = GlobalEnv::new_default();

    #[derive(Clone, Debug)]
    struct MyType {
        x: Rc<RefCell<(i64, i64)>>,
    }

    impl crate::vval::VValUserData for MyType {
        fn s(&self) -> String {
            format!("$<MyType({:?})>", self.x.borrow())
        }
        fn i(&self) -> i64 {
            self.x.borrow_mut().1
        }
        fn as_any(&mut self) -> &mut dyn std::any::Any {
            self
        }
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
        },
        Some(0),
        Some(0),
    );

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
            } else {
                VVal::None
            })
        },
        Some(1),
        Some(1),
    );

    let mut ctx = crate::compiler::EvalContext::new(global_env);

    let r = &mut ctx
        .eval(
            r#"
        !x = new_mytype[];
        !i = modify_mytype x;
        $[i, x, x.foo, x :foo2, x.foo[99], x[:kkk]]
    "#,
        )
        .unwrap();

    assert_eq!(
        r.s(),
        "$[98,$<MyType((14, 84))>,\"foo\",:foo2,$p(\"foo\",99),:kkk]",
        "Userdata implementation works"
    );
}

#[test]
fn check_bytes_impl() {
    #[cfg(feature = "serde_json")]
    assert_eq!(
        ve("std:ser:json $b\"abc\""),
        "\"[\\n  97,\\n  98,\\n  99\\n]\"",
        "JSON serializer for bytes ok"
    );

    assert_eq!(
        ve("str $b\"abc\""),
        "\"abc\"",
        "Bytes to String by 1:1 Byte to Unicode Char mapping"
    );
    assert_eq!(
        ve("str $b\"äbcß\""),
        "\"Ã¤bcÃ\\u{9f}\"",
        "Bytes to String by 1:1 Byte to Unicode Char mapping"
    );
    assert_eq!(ve("std:str:from_latin1 $b\"Äß\""), "\"Ã\\u{84}Ã\\u{9f}\"", "Bytes from latin1");
    assert_eq!(ve("std:str:from_latin1 $b\"\\xFF\\xF0\""), "\"ÿð\"", "Bytes from latin1");
    assert_eq!(ve("std:str:from_utf8 $b\"äbcß\""), "\"äbcß\"", "Bytes to String from UTF8");
    assert_eq!(
        ve("std:str:from_utf8 $b\"\\xC4\\xC3\""),
        "$e \"str:from_utf8 decoding error: invalid utf-8 sequence of 1 bytes from index 0\"",
        "Bytes to String from invalid UTF8"
    );
    assert_eq!(
        ve("std:str:from_utf8_lossy $b\"\\xC4\\xC3\""),
        "\"��\"",
        "Bytes to String from invalid UTF8 lossy"
    );
    assert_eq!(
        ve("std:str:to_bytes \"aäß\""),
        "$b\"a\\xC3\\xA4\\xC3\\x9F\"",
        "Bytes from String as UTF8"
    );
    assert_eq!(ve("std:str:to_bytes_latin1 \"Äß\""), "$b\"\\xC4\\xDF\"", "Bytes from latin1");
    assert_eq!(ve("std:str:to_bytes_latin1 \"\\u{FE00}\""), "$b\"?\"", "Bytes from latin1");
    assert_eq!(
        ve("std:str:to_bytes_latin1 \"\\u{FF}\\u{F0}\""),
        "$b\"\\xFF\\xF0\"",
        "Bytes from latin1"
    );
    assert_eq!(
        ve("std:str:from_utf8 ~ std:str:to_bytes \"aäß\""),
        "\"aäß\"",
        "Bytes from String as UTF8 into String again"
    );
    assert_eq!(ve("$b\"abc\" 1"), "$b'b'", "Get single byte from bytes");
    assert_eq!(ve("$b\"abcdef\" 0 2"), "$b\"ab\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" 3 3"), "$b\"def\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" $[3, 3]"), "$b\"def\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" $[3]"), "$b\"def\"", "Substring bytes operation");
    assert_eq!(ve("$b\"abcdef\" ${abcdef = 10}"), "10", "Bytes as map key");
    assert_eq!(ve("std:bytes:to_vec $b\"abcdef\""), "$[97,98,99,100,101,102]", "bytes:to_vec");
    assert_eq!(
        ve("std:bytes:from_vec ~ std:bytes:to_vec $b\"abcdef\""),
        "$b\"abcdef\"",
        "bytes:from_vec"
    );
    assert_eq!(ve("std:bytes:from_vec $[]"), "$b\"\"", "bytes:from_vec");
    assert_eq!(ve("std:bytes:from_vec $[1,2,3]"), "$b\"\\x01\\x02\\x03\"", "bytes:from_vec");

    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\""), "\"616263FF\"");
    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\" 6"), "\"616263 FF\"");
    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\" 6 \":\""), "\"616263:FF\"");
    assert_eq!(ve("std:bytes:to_hex $b\"abc\\xFF\" 1 \":\""), "\"6:1:6:2:6:3:F:F\"");

    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\""), "$b\"abc\\xFF\"");
    assert_eq!(ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 6"), "$b\"abc\\xFF\"");
    assert_eq!(
        ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 6 \":\""),
        "$b\"abc\\xFF\""
    );
    assert_eq!(
        ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 1 \":\""),
        "$b\"abc\\xFF\""
    );
    assert_eq!(
        ve("std:bytes:from_hex ~ std:bytes:to_hex $b\"\\x00abc\\xFF\" 1 \":\""),
        "$b\"\\0abc\\xFF\""
    );

    assert_eq!(ve("std:str:to_char_vec $q ABC "), "$[65,66,67]");
    assert_eq!(ve("$q ABC | std:str:to_char_vec | std:str:from_char_vec"), "\"ABC\"");
}

#[test]
fn check_ref() {
    assert_eq!(ve("!:global x = 3; x"), "3");

    assert_eq!(ve("!x = $&&1; $*x"), "1");
    assert_eq!(ve("!x = $&&1; .*x = 2; $*x"), "2");
    assert_eq!(ve("!:global x = $&&1; x"), "$&&1");
    assert_eq!(ve("!:global x = $&&1; $*x"), "1");
    assert_eq!(ve("!:global x = $&&1; .*x = 2; $*x"), "2");
    assert_eq!(ve("!(x, y) = $[$&&1, $&&2]; $[$*x, $*y]"), "$[1,2]");
    assert_eq!(ve("!(x, y) = $[$&&1, $&&2]; $[x, y]"), "$[$&&1,$&&2]");
    assert_eq!(ve("!(x, y) = $[$&&1, $&&2]; .*(x, y) = $[33, 34]; $[x, y]"), "$[$&&33,$&&34]");
    assert_eq!(
        ve(r#"
            !:global (x, y) = $[$&&1, $&&2];
            .*(x, y) = $[33, 34];
            $[x, y]
        "#),
        "$[$&&33,$&&34]"
    );
    assert_eq!(
        ve(r#"
            !(x, y) = $[$&&1, $&&2];
            std:assert_eq x &> type "ref_strong";
            !z = $w& y;
            !f = {
                .*x = $*x + 1;
                .z = z + 2;
                $[x, $:z]
            };
            !r = $[];
            std:push r ~ str f[];
            .x = $n;
            .y = $n;
            std:push r ~ str f[];
            $[r, z]
        "#),
        "$[$[\"$[$&&2,$&&4]\",\"$[1,$&&$n]\"],$n]"
    );
    assert_eq!(
        ve(r#"
        !self = $&&${};
        !wself = $w& self;
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
    "#),
        "$[10,100,0,$n]"
    );
}

#[test]
fn check_set_key() {
    assert_eq!(
        ve(r#"
        !self = ${};
        self.x = 22;
        self.x;
    "#),
        "22"
    );
    assert_eq!(
        ve(r#"
        !self = $&&${};
        self.x = { 23 };
        self.x[];
    "#),
        "23"
    );
}

#[test]
fn check_append_prepend() {
    assert_eq!(ve("std:append 1 2"), "$[1,2]");
    assert_eq!(ve("std:append $[1, 2] 3"), "$[1,2,3]");
    assert_eq!(ve("std:append $[1, 2] 3 4 $[5]"), "$[1,2,3,4,5]");
    assert_eq!(ve("std:append 1 2 3 4 $[5, 6, 7, 8]"), "$[1,2,3,4,5,6,7,8]");
    assert_eq!(ve("std:append 1"), "$[1]");

    assert_eq!(ve("std:prepend 1 2"), "$[2,1]");
    assert_eq!(ve("std:prepend $[1, 2] 3"), "$[3,1,2]");
    assert_eq!(ve("std:prepend $[1, 2] 3 4 $[5]"), "$[5,4,3,1,2]");
    assert_eq!(ve("std:prepend 1 2 3 4 $[5, 6, 7, 8]"), "$[8,7,6,5,4,3,2,1]");
    assert_eq!(ve("std:prepend 1"), "$[1]");
}

#[test]
fn check_apply() {
    assert_eq!(ve("std:str:cat[[$[1,2,3]]]"), "\"123\"");
    assert_eq!(ve("std:assert_eq std:str:cat[[$[1,2,3]]] \"123\""), "$true");
    assert_eq!(ve("!a = $[4,5,6]; std:str:cat[[a]]"), "\"456\"");

    assert_eq!(ve("$[1, 2, 3, 4] &@> `+`"), "10");
    assert_eq!(ve("`+` <@& $[1, 2, 3, 4]"), "10");

    assert_eq!(ve("$[1, 2, 3, 4] &@> std:str:cat"), "\"1234\"");
    assert_eq!(ve("std:str:cat <@& $[1, 2, 3, 4]"), "\"1234\"");

    assert_eq!(ve("($iter 0 => 10) &> std:values &@> std:str:cat"), "\"0123456789\"");
}

#[test]
fn check_call_order() {
    assert_eq!(
        ve(r#"
        !v = $[];
        std:push v ~ ({
            std:push v 1;
            { std:push v $[3, _]; 4 }
        }[])[[$[{ std:push v 2; 3.5 }[]]]];
        v
    "#),
        "$[1,2,$[3,3.5],4]"
    );
    assert_eq!(
        ve(r#"
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
        "$[1,2,3,4,2.5,3.5,4.5,5,6]"
    );
}

#[test]
fn check_cyclic_str_write() {
    assert_eq!(ve(r#"!x = $&&0; .*x = x; x"#), "$<1=>$&&$<1>");
    assert_eq!(ve(r#"!x = $&1;  .x = $:x; x"#), "$<1=>$&&$<1>");
    assert_eq!(ve(r#"!x = $&0;  .*x = $:x; !y = $w& x; y"#), "$<1=>$w&$<1>");
    assert_eq!(
        ve(r#"
        !x = $[1,2];
        !y = ${};
        y.x = x;
        std:push x y;
        x
    "#),
        "$<1=>$[1,2,${x=$<1>}]"
    );
    assert_eq!(
        ve(r#"
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
        "$<1=>$[1,2,${x=$<1>},$<1>,$<2=>$[$<2>,$<1>]]"
    );

    assert_eq!(ve(r#"!x = $[]; std:push x $&&x; $[x.0, x]"#), "$[$<1=>$&&$<2=>$[$<1>],$<2>]");
    assert_eq!(ve(r#"
        !x  = $& ${};
        !x_ = $w& $:x;
        x.f = { x_.b };
        $[x.f, $:x]
    "#),
    "$[$<1=>&F{@<compiler:s_eval>:4:15 Func[f],amin=0,amax=0,locals=0,upvalues=$[$<2=>$w&${f=$<1>}]},$<2>]");

    assert_eq!(
        ve(r#"
        !x = $[];
        std:push x ~ $&$e x;
        x
    "#),
        "$<1=>$[$&$e $<1> [@ <compiler:s_eval>:3:27 Err]]"
    );
}

#[test]
fn check_byte_str_index() {
    assert_eq!(ve("$q$abc$ 0"), "'a'");
    assert_eq!(ve("$q$abc$ 2"), "'c'");
    assert_eq!(ve("0 $q$abc$"), "'a'");
    assert_eq!(ve("2 $q$abc$"), "'c'");
    assert_eq!(ve("$q$abc$.0"), "'a'");
    assert_eq!(ve("$q$abc$.2"), "'c'");
    assert_eq!(ve("$Q$abc$ 0"), "$b'a'");
    assert_eq!(ve("$Q$abc$ 2"), "$b'c'");
    assert_eq!(ve("0 $Q$abc$"), "$b'a'");
    assert_eq!(ve("2 $Q$abc$"), "$b'c'");
    assert_eq!(ve("$Q$abc$.0"), "$b'a'");
    assert_eq!(ve("$Q$abc$.2"), "$b'c'");
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
    assert_eq!(ve("std:num:sqrt   2       | (\\_ * 10000000) | std:num:round"), "14142136");
    assert_eq!(ve("std:num:cbrt   2       | (\\_ * 10000000) | std:num:round"), "12599210");
    assert_eq!(ve("std:num:to_degrees   2 | (\\_ * 10000000) | std:num:round"), "1145915590");
    assert_eq!(ve("std:num:to_radians   2 | (\\_ * 10000000) | std:num:round"), "349066");
    assert_eq!(ve("std:num:tan   2        | (\\_ * 10000000) | std:num:round"), "-21850399");
    assert_eq!(ve("std:num:tanh  2        | (\\_ * 10000000) | std:num:round"), "9640276");
    assert_eq!(ve("std:num:sin   2        | (\\_ * 10000000) | std:num:round"), "9092974");
    assert_eq!(ve("std:num:sinh  2        | (\\_ * 10000000) | std:num:round"), "36268604");
    assert_eq!(ve("std:num:cos   2        | (\\_ * 10000000) | std:num:round"), "-4161468");
    assert_eq!(ve("std:num:cosh  2        | (\\_ * 10000000) | std:num:round"), "37621957");
    assert_eq!(ve("std:num:atan   2       | (\\_ * 10000000) | std:num:round"), "11071487");
    assert_eq!(ve("std:num:atanh  0.5     | (\\_ * 10000000) | std:num:round"), "5493061");
    assert_eq!(ve("std:num:asin   0.5     | (\\_ * 10000000) | std:num:round"), "5235988");
    assert_eq!(ve("std:num:asinh  0.5     | (\\_ * 10000000) | std:num:round"), "4812118");
    assert_eq!(ve("std:num:acos   0.5     | (\\_ * 10000000) | std:num:round"), "10471976");
    assert_eq!(ve("std:num:acosh  2.1     | (\\_ * 10000000) | std:num:round"), "13728591");
    assert_eq!(ve("std:num:ln     200     | (\\_ * 10000000) | std:num:round"), "52983174");
    assert_eq!(ve("std:num:log2   200     | (\\_ * 10000000) | std:num:round"), "76438562");
    assert_eq!(ve("std:num:log10  200     | (\\_ * 10000000) | std:num:round"), "23010300");
    assert_eq!(ve("std:num:exp_m1   2     | (\\_ * 10000000) | std:num:round"), "63890561");
    assert_eq!(ve("std:num:exp      2     | (\\_ * 10000000) | std:num:round"), "73890561");
    assert_eq!(ve("std:num:exp2   10"), "1024");
    assert_eq!(ve("std:num:log   100 3    | (\\_ * 10000000) | std:num:round"), "41918065");
    assert_eq!(ve("std:num:pow   100 3"), "1000000");
    assert_eq!(ve("std:num:pow   100 3.1  | (\\_ * 10000000) | std:num:round"), "15848931924611");
    assert_eq!(ve("std:num:abs   -1.2"), "1.2");
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
    assert_eq!(ve("!sum = $&0; ${a=10, b=20, c=30} {|2| .sum = sum + _; }; $*sum"), "60");
}

#[test]
fn check_int_float_conversion() {
    assert_eq!(ve("std:num:int_to_open01 0"), "0.00000000000000011102230246251565");
    assert_eq!(ve("std:num:int_to_open01 -1"), "0.9999999999999999");
    assert_eq!(ve("std:num:int_to_open_closed01 0"), "0.00000000000000011102230246251565");
    assert_eq!(ve("std:num:int_to_open_closed01 -1"), "1");
    assert_eq!(ve("std:num:int_to_closed_open01 0"), "0");
    assert_eq!(ve("std:num:int_to_closed_open01 -1"), "0.9999999999999999");
}

#[test]
fn check_splitmix64() {
    assert_eq!(
        ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next s
    "),
        "4473449133009263371"
    );
    assert_eq!(
        ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next s 4
    "),
        "$[4473449133009263371,-9009341174627168353,7739434774028954414,-453282142843114385]"
    );
    assert_eq!(
        ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_open01 s 4
    "),
        "$[0.24250616342560194,0.5116026362903058,0.41955559979060253,0.9754275257990305]"
    );
    assert_eq!(
        ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_open_closed01 s 4
    "),
        "$[0.24250616342560205,0.5116026362903059,0.41955559979060264,0.9754275257990306]"
    );
    assert_eq!(
        ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_closed_open01 s 4
    "),
        "$[0.24250616342560194,0.5116026362903058,0.41955559979060253,0.9754275257990305]"
    );
    assert_eq!(
        ve(r"
        !s = std:rand:split_mix64_new_from 120312302310;
        std:rand:split_mix64_next_open01 s
    "),
        "0.24250616342560194"
    );
}

#[test]
fn check_user_obj_macro() {
    use crate::set_vval_method;

    let o = VVal::map();
    let oo = VVal::vec();
    oo.push(VVal::Int(10));
    set_vval_method!(o, oo, get_it, None, None, _env, _argc, {
        Ok(oo.at(0).unwrap_or(VVal::Int(99)))
    });
    set_vval_method!(o, oo, get_it2x, None, None, _env, _argc, {
        Ok(VVal::Int(oo.at(0).unwrap_or(VVal::Int(99)).i() * 2))
    });

    let mut ctx = EvalContext::new_default();
    ctx.set_global_var("O", &o);
    assert_eq!(ctx.eval("O.get_it[]").unwrap().s(), "10");
    oo.set_at(0, VVal::Int(11));
    assert_eq!(ctx.eval("O.get_it2x[]").unwrap().s(), "22");
}

#[test]
fn check_for() {
    assert_eq!(ve("!o = $&$q 1 ; for :XYZ \\.o = _ o; $*o"), "\"ZYX1\"");
    assert_eq!(ve("!r = $&:XYZ; !o = $&$q 1 ; for ($w& r) \\.o = _ o; $*o"), "\"ZYX1\"");
    assert_eq!(ve("!o = $&$q 1 ; for $&:XYZ \\.o = _ o; $*o"), "\"ZYX1\"");
    assert_eq!(ve("!o = $&$q 1 ; for $&&:XYZ \\.o = _ o; $*o"), "\"ZYX1\"");
    assert_eq!(
        ve("!o = $&$q 1 ; for (std:to_drop {||}) \\.o = _ o; $*o"),
        "std:to_drop[&F{@<compiler:s_eval>:1:32 Func[o],amin=any,amax=any,locals=0,upvalues=$[]}]"
    );
    assert_eq!(ve("!o = $&$q 1 ; for \"XYZ\" \\.o = _ o; $*o"), "\"ZYX1\"");
    assert_eq!(ve("!o = $&$b\"L\"; for $b\"@XZ\" \\.o = _ o; $*o"), "$b\"ZX@L\"");
    assert_eq!(ve("!o = $&0; for $[1,2,3] \\.o = o + _; $*o"), "6");
    assert_eq!(
        ve(r"
            !x = $&0;
            !o = $&0;
            for ${a=3, b=2, c=4} {
                .x = x + (@ | 1 | std:bytes:to_vec | 0);
                .o = _ + o;
            };
            $[x, o]
        "),
        "$[294,9]"
    );
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
    assert_eq!(
        ve(r"
        !sm = std:rand:split_mix64_new_from 1234;
        std:shuffle { std:rand:split_mix64_next sm }
            $[1,2,3,4,5,6,7,8];
    "),
        "$[2,1,7,4,8,5,3,6]"
    );
}

#[test]
fn check_sort() {
    assert_eq!(ve("std:sort std:cmp:str:asc  $[:c, :x, :a, :b]"), "$[:a,:b,:c,:x]");
    assert_eq!(ve("std:sort std:cmp:str:desc $[:c, :x, :a, :b]"), "$[:x,:c,:b,:a]");
    assert_eq!(ve("std:sort std:cmp:str:asc  $[3, 2, 5, 9, 0, -1]"), "$[-1,0,2,3,5,9]");
    assert_eq!(ve("std:sort std:cmp:str:desc $[3, 2, 5, 9, 0, -1]"), "$[9,5,3,2,0,-1]");
    assert_eq!(ve("std:sort std:cmp:num:asc  $[3, 2, 5, 9, 0, -1]"), "$[-1,0,2,3,5,9]");
    assert_eq!(ve("std:sort std:cmp:num:desc $[3, 2, 5, 9, 0, -1]"), "$[9,5,3,2,0,-1]");

    assert_eq!(ve("std:sort std:cmp:num:asc $[16 => 100, 16 => 90]"), "$[$p(16,100),$p(16,90)]");
}

#[test]
fn check_copy() {
    assert_eq!(
        ve("!a = $[1,2,3]; a.0 = 10; !b = std:copy a; b.1 = 20; $[a,b]"),
        "$[$[10,2,3],$[10,20,3]]"
    );
    assert_eq!(
        ve("!a = ${a=1}; a.a = 10; !b = std:copy a; b.a = 20; $[a,b]"),
        "$[${a=10},${a=20}]"
    );
}

#[test]
fn check_borrow_error() {
    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { x.1 = _; }
    "),
    "EXEC ERR: Caught Panic: Can't mutate borrowed value: $[1,2,3]\n    <compiler:s_eval>:3:11 Func[x] [1]\n    <compiler:s_eval>:3:11 Call [&F{@<compiler:s_eval>:3:11 Func[x],...]\n");

    assert_eq!(ve(r"
        !x = ${a=1};
        x { x.a = $[_, _1]; }
    "),
    "EXEC ERR: Caught Panic: Can't mutate borrowed value: ${a=1}\n    <compiler:s_eval>:3:11 Func[a] [1, \"a\"]\n    <compiler:s_eval>:3:11 Call [&F{@<compiler:s_eval>:3:11 Func[a],...]\n");

    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { std:prepend x $[_] }
    "),
    "EXEC ERR: Caught Panic: Can't mutate borrowed value: $[1,2,3]\n    <compiler:s_eval>:3:25 Call [$[1,2,3], $[1]]\n    <compiler:s_eval>:3:11 Func[x] [1]\n    <compiler:s_eval>:3:11 Call [&F{@<compiler:s_eval>:3:11 Func[x],...]\n");

    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { std:append x $[_] }
    "),
    "EXEC ERR: Caught Panic: Can't mutate borrowed value: $[1,2,3]\n    <compiler:s_eval>:3:24 Call [$[1,2,3], $[1]]\n    <compiler:s_eval>:3:11 Func[x] [1]\n    <compiler:s_eval>:3:11 Call [&F{@<compiler:s_eval>:3:11 Func[x],...]\n");

    assert_eq!(ve(r"
        !x = $[1,2,3];
        x { std:take 2 x; _ }
    "),
    "EXEC ERR: Caught Panic: Can't mutate borrowed value: $[1,2,3]\n    <compiler:s_eval>:3:22 Call [2, $[1,2,3]]\n    <compiler:s_eval>:3:11 Func[x] [1]\n    <compiler:s_eval>:3:11 Call [&F{@<compiler:s_eval>:3:11 Func[x],...]\n");
}

#[test]
fn check_test_import() {
    assert_eq!(
        ve(r"
        !@import x tests:test_mod_r1;
        x:f[10]
    "),
        "40"
    );
    assert_eq!(
        ve(r"
        !@import x tests:test_paths_mod;
        x:xxx[]
    "),
        "123"
    );
}

#[test]
fn check_test_module_imp_wlambda() {
    assert_eq!(
        ve(r"
        !@import x tests:test_mod_r2;
        x:s 1.4 => $[:x];
    "),
        "\"$p(1.4,$[:x])\""
    );
    assert_eq!(
        ve(r"
        !@import x tests:test_mod_r3;
        $[x:s 1.4 => $[:x], x:s2 1.4 => $[:x]]
    "),
        "$[\"$p(1.4,$[:x])\",\"$p(1.4,$[:x])\"]"
    );
}

#[test]
fn check_field_access() {
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1"), "$[1,2,$[1,2,3]]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1.2"), "$[1,2,3]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1.2.3"), "$n");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].1.2.2"), "3");
    assert_eq!(ve("2 ~ 2 ~ 1 $[1,$[1,2,$[1,2,3]]]"), "3");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].(0 + 1)"), "$[1,2,$[1,2,3]]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].(0 + 1).(1 + 1)"), "$[1,2,3]");
    assert_eq!(ve("$[1,$[1,2,$[1,2,3]]].(0 + 1).(1 + 1).(1 + 1)"), "3");
    assert_eq!(ve("${a=${b=${c=9}}}.a"), "${b=${c=9}}");
    assert_eq!(ve("${a=${b=${c=9}}}.a.b"), "${c=9}");
    assert_eq!(ve("${a=${b=${c=9}}}.a.b.c"), "9");
    assert_eq!(ve("${a=${b=${c=9}}}.\"a\".\"b\".c"), "9");
    assert_eq!(ve("${a=${b=${c=9}}}.a.\"b\".c"), "9");
    assert_eq!(ve("${a=${b=${c=9}}}.\"a\".\"b\".\"c\""), "9");
    assert_eq!(ve("${a=${b=${c=9}}}.(\"a\" \"\").\"b\".\"c\""), "9");
    assert_eq!(ve("${a=${b=${c=9}}}.(\"a\" \"\").(\"b\" \"\").(\"c\" \"\")"), "9");
}

#[test]
fn check_method_calls() {
    // Idiomatic class making:
    assert_eq!(
        ve(r"
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
    "),
        "440"
    );
    assert_eq!(ve("!v = $&${ _proto = ${ a = { 11 } } }; v.(\"\" \"a\")[]"), "11");
    assert_eq!(ve("!v = $&${ _proto = ${ a = { 10 } } }; v.a[]"), "10");

    // Simple vector call table access still works as usual:
    assert_eq!(ve("!v = $[{ _ }]; v.0 20"), "20");
    assert_eq!(ve("!v = $[1,2,$[10,{ _ }]]; v.2.1 20"), "20");
    assert_eq!(ve("!v = $[$[1,2,$[10,{ _ }]]]; v.0.2.1 20"), "20");
    assert_eq!(ve("!v = $[$[$[1,2,$[10,{ _ }]]]]; v.0.0.2.1 20"), "20");

    // Does it work on references?
    assert_eq!(
        ve(r"
        !class = ${ a = { 10 }};
        !v = $&${_proto = class};
        v.a[]
    "),
        "10"
    );

    // Does it work with arrays?
    assert_eq!(
        ve(r"
        !class = ${ a = { 11 }};
        !v = $[class];
        v.a[]
    "),
        "11"
    );

    // Does it work with arrays and $data
    assert_eq!(
        ve(r"
        !class = ${ a = { $s.b[] * $d.x }, b = { 10 } };
        !v = $[class, ${ x = 10 }];
        v.a[]
    "),
        "100"
    );

    // Does it work with $data?
    assert_eq!(
        ve(r"
        !class = ${ a = { $s.b[] * $d.x }, b = { 10 } };
        !v = ${ _proto = class, _data = ${ x = 11 } };
        v.a[]
    "),
        "110"
    );

    // Idiomatic class making:
    assert_eq!(
        ve(r"
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
    "),
        "440"
    );

    // Idiomatic class making and recursive $self access:
    assert_eq!(
        ve(r"
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
    "),
        "460"
    );

    assert_eq!(
        ve(r"
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
    "),
        "461"
    );

    assert_eq!(
        ve(r"
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
    "),
        "22002"
    );

    // Access by symbol/string
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ y = 99, a = { .x = $[$self.y, _] } };
        o.a 10;
        $*x
    "),
        "$[99,10]"
    );
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } };
        o.b.a 10;
        $*x
    "),
        "$[99,10]"
    );
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } };
        o.c.b.a 10;
        $*x
    "),
        "$[99,10]"
    );

    // Access by string
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ d = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } } };
        o.d.c.b.($q$a$) 10;
        $*x
    "),
        "$[99,10]"
    );

    // Access by Key/Call
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ y = 99, a = { .x = $[$self.y, _] } };
        o.($q$a$ $q$$) 11;
        $*x
    "),
        "$[99,11]"
    );
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ y = 99, a = { .x = $[$self.y, _] } };
        !mkkey = { :a };
        o.(mkkey[]) 11;
        $*x
    "),
        "$[99,11]"
    );
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } };
        o.b.($q$a$ $q$$) 11;
        $*x
    "),
        "$[99,11]"
    );
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } };
        o.c.b.($q$a$ $q$$) 11;
        $*x
    "),
        "$[99,11]"
    );
    assert_eq!(
        ve(r"
        !x = $&$n;
        !o = ${ d = ${ c = ${ b = ${ y = 99, a = { .x = $[$self.y, _] } } } } };
        o.d.c.b.($q$a$ $q$$) 11;
        $*x
    "),
        "$[99,11]"
    );

    // Prototyped inheritance:
    assert_eq!(
        ve(r"
        !x = $&$n;
        !class = ${ a = { .x = $[$self.y, _] } };
        !o = ${ y = 99, _proto = class };
        o.a 10;
        $*x
    "),
        "$[99,10]"
    );

    // Prototyped multi level inheritance:
    assert_eq!(
        ve(r"
        !x = $&$n;
        !super = ${ a = { .x = $[$self.y, _, $self.b[]] } };
        !class = ${ _proto = super, b = { 13 } };
        !o = ${ y = 99, _proto = class };
        o.a 14;
        $*x
    "),
        "$[99,14,13]"
    );
}

#[test]
fn capture_ref_semantics() {
    assert_eq!(ve(r" !x = $& 10;   x "), "10");
    assert_eq!(ve(r" !x = $& 10; $*x "), "10");
    assert_eq!(ve(r" !x = $& 10; $:x "), "$&&10");
    assert_eq!(ve(r" !x = 10;     !y = { x }[]; $[x,  y] "), "$[10,10]");
    assert_eq!(ve(r" !x = 10;     !y = { x }[]; $[$*x, y] "), "$[10,10]");
    assert_eq!(ve(r" !x = 10;     !y = { x }[]; $[$:x, y] "), "$[$&&10,10]");
    assert_eq!(ve(r" !x = $& 10;  !y = { x }[]; $[x,  y] "), "$[10,10]");
    assert_eq!(ve(r" !x = $& 10;  !y = { x }[]; $[$*x, y] "), "$[10,10]");
    assert_eq!(ve(r" !x = $& 10;  !y = { x }[]; $[$:x, y] "), "$[$&&10,10]");
    assert_eq!(ve(r" !x = $&& 10; !y = { x }[]; $[x, y] "), "$[$<1=>$&&10,$<1>]");
    assert_eq!(ve(r" !x = $&& 10; !y = { x }[]; $[$*x, y] "), "$[10,$&&10]");
    assert_eq!(ve(r" !x = $&& 10; !y = { x }[]; $[$:x, y] "), "$[$&&$<1=>$&&10,$<1>]");

    assert_eq!(ve(r" !x = $& 10;  !y = std:ref:weaken $:x;   y "), "$n");
    assert_eq!(ve(r" !x = $& 10;  !y = std:ref:weaken $:x; $*y "), "10");
    assert_eq!(ve(r" !x = $& 10;  !y = std:ref:weaken $:x; $:y "), "$&&10");
    assert_eq!(ve(r" !x = $&& 10; !y = std:ref:weaken $:x;   y "), "$n");
    assert_eq!(ve(r" !x = $&& 10; !y = std:ref:weaken $:x; $*y "), "10");
    assert_eq!(ve(r" !x = $&& 10; !y = std:ref:weaken $:x; $:y "), "$&&10");

    assert_eq!(ve("!x = 10; !y = { $:x }[]; .x = 20; y => type <& y"), "$p($&&20,\"ref_strong\")");
    assert_eq!(
        ve("!x = 10; !y = { std:ref:hide $:x }[];       .x = 20; y => type <& y"),
        "$p(20,\"integer\")"
    );
    assert_eq!(
        ve("!x = 10; !y = { std:ref:strengthen $:x }[]; .x = 20; .y = 11; $[x, y]"),
        "$[20,11]"
    );
    assert_eq!(
        ve("!x = 10; !y = { std:ref:strengthen $:x }[]; .x = 20; .*y = 11; $[x, $*y, y]"),
        "$[11,11,$&&11]"
    );

    assert_eq!(ve("!x =     10; !y = $:x; .x  = 11; $[x,y,$:y]"), "$[11,$<1=>$&&10,$<1>]");
    assert_eq!(ve("!x = $&  10; !y = $:x; .x  = 11; $[x,y,$:y]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(ve("!x = $&& 10; !y = $:x; .x  = 11; $[x,y,$:y]"), "$[11,$<1=>$&&10,$<1>]");
    assert_eq!(ve("!x =     10; !y = $:x; .*x = 11; $[x,y,$:y]"), "$[11,$<1=>$&&10,$<1>]");
    assert_eq!(ve("!x = $&  10; !y = $:x; .*x = 11; $[x,y,$:y]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(ve("!x = $&& 10; !y = $:x; .*x = 11; $[x,y,$:y]"), "$[$<1=>$&&11,$<1>,$<1>]");
    assert_eq!(
        ve(r"!x_ = $&& 10; !x = $w& x_;
                                !y = $:x; .*x = 11; $[x,y,$:y]"),
        "$[$<1=>$w&11,$<1>,$<1>]"
    );

    assert_eq!(ve("!x =     10; { !y = $:x; .x  = 11; $[x,y,$:y] }[]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(ve("!x = $&  10; { !y = $:x; .x  = 11; $[x,y,$:y] }[]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(ve("!x = $&& 10; { !y = $:x; .x  = 11; $[x,y,$:y] }[]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(
        ve(r"!x_ = $&& 10; !x = $w& x_;
                                { !y = $:x; .x  = 11; $[x,y,$:y] }[]"),
        "$[11,$<1=>$&&11,$<1>]"
    );
    assert_eq!(ve("!x =     10; { !y = $:x; .*x = 11; $[x,y,$:y] }[]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(ve("!x = $&  10; { !y = $:x; .*x = 11; $[x,y,$:y] }[]"), "$[11,$<1=>$&&11,$<1>]");
    assert_eq!(
        ve("!x = $&& 10; { !y = $:x; .*x = 11; $[x,y,$:y] }[]"),
        "$[$<1=>$&&11,$<2=>$&&$<1>,$<2>]"
    );
    assert_eq!(
        ve(r"!x_ = $&& 10; !x = $w& x_;
                                { !y = $:x; .*x = 11; $[x,y,$:y] }[]"),
        "$[11,$<1=>$&&11,$<1>]"
    );

    assert_eq!(ve("!x =     10; { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&  10; { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(
        ve(r"!x_ = $&& 10; !x = $w& x_;
                                { .x = 13; { !y = $:x; .x  = 11; $[x,y] }[] }[]"),
        "$[11,$&&11]"
    );
    assert_eq!(ve("!x =     10; { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&  10; { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(ve("!x = $&& 10; { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"), "$[11,$&&11]");
    assert_eq!(
        ve(r"!x_ = $&& 10; !x = $w& x_;
                                { .x = 13; { !y = $:x; .*x = 11; $[x,y] }[] }[]"),
        "$[11,$&&11]"
    );
}

#[test]
fn closure_generator() {
    assert_eq!(
        ve(r"
        !mk_gen = {
            !x = $& 0; # strongly captured does keep it alive!
            { .x = x + 1; x }
        };

        !g1 = mk_gen[];
        !g2 = mk_gen[];
        $[g1[], g2[]]
    "),
        "$[1,1]"
    );

    assert_eq!(
        ve(r"
        !mk_gen = {
            !x = 0; # strongly captured does keep it alive!
            { .x = x + 1; x }
        };

        !g1 = mk_gen[];
        !g2 = mk_gen[];
        $[g1[], g2[]]
    "),
        "$[1,1]"
    );

    assert_eq!(
        ve(r"
        !mk_gen = {
            !x = $&& 0; # strongly captured keeps alive!
            { .*x = $*x + 1; $*x }
        };

        !g1 = mk_gen[];
        !g2 = mk_gen[];
        g1[];
        g1[];
        g2[];
        g1[];
        g2[];
        $[g1[], g2[]]
    "),
        "$[4,3]"
    );
}

#[test]
fn ref_semantics() {
    assert_eq!(
        ve(r"
        !dropped = $false;
        !self = ${};
        self.drop = std:to_drop {|| .dropped = $true; };
        self.foo = { self.x = self.x + 1; self.x };
        self.x = 10;
        !ret = self.foo[];
        !ret2 = self.x;
        .self = $n;
        $[ret, ret2, dropped]
    "),
        "$[11,11,$true]"
    );

    assert_eq!(
        ve(r"
        !dropped = $false;
        !self = $n;
        .self = ${
            drop = std:to_drop {|| .dropped = $true; },
            foo  = { self.x = self.x + 1; self.x },
            x    = 10,
        };
        !ret = self.foo[];
        !ret2 = self.x;
        .self = $n;
        $[ret, ret2, dropped]
    "),
        "$[11,11,$true]"
    );

    assert_eq!(
        ve(r"
        !x = 10;
        { .x = 20 }[];
        x
    "),
        "20"
    );
    assert_eq!(
        ve(r"
        !x = $& 10;
        { .x = 20 }[];
        x
    "),
        "20"
    );
    assert_eq!(
        ve(r"
        !x = $&& 10;
        { .x = 20 }[];
        $*x
    "),
        "20"
    );

    assert_eq!(
        ve(r"
        !x = 10;
        { .x = 20 }[];
        .x = x + 1;
        x
    "),
        "21"
    );
    assert_eq!(
        ve(r"
        !x = $& 10;
        { .x = x + 1 }[];
        .x = x + 1;
        x
    "),
        "12"
    );
    assert_eq!(
        ve(r"
        !x = $&& 10;
        { .x = 20 }[];
        .x = $*x + 1;
        $*x
    "),
        "21"
    );

    assert_eq!(
        ve(r"
        !x = 10;
        !f = { .x = x + 1 };
        f[];
        .x = x + 1;
        f[];
        x
    "),
        "13"
    );

    assert_eq!(
        ve(r"
        !x = $& 10;
        !f = { .x = x + 1 };
        f[];
        .x = x + 1;
        f[];
        x
    "),
        "13"
    );

    assert_eq!(
        ve(r"
        !x = $&& 10;
        !f = { .x = x + 1 };
        f[];
        .*x = $*x + 1;
        f[];
        $*x
    "),
        "13"
    );

    assert_eq!(
        ve(r"
        !x = $&& 19;
        !y = std:ref:weaken x;
        !f = { y }; # weak refs are captured and stay weak
        !a = str f[];
        .x = $n;
        !b = str f[];
        std:str:join $q$,$ $[a, b];
    "),
        "\"19,\""
    );
}

#[test]
fn check_trim() {
    assert_eq!(ve("$qX foo X | std:str:trim_start"), "\"foo \"");
    assert_eq!(ve("$qX foo X | std:str:trim_end"), "\" foo\"");
    assert_eq!(ve("$qX foo X | std:str:trim"), "\"foo\"");
    assert_eq!(ve("$qX foo \n X | std:str:trim"), "\"foo\"");
}

#[test]
fn check_accumulator() {
    assert_eq!(ve(r"$@v   $[1,2,3]\$+2*_"), "$[2,4,6]");
    assert_eq!(ve(r"$@vec $[1,2,3]\$+2*_"), "$[2,4,6]");

    assert_eq!(ve(r"$@i   $[1,2,3]\$+_"), "6");
    assert_eq!(ve(r"$@int $[1,2,3]\$+_"), "6");

    assert_eq!(ve(r"$@s      $[1,2,3]\$+_"), "\"123\"");
    assert_eq!(ve(r"$@string $[1,2,3]\$+_"), "\"123\"");

    assert_eq!(ve(r"$@b      $[1,2,3]\$+_"), "$b\"\\x01\\x02\\x03\"");
    assert_eq!(ve(r"$@bytes  $[1,2,3]\$+_"), "$b\"\\x01\\x02\\x03\"");

    assert_eq!(ve(r"std:num:round ~ 10.0 * $@f     $[1.1,2.1,3.1]\$+_"), "63");
    assert_eq!(ve(r"std:num:round ~ 10.0 * $@float $[1.1,2.1,3.1]\$+_"), "63");

    assert_eq!(ve(r"($@m   $[1,2,3]\$+_ 2*_).2"), "4");
    assert_eq!(ve(r"($@map $[1,2,3]\$+_ 2*_).2"), "4");

    assert_eq!(ve("$@s $+10"), "\"10\"");
    assert_eq!(ve("$@s $+10.1"), "\"10.1\"");
    assert_eq!(ve("$@s $+$b\"ABC\""), "\"ABC\"");
    assert_eq!(ve("$@s $+$t"), "\"$true\"");
    assert_eq!(ve("$@s $+$f"), "\"$false\"");
    assert_eq!(ve("$@s $+\"ABC\""), "\"ABC\"");

    assert_eq!(ve("$@b $+10"), "$b\"\\n\"");
    assert_eq!(ve("$@b $+10.1"), "$b\"\\n\"");
    assert_eq!(ve("$@b $+$b\"ABC\""), "$b\"ABC\"");
    assert_eq!(ve("$@b $+$t"), "$b\"\\x01\"");
    assert_eq!(ve("$@b $+$f"), "$b\"\\0\"");
    assert_eq!(ve("$@b $+\"ABC\""), "$b\"ABC\"");

    assert_eq!(
        ve(r"
        $@v { !s = $@s { $+ :a; $+ :b }[]; $+ s; $+ s }[]
    "),
        "$[$<1=>\"ab\",$<1>]"
    );
    assert_eq!(
        ve(r"
        $@v $+ ($@s $[1,2,3] \$+ _)
    "),
        "$[\"123\"]"
    );
    assert_eq!(
        ve(r"
        !f = {
            $@v $+ ($@s $[1,2,3] \$+ _)
        };
        $@m $+ :j f[]
    "),
        "${j=$[\"123\"]}"
    );
    assert_eq!(
        ve(r"
        !f = \:x{
            $@v $+ ($@s
                (return :x 10))
        };
        $@m $+ :l f[]
    "),
        "${l=10}"
    );

    assert_eq!(
        ve(r"
        !res = $[];
        std:push res $@s iter i $i(1, 3) {
            !x = $@@;
            $+ ~ str i;
            std:push res $p(x, $@@);
        };
    "),
        "$[$p(\"\",$<1=>\"1\"),$p($<1>,$<2=>\"12\"),$<2>]"
    );

    assert_eq!(ve(r#"$@i $p(";", 0) "1;3;44;2;4" |> $+"#), "54");
}

#[test]
fn check_return_sp() {
    assert_eq!(
        ve(r"
        !:global g = {|| 11 };
        !:global f = { g (return 10) };
        f[];
    "),
        "10"
    );
    assert_eq!(ve("{|| std:num:log2 (return 12) }[4,5]"), "12");
    assert_eq!(
        ve(r"
        {|| std:num:log2 (+ 5 6) (return 13) (+ 1 2) (+ 3 4)}[4,5]
    "),
        "13"
    );
}

#[test]
fn check_accum() {
    assert_eq!(ve("std:accum $b\"a\" 1"), "$b\"a\\x01\"");
    assert_eq!(ve("std:accum $b\"a\" 2.2"), "$b\"a\\x02\"");
    assert_eq!(ve("std:accum $b\"a\" \"abcde\""), "$b\"aabcde\"");
    assert_eq!(ve("std:accum $b\"a\" $b\"abcde\""), "$b\"aabcde\"");
    assert_eq!(ve("std:accum $b\"a\" $t"), "$b\"a\\x01\"");

    assert_eq!(ve("std:accum \"a\" 1"), "\"a1\"");
    assert_eq!(ve("std:accum \"a\" 2.2"), "\"a2.2\"");
    assert_eq!(ve("std:accum \"a\" \"abcde\""), "\"aabcde\"");
    assert_eq!(ve("std:accum \"a\" $b\"abcde\""), "\"aabcde\"");
    assert_eq!(ve("std:accum \"a\" $t"), "\"a$true\"");

    assert_eq!(ve("std:accum 10 1"), "11");
    assert_eq!(ve("std:accum 11 2.2"), "13");
    assert_eq!(ve("std:accum 12 \"3\""), "15");

    assert_eq!(ve("std:accum 10.2 1"), "11.2");
    assert_eq!(ve("std:num:round 10.0 * (std:accum 11.2 2.1)"), "133");
    assert_eq!(ve("std:accum 12.2 \"3\""), "15.2");

    assert_eq!(ve("std:accum $[1] \"3\""), "$[1,\"3\"]");
    assert_eq!(ve("std:accum $[1] $[2,3]"), "$[1,$[2,3]]");
}

#[test]
fn check_error_reporting_func() {
    assert_eq!(ve("!f = {}; f 10;"),
        "EXEC ERR: Caught Panic: function expects at most 0 arguments, got 1\n    <compiler:s_eval>:1:6 Func[f] [10]\n    <compiler:s_eval>:1:12 Call [10]\n");
    assert_eq!(ve("!x = ${}; x.foo = {}; x.foo 10;"),
        "EXEC ERR: Caught Panic: function expects at most 0 arguments, got 1\n    <compiler:s_eval>:1:19 Func[foo] [10]\n    <compiler:s_eval>:1:29 Call [10]\n");
    assert_eq!(
        ve("!x = ${(foo) = {}}; x.foo 10;"),
        "COMPILE ERROR: <compiler:s_eval>:1:12 Compilation Error: Variable 'foo' undefined"
    );
    assert_eq!(ve("!x = ${foo = {}}; x.foo 10;"),
        "EXEC ERR: Caught Panic: function expects at most 0 arguments, got 1\n    <compiler:s_eval>:1:14 Func[foo] [10]\n    <compiler:s_eval>:1:25 Call [10]\n");
}

#[test]
fn check_const() {
    assert_eq!(ve("!:const X = 32; X"), "32");
    assert_eq!(ve("!:const X = 32.4; X"), "32.4");
    assert_eq!(ve("!:const X = :XX; X"), ":XX");
    assert_eq!(ve("!:const X = $[1,2]; X.1"), "2");
    assert_eq!(ve("!:const X = ${A=32}; X.A"), "32");
    assert_eq!(ve("!:const X = \"fo\"; X"), "\"fo\"");
    assert_eq!(ve("!:const (A,B,X) = $[1,3,4]; $[X,B,A]"), "$[4,3,1]");
    assert_eq!(ve("!:const (A,B,X) = ${A=1,B=3,X=4}; $[X,B,A]"), "$[4,3,1]");

    assert_eq!(
        ve(r"
        !@import c tests:test_mod_const;
        c:XX
    "),
        "32"
    );

    assert_eq!(
        ve(r"
        !@import c tests:test_mod_const;
        c:X2
    "),
        "$[1,2]"
    );

    assert_eq!(
        ve(r"
        !:const K = $i(1, 2, 3);
        K
    "),
        "$i(1,2,3)"
    );

    assert_eq!(
        ve(r"
        !:const K = $i(1, 2, 3, 5);
        K
    "),
        "$i(1,2,3,5)"
    );

    assert_eq!(
        ve(r"
        !:const K = $i(1, 2);
        K
    "),
        "$i(1,2)"
    );

    assert_eq!(
        ve(r"
        !:const K = $f(1, 2);
        K
    "),
        "$f(1,2)"
    );

    assert_eq!(
        ve(r"
        !:const K = $f(1, 2, 3);
        K
    "),
        "$f(1,2,3)"
    );

    assert_eq!(
        ve(r"
        !:const K = $f(1, 2, 3, 4);
        K
    "),
        "$f(1,2,3,4)"
    );

    assert_eq!(
        ve(r"
        !:const P = $p(:a, :b);
        $[P.v, P.k]
    "),
        "$[:a,:b]"
    );
}

#[test]
fn check_threads() {
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $b\"abc\" }).join[];
    "),
        "$b\"abc\""
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $o(49) }).join[];
    "),
        "$o(49)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $p(:foo,2) }).join[];
    "),
        "$p(:foo,2)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $f(1,2) }).join[];
    "),
        "$f(1,2)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $f(1,2,3) }).join[];
    "),
        "$f(1,2,3)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $f(1,2,3,4) }).join[];
    "),
        "$f(1,2,3,4)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $i(1,2) }).join[];
    "),
        "$i(1,2)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $i(1,2,3) }).join[];
    "),
        "$i(1,2,3)"
    );
    assert_eq!(
        ve("
        (std:thread:spawn $q{ $i(1,2,3,4) }).join[];
    "),
        "$i(1,2,3,4)"
    );
    assert_eq!(
        ve("
        !h = std:thread:spawn $q( $[1,2,${a=20},:x] );
        h.join[];
    "),
        "$[1,2,${a=20},:x]"
    );
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

    assert_eq!(
        ve(r#"
        !chan = std:sync:mpsc:new[];
        !h = std:thread:spawn $code {
            !@import std;
            !@wlambda;

            chan.send 20;
            99
        } ${ chan = chan };

        !msg = chan.recv_timeout $p(:ms, 1000);
        std:assert_eq (type msg) "optional";

        $[h.join[], msg[]];
    "#),
        "$[99,20]"
    );

    assert_eq!(
        ve(r#"
        !chan = std:sync:mpsc:new[];
        !chan2 = std:sync:mpsc:new[];
        !h = std:thread:spawn $code {
            !@import std;
            !@wlambda;

            !m = chan2.recv[];
            chan.send m;
            99
        } ${ chan = chan, chan2 = chan2 };

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
    "#),
        "$[99,\"test\"]"
    );
}

#[test]
fn check_thread_valslot() {
    assert_eq!(
        ve(r#"
        !slt1 = std:sync:slot:new[];
        !slt2 = std:sync:slot:new[];

        std:assert slt1.check_empty[];

        std:assert_eq slt1.try_recv[] $o();

        !h = std:thread:spawn $code {
            !@import std;
            _READY.send 11;

            slt1.send $p(:x, $[1,2,3]);
            slt2.recv[];
        } ${ slt1 = slt1, slt2 = slt2 };

        std:assert_eq h.recv_ready[] 11;

        !r1 = slt1.recv[];

        std:assert_eq (str r1) (str $p(:x, $[1,2,3]));
        slt2.send $true;
        h.join[]
    "#),
        "$true"
    );

    assert_eq!(
        ve(r#"
        !slt1 = std:sync:slot:new[];
        !slt2 = std:sync:slot:new[];

        !h = std:thread:spawn $code {
            !@import std;
            _READY.send $true;

            slt2.send 45;
            std:thread:sleep $p(:ms, 500);
            slt1.send 44;

            100
        } ${ slt1 = slt1, slt2 = slt2 };

        std:assert ~ unwrap h.recv_ready[];

        !r1 = slt1.recv_timeout $p(:ms, 100);
        std:assert_eq r1 $o();

        std:assert_eq slt2.try_recv[] $o(45);

        .r1 = slt1.recv_timeout $p(:ms, 100);
        std:assert_eq r1 $o();
        .r1 = slt1.recv_timeout $p(:ms, 500);
        std:assert_eq r1 $o(44);

        h.join[]
    "#),
        "100"
    );

    assert_eq!(
        ve(r#"
        !slt1 = std:sync:slot:new[];

        !h = std:thread:spawn $code {
            !@import std;
            _READY.send $true;

            std:assert_eq _READY.check_empty[] $false;

            slt1.send :ok;

            !ret = _READY.wait_empty[];
            std:assert_eq ret $true;

            100
        } ${ slt1 = slt1 };

        slt1.recv_timeout $p(:ms, 500);

        std:assert ~ unwrap h.recv_ready[];

        h.join[]
    "#),
        "100"
    );
}

#[test]
fn check_nvec() {
    assert_eq!(ve("$i(1, 2)"), "$i(1,2)");
    assert_eq!(ve("$i(1, 2) * 2"), "$i(2,4)");
    assert_eq!(ve("$f(1, 2) / 2"), "$f(0.5,1)");
    assert_eq!(ve("$f(2, 0) - $f(2, 0)"), "$f(0,0)");
    assert_eq!(ve("$i(2, 0) - $f(2, 0)"), "$i(0,0)");
    assert_eq!(ve("$f(2, 0) + $f(0, 2)"), "$f(2,2)");
    assert_eq!(ve("$f(2, 0) + $i(1, 2)"), "$f(3,2)");
    assert_eq!(ve("$i(2, 0) + $f(1, 2)"), "$i(3,2)");
    assert_eq!(ve("$f(2, 0) + $f(2, 2)"), "$f(4,2)");
    assert_eq!(ve("$i(2, 0) + ${y=2,x=1,z=0}"), "$i(3,2,0)");
    assert_eq!(ve("$i(2, 0) + $[2,1,3]"), "$i(4,1,3)");
    assert_eq!(ve("$f(2, 0) == ${x=2,y=0}"), "$false");
    assert_eq!(ve("$i(0, 0) == ${}"), "$false");
    assert_eq!(ve("$i(0, 0) == ${}"), "$false");
    assert_eq!(ve("$i(0, 0) == $f(0, 0)"), "$false");

    assert_eq!(ve("$i(2, 0) + $p(1, 2)"), "$i(3,2)");

    assert_eq!(ve("$i(2, 3).x"), "2");
    assert_eq!(ve("$i(2, 3).y"), "3");
    assert_eq!(ve("$i(2, 3, 4).x"), "2");
    assert_eq!(ve("$i(2, 3, 4).y"), "3");
    assert_eq!(ve("$i(2, 3, 4).z"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).x"), "2");
    assert_eq!(ve("$i(2, 3, 4, 5).y"), "3");
    assert_eq!(ve("$i(2, 3, 4, 5).z"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).w"), "5");

    assert_eq!(ve("$f(2.1, 3.1).x"), "2.1");
    assert_eq!(ve("$f(2.1, 3.1).y"), "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).x"), "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).y"), "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).z"), "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).x"), "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).y"), "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).z"), "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).w"), "5.3");

    assert_eq!(ve("$i(2, 3).0"), "2");
    assert_eq!(ve("$i(2, 3).1"), "3");
    assert_eq!(ve("$i(2, 3, 4).0"), "2");
    assert_eq!(ve("$i(2, 3, 4).1"), "3");
    assert_eq!(ve("$i(2, 3, 4).2"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).0"), "2");
    assert_eq!(ve("$i(2, 3, 4, 5).1"), "3");
    assert_eq!(ve("$i(2, 3, 4, 5).2"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).3"), "5");

    assert_eq!(ve("$f(2.1, 3.1).0"), "2.1");
    assert_eq!(ve("$f(2.1, 3.1).1"), "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).0"), "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).1"), "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2).2"), "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).0"), "2.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).1"), "3.1");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).2"), "4.2");
    assert_eq!(ve("$f(2.1, 3.1, 4.2, 5.3).3"), "5.3");

    assert_eq!(ve("$i(2, 3).r"), "2");
    assert_eq!(ve("$i(2, 3).g"), "3");
    assert_eq!(ve("$i(2, 3, 4).r"), "2");
    assert_eq!(ve("$i(2, 3, 4).g"), "3");
    assert_eq!(ve("$i(2, 3, 4).b"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).r"), "2");
    assert_eq!(ve("$i(2, 3, 4, 5).g"), "3");
    assert_eq!(ve("$i(2, 3, 4, 5).b"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).a"), "5");

    assert_eq!(ve("$i(2, 3).h"), "2");
    assert_eq!(ve("$i(2, 3).s"), "3");
    assert_eq!(ve("$i(2, 3, 4).h"), "2");
    assert_eq!(ve("$i(2, 3, 4).s"), "3");
    assert_eq!(ve("$i(2, 3, 4).v"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).h"), "2");
    assert_eq!(ve("$i(2, 3, 4, 5).s"), "3");
    assert_eq!(ve("$i(2, 3, 4, 5).v"), "4");
    assert_eq!(ve("$i(2, 3, 4, 5).a"), "5");

    assert_eq!(ve("$i(1, 2).xx"), "$i(1,1)");
    assert_eq!(ve("$i(1, 2).xxx"), "$i(1,1,1)");
    assert_eq!(ve("$i(1, 2).xyxy"), "$i(1,2,1,2)");
    assert_eq!(ve("$i(1, 2, 3).zx"), "$i(3,1)");
    assert_eq!(ve("$i(1, 2, 3).zxy"), "$i(3,1,2)");
    assert_eq!(ve("$i(1, 2, 3).zx"), "$i(3,1)");
    assert_eq!(ve("$i(1, 2, 3).zx"), "$i(3,1)");

    assert_eq!(ve("$i(255, 128, 64).rrg"), "$i(255,255,128)");
    assert_eq!(ve("$i(255, 128).rgba"), "$i(255,128,0,0)");
    assert_eq!(ve("$i(255, 128, 64).rgba"), "$i(255,128,64,0)");
    assert_eq!(ve("$i(255, 128, 64, 255).rgba"), "$i(255,128,64,255)");
    assert_eq!(ve("$i(255, 128, 64).hhs"), "$i(255,255,128)");
    assert_eq!(ve("$i(255, 128).hsva"), "$i(255,128,0,0)");
    assert_eq!(ve("$i(255, 128, 64).hsva"), "$i(255,128,64,0)");
    assert_eq!(ve("$i(255, 128, 64, 255).hsva"), "$i(255,128,64,255)");

    assert_eq!(ve("ivec $p(4.3, 5.4)"), "$i(4,5)");
    assert_eq!(ve("ivec2 $p(4.3, 5.4)"), "$i(4,5)");
    assert_eq!(ve("ivec3 $p(4.3, 5.4)"), "$i(4,5,0)");
    assert_eq!(ve("ivec4 $p(4.3, 5.4)"), "$i(4,5,0,0)");

    assert_eq!(ve("fvec $p(4.3, 5.4)"), "$f(4.3,5.4)");
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

    assert_eq!(ve("nvec_len $i(0,0)"), "2");
    assert_eq!(ve("nvec_len $i(0,0,0)"), "3");
    assert_eq!(ve("nvec_len $i(0,0,0,0)"), "4");
    assert_eq!(ve("nvec_len $f(0,0)"), "2");
    assert_eq!(ve("nvec_len $f(0,0,0)"), "3");
    assert_eq!(ve("nvec_len $f(0,0,0,0)"), "4");
    assert_eq!(ve("nvec_len $p(0,0)"), "0");

    assert_eq!(ve("!v = $i(1, 2, 3, 4); $i(v.0, v.1, v.2, v.3)"), "$i(1,2,3,4)");
    assert_eq!(ve("!v = $i(1, 2, 3, 4); $i(v.0, v.1, v.2)"), "$i(1,2,3)");
    assert_eq!(ve("!v = $i(1, 2, 3, 4); $i(v.0, v.1)"), "$i(1,2)");
}

#[test]
fn check_pairs() {
    assert_eq!(ve("$p(1 + 2, 3 + 4)"), "$p(3,7)");
    assert_eq!(ve("$p(:a, :f).0"), ":a");
    assert_eq!(ve("$p(:a, :f).1"), ":f");
    assert_eq!(ve("$p(:a, :f).car"), ":a");
    assert_eq!(ve("$p(:a, :f).cdr"), ":f");
    assert_eq!(ve("$p(:a, :f).first"), ":a");
    assert_eq!(ve("$p(:a, :f).second"), ":f");
    assert_eq!(ve("$p(:a, :f).head"), ":a");
    assert_eq!(ve("$p(:a, :f).tail"), ":f");
    assert_eq!(ve("$true $p(:a, :f)"), ":f");
    assert_eq!(ve("$false $p(:a, :f)"), ":a");
    assert_eq!(ve("cons :a :f"), "$p(:a,:f)");
    assert_eq!(ve("cons :a :f |> 0"), ":a");
    assert_eq!(ve("cons :a :f |> 1"), ":f");

    assert_eq!(ve("(cons :a :f) == $p(:a,:f)"), "$true");
    assert_eq!(ve("(cons :b :f) == $p(:a,:f)"), "$false");

    assert_eq!(ve("bool $p($t,$t)"), "$true");
    assert_eq!(ve("bool $p($f,$t)"), "$true");
    assert_eq!(ve("bool $p($t,$f)"), "$true");
    assert_eq!(ve("bool $p($f,$f)"), "$false");

    assert_eq!(ve("int $p(3.3,4.4)"), "3");
    assert_eq!(ve("float $p(3.3,4.4)"), "3.3");

    assert_eq!(
        ve(r"
        !p1 = $p(1, 2);
        !p2 = $p(1, 2);
        $[std:ref_id[p1] == std:ref_id[p2],
          std:ref_id[p1] == std:ref_id[p2]]
    "),
        "$[$false,$false]"
    );

    assert_eq!(ve("$p(0, 3)             $q ABCDEFG "), "\"ABC\"");
    assert_eq!(ve("$p(\";\", 2)         $q AB.C.DE.FG "), "$[\"AB.C.DE.FG\"]");
    assert_eq!(ve("$p(\";\", 2)         $q AB;C;DE;FG "), "$[\"AB\",\"C;DE;FG\"]");
    assert_eq!(ve("$p(\";\", 2)         $q ;C;DE;FG "), "$[\"\",\"C;DE;FG\"]");
    assert_eq!(ve("$p(\";\", 3)         $q ;C; "), "$[\"\",\"C\",\"\"]");
    assert_eq!(ve("$p(\";\", 0)         $q AB;C;DE;FG "), "$[\"AB\",\"C\",\"DE\",\"FG\"]");
    assert_eq!(ve("$p(\";\", 0)         $q ;C;DE; "), "$[\"\",\"C\",\"DE\",\"\"]");
    assert_eq!(ve("$p(\";\", \"_\")     $q AB;C;DE;FG "), "\"AB_C_DE_FG\"");

    assert_eq!(ve("$p(0, 3)               $Q ABCDEFG "), "$b\"ABC\"");
    assert_eq!(ve("$p($b\";\", 2)         $Q AB;C;DE;FG "), "$[$b\"AB\",$b\"C;DE;FG\"]");
    assert_eq!(ve("$p($b\";\", 2)         $Q ;C;DE;FG "), "$[$b\"\",$b\"C;DE;FG\"]");
    assert_eq!(ve("$p($b\";\", 3)         $Q ;C; "), "$[$b\"\",$b\"C\",$b\"\"]");
    assert_eq!(
        ve("$p($b\";\", 0)         $Q AB;C;DE;FG "),
        "$[$b\"AB\",$b\"C\",$b\"DE\",$b\"FG\"]"
    );
    assert_eq!(ve("$p($b\";\", 0)         $Q ;C;DE; "), "$[$b\"\",$b\"C\",$b\"DE\",$b\"\"]");

    assert_eq!(ve("$p($b\";;\", 2)        $Q AB;C;DE;FG "), "$[$b\"AB;C;DE;FG\"]");
    assert_eq!(ve("$p($b\";;\", 2)        $Q AB;;C;;DE;;FG "), "$[$b\"AB\",$b\"C;;DE;;FG\"]");
    assert_eq!(ve("$p($b\";;\", 2)        $Q ;;C;;DE;;FG "), "$[$b\"\",$b\"C;;DE;;FG\"]");
    assert_eq!(ve("$p($b\";;\", 3)        $Q ;;C; "), "$[$b\"\",$b\"C;\"]");
    assert_eq!(ve("$p($b\";;\", 3)        $Q ;;C;; "), "$[$b\"\",$b\"C\",$b\"\"]");
    assert_eq!(
        ve("$p($b\";;\", 0)        $Q AB;;C;;DE;;FG "),
        "$[$b\"AB\",$b\"C\",$b\"DE\",$b\"FG\"]"
    );
    assert_eq!(ve("$p($b\";;\", 0)        $Q ;;C;;DE;; "), "$[$b\"\",$b\"C\",$b\"DE\",$b\"\"]");
    assert_eq!(ve("$p($b\";;\", 0)        $Q ;;C;;DE; "), "$[$b\"\",$b\"C\",$b\"DE;\"]");

    assert_eq!(ve("$p($b\";\", $b\"_\")     $Q AB;C;DE;FG "), "$b\"AB_C_DE_FG\"");
    assert_eq!(ve("$p($b\";;\", $b\"_\")    $Q AB;;C;;DE;;FG "), "$b\"AB_C_DE_FG\"");
    assert_eq!(ve("$p($b\";;;\", $b\"_\")   $Q ;;;AB;;;C;;;DE;;;FG;;; "), "$b\"_AB_C_DE_FG_\"");
    assert_eq!(ve("$p($b\";;\", $b\"____\") $Q AB;;C;;DE;;FG "), "$b\"AB____C____DE____FG\"");

    assert_eq!(ve("$p(\";;\", $b\"____\") $Q AB;;C;;DE;;FG "), "$b\"AB____C____DE____FG\"");
    assert_eq!(ve("$p(\";;\", $b\'_\')    $Q AB;;C;;DE;;FG "), "$b\"AB_C_DE_FG\"");
    assert_eq!(ve("$p(\";;\", \'_\')      $Q AB;;C;;DE;;FG "), "$b\"AB_C_DE_FG\"");
    assert_eq!(ve("$p(\";;\", $b\"____\") $Q AB;;C;;DE;;FG "), "$b\"AB____C____DE____FG\"");
    assert_eq!(ve("$p(\';\', $b\'_\')     $Q AB;;C;;DE;;FG "), "$b\"AB__C__DE__FG\"");
    assert_eq!(ve("$p($b\';\', \'_\')     $Q AB;;C;;DE;;FG "), "$b\"AB__C__DE__FG\"");

    assert_eq!(ve("$q AB;;C;;DE;;FG  $p(\";;\", \"____\")"), "\"AB____C____DE____FG\"");
    assert_eq!(ve("$Q AB;;C;;DE;;FG  $p($b\";;\", $b\"____\")"), "$b\"AB____C____DE____FG\"");

    assert_eq!(ve("$q ABCDEF  $i(2,3)"), "\"CDE\"");
    assert_eq!(ve("$Q ABCDEF  $i(2,3)"), "$b\"CDE\"");

    assert_eq!(ve("1 => 2 => 3 => 4"), "$p(1,$p(2,$p(3,4)))");
}

#[test]
fn check_if() {
    assert_eq!(
        ve(r"
    !res =
        ? { !x = 2; x > 1 } 39;

    std:assert_eq res 39;
    "),
        "$true"
    );
    assert_eq!(v("? { !x = 10; x > 1 } 99 49"), "99");
    assert_eq!(v("if { !x = 10; x > 1 } 99 49"), "99");
    assert_eq!(v("? { !x = 10; x > 1 } 99"), "99");
    assert_eq!(v("if { !x = 10; x > 1 } 99"), "99");
    assert_eq!(v("!res = ? { !x = 10; x > 1 } 99 49; res"), "99");
    assert_eq!(v("!res = ? { !x = 10; x > 1 } 99; res"), "99");
    assert_eq!(ve("? 2 > 3 { 1 } { 2 }"), "2");
    assert_eq!(ve("? 2 < 3 { 1 } { 2 }"), "1");
    assert_eq!(ve("if 2 > 3 { 1 } { 2 }"), "2");
    assert_eq!(ve("if 2 < 3 { 1 } { 2 }"), "1");
    assert_eq!(
        ve(r"
        !y = ? 2 < 3 { !x = 11; 1 } { 2 };
        y
    "),
        "1"
    );
    assert_eq!(
        ve(r"
        !x = 11;
        !k = $n;
        !y = ? 2 < 3 {
            !x = $&& 21;
            .k = { $*x };
            1
        } {
            2
        };
        $[x, y, k[]]
    "),
        "$[11,1,21]"
    );
    assert_eq!(
        ve(r"
        !x = 10;
        !k = $n;
        !y = ? 2 < 3 {
            !x = $&& 20;
            ? $t {
                .k = { $*x };
            };
            1
        };
        $[x, y, k[]]
    "),
        "$[10,1,20]"
    );
    assert_eq!(
        ve(r"
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
    "),
        "$[10,1,30]"
    );
    assert_eq!(ve("? 1"),
        "COMPILE ERROR: <compiler:s_eval>:1:3 Compilation Error: ?/if takes 1 or 2 arguments (condition and expression)");
    assert_eq!(ve("? 1 2 3 4"),
        "COMPILE ERROR: <compiler:s_eval>:1:3 Compilation Error: ?/if takes 1 or 2 arguments (condition and expression)");
}

#[test]
fn check_syntax_ret_val_direct_block() {
    assert_eq!(ve("$@v iter i { !x = $[1,2,3]; x } { $+ i }"), "$[1,2,3]");
    assert_eq!(ve("block { !x = 33; x }"), "33");
    assert_eq!(ve("std:str:cat 55 (? {!x = $true; x } 99) 66"), "\"559966\"");
    assert_eq!(ve("std:str:cat (? $true {!x = 23; x }) (? $true {!x = 99; x }) 66"), "\"239966\"");
    assert_eq!(ve("? {!x = $true; x }[] 99"), "99");
    assert_eq!(ve("? {!x = $true; x } 99"), "99");
    assert_eq!(ve("? $true { !x = 1; x }"), "1");
    assert_eq!(ve("? {!x = $true; x }[] { !x = 1; x }"), "1");
    assert_eq!(ve("!f = ? $true  { !x = 1; x } { !y = 2; y }; f"), "1");
    assert_eq!(ve("!f = ? $false { !x = 1; x } { !y = 2; y }; f"), "2");
    assert_eq!(ve("match 3 3 => { !x = 5; x }"), "5");
    assert_eq!(ve("match 3 3 => { !x = 5; x } { !f = 6; f }"), "5");
    assert_eq!(ve("match 4 3 => { !x = 5; x } { !f = 6; f }"), "6");
    assert_eq!(ve("jump 0 { !x = 5; x }"), "5");
    assert_eq!(ve("jump 1 $n { !x = 6; x }"), "6");
    assert_eq!(ve("? $true { !x = 1; x } { !y = 2; y }"), "1");
    assert_eq!(ve("? $false { !x = 1; x } { !y = 2; y }"), "2");
}

#[test]
fn check_backslash_function() {
    assert_eq!(ve("!x = $[\\1,\\2,\\3 + _,5]; $[x.1[],x.2 4,x.3[]]"), "$[2,7,5]");
    assert_eq!(ve("!x = ${a = \\1, b = \\2 * _, c = 9}; $[x.a[],x.b 4,x.c[]]"), "$[1,8,9]");
}

#[test]
fn check_iter() {
    assert_eq!(ve("!x = 0; for $n {|| .x = 20 }; x"), "0");
    assert_eq!(
        ve(r"
        !x = $[1,2,3,4,5,6,7];
        !r = 0;
        iter i x {
            .r = r + i;
        };
        r
    "),
        "28"
    );
    assert_eq!(
        ve(r"
        !x = $[1,2,3,4];
        !r = 0;
        iter i x {
            .r = r + i;
            ? r >= 6 break[];
        };
        r
    "),
        "6"
    );
    assert_eq!(
        v(r"
        !sum = 0;
        iter i $[1, 2, 3, 4] { .sum = sum + i; };
        sum
    "),
        "10"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i ${a=10} { !(v, k) = i; std:push r i; std:push r v; std:push r k; };
        r
    "),
        "$[$p(10,$<1=>\"a\"),10,$<1>]"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i $i(1, 10) { std:push r i; };
        r
    "),
        "$[1,2,3,4,5,6,7,8,9]"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i $p(1, 10) { std:push r i; };
        r
    "),
        "$[1,2,3,4,5,6,7,8,9]"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i 1 => 10 { std:push r i; };
        r
    "),
        "$[1,2,3,4,5,6,7,8,9]"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i $i(1, 10, 2) { std:push r i; };
        r
    "),
        "$[1,3,5,7,9]"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i $f(0.0, 10.0, 3.2) { std:push r int[i * 10]; };
        r
    "),
        "$[0,32,64,96]"
    );

    assert_eq!(
        v(r"
        !r = $[];
        iter i $f(0.0, 10.0) { std:push r int[i * 10]; };
        r
    "),
        "$[0,10,20,30,40,50,60,70,80,90]"
    );

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

    assert_eq!(
        v(r"
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
    "),
        "$[$p(2,90),$p(2,80),$p(2,70),$p(4,90),$p(4,80),$p(4,70)]"
    );

    assert_eq!(
        v(r"
        $@v iter i $i(1,4) {
            ? i == 2 next[];
            $+ i
        }
    "),
        "$[1,3]"
    );

    assert_eq!(
        v(r"
        !x = 0;
        iter i $i(1,4) {
            ? i == 2 next[];
            .x = x + i;
        };
        x
    "),
        "4"
    );

    assert_eq!(
        v(r"
        !a = $[1,2,3,4];
        !b = $[90,80,70];
        !v = $[];

        $@v
            iter ai a {
                ? (ai % 2) == 0 next[];
                $+ $p(ai, ai);
            }
    "),
        "$[$p(1,1),$p(3,3)]"
    );

    assert_eq!(
        v(r"
        !a = $[1,2,3,4];
        !b = $[90,80];

        $@v
            iter ai a {
                iter bi b {
                    ? (ai % 2) == 0 next[];
                    $+ $p(ai, bi);
                }
            }
    "),
        "$[$p(1,90),$p(1,80),$p(3,90),$p(3,80)]"
    );

    assert_eq!(
        v(r"
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
    "),
        "$[$p(1,99),$p(1,100),$p(3,99),$p(3,100)]"
    );

    assert_eq!(
        v(r"
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
    "),
        "$[$p(1,99),$p(1,100),$p(1,200),$p(3,99),$p(3,100),$p(3,200)]"
    );

    assert_eq!(
        v(r"
        $@v
            iter ai $i(0,2) {
                $+ $@i
                    iter bi $i(1,2) {
                        $+ bi;
                        break[];
                    };
            }
    "),
        "$[1,1]"
    );

    assert_eq!(
        v(r"
        $@v
            iter ai $i(3,5) {
                $+ $@i
                    iter bi $i(ai,ai + 1) {
                        $+ bi;
                        break[];
                    };
            }
    "),
        "$[3,4]"
    );

    assert_eq!(
        v(r"
        !a = $[1,2,3,4];
        $@v
            iter ai a {
                $+ $@i
                    iter bi $i(0,1) {
                        $+ bi + ai;
                        break[];
                    };
            }
    "),
        "$[1,2,3,4]"
    );

    assert_eq!(
        v(r"
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
    "),
        "$[399,0,399,0]"
    );

    assert_eq!(
        v(r"
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
    "),
        "$[399,0,399,0]"
    );

    assert_eq!(
        v(r"
        !it = $iter $i(0,10);

        $@v it \$+ _;
    "),
        "$[0,1,2,3,4,5,6,7,8,9]"
    );

    assert_eq!(
        v(r"
        !it = $iter $i(0,3);

        $@v it $+;
    "),
        "$[0,1,2]"
    );

    assert_eq!(
        v(r"
        !it = $iter $i(0,4);
        $@v it \$+ $p(_, unwrap it[]);
    "),
        "$[$p(0,1),$p(2,3)]"
    );

    assert_eq!(v(r"
        !it = $iter $i(0,5);
        $@v it \$+ $p(_, unwrap it[]);
    "), "Runtime error: Panic: unwrap empty option!\n        $n\n    <wlambda::eval>:3:31 Call [$o()]\n    <wlambda::eval>:3:15 Func[it] [4]\n    <wlambda::eval>:3:14 Call [&F{@<wlambda::eval>:3:15 Func[it],a...]\n");
}

#[test]
fn check_function_local_space() {
    assert_eq!(
        v(r"
        { len ~ ? $true { !x = 4; :abc }; }[]
    "),
        "3"
    );
    assert_eq!(
        v(r"
        {
            len $@s
                iter i $i(0,1) {
                    $+ :abcd;
                    break[];
                };
        }[]
    "),
        "4"
    );
    assert_eq!(
        v(r"
        !x = 0;
        iter ai $i(0,1) {
            .x = len $@s
                iter i $i(0,1) {
                    $+ :kkkkk;
                    break[];
                };
        };
        x
    "),
        "5"
    );
}

#[test]
fn check_iter_data() {
    assert_eq!(
        ve(r"
        !it = $iter $[1,2,3,4];
        $[int it, it[], ${a=float[it] + 0.2}, ${b=it[]}]
    "),
        "$[1,$o(2),${a=3.2},${b=$o(4)}]"
    );

    assert_eq!(
        ve(r"
        !it = $iter $q abc ;
        it[];
        it[][];
    "),
        "\'b\'"
    );

    assert_eq!(
        ve(r"
        !it = $iter $Q abc ;
        it[];
        it[][];
    "),
        "$b\'b\'"
    );

    assert_eq!(
        ve(r"
        !it = $iter $i(0, 10);
        it[];
        $[
            it[][],
            it[][],
            it[][],
        ]
    "),
        "$[1,2,3]"
    );

    assert_eq!(
        ve(r"
        !it = $iter $i(0, 10, 2);
        it[];
        $[
            it[][],
            it[][],
            it[][],
        ]
    "),
        "$[2,4,6]"
    );

    assert_eq!(
        ve(r"
        !it = $iter $true;
        $[
            it[][],
            it[][],
        ]
    "),
        "$[$true,$n]"
    );

    // splicing!
    assert_eq!(
        ve(r"
        !it = $iter $i(5,8);
        $[:a, *it, :b]
    "),
        "$[:a,5,6,7,:b]"
    );

    // zip!
    assert_eq!(
        ve(r"
        !it_a = $iter $[1, 2, 3, 4];
        !it_b = $iter $[21, 22, 23];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "),
        "$[$p(1,21),$p(2,22),$p(3,23),$o()]"
    );

    assert_eq!(
        ve(r"
        !it_a = $iter $[1, 2, 3];
        !it_b = $iter $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "),
        "$[$p(1,21),$p(2,22),$p(3,23),$o()]"
    );

    assert_eq!(
        ve(r"
        !it_a = $iter $[1, 2, 3];
        !it_b = $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "),
        "$[$p(1,21),$p(2,22),$p(3,23),$o()]"
    );

    assert_eq!(
        ve(r"
        !it_a = $iter $[1, 2, 3, $none];
        !it_b = $iter $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][], it[][], it[]]
    "),
        "$[$p(1,21),$p(2,22),$p(3,23),$o($p($n,24))]"
    );

    assert_eq!(
        ve(r"
        !it_a = $iter ${a = 10};
        !it_b = $iter $[21, 22, 23, 24];
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][]]
    "),
        "$[$p($p(10,\"a\"),21),$n]"
    );

    assert_eq!(
        ve(r"
        !it_a = $iter ${a = 10};
        !it_b = ${b = 20};
        !it = $iter $p(it_a, it_b);

        $[it[][], it[][]]
    "),
        "$[$p($p(10,\"a\"),$p(20,\"b\")),$n]"
    );

    assert_eq!(
        ve(r"
        !it_a = $iter $[1, 11, 2, 12, 3, 13, 4, 14];
        !it = $iter $p(it_a, it_a);

        $[it[][], it[][], it[][], it[]]
    "),
        "$[$p(1,11),$p(2,12),$p(3,13),$o($p(4,14))]"
    );
}

#[test]
fn check_kve_funcs() {
    assert_eq!(ve("std:sort ~ std:keys      ${a=10,b=20,c=30}"), "$[\"a\",\"b\",\"c\"]");
    assert_eq!(ve("std:sort ~ std:values    ${a=10,b=20,c=30}"), "$[10,20,30]");
    assert_eq!(ve("!it = $iter ${a=10,b=20,c=30}; std:sort $[*it]"), "$[10,20,30]");
    assert_eq!(ve("std:keys      $[4,5,6,7,8]"), "$[0,1,2,3,4]");
    assert_eq!(ve("std:values    $[4,5,6,7,8]"), "$[4,5,6,7,8]");
    assert_eq!(ve("!it = $iter $p(:enumerate, $[4,5,6,7,8]); $[*it]"), "$[0,1,2,3,4]");
    assert_eq!(ve("!it = $iter $i(9,30,4); std:keys      it"), "$[]");
    assert_eq!(ve("!it = $iter ${a=10};    std:keys      it"), "$[\"a\"]");
    assert_eq!(ve("!it = $iter $i(9,30,4); std:values    it"), "$[9,13,17,21,25,29]");
    assert_eq!(ve("!it = $iter $i(9,30,4); $[*$iter $p(:enumerate, it)]"), "$[0,1,2,3,4,5]");
    assert_eq!(ve("std:values $i(1,2)"), "$[1,2]");
    assert_eq!(ve("std:values $i(1,2,3)"), "$[1,2,3]");
    assert_eq!(ve("std:values $i(1,2,3,4)"), "$[1,2,3,4]");
    assert_eq!(ve("std:values $f(1,2)"), "$[1,2]");
    assert_eq!(ve("std:values $f(1,2,3)"), "$[1,2,3]");
    assert_eq!(ve("std:values $f(1,2,3,4)"), "$[1,2,3,4]");
    assert_eq!(ve("fvec ~ std:values $f(1,2)"), "$f(1,2)");
    assert_eq!(ve("fvec ~ std:values $f(1,2,3)"), "$f(1,2,3)");
    assert_eq!(ve("fvec ~ std:values $f(1,2,3,4)"), "$f(1,2,3,4)");
    assert_eq!(ve("ivec ~ std:values $f(1,2)"), "$i(1,2)");
    assert_eq!(ve("ivec ~ std:values $f(1,2,3)"), "$i(1,2,3)");
    assert_eq!(ve("ivec ~ std:values $f(1,2,3,4)"), "$i(1,2,3,4)");
}

#[test]
fn check_delete() {
    assert_eq!(ve("!v = $[1,2,3];     std:delete v 1; v"), "$[1,3]");
    assert_eq!(ve("!v = ${a=10,b=20}; std:delete v :a; v"), "${b=20}");
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
    assert_eq!(ve("$o(10)"), "$o(10)");
    assert_eq!(ve("$o()"), "$o()");

    assert_eq!(ve("is_optional $o(10)"), "$true");
    assert_eq!(ve("is_optional $o()"), "$true");
    assert_eq!(ve("is_optional $none"), "$false");

    assert_eq!(ve("is_some $o(10)"), "$true");
    assert_eq!(ve("is_some $o()"), "$false");

    assert_eq!(ve("is_none $o(10)"), "$false");
    assert_eq!(ve("is_none $o()"), "$true");

    assert_eq!(ve("$o(10) == $o(10)"), "$true");
    assert_eq!(ve("$o(11) == $o(10)"), "$false");
    assert_eq!(ve("$o() == $o()"), "$true");
    assert_eq!(ve("$o() == $o(10)"), "$false");
    assert_eq!(ve("$o(10) == $o()"), "$false");

    assert_eq!(ve("bool $o()"), "$false");
    assert_eq!(ve("bool $o(30)"), "$true");
    assert_eq!(ve("bool $o(\"test\")"), "$true");
    assert_eq!(ve("not $o(30)"), "$false");
    assert_eq!(ve("not $o()"), "$true");
    assert_eq!(ve("not $o(\"xx\")"), "$false");

    assert_eq!(ve("$o(10)[]"), "10");
    assert_eq!(ve("$o()[]"), "$n");

    assert_eq!(ve("$o() 340"), "EXEC ERR: Caught Panic: Calling $none is invalid\n        [340]\n    <compiler:s_eval>:1:6 Call [340]\n");
    assert_eq!(ve("$o(\"f\") \"a\" 340 4 :vvv"), "\"fa3404vvv\"");

    assert_eq!(
        ve(r"
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
        "$[$true,$true,$true,$true,$true]"
    );
}

#[test]
fn check_code_string_literals() {
    assert_eq!(
        ve(r#"
        !code = $code 1 + 2;
        code
    "#),
        "\"1 + 2\""
    );
    assert_eq!(
        ve(r#"
        !code = $code { 1 + 2 };
        code
    "#),
        "\"1 + 2 \""
    );
    assert_eq!(
        ve(r#"
        !code = $c {{
            !a = 302;
            !b = $i(1, 2, 3);
        }};
        code
    "#),
        "\"{\\n            !a = 302;\\n            !b = $i(1, 2, 3);\\n        }\""
    );

    assert_eq!(ve(r#"
        !code = $c {{
            !a = 302;
            !b = $i(1, 2, 3);
            !x = $c (a b c "fff");
        }};
        code
    "#), "\"{\\n            !a = 302;\\n            !b = $i(1, 2, 3);\\n            !x = $c (a b c \\\"fff\\\");\\n        }\"");

    assert_eq!(ve(r#"
        !code = $c {
            !a = 302;
            !b = $i(1, 2, 3);
            !x = 
        };
        code
    "#), "PARSE ERROR: <compiler:s_eval>:6:9 Expected literal value, sub expression, block, key or identifier\nat code:\n6   | };\n7   |         code\n8   |     \n");
}

#[test]
fn check_quote() {
    assert_eq!(
        ve(r#"
        $[$q{fewof wefewop
            fwe []
            []
        }, 121]
    "#),
        "$[\"fewof wefewop\\n            fwe []\\n            []\\n        \",121]"
    );
    assert_eq!(
        ve(r#"
        $q#fewof wefewop
            fwe { feofwef [ XX }(]})]w}
        #
    "#),
        "\"fewof wefewop\\n            fwe { feofwef [ XX }(]})]w}\\n        \""
    );
    assert_eq!(
        ve(r#"
$q
fooooob b fewif wifw
"#),
        "\"fooooob b fewif wifw\""
    );
    assert_eq!(
        ve("$q\tfeiifjweo few \n feiowf wef w \n    fewf oiwejfw \n\t"),
        "\"feiifjweo few \\n feiowf wef w \\n    fewf oiwejfw \\n\""
    );
}

#[test]
fn check_regex_patterns() {
    assert_eq!(ve("type $r(a)"), "\"function\"");
    assert_eq!(ve("$r(a\\)"), "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: bad pattern: <pattern>:1:3 EOF while parsing: Unexpected EOF\nat code:\n1   | \n");

    assert_eq!(ve("$r{(^$+a)rba} $q foobaaarba "), "$[\"aaarba\",\"aaa\"]");
    assert_eq!(ve("$r($+a)       $q foobaaarba "), "$[\"aaa\"]");
    assert_eq!(ve("$r($+a)       $q foobaaarba ; `\\\\`"), "$[\"aaa\"]");
    assert_eq!(ve("$r($+a)       $q foobaaarba ; $\\"), "$[\"aaa\"]");

    assert_eq!(
        ve(r#"
        !rx = $r/ (^$+[0123456789]) $*$s + $*$s (^$+[0123456789]) /;
        ? rx["4943 + 32"] \"got:" $\.1 "+" $\.2;
    "#),
        "\"got:4943+32\""
    );

    assert_eq!(
        ve(r#"
        !rx = std:pattern ~ $q/(^$+[0123456789]) $*$s +/ $q/$*$s (^$+[0123456789])/;
        ? rx["4943 + 32"] \"got:" $\.1 "+" $\.2;
    "#),
        "\"got:4943+32\""
    );

    assert_eq!(
        ve(r#"
        !rx = $r/$^foo/;
        rx "foobar"
    "#),
        "$[\"foo\"]"
    );

    assert_eq!(
        ve(r#"
        !rx = $r/($^foo)/;
        rx "foobar"
    "#),
        "$[\"foo\"]"
    );

    assert_eq!(
        ve(r#"
        !rx = $r/(^$^foo)bar/;
        rx "foobarx"
    "#),
        "$[\"foobar\",\"foo\"]"
    );

    assert_eq!(
        ve(r#"
        !res =
            ? ($r< $<*? (^$+[\\/]) * > "foo//\\/foo") {
                std:assert_eq $\.0 "foo//\\/foo";
                $\.1
            };
        res
    "#),
        "\"//\\\\/\""
    );

    assert_eq!(
        ve(r#"
        $r/$+[a-z]/ &> "abz"
    "#),
        "$[\"z\"]"
    );

    assert_eq!(
        ve(r#"
        $r/$+[a-z]/ <& "abz"
    "#),
        "$[\"abz\"]"
    );

    assert_eq!(
        ve(r#"
        "İ" &> $r/$&L*/;
    "#),
        ""
    );
}

#[test]
fn check_tree_match() {
    assert_eq!(ve("type $S(a/b)"), "\"function\"");

    assert_eq!(ve("$S(a/b)  ${a = ${b = 20}}"), "$[20]");
    assert_eq!(ve("$S(a/b]) ${a = ${b = 20}}"),
        "COMPILE ERROR: <compiler:s_eval>:1:10 Compilation Error: bad selector: <selector>:1:4 Unexpected token ']'. At end of selector\nat code:\n1   | ]\n");
    assert_eq!(ve("unwrap_err ~ std:selector $q a/b] "),
        "\"bad selector: <selector>:1:4 Unexpected token \\']\\'. At end of selector\\nat code:\\n1   | ]\\n, selector was: /a/b]/\"");
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
    assert_eq!(
        ve(r#"
        $S(*/a/^*/^d) $[
            ${ a = $[ ${ c = 10 }, ${ c = 20 }, ${ c = 30 } ] },
            ${ a = $[ ${ c = 11 }, ${ c = 22 }, ${ c = 34 } ] },
            ${ a = $[ ${ c = 12 }, ${ c = 23 }, ${ c = 35 } ] },
            ${ a = $[ ${ c = 13 }, ${ c = 24 }, ${ c = 36 } ] },
        ];
        $\
    "#),
        "$n"
    );
}

#[test]
fn check_iter_var_closure() {
    assert_eq!(
        ve(r#"
        !sum = 0;
        !v = $[];
        iter i $i(0, 10) {
            std:push v { i };
        };
        v { .sum = sum + _[]; };
        sum
    "#),
        "0"
    );

    assert_eq!(
        ve(r#"
        !sum = 0;
        !v = $[];
        !sum2 = $@i
            iter i $i(0, 10) {
                !i = i;
                $+ i;
                std:push v { i };
            };
        v { .sum = sum + _[]; };
        $p(sum, sum2)
    "#),
        "$p(45,45)"
    );
}

#[test]
fn check_struct_patterns() {
    assert_eq!(ve("($M x)               10"), "${x=10}");
    assert_eq!(ve("($M 1.0)             1"), "$n");
    assert_eq!(ve("($M 1)               1"), "${}");
    assert_eq!(ve("($M 1)               1.0"), "$n");
    assert_eq!(ve("($M 1.0)             1.0"), "${}");

    assert_eq!(ve("($M $true)           $&&$true"), "${}");
    assert_eq!(ve("($M $true)           $&&$false"), "$n");
    assert_eq!(ve("($M $false)          $&&$true"), "$n");
    assert_eq!(ve("($M $false)          $&&$false"), "${}");
    assert_eq!(ve("($M $true)           $true"), "${}");
    assert_eq!(ve("($M $true)           $false"), "$n");
    assert_eq!(ve("($M $false)          $true"), "$n");
    assert_eq!(ve("($M $false)          $false"), "${}");

    assert_eq!(ve("($M _type :integer)              10"),       "COMPILE ERROR: <compiler:s_eval>:1:11 Compilation Error: invalid test function in structure pattern: _type");
    assert_eq!(ve("($M _type? :integer)             10"), "${}");
    assert_eq!(ve("($M _type? :integer :string)     10"), "${}");
    assert_eq!(ve("($M _type? :integer :string)     (str 10)"), "${}");
    assert_eq!(ve("($M x ~ _type? :integer)         10"), "${x=10}");
    assert_eq!(ve("($M x ~ _type? :integer :string) 10 &> str"), "${x=\"10\"}");

    assert_eq!(ve("($M 3 4)             4"),            "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: invalid variable binding in structure pattern: 3");
    assert_eq!(ve("($M x 4)             4"), "${x=4}");
    assert_eq!(ve("($M x 4 5 6)         5"), "${x=5}");
    assert_eq!(ve("($M x 4 5 :x => 6)   :x => 6"), "${x=$p(:x,6)}");

    assert_eq!(ve("($M $p(x, y))        20 => 30"), "${x=20,y=30}");
    assert_eq!(ve("($M $p(x, 30))       20 => 30"), "${x=20}");
    assert_eq!(ve("($M $p(x, 40))       20 => 30"), "$n");
    assert_eq!(ve("($M $p(20, y))       20 => 30"), "${y=30}");
    assert_eq!(ve("($M $p(:x, y))       :x => 30"), "${y=30}");

    assert_eq!(ve("($M $p(?, y))        :o => 30"), "${y=30}");
    assert_eq!(ve("($M $p(? 1 2 :o, y)) :o => 30"), "${y=30}");
    assert_eq!(ve("($M $p(? 1 2 :x, y)) :o => 30"), "$n");

    assert_eq!(ve("($M $i(x, y))        $i(1,2)"), "${x=1,y=2}");
    assert_eq!(ve("($M $i(x, y, z))     $i(1,2,3)"), "${x=1,y=2,z=3}");
    assert_eq!(ve("($M $i(x, y, z, w))  $i(1,2,3,4)"), "${w=4,x=1,y=2,z=3}");

    assert_eq!(ve("($M $i(:x, y, z, w)) $i(1,2,3,4)"), "$n");
    assert_eq!(ve("($M $i(1, 2, z, w))  $i(1,2,3,4)"), "${w=4,z=3}");

    assert_eq!(ve("($M $f(1.1, y))          $f(1.1,2)"), "${y=2}");
    assert_eq!(ve("($M $f(1.1, 2.0, z))     $f(1.1,2,3)"), "${z=3}");
    assert_eq!(ve("($M $f(1.1, 2, z))       $f(1.1,2,3)"), "$n");
    assert_eq!(ve("($M $f(1.1, 2.0, z))     $f(1.1,2,3,4)"), "$n");
    assert_eq!(ve("($M $f(1.1, 2.0, z, w))  $f(1.1,2,3,4)"), "${w=4,z=3}");

    assert_eq!(ve("($M $f(1.1, 2.0, z, w))  $f(1.1,2,3,4); $\\"), "${w=4,z=3}");

    assert_eq!(ve("($M $error 20)        $error 20"), "${}");
    assert_eq!(ve("($M $error 20)        $error 21"), "$n");
    assert_eq!(ve("($M $error ?)         $error 22"), "${}");
    assert_eq!(ve("($M x $error ?)       $error 23"), "${x=$e 23 [@ <compiler:s_eval>:1:20 Err]}");
    assert_eq!(
        ve("($M x $error (y ?))   $error 24"),
        "${x=$e 24 [@ <compiler:s_eval>:1:20 Err],y=24}"
    );

    assert_eq!(ve("($M x 10)        $o(10)"), "${x=$o(10)}");
    assert_eq!(ve("($M x 10)        $o(11)"), "$n");
    assert_eq!(ve("($M x $n)        $o()"), "${x=$o()}");

    assert_eq!(ve("($M x $n)        $n"), "${x=$n}");
    assert_eq!(ve("($M x $n)        10"), "$n");

    assert_eq!(ve("($M $o())        $o(10)"), "$n");
    assert_eq!(ve("($M $o())        10"), "$n");
    assert_eq!(ve("($M $o())        $n"), "$n");
    assert_eq!(ve("($M $o())        $o()"), "${}");
    assert_eq!(ve("($M $o(20))      $o(20)"), "${}");
    assert_eq!(ve("($M $o(20))      20"), "$n");
    assert_eq!(ve("($M x $o())      $o()"), "${x=$o()}");
    assert_eq!(ve("($M x $o(20))    $o(20)"), "${x=$o(20)}");
    assert_eq!(ve("($M a $o(b 20))  $o(20)"), "${a=$o(20),b=20}");

    assert_eq!(ve("($M $q foo )    $q/foo/"), "${}");
    assert_eq!(ve("($M x $q foo )  $q/foo/"), "${x=\"foo\"}");
    assert_eq!(ve("($M x $Q foo )  $q/foo/"), "$n");
    assert_eq!(ve("($M x $Q foo )  $b\"foo\""), "${x=$b\"foo\"}");
    assert_eq!(
        ve("($M x $r/f(^$*o)*(^$+(bar))/)  \"fooooXXXbarbarbar\""),
        "${x=$[\"fooooXXXbarbarbar\",\"oooo\",\"bar\"]}"
    );
    assert_eq!(
        ve("($M s (x $r/f(^$*o)*(^$+(bar))/))  \"fooXbar\""),
        "${s=\"fooXbar\",x=$[\"fooXbar\",\"oo\",\"bar\"]}"
    );
    assert_eq!(ve("($M s (? $r/f(^$*o)*(^$+(bar))/))  \"fooXbar\""), "${s=\"fooXbar\"}");
    assert_eq!(ve("($M x $S& */a &)  $[${a=2},${a=3},${a=4}]"), "${x=$[2,3,4]}");

    assert_eq!(ve("($M m ${ a = 3, b = 20 })        ${a=3, b=20, c=10}"), "${m=${a=3,b=20,c=10}}");
    assert_eq!(
        ve("($M m ${ a = x, b = y 10 20 })   ${a=3, b=20, c=10}"),
        "${m=${a=3,b=20,c=10},x=3,y=20}"
    );
    assert_eq!(ve("($M m ${ a = x, b = y 10 20 })   ${a=3, b=40, c=10}"), "$n");
    assert_eq!(ve("($M m ${ a = x, d = 10 })        ${a=3, b=40, c=10}"), "$n");
    assert_eq!(ve("($M m ${ $r/a*b/ = 20, a = 3, b = 20 })  ${a=3, b=20, c=10}"), "$n");
    assert_eq!(
        ve("($M m ${ (?) = 20 })                     ${axb=3, axxb=20, b=20}"),
        "${m=${axb=3,axxb=20,b=20}}"
    );
    assert_eq!(
        ve("($M m ${ (x) = 20 })                     ${axb=3, axxb=20, b=21}"),
        "${m=${axb=3,axxb=20,b=21},x=:axxb}"
    );
    assert_eq!(
        ve("($M m ${ (x) = y 20 })                   ${axb=3, axxb=20, b=21}"),
        "${m=${axb=3,axxb=20,b=21},x=:axxb,y=20}"
    );
    assert_eq!(
        ve("($M m ${ $r/a*b/ = 20, axb = 3, b = 20 }) ${axb=3, axxb=20, b=20}"),
        "${m=${axb=3,axxb=20,b=20}}"
    );
    assert_eq!(ve("($M m ${ $r/a*b/ = 20, a = 3, b = 20 }) ${a=3, b=20, c=10}"), "$n");

    assert_eq!(
        ve("($M $[_type? :string :integer, _type? :symbol :string])         $[1, :f]"),
        "${}"
    );
    assert_eq!(
        ve("($M $[a ~ _type? :string :integer, b ~ _type? :symbol :string]) $[\"x\", :f]"),
        "${a=\"x\",b=:f}"
    );
    assert_eq!(ve("($M x $[1, 2, 3, 4])    $[1, 2, 3, 4]"), "${x=$[1,2,3,4]}");
    assert_eq!(ve("($M x $[1, y, z 3, 4])  $[1, 2, 3, 4]"), "${x=$[1,2,3,4],y=2,z=3}");

    assert_eq!(ve("($M x $[1, _*, 4])               $[1, 2, 3, 4]"), "${x=$[1,2,3,4]}");
    assert_eq!(ve("($M x $[1, _*, 4])               $[1, 4]"), "${x=$[1,4]}");
    assert_eq!(ve("($M x $[1, _*, 4])               $[1, 2, 4]"), "${x=$[1,2,4]}");
    assert_eq!(ve("($M x $[1, _*])                  $[1, 2, 4]"), "${x=$[1,2,4]}");
    assert_eq!(ve("($M x $[1, _*])                  $[1]"), "${x=$[1]}");
    assert_eq!(ve("($M x $[_*])                     $[1]"), "${x=$[1]}");
    assert_eq!(ve("($M x $[_*])                     $[]"), "${x=$[]}");
    assert_eq!(ve("($M x $[y _*])                   $[1]"), "${x=$[1],y=$[1]}");
    assert_eq!(ve("($M x $[y _*])                   $[]"), "${x=$[],y=$[]}");

    assert_eq!(ve("($M x $[1, _+, 4])               $[1, 2, 3, 4]"), "${x=$[1,2,3,4]}");
    assert_eq!(ve("($M x $[1, _+, 4])               $[1, 4]"), "$n");
    assert_eq!(ve("($M x $[1, _+, 4])               $[1, 2, 4]"), "${x=$[1,2,4]}");
    assert_eq!(ve("($M x $[1, _+])                  $[1, 2, 4]"), "${x=$[1,2,4]}");
    assert_eq!(ve("($M x $[1, y _+, 4])             $[1, 2, 4]"), "${x=$[1,2,4],y=$[2]}");
    assert_eq!(ve("($M x $[1, y _+])                $[1, 2, 4]"), "${x=$[1,2,4],y=$[2,4]}");
    assert_eq!(ve("($M x $[1, _+])                  $[1]"), "$n");
    assert_eq!(ve("($M x $[_+])                     $[1]"), "${x=$[1]}");
    assert_eq!(ve("($M x $[_+])                     $[]"), "$n");
    assert_eq!(ve("($M x $[y _+])                   $[1]"), "${x=$[1],y=$[1]}");
    assert_eq!(ve("($M x $[y _+])                   $[]"), "$n");

    assert_eq!(ve("($M x $[1, y _?, 4])             $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[1, y _?, 4])             $[1, 4]"), "${x=$[1,4],y=$o()}");
    assert_eq!(ve("($M x $[1, y _?, 4])             $[1, 2, 4]"), "${x=$[1,2,4],y=$o(2)}");
    assert_eq!(ve("($M x $[1, y _?])                $[1, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[1, y _?])                $[1, 3]"), "${x=$[1,3],y=$o(3)}");
    assert_eq!(ve("($M x $[1, y _?])                $[1]"), "${x=$[1],y=$o()}");
    assert_eq!(ve("($M x $[y _?])                   $[1]"), "${x=$[1],y=$o(1)}");
    assert_eq!(ve("($M x $[y _?])                   $[]"), "${x=$[],y=$o()}");

    assert_eq!(ve("($M x $[1, y _*])                $[1, 2, 4]"), "${x=$[1,2,4],y=$[2,4]}");
    assert_eq!(ve("($M x $[_*])                     $[1, 2, 4]"), "${x=$[1,2,4]}");
    assert_eq!(ve("($M x $[z _*])                   $[1, 2, 4]"), "${x=$[1,2,4],z=$[1,2,4]}");
    assert_eq!(
        ve("($M x $[1, y _*, 5])             $[1, 2, 4, 6, 7, 5]"),
        "${x=$[1,2,4,6,7,5],y=$[2,4,6,7]}"
    );
    assert_eq!(ve("($M x $[1, y _*, 5])             $[1, 2, 4, 6, 7, 5, 7]"), "$n");
    assert_eq!(
        ve("($M x $[1, y _*, 5, 7])          $[1, 2, 4, 6, 7, 5, 7]"),
        "${x=$[1,2,4,6,7,5,7],y=$[2,4,6,7]}"
    );
    assert_eq!(
        ve("($M x $[1, y _*, 5, 7])          $[1,2,4,6,7,5,6,7,8,5,7]"),
        "${x=$[1,2,4,6,7,5,6,7,8,5,7],y=$[2,4,6,7,5,6,7,8]}"
    );
    assert_eq!(ve("($M x $[1, y 2 4, z 2 4])        $[1, 2, 4]"), "${x=$[1,2,4],y=2,z=4}");
    assert_eq!(ve("($M x $[1, y 2 4, z 2 4])        $[1, 4, 4]"), "${x=$[1,4,4],y=4,z=4}");

    assert_eq!(ve("($M x $[1, _* 2, 4])             $[1, 2, 2, 4]"), "${x=$[1,2,2,4]}");
    assert_eq!(ve("($M x $[1, _* (? 2 3), 4])       $[1, 2, 3, 4]"), "${x=$[1,2,3,4]}");
    assert_eq!(ve("($M x $[1, y ~ _* (? 2 3), 4])   $[1, 2, 3, 4]"), "${x=$[1,2,3,4],y=$[2,3]}");
    assert_eq!(ve("($M x $[1, y (_* 2), 4])         $[1, 2, 2, 4]"), "${x=$[1,2,2,4],y=$[2,2]}");

    assert_eq!(ve("($M x $[1, _* 2, 5])             $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[1, _* (? 2 3), 5])       $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[1, y ~ _* (? 2 3), 5])   $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[1, y (_* 2), 5])         $[1, 2, 2, 4]"), "$n");

    assert_eq!(ve("($M x $[_* 2, 4])                $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* (? 2 3), 4])          $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[y (_* 2), 4])            $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* 2])                   $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* (? 2 3)])             $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[y (_* 2)])               $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* 2, 4])                $[2, 2, 2, 4]"), "${x=$[2,2,2,4]}");
    assert_eq!(ve("($M x $[_* (? 2 3), 4])          $[2, 2, 3, 4]"), "${x=$[2,2,3,4]}");
    assert_eq!(ve("($M x $[y (_* 2), 4])            $[2, 2, 2, 4]"), "${x=$[2,2,2,4],y=$[2,2,2]}");
    assert_eq!(ve("($M x $[_* 2])                   $[2, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* (? 2 3)])             $[2, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[y (_* 2)])               $[2, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* 2])                   $[2, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_* (? 2 3)])             $[2, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[_* (? 2 3 4)])           $[2, 2, 3, 4]"), "${x=$[2,2,3,4]}");
    assert_eq!(ve("($M x $[y (_* 2), _?])           $[2, 2, 2, 4]"), "${x=$[2,2,2,4],y=$[2,2,2]}");
    assert_eq!(
        ve("($M x $[y (_* (o 2)), f _?])     $[2, 2, 2, 4]"),
        "${f=$o(4),o=2,x=$[2,2,2,4],y=$[2,2,2]}"
    );
    assert_eq!(ve("($M x $[_* 2])                   $[]"), "${x=$[]}");
    assert_eq!(ve("($M x $[_* 2])                   $[1]"), "$n");
    assert_eq!(ve("($M x $[_* 2])                   $[2]"), "${x=$[2]}");
    assert_eq!(ve("($M x $[1, y ~ _* 2])            $[1]"), "${x=$[1],y=$[]}");
    assert_eq!(ve("($M x $[2, y ~ _* 2])            $[2]"), "${x=$[2],y=$[]}");

    assert_eq!(ve("($M x $[_+ 2, 4])                $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ (? 2 3), 4])          $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[y (_+ 2), 4])            $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ 2])                   $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ (? 2 3)])             $[1, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[y (_+ 2)])               $[1, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ 2, 4])                $[2, 2, 2, 4]"), "${x=$[2,2,2,4]}");
    assert_eq!(ve("($M x $[_+ (? 2 3), 4])          $[2, 2, 3, 4]"), "${x=$[2,2,3,4]}");
    assert_eq!(ve("($M x $[y (_+ 2), 4])            $[2, 2, 2, 4]"), "${x=$[2,2,2,4],y=$[2,2,2]}");
    assert_eq!(ve("($M x $[_+ 2])                   $[2, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ (? 2 3)])             $[2, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[y (_+ 2)])               $[2, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ 2])                   $[2, 2, 2, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ (? 2 3)])             $[2, 2, 3, 4]"), "$n");
    assert_eq!(ve("($M x $[_+ (? 2 3 4)])           $[2, 2, 3, 4]"), "${x=$[2,2,3,4]}");
    assert_eq!(ve("($M x $[y (_+ 2), _?])           $[2, 2, 2, 4]"), "${x=$[2,2,2,4],y=$[2,2,2]}");
    assert_eq!(
        ve("($M x $[y (_+ (o 2)), f _?])     $[2, 2, 2, 4]"),
        "${f=$o(4),o=2,x=$[2,2,2,4],y=$[2,2,2]}"
    );
    assert_eq!(ve("($M x $[_+ 2])                   $[]"), "$n");
    assert_eq!(ve("($M x $[_+ 2])                   $[1]"), "$n");
    assert_eq!(ve("($M x $[_+ 2])                   $[2]"), "${x=$[2]}");
    assert_eq!(ve("($M x $[1, y ~ _+ 2])            $[1]"), "$n");
    assert_eq!(ve("($M x $[2, y ~ _+ 2])            $[2]"), "$n");
    assert_eq!(ve("($M x $[2, y ~ _+ 2])            $[2,2]"), "${x=$[2,2],y=$[2]}");

    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _+ 2, c ~ _* 4])       $[3,3,3,2,2,2,2,4]"),
        "${a=$[3,3,3],b=$[2,2,2,2],c=$[4],x=$[3,3,3,2,2,2,2,4]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _+ 2, c ~ _* 4])       $[2,2,2,2,4]"),
        "${a=$[],b=$[2,2,2,2],c=$[4],x=$[2,2,2,2,4]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _+ 2, c ~ _* 4])       $[3,3,3,2,2,2,2]"),
        "${a=$[3,3,3],b=$[2,2,2,2],c=$[],x=$[3,3,3,2,2,2,2]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _+ 2, c ~ _* 4])       $[2,2,2,2]"),
        "${a=$[],b=$[2,2,2,2],c=$[],x=$[2,2,2,2]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _+ 2, c ~ _* 4])       $[2]"),
        "${a=$[],b=$[2],c=$[],x=$[2]}"
    );
    assert_eq!(ve("($M x $[a ~ _* 3, b ~ _+ 2, c ~ _* 4])       $[3,3,3,4]"), "$n");

    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _* 2, c ~ _* 4])       $[3,3,3,2,2,2,2,4]"),
        "${a=$[3,3,3],b=$[2,2,2,2],c=$[4],x=$[3,3,3,2,2,2,2,4]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _* 2, c ~ _* 4])       $[2,2,2,2,4]"),
        "${a=$[],b=$[2,2,2,2],c=$[4],x=$[2,2,2,2,4]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _* 2, c ~ _* 4])       $[3,3,3,2,2,2,2]"),
        "${a=$[3,3,3],b=$[2,2,2,2],c=$[],x=$[3,3,3,2,2,2,2]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _* 2, c ~ _* 4])       $[2,2,2,2]"),
        "${a=$[],b=$[2,2,2,2],c=$[],x=$[2,2,2,2]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _* 2, c ~ _* 4])       $[2]"),
        "${a=$[],b=$[2],c=$[],x=$[2]}"
    );
    assert_eq!(
        ve("($M x $[a ~ _* 3, b ~ _* 2, c ~ _* 4])       $[3,3,3,4]"),
        "${a=$[3,3,3],b=$[],c=$[4],x=$[3,3,3,4]}"
    );

    assert_eq!(ve("($M x $[_+ $none]) $[]"), "$n");
    assert_eq!(ve("($M x $[_+ $none]) $[$n]"), "${x=$[$n]}");

    assert_eq!(ve("($M x $[_? $none]) $[]"), "${x=$[]}");
    assert_eq!(ve("($M x $[_? $none]) $[$n]"), "${x=$[$n]}");

    assert_eq!(ve("($M x $[a ~ _? 4, b ~ _*]) $[4, 3]"), "${a=4,b=$[3],x=$[4,3]}");
    assert_eq!(ve("($M x $[a ~ _? 5, b ~ _*]) $[4, 3]"), "${b=$[4,3],x=$[4,3]}");

    assert_eq!(ve("($M $[_* 1 2]) $[4, 3]"),                "COMPILE ERROR: <compiler:s_eval>:1:10 Compilation Error: _* takes only 1 argument in list structure pattern: $[$%:Call,$[$%:Var,:\"_*\"],1,2]");
    assert_eq!(ve("($M $[_+ 1 2]) $[4, 3]"),                "COMPILE ERROR: <compiler:s_eval>:1:10 Compilation Error: _* takes only 1 argument in list structure pattern: $[$%:Call,$[$%:Var,:_+],1,2]");
    assert_eq!(ve("($M $[_? 1 2]) $[4, 3]"),                "COMPILE ERROR: <compiler:s_eval>:1:10 Compilation Error: _* takes only 1 argument in list structure pattern: $[$%:Call,$[$%:Var,:\"_?\"],1,2]");

    assert_eq!(
        ve("($M x $[:a, $S& */a &])    $[:a, $[${a=2},${a=3},${a=4}]]"),
        "${x=$[:a,$[${a=2},${a=3},${a=4}]]}"
    );
    assert_eq!(
        ve("($M x $[:a, o $S& */a &])  $[:a, $[${a=2},${a=3},${a=4}]]"),
        "${o=$[2,3,4],x=$[:a,$[${a=2},${a=3},${a=4}]]}"
    );

    assert_eq!(ve("($M (a $q(X)) &or (b $q(Y))) $q(X)"), "${a=\"X\"}");
    assert_eq!(ve("($M (a $q(X)) &or (b $q(Y))) $q(Y)"), "${b=\"Y\"}");
    assert_eq!(ve("($M (a $q(X)) &and (b ?))    $q(X)"), "${a=$<1=>\"X\",b=$<1>}");
}

#[test]
fn check_struct_match() {
    assert_eq!(
        ve("match $i(1,2,3,4)"),
        "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: match takes at least 2 arguments"
    );
    assert_eq!(ve("match $i(1,2,3,4) 20 30"),                       "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: match argument 2 is not a pair: 20");
    assert_eq!(ve("match $i(1,2,3,4) $i(1,2,z,w) { $[z,w] };"),     "COMPILE ERROR: <compiler:s_eval>:1:7 Compilation Error: match argument 2 is not a pair: $[$%:IVec,1,2,$[$%:Var,:z],$[$%:Var,:w]]");

    assert_eq!(ve("match $i(1,2) $i(1,:s) => 11 0 => 42;"), "$n");
    assert_eq!(ve("match $i(1,2) $i(1,:s) => 11 0 => 42 55;"), "55");

    assert_eq!(ve("match $i(1,2,3,4) 30"), "30");
    assert_eq!(ve("match $i(1,2,3,4) $i(1,2,z,w) => { $[$\\.z,$\\.w] };"), "$[3,4]");
    assert_eq!(ve("match $i(1,2)     $i(1,:s)    => 11 42;"), "42");
    assert_eq!(
        ve(r#"
        match $i(1,2,3)
            $i(x,1,y)   => (:a => $\)
            $i(x,2,y)   => (:b => $\)
            $i(x,3,y)   => (:c => $\)
            40;
    "#),
        "$p(:b,${x=1,y=3})"
    );
    assert_eq!(
        ve(r#"
        !m = \match _
            (i (o $S(*/x)))  => (:sel => $\)
            $i(x,  1, y)     => (:a => $\.x)
            $i(x,  2, y)     => (:b => $\.x)
            $i(x,  3, y)     => (:c => $\.x)
            $i(x, 99, y)     => { !(x, y) = $\; x + y }
            :nothing;

        std:assert_eq m[$[${f=10},${l=30}]]         :nothing;
        std:assert_eq str[m[$[${x=10},${l=30}]]]    (str $p(:sel,${i=$[${x=10},${l=30}],o=$[10]}));
        std:assert_eq str[m[$[${x=10},${x=30}]]]    (str $p(:sel,${i=$[${x=10},${x=30}],o=$[10,30]}));
        std:assert_eq m[$i(3,2,5)]           $p(:b, 3);
        std:assert_eq m[$i(3,9,5)]           :nothing;
        std:assert_eq m[$i(3,99,5)]          8;
        $[
            m[$i(3,2,5)],
            m[$i(3,9,5)],
            m[$i(3,99,5)],
        ]
    "#),
        "$[$p(:b,3),:nothing,8]"
    );
    assert_eq!(
        ve(r#"
        match '\<ACK>'
            '\<ENQ>' => 10
            $b'\<ENQ>' => 20
            $b'\<ACK>' => 30
            '\<ACK>' => 31;
    "#),
        "31"
    );
    assert_eq!(
        ve(r#"
        match '\<ENQ>'
            '\<ENQ>' => 10
            $b'\<ENQ>' => 20;
    "#),
        "10"
    );
    assert_eq!(
        ve(r#"
        match $b'\<ENQ>'
            $b'\<ENQ>' => 10
            $b'\<ACK>' => 20;
    "#),
        "10"
    );
    assert_eq!(
        ve(r#"
        match $b'\<ACK>'
            $b'\<ENQ>' => 10
            $b'\<ACK>' => 20;
    "#),
        "20"
    );
}

#[test]
fn check_jump_table() {
    assert_eq!(ve("jump -1 10 30"), "30");
    assert_eq!(ve("jump 2  10 30"), "30");
    assert_eq!(ve("jump 0  10 30"), "10");
    assert_eq!(ve("jump 1  10 30"), "30");
    assert_eq!(ve("(jump 1  10 20 30) + 11"), "31");
    assert_eq!(
        ve("$@v iter i -1 => 4 { $+ ~ jump i { !x = 10; str x } { 20 } 30 }"),
        "$[30,\"10\",20,30,30]"
    );
}

#[test]
fn check_formatter() {
    assert_eq!(v2s("$F\"\" []"), "");
    assert_eq!(v2s("$F\"ax\" []"), "ax");
    assert_eq!(v2s("$F\"a{{}}x\" []"), "a{}x");
    assert_eq!(v2s("$F\"a}}x\" []"), "a}x");
    assert_eq!(v2s("$F$b\"\" []"), "");
    assert_eq!(v2s("$F$b\"ax\" []"), "ax");
    assert_eq!(v2s("$F$b\"a{{}}x\" []"), "a{}x");
    assert_eq!(v2s("$F$b\"a}}x\" []"), "a}x");

    assert_eq!(v2s("$F\"a{}x\" 10"), "a10x");
    assert_eq!(v2s("$F\"a{1}{0}x\" 10 22"), "a2210x");
    assert_eq!(v2s("$F\"a{1}{0}{}x\" 10 22"), format!("a{1}{0}{}x", 10, 22));
    assert_eq!(v2s("$F\"a{1}{0}{}x\" 10 22 33"),            "Runtime error: Panic: function expects at most 2 arguments, got 3\n        [10, 22, 33]\n    <wlambda::eval>:1:16 Call [10, 22, 33]\n");
    assert_eq!(v2s("$F\"a{1}{0}{}{}x\" 10 22"), format!("a{1}{0}{}{}x", 10, 22));
    assert_eq!(v2s("$F\"a{x}{y}x\" $i(3, 4, 5, 6)"), "a34x");
    assert_eq!(v2s("$F\"a{zx}{xx}x\" $i(3, 4, 5, 6)"), "a(5,3)(3,3)x");
    assert_eq!(v2s("$F$b\"a{}x\" 10"), "a10x");
    assert_eq!(v2s("$F$b\"a{1}{0}x\" 10 22"), "a2210x");
    assert_eq!(v2s("$F$b\"a{1}{0}{}x\" 10 22"), format!("a{1}{0}{}x", 10, 22));

    assert_eq!(v2s("$F\"a{:<5}x\"  ~ str 399"), "a399  x");
    assert_eq!(v2s("$F\"a{:<5}x\"  ~ str 399"), "a399  x");
    assert_eq!(v2s("$F\"a{:>5}x\"  ~ str 399"), "a  399x");
    assert_eq!(v2s("$F\"a{:^5}x\"  ~ str 399"), "a 399 x");

    assert_eq!(v2s("$F\"a{:!i}\"     4.3456"), format!("a{}", 4));
    assert_eq!(v2s("$F\"a{:0!i}\"    4.3456"), format!("a{:0}", 4));

    assert_eq!(v2s("$F\"a{:7!i}\"   4.3456"), format!("a{:7}", 4));
    assert_eq!(v2s("$F\"a{:<7!i}\"  4.3456"), format!("a{:<7}", 4));
    assert_eq!(v2s("$F\"a{:>7!i}\"  4.3456"), format!("a{:>7}", 4));
    assert_eq!(v2s("$F\"a{:^7!i}\"  4.3456"), format!("a{:^7}", 4));

    assert_eq!(v2s("$F\"a{:7!ix}\"   4.3456"), format!("a{:7x}", 4));
    assert_eq!(v2s("$F\"a{:<7!ix}\"  4.3456"), format!("a{:<7x}", 4));
    assert_eq!(v2s("$F\"a{:>7!ix}\"  4.3456"), format!("a{:>7x}", 4));
    assert_eq!(v2s("$F\"a{:^7!ix}\"  4.3456"), format!("a{:^7x}", 4));

    assert_eq!(v2s("$F\"a{:7!io}\"   4.3456"), format!("a{:7o}", 4));
    assert_eq!(v2s("$F\"a{:<7!io}\"  4.3456"), format!("a{:<7o}", 4));
    assert_eq!(v2s("$F\"a{:>7!io}\"  4.3456"), format!("a{:>7o}", 4));
    assert_eq!(v2s("$F\"a{:^7!io}\"  4.3456"), format!("a{:^7o}", 4));

    assert_eq!(v2s("$F\"a{:7!ib}\"   4.3456"), format!("a{:7b}", 4));
    assert_eq!(v2s("$F\"a{:<7!ib}\"  4.3456"), format!("a{:<7b}", 4));
    assert_eq!(v2s("$F\"a{:>7!ib}\"  4.3456"), format!("a{:>7b}", 4));
    assert_eq!(v2s("$F\"a{:^7!ib}\"  4.3456"), format!("a{:^7b}", 4));

    assert_eq!(v2s("$F\"a{:07!i}\"   4.3456"), format!("a{:07}", 4));
    assert_eq!(v2s("$F\"a{:<07!i}\"  4.3456"), format!("a{:<07}", 4));
    assert_eq!(v2s("$F\"a{:>07!i}\"  4.3456"), format!("a{:>07}", 4));
    assert_eq!(v2s("$F\"a{:^07!i}\"  4.3456"), format!("a{:^07}", 4));

    assert_eq!(v2s("$F\"a{:!f}\"     4.3456"), format!("a{}", 4.3456));
    assert_eq!(v2s("$F\"a{:0!f}\"    4.3456"), format!("a{:0}", 4.3456));
    assert_eq!(v2s("$F\"a{:0.2!f}\"  4.3456"), format!("a{:0.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:05.2}\"   4.3456"), format!("a{:05.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:05.2!f}\" 4.3456"), format!("a{:05.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:5.2!f}\"  4.3456"), format!("a{:5.2}", 4.3456));

    assert_eq!(v2s("$F\"a{:7!f}\"   4.3456"), format!("a{:7}", 4.3456));
    assert_eq!(v2s("$F\"a{:<7!f}\"  4.3456"), format!("a{:<7}", 4.3456));
    assert_eq!(v2s("$F\"a{:>7!f}\"  4.3456"), format!("a{:>7}", 4.3456));
    assert_eq!(v2s("$F\"a{:^7!f}\"  4.3456"), format!("a{:^7}", 4.3456));
    assert_eq!(v2s("$F\"a{:7.2}\"   4.3456"), format!("a{:7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:<7.2}\"  4.3456"), format!("a{:<7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:>7.2}\"  4.3456"), format!("a{:>7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:^7.2}\"  4.3456"), format!("a{:^7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:7.2!f}\"   4.3456"), format!("a{:7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:<7.2!f}\"  4.3456"), format!("a{:<7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:>7.2!f}\"  4.3456"), format!("a{:>7.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:^7.2!f}\"  4.3456"), format!("a{:^7.2}", 4.3456));

    assert_eq!(v2s("$F\"a{:07!f}\"   4.3456"), format!("a{:07}", 4.3456));
    assert_eq!(v2s("$F\"a{:<07!f}\"  4.3456"), format!("a{:<07}", 4.3456));
    assert_eq!(v2s("$F\"a{:>07!f}\"  4.3456"), format!("a{:>07}", 4.3456));
    assert_eq!(v2s("$F\"a{:^07!f}\"  4.3456"), format!("a{:^07}", 4.3456));
    assert_eq!(v2s("$F\"a{:07.2!f}\"   4.3456"), format!("a{:07.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:<07.2!f}\"  4.3456"), format!("a{:<07.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:>07.2!f}\"  4.3456"), format!("a{:>07.2}", 4.3456));
    assert_eq!(v2s("$F\"a{:^07.2!f}\"  4.3456"), format!("a{:^07.2}", 4.3456));

    assert_eq!(
        v2s("(std:formatter \"{2:07.1!f} {0:!i} {1:!i}\") 1.23 3.45 6.78"),
        format!("{2:07.1} {0} {1}", 1.23 as i64, 3.45 as i64, 6.78)
    );

    assert_eq!(v2s("$F\"{:^8.3!f}\" $f(1.4, 3.43453, 5.6454)"), "( 1.400  , 3.435  , 5.645  )");
    assert_eq!(v2s("$F\"{:<8.3!f}\" $f(1.4, 3.43453, 5.6454)"), "(1.400   ,3.435   ,5.645   )");
    assert_eq!(v2s("$F\"{:>8.3!f}\" $f(1.4, 3.43453, 5.6454)"), "(   1.400,   3.435,   5.645)");
    assert_eq!(v2s("$F\"{:08.3!f}\" $f(1.4, 3.43453, 5.6454)"), "(0001.400,0003.435,0005.645)");
    assert_eq!(v2s("$F\"{:^8!i}\"   $f(1.4, 3.43453, 5.6454)"), "(   1    ,   3    ,   5    )");
    assert_eq!(v2s("$F\"{:<8!i}\"   $f(1.4, 3.43453, 5.6454)"), "(1       ,3       ,5       )");
    assert_eq!(v2s("$F\"{:>8!i}\"   $f(1.4, 3.43453, 5.6454)"), "(       1,       3,       5)");
    assert_eq!(v2s("$F\"{:08!i}\"   $f(1.4, 3.43453, 5.6454)"), "(00000001,00000003,00000005)");
    assert_eq!(v2s("$F\"{:^8!i}\"   $i(1, 3, 5)"), "(   1    ,   3    ,   5    )");
    assert_eq!(v2s("$F\"{:<8!i}\"   $i(1, 3, 5)"), "(1       ,3       ,5       )");
    assert_eq!(v2s("$F\"{:>8!i}\"   $i(1, 3, 5)"), "(       1,       3,       5)");
    assert_eq!(v2s("$F\"{:08!i}\"   $i(1, 3, 5)"), "(00000001,00000003,00000005)");

    assert_eq!(v2s("$F\"{:08!i}\"      1 => 2"), "(00000001,00000002)");
    assert_eq!(v2s("$F\"{:<8}\"        1 => 2"), "(1       ,2       )");
    assert_eq!(v2s("$F\"{:^8}\"        1 => 2"), "(   1    ,   2    )");
    assert_eq!(v2s("$F\"{:>8}\"        1 => 2"), "(       1,       2)");
    assert_eq!(v2s("$F\"{:8}\"         1 => 2"), "(1       ,2       )");
    assert_eq!(v2s("$F\"{:8}\"         :a => :b"), "(a       ,b       )");
    assert_eq!(v2s("$F\"{:010.3!f}\"   1.234567 => 2.987654"), "(000001.235,000002.988)");
    assert_eq!(v2s("$F\"{:<10.3!f}\"   1.234567 => 2.987654"), "(1.235     ,2.988     )");
    assert_eq!(v2s("$F\"{:>10.3!f}\"   1.234567 => 2.987654"), "(     1.235,     2.988)");

    assert_eq!(v2s("$F\"{:08!i}\"      $[] +> 1 +> 2"), "[00000001,00000002]");
    assert_eq!(v2s("$F\"{:<8}\"        $[] +> 1 +> 2"), "[1       ,2       ]");
    assert_eq!(v2s("$F\"{:^8}\"        $[] +> 1 +> 2"), "[   1    ,   2    ]");
    assert_eq!(v2s("$F\"{:>8}\"        $[] +> 1 +> 2"), "[       1,       2]");
    assert_eq!(v2s("$F\"{:8}\"         $[] +> 1 +> 2"), "[1       ,2       ]");
    assert_eq!(v2s("$F\"{:8}\"         $[] +> :a +> :b"), "[a       ,b       ]");

    assert_eq!(v2s("$F\"a{:<7.3!f}\"  4.3456"), format!("a{:<7.3}", 4.3456));
    assert_eq!(v2s("$F\"a{:<7.3}\"  4.3456"), format!("a{:<7.3}", 4.3456));

    assert_eq!(
        ve("$F$Q x=\\xFF{}\\xF3 $b\"\\xF0\\xFF\\xF2\""),
        "$b\"x=\\\\xFF\\xF0\\xFF\\xF2\\\\xF3\""
    );
    assert_eq!(ve("$F$q x=\\xFF{}\\xF3 $b\"\\xF0\\xFF\\xF2\""), "\"x=\\\\xFFðÿò\\\\xF3\"");
    assert_eq!(ve("$F$q x=\\xFF{}\\xF3 \"\\xF0\\xFF\\xF2\""), "\"x=\\\\xFFðÿò\\\\xF3\"");
    assert_eq!(
        ve("$F$Q x=\\xFF{}\\xF3 \"\\xF0\\xFF\\xF2\""),
        "$b\"x=\\\\xFF\\xC3\\xB0\\xC3\\xBF\\xC3\\xB2\\\\xF3\""
    );

    assert_eq!(ve("$F \"   {}   \" 10"), "\"   10   \"");
    assert_eq!(ve("$F \"# {}   \" 10"), "\"# 10   \"");

    //    assert_eq!(v2s("$F\"a{:>5}x\"  399"),     format!("a{:>5}x", 399));
    //    assert_eq!(v2s("$F\"a{:^5}x\"  399"),     format!("a{:^5}x", 399));

    //    assert_eq!(v2s("$F\"a{:<5}x\"  399"),     format!("a{:<5}x", 399));
    //    assert_eq!(v2s("$F\"a{:>5}x\"  399"),     format!("a{:>5}x", 399));
    //    assert_eq!(v2s("$F\"a{:^5}x\"  399"),     format!("a{:^5}x", 399));
    //
    //    assert_eq!(v2s("$F\"a{:^05}x\"  399"),    format!("a{:^05}x", 399));
    //    assert_eq!(v2s("$F\"a{:<05}x\"  399"),    format!("a{:<05}x", 399));
    //    assert_eq!(v2s("$F\"a{:>05}x\"  399"),    format!("a{:>05}x", 399));
    //    assert_eq!(v2s("$F\"a{:#x}x\" 399"),     "a0xffffx");
    //
    //    assert_eq!(v2s("$F$b\"a{}x\" $b\"\\xFF\""),             "a\\xFFx");
}

#[test]
fn check_color_functions() {
    assert_eq!(ve("ivec ~ (std:v:hex2rgba_f :33C0CC80) * 100"), "$i(20,75,80,50)");
    assert_eq!(ve("std:v:hex2rgba_i :33C0CC80"), "$i(51,192,204,128)");

    assert_eq!(ve("std:v:hex2hsva_f :FFFFFFFF"), "$f(360,100,100,100)");
    assert_eq!(ve("std:v:hex2hsva_i :FFFFFFFF"), "$i(360,100,100,100)");
    assert_eq!(ve("std:v:hex2hsva_f :55FFFFFF"), "$f(120,100,100,100)");
    assert_eq!(ve("std:v:hex2hsva_i :55FFFFFF"), "$i(120,100,100,100)");
    assert_eq!(ve("std:v:hex2hsva_f :00FFFFFF"), "$f(0,100,100,100)");
    assert_eq!(ve("std:v:hex2hsva_i :00FFFFFF"), "$i(0,100,100,100)");
    assert_eq!(ve("std:v:hex2hsva_f :00FF00FF"), "$f(0,100,0,100)");
    assert_eq!(ve("std:v:hex2hsva_i :00FF00FF"), "$i(0,100,0,100)");
    assert_eq!(ve("std:v:hex2hsva_f :0000FFFF"), "$f(0,0,100,100)");
    assert_eq!(ve("std:v:hex2hsva_i :0000FFFF"), "$i(0,0,100,100)");
    assert_eq!(ve("std:v:hex2hsva_f :000000FF"), "$f(0,0,0,100)");
    assert_eq!(ve("std:v:hex2hsva_i :000000FF"), "$i(0,0,0,100)");
    assert_eq!(ve("std:v:hex2hsva_f :00000000"), "$f(0,0,0,0)");
    assert_eq!(ve("std:v:hex2hsva_i :00000000"), "$i(0,0,0,0)");

    assert_eq!(ve("std:v:rgba2hex $i(255, 128, 64,  255)"), "\"ff8040ff\"");
    assert_eq!(ve("std:v:rgba2hex $f(1.0, 0.5, 0.25, 1.0)"), "\"ff8040ff\"");
    assert_eq!(ve("std:v:rgba2hex $i(255, 128, 64)"), "\"ff8040ff\"");
    assert_eq!(ve("std:v:rgba2hex $f(1.0, 0.5, 0.25)"), "\"ff8040ff\"");

    assert_eq!(ve("std:v:hsv2rgb $i(0,   100, 100)"), "$i(255,0,0)");
    assert_eq!(ve("std:v:hsv2rgb $i(120, 100, 100)"), "$i(0,255,0)");
    assert_eq!(ve("std:v:hsv2rgb $i(240, 100, 100)"), "$i(0,0,255)");
    assert_eq!(ve("std:v:hsv2rgb $i(0, 0, 50)"), "$i(128,128,128)");
    assert_eq!(ve("std:v:hsv2rgb $i(50, 50, 50)"), "$i(128,117,64)");
    assert_eq!(ve("std:v:hsv2rgb $i(360, 50, 50)"), "$i(128,64,64)");
    assert_eq!(ve("std:v:hsv2rgb $i(0, 100, 50)"), "$i(128,0,0)");
    assert_eq!(ve("std:v:hsv2rgb $i(0, 50, 100)"), "$i(255,128,128)");
    assert_eq!(ve("std:v:hsv2rgb $f(81.0, 0.5, 0.5)"), "$f(0.4125,0.5,0.25)");
    assert_eq!(ve("std:v:hsv2rgb $f(0, 1.0, 1.0)"), "$f(1,0,0)");
    assert_eq!(ve("std:v:hsv2rgb $f(0, 0.5, 1.0)"), "$f(1,0.5,0.5)");
    assert_eq!(ve("std:v:hsv2rgb $i(360, 50, 50, 50)"), "$i(128,64,64,128)");
    assert_eq!(ve("std:v:hsv2rgb $i(50, 100, 50, 50)"), "$i(128,106,0,128)");
    assert_eq!(ve("std:v:hsv2rgb $i(50, 50, 100, 50)"), "$i(255,234,128,128)");
    assert_eq!(ve("std:v:hsv2rgb $f(1.2, 0.5, 0.5, 1.0)"), "$f(0.5,0.255,0.25,1)");
    assert_eq!(ve("std:v:hsv2rgb $f(51, 1.0, 0.5, 0.5)"), "$f(0.5,0.425,0,0.5)");
    assert_eq!(ve("std:v:hsv2rgb $f(270, 0.5, 1.0, 0.0)"), "$f(0.75,0.5,1,0)");

    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(0,   100, 100)"), "$i(0,100,100)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(120, 100, 100)"), "$i(120,100,100)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(240, 100, 100)"), "$i(240,100,100)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(0, 0, 50)"), "$i(0,0,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(50, 50, 50)"), "$i(50,50,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(360, 50, 50)"), "$i(0,50,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(0, 100, 50)"), "$i(0,100,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(0, 50, 100)"), "$i(0,50,100)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $f(81.0, 0.5, 0.5)"), "$f(81,0.5,0.5)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $f(0, 1.0, 1.0)"), "$f(0,1,1)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $f(0, 0.5, 1.0)"), "$f(0,0.5,1)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(360, 50, 50, 50)"), "$i(0,50,50,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(50, 100, 50, 50)"), "$i(50,100,50,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $i(50, 50, 100, 50)"), "$i(50,50,100,50)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $f(30, 0.5, 0.5, 1.0)"), "$f(30,0.5,0.5,1)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $f(51, 1.0, 0.5, 0.5)"), "$f(51,1,0.5,0.5)");
    assert_eq!(ve("std:v:rgb2hsv ~ std:v:hsv2rgb $f(270, 0.5, 1.0, 0.0)"), "$f(270,0.5,1,0)");
}

#[test]
fn check_byte_replace() {
    assert_eq!(ve("std:bytes:replace $q/fooxxxbar/ $q/xxx/ $q/yyy/"), "$b\"fooyyybar\"");
    assert_eq!(ve("std:bytes:replace $q/fooxxxbar/       $q/xxx/ $q//"), "$b\"foobar\"");
    assert_eq!(ve("std:bytes:replace $q/xxxfooxxxbar/    $q/xxx/ $q//"), "$b\"foobar\"");
    assert_eq!(ve("std:bytes:replace $q/xxxfooxxxbarxxx/ $q/xxx/ $q//"), "$b\"foobar\"");
    assert_eq!(ve("std:bytes:replace $q/fooxxxbar/ $q/xxx/ $q/yyyy/"), "$b\"fooyyyybar\"");

    assert_eq!(ve("std:bytes:replace $q/fooxxxbar/ $q//    $q/y/"), "$b\"yfyoyoyxyxyxybyayr\"");

    assert_eq!(ve("std:bytes:replace $q/fooxxxbar/ $q/xxx/ $q/yyxxxyy/"), "$b\"fooyyxxxyybar\"");

    assert_eq!(
        ve("std:bytes:replace $q/fooxxxbarxxxbox/ $q/xxx/ $q/yyyy/"),
        "$b\"fooyyyybaryyyybox\""
    );
    assert_eq!(
        ve("std:bytes:replace $q/fooxxxbarxxxbox/ $q/xxx/ $q/yyy/"),
        "$b\"fooyyybaryyybox\""
    );
    assert_eq!(ve("std:bytes:replace $q/fooxxxbarxxxbox/ $q/xxx/ $q/yy/"), "$b\"fooyybaryybox\"");
    assert_eq!(ve("std:bytes:replace $q/fooxxxbarxxxbox/ $q/xxx/ $q/y/"), "$b\"fooybarybox\"");
    assert_eq!(ve("std:bytes:replace $q/fooxxxbarxxxbox/ $q/xxx/ $q//"), "$b\"foobarbox\"");
    assert_eq!(ve("std:bytes:replace $Q/fooxxxbarxxxbox/ $Q/xxx/ $Q//"), "$b\"foobarbox\"");
    assert_eq!(ve("std:bytes:replace $Q/fooxxxbarxxxbox/ $b'x' $b'o'"), "$b\"fooooobaroooboo\"");
}

#[test]
fn check_op_assignment() {
    assert_eq!(ve("!x = $[1,2,3]; x.1 += 10; x"), "$[1,12,3]");
    assert_eq!(ve("!x = $[1,2,3]; x.1 <== 20; x"), "$[1,$true,3]");
    assert_eq!(ve("!x = $[1,2,3]; x.1 <= 20; x"), "$[1,2,3]");
    assert_eq!(ve("!x = $[1,2,3]; x.1 < = 20; x"), "$[1,$true,3]");
    assert_eq!(ve("!x = $[1,2,3]; x.1 <<= 2; x"), "$[1,8,3]");
    assert_eq!(ve("!x = $[1,2,3]; x.1 === 2; x"), "$[1,$true,3]");

    assert_eq!(ve("!x = 2; .x += 10;  x"), "12");
    assert_eq!(ve("!x = 2; .x + = 10; x"), "12");
    assert_eq!(ve("!x = 2; .x <== 20; x"), "$true");
    assert_eq!(ve("!x = 2; .x < = 20; x"), "$true");
    assert_eq!(ve("!x = 2; .x > = 20; x"), "$false");
    assert_eq!(ve("!x = 2; .x <<= 2; x"), "8");
    assert_eq!(ve("!x = 2; .x === 2; x"), "$true");

    assert_eq!(ve("!x = $&&2; .*x *= 2; x"), "$&&4");
    assert_eq!(ve("!x = $&&2; .x *= 2; x"), "4");

    assert_eq!(ve("!x = \\_ * 10; .x <&= 10; x"), "100");
    assert_eq!(ve("!x = 10; .x &>= \\_ * 20; x"), "200");

    assert_eq!(ve("!x = ${ y = ${ o = ${ l = $[0] } } }; x.y.o.l.0 += 4; x.y.o.l.0"), "4");
    assert_eq!(ve("!x = ${ y = ${ o = ${ l = $[0, 0] } } }; x.y.o.l.1 += 4; x.y.o.l.1"), "4");
    assert_eq!(ve("!x = ${ y = ${ o = 0 } }; x.y.o += 2; x.y.o"), "2");
    assert_eq!(ve("!x = ${ y = ${ o = ${ l = 0 } } }; x.y.o.l += 3; x.y.o.l"), "3");
    assert_eq!(ve("!x = ${ y = 0 }; x.y += 10; x.y"), "10");
}

#[test]
fn check_list_add_ops() {
    assert_eq!(
        ve("1 +> 2 +> 3"),
        "$e \"Can\\\'t add to non collection, got \\\'1\\\' (type integer)\""
    );
    assert_eq!(ve("$[] +> 2 +> 3"), "$[2,3]");
    assert_eq!(ve("1 <+ 2 <+ $[]"), "$[1,2]");
    assert_eq!(ve("1 <+ +2 <+ $[]"), "$[1,2]");

    assert_eq!(ve("$[] +> ($iter 1 => 3)"), "$[1,2]");
    assert_eq!(ve("($iter 1 => 3) <+ $[]"), "$[2,1]");
    assert_eq!(ve("$[] +> ($iter 1 => 3) +> 3"), "$[1,2,3]");
    assert_eq!(ve("$[9] +> ($iter 1 => 3) +> 3"), "$[9,1,2,3]");

    assert_eq!(ve("${} +> :a => 10 +> :b => 20 * 2 +> :x"), "${a=10,b=40,x=:x}");
    assert_eq!(ve("${} +> :a +> 10"), "${10=10,a=:a}");
    assert_eq!(ve("${} +> ($iter ${ x = 20 })"), "${x=20}");
    assert_eq!(ve("${} +> ${ x = 20 }"), "${x=20}");
    assert_eq!(ve("${} +> ($iter 0 => 3)"), "${0=0,1=1,2=2}");
    assert_eq!(ve("${} +> $[:x, 1, 2, 3] +> $[:y, 3, 4, 5]"), "${x=$[:x,1,2,3],y=$[:y,3,4,5]}");
    assert_eq!(ve("${} +> ${ k = 10 } +> ${ x = 1, y = 2 }"), "${k=10,x=1,y=2}");
    assert_eq!(ve("${ a = 40 } +> ${ k = 10 } +> ${ x = 1, y = 2 }"), "${a=40,k=10,x=1,y=2}");

    assert_eq!(ve("\"\" +> :a +> \"b\" +> $b\"c\" +> 10"), "\"abc10\"");
    assert_eq!(ve("$b\"\" +> :a +> :b +> :c +> $b\"\\xFF\""), "$b\"abc\\xFF\"");
    assert_eq!(
        ve("$b\"\" +> :a +> ($iter $[:a, :b, :c]) +> :c +> $b\"\\xFF\""),
        "$b\"aabcc\\xFF\""
    );

    assert_eq!(ve("{ _ * 2 } +> 1 +> 2"), "4");
    assert_eq!(ve("1 <+ 2 <+ { _ * 3 }"), "3");
    assert_eq!(ve("!v = $[]; 1 <+ 2 <+ { std:unshift v _ * 3; v }"), "$[3,6]");
    assert_eq!(
        ve("!v = $[]; ($iter 1 => 4) <+ 1 <+ 2 <+ { std:unshift v _ * 3; v }"),
        "$[9,6,3,3,6]"
    );
    assert_eq!(ve("!v = $[]; { std:push v _ * 3; v } +> ($iter 1 => 4) +> 1 +> 2"), "$[3,6,9,3,6]");

    assert_eq!(ve("`+>` $[] 1 2 3"), "$[1,2,3]");
    assert_eq!(ve("`+>` { _ * 10 } 1"), "10");
    assert_eq!(ve("`<+` $[] 1 2 3"), "$[3,2,1]");
    assert_eq!(ve("`<+` { _ * 10 } 1"), "10");

    assert_eq!(ve("!m = $[]; .m +>= 1; .m +>= 2; m"), "$[1,2]");
    assert_eq!(ve("!m = $[$n,$[]]; m.1 +>= 2; m"), "$[$n,$[2]]");
    assert_eq!(ve("!x = $[]; !m = 1; .m <+= x; .m <+= x; m"), "$<1=>$[$<1>,1]");

    assert_eq!(ve("$b\"abc\" +> 'd' +> $b'd' +> $b\"ef\""), "$b\"abcddef\"");
    assert_eq!(ve("\"abc\" +> 'd' +> $b'd' +> \"ef\""), "\"abcddef\"");
}

#[test]
fn check_find() {
    assert_eq!(ve("std:str:find :d  :abc"), "$n");
    assert_eq!(ve("std:str:find :c  :abc"), "2");
    assert_eq!(ve("std:str:find :a  :abc"), "0");
    assert_eq!(ve("std:str:find :xf :xfcxfc"), "0");
    assert_eq!(ve("std:str:find :xf :xfcxfc 2"), "3");
    assert_eq!(ve("std:str:find :xf :xfcxfc 20"), "$n");

    assert_eq!(ve("$q FOOBAR $p(0,   \'B\')"), "3");
    assert_eq!(ve("$Q FOOBAR $p(0,   \'B\')"), "3");
    assert_eq!(ve("$q FOOBAR $p(0, $b\'B\')"), "3");
    assert_eq!(ve("$Q FOOBAR $p(0, $b\'B\')"), "3");
    assert_eq!(ve("$Q FOOBAR $p(0,      :B)"), "3");
    assert_eq!(ve("$q FOOBAR $p(0,   \"BA\")"), "3");
    assert_eq!(ve("$Q FOOBAR $p(0,   \"BA\")"), "3");
    assert_eq!(ve("$q FOOBAR $p(0, $b\"BA\")"), "3");
    assert_eq!(ve("$Q FOOBAR $p(0, $b\"BA\")"), "3");
    assert_eq!(ve("$Q FOOBAR $p(0,      :B)"), "3");

    assert_eq!(ve("$q FOOBAR $p(3,   \'B\')"), "3");
    assert_eq!(ve("$Q FOOBAR $p(3,   \'B\')"), "3");
    assert_eq!(ve("$q FOOBAR $p(3, $b\'B\')"), "3");
    assert_eq!(ve("$Q FOOBAR $p(3, $b\'B\')"), "3");
    assert_eq!(ve("$Q FOOBAR $p(3,      :B)"), "3");
    assert_eq!(ve("$q FOOBAR $p(3,   \"BA\")"), "3");
    assert_eq!(ve("$Q FOOBAR $p(3,   \"BA\")"), "3");
    assert_eq!(ve("$q FOOBAR $p(3, $b\"BA\")"), "3");
    assert_eq!(ve("$Q FOOBAR $p(3, $b\"BA\")"), "3");
    assert_eq!(ve("$Q FOOBAR $p(3,      :B)"), "3");

    assert_eq!(ve("$q FOAOBAOR $p(3,   \'A\')"), "5");
    assert_eq!(ve("$Q FOAOBAOR $p(3,   \'A\')"), "5");
    assert_eq!(ve("$q FOAOBAOR $p(3, $b\'A\')"), "5");
    assert_eq!(ve("$Q FOAOBAOR $p(3, $b\'A\')"), "5");
    assert_eq!(ve("$Q FOAOBAOR $p(3,      :A)"), "5");
    assert_eq!(ve("$q FOAOBAOR $p(3,   \"AO\")"), "5");
    assert_eq!(ve("$Q FOAOBAOR $p(3,   \"AO\")"), "5");
    assert_eq!(ve("$q FOAOBAOR $p(3, $b\"AO\")"), "5");
    assert_eq!(ve("$Q FOAOBAOR $p(3, $b\"AO\")"), "5");
    assert_eq!(ve("$Q FOAOBAOR $p(3,      :A)"), "5");

    assert_eq!(ve("$q FOOBAR $p(0,   \'F\')"), "0");
    assert_eq!(ve("$Q FOOBAR $p(0,   \'F\')"), "0");
    assert_eq!(ve("$q FOOBAR $p(0, $b\'F\')"), "0");
    assert_eq!(ve("$Q FOOBAR $p(0, $b\'F\')"), "0");
    assert_eq!(ve("$Q FOOBAR $p(0,      :F)"), "0");
    assert_eq!(ve("$q FOOBAR $p(0,   \"FO\")"), "0");
    assert_eq!(ve("$Q FOOBAR $p(0,   \"FO\")"), "0");
    assert_eq!(ve("$q FOOBAR $p(0, $b\"FO\")"), "0");
    assert_eq!(ve("$Q FOOBAR $p(0, $b\"FO\")"), "0");
    assert_eq!(ve("$Q FOOBAR $p(0,      :F)"), "0");

    assert_eq!(ve("$q FOOBAR $p(0,   \'q\')"), "$n");
    assert_eq!(ve("$Q FOOBAR $p(0,   \'q\')"), "$n");
    assert_eq!(ve("$q FOOBAR $p(0, $b\'q\')"), "$n");
    assert_eq!(ve("$Q FOOBAR $p(0, $b\'q\')"), "$n");
    assert_eq!(ve("$Q FOOBAR $p(0,      :q)"), "$n");
    assert_eq!(ve("$q FOOBAR $p(0,   \"qA\")"), "$n");
    assert_eq!(ve("$Q FOOBAR $p(0,   \"qA\")"), "$n");
    assert_eq!(ve("$q FOOBAR $p(0, $b\"qA\")"), "$n");
    assert_eq!(ve("$Q FOOBAR $p(0, $b\"qA\")"), "$n");
    assert_eq!(ve("$Q FOOBAR $p(0,      :q)"), "$n");

    assert_eq!(ve("!s = $q xfcxfc ; !i = std:str:find :xf s 2; i"), "3");
}

#[test]
fn check_write_str() {
    assert_eq!(ve("str $&&10"), "\"10\"");
    assert_eq!(ve("std:write_str $&&10"), "\"$&&10\"");
    assert_eq!(ve("std:write_str \"foo\""), "\"\\\"foo\\\"\"");
}

#[test]
fn check_slice() {
    assert_eq!(ve("$p(2, 2) $q ABCDEF "), "\"CD\"");
    assert_eq!(ve("$p(2, 2) $Q ABCDEF "), "$b\"CD\"");
    assert_eq!(ve("$p(2, 2) $[1,2,3,4,5]"), "$[3,4]");
    assert_eq!(ve("$p(2, 2) $iter $[1,2,3,4,5]"), "$[3,4]");
    assert_eq!(ve("$i(2, 2) $q ABCDEF "), "\"CD\"");
    assert_eq!(ve("$i(2, 2) $Q ABCDEF "), "$b\"CD\"");
    assert_eq!(ve("$i(2, 2) $[1,2,3,4,5]"), "$[3,4]");
    assert_eq!(ve("$i(2, 2) $iter $[1,2,3,4,5]"), "$[3,4]");

    assert_eq!(ve("$p(1, 2) $i(1, 2, 3, 4)"), "$[2,3]");

    assert_eq!(ve("$i(0, 1) $i(1, 2)"), "$[1]");
    assert_eq!(ve("$i(0, 2) $i(1, 2)"), "$[1,2]");
    assert_eq!(ve("$i(1, 1) $i(1, 2)"), "$[2]");
    assert_eq!(ve("$i(1, 2) $i(1, 2)"), "$[2]");

    assert_eq!(ve("$i(0, 1) $i(1, 2, 3)"), "$[1]");
    assert_eq!(ve("$i(0, 2) $i(1, 2, 3)"), "$[1,2]");
    assert_eq!(ve("$i(0, 3) $i(1, 2, 3)"), "$[1,2,3]");
    assert_eq!(ve("$i(1, 1) $i(1, 2, 3)"), "$[2]");
    assert_eq!(ve("$i(1, 2) $i(1, 2, 3)"), "$[2,3]");
    assert_eq!(ve("$i(1, 3) $i(1, 2, 3)"), "$[2,3]");
    assert_eq!(ve("$i(2, 0) $i(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(2, 1) $i(1, 2, 3)"), "$[3]");
    assert_eq!(ve("$i(2, 2) $i(1, 2, 3)"), "$[3]");
    assert_eq!(ve("$i(2, 3) $i(1, 2, 3)"), "$[3]");
    assert_eq!(ve("$i(3, 0) $i(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(3, 1) $i(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(3, 2) $i(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(3, 3) $i(1, 2, 3)"), "$[]");

    assert_eq!(ve("$i(0, 1) $i(1, 2, 3, 4)"), "$[1]");
    assert_eq!(ve("$i(0, 2) $i(1, 2, 3, 4)"), "$[1,2]");
    assert_eq!(ve("$i(0, 3) $i(1, 2, 3, 4)"), "$[1,2,3]");
    assert_eq!(ve("$i(0, 4) $i(1, 2, 3, 4)"), "$[1,2,3,4]");
    assert_eq!(ve("$i(1, 1) $i(1, 2, 3, 4)"), "$[2]");
    assert_eq!(ve("$i(1, 2) $i(1, 2, 3, 4)"), "$[2,3]");
    assert_eq!(ve("$i(1, 3) $i(1, 2, 3, 4)"), "$[2,3,4]");
    assert_eq!(ve("$i(1, 4) $i(1, 2, 3, 4)"), "$[2,3,4]");
    assert_eq!(ve("$i(2, 0) $i(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(2, 1) $i(1, 2, 3, 4)"), "$[3]");
    assert_eq!(ve("$i(2, 2) $i(1, 2, 3, 4)"), "$[3,4]");
    assert_eq!(ve("$i(2, 3) $i(1, 2, 3, 4)"), "$[3,4]");
    assert_eq!(ve("$i(2, 4) $i(1, 2, 3, 4)"), "$[3,4]");
    assert_eq!(ve("$i(3, 0) $i(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(3, 1) $i(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(3, 2) $i(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(3, 3) $i(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(3, 4) $i(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(4, 0) $i(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 1) $i(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 2) $i(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 3) $i(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 4) $i(1, 2, 3, 4)"), "$[]");

    assert_eq!(ve("$i(0, 1) $f(1, 2)"), "$[1]");
    assert_eq!(ve("$i(0, 2) $f(1, 2)"), "$[1,2]");
    assert_eq!(ve("$i(1, 1) $f(1, 2)"), "$[2]");
    assert_eq!(ve("$i(1, 2) $f(1, 2)"), "$[2]");

    assert_eq!(ve("$i(0, 1) $f(1, 2, 3)"), "$[1]");
    assert_eq!(ve("$i(0, 2) $f(1, 2, 3)"), "$[1,2]");
    assert_eq!(ve("$i(0, 3) $f(1, 2, 3)"), "$[1,2,3]");
    assert_eq!(ve("$i(1, 1) $f(1, 2, 3)"), "$[2]");
    assert_eq!(ve("$i(1, 2) $f(1, 2, 3)"), "$[2,3]");
    assert_eq!(ve("$i(1, 3) $f(1, 2, 3)"), "$[2,3]");
    assert_eq!(ve("$i(2, 0) $f(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(2, 1) $f(1, 2, 3)"), "$[3]");
    assert_eq!(ve("$i(2, 2) $f(1, 2, 3)"), "$[3]");
    assert_eq!(ve("$i(2, 3) $f(1, 2, 3)"), "$[3]");
    assert_eq!(ve("$i(3, 0) $f(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(3, 1) $f(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(3, 2) $f(1, 2, 3)"), "$[]");
    assert_eq!(ve("$i(3, 3) $f(1, 2, 3)"), "$[]");

    assert_eq!(ve("$i(0, 1) $f(1, 2, 3, 4)"), "$[1]");
    assert_eq!(ve("$i(0, 2) $f(1, 2, 3, 4)"), "$[1,2]");
    assert_eq!(ve("$i(0, 3) $f(1, 2, 3, 4)"), "$[1,2,3]");
    assert_eq!(ve("$i(0, 4) $f(1, 2, 3, 4)"), "$[1,2,3,4]");
    assert_eq!(ve("$i(1, 1) $f(1, 2, 3, 4)"), "$[2]");
    assert_eq!(ve("$i(1, 2) $f(1, 2, 3, 4)"), "$[2,3]");
    assert_eq!(ve("$i(1, 3) $f(1, 2, 3, 4)"), "$[2,3,4]");
    assert_eq!(ve("$i(1, 4) $f(1, 2, 3, 4)"), "$[2,3,4]");
    assert_eq!(ve("$i(2, 0) $f(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(2, 1) $f(1, 2, 3, 4)"), "$[3]");
    assert_eq!(ve("$i(2, 2) $f(1, 2, 3, 4)"), "$[3,4]");
    assert_eq!(ve("$i(2, 3) $f(1, 2, 3, 4)"), "$[3,4]");
    assert_eq!(ve("$i(2, 4) $f(1, 2, 3, 4)"), "$[3,4]");
    assert_eq!(ve("$i(3, 0) $f(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(3, 1) $f(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(3, 2) $f(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(3, 3) $f(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(3, 4) $f(1, 2, 3, 4)"), "$[4]");
    assert_eq!(ve("$i(4, 0) $f(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 1) $f(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 2) $f(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 3) $f(1, 2, 3, 4)"), "$[]");
    assert_eq!(ve("$i(4, 4) $f(1, 2, 3, 4)"), "$[]");
}

#[test]
fn check_reverse() {
    assert_eq!(ve("std:reverse $q ABCDEF "), "\"FEDCBA\"");
    assert_eq!(ve("std:reverse $Q ABCDEF "), "$b\"FEDCBA\"");
    assert_eq!(ve("std:reverse $[1,2,3,4]"), "$[4,3,2,1]");
    assert_eq!(ve("std:reverse $iter $[1,2,3,4]"), "$[4,3,2,1]");
}

#[test]
fn check_process_os() {
    assert_eq!(
        ve(r#"
        ? std:sys:os[] == $q linux {
            !ret = std:process:run :sh $[$q/-c/, $q/echo "test"/];
            $DEBUG ret;
            std:assert_eq ret.status                  0;
            std:assert_eq ret.success                 $true;
            std:assert_eq std:str:trim[ret.stderr]    $q//;
            std:assert_eq std:str:trim[ret.stdout]    $q/test/;
            $true
        } {
            !ret = std:process:run :cmd $[$q|/C|, $q/echo test/];
            std:assert_eq ret.status    0;
            std:assert_eq ret.success   $true;
            std:assert_eq std:str:trim[ret.stderr]    $q//;
            std:assert_eq std:str:trim[ret.stdout]    $q/test/;
            $true
        };
    "#),
        "$true"
    );
}

#[test]
fn check_regex_pattern_global() {
    assert_eq!(ve("($M x $r $+f ) \"fffoooffffffoofffof\""), "${x=$[\"fff\"]}");
    assert_eq!(
        ve("($M x $rg $+f ) \"fffoooffffffoofffof\""),
        "${x=$[$[\"fff\"],$[\"ffffff\"],$[\"fff\"],$[\"f\"]]}"
    );
    assert_eq!(
        ve("$@v $rg $+f  $q fffooffffofof $+"),
        "$[$[\"fff\"],$[\"ffff\"],$[\"f\"],$[\"f\"]]"
    );
    assert_eq!(
        ve("$@v $rg $+f  $q fffooffffofof { $+ $\\ }"),
        "$[$[\"fff\"],$[\"ffff\"],$[\"f\"],$[\"f\"]]"
    );
    assert_eq!(ve("$rg $+f  $q fffooffffofof { ? len[_.0] > 3 { break _.0; } }"), "\"ffff\"");
    assert_eq!(
        ve("$@v $rg $+f  $q fffooffffofof { ? len[_.0] > 3 { next[]; } { $+ _.0 } }"),
        "$[\"fff\",\"f\",\"f\"]"
    );
    assert_eq!(
        ve("$@v $rg $+f  $q fffooffffofof { $e 11 }"),
        "EXEC ERR: Caught Panic: Dropped error value: 11\n    <compiler:s_eval>:1:34 Err 11\n"
    );

    assert_eq!(ve("$@v (std:pattern $q $+f :g) $q fffooffffofof { ? len[_.0] > 3 { next[]; } { $+ _.0 } }"), "$[\"fff\",\"f\",\"f\"]");
    assert_eq!(
        ve("$@v (std:pattern $q $+f :g) $q fffooffffofof { $e 11 }"),
        "EXEC ERR: Caught Panic: Dropped error value: 11\n    <compiler:s_eval>:1:49 Err 11\n"
    );

    assert_eq!(ve("$@v $rg\"$^$+f\" \"ffooofffofo\" \\$+ _.0"), "$[\"ff\"]");

    assert_eq!(
        ve(r#"
        !f = $M $[_?, x $r/(^a)$*[^b](^$+b)$<*?c/, _*];
        f $[$[], "afoefoeeoobbbbfec", 22];
        $\.x
    "#),
        "$[\"afoefoeeoobbbbfec\",\"a\",\"bbbb\"]"
    );

    assert_eq!(ve(r#"
        !f = $M $[_?, x $rg/(^a)$*[^b](^$+b)$<*?c/, _*];
        f $[$[], "afoefoeeoobbbbfecabbbcabc", 22];
        $\.x
    "#), "$[$[\"afoefoeeoobbbbfec\",\"a\",\"bbbb\"],$[\"abbbc\",\"a\",\"bbb\"],$[\"abc\",\"a\",\"b\"]]");
}

#[test]
fn check_regex_pattern_substitution() {
    assert_eq!(ve("$rs\"$+f\" \"foofffooof\" {:x}"), "\"xooxooox\"");
    assert_eq!(ve("$rs\"$+f\" \"ff\" {:x}"), "\"x\"");
    assert_eq!(ve("$rs\"$+f\" \"\" {:x}"), "\"\"");
    assert_eq!(ve("$rs\"$+f\" \"o\" {:x}"), "\"o\"");
    assert_eq!(ve("$rs\"$+f\" \"offfo\" {:x}"), "\"oxo\"");

    assert_eq!(ve("(std:pattern $q $+f :s) $q fffooffffofof { :x }"), "\"xooxoxox\"");

    // simple rot13:
    assert_eq!(
        ve(r#"
        $rs"?" "jynzoqn"
            \std:str:from_char_vec
                $[((((0 ~ std:str:to_char_vec _.0) - 97) + 13)
                   % 26)
                  + 97]
    "#),
        "\"wlambda\""
    );
}

#[test]
fn check_call_self() {
    assert_eq!(ve("$true[]"), "$true");
    assert_eq!(ve("$false[]"), "$false");
    assert_eq!(ve(":x[]"), ":x");
    assert_eq!(ve("$o()[]"), "$n");
    assert_eq!(ve("$o(10)[]"), "10");
    assert_eq!(ve("$q fefe []"), "\"fefe\"");
    assert_eq!(ve("$Q fefe []"), "$b\"fefe\"");
    assert_eq!(ve("$p(12,4)[]"), "$p(12,4)");
    assert_eq!(ve("$p(:f,:t)[]"), "$p(:f,:t)");
    assert_eq!(ve("$i(1,2)[]"), "$i(1,2)");
    assert_eq!(ve("$i(1,2,3)[]"), "$i(1,2,3)");
    assert_eq!(ve("$i(1,2,3,4)[]"), "$i(1,2,3,4)");
    assert_eq!(ve("$f(1,2)[]"), "$f(1,2)");
    assert_eq!(ve("$f(1,2,3)[]"), "$f(1,2,3)");
    assert_eq!(ve("$f(1,2,3,4)[]"), "$f(1,2,3,4)");
    assert_eq!(ve("$n[]"),                "EXEC ERR: Caught Panic: Calling $none is invalid\n        []\n    <compiler:s_eval>:1:3 Call []\n");
}

#[test]
fn check_pack() {
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q b $[]"), "\"00\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q b $[:@]"), "\"40\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q xbx $[$b\"\\x01\"]"), "\"000100\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q z $[$b\"\\x01\"]"), "\"0100\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q zb $[$b\"\\x01\",:@]"), "\"010040\"");
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q zby $[$b\"\\x01\",:@,$b\"a\\x02a\"]"),
        "\"010040610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q zbs8 $[$b\"\\x01\",:@,$b\"a\\x02a\"]"),
        "\"01004003610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q zbs16 $[$b\"\\x01\",:@,$b\"a\\x02a\"]"),
        "\"0100400300610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q zbs32 $[$b\"\\x01\",:@,$b\"a\\x02a\"]"),
        "\"01004003000000610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q zbs64 $[$b\"\\x01\",:@,$b\"a\\x02a\"]"),
        "\"0100400300000000000000610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q zbs128 $[$b\"\\x01\",:@,$b\"a\\x02a\"]"),
        "\"01004003000000000000000000000000000000610261\""
    );

    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q >s8   $[$b\"a\\x02a\"]"), "\"03610261\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q >s16  $[$b\"a\\x02a\"]"), "\"0003610261\"");
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q >s32  $[$b\"a\\x02a\"]"),
        "\"00000003610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q >s64  $[$b\"a\\x02a\"]"),
        "\"0000000000000003610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q >s128 $[$b\"a\\x02a\"]"),
        "\"00000000000000000000000000000003610261\""
    );

    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q <s8   $[$b\"a\\x02a\"]"), "\"03610261\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q <s16  $[$b\"a\\x02a\"]"), "\"0300610261\"");
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q <s32  $[$b\"a\\x02a\"]"),
        "\"03000000610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q <s64  $[$b\"a\\x02a\"]"),
        "\"0300000000000000610261\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q <s128 $[$b\"a\\x02a\"]"),
        "\"03000000000000000000000000000000610261\""
    );

    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q u8xu16xu32xu64xu128 $[0x12,0x3456,0x67891234,0x12ABCDEF345678,0x0123456789ABCDEF]"),
               "\"12005634003412896700785634EFCDAB120000EFCDAB89674523010000000000000000\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q <u8xu16xu32xu64xu128 $[0x12,0x3456,0x67891234,0x12ABCDEF345678,0x0123456789ABCDEF]"),
               "\"12005634003412896700785634EFCDAB120000EFCDAB89674523010000000000000000\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q >u8xu16xu32xu64xu128 $[0x12,0x3456,0x67891234,0x12ABCDEF345678,0x0123456789ABCDEF]"),
               "\"120034560067891234000012ABCDEF3456780000000000000000000123456789ABCDEF\"");
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q >i32u32 $[-0xFFFF,-0xFFFF]"),
        "\"FFFF0001FFFF0001\""
    );
    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q >i32u32 $[-0x01,-0x01]"),
        "\"FFFFFFFFFFFFFFFF\""
    );

    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q f $[0.1234]"), "\"24B9FC3D\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q >f $[0.1234]"), "\"3DFCB924\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q <f $[0.1234]"), "\"24B9FC3D\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q d $[0.1234]"), "\"F38E53742497BF3F\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q >d $[0.1234]"), "\"3FBF972474538EF3\"");
    assert_eq!(ve("std:bytes:to_hex ~ std:bytes:pack $q <d $[0.1234]"), "\"F38E53742497BF3F\"");

    assert_eq!(
        ve("std:bytes:to_hex ~ std:bytes:pack $q <d>xf $[0.1234,0.1234]"),
        "\"F38E53742497BF3F003DFCB924\""
    );
}

#[test]
fn check_unpack() {
    assert_eq!(ve("std:bytes:unpack $q zz $b\"aaa\\x00bbbbbb\\x00\""), "$[$b\"aaa\",$b\"bbbbbb\"]");
    assert_eq!(
        ve("std:bytes:unpack $q zy $b\"aaa\\x00bbbbbb\\x00\""),
        "$[$b\"aaa\",$b\"bbbbbb\\0\"]"
    );
    assert_eq!(ve("std:bytes:unpack $q zz $b\"aaaaaa\""), "$[$b\"aaaaaa\"]");
    assert_eq!(
        ve("std:bytes:unpack $q xc4c1xz $b\"aaa\\x00bbbbbb\\x00\""),
        "$[$b\"aa\\0b\",$b\"b\",$b\"bbb\"]"
    );
    assert_eq!(
        ve("std:bytes:unpack $q bc4c1bzb $b\"aaa\\x00bbbbbb\\x00\""),
        "$[$b\"a\",$b\"aa\\0b\",$b\"b\",$b\"b\",$b\"bbb\"]"
    );

    assert_eq!(ve("std:bytes:unpack $q u8 $b\"\\x3A\""), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q xu16 $b\"\\x00\\x3A\\x00\""), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q >xu16 $b\"\\x00\\x00\\x3A\""), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q <xu16 $b\"\\x00\\x3A\\x00\""), "$[58]");

    assert_eq!(ve("std:bytes:unpack $q <xu128 ~ std:bytes:pack $q <xu128 $[58]"), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q <xu64 ~ std:bytes:pack $q <xu64 $[58]"), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q <xu32 ~ std:bytes:pack $q <xu32 $[58]"), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q >xu128 ~ std:bytes:pack $q >xu128 $[58]"), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q >xu64 ~ std:bytes:pack $q >xu64 $[58]"), "$[58]");
    assert_eq!(ve("std:bytes:unpack $q >xu32 ~ std:bytes:pack $q >xu32 $[58]"), "$[58]");
    assert_eq!(
        ve("std:bytes:unpack $q <xu64 ~ std:bytes:pack $q >xu64 $[58]"),
        "$[4179340454199820288]"
    );
    assert_eq!(
        ve("std:bytes:unpack $q >xu64 ~ std:bytes:pack $q <xu64 $[58]"),
        "$[4179340454199820288]"
    );

    assert_eq!(ve("std:bytes:unpack $q i8u8 $b\"\\xFF\\xFF\""), "$[-1,255]");
    assert_eq!(ve("std:bytes:unpack $q i16u16 $b\"\\xFF\\xFF\\xFF\\xFF\""), "$[-1,65535]");
    assert_eq!(
        ve("std:bytes:unpack $q i32u32 ~ std:bytes:pack $q i32u32 $[-1, -1]"),
        "$[-1,4294967295]"
    );
    assert_eq!(ve("std:bytes:unpack $q i64u64 ~ std:bytes:pack $q i64u64 $[-1, -1]"), "$[-1,-1]");

    assert_eq!(ve("std:num:round ~ 100000000.0 * 0[std:bytes:unpack $q >xf ~ std:bytes:pack $q >xf $[0.12345678]]"),
               "12345678");
    assert_eq!(ve("std:num:round ~ 100000000000000000.0 * 0[std:bytes:unpack $q >xd ~ std:bytes:pack $q >xd $[0.12345678912345678]]"),
               "12345678912345678");

    assert_eq!(
        ve("std:bytes:unpack $q i8xs8u16 ~ std:bytes:pack $q i8xs8u16 $[42, $Q FOOBAR , 25500]"),
        "$[42,$b\"FOOBAR\",25500]"
    );
    assert_eq!(
        ve("std:bytes:unpack $q i8xs16u16 ~ std:bytes:pack $q i8xs16u16 $[42, $Q FOOBAR , 25500]"),
        "$[42,$b\"FOOBAR\",25500]"
    );
    assert_eq!(
        ve("std:bytes:unpack $q i8xs32u16 ~ std:bytes:pack $q i8xs32u16 $[42, $Q FOOBAR , 25500]"),
        "$[42,$b\"FOOBAR\",25500]"
    );
    assert_eq!(
        ve("std:bytes:unpack $q i8xs64u16 ~ std:bytes:pack $q i8xs64u16 $[42, $Q FOOBAR , 25500]"),
        "$[42,$b\"FOOBAR\",25500]"
    );
    assert_eq!(ve("std:bytes:unpack $q i8xs128u16 ~ std:bytes:pack $q i8xs128u16 $[42, $Q FOOBAR , 25500]"),
               "$[42,$b\"FOOBAR\",25500]");
}

#[test]
fn check_find_bytes() {
    assert_eq!(ve("std:bytes:find :f   $q ofofo "), "1");
    assert_eq!(ve("std:bytes:find :f   $q ooo "), "$n");
    assert_eq!(ve("std:bytes:find :fff $q fff "), "0");
    assert_eq!(ve("std:bytes:find :fff $q ooofff "), "3");
    assert_eq!(ve("std:bytes:find :fff $q fff 0"), "0");
    assert_eq!(ve("std:bytes:find :fff $q ooofff 0"), "3");
    assert_eq!(ve("std:bytes:find :fff $q ooofff 2"), "3");
    assert_eq!(ve("std:bytes:find :fff $q ooofff 3"), "3");
    assert_eq!(ve("std:bytes:find :fff $q ooofff 4"), "$n");
    assert_eq!(ve("std:bytes:find :fff $q ooofff 9"), "$n");

    assert_eq!(ve("std:bytes:find $Q f   $Q ofofo "), "1");
    assert_eq!(ve("std:bytes:find $Q f   $Q ooo "), "$n");
    assert_eq!(ve("std:bytes:find $Q fff $Q fff "), "0");
    assert_eq!(ve("std:bytes:find $Q fff $Q ooofff "), "3");
    assert_eq!(ve("std:bytes:find $Q fff $Q fff 0"), "0");
    assert_eq!(ve("std:bytes:find $Q fff $Q ooofff 0"), "3");
    assert_eq!(ve("std:bytes:find $Q fff $Q ooofff 2"), "3");
    assert_eq!(ve("std:bytes:find $Q fff $Q ooofff 3"), "3");
    assert_eq!(ve("std:bytes:find $Q fff $Q ooofff 4"), "$n");
    assert_eq!(ve("std:bytes:find $Q fff $Q ooofff 9"), "$n");
}

#[test]
fn check_ascii_character_names() {
    assert_eq!(ve(r#"$b"\<NULL>""#), "$b\"\\0\"");
    assert_eq!(ve(r#"$b"\<SOH>""#), "$b\"\\x01\"");
    assert_eq!(ve(r#"$b"\<STX>""#), "$b\"\\x02\"");
    assert_eq!(ve(r#"$b"\<ETX>""#), "$b\"\\x03\"");
    assert_eq!(ve(r#"$b"\<EOT>""#), "$b\"\\x04\"");
    assert_eq!(ve(r#"$b"\<ENQ>""#), "$b\"\\x05\"");
    assert_eq!(ve(r#"$b"\<ACK>""#), "$b\"\\x06\"");
    assert_eq!(ve(r#"$b"\<BEL>""#), "$b\"\\x07\"");
    assert_eq!(ve(r#"$b"\<BS>""#), "$b\"\\x08\"");
    assert_eq!(ve(r#"$b"\<HT>""#), "$b\"\\t\"");
    assert_eq!(ve(r#"$b"\<LF>""#), "$b\"\\n\"");
    assert_eq!(ve(r#"$b"\<VT>""#), "$b\"\\x0B\"");
    assert_eq!(ve(r#"$b"\<FF>""#), "$b\"\\x0C\"");
    assert_eq!(ve(r#"$b"\<CR>""#), "$b\"\\r\"");
    assert_eq!(ve(r#"$b"\<SO>""#), "$b\"\\x0E\"");
    assert_eq!(ve(r#"$b"\<SI>""#), "$b\"\\x0F\"");
    assert_eq!(ve(r#"$b"\<DLE>""#), "$b\"\\x10\"");
    assert_eq!(ve(r#"$b"\<DC1>""#), "$b\"\\x11\"");
    assert_eq!(ve(r#"$b"\<DC2>""#), "$b\"\\x12\"");
    assert_eq!(ve(r#"$b"\<DC3>""#), "$b\"\\x13\"");
    assert_eq!(ve(r#"$b"\<DC4>""#), "$b\"\\x14\"");
    assert_eq!(ve(r#"$b"\<NAK>""#), "$b\"\\x15\"");
    assert_eq!(ve(r#"$b"\<SYN>""#), "$b\"\\x16\"");
    assert_eq!(ve(r#"$b"\<ETB>""#), "$b\"\\x17\"");
    assert_eq!(ve(r#"$b"\<CAN>""#), "$b\"\\x18\"");
    assert_eq!(ve(r#"$b"\<EM>""#), "$b\"\\x19\"");
    assert_eq!(ve(r#"$b"\<SUB>""#), "$b\"\\x1A\"");
    assert_eq!(ve(r#"$b"\<ESC>""#), "$b\"\\x1B\"");
    assert_eq!(ve(r#"$b"\<FS>""#), "$b\"\\x1C\"");
    assert_eq!(ve(r#"$b"\<GS>""#), "$b\"\\x1D\"");
    assert_eq!(ve(r#"$b"\<RS>""#), "$b\"\\x1E\"");
    assert_eq!(ve(r#"$b"\<US>""#), "$b\"\\x1F\"");
    assert_eq!(ve(r#"$b"\<DEL>""#), "$b\"\\x7F\"");
    assert_eq!(ve(r#"$b"\<SPACE>""#), "$b\" \"");
    assert_eq!(ve(r#"$b"\<NBSP>""#), "$b\"\\xFF\"");

    assert_eq!(ve(r#"$b"\<null>""#), "$b\"\\0\"");
    assert_eq!(ve(r#"$b"\<soh>""#), "$b\"\\x01\"");
    assert_eq!(ve(r#"$b"\<stx>""#), "$b\"\\x02\"");
    assert_eq!(ve(r#"$b"\<etx>""#), "$b\"\\x03\"");
    assert_eq!(ve(r#"$b"\<eot>""#), "$b\"\\x04\"");
    assert_eq!(ve(r#"$b"\<enq>""#), "$b\"\\x05\"");
    assert_eq!(ve(r#"$b"\<ack>""#), "$b\"\\x06\"");
    assert_eq!(ve(r#"$b"\<bel>""#), "$b\"\\x07\"");
    assert_eq!(ve(r#"$b"\<bs>""#), "$b\"\\x08\"");
    assert_eq!(ve(r#"$b"\<ht>""#), "$b\"\\t\"");
    assert_eq!(ve(r#"$b"\<lf>""#), "$b\"\\n\"");
    assert_eq!(ve(r#"$b"\<vt>""#), "$b\"\\x0B\"");
    assert_eq!(ve(r#"$b"\<ff>""#), "$b\"\\x0C\"");
    assert_eq!(ve(r#"$b"\<cr>""#), "$b\"\\r\"");
    assert_eq!(ve(r#"$b"\<so>""#), "$b\"\\x0E\"");
    assert_eq!(ve(r#"$b"\<si>""#), "$b\"\\x0F\"");
    assert_eq!(ve(r#"$b"\<dle>""#), "$b\"\\x10\"");
    assert_eq!(ve(r#"$b"\<dc1>""#), "$b\"\\x11\"");
    assert_eq!(ve(r#"$b"\<dc2>""#), "$b\"\\x12\"");
    assert_eq!(ve(r#"$b"\<dc3>""#), "$b\"\\x13\"");
    assert_eq!(ve(r#"$b"\<dc4>""#), "$b\"\\x14\"");
    assert_eq!(ve(r#"$b"\<nak>""#), "$b\"\\x15\"");
    assert_eq!(ve(r#"$b"\<syn>""#), "$b\"\\x16\"");
    assert_eq!(ve(r#"$b"\<etb>""#), "$b\"\\x17\"");
    assert_eq!(ve(r#"$b"\<can>""#), "$b\"\\x18\"");
    assert_eq!(ve(r#"$b"\<em>""#), "$b\"\\x19\"");
    assert_eq!(ve(r#"$b"\<sub>""#), "$b\"\\x1A\"");
    assert_eq!(ve(r#"$b"\<esc>""#), "$b\"\\x1B\"");
    assert_eq!(ve(r#"$b"\<fs>""#), "$b\"\\x1C\"");
    assert_eq!(ve(r#"$b"\<gs>""#), "$b\"\\x1D\"");
    assert_eq!(ve(r#"$b"\<rs>""#), "$b\"\\x1E\"");
    assert_eq!(ve(r#"$b"\<us>""#), "$b\"\\x1F\"");
    assert_eq!(ve(r#"$b"\<del>""#), "$b\"\\x7F\"");
    assert_eq!(ve(r#"$b"\<space>""#), "$b\" \"");
    assert_eq!(ve(r#"$b"\<nbsp>""#), "$b\"\\xFF\"");
}

#[test]
fn check_tcp() {
    let thrd = std::thread::spawn(move || {
        ve(r#"
            std:net:tcp:listen "127.0.0.1" => 19323 {!(con) = @;
                std:io:write con "HELLO!\r\n";
                !end = $false;
                while not[end] {
                    match std:io:read_some[con]
                        $o(buf) => {
                            ? is_some <& (std:bytes:find $b"q" $\.buf) {
                                std:io:write con "QUIT";
                                std:io:flush con;
                                return :all $none;
                            } { };
                            std:io:write con $\.buf;
                            std:io:flush con;
                        }
                        { .end = $true; };
                };
            };
        "#);
    });

    assert_eq!(
        ve(r#"
        !socket = $n;
        iter _ (0 => 100) {
            .socket = std:net:tcp:connect "127.0.0.1:19323";
            ? is_err[socket] {
                std:thread:sleep $p(:ms, 50);
                next[];
            };
        };
        std:io:read_some socket;
    "#),
        "$o($b\"HELLO!\\r\\n\")"
    );

    assert_eq!(
        ve(r#"
        !socket = std:net:tcp:connect "127.0.0.1:19323";
        std:io:read_some socket;
    "#),
        "$o($b\"HELLO!\\r\\n\")"
    );

    assert_eq!(
        ve(r#"
        !socket = std:net:tcp:connect "127.0.0.1:19323";
        !first = std:io:read_some socket;
        std:io:write_some socket $b"FOOBAR";
        std:io:flush socket;
        unwrap[first] => unwrap[std:io:read_some socket]
    "#),
        "$p($b\"HELLO!\\r\\n\",$b\"FOOBAR\")"
    );

    assert_eq!(
        ve(r#"
        !socket = std:net:tcp:connect "127.0.0.1:19323";
        !slot = std:sync:slot:new[];

        !h = std:thread:spawn $q{
            !@wlambda;
            !@import std;

            !socket = THREAD_ARG0;
            !first = std:io:read_some socket;
            std:io:write_some socket $b"q";
            std:io:flush socket;

            THREAD_ARG1.send("OK");
        } $[socket, slot];

        !h2 = std:thread:spawn $q{
            !@wlambda;
            !@import std;

            std:assert_eq slot.recv[] "OK";
            !first = std:io:read_some socket;
            unwrap[first]
        } ${ socket = socket, slot = slot };

        h.join[];
        h2.join[]
    "#),
        "$b\"QUIT\""
    );

    thrd.join().unwrap();
}

#[test]
fn check_tcp_buffered() {
    let thrd = std::thread::spawn(move || {
        ve(r#"
            std:net:tcp:listen "127.0.0.1" => 19324 {!(con) = @;
                std:io:write con "HELLO!\r\n";
                !end = $false;
                while not[end] {
                    match std:io:read_some[con]
                        $o(buf) => {
                            ? is_some <& (std:bytes:find $b"q" $\.buf) {
                                std:io:write con "QUIT";
                                std:io:flush con;
                                return :all $none;
                            } { };
                            std:io:write con $\.buf;
                            std:io:flush con;
                        }
                        { .end = $true; };
                };
            };
        "#);
    });

    assert_eq!(
        ve(r#"
        !socket = $n;
        iter _ (0 => 100) {
            .socket = std:net:tcp:connect "127.0.0.1:19324";
            ? is_err[socket] {
                std:thread:sleep $p(:ms, 50);
                next[];
            };
        };
        !buf_rd = std:io:buffered:reader socket;
        std:io:write socket "TEST\n";
        std:io:flush socket;
        !line1 = std:io:read_line buf_rd;
        !line2 = std:io:read_line buf_rd;
        line1 line2
    "#),
        "\"HELLO!\\r\\nTEST\\n\""
    );

    // testing buffered writing and reading
    assert_eq!(
        ve(r#"
        !socket = $n;
        iter _ (0 => 100) {
            .socket = std:net:tcp:connect "127.0.0.1:19324";
            ? is_err[socket] {
                std:thread:sleep $p(:ms, 50);
                next[];
            };
        };
        std:net:tcp:set_timeouts socket :ms => 100 :ms => 100;

        !buf_rd = std:io:buffered:reader socket;
        !buf_wr = std:io:buffered:writer socket;
        !line1 = std:io:read_line buf_rd;
        std:io:write buf_wr "TEST2\n";
        std:thread:sleep $p(:ms, 200);
        !line2 = ? is_err[std:io:read_line buf_rd] "OK_TIMEOUT" "NOTOK";
        std:io:flush buf_wr;
        std:thread:sleep $p(:ms, 200);
        !line3 = std:io:read_line buf_rd;

        std:io:write socket "TEST3\n";
        std:thread:sleep $p(:ms, 200);
        !line4 = ? is_err[std:io:read_line buf_rd] "NOTOK_TOUT" "OK_NO_TOUT";

        std:io:write socket "q";
        std:io:flush socket;

        line1 "," line2 "," line3 "," line4
    "#),
        "\"HELLO!\\r\\n,OK_TIMEOUT,TEST2\\n,OK_NO_TOUT\""
    );

    thrd.join().unwrap();
}

#[test]
fn check_chars_and_bytes() {
    assert_eq!(ve("type        'x'"), "\"char\"");
    assert_eq!(ve("type        $b'x'"), "\"byte\"");
    assert_eq!(ve("is_char     $b'x'"), "$false");
    assert_eq!(ve("is_byte     $b'x'"), "$true");
    assert_eq!(ve("is_bytes    $b'x'"), "$false");
    assert_eq!(ve("is_char     'x'"), "$true");
    assert_eq!(ve("is_byte     'x'"), "$false");
    assert_eq!(ve("is_bytes    'x'"), "$false");
    assert_eq!(ve("float       'x'"), "120");
    assert_eq!(ve("type ~ \"abc\".0"), "\"char\"");
    assert_eq!(ve("type ~ $b\"abc\".0"), "\"byte\"");
    assert_eq!(ve("\"a\"      'a'"), "\"aa\"");
    assert_eq!(ve("\"a\"    $b'a'"), "\"aa\"");
    assert_eq!(ve("$b\"a\"    'a'"), "$b\"aa\"");
    assert_eq!(ve("$b\"a\"  $b'a'"), "$b\"aa\"");
    assert_eq!(ve("type ~ $b\"abc\".0"), "\"byte\"");
    assert_eq!(ve("int       $b'x'"), "120");
    assert_eq!(ve("float     $b'x'"), "120");
    assert_eq!(ve("int         'x'"), "120");
    assert_eq!(ve("float       'x'"), "120");
    assert_eq!(ve("int       $b'ä'"), "228");
    assert_eq!(ve("float     $b'ä'"), "228");
    assert_eq!(ve("int         'ä'"), "228");
    assert_eq!(ve("float       'ä'"), "228");
    assert_eq!(ve("int       $b'☯'"), "63");
    assert_eq!(ve("float     $b'☯'"), "63");
    assert_eq!(ve("int         '☯'"), "9775");
    assert_eq!(ve("float       '☯'"), "9775");
    assert_eq!(ve("char        \"xoo\""), "\'x\'");
    assert_eq!(ve("byte        \"xoo\""), "$b\'x\'");
    assert_eq!(ve("char        42"), "\'*\'");
    assert_eq!(ve("byte        42"), "$b\'*\'");
    assert_eq!(ve("char ~ float 42"), "\'*\'");
    assert_eq!(ve("byte ~ float 42"), "$b\'*\'");
    assert_eq!(ve("char        0x3237"), "\'㈷\'");
    assert_eq!(ve("byte        0x3237"), "$b\'?\'");
    assert_eq!(ve("char ~ float 0x3237"), "\'㈷\'");
    assert_eq!(ve("byte ~ float 0x3237"), "$b\'?\'");
    assert_eq!(ve("str     '\x42'"), "\"B\"");
    assert_eq!(ve("str   $b'\x42'"), "\"B\"");
    assert_eq!(ve("len     '\x42'"), "1");
    assert_eq!(ve("len   $b'\x42'"), "1");
    assert_eq!(ve("\"a☯\".0"), "\'a\'");
    assert_eq!(ve("\"a☯\".1"), "\'☯\'");
    assert_eq!(ve("$b\"a☯\".0"), "$b\'a\'");
    assert_eq!(ve("$b\"a☯\".1"), "$b\'\\xE2\'");
    assert_eq!(ve("std:str:len   '\x42'"), "1");
    assert_eq!(ve("std:str:len $b'\x42'"), "1");
    assert_eq!(ve("$@bytes iter c \"abcÄ\" ~ $+ c"), "$b\"abc\\xC4\"");
    assert_eq!(ve("$@bytes iter b $b\"abcÄ\" ~ $+ b"), "$b\"abc\\xC3\\x84\"");
    assert_eq!(ve("$@bytes $+ '@'"), "$b\"@\"");
    assert_eq!(ve("$@bytes $+ $b'@'"), "$b\"@\"");
    assert_eq!(ve("$@s iter c \"abcÄ\" ~ $+ c"), "\"abcÄ\"");
    assert_eq!(ve("$@s iter b $b\"abcÄ\" ~ $+ b"), "\"abcÃ\\u{84}\"");
    assert_eq!(ve("$@s $+ '@'"), "\"@\"");
    assert_eq!(ve("$@s $+ $b'@'"), "\"@\"");
    assert_eq!(ve("$@v iter c   \"ab\" ~ $+ ~ type c"), "$[\"char\",\"char\"]");
    assert_eq!(ve("$@v iter c $b\"ab\" ~ $+ ~ type c"), "$[\"byte\",\"byte\"]");
    assert_eq!(ve("$p($b'o', $b'x') 'o'"), "$true");
    assert_eq!(ve("$p('o', 'x')     'o'"), "$true");
    assert_eq!(ve("$p($b'o', $b'x') $b'o'"), "$true");
    assert_eq!(ve("$p('o', 'x')     $b'o'"), "$true");
    assert_eq!(ve("$p($b'o', $b'x') 'y'"), "$false");
    assert_eq!(ve("$p('o', 'x')     'y'"), "$false");
    assert_eq!(ve("$p($b'o', $b'x') $b'y'"), "$false");
    assert_eq!(ve("$p('o', 'x')     $b'y'"), "$false");
    assert_eq!(ve("'o'   $p($b'o', $b'x')"), "$true");
    assert_eq!(ve("'o'   $p('o', 'x')"), "$true");
    assert_eq!(ve("$b'o' $p($b'o', $b'x')"), "$true");
    assert_eq!(ve("$b'o' $p('o', 'x')"), "$true");
    assert_eq!(ve("'y'   $p($b'o', $b'x')"), "$false");
    assert_eq!(ve("'y'   $p('o', 'x')"), "$false");
    assert_eq!(ve("$b'y' $p($b'o', $b'x')"), "$false");
    assert_eq!(ve("$b'y' $p('o', 'x')"), "$false");
    assert_eq!(ve("\"foobar\"   $p($b'o', $b'x')"), "\"fxxbar\"");
    assert_eq!(ve("\"foobar\"   $p('o', 'x')"), "\"fxxbar\"");
    assert_eq!(ve("$b\"foobar\" $p($b'o', $b'x')"), "$b\"fxxbar\"");
    assert_eq!(ve("$b\"foobar\" $p('o', 'x')"), "$b\"fxxbar\"");
    assert_eq!(ve("std:str:len $b'\x42'"), "1");
    assert_eq!(ve("std:char:to_uppercase 'x'"), "\'X\'");
    assert_eq!(ve("std:char:to_uppercase 'x'"), "\'X\'");
    assert_eq!(ve("std:char:to_lowercase $b'X'"), "\'x\'");
    assert_eq!(ve("std:char:to_lowercase $b'X'"), "\'x\'");
    assert_eq!(ve("$p(0, 'z') \"abze\""), "2");
    assert_eq!(ve("$p(2, 'z') \"abze\""), "2");
    assert_eq!(ve("$p(3, 'z') \"abze\""), "$n");
    assert_eq!(ve("$p(0, $b'z') \"abze\""), "2");
    assert_eq!(ve("$p(2, $b'z') \"abze\""), "2");
    assert_eq!(ve("$p(3, $b'z') \"abze\""), "$n");
    assert_eq!(ve("$p(0, 'z') $b\"abze\""), "2");
    assert_eq!(ve("$p(2, 'z') $b\"abze\""), "2");
    assert_eq!(ve("$p(3, 'z') $b\"abze\""), "$n");
    assert_eq!(ve("$p(0, $b'z') $b\"abze\""), "2");
    assert_eq!(ve("$p(2, $b'z') $b\"abze\""), "2");
    assert_eq!(ve("$p(3, $b'z') $b\"abze\""), "$n");
    assert_eq!(ve("$p('a', 'z') 'a'"), "$true");
    assert_eq!(ve("$p('a', 'z') 'A'"), "$false");
    assert_eq!(ve("$p('a', 'z') $b'a'"), "$true");
    assert_eq!(ve("$p($b'a', $b'z') 'm'"), "$true");
    assert_eq!(ve("$p($b'a', $b'z') $b'm'"), "$true");
    assert_eq!(ve("$p($b'a', $b'z') $b'a'"), "$true");
    assert_eq!(ve("$p(0, 100) '\\x01'"), "$true");
    assert_eq!(ve("$p(0, 100) '\\xFF'"), "$false");
    assert_eq!(ve("$i(0, 100) '\\x01'"), "$true");
    assert_eq!(ve("$i(0, 100) '\\xFF'"), "$false");
    assert_eq!(ve("$p(0, 100) $b'\\x01'"), "$true");
    assert_eq!(ve("$p(0, 100) $b'\\xFF'"), "$false");
    assert_eq!(ve("$i(0, 100) $b'\\x01'"), "$true");
    assert_eq!(ve("$i(0, 100) $b'\\xFF'"), "$false");
    assert_eq!(ve("$p('a', 4)[]"), "\"aaaa\"");
    assert_eq!(ve("$p('Ä', 4)[]"), "\"ÄÄÄÄ\"");
    assert_eq!(ve("$p($b'a', 4)[]"), "$b\"aaaa\"");
    assert_eq!(ve("$p($b'Ä', 4)[]"), "$b\"\\xC4\\xC4\\xC4\\xC4\"");
    assert_eq!(ve("$p(4, 'a')[]"), "\"aaaa\"");
    assert_eq!(ve("$p(4, 'Ä')[]"), "\"ÄÄÄÄ\"");
    assert_eq!(ve("$p(4, $b'a')[]"), "$b\"aaaa\"");
    assert_eq!(ve("$p(4, $b'Ä')[]"), "$b\"\\xC4\\xC4\\xC4\\xC4\"");
}

#[test]
fn check_concat() {
    assert_eq!(ve("'X'     \"Y\" $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b'X'   \"Y\" $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("\"X\"   \"Y\" $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b\"X\" \"Y\" $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("'X'     $b\"Y\" $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b'X'   $b\"Y\" $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("\"X\"   $b\"Y\" $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b\"X\" $b\"Y\" $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("'X'     'Y' $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b'X'   'Y' $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("\"X\"   'Y' $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b\"X\" 'Y' $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("'X'     $b'Y' $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b'X'   $b'Y' $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
    assert_eq!(ve("\"X\"   $b'Y' $b\"Z\" 'U' $b'O' 20"), "\"XYZUO20\"");
    assert_eq!(ve("$b\"X\" $b'Y' $b\"Z\" 'U' $b'O' 20"), "$b\"XYZUO20\"");
}

#[test]
#[cfg(feature = "serde_json")]
fn check_json_char_byte() {
    assert_eq!(ve("std:ser:json $['x',$b'x'] $t"), "\"[\\\"x\\\",120]\"");
}

#[test]
#[cfg(feature = "toml")]
fn check_toml() {
    assert_eq!(ve("std:ser:toml ${a = $[1,2,3]}"), "\"a = [\\n    1,\\n    2,\\n    3,\\n]\\n\"");
    assert_eq!(ve("std:ser:toml ${a = $[\"fooo\"]}"), "\"a = [\\'fooo\\']\\n\"");
    assert_eq!(
        ve("std:ser:toml ${x=${a = $[1,2,3]}}"),
        "\"[x]\\na = [\\n    1,\\n    2,\\n    3,\\n]\\n\""
    );
    assert_eq!(
        ve("std:ser:toml ${x=${c = $[123, $true]}}"),
        "\"[x]\\nc = [\\n    123,\\n    true,\\n]\\n\""
    );

    assert_eq!(
        ve("std:deser:toml $q{\
        [foo]
        x = true
    }"),
        "${foo=${x=$true}}"
    );

    assert_eq!(
        ve("std:deser:toml $q{\
        [foo]
        x = 1.2
    }"),
        "${foo=${x=1.2}}"
    );

    assert_eq!(
        ve("std:deser:toml $q{\
        [foo]
        x = \"XXXX\"
    }"),
        "${foo=${x=\"XXXX\"}}"
    );
}

#[test]
fn check_mutate_point() {
    assert_eq!(ve("$i(0,0).0 = 3"), "EXEC ERR: Caught Panic: Can't mutate integer vector");
    assert_eq!(ve("$f(0,0).0 = 3"), "EXEC ERR: Caught Panic: Can't mutate float vector");
    assert_eq!(ve("$p(0,0).0 = 3"), "EXEC ERR: Caught Panic: Can't mutate pair");
}

#[test]
fn check_udp1() {
    let thrd = std::thread::spawn(move || {
        ve(r#"
            !soc = std:net:udp:new "0.0.0.0:31889";

            iter _ 0 => 10 {
                std:net:udp:send soc $b"FOOBAR" "127.0.0.1:31888";
                std:thread:sleep $p(:ms, 100);
            };
        "#);
    });

    assert_eq!(
        ve(r#"
        !soc = std:net:udp:new "0.0.0.0:31888";
        std:net:udp:recv soc;
    "#),
        "$p($b\"FOOBAR\",\"127.0.0.1:31889\")"
    );

    thrd.join().unwrap();
}

#[test]
fn check_udp2() {
    let thrd = std::thread::spawn(move || {
        ve(r#"
            std:thread:sleep $p(:ms, 400);
            !soc = std:net:udp:new "0.0.0.0:31819" "127.0.0.1:31818";

            iter _ 0 => 10 {
                std:net:udp:send soc $b"FOOBAR";
                std:thread:sleep $p(:ms, 100);
            };
        "#);
    });

    assert_eq!(
        ve(r#"
        !soc = std:net:udp:new "0.0.0.0:31818";
        std:net:udp:recv soc;
    "#),
        "$p($b\"FOOBAR\",\"127.0.0.1:31819\")"
    );

    thrd.join().unwrap();
}

#[test]
fn check_map() {
    assert_eq!(ve("map { _ * 10 } $[1,2,3,4]"), "$[10,20,30,40]");
    assert_eq!(ve("map { _ * 10 } ~ $iter (1 => 5)"), "$[10,20,30,40]");
    assert_eq!(ve("map { _ 'x' } \"abcdef\""), "$[\"ax\",\"bx\",\"cx\",\"dx\",\"ex\",\"fx\"]");
}

#[test]
fn check_filter() {
    assert_eq!(ve("filter { _ % 2 == 0 } $[1,2,3,4]"), "$[2,4]");
    assert_eq!(ve("filter { _ % 2 == 0 } ~ $iter 0 => 10"), "$[0,2,4,6,8]");
    assert_eq!(ve("filter { _ >= 'e' } \"abcdef\""), "$['e','f']");
    assert_eq!(ve("map { _ * 10 } ~ filter { _ % 2 == 0 } 0 => 10"), "$[0,20,40,60,80]");

    assert_eq!(ve("filter { @.0 % 2 == 0 } ${a = 4}"), "$[$p(4,\"a\")]");
    assert_eq!(ve("filter { @.1 == \"a\" } ${b = 3, a = 2}"), "$[$p(2,\"a\")]");
    assert_eq!(ve("map { @.0 * 3 } ${a = 2}"), "$[6]");
    assert_eq!(ve("map { @.1 }     ${a = 2}"), "$[\"a\"]");
}

#[test]
fn check_iter_arg_bug() {
    assert_eq!(ve("std:str:from_char_vec $iter $b\"abc\""), "\"abc\"");
    assert_eq!(ve("std:fold 1 { _ + _1 } $iter 1 => 5"), "11");
    assert_eq!(ve("map { _ * 2 } ~ $iter 0 => 4"), "$[0,2,4,6]");
    assert_eq!(ve("filter { _ % 2 == 0 } ~ $iter 1 => 10"), "$[2,4,6,8]");
    assert_eq!(ve("$@vec for ($iter 4 => 7) { $+ _ }"), "$[4,5,6]");
}

#[test]
fn check_iter_function_gen() {
    assert_eq!(
        ve(r#"
        !c = 0;
        !f = { .c = c + 1; if c > 10 { $o() } { $o(c) } };
        $@v iter i f { $+ i }
    "#),
        "$[1,2,3,4,5,6,7,8,9,10]"
    );

    assert_eq!(
        ve(r#"
        !c = 0;
        !f = { .c = c + 1; if c > 10 { $o() } { $o(c) } };
        ($iter f)[]
    "#),
        "$o(1)"
    );

    assert_eq!(
        ve(r#"
        !c = 0;
        !f = { .c = c + 1; if c > 10 { $o() } { $o(c) } };
        $@v iter x $p(($iter 1 => 11), f) { $+ x }
    "#),
        "$[$p(1,1),$p(2,2),$p(3,3),$p(4,4),$p(5,5),$p(6,6),$p(7,7),$p(8,8),$p(9,9),$p(10,10)]"
    );

    assert_eq!(
        ve(r#"
        !c = 0;
        !f = { .c = c + 1; if c > 10 { $o() } { $o(c) } };
        $@v iter i ($iter f) { $+ i }
    "#),
        "$[1,2,3,4,5,6,7,8,9,10]"
    );
}

#[test]
fn parser_crash_bad_op() {
    assert_eq!(
        ve("!foo = { } !@export foo = foo;"),
        "PARSE ERROR: <compiler:s_eval>:1:12 Expected literal value, sub expression, block, key or identifier\nat code:\n1   | !@export foo = foo;\n");
}

#[test]
fn check_edit_distance() {
    // Adapted from https://github.com/febeling/edit-distance/blob/master/tests/tests.rs
    // APL-2.0 License
    assert_eq!(ve("std:str:edit_distance $q AAA $q ABA "), "1");
    assert_eq!(ve("std:str:edit_distance $q  $q kitten "), "6");
    assert_eq!(ve("std:str:edit_distance $q sitting $q  "), "7");
    assert_eq!(ve("std:str:edit_distance $q kitten $q sitting "), "3");
    assert_eq!(ve("std:str:edit_distance $q Tier $q Tor "), "2");
    assert_eq!(ve("std:str:edit_distance $q ☀☂☃☄ $q ☀☂☃☄ "), "0");
    assert_eq!(ve("std:str:edit_distance $q ☀☂☃☄ $q X☂☃☄ "), "1");
    assert_eq!(ve("std:str:edit_distance $q[ฎ ฏ ฐ] $q[a b c]"), "3");
}

#[test]
fn check_process_wait() {
    assert_eq!(
        ve(r#"
        !hdl = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "FOR /L %i IN (0, 1, 2) DO ping -n 2 127.0.0.1 & exit 20"
            ];
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "for i in `seq 0 10`; do echo $i; sleep 0.1; done; exit 20"
            ];
        };

        !res = std:process:wait hdl;

        std:assert ~ not res.success;
        std:assert_eq res.status 20;
    "#),
        "$true"
    );
}

#[test]
fn check_process_kill_wait() {
    assert_eq!(
        ve(r#"
        !hdl = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "FOR /L %i IN (0, 1, 10) DO ping -n 2 127.0.0.1 & exit 20"
            ];
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "for i in `seq 0 10`; do echo $i; sleep 0.1; done; exit 20"
            ];
        };

        !res = std:process:kill_wait hdl;

        std:displayln res;

        std:assert ~ not res.success;
        if std:sys:os[] == "windows" {
            std:assert_eq res.status 1;
        } {
            std:assert_eq res.status -1;
        };
    "#),
        "$true"
    );
}

#[test]
fn check_process_try_wait() {
    assert_eq!(
        ve(r#"
        !hdl = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "FOR /L %i IN (0, 1, 10) DO ping -n 2 127.0.0.1 & exit 20"
            ];
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "for i in `seq 0 10`; do echo $i; sleep 0.1; done; exit 20"
            ];
        };

        !counter = 0;
        !ret = $none;
        while $true {
            std:thread:sleep :ms => 250;
            .counter += 1;

            .ret = unwrap ~ std:process:try_wait hdl;
            if ret {
                break ret;
            };
        };

        std:assert counter > 0;
        std:assert ~ not ret.success;
        std:assert ret.status == 20;
    "#),
        "$true"
    );
}

#[test]
fn check_buffered_io() {
    // checking read_line:
    assert_eq!(
        ve(r#"
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "echo TEST123"
            ] :io;
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "echo TEST123"
            ] :io;
        };

        !hdls = std:process:take_pipes proc;
        std:io:buffered:reader hdls.stdout;
        !lines = "";
        while $t {
            !line = std:io:read_line hdls.stdout;
            if len[line] == 0 {
                break[];
            };
            .lines = lines "(" (std:str:trim line) ")";
        };
        std:assert lines &> $r/\(TEST123\)/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );

    // checking read_until:
    assert_eq!(
        ve(r#"
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "echo TEST123"
            ] :io;
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "echo TEST123"
            ] :io;
        };

        !hdls = std:process:take_pipes proc;
        std:io:buffered:reader hdls.stdout;
        !lines = "";
        while $t {
            !line = std:io:read_until hdls.stdout "\n";
            if len[line] == 0 {
                break[];
            };
            .lines = lines "(" (std:str:trim line) ")";
        };
        std:assert lines &> $r/\(TEST123\)/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );
}

#[test]
fn check_process_io() {
    // checking take_pipes and read_all:
    assert_eq!(
        ve(r#"
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "echo TEST123"
            ] :io;
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "echo TEST123"
            ] :io;
        };

        !hdls = std:process:take_pipes proc;
        !output = std:io:read_all hdls.stdout;
        std:assert output &> $r/TEST123/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );

    // checking take_pipes and read_some:
    assert_eq!(
        ve(r#"
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "echo TEST123"
            ] :io;
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "echo TEST123"
            ] :io;
        };

        !hdls = std:process:take_pipes proc;
        !output = unwrap ~ std:io:read_some hdls.stdout;
        std:assert output &> $r/TEST123/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );

    // checking take_pipes and read_all on stderr:
    assert_eq!(
        ve(r#"
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $[
                "/C", "echo TEST123 1>&2"
            ] :ie;
        } {
            unwrap ~ std:process:spawn "bash" $[
                "-c", "echo TEST123 1>&2"
            ] :ie;
        };

        !hdls = std:process:take_pipes proc;
        !output = unwrap ~ std:io:read_all hdls.stderr;
        std:assert output &> $r/TEST123/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );

    // checking take_pipes, read_all and write:
    assert_eq!(
        ve(r#"
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $["/Q"] :ioe;
        } {
            unwrap ~ std:process:spawn "bash" $[] :ioe;
        };

        !hdls = std:process:take_pipes proc;
        std:io:write hdls.stdin "echo TEST123\r\n";
        std:io:flush hdls.stdin;
        hdls.stdin = $n;
        !output = unwrap ~ std:io:read_all hdls.stdout;
        std:assert output &> $r/TEST123/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );

    // checking take_pipes, read_all on stderr and write:
    assert_eq!(
        ve(r#"
        !nl = if std:sys:os[] == "windows" "\r\n" "\n";
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $["/Q"] :ioe;
        } {
            unwrap ~ std:process:spawn "bash" $[] :ioe;
        };

        !hdls = std:process:take_pipes proc;
        std:io:write hdls.stdin ("echo TEST123 1>&2" nl);
        std:io:flush hdls.stdin;
        hdls.stdin = $n;
        !output = unwrap ~ std:io:read_all hdls.stderr;
        std:assert output &> $r/TEST123/ "Found echo output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );

    // checking take_pipes, threaded read_some on stdout and write_some:
    assert_eq!(
        ve(r#"
        !nl = if std:sys:os[] == "windows" "\r\n" "\n";
        !proc = if std:sys:os[] == "windows" {
            unwrap ~ std:process:spawn "cmd.exe" $["/Q"] :ioe;
        } {
            unwrap ~ std:process:spawn "bash" $[] :ioe;
        };

        !hdls = std:process:take_pipes proc;

        !th = std:thread:spawn $code{
            _READY.send :ok;
            !text = "";
            while $t {
                !rd = std:io:read_some stdout;
                if is_none[rd] {
                    break[];
                } {
                    if len[rd] > 0 {
                        std:displayln "READ:" rd;
                        .text = text unwrap[rd];
                    };
                }
            };

            $rs/$?\r\n/ text { "{CRLF}" }
        } ${ stdout = hdls.stdout };
        std:assert_eq th.recv_ready[] :ok;

        std:io:write_some hdls.stdin "echo ";
        std:io:flush hdls.stdin;
        std:thread:sleep :ms => 500;
        std:io:write_some hdls.stdin ("TEST123" nl);
        std:io:flush hdls.stdin;
        std:io:write hdls.stdin ("exit" nl);
        std:io:flush hdls.stdin;

        !thrdout = th.join[];
        std:assert thrdout &> $r/TEST123/ "Found echo output in thread output";

        !ret = unwrap ~ std:process:wait proc;
        ret.success
    "#),
        "$true"
    );
}

#[test]
fn check_div_zero() {
    assert_eq!(ve("1 % 0"),
        "EXEC ERR: Caught Panic: Remainder with divisor of 0: 1%0\n    <compiler:s_eval>:1:3 BinOpMod [1, 0]\n");
    assert_eq!(ve("1.0 % 0.0"), "NaN");
    assert_eq!(
        ve("1 / 0"),
        "EXEC ERR: Caught Panic: Division by 0: 1/0\n    <compiler:s_eval>:1:3 BinOpDiv [1, 0]\n"
    );
    assert_eq!(ve("1.0 / 0.0"), "inf");
    assert_eq!(ve("`%` 1 0"),
        "EXEC ERR: Caught Panic: Remainder with divisor by 0\n        [1, 0]\n    <compiler:s_eval>:1:5 Call [1, 0]\n");
    assert_eq!(ve("`/` 1 0"),
        "EXEC ERR: Caught Panic: Division by 0\n        [1, 0]\n    <compiler:s_eval>:1:5 Call [1, 0]\n");
}

#[test]
fn check_std_wlambda_parse() {
    assert_eq!(ve("std:wlambda:parse $code{ 1 + 2 }"), "$[$%:Block,$[$%:BinOpAdd,1,2]]");
    assert_eq!(
        ve("std:syn:pos ~ 0 ~ 2 ~ 1 ~ std:wlambda:parse $code{ !f = { 1 + 2 } } :x"),
        "$[\"x\",1,6]"
    );
    assert_eq!(
        ve("std:syn:type ~ 0 ~ 2 ~ 1 ~ std:wlambda:parse $code{ !f = { 1 + 2 } } :x"),
        ":Func"
    );
    assert_eq!(ve("$%:Func"), "$%:Func");
    assert_eq!(ve("std:syn:pos $%:Func"), "$[\"<compiler:s_eval>\",1,14]");
    assert_eq!(ve("std:syn:type $%:Func"), ":Func");

    assert_eq!(
        ve(r#"
        match (std:wlambda:parse $code{ o 1 2 })
            $[$%:Block, $[$%:Call, fun, arg1, arg2]] => {
                $[$\.fun, $\.arg1, $\.arg2]
            };
    "#),
        "$[$[$%:Var,:o],1,2]"
    );
}

#[test]
fn check_parse_error_sym_start() {
    assert_eq!(
        ve(":"),
        "PARSE ERROR: <compiler:s_eval>:1:2 EOF while parsing: identifier\nat code:\n1   | \n"
    );
}

#[test]
#[cfg(feature = "mqtt")]
fn check_mqtt() {
    assert_eq!(
        ve(r#"
        !broker = std:mqtt:broker:new ${
            listen         = "0.0.0.0:1889",
            console_listen = "0.0.0.0:18089",
        };

        # sleep a bit until the broker is initialized:
        std:thread:sleep :ms => 900;

        !chan = std:sync:mpsc:new[];
        !cl = std:mqtt:client:new chan "test1" "localhost" 1889;

        # let it connect:
        std:thread:sleep :ms => 200;

        !_ = cl.subscribe "test/me";
        !_ = cl.publish "test/me" $b"test123\xFF";

        std:assert_str_eq chan.recv[] $p(:"$WL/connected", $n);
        std:assert_str_eq chan.recv[] $p(:"$WL/subscribed", $n);
        std:assert_str_eq chan.recv[] $p("test/me", $b"test123\xFF");
        1
    "#),
        "1"
    );
}

#[test]
#[cfg(feature = "mqtt")]
fn check_mqtt_broker_link1() {
    assert_eq!(
        ve(r#"
        !broker = std:mqtt:broker:new ${
        name = "X",
            listen         = "0.0.0.0:8884",
            console_listen = "0.0.0.0:18191",
        };

        # sleep a bit until the broker is initialized:
        std:thread:sleep :ms => 900;

        !chan = std:sync:mpsc:new[];
        !cl = std:mqtt:client:new chan "test1" "localhost" 8884;

        # let it connect:
        std:thread:sleep :ms => 500;

        !_ = cl.subscribe "test/me";

        std:assert_str_eq chan.recv[] $p(:"$WL/connected", $n);
        std:assert_str_eq chan.recv[] $p(:"$WL/subscribed", $n);

        !_ = broker.publish "test/me" $b"test123\xFF";
        std:assert_str_eq chan.recv[] $p("test/me", $b"test123\xFF");
        1
    "#),
        "1"
    );
}

#[test]
#[cfg(feature = "mqtt")]
fn check_mqtt_broker_link2() {
    assert_eq!(
        ve(r#"
        !chan = std:sync:mpsc:new[];
        !broker = std:mqtt:broker:new ${
            listen         = "0.0.0.0:1885",
            console_listen = "0.0.0.0:18082",
            link = ${
                recv   = chan,
                topics = $["foo", "x"],
            },
        };

        # sleep a bit until the broker is initialized:
        std:thread:sleep :ms => 900;

        !cl = std:mqtt:client:new chan "test1" "localhost" 1885;

        # let it connect:
        std:thread:sleep :ms => 500;
        !_ = cl.publish "foo" $b"a";
        !_ = cl.publish "x" $b"b";
        !_ = cl.publish "y" $b"c";
        !_ = cl.publish "x" $b"d";

        !next_non_wl = {
            !(topic, payload) = chan.recv[];
            while ($p(0, 1) (str topic)) == "$" {
                .(topic, payload) = chan.recv[];
            };
            $p(topic, payload)
        };

        std:assert_str_eq next_non_wl[] $p("foo", $b"a");
        std:assert_str_eq next_non_wl[] $p("x",   $b"b");
        std:assert_str_eq next_non_wl[] $p("x",   $b"d");
        1
    "#),
        "1"
    );
}

#[test]
#[cfg(feature = "mqtt")]
fn check_mqtt_client_loop() {
    assert_eq!(
        ve(r#"
        !broker = std:mqtt:broker:new ${
            listen         = "0.0.0.0:1883",
            console_listen = "0.0.0.0:18080",
        };

        # sleep a bit until the broker is initialized:
        std:thread:sleep :ms => 500;

        !chan = std:sync:mpsc:new[];
        !cl = std:mqtt:client:new chan "test1" "localhost" 1883;

        # let it connect:
        std:thread:sleep :ms => 200;

        !_ = cl.subscribe "test/me";
        !_ = cl.publish "test/me" $b"test123\xFF";
        !_ = cl.publish "test/me" $b"quit";

        !got_some_stuff = $n;

        while $t {
            match chan.recv[]
                $p(topic, $b"quit") => { break[]; }
                $p(topic, data)     => { .got_some_stuff = std:copy $\; }; # std:copy because $\ is changing!
        };

        $p(got_some_stuff.topic, got_some_stuff.data)
    "#),
        "$p(\"test/me\",$b\"test123\\xFF\")"
    );
}

#[test]
#[cfg(feature = "http")]
fn check_http_get() {
    assert_eq!(
        ve(r#"
        (std:thread:spawn $code{
            _READY.send $t;

            unwrap ~ std:net:tcp:listen "0.0.0.0:9291" {!(socket) = @;
                !buf = $b"";
                !done = $f;
                while not[done] {
                    match std:io:read_some[socket]
                        $o(buf) => { .buf = buf +> $\.buf; }
                        $o()    => { .done = $t; }
                        ($e _)  => { .done = $t; };
                    if buf &> $r%GET\ \/\ HTTP\/1.1% {
                        .done = $t;
                    };
                };

                std:io:write socket "HTTP/1.1 200 OK\r\n\r\nTest 123";
            };
        }).recv_ready[];

        !client = std:http:client:new[];
        !response = std:http:get client "http://127.0.0.1:9291";

        !body = std:str:from_utf8_lossy response.body;
        std:assert_eq response.status 200;
        body
    "#),
        "\"Test 123\""
    );
}

#[test]
#[cfg(feature = "http")]
fn check_http_post() {
    assert_eq!(ve(r#"
        (std:thread:spawn $code{
            _READY.send $t;

            unwrap ~ std:net:tcp:listen "0.0.0.0:9292" {!(socket) = @;
                !buf = $b"";
                !done = $f;
                while not[done] {
                    match std:io:read_some[socket]
                        $o(buf) => { .buf = buf +> $\.buf; }
                        $o()    => { .done = $t; }
                        ($e _)  => { .done = $t; };
                    if buf &> $r%XXX% {
                        .done = $t;
                    };
                };

                std:io:write socket ("HTTP/1.1 200 OK\r\n\r\nTest 567" buf);
            };
        }).recv_ready[];

        !client = std:http:client:new[];
        !response = std:http:post client "http://127.0.0.1:9292" "XXX";

        !body = std:str:from_utf8_lossy response.body;
        std:assert_eq response.status 200;
        body
    "#),
    "\"Test 567POST / HTTP/1.1\\r\\ncontent-length: 3\\r\\naccept: */*\\r\\nhost: 127.0.0.1:9292\\r\\n\\r\\nXXX\"");
}

#[test]
#[cfg(feature = "http")]
fn check_http_request() {
    assert_eq!(ve(r#"
        (std:thread:spawn $code{
            _READY.send $t;

            unwrap ~ std:net:tcp:listen "0.0.0.0:9295" {!(socket) = @;
                !buf = $b"";
                !done = $f;
                while not[done] {
                    match std:io:read_some[socket]
                        $o(buf) => { .buf = buf +> $\.buf; }
                        $o()    => { .done = $t; }
                        ($e _)  => { .done = $t; };
                    if buf &> $r%YYY% {
                        .done = $t;
                    };
                };

                std:io:write socket ("HTTP/1.1 200 OK\r\n\r\nTest 567" buf);
            };
        }).recv_ready[];

        !client = std:http:client:new[];
        !response = std:http:request client :PUT "http://127.0.0.1:9295" "YYY" ${
            @basic_auth  = $["USER", "PW"],
            @query = ${
                fo = "foo/?&BAR",
            },
        };

        !body = std:str:from_utf8_lossy response.body;
        std:assert_eq response.status 200;
        body
    "#),
    "\"Test 567PUT /?fo=foo%2F%3F%26BAR HTTP/1.1\\r\\nauthorization: Basic VVNFUjpQVw==\\r\\ncontent-length: 3\\r\\naccept: */*\\r\\nhost: 127.0.0.1:9295\\r\\n\\r\\nYYY\"");

    assert_eq!(ve(r#"
        (std:thread:spawn $code{
            _READY.send $t;

            unwrap ~ std:net:tcp:listen "0.0.0.0:9299" {!(socket) = @;
                !buf = $b"";
                !done = $f;
                while not[done] {
                    match std:io:read_some[socket]
                        $o(buf) => { .buf = buf +> $\.buf; }
                        $o()    => { .done = $t; }
                        ($e _)  => { .done = $t; };
                    if buf &> $r%YYY% {
                        .done = $t;
                    };
                };

                std:io:write socket ("HTTP/1.1 200 OK\r\n\r\nTest 567" buf);
            };
        }).recv_ready[];

        !client = std:http:client:new[];
        !response = std:http:request client :PUT "http://127.0.0.1:9299" "YYY" ${
            @bearer_auth = "TOKEN",
        };

        !body = std:str:from_utf8_lossy response.body;
        std:assert_eq response.status 200;
        body
    "#),
    "\"Test 567PUT / HTTP/1.1\\r\\nauthorization: Bearer TOKEN\\r\\ncontent-length: 3\\r\\naccept: */*\\r\\nhost: 127.0.0.1:9299\\r\\n\\r\\nYYY\"");

    assert_eq!(ve(r#"
        (std:thread:spawn $code{
            _READY.send $t;

            unwrap ~ std:net:tcp:listen "0.0.0.0:9294" {!(socket) = @;
                !buf = $b"";
                !done = $f;
                while not[done] {
                    match std:io:read_some[socket]
                        $o(buf) => { .buf = buf +> $\.buf; }
                        $o()    => { .done = $t; }
                        ($e _)  => { .done = $t; };
                    if buf &> $r%YYY% {
                        .done = $t;
                    };
                };

                std:io:write socket ("HTTP/1.1 200 OK\r\n\r\nTest 567" buf);
            };
        }).recv_ready[];

        !client = std:http:client:new[];
        !response = std:http:request client :PUT "http://127.0.0.1:9294" "YYY" ${
            XXX = 123,
        };

        !body = std:str:from_utf8_lossy response.body;
        std:assert_eq response.status 200;
        body
    "#),
    "\"Test 567PUT / HTTP/1.1\\r\\nxxx: 123\\r\\ncontent-length: 3\\r\\naccept: */*\\r\\nhost: 127.0.0.1:9294\\r\\n\\r\\nYYY\"");
}

#[test]
fn check_some_none_err_or() {
    assert_eq!(ve("$n     // 10"), "10");
    assert_eq!(ve("$o()   // 10"), "10");
    assert_eq!(ve("$o(20) // 10"), "20");
    assert_eq!(ve("$false // 10"), "$false");
    assert_eq!(ve("is_err ~ ($e 1) // 10"), "$true");

    assert_eq!(ve("$n     /? 10"), "10");
    assert_eq!(ve("$o()   /? 10"), "10");
    assert_eq!(ve("$o(20) /? 10"), "20");
    assert_eq!(ve("$false /? 10"), "$false");
    assert_eq!(ve("($e 1) /? 10"), "10");

    assert_eq!(ve("$n     /$n 10"), "10");
    assert_eq!(ve("$o()   /$n 10"), "$o()");
    assert_eq!(ve("$o(20) /$n 10"), "$o(20)");
    assert_eq!(ve("$false /$n 10"), "$false");
    assert_eq!(ve("is_err ~ ($e 1) /$n 10"), "$true");

    assert_eq!(ve("$n     /$o 10"), "$n");
    assert_eq!(ve("$o()   /$o 10"), "10");
    assert_eq!(ve("$o(20) /$o 10"), "20");
    assert_eq!(ve("$false /$o 10"), "$false");
    assert_eq!(ve("is_err ~ ($e 1) /$o 10"), "$true");

    assert_eq!(ve("$n     /$e 10"), "$n");
    assert_eq!(ve("$o()   /$e 10"), "$o()");
    assert_eq!(ve("$o(20) /$e 10"), "$o(20)");
    assert_eq!(ve("$false /$e 10"), "$false");
    assert_eq!(ve("($e 1) /$e 10"), "10");

    assert_eq!(ve("$n     /$e 10 // 33"), "33");
    assert_eq!(ve("$o()   /$e 10 // 33"), "33");
    assert_eq!(ve("$o(20) /$e 10 // 33"), "20");
    assert_eq!(ve("$false /$e 10 // 33"), "$false");
    assert_eq!(ve("($e 1) /$e 10 // 33"), "10");

    assert_eq!(ve("$n     /$e 10 /? 33"), "33");
    assert_eq!(ve("$o()   /$e 10 /? 33"), "33");
    assert_eq!(ve("$o(20) /$e 10 /? 33"), "20");
    assert_eq!(ve("$false /$e 10 /? 33"), "$false");
    assert_eq!(ve("($e 1) /$e 10 /? 33"), "10");

    assert_eq!(ve("`//` $n     10"), "10");
    assert_eq!(ve("`//` $o()   10"), "10");
    assert_eq!(ve("`//` $o(20) 10"), "20");
    assert_eq!(ve("`//` $false 10"), "$false");
    assert_eq!(ve("is_err ~ `//` ($e 1) 10"), "$true");

    assert_eq!(ve("`/?` $n     10"), "10");
    assert_eq!(ve("`/?` $o()   10"), "10");
    assert_eq!(ve("`/?` $o(20) 10"), "20");
    assert_eq!(ve("`/?` $false 10"), "$false");
    assert_eq!(ve("`/?` ($e 1) 10"), "10");

    assert_eq!(ve("`/$n` $n     10"), "10");
    assert_eq!(ve("`/$n` $o()   10"), "$o()");
    assert_eq!(ve("`/$n` $o(20) 10"), "$o(20)");
    assert_eq!(ve("`/$n` $false 10"), "$false");
    assert_eq!(ve("is_err ~ `/$n` ($e 1) 10"), "$true");

    assert_eq!(ve("`/$o` $n     10"), "$n");
    assert_eq!(ve("`/$o` $o()   10"), "10");
    assert_eq!(ve("`/$o` $o(20) 10"), "20");
    assert_eq!(ve("`/$o` $false 10"), "$false");
    assert_eq!(ve("is_err ~ `/$o` ($e 1) 10"), "$true");

    assert_eq!(ve("`/$e` $n     10"), "$n");
    assert_eq!(ve("`/$e` $o()   10"), "$o()");
    assert_eq!(ve("`/$e` $o(20) 10"), "$o(20)");
    assert_eq!(ve("`/$e` $false 10"), "$false");
    assert_eq!(ve("`/$e` ($e 1) 10"), "10");
}

#[test]
fn check_some_none_err_or_short_circuit() {
    assert_eq!(ve("!x = 99; !r = { .x = $n     // return[-1]; 2 }[]; $p(r, x)"), "$p(-1,99)");
    assert_eq!(ve("!x = 99; !r = { .x = $o()   // return[-1]; 2 }[]; $p(r, x)"), "$p(-1,99)");
    assert_eq!(ve("!x = 99; !r = { .x = $o(20) // return[-1]; 2 }[]; $p(r, x)"), "$p(2,20)");
    assert_eq!(
        ve("!x = 99; !r = { .x = ($e 1) // return[-1]; 2 }[]; $p(r, is_err x)"),
        "$p(2,$true)"
    );
    assert_eq!(ve("!x = 99; !r = { .x = 200    // return[-1]; 2 }[]; $p(r, x)"), "$p(2,200)");

    assert_eq!(ve("!x = 99; !r = { .x = $n     /? return[-1]; 2 }[]; $p(r, x)"), "$p(-1,99)");
    assert_eq!(ve("!x = 99; !r = { .x = $o()   /? return[-1]; 2 }[]; $p(r, x)"), "$p(-1,99)");
    assert_eq!(ve("!x = 99; !r = { .x = $o(20) /? return[-1]; 2 }[]; $p(r, x)"), "$p(2,20)");
    assert_eq!(
        ve("!x = 99; !r = { .x = ($e 1) /? return[-1]; 2 }[]; $p(r, is_err x)"),
        "$p(-1,$false)"
    );
    assert_eq!(ve("!x = 99; !r = { .x = 200    /? return[-1]; 2 }[]; $p(r, x)"), "$p(2,200)");

    assert_eq!(ve("!x = 99; !r = { .x = $n     /$n return[-1]; 2 }[]; $p(r, x)"), "$p(-1,99)");
    assert_eq!(ve("!x = 99; !r = { .x = $o()   /$n return[-1]; 2 }[]; $p(r, x)"), "$p(2,$o())");
    assert_eq!(ve("!x = 99; !r = { .x = $o(20) /$n return[-1]; 2 }[]; $p(r, x)"), "$p(2,$o(20))");
    assert_eq!(
        ve("!x = 99; !r = { .x = ($e 1) /$n return[-1]; 2 }[]; $p(r, is_err x)"),
        "$p(2,$true)"
    );
    assert_eq!(ve("!x = 99; !r = { .x = 200    /$n return[-1]; 2 }[]; $p(r, x)"), "$p(2,200)");

    assert_eq!(ve("!x = 99; !r = { .x = $n     /$e return[-1]; 2 }[]; $p(r, x)"), "$p(2,$n)");
    assert_eq!(ve("!x = 99; !r = { .x = $o()   /$e return[-1]; 2 }[]; $p(r, x)"), "$p(2,$o())");
    assert_eq!(ve("!x = 99; !r = { .x = $o(20) /$e return[-1]; 2 }[]; $p(r, x)"), "$p(2,$o(20))");
    assert_eq!(
        ve("!x = 99; !r = { .x = ($e 1) /$e return[-1]; 2 }[]; $p(r, is_err x)"),
        "$p(-1,$false)"
    );
    assert_eq!(ve("!x = 99; !r = { .x = 200    /$e return[-1]; 2 }[]; $p(r, x)"), "$p(2,200)");

    assert_eq!(ve("!x = 99; !r = { .x = $n     /$o return[-1]; 2 }[]; $p(r, x)"), "$p(2,$n)");
    assert_eq!(ve("!x = 99; !r = { .x = $o()   /$o return[-1]; 2 }[]; $p(r, x)"), "$p(-1,99)");
    assert_eq!(ve("!x = 99; !r = { .x = $o(20) /$o return[-1]; 2 }[]; $p(r, x)"), "$p(2,20)");
    assert_eq!(
        ve("!x = 99; !r = { .x = ($e 1) /$o return[-1]; 2 }[]; $p(r, is_err x)"),
        "$p(2,$true)"
    );
    assert_eq!(ve("!x = 99; !r = { .x = 200    /$o return[-1]; 2 }[]; $p(r, x)"), "$p(2,200)");
}

#[test]
fn check_prelude_deflate() {
    if cfg!(feature = "flate2") {
        assert_eq!(ve("len ~ std:bytes:deflate:encode $q'AAAAAAAAAAAAAAAA'"), "5");
        assert_eq!(
            ve("std:bytes:deflate:decode ~ std:bytes:deflate:encode $q'AAAAAAAAAAAAAAAA'"),
            "$b\"AAAAAAAAAAAAAAAA\""
        );
        assert_eq!(ve("len ~ std:bytes:zlib:encode $q'AAAAAAAAAAAAAAAAAAAAAAAAAA'"), "11");
        assert_eq!(
            ve("std:bytes:zlib:decode ~ std:bytes:zlib:encode $q'AAAAAAAAAAAAAAAAAAAAAAAAAA'"),
            "$b\"AAAAAAAAAAAAAAAAAAAAAAAAAA\""
        );
        assert_eq!(ve("len ~ std:bytes:gzip:encode $q'AAAAAAAAAAAAAAAAAAAAAAAAAA'"), "23");
        assert_eq!(
            ve("std:bytes:gzip:decode ~ std:bytes:gzip:encode $q'AAAAAAAAAAAAAAAAAAAAAAAAAA'"),
            "$b\"AAAAAAAAAAAAAAAAAAAAAAAAAA\""
        );
    }
}

#[test]
fn check_prelude_lzw() {
    assert_eq!(ve("len ~ std:bytes:lzw:encode $q'AAAAAAAAAAAAAAAAAAAAAAAAAA'"), "12");
    assert_eq!(
        ve("std:bytes:lzw:decode ~ std:bytes:lzw:encode $q'AAAAAAAAAAAAAAAAAAAAAAAAAA'"),
        "$b\"AAAAAAAAAAAAAAAAAAAAAAAAAA\""
    );
}

#[test]
fn check_chem() {
    assert_eq!(ve("!c = std:chem:parse :FeH2O; c.first_elem_info[].atomic_number"), "26");
    assert_eq!(
        ve(r#"
        !sum_formula = std:chem:parse "c14 (h19)2 n o2 cl h";
        $@i sum_formula.for_each_element \$+ _2.atomic_mass * _1
    "#),
        "287"
    );
    assert_eq!(
        ve(r#"
        !sum_formula = std:chem:parse "c14 (h19)2 n o2 cl h";
        $@f sum_formula.for_each_element \$+ _2.atomic_mass * _1
    "#),
        "288.921"
    );

    assert_eq!(ve("std:chem:parse :H1"), "$<Chem:H>");
    assert_eq!(ve("std:chem:parse :H9"), "$<Chem:H9>");
    assert_eq!(ve("std:chem:parse :H"), "$<Chem:H>");
    assert_eq!(ve("std:chem:parse :He"), "$<Chem:He>");
    assert_eq!(ve("std:chem:parse :Li"), "$<Chem:Li>");
    assert_eq!(ve("std:chem:parse :Be"), "$<Chem:Be>");
    assert_eq!(ve("std:chem:parse :B"), "$<Chem:B>");
    assert_eq!(ve("std:chem:parse :C"), "$<Chem:C>");
    assert_eq!(ve("std:chem:parse :N"), "$<Chem:N>");
    assert_eq!(ve("std:chem:parse :O"), "$<Chem:O>");
    assert_eq!(ve("std:chem:parse :F"), "$<Chem:F>");
    assert_eq!(ve("std:chem:parse :Ne"), "$<Chem:Ne>");
    assert_eq!(ve("std:chem:parse :Na"), "$<Chem:Na>");
    assert_eq!(ve("std:chem:parse :Mg"), "$<Chem:Mg>");
    assert_eq!(ve("std:chem:parse :Al"), "$<Chem:Al>");
    assert_eq!(ve("std:chem:parse :Si"), "$<Chem:Si>");
    assert_eq!(ve("std:chem:parse :P"), "$<Chem:P>");
    assert_eq!(ve("std:chem:parse :S"), "$<Chem:S>");
    assert_eq!(ve("std:chem:parse :Cl"), "$<Chem:Cl>");
    assert_eq!(ve("std:chem:parse :Ar"), "$<Chem:Ar>");
    assert_eq!(ve("std:chem:parse :K"), "$<Chem:K>");
    assert_eq!(ve("std:chem:parse :Ca"), "$<Chem:Ca>");
    assert_eq!(ve("std:chem:parse :Sc"), "$<Chem:Sc>");
    assert_eq!(ve("std:chem:parse :Ti"), "$<Chem:Ti>");
    assert_eq!(ve("std:chem:parse :V"), "$<Chem:V>");
    assert_eq!(ve("std:chem:parse :Cr"), "$<Chem:Cr>");
    assert_eq!(ve("std:chem:parse :Mn"), "$<Chem:Mn>");
    assert_eq!(ve("std:chem:parse :Fe"), "$<Chem:Fe>");
    assert_eq!(ve("std:chem:parse :Co"), "$<Chem:Co>");
    assert_eq!(ve("std:chem:parse :Ni"), "$<Chem:Ni>");
    assert_eq!(ve("std:chem:parse :Cu"), "$<Chem:Cu>");
    assert_eq!(ve("std:chem:parse :Zn"), "$<Chem:Zn>");
    assert_eq!(ve("std:chem:parse :Ga"), "$<Chem:Ga>");
    assert_eq!(ve("std:chem:parse :Ge"), "$<Chem:Ge>");
    assert_eq!(ve("std:chem:parse :As"), "$<Chem:As>");
    assert_eq!(ve("std:chem:parse :Se"), "$<Chem:Se>");
    assert_eq!(ve("std:chem:parse :Br"), "$<Chem:Br>");
    assert_eq!(ve("std:chem:parse :Kr"), "$<Chem:Kr>");
    assert_eq!(ve("std:chem:parse :Rb"), "$<Chem:Rb>");
    assert_eq!(ve("std:chem:parse :Sr"), "$<Chem:Sr>");
    assert_eq!(ve("std:chem:parse :Y"), "$<Chem:Y>");
    assert_eq!(ve("std:chem:parse :Zr"), "$<Chem:Zr>");
    assert_eq!(ve("std:chem:parse :Nb"), "$<Chem:Nb>");
    assert_eq!(ve("std:chem:parse :Mo"), "$<Chem:Mo>");
    assert_eq!(ve("std:chem:parse :Tc"), "$<Chem:Tc>");
    assert_eq!(ve("std:chem:parse :Ru"), "$<Chem:Ru>");
    assert_eq!(ve("std:chem:parse :Rh"), "$<Chem:Rh>");
    assert_eq!(ve("std:chem:parse :Pd"), "$<Chem:Pd>");
    assert_eq!(ve("std:chem:parse :Ag"), "$<Chem:Ag>");
    assert_eq!(ve("std:chem:parse :Cd"), "$<Chem:Cd>");
    assert_eq!(ve("std:chem:parse :In"), "$<Chem:In>");
    assert_eq!(ve("std:chem:parse :Sn"), "$<Chem:Sn>");
    assert_eq!(ve("std:chem:parse :Sb"), "$<Chem:Sb>");
    assert_eq!(ve("std:chem:parse :Te"), "$<Chem:Te>");
    assert_eq!(ve("std:chem:parse :I"), "$<Chem:I>");
    assert_eq!(ve("std:chem:parse :Xe"), "$<Chem:Xe>");
    assert_eq!(ve("std:chem:parse :Cs"), "$<Chem:Cs>");
    assert_eq!(ve("std:chem:parse :Ba"), "$<Chem:Ba>");
    assert_eq!(ve("std:chem:parse :La"), "$<Chem:La>");
    assert_eq!(ve("std:chem:parse :Ce"), "$<Chem:Ce>");
    assert_eq!(ve("std:chem:parse :Pr"), "$<Chem:Pr>");
    assert_eq!(ve("std:chem:parse :Nd"), "$<Chem:Nd>");
    assert_eq!(ve("std:chem:parse :Pm"), "$<Chem:Pm>");
    assert_eq!(ve("std:chem:parse :Sm"), "$<Chem:Sm>");
    assert_eq!(ve("std:chem:parse :Eu"), "$<Chem:Eu>");
    assert_eq!(ve("std:chem:parse :Gd"), "$<Chem:Gd>");
    assert_eq!(ve("std:chem:parse :Tb"), "$<Chem:Tb>");
    assert_eq!(ve("std:chem:parse :Dy"), "$<Chem:Dy>");
    assert_eq!(ve("std:chem:parse :Ho"), "$<Chem:Ho>");
    assert_eq!(ve("std:chem:parse :Er"), "$<Chem:Er>");
    assert_eq!(ve("std:chem:parse :Tm"), "$<Chem:Tm>");
    assert_eq!(ve("std:chem:parse :Yb"), "$<Chem:Yb>");
    assert_eq!(ve("std:chem:parse :Lu"), "$<Chem:Lu>");
    assert_eq!(ve("std:chem:parse :Hf"), "$<Chem:Hf>");
    assert_eq!(ve("std:chem:parse :Ta"), "$<Chem:Ta>");
    assert_eq!(ve("std:chem:parse :W"), "$<Chem:W>");
    assert_eq!(ve("std:chem:parse :Re"), "$<Chem:Re>");
    assert_eq!(ve("std:chem:parse :Os"), "$<Chem:Os>");
    assert_eq!(ve("std:chem:parse :Ir"), "$<Chem:Ir>");
    assert_eq!(ve("std:chem:parse :Pt"), "$<Chem:Pt>");
    assert_eq!(ve("std:chem:parse :Au"), "$<Chem:Au>");
    assert_eq!(ve("std:chem:parse :Hg"), "$<Chem:Hg>");
    assert_eq!(ve("std:chem:parse :Tl"), "$<Chem:Tl>");
    assert_eq!(ve("std:chem:parse :Pb"), "$<Chem:Pb>");
    assert_eq!(ve("std:chem:parse :Bi"), "$<Chem:Bi>");
    assert_eq!(ve("std:chem:parse :Po"), "$<Chem:Po>");
    assert_eq!(ve("std:chem:parse :At"), "$<Chem:At>");
    assert_eq!(ve("std:chem:parse :Rn"), "$<Chem:Rn>");
    assert_eq!(ve("std:chem:parse :Fr"), "$<Chem:Fr>");
    assert_eq!(ve("std:chem:parse :Ra"), "$<Chem:Ra>");
    assert_eq!(ve("std:chem:parse :Ac"), "$<Chem:Ac>");
    assert_eq!(ve("std:chem:parse :Th"), "$<Chem:Th>");
    assert_eq!(ve("std:chem:parse :Pa"), "$<Chem:Pa>");
    assert_eq!(ve("std:chem:parse :U"), "$<Chem:U>");
    assert_eq!(ve("std:chem:parse :Np"), "$<Chem:Np>");
    assert_eq!(ve("std:chem:parse :Pu"), "$<Chem:Pu>");
    assert_eq!(ve("std:chem:parse :Am"), "$<Chem:Am>");
    assert_eq!(ve("std:chem:parse :Cm"), "$<Chem:Cm>");
    assert_eq!(ve("std:chem:parse :Bk"), "$<Chem:Bk>");
    assert_eq!(ve("std:chem:parse :Cf"), "$<Chem:Cf>");
    assert_eq!(ve("std:chem:parse :Es"), "$<Chem:Es>");
    assert_eq!(ve("std:chem:parse :Fm"), "$<Chem:Fm>");
    assert_eq!(ve("std:chem:parse :Md"), "$<Chem:Md>");
    assert_eq!(ve("std:chem:parse :No"), "$<Chem:No>");
    assert_eq!(ve("std:chem:parse :Lr"), "$<Chem:Lr>");
    assert_eq!(ve("std:chem:parse :Rf"), "$<Chem:Rf>");
    assert_eq!(ve("std:chem:parse :Db"), "$<Chem:Db>");
    assert_eq!(ve("std:chem:parse :Sg"), "$<Chem:Sg>");
    assert_eq!(ve("std:chem:parse :Bh"), "$<Chem:Bh>");
    assert_eq!(ve("std:chem:parse :Hs"), "$<Chem:Hs>");
    assert_eq!(ve("std:chem:parse :Mt"), "$<Chem:Mt>");
    assert_eq!(ve("std:chem:parse :Ds"), "$<Chem:Ds>");
    assert_eq!(ve("std:chem:parse :Rg"), "$<Chem:Rg>");
    assert_eq!(ve("std:chem:parse :Cn"), "$<Chem:Cn>");
    assert_eq!(ve("std:chem:parse :Nh"), "$<Chem:Nh>");
    assert_eq!(ve("std:chem:parse :Fl"), "$<Chem:Fl>");
    assert_eq!(ve("std:chem:parse :Mc"), "$<Chem:Mc>");
    assert_eq!(ve("std:chem:parse :Lv"), "$<Chem:Lv>");
    assert_eq!(ve("std:chem:parse :Ts"), "$<Chem:Ts>");
    assert_eq!(ve("std:chem:parse :Og"), "$<Chem:Og>");

    assert_eq!(ve("std:chem:parse :h"), "$<Chem:H>");
    assert_eq!(ve("std:chem:parse :he"), "$<Chem:He>");
    assert_eq!(ve("std:chem:parse :li"), "$<Chem:Li>");
    assert_eq!(ve("std:chem:parse :be"), "$<Chem:Be>");
    assert_eq!(ve("std:chem:parse :b"), "$<Chem:B>");
    assert_eq!(ve("std:chem:parse :c"), "$<Chem:C>");
    assert_eq!(ve("std:chem:parse :n"), "$<Chem:N>");
    assert_eq!(ve("std:chem:parse :o"), "$<Chem:O>");
    assert_eq!(ve("std:chem:parse :f"), "$<Chem:F>");
    assert_eq!(ve("std:chem:parse :ne"), "$<Chem:Ne>");
    assert_eq!(ve("std:chem:parse :na"), "$<Chem:Na>");
    assert_eq!(ve("std:chem:parse :mg"), "$<Chem:Mg>");
    assert_eq!(ve("std:chem:parse :al"), "$<Chem:Al>");
    assert_eq!(ve("std:chem:parse :si"), "$<Chem:Si>");
    assert_eq!(ve("std:chem:parse :p"), "$<Chem:P>");
    assert_eq!(ve("std:chem:parse :s"), "$<Chem:S>");
    assert_eq!(ve("std:chem:parse :cl"), "$<Chem:Cl>");
    assert_eq!(ve("std:chem:parse :ar"), "$<Chem:Ar>");
    assert_eq!(ve("std:chem:parse :k"), "$<Chem:K>");
    assert_eq!(ve("std:chem:parse :ca"), "$<Chem:Ca>");
    assert_eq!(ve("std:chem:parse :sc"), "$<Chem:Sc>");
    assert_eq!(ve("std:chem:parse :ti"), "$<Chem:Ti>");
    assert_eq!(ve("std:chem:parse :v"), "$<Chem:V>");
    assert_eq!(ve("std:chem:parse :cr"), "$<Chem:Cr>");
    assert_eq!(ve("std:chem:parse :mn"), "$<Chem:Mn>");
    assert_eq!(ve("std:chem:parse :fe"), "$<Chem:Fe>");
    assert_eq!(ve("std:chem:parse :co"), "$<Chem:Co>");
    assert_eq!(ve("std:chem:parse :ni"), "$<Chem:Ni>");
    assert_eq!(ve("std:chem:parse :cu"), "$<Chem:Cu>");
    assert_eq!(ve("std:chem:parse :zn"), "$<Chem:Zn>");
    assert_eq!(ve("std:chem:parse :ga"), "$<Chem:Ga>");
    assert_eq!(ve("std:chem:parse :ge"), "$<Chem:Ge>");
    assert_eq!(ve("std:chem:parse :as"), "$<Chem:As>");
    assert_eq!(ve("std:chem:parse :se"), "$<Chem:Se>");
    assert_eq!(ve("std:chem:parse :br"), "$<Chem:Br>");
    assert_eq!(ve("std:chem:parse :kr"), "$<Chem:Kr>");
    assert_eq!(ve("std:chem:parse :rb"), "$<Chem:Rb>");
    assert_eq!(ve("std:chem:parse :sr"), "$<Chem:Sr>");
    assert_eq!(ve("std:chem:parse :y"), "$<Chem:Y>");
    assert_eq!(ve("std:chem:parse :zr"), "$<Chem:Zr>");
    assert_eq!(ve("std:chem:parse :nb"), "$<Chem:Nb>");
    assert_eq!(ve("std:chem:parse :mo"), "$<Chem:Mo>");
    assert_eq!(ve("std:chem:parse :tc"), "$<Chem:Tc>");
    assert_eq!(ve("std:chem:parse :ru"), "$<Chem:Ru>");
    assert_eq!(ve("std:chem:parse :rh"), "$<Chem:Rh>");
    assert_eq!(ve("std:chem:parse :pd"), "$<Chem:Pd>");
    assert_eq!(ve("std:chem:parse :ag"), "$<Chem:Ag>");
    assert_eq!(ve("std:chem:parse :cd"), "$<Chem:Cd>");
    assert_eq!(ve("std:chem:parse :in"), "$<Chem:In>");
    assert_eq!(ve("std:chem:parse :sn"), "$<Chem:Sn>");
    assert_eq!(ve("std:chem:parse :sb"), "$<Chem:Sb>");
    assert_eq!(ve("std:chem:parse :te"), "$<Chem:Te>");
    assert_eq!(ve("std:chem:parse :i"), "$<Chem:I>");
    assert_eq!(ve("std:chem:parse :xe"), "$<Chem:Xe>");
    assert_eq!(ve("std:chem:parse :cs"), "$<Chem:Cs>");
    assert_eq!(ve("std:chem:parse :ba"), "$<Chem:Ba>");
    assert_eq!(ve("std:chem:parse :la"), "$<Chem:La>");
    assert_eq!(ve("std:chem:parse :ce"), "$<Chem:Ce>");
    assert_eq!(ve("std:chem:parse :pr"), "$<Chem:Pr>");
    assert_eq!(ve("std:chem:parse :nd"), "$<Chem:Nd>");
    assert_eq!(ve("std:chem:parse :pm"), "$<Chem:Pm>");
    assert_eq!(ve("std:chem:parse :sm"), "$<Chem:Sm>");
    assert_eq!(ve("std:chem:parse :eu"), "$<Chem:Eu>");
    assert_eq!(ve("std:chem:parse :gd"), "$<Chem:Gd>");
    assert_eq!(ve("std:chem:parse :tb"), "$<Chem:Tb>");
    assert_eq!(ve("std:chem:parse :dy"), "$<Chem:Dy>");
    assert_eq!(ve("std:chem:parse :ho"), "$<Chem:Ho>");
    assert_eq!(ve("std:chem:parse :er"), "$<Chem:Er>");
    assert_eq!(ve("std:chem:parse :tm"), "$<Chem:Tm>");
    assert_eq!(ve("std:chem:parse :yb"), "$<Chem:Yb>");
    assert_eq!(ve("std:chem:parse :lu"), "$<Chem:Lu>");
    assert_eq!(ve("std:chem:parse :hf"), "$<Chem:Hf>");
    assert_eq!(ve("std:chem:parse :ta"), "$<Chem:Ta>");
    assert_eq!(ve("std:chem:parse :w"), "$<Chem:W>");
    assert_eq!(ve("std:chem:parse :re"), "$<Chem:Re>");
    assert_eq!(ve("std:chem:parse :os"), "$<Chem:Os>");
    assert_eq!(ve("std:chem:parse :ir"), "$<Chem:Ir>");
    assert_eq!(ve("std:chem:parse :pt"), "$<Chem:Pt>");
    assert_eq!(ve("std:chem:parse :au"), "$<Chem:Au>");
    assert_eq!(ve("std:chem:parse :hg"), "$<Chem:Hg>");
    assert_eq!(ve("std:chem:parse :tl"), "$<Chem:Tl>");
    assert_eq!(ve("std:chem:parse :pb"), "$<Chem:Pb>");
    assert_eq!(ve("std:chem:parse :bi"), "$<Chem:Bi>");
    assert_eq!(ve("std:chem:parse :po"), "$<Chem:Po>");
    assert_eq!(ve("std:chem:parse :at"), "$<Chem:At>");
    assert_eq!(ve("std:chem:parse :rn"), "$<Chem:Rn>");
    assert_eq!(ve("std:chem:parse :fr"), "$<Chem:Fr>");
    assert_eq!(ve("std:chem:parse :ra"), "$<Chem:Ra>");
    assert_eq!(ve("std:chem:parse :ac"), "$<Chem:Ac>");
    assert_eq!(ve("std:chem:parse :th"), "$<Chem:Th>");
    assert_eq!(ve("std:chem:parse :pa"), "$<Chem:Pa>");
    assert_eq!(ve("std:chem:parse :u"), "$<Chem:U>");
    assert_eq!(ve("std:chem:parse :np"), "$<Chem:Np>");
    assert_eq!(ve("std:chem:parse :pu"), "$<Chem:Pu>");
    assert_eq!(ve("std:chem:parse :am"), "$<Chem:Am>");
    assert_eq!(ve("std:chem:parse :cm"), "$<Chem:Cm>");
    assert_eq!(ve("std:chem:parse :bk"), "$<Chem:Bk>");
    assert_eq!(ve("std:chem:parse :cf"), "$<Chem:Cf>");
    assert_eq!(ve("std:chem:parse :es"), "$<Chem:Es>");
    assert_eq!(ve("std:chem:parse :fm"), "$<Chem:Fm>");
    assert_eq!(ve("std:chem:parse :md"), "$<Chem:Md>");
    assert_eq!(ve("std:chem:parse :no"), "$<Chem:No>");
    assert_eq!(ve("std:chem:parse :lr"), "$<Chem:Lr>");
    assert_eq!(ve("std:chem:parse :rf"), "$<Chem:Rf>");
    assert_eq!(ve("std:chem:parse :db"), "$<Chem:Db>");
    assert_eq!(ve("std:chem:parse :sg"), "$<Chem:Sg>");
    assert_eq!(ve("std:chem:parse :bh"), "$<Chem:Bh>");
    assert_eq!(ve("std:chem:parse :hs"), "$<Chem:Hs>");
    assert_eq!(ve("std:chem:parse :mt"), "$<Chem:Mt>");
    assert_eq!(ve("std:chem:parse :ds"), "$<Chem:Ds>");
    assert_eq!(ve("std:chem:parse :rg"), "$<Chem:Rg>");
    assert_eq!(ve("std:chem:parse :cn"), "$<Chem:Cn>");
    assert_eq!(ve("std:chem:parse :nh"), "$<Chem:Nh>");
    assert_eq!(ve("std:chem:parse :fl"), "$<Chem:Fl>");
    assert_eq!(ve("std:chem:parse :mc"), "$<Chem:Mc>");
    assert_eq!(ve("std:chem:parse :lv"), "$<Chem:Lv>");
    assert_eq!(ve("std:chem:parse :ts"), "$<Chem:Ts>");
    assert_eq!(ve("std:chem:parse :og"), "$<Chem:Og>");

    assert_eq!(ve("std:chem:parse :OHFe"), "$<Chem:OHFe>");
    assert_eq!(ve("std:chem:parse :OHeFe"), "$<Chem:OHeFe>");
    assert_eq!(ve("std:chem:parse :OLiFe"), "$<Chem:OLiFe>");
    assert_eq!(ve("std:chem:parse :OBeFe"), "$<Chem:OBeFe>");
    assert_eq!(ve("std:chem:parse :OBFe"), "$<Chem:OBFe>");
    assert_eq!(ve("std:chem:parse :OCFe"), "$<Chem:OCFe>");
    assert_eq!(ve("std:chem:parse :ONFe"), "$<Chem:ONFe>");
    assert_eq!(ve("std:chem:parse :OOFe"), "$<Chem:OOFe>");
    assert_eq!(ve("std:chem:parse :OFFe"), "$<Chem:OFFe>");
    assert_eq!(ve("std:chem:parse :ONeFe"), "$<Chem:ONeFe>");
    assert_eq!(ve("std:chem:parse :ONaFe"), "$<Chem:ONaFe>");
    assert_eq!(ve("std:chem:parse :OMgFe"), "$<Chem:OMgFe>");
    assert_eq!(ve("std:chem:parse :OAlFe"), "$<Chem:OAlFe>");
    assert_eq!(ve("std:chem:parse :OSiFe"), "$<Chem:OSiFe>");
    assert_eq!(ve("std:chem:parse :OPFe"), "$<Chem:OPFe>");
    assert_eq!(ve("std:chem:parse :OSFe"), "$<Chem:OSFe>");
    assert_eq!(ve("std:chem:parse :OClFe"), "$<Chem:OClFe>");
    assert_eq!(ve("std:chem:parse :OArFe"), "$<Chem:OArFe>");
    assert_eq!(ve("std:chem:parse :OKFe"), "$<Chem:OKFe>");
    assert_eq!(ve("std:chem:parse :OCaFe"), "$<Chem:OCaFe>");
    assert_eq!(ve("std:chem:parse :OScFe"), "$<Chem:OScFe>");
    assert_eq!(ve("std:chem:parse :OTiFe"), "$<Chem:OTiFe>");
    assert_eq!(ve("std:chem:parse :OVFe"), "$<Chem:OVFe>");
    assert_eq!(ve("std:chem:parse :OCrFe"), "$<Chem:OCrFe>");
    assert_eq!(ve("std:chem:parse :OMnFe"), "$<Chem:OMnFe>");
    assert_eq!(ve("std:chem:parse :OFeFe"), "$<Chem:OFeFe>");
    assert_eq!(ve("std:chem:parse :OCoFe"), "$<Chem:OCoFe>");
    assert_eq!(ve("std:chem:parse :ONiFe"), "$<Chem:ONiFe>");
    assert_eq!(ve("std:chem:parse :OCuFe"), "$<Chem:OCuFe>");
    assert_eq!(ve("std:chem:parse :OZnFe"), "$<Chem:OZnFe>");
    assert_eq!(ve("std:chem:parse :OGaFe"), "$<Chem:OGaFe>");
    assert_eq!(ve("std:chem:parse :OGeFe"), "$<Chem:OGeFe>");
    assert_eq!(ve("std:chem:parse :OAsFe"), "$<Chem:OAsFe>");
    assert_eq!(ve("std:chem:parse :OSeFe"), "$<Chem:OSeFe>");
    assert_eq!(ve("std:chem:parse :OBrFe"), "$<Chem:OBrFe>");
    assert_eq!(ve("std:chem:parse :OKrFe"), "$<Chem:OKrFe>");
    assert_eq!(ve("std:chem:parse :ORbFe"), "$<Chem:ORbFe>");
    assert_eq!(ve("std:chem:parse :OSrFe"), "$<Chem:OSrFe>");
    assert_eq!(ve("std:chem:parse :OYFe"), "$<Chem:OYFe>");
    assert_eq!(ve("std:chem:parse :OZrFe"), "$<Chem:OZrFe>");
    assert_eq!(ve("std:chem:parse :ONbFe"), "$<Chem:ONbFe>");
    assert_eq!(ve("std:chem:parse :OMoFe"), "$<Chem:OMoFe>");
    assert_eq!(ve("std:chem:parse :OTcFe"), "$<Chem:OTcFe>");
    assert_eq!(ve("std:chem:parse :ORuFe"), "$<Chem:ORuFe>");
    assert_eq!(ve("std:chem:parse :ORhFe"), "$<Chem:ORhFe>");
    assert_eq!(ve("std:chem:parse :OPdFe"), "$<Chem:OPdFe>");
    assert_eq!(ve("std:chem:parse :OAgFe"), "$<Chem:OAgFe>");
    assert_eq!(ve("std:chem:parse :OCdFe"), "$<Chem:OCdFe>");
    assert_eq!(ve("std:chem:parse :OInFe"), "$<Chem:OInFe>");
    assert_eq!(ve("std:chem:parse :OSnFe"), "$<Chem:OSnFe>");
    assert_eq!(ve("std:chem:parse :OSbFe"), "$<Chem:OSbFe>");
    assert_eq!(ve("std:chem:parse :OTeFe"), "$<Chem:OTeFe>");
    assert_eq!(ve("std:chem:parse :OIFe"), "$<Chem:OIFe>");
    assert_eq!(ve("std:chem:parse :OXeFe"), "$<Chem:OXeFe>");
    assert_eq!(ve("std:chem:parse :OCsFe"), "$<Chem:OCsFe>");
    assert_eq!(ve("std:chem:parse :OBaFe"), "$<Chem:OBaFe>");
    assert_eq!(ve("std:chem:parse :OLaFe"), "$<Chem:OLaFe>");
    assert_eq!(ve("std:chem:parse :OCeFe"), "$<Chem:OCeFe>");
    assert_eq!(ve("std:chem:parse :OPrFe"), "$<Chem:OPrFe>");
    assert_eq!(ve("std:chem:parse :ONdFe"), "$<Chem:ONdFe>");
    assert_eq!(ve("std:chem:parse :OPmFe"), "$<Chem:OPmFe>");
    assert_eq!(ve("std:chem:parse :OSmFe"), "$<Chem:OSmFe>");
    assert_eq!(ve("std:chem:parse :OEuFe"), "$<Chem:OEuFe>");
    assert_eq!(ve("std:chem:parse :OGdFe"), "$<Chem:OGdFe>");
    assert_eq!(ve("std:chem:parse :OTbFe"), "$<Chem:OTbFe>");
    assert_eq!(ve("std:chem:parse :ODyFe"), "$<Chem:ODyFe>");
    assert_eq!(ve("std:chem:parse :OHoFe"), "$<Chem:OHoFe>");
    assert_eq!(ve("std:chem:parse :OErFe"), "$<Chem:OErFe>");
    assert_eq!(ve("std:chem:parse :OTmFe"), "$<Chem:OTmFe>");
    assert_eq!(ve("std:chem:parse :OYbFe"), "$<Chem:OYbFe>");
    assert_eq!(ve("std:chem:parse :OLuFe"), "$<Chem:OLuFe>");
    assert_eq!(ve("std:chem:parse :OHfFe"), "$<Chem:OHfFe>");
    assert_eq!(ve("std:chem:parse :OTaFe"), "$<Chem:OTaFe>");
    assert_eq!(ve("std:chem:parse :OWFe"), "$<Chem:OWFe>");
    assert_eq!(ve("std:chem:parse :OReFe"), "$<Chem:OReFe>");
    assert_eq!(ve("std:chem:parse :OOsFe"), "$<Chem:OOsFe>");
    assert_eq!(ve("std:chem:parse :OIrFe"), "$<Chem:OIrFe>");
    assert_eq!(ve("std:chem:parse :OPtFe"), "$<Chem:OPtFe>");
    assert_eq!(ve("std:chem:parse :OAuFe"), "$<Chem:OAuFe>");
    assert_eq!(ve("std:chem:parse :OHgFe"), "$<Chem:OHgFe>");
    assert_eq!(ve("std:chem:parse :OTlFe"), "$<Chem:OTlFe>");
    assert_eq!(ve("std:chem:parse :OPbFe"), "$<Chem:OPbFe>");
    assert_eq!(ve("std:chem:parse :OBiFe"), "$<Chem:OBiFe>");
    assert_eq!(ve("std:chem:parse :OPoFe"), "$<Chem:OPoFe>");
    assert_eq!(ve("std:chem:parse :OAtFe"), "$<Chem:OAtFe>");
    assert_eq!(ve("std:chem:parse :ORnFe"), "$<Chem:ORnFe>");
    assert_eq!(ve("std:chem:parse :OFrFe"), "$<Chem:OFrFe>");
    assert_eq!(ve("std:chem:parse :ORaFe"), "$<Chem:ORaFe>");
    assert_eq!(ve("std:chem:parse :OAcFe"), "$<Chem:OAcFe>");
    assert_eq!(ve("std:chem:parse :OThFe"), "$<Chem:OThFe>");
    assert_eq!(ve("std:chem:parse :OPaFe"), "$<Chem:OPaFe>");
    assert_eq!(ve("std:chem:parse :OUFe"), "$<Chem:OUFe>");
    assert_eq!(ve("std:chem:parse :ONpFe"), "$<Chem:ONpFe>");
    assert_eq!(ve("std:chem:parse :OPuFe"), "$<Chem:OPuFe>");
    assert_eq!(ve("std:chem:parse :OAmFe"), "$<Chem:OAmFe>");
    assert_eq!(ve("std:chem:parse :OCmFe"), "$<Chem:OCmFe>");
    assert_eq!(ve("std:chem:parse :OBkFe"), "$<Chem:OBkFe>");
    assert_eq!(ve("std:chem:parse :OCfFe"), "$<Chem:OCfFe>");
    assert_eq!(ve("std:chem:parse :OEsFe"), "$<Chem:OEsFe>");
    assert_eq!(ve("std:chem:parse :OFmFe"), "$<Chem:OFmFe>");
    assert_eq!(ve("std:chem:parse :OMdFe"), "$<Chem:OMdFe>");
    assert_eq!(ve("std:chem:parse :ONoFe"), "$<Chem:ONoFe>");
    assert_eq!(ve("std:chem:parse :OLrFe"), "$<Chem:OLrFe>");
    assert_eq!(ve("std:chem:parse :ORfFe"), "$<Chem:ORfFe>");
    assert_eq!(ve("std:chem:parse :ODbFe"), "$<Chem:ODbFe>");
    assert_eq!(ve("std:chem:parse :OSgFe"), "$<Chem:OSgFe>");
    assert_eq!(ve("std:chem:parse :OBhFe"), "$<Chem:OBhFe>");
    assert_eq!(ve("std:chem:parse :OHsFe"), "$<Chem:OHsFe>");
    assert_eq!(ve("std:chem:parse :OMtFe"), "$<Chem:OMtFe>");
    assert_eq!(ve("std:chem:parse :ODsFe"), "$<Chem:ODsFe>");
    assert_eq!(ve("std:chem:parse :ORgFe"), "$<Chem:ORgFe>");
    assert_eq!(ve("std:chem:parse :OCnFe"), "$<Chem:OCnFe>");
    assert_eq!(ve("std:chem:parse :ONhFe"), "$<Chem:ONhFe>");
    assert_eq!(ve("std:chem:parse :OFlFe"), "$<Chem:OFlFe>");
    assert_eq!(ve("std:chem:parse :OMcFe"), "$<Chem:OMcFe>");
    assert_eq!(ve("std:chem:parse :OLvFe"), "$<Chem:OLvFe>");
    assert_eq!(ve("std:chem:parse :OTsFe"), "$<Chem:OTsFe>");
    assert_eq!(ve("std:chem:parse :OOgFe"), "$<Chem:OOgFe>");

    assert_eq!(ve("std:chem:parse :OhFe"), "$<Chem:OHFe>");
    assert_eq!(ve("std:chem:parse :OheFe"), "$<Chem:OHeFe>");
    assert_eq!(ve("std:chem:parse :OliFe"), "$<Chem:OLiFe>");
    assert_eq!(ve("std:chem:parse :ObeFe"), "$<Chem:OBeFe>");
    assert_eq!(ve("std:chem:parse :ObFe"), "$<Chem:OBFe>");
    assert_eq!(ve("std:chem:parse :OcFe"), "$<Chem:OCFe>");
    assert_eq!(ve("std:chem:parse :OnFe"), "$<Chem:ONFe>");
    assert_eq!(ve("std:chem:parse :OoFe"), "$<Chem:OOFe>");
    assert_eq!(ve("std:chem:parse :OfFe"), "$<Chem:OFFe>");
    assert_eq!(ve("std:chem:parse :OneFe"), "$<Chem:ONeFe>");
    assert_eq!(ve("std:chem:parse :OnaFe"), "$<Chem:ONaFe>");
    assert_eq!(ve("std:chem:parse :OmgFe"), "$<Chem:OMgFe>");
    assert_eq!(ve("std:chem:parse :OalFe"), "$<Chem:OAlFe>");
    // Osi => Os, I
    assert_eq!(ve("std:chem:parse :OSiFe"), "$<Chem:OSiFe>");
    assert_eq!(ve("std:chem:parse :OsIFe"), "$<Chem:OsIFe>");
    assert_eq!(ve("std:chem:parse :OpFe"), "$<Chem:OPFe>");
    // OSFe => OsFe
    assert_eq!(ve("std:chem:parse :OsFe"), "$<Chem:OsFe>");
    assert_eq!(ve("std:chem:parse :OclFe"), "$<Chem:OClFe>");
    assert_eq!(ve("std:chem:parse :OarFe"), "$<Chem:OArFe>");
    assert_eq!(ve("std:chem:parse :OkFe"), "$<Chem:OKFe>");
    assert_eq!(ve("std:chem:parse :OcaFe"), "$<Chem:OCaFe>");
    // OScFe
    assert_eq!(ve("std:chem:parse :OScFe"), "$<Chem:OScFe>");
    assert_eq!(ve("std:chem:parse :OtiFe"), "$<Chem:OTiFe>");
    assert_eq!(ve("std:chem:parse :OvFe"), "$<Chem:OVFe>");
    assert_eq!(ve("std:chem:parse :OcrFe"), "$<Chem:OCrFe>");
    assert_eq!(ve("std:chem:parse :OmnFe"), "$<Chem:OMnFe>");
    assert_eq!(ve("std:chem:parse :OfeFe"), "$<Chem:OFeFe>");
    assert_eq!(ve("std:chem:parse :OcoFe"), "$<Chem:OCoFe>");
    assert_eq!(ve("std:chem:parse :OniFe"), "$<Chem:ONiFe>");
    assert_eq!(ve("std:chem:parse :OcuFe"), "$<Chem:OCuFe>");
    assert_eq!(ve("std:chem:parse :OznFe"), "$<Chem:OZnFe>");
    assert_eq!(ve("std:chem:parse :OGaFe"), "$<Chem:OGaFe>");
    assert_eq!(ve("std:chem:parse :OGeFe"), "$<Chem:OGeFe>");
    assert_eq!(ve("std:chem:parse :OasFe"), "$<Chem:OAsFe>");
    assert_eq!(ve("std:chem:parse :OSeFe"), "$<Chem:OSeFe>");
    assert_eq!(ve("std:chem:parse :ObrFe"), "$<Chem:OBrFe>");
    assert_eq!(ve("std:chem:parse :OkrFe"), "$<Chem:OKrFe>");
    assert_eq!(ve("std:chem:parse :OrbFe"), "$<Chem:ORbFe>");
    assert_eq!(ve("std:chem:parse :OSrFe"), "$<Chem:OSrFe>");
    assert_eq!(ve("std:chem:parse :OyFe"), "$<Chem:OYFe>");
    assert_eq!(ve("std:chem:parse :OzrFe"), "$<Chem:OZrFe>");
    assert_eq!(ve("std:chem:parse :OnbFe"), "$<Chem:ONbFe>");
    assert_eq!(ve("std:chem:parse :OmoFe"), "$<Chem:OMoFe>");
    assert_eq!(ve("std:chem:parse :OtcFe"), "$<Chem:OTcFe>");
    assert_eq!(ve("std:chem:parse :OruFe"), "$<Chem:ORuFe>");
    assert_eq!(ve("std:chem:parse :OrhFe"), "$<Chem:ORhFe>");
    assert_eq!(ve("std:chem:parse :OpdFe"), "$<Chem:OPdFe>");
    assert_eq!(ve("std:chem:parse :OagFe"), "$<Chem:OAgFe>");
    assert_eq!(ve("std:chem:parse :OcdFe"), "$<Chem:OCdFe>");
    assert_eq!(ve("std:chem:parse :OinFe"), "$<Chem:OInFe>");
    assert_eq!(ve("std:chem:parse :OSnFe"), "$<Chem:OSnFe>");
    assert_eq!(ve("std:chem:parse :OSbFe"), "$<Chem:OSbFe>");
    assert_eq!(ve("std:chem:parse :OsnFe"), "$<Chem:OsNFe>");
    assert_eq!(ve("std:chem:parse :OsbFe"), "$<Chem:OsBFe>");
    assert_eq!(ve("std:chem:parse :OteFe"), "$<Chem:OTeFe>");
    assert_eq!(ve("std:chem:parse :OiFe"), "$<Chem:OIFe>");
    assert_eq!(ve("std:chem:parse :OxeFe"), "$<Chem:OXeFe>");
    assert_eq!(ve("std:chem:parse :OcsFe"), "$<Chem:OCsFe>");
    assert_eq!(ve("std:chem:parse :ObaFe"), "$<Chem:OBaFe>");
    assert_eq!(ve("std:chem:parse :OlaFe"), "$<Chem:OLaFe>");
    assert_eq!(ve("std:chem:parse :OceFe"), "$<Chem:OCeFe>");
    assert_eq!(ve("std:chem:parse :OprFe"), "$<Chem:OPrFe>");
    assert_eq!(ve("std:chem:parse :OndFe"), "$<Chem:ONdFe>");
    assert_eq!(ve("std:chem:parse :OpmFe"), "$<Chem:OPmFe>");
    assert_eq!(ve("std:chem:parse :OSmFe"), "$<Chem:OSmFe>");
    assert_eq!(ve("std:chem:parse :OeuFe"), "$<Chem:OEuFe>");
    assert_eq!(ve("std:chem:parse :OGdFe"), "$<Chem:OGdFe>");
    assert_eq!(ve("std:chem:parse :OtbFe"), "$<Chem:OTbFe>");
    assert_eq!(ve("std:chem:parse :OdyFe"), "$<Chem:ODyFe>");
    assert_eq!(ve("std:chem:parse :OhoFe"), "$<Chem:OHoFe>");
    assert_eq!(ve("std:chem:parse :OerFe"), "$<Chem:OErFe>");
    assert_eq!(ve("std:chem:parse :OtmFe"), "$<Chem:OTmFe>");
    assert_eq!(ve("std:chem:parse :OybFe"), "$<Chem:OYbFe>");
    assert_eq!(ve("std:chem:parse :OluFe"), "$<Chem:OLuFe>");
    assert_eq!(ve("std:chem:parse :OhfFe"), "$<Chem:OHfFe>");
    assert_eq!(ve("std:chem:parse :OtaFe"), "$<Chem:OTaFe>");
    assert_eq!(ve("std:chem:parse :OwFe"), "$<Chem:OWFe>");
    assert_eq!(ve("std:chem:parse :OreFe"), "$<Chem:OReFe>");
    assert_eq!(ve("std:chem:parse :OosFe"), "$<Chem:OOsFe>");
    assert_eq!(ve("std:chem:parse :OirFe"), "$<Chem:OIrFe>");
    assert_eq!(ve("std:chem:parse :OptFe"), "$<Chem:OPtFe>");
    assert_eq!(ve("std:chem:parse :OauFe"), "$<Chem:OAuFe>");
    assert_eq!(ve("std:chem:parse :OhgFe"), "$<Chem:OHgFe>");
    assert_eq!(ve("std:chem:parse :OtlFe"), "$<Chem:OTlFe>");
    assert_eq!(ve("std:chem:parse :OpbFe"), "$<Chem:OPbFe>");
    assert_eq!(ve("std:chem:parse :ObiFe"), "$<Chem:OBiFe>");
    assert_eq!(ve("std:chem:parse :OpoFe"), "$<Chem:OPoFe>");
    assert_eq!(ve("std:chem:parse :OatFe"), "$<Chem:OAtFe>");
    assert_eq!(ve("std:chem:parse :OrnFe"), "$<Chem:ORnFe>");
    assert_eq!(ve("std:chem:parse :OfrFe"), "$<Chem:OFrFe>");
    assert_eq!(ve("std:chem:parse :OraFe"), "$<Chem:ORaFe>");
    assert_eq!(ve("std:chem:parse :OacFe"), "$<Chem:OAcFe>");
    assert_eq!(ve("std:chem:parse :OthFe"), "$<Chem:OThFe>");
    assert_eq!(ve("std:chem:parse :OpaFe"), "$<Chem:OPaFe>");
    assert_eq!(ve("std:chem:parse :OuFe"), "$<Chem:OUFe>");
    assert_eq!(ve("std:chem:parse :OnpFe"), "$<Chem:ONpFe>");
    assert_eq!(ve("std:chem:parse :OpuFe"), "$<Chem:OPuFe>");
    assert_eq!(ve("std:chem:parse :OamFe"), "$<Chem:OAmFe>");
    assert_eq!(ve("std:chem:parse :OcmFe"), "$<Chem:OCmFe>");
    assert_eq!(ve("std:chem:parse :ObkFe"), "$<Chem:OBkFe>");
    assert_eq!(ve("std:chem:parse :OcfFe"), "$<Chem:OCfFe>");
    assert_eq!(ve("std:chem:parse :OesFe"), "$<Chem:OEsFe>");
    assert_eq!(ve("std:chem:parse :OfmFe"), "$<Chem:OFmFe>");
    assert_eq!(ve("std:chem:parse :OmdFe"), "$<Chem:OMdFe>");
    assert_eq!(ve("std:chem:parse :OnoFe"), "$<Chem:ONoFe>");
    assert_eq!(ve("std:chem:parse :OlrFe"), "$<Chem:OLrFe>");
    assert_eq!(ve("std:chem:parse :OrfFe"), "$<Chem:ORfFe>");
    assert_eq!(ve("std:chem:parse :OdbFe"), "$<Chem:ODbFe>");
    assert_eq!(ve("std:chem:parse :OSgFe"), "$<Chem:OSgFe>");
    assert_eq!(ve("std:chem:parse :ObhFe"), "$<Chem:OBhFe>");
    assert_eq!(ve("std:chem:parse :OhsFe"), "$<Chem:OHsFe>");
    assert_eq!(ve("std:chem:parse :OmtFe"), "$<Chem:OMtFe>");
    assert_eq!(ve("std:chem:parse :OdsFe"), "$<Chem:ODsFe>");
    assert_eq!(ve("std:chem:parse :OrgFe"), "$<Chem:ORgFe>");
    assert_eq!(ve("std:chem:parse :OcnFe"), "$<Chem:OCnFe>");
    assert_eq!(ve("std:chem:parse :OnhFe"), "$<Chem:ONhFe>");
    assert_eq!(ve("std:chem:parse :OflFe"), "$<Chem:OFlFe>");
    assert_eq!(ve("std:chem:parse :OmcFe"), "$<Chem:OMcFe>");
    assert_eq!(ve("std:chem:parse :OlvFe"), "$<Chem:OLvFe>");
    assert_eq!(ve("std:chem:parse :OtsFe"), "$<Chem:OTsFe>");
    assert_eq!(ve("std:chem:parse :OogFe"), "$<Chem:OOgFe>");

    assert_eq!(ve("(std:chem:parse :H2Fe3O).canonical[]"), "$<Chem:H2OFe3>");
    assert_eq!(ve("(std:chem:parse \"H2(Zr3La9)2Fe3O\").canonical[]"), "$<Chem:H2OFe3Zr6La18>");
    assert_eq!(ve("(std:chem:parse :H2Fe3O).canonical_hill[]"), "$<Chem:H2Fe3O>");
    assert_eq!(
        ve("(std:chem:parse \"H2(Zr3La9)2Fe3O\").canonical_hill[]"),
        "$<Chem:H2Fe3La18OZr6>"
    );
    assert_eq!(ve("(std:chem:parse :H2OFe3).canonical_hill[]"), "$<Chem:H2Fe3O>");
    assert_eq!(
        ve("(std:chem:parse \"(h19)2 n o2 cl h c14\").canonical_hill[]"),
        "$<Chem:C14H39ClNO2>"
    );
    assert_eq!(ve("!s = $@s iter e std:chem:data[] { $+ e.symbol }; (std:chem:parse s).canonical_hill[]"),
"$<Chem:CHAcAgAlAmArAsAtAuBBaBeBhBiBkBrCaCdCeCfClCmCnCoCrCsCuDbDsDyErEsEuFFeFlFmFrGaGdGeHeHfHgHoHsIInIrKKrLaLiLrLuLvMcMdMgMnMoMtNNaNbNdNeNhNiNoNpOOgOsPPaPbPdPmPoPrPtPuRaRbReRfRgRhRnRuSSbScSeSgSiSmSnSrTaTbTcTeThTiTlTmTsUVWXeYYbZnZr>");
}

#[test]
fn check_test_min_max() {
    assert_eq!(ve("std:max 0.1 0.4 -0.2 0.6"), "0.6");
    assert_eq!(ve("std:max :a :C :D :A"), ":a");
    assert_eq!(ve("std:max \"a\" \"C\" \"D\" \"A\""), "\"a\"");
    assert_eq!(ve("std:max 0.1"), "0.1");
    assert_eq!(ve("std:max 10 11.1 11.2"), "11");
    assert_eq!(ve("std:min 0.1 0.4 -0.2 0.6"), "-0.2");
    assert_eq!(ve("std:min :a :C :D :A"), ":A");
    assert_eq!(ve("std:min \"a\" \"C\" \"D\" \"A\""), "\"A\"");
    assert_eq!(ve("std:min 0.1"), "0.1");
    assert_eq!(ve("std:min 10 11.1 11.2"), "10");
}

#[test]
fn check_multi_def() {
    assert_eq!(
        ve("!r = 14; if $t { !o = 12; if $t { !y = 12; if $t { !x = 1; !x = 2; }; }; o }; r"),
        "14"
    );
    assert_eq!(ve("!o = 15; if $t { !y = 12; if $t { !x = 1; !x = 2; }; }; o"), "15");
    assert_eq!(ve("!y = 13; if $t { !(x, c) = 1 => 2; }; y"), "13");
    assert_eq!(ve("!y = 12; if $t { !(x, y) = 1 => 2; }; y"), "12");
    assert_eq!(ve("!y = 11; if $t { !(x, x) = 1 => 2; }; y"), "11");
    assert_eq!(ve("!y = 10; if $t { !x = 1; !x = 2; }; y"), "10");
}

#[test]
fn check_insert_remove_shift() {
    assert_eq!(ve("std:remove 100 $[1,2,3,4,5,6]"), "$o()");
    assert_eq!(ve("std:remove 1 $[1,2,3,4,5,6]"), "$o(2)");
    assert_eq!(ve("std:remove 0 $[1,2,3,4,5,6]"), "$o(1)");
    assert_eq!(ve("std:remove -1 $[1,2,3,4,5,6]"), "$o(6)");
    assert_eq!(ve("std:remove -100 $[1,2,3,4,5,6]"), "$o(1)");

    assert_eq!(ve("!x = $[1,2,3,4,5,6]; $[std:shift x, std:shift x]"), "$[$o(1),$o(2)]");
    assert_eq!(ve("std:shift $[]"), "$o()");
    assert_eq!(ve("std:insert $[1,2,3,4] 2 \"a\""), "$[1,2,\"a\",3,4]");
    assert_eq!(ve("std:insert $[1,2,3,4] -1 \"a\""), "$[1,2,3,4,\"a\"]");
    assert_eq!(ve("std:insert $[1,2,3,4] -2 \"a\""), "$[1,2,3,\"a\",4]");
    assert_eq!(ve("std:insert $[1,2,3,4] -100 \"a\""), "$[\"a\",1,2,3,4]");
}

#[test]
fn check_string_splits() {
    assert_eq!(
        ve("std:str:split_any \";, \" \"abc;;;feofoefe,, fewfioewfw;, 333\""),
        "$[\"abc\",\"\",\"\",\"feofoefe\",\"\",\"\",\"fewfioewfw\",\"\",\"\",\"333\"]");
    assert_eq!(ve("std:str:split_any \";, \" \"\""), "$[\"\"]");
    assert_eq!(ve("std:str:split_any \";, \" \"A; ,,\""), "$[\"A\",\"\",\"\",\"\",\"\"]");
    assert_eq!(ve("std:str:split_any \";, \" \"; ,,\""), "$[\"\",\"\",\"\",\"\",\"\"]");

    assert_eq!(
        ve("std:str:split_any_non_empty \";, \" \"abc;;;feofoefe,, fewfioewfw;, 333\""),
        "$[\"abc\",\"feofoefe\",\"fewfioewfw\",\"333\"]");
    assert_eq!(ve("std:str:split_any_non_empty \";, \" \"\""), "$[]");
    assert_eq!(ve("std:str:split_any_non_empty \";, \" \"A; ,,\""), "$[\"A\"]");
    assert_eq!(ve("std:str:split_any_non_empty \";, \" \"; ,,\""), "$[]");

    assert_eq!(
        ve("std:str:split_whitespace \"He told her    to get lost!\""),
        "$[\"He\",\"told\",\"her\",\"to\",\"get\",\"lost!\"]");

    assert_eq!(
        ve("std:str:nlp:en:extract_pure_words \"He told her    to get lost, saying \\\"You lol\\\" *chuckling under his statements*!\""),
        "$[\"He\",\"told\",\"her\",\"to\",\"get\",\"lost\",\"saying\",\"You\",\"lol\",\"chuckling\",\"under\",\"his\",\"statements\",\"!\"]");

    assert_eq!(
        ve("std:str:nlp:en:extract_pure_words \"You're You'rea it' it's\""),
        "$[\"Youre\",\"You\",\"rea\",\"it\",\"its\"]");

    assert_eq!(
        ve("std:str:nlp:extract_quoted \"'\" \"He told her    to get lost, saying \\\'You lol\\\' *chuckling under his statements*!\""),
        "$[$p($n,\"He told her    to get lost, saying \"),$p(\"\\'\",\"You lol\"),\" *chuckling under his statements*!\"]");

    assert_eq!(
        ve("std:str:nlp:extract_quoted \"'\" \"'X'Y'''X'\""),
        "$[$p($<1=>\"\\'\",\"X\"),$p($n,\"Y\"),$p($<1>,\"X\")]");

    assert_eq!(
        ve("std:str:nlp:extract_quoted \"'\" \"'OOOO\""),
        "$[\"OOOO\"]");

    assert_eq!(
        ve("std:str:nlp:extract_quoted \"'\" \"'\""),
        "$[]");

    assert_eq!(
        ve("std:str:nlp:extract_quoted \"He told her, \\\"You are fine!\\\"   \""),
        "$[]");
}

#[test]
fn check_auto_correlate() {

    assert_eq!(
        ve(r#"
            std:str:auto_correlate_longest_sublist $[
                $[1,2,3,4,5,6,7,8,0,0,0,1,2],
                $[9,4,3,2,76,65,4,1,2,3,4,5,7,5,5,45,6,7],
                $[0,0,0,0,1,2,3,0,0,0,1,2,3,4,5,6],
                $[-1,-1,-1,6,7,8,0],
            ] 4;
        "#),
        "$[$[0,0,1,7,5,$[1,2,3,4,5]],$[0,0,2,10,6,$[1,2,3,4,5,6]],$[0,5,3,3,4,$[6,7,8,0]],$[0,8,2,1,5,$[0,0,0,1,2]],$[0,8,2,7,5,$[0,0,0,1,2]]]");

}
