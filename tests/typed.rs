// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
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
fn chk_types_add() {
    assert_eq!(ve("!x: float = 1 + \"2\""), "10"); // Expecting a type error!
}
