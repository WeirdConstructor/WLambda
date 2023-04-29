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
fn check_type_simple() {
    assert_eq!(ve("!v @Int = 10; v"), "10");
    assert_eq!(ve("!v @Int = 10.1; v"),
        "COMPILE ERROR: <compiler:s_eval>:1:2 Compilation Error: Incompatible types: Int <=> Float");
// TODO:   assert_eq!(ve("!v @Any = 10.1; v"), "10.1");
    assert_eq!(
        ve("!(a @Float, b @Float) = $p(10, 11.1); a"),
        "COMPILE ERROR: Float != Int"
    );
    assert_eq!(
        ve("!(a @Int, b @Float) = $p(10, 11.1); a + int[b]"),
        "21"
    );
    // TODO:
    //  - Find a way to define types for operators like `+`.
    //  - Make function calls search the types of the arguments
}
