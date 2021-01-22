use wlambda::*;
use std::rc::Rc;
use std::cell::RefCell;
use wlambda::rt_compiler::compile_str;

#[test]
fn rt_check_compile() {
    let rtp = compile_str("32", "<test>");
}
