use wlambda::*;
use std::rc::Rc;
use std::cell::RefCell;
use wlambda::rt_compiler::compile_str;
use wlambda::rt_compiler::RtVal;
use wlambda::rt_compiler::RtProg;
use std::io::Write;

macro_rules! sizeof_writeln {
    ($write: ident, $type: ty) => {
        writeln!($write, "sizeof {:40}:  {:2} bytes",
                 stringify!($type),
                 std::mem::size_of::<$type>()).expect("stdout access works");
    }
}

#[test]
fn rt_check_compile() {
    let rtp = compile_str("32", "<test>");

    let mut write =  std::io::stdout();
    sizeof_writeln!(write, wlambda::rt_compiler::RtVal);
    assert!(false);
}
