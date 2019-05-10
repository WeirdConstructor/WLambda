// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;
use parser::*; // ParseState;
use compiler::*; // ParseState;
use vval::*;
use std::rc::Rc;
use std::cell::RefCell;

//use std::iter::Peekable;
//use std::fmt;
//
pub enum BAL {
    S(i8),
    Nul,
    Bar,
}

fn main() {
    let v = eval("!:ref x = 0; range 0 10000000 1 { .x = x + 1 }; x");
    println!("> {:?}", v);
}

