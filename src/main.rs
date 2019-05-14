// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;
use compiler::*; // ParseState;
use crate::prelude::*;
use std::time::Instant;

//use std::iter::Peekable;
//use std::fmt;
//
pub enum BAL {
    S(i8),
    Nul,
    Bar,
}

fn main() {
    let mut v = String::from("");

    let pt = compiler::parse("!:ref x = 0; range 0 10000000 1 { .x = x + 1 }; x");
    let global = create_wlamba_prelude();

    v = compiler::eval_tree(pt, global, 2);
    println!("> {:?}", v);
}

