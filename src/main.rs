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
    let mut rts = 0.0;
    let mut cnt = 0;
    let mut v = String::from("");
    for i in 0..10 {
        let now = Instant::now();
        v = eval("!:ref x = 0; range 0 1000000 1 { .x = x + 1 }; x");
        rts = rts + (now.elapsed().as_millis() as f64);
        cnt = cnt + 1;
    }
    println!("*** runtime: {} ({} runs)", rts / (cnt as f64), cnt);
    println!("> {:?}", v);
}

