// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;
use parser::*; // ParseState;
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
//    let mut ps = ParseState::new(" # weof weof w\n10", "<input>");
//    let mut ps = ParseState::new("#fo\n    # bare \n   10  ", "<input>");
    let mut ps = ParseState::new("10 +10 -2r1111 -16rFF", "<input>");

    let ret = parse_stmt(&mut ps);
    match ret {
        Ok(v)   => println!("OK: {}", v.s()),
        Err(x)  => println!("ERR: {}:\n---------------\n{}\n---------------\n", x, ps.rest()),
    }

    let ret = parse_stmt(&mut ps);
    match ret {
        Ok(v)   => println!("OK: {}", v.s()),
        Err(x)  => println!("ERR: {}:\n---------------\n{}\n---------------\n", x, ps.rest()),
    }

    let ret = parse_stmt(&mut ps);
    match ret {
        Ok(v)   => println!("OK: {}", v.s()),
        Err(x)  => println!("ERR: {}:\n---------------\n{}\n---------------\n", x, ps.rest()),
    }

    let ret = parse_stmt(&mut ps);
    match ret {
        Ok(v)   => println!("OK: {}", v.s()),
        Err(x)  => println!("ERR: {}:\n---------------\n{}\n---------------\n", x, ps.rest()),
    }

    let mut ps = ParseState::new("10", "in"); // +10 -2r1111 -16rFF", "<input>");
    println!("O: {}", ps.lookahead("10"));
    println!("R[{}]", ps.rest());

    let ff = VVal::Nul;
    println!("SI: {}", std::mem::size_of::<VVal>());
    println!("SI: {}", std::mem::size_of::<Syntax>());
    println!("SI: {}", std::mem::size_of::<BAL>());
    println!("SI: {}", std::mem::size_of::<Rc<RefCell<std::vec::Vec<VVal>>>>());

//    let o  : i64 = 2575;
//    let pf : f64 = p as f64;
//    let po : f64 = o as f64;
//    let f  : f64 = pf / po;

//    ps.cur = ps.chars.next().unwrap();
//    println!("FOJ {}", ps.chars.next().unwrap());
//    println!("FO: {:?}", ps.cur);
}

