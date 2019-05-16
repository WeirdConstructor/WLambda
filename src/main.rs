// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;

use compiler::eval_tree;
use parser::parse;
use crate::prelude::create_wlamba_prelude;

fn main() {
    let pt = parse("!:ref x = 0; range 0 10000000 1 { .x = x + 1 }; x", 0).unwrap();
    let global = create_wlamba_prelude();
    let v = eval_tree(pt, global, 2);
    println!("> {:?}", v);
}

