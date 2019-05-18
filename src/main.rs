// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;

use compiler::eval;
use crate::prelude::create_wlamba_prelude;

fn main() {
    let global = create_wlamba_prelude();
    let mut rl = rustyline::Editor::<()>::new();
    loop {
    let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let r = eval(&line, global.clone());
                println!("> {}", r.s());
            },
            Err(_) => {
                println!("No input");
                break;
            },
        }
    }
}

