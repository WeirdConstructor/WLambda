// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;

use vval::Env;
use vval::VVal;
use crate::prelude::create_wlamba_prelude;

fn main() {
    let global = create_wlamba_prelude();
    global.borrow_mut().add_func(
        "dump_stack",
        move |env: &mut Env, _argc: usize| {
            env.dump_stack();
            Ok(VVal::Nul)
        }, Some(0), Some(0));

    let mut ctx = compiler::EvalContext::new(global);

    let mut rl = rustyline::Editor::<()>::new();
    if rl.load_history("wlambda.history").is_ok() {
        println!("Loaded history from 'wlambda.history' file.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                match ctx.eval(&line) {
                    Ok(v) => {
                        println!("> {}", v.s());
                    },
                    Err(e) => {
                        println!("*** {}", e);
                    }
                }
            },
            Err(_) => { break; },
        }
    }
    if rl.save_history("wlambda.history").is_ok() {
        println!("Saved history to 'wlambda.history'");
    }
}

