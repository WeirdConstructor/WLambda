// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod prelude;

use vval::Env;
use vval::VVal;
use compiler::eval_in_ctx;
use crate::prelude::create_wlamba_prelude;

struct ReplState {
    dump_stack: bool,
}

fn main() {
    let global = create_wlamba_prelude();
    global.borrow_mut().add_func("dump_stack", move |env: &mut Env, _argc: usize| {
        env.dump_stack();
        Ok(VVal::Nul)
    });

    let mut ctx = compiler::EvalContext::new(global);

    let mut rl = rustyline::Editor::<()>::new();
    rl.load_history("wlambda.history");
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                match eval_in_ctx(&line, &mut ctx) {
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
    rl.save_history("wlambda.history");
}

