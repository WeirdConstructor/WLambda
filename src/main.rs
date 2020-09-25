// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use wlambda::vval::Env;
use wlambda::vval::VVal;
use wlambda::compiler::{GlobalEnv, EvalContext};

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
//    println!("sizeof {} Result<> bytes", std::mem::size_of::<Result<VVal, crate::vval::StackAction>>());
//    println!("sizeof {} SynPos bytes", std::mem::size_of::<crate::vval::SynPos>());
//    println!("sizeof {} NVec<f64> bytes", std::mem::size_of::<crate::nvec::NVec<f64>>());
//    println!("sizeof {} Op bytes", std::mem::size_of::<crate::ops::Op>());
//    println!("sizeof {} ResPos bytes", std::mem::size_of::<crate::compiler::ResPos>());
//    println!("sizeof {} VVal bytes", std::mem::size_of::<VVal>());
//    println!("sizeof {} Box<String> bytes", std::mem::size_of::<Box<String>>());

////    println!("sizeof OP:{} bytes", std::mem::size_of::<(ResPos, Box<String>, Box<String>, Box<String>, ResPos)>());
//
//    let argv : Vec<String> = std::env::args().collect();
//    let contents = std::fs::read_to_string(&argv[1]).unwrap();
//    let r = crate::vm::gen(&contents);
//    println!("R: {}", r);
//    return;

    let argv : Vec<String> = std::env::args().collect();

    let global = GlobalEnv::new_default();
    global.borrow_mut().add_func(
        "dump_stack",
        move |env: &mut Env, _argc: usize| {
            env.dump_stack();
            Ok(VVal::None)
        }, Some(0), Some(0));


    let mut ctx = EvalContext::new(global);

    let v_argv = VVal::vec();
    for a in argv.iter().skip(1) {
        v_argv.push(VVal::new_str(&a));
    }
    ctx.set_global_var("@@", &v_argv);

    if argv.len() > 1 {
        if argv[1] == "-parse" {
            let contents = std::fs::read_to_string(&argv[2]).unwrap();
            wlambda::parser::parse(&contents, &argv[2]).expect("successful parse");

        } else if argv.len() > 2 && argv[1] == "-e" {
            v_argv.delete_key(&VVal::Int(0)).expect("-e argument");
            v_argv.delete_key(&VVal::Int(0)).expect("script argument");

            match ctx.eval(&argv[2]) {
                Ok(v)  => { v.with_s_ref(|s| println!("{}", s)); },
                Err(e) => { println!("*** {}", e); }
            }
        } else {
            v_argv.delete_key(&VVal::Int(0)).expect("file argument");

            match ctx.eval_file(&argv[1]) {
                Ok(_) => (),
                Err(e) => {
                    eprintln!("ERROR: {}", e);
                }
            }
        }
        return;
    }

    #[cfg(feature="rustyline")]
    {
        let mut rl = rustyline::Editor::<()>::new();
        if rl.load_history("wlambda.history").is_ok() {
            println!("Loaded history from 'wlambda.history' file.");
        }

        eprintln!("WLambda Version {}", VERSION);
        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    match ctx.eval(&line) {
                        Ok(v)  => {
                            println!("> {}", v.s());
                            ctx.set_global_var("@@", &v);
                        },
                        Err(e) => { println!("*** {}", e); }
                    }
                },
                Err(_) => { break; },
            }
        }
        if rl.save_history("wlambda.history").is_ok() {
            println!("Saved history to 'wlambda.history'");
        }
    }

    #[cfg(not (feature="rustyline"))]
    {
        eprintln!("WLambda Version {}", VERSION);
        loop {
            use std::io::{self, BufRead};
            for line in io::stdin().lock().lines() {
                let l = line.unwrap();

                match ctx.eval(&l) {
                    Ok(v)  => {
                        println!("> {}", v.s());
                        ctx.set_global_var("@@", &v);
                    },
                    Err(e) => { println!("*** {}", e); }
                }
            }
        }
    }
}

