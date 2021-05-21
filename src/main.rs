// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use wlambda::vval::Env;
use wlambda::vval::{StackAction, VVal};
use wlambda::compiler::{GlobalEnv, EvalContext, EvalError};

const VERSION: &str = env!("CARGO_PKG_VERSION");

const DOCUMENTATION_JSON: &str = include_str!("cmdline_doc.json");

struct DocSection {
    title:  String,
    body:   Vec<String>,
}

impl DocSection {
    fn print_title(&self) {
        println!("*** {}", self.title);
    }

    fn print(&self) {
        println!("*** {}", self.title);
        for l in self.body.iter() {
            println!("    {}", l);
        }
    }
}

struct Doc {
    sections: Vec<DocSection>,
}

impl Doc {
    fn new(doc: VVal) -> Self {
        let mut sections = vec![];
        doc.for_each(|v| {
            let title = v.v_s_raw(0);

            let mut body = vec![];
            v.v_(1).for_each(|l| {
                l.with_s_ref(|s|{
                    body.push(s.to_string());
                });
            });

            sections.push(DocSection {
                title,
                body,
            });
        });

        Self {
            sections
        }
    }

    fn print_sections_by_title(&self, parts: &[&str], with_body: bool) {
        for section in self.sections.iter() {
            let mut all_found = true;
            for p in parts {
                if !section.title.to_lowercase().contains(&p.to_lowercase()) {
                    all_found = false;
                }
            }

            if all_found {
                if with_body {
                    section.print();
                } else {
                    section.print_title();
                }
            }
        }
    }

    fn print_sections_by_title_or_body(&self, parts: &[&str], with_body: bool) {
        for section in self.sections.iter() {
            let mut all_found = true;
            for p in parts {
                if !section.title.to_lowercase().contains(&p.to_lowercase()) {
                    all_found = false;
                }
            }

            if !all_found {
                all_found = true;

                for p in parts {
                    for l in section.body.iter() {
                        if !l.to_lowercase().contains(&p.to_lowercase()) {
                            all_found = false;
                        }
                    }
                }
            }

            if all_found {
                if with_body {
                    section.print();
                } else {
                    section.print_title();
                }
            }
        }
    }
}

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
    let doc = VVal::from_json(DOCUMENTATION_JSON).unwrap();
    let doc = Doc::new(doc);

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
                Ok(v) => {
                    std::process::exit(v.i() as i32);
                },
                Err(EvalError::ExecError(StackAction::Break(v)))  => {
                    std::process::exit(v.i() as i32);
                },
                Err(EvalError::ExecError(StackAction::Return(v)))  => {
                    std::process::exit(v.1.i() as i32);
                },
                Err(e) => {
                    eprintln!("ERROR: {}", e);
                    std::process::exit(1);
                }
            }
        }
        return;
    }

    fn handle_doc_commands(doc: &Doc, line: &str) -> bool {
        let cmd = line.split_whitespace().collect::<Vec<&str>>();
        if cmd.len() > 1 {
            match cmd[0] {
                "?#" | "?" => {
                    doc.print_sections_by_title(
                        &cmd[1..], cmd[0] != "?#");
                    return true;
                },
                "?#*" | "?*" => {
                    doc.print_sections_by_title_or_body(
                        &cmd[1..], cmd[0] != "?#");
                    return true;
                },
                _ => {}
            }
        } else if cmd.len() == 1 {
            if cmd[0] == "?" {
                println!("REPL Usage:");
                println!("?   <term1> <term2> ... - Search in section headers");
                println!("?#  <term1> <term2> ... - Search in section headers, display only headers");
                println!("?*  <term1> <term2> ... - Search in section bodies too");
                println!("?*# <term1> <term2> ... - Search in section bodies too, display only headers");
            }
            return true;
        }

        false
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

                    if handle_doc_commands(&doc, &line) {
                        continue;
                    }

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

                if handle_doc_commands(&doc, &l) {
                    continue;
                }

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

