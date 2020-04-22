// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

mod vval;
mod parser;
mod compiler;
mod ops;
mod vm;
mod prelude;
mod util;
mod nvec;
mod vval_user_obj;
mod csv;
mod threads;

use vval::Env;
use vval::VVal;
use crate::compiler::{GlobalEnv, EvalContext};

const VERSION: &str = env!("CARGO_PKG_VERSION");

use std::cell::RefCell;
use std::rc::Rc;
use fnv::FnvHashMap;

struct Cached {
//    obj: std::rc::Weak<RefCell<FnvHashMap<String, VVal>>>,
    obj: u64,
    key: std::rc::Rc<RefCell<String>>,
    idx: usize,
}

impl Cached {
    fn is_entry(&self, o: u64, k: &VVal) -> Option<usize> {
        if self.obj == o && k.ptr_eq_s(&self.key) {
            Some(self.idx)
        } else {
            None
        }
    }
}

fn benchme(n: usize) {
    let v = VVal::map();
    v.set_key_mv(String::from("x"), VVal::Int(10));
    v.set_key_mv(String::from("y"), VVal::Int(11));
    v.set_key_mv(String::from("a"), VVal::Int(12));
    v.set_key_mv(String::from("b"), VVal::Int(13));

    let t = std::time::Instant::now();
    let mut sum = 0;
    for i in 0..n {
        let x = v.get_key("x").unwrap().i();
        let y = v.get_key("y").unwrap().i();
        let a = v.get_key("a").unwrap().i();
        let b = v.get_key("b").unwrap().i();
        sum += x + y + a + b;
    }

    println!("SUM: {} in {}", sum, t.elapsed().as_millis());

    let t = std::time::Instant::now();
    let k_x = VVal::new_str_mv(String::from("x"));
    let k_y = VVal::new_str_mv(String::from("y"));
    let k_a = VVal::new_str_mv(String::from("a"));
    let k_b = VVal::new_str_mv(String::from("b"));
    let mut c_x : Option<Cached> = None;
    let mut c_y : Option<Cached> = None;
    let mut c_a : Option<Cached> = None;
    let mut c_b : Option<Cached> = None;
    if let VVal::Map(f) = &v {
        if let VVal::Str(s) = &k_x {
            c_x = Some(Cached {
                obj: 11,
                key: s.clone(),
                idx: 0,
            });
        }
        if let VVal::Str(s) = &k_y {
            c_y = Some(Cached {
                obj: 11,
                key: s.clone(),
                idx: 1,
            });
        }
        if let VVal::Str(s) = &k_a {
            c_a = Some(Cached {
                obj: 11,
                key: s.clone(),
                idx: 2,
            });
        }
        if let VVal::Str(s) = &k_b {
            c_b = Some(Cached {
                obj: 11,
                key: s.clone(),
                idx: 3,
            });
        }
    }
    let dat = VVal::vec();
    dat.set(0, VVal::Int(10));
    dat.set(1, VVal::Int(11));
    dat.set(2, VVal::Int(12));
    dat.set(3, VVal::Int(13));
    let mut sum = 0;
    for i in 0..n {
        let x =
            if let Some(c_x) = &c_x {
                if let Some(idx) = c_x.is_entry(11, &k_x) {
                    dat.at(idx).unwrap()
                } else {
                    VVal::None
                }
            } else {
                    VVal::None
            };
        let y =
            if let Some(c_y) = &c_y {
                if let Some(idx) = c_y.is_entry(11, &k_y) {
                    dat.at(idx).unwrap()
                } else {
                    VVal::None
                }
            } else {
                    VVal::None
            };
        let a =
            if let Some(c_a) = &c_a {
                if let Some(idx) = c_a.is_entry(11, &k_a) {
                    dat.at(idx).unwrap()
                } else {
                    VVal::None
                }
            } else {
                    VVal::None
            };
        let b =
            if let Some(c_b) = &c_b {
                if let Some(idx) = c_b.is_entry(11, &k_b) {
                    dat.at(idx).unwrap()
                } else {
                    VVal::None
                }
            } else {
                    VVal::None
            };
        sum += x.i() + y.i() + a.i() + b.i();
    }

    println!("SUM: {} in {}", sum, t.elapsed().as_millis());
}

fn main() {
    benchme(10000000);
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

    let global = GlobalEnv::new_default();
    global.borrow_mut().add_func(
        "dump_stack",
        move |env: &mut Env, _argc: usize| {
            env.dump_stack();
            Ok(VVal::None)
        }, Some(0), Some(0));

    let mut ctx = EvalContext::new(global);

    let argv : Vec<String> = std::env::args().collect();
    if argv.len() > 1 {
        if argv[1] == "-parse" {
            let contents = std::fs::read_to_string(&argv[2]).unwrap();
            parser::parse(&contents, &argv[2]);
        } else {
            match ctx.eval_file(&argv[1]) {
                Ok(_) => (),
                Err(e) => {
                    eprintln!("ERROR: {}", e);
                }
            }
        }
        return;
    }

    #[cfg(feature="rustline")]
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
                        Ok(v)  => { println!("> {}", v.s()); },
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

    #[cfg(not (feature="rustline"))]
    {
        eprintln!("WLambda Version {}", VERSION);
        loop {
            use std::io::{self, BufRead};
            for line in io::stdin().lock().lines() {
                match ctx.eval(&line.unwrap()) {
                    Ok(v)  => { println!("> {}", v.s()); },
                    Err(e) => { println!("*** {}", e); }
                }
            }
        }
    }
}

