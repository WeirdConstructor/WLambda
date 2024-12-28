#[cfg(feature = "regex")]
use std::fs::File;
#[cfg(feature = "regex")]
use std::io::{BufRead, BufReader};

#[cfg(feature = "regex")]
use wlambda::prelude::*;

#[cfg(feature = "regex")]
fn get_functions_from_file(filename: &str) -> Vec<String> {
    use regex::Regex;
    let f = File::open(filename).expect("Open file");

    let rx = Regex::new("^#####?\\s+(?:<a.*?</a>\\s*)?[0-9\\.]*\\s*-\\s*(\\S+)").unwrap();

    let mut functions = Vec::new();

    for (_, l) in BufReader::new(f).lines().enumerate() {
        let line = l.unwrap();

        if let Some(c) = rx.captures(&line) {
            for cap in c.iter() {
                if let Some(c) = cap {
                    functions.push(c.as_str().to_string().replace("\\_", "_"));
                }
            }
        }
    }

    functions
}

#[cfg(feature = "regex")]
#[test]
fn wlambda_functions() {
    let documented_funs = get_functions_from_file("doc/wlambda_reference.md");

    let mut total = 0;
    let mut count_missing = 0;

    let mut missing = vec![];

    let core_syms = core_symbol_table();
    let mut core_syms = core_syms.list();
    core_syms.sort();
    for core_sym in core_syms {
        total += 1;
        if let Some(_) = documented_funs.iter().find(|f: &&String| { println!("TEST: {} {}", **f, core_sym); **f == core_sym }) {
            println!("OK - '{}'", core_sym);
        } else {
            println!("MISSING - '{}'", core_sym);
            missing.push(core_sym);
            count_missing += 1;
        }
    }

    let std_syms = std_symbol_table();
    let mut std_syms = std_syms.list();
    std_syms.sort();
    for std_sym in std_syms {
        total += 1;
        let std_sym = "std:".to_string() + &std_sym;
        if let Some(_) = documented_funs.iter().find(|f: &&String| **f == std_sym) {
            println!("OK - {}", std_sym);
        } else {
            println!("MISSING - '{}'", std_sym);
            missing.push(std_sym);
            count_missing += 1;
        }
    }

    if count_missing > 0 {
        eprintln!("--------- MISSING SYMBOLS ---------");
        for s in missing {
            eprintln!("MISSING: {}", s);
        }
        panic!("Found {}/{} undocumented functions!", count_missing, total);
    } else {
        println!("Found {} functions!", total);
    }
}
