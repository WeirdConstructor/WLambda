#[cfg(feature="regex")]
use std::fs::File;
#[cfg(feature="regex")]
use std::io::{BufRead, BufReader};

#[cfg(feature="regex")]
use wlambda::prelude::*;

#[cfg(feature="regex")]
fn get_functions_from_file(filename: &str) -> Vec<String> {
    use regex::Regex;
    let f = File::open(filename).expect("Open file");

    let rx = Regex::new("^####\\s+(?:<a.*?</a>\\s*)?[0-9\\.]*\\s*-\\s*(\\S+)").unwrap();

    let mut functions = Vec::new();

    for (_, l) in BufReader::new(f).lines().enumerate() {
        let line = l.unwrap();

        if let Some(c) = rx.captures(&line) {
            for cap in c.iter() {
                if let Some(c) = cap {
                    functions.push(c.as_str().to_string());
                }
            }
        }
    }

    functions
}

#[cfg(feature="regex")]
#[test]
fn wlambda_functions() {
    let documented_funs = get_functions_from_file("src/prelude.rs");

    let core_syms = core_symbol_table();
    let mut core_syms = core_syms.list();
    core_syms.sort();
    for core_sym in core_syms {
        if let Some(_) = documented_funs.iter().find(|f: &&String| **f == core_sym) {
            println!("OK - '{}'", core_sym);
        } else {
            panic!("Undocumented core function: '{}'", core_sym);
        }
    }

    let std_syms = std_symbol_table();
    let mut std_syms = std_syms.list();
    std_syms.sort();
    for std_sym in std_syms {
        let std_sym = "std:".to_string() + &std_sym;
        if let Some(_) = documented_funs.iter().find(|f: &&String| **f == std_sym) {
            println!("OK - {}", std_sym);
        } else {
            panic!("Undocumented core function: '{}'", std_sym);
        }
    }
}
