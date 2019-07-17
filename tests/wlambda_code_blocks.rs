use std::fs::File;
use regex::Regex;
use std::io::{BufRead, BufReader, Result};

fn get_scripts_from_file(filename: &str) -> Vec<(String, String)> {
    let f = File::open(filename).expect("Open file");

    let rx = Regex::new("```wlambda").unwrap();
    let rx_end = Regex::new("```").unwrap();

    let mut code_snippets = Vec::new();

    let mut code_name = String::from("");
    let mut code      = String::from("");
    let mut in_code = false;
    for (lidx, l) in BufReader::new(f).lines().enumerate() {
        let line = l.unwrap();

        if in_code && rx_end.is_match(&line) {
            in_code = false;
            code_snippets.push((code_name, code));
            code_name = String::from("");
            code      = String::from("");

        } else if !in_code && rx.is_match(&line) {
            in_code = true;
            code_name = format!("{} line {}", filename, lidx + 1);

        } else if in_code {
            code += &line;
            code += "\n";
        }
    }

    code_snippets
}

#[test]
fn main() {
    use wlambda::prelude::create_wlamba_prelude;
    use wlambda::compiler::EvalContext;

    let lfmr =
        std::rc::Rc::new(
            std::cell::RefCell::new(
                wlambda::compiler::LocalFileModuleResolver::new()));

    for (name, snip) in get_scripts_from_file("src/prelude.rs") {
        let global = create_wlamba_prelude();
        global.borrow_mut().set_resolver(lfmr.clone());
        let mut ctx = EvalContext::new(global);
        match ctx.eval(&snip) {
            Ok(v) => { println!("result '{}': {}", name, v.s()); },
            Err(e) => {
                panic!(format!("Failed code snippet '{}': {}",
                               name, e));
            }
        }
    }
}
