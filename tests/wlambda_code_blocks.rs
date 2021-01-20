#[cfg(feature="regex")]
use std::fs::File;
#[cfg(feature="regex")]
use std::io::{BufRead, BufReader};

#[cfg(feature="regex")]
fn get_scripts_from_file(filename: &str) -> Vec<(String, String)> {
    use regex::Regex;
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

#[cfg(feature="regex")]
fn execute_script(name: &str, snippet: &str) {
    use wlambda::EvalContext;

    print!("- code block '{}'...", name);
    let mut ctx = EvalContext::new_default();
    match ctx.eval(snippet) {
        Ok(v) => { println!("  result: {}", v.s()); },
        Err(e) => {
            panic!(format!("   FAILED code snippet '{}': {}", name, e));
        }
    }
}

#[test]
fn wlambda_code_blocks() {

    #[cfg(feature="regex")]
    {
        for (name, snip) in get_scripts_from_file("doc/wlambda_reference.md") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/prelude.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/parser.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/compiler.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/lib.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/vval.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/threads.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/formatter.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/packer.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/struct_pattern.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/selector.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/rpc_helper.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/util.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/stdlib/csv.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/stdlib/xml.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/stdlib/process.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/stdlib/mod.rs") {
            execute_script(&name, &snip);
        }

        for (name, snip) in get_scripts_from_file("src/stdlib/net.rs") {
            execute_script(&name, &snip);
        }
    }
}
