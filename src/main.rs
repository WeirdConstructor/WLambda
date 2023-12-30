// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use wlambda::compiler::{EvalContext, EvalError, GlobalEnv};
use wlambda::vval::Env;
use wlambda::vval::{StackAction, VVal};

const VERSION: &str = env!("CARGO_PKG_VERSION");

const DOCUMENTATION_JSON: &str = include_str!("cmdline_doc.json");

struct DocSection {
    title: String,
    body: Vec<String>,
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
                l.with_s_ref(|s| {
                    body.push(s.to_string());
                });
            });

            sections.push(DocSection { title, body });
        });

        Self { sections }
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

#[cfg(feature = "serde_json")]
fn get_doc() -> VVal {
    VVal::from_json(DOCUMENTATION_JSON).unwrap()
}

#[cfg(not(feature = "serde_json"))]
fn get_doc() -> VVal {
    VVal::None
}

fn print_info() {
    eprintln!("WLambda Version {}", VERSION);
    eprintln!("Copyright (C) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>");
    eprintln!("License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.");
    eprintln!("This is free software: you are free to change and redistribute it.");
    eprintln!("There is NO WARRANTY, to the extent permitted by law.");
    eprintln!("");
    eprintln!("For documentation visit: <http://wlambda.m8geil.de>.");
    eprintln!("");
}

fn main() {
    use std::cell::RefCell;
    use std::rc::Rc;
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
    let doc = get_doc();
    let doc = Doc::new(doc);

    let argv: Vec<String> = std::env::args().collect();

    let lfmr = Rc::new(RefCell::new(wlambda::compiler::LocalFileModuleResolver::new()));

    let global = GlobalEnv::new_default();
    global.borrow_mut().set_resolver(lfmr.clone());
    global.borrow_mut().add_func(
        "dump_stack",
        move |env: &mut Env, _argc: usize| {
            env.dump_stack();
            Ok(VVal::None)
        },
        Some(0),
        Some(0),
    );

    let mut ctx = EvalContext::new(global);

    let v_argv = VVal::vec();
    for a in argv.iter().skip(1) {
        v_argv.push(VVal::new_str(a));
    }
    ctx.set_global_var("@@", &v_argv);
    ctx.set_global_var("@path", &VVal::None);
    ctx.set_global_var("@dir", &VVal::None);

    if let Ok(exe_path) = std::env::current_exe() {
        let buf: Vec<u8> = std::fs::read(exe_path).expect("wlambda.exe can open own EXE file");

        let mut exe_part = &buf[..];
        let mut tail = None;
        let mut no_print_info = false;
        for i in 0..buf.len() {
            if (i + 7) < buf.len()
                && buf[i] == b'W'
                && buf[i + 1] == b'L'
                && buf[i + 2] == b'T'
                && buf[i + 3] == b'A'
                && buf[i + 4] == b'I'
                && buf[i + 5] == b'L'
                && (buf[i + 6] == b'~' || buf[i + 6] == b'#')
            {
                no_print_info = buf[i + 6] != b'~';

                exe_part = &buf[0..i];
                tail = Some(buf[i + 7..].to_vec());
                break;
            }
        }

        if argv.len() > 3 && (argv[1] == "-p" || argv[1] == "-P") {
            let input_zip = std::fs::read(&argv[2]).expect("Can read input file");
            let output_file = argv[3].clone();
            let mut outfile_data = exe_part.to_vec();
            let mut tag = b"VKSAHK".to_vec();
            for ch in tag.iter_mut() {
                if *ch != b'A' {
                    *ch += 1;
                }
            }
            outfile_data.extend_from_slice(&tag[..]);
            outfile_data.extend_from_slice(if argv[1] == "-P" { b"#" } else { b"~" });
            outfile_data.extend_from_slice(&input_zip);
            std::fs::write(&output_file, outfile_data).expect("Can read output file");
            println!("Written '{}'", output_file);
            return;
        } else if argv.len() > 2 && argv[1] == "-x" {
            if let Some(tail) = tail {
                let output_file = argv[2].clone();
                std::fs::write(&output_file, tail).expect("Can read output file");
                println!("Written '{}'", output_file);
            }
            return;
        }

        if let Some(tail) = tail {
            ctx.set_global_var("_ENABLE_WLAMBDA_REPL_", &VVal::Bol(false));
            #[allow(unused_mut)]
            let mut handled_zip = false;

            #[cfg(feature = "zip")]
            {
                use std::io::Cursor;
                use std::io::Read;

                let mut tail_buf = Cursor::new(&tail[..]);
                if let Ok(mut zip_arch) = zip::ZipArchive::new(&mut tail_buf) {
                    let mut main_file = None;
                    handled_zip = true;

                    for i in 0..zip_arch.len() {
                        let mut file = zip_arch.by_index(i).expect("Indexing ZIP file works");
                        let mut code = String::new();
                        if file.is_file() {
                            match file.read_to_string(&mut code) {
                                Ok(_) => {
                                    if file.name() == "main.wl" {
                                        main_file = Some(code);
                                    } else {
                                        eprintln!(
                                            "wltail preload: {} (len={})",
                                            file.name(),
                                            code.len()
                                        );
                                        lfmr.borrow_mut().preload(file.name(), code);
                                    }
                                }
                                Err(e) => {
                                    eprintln!(
                                        "Couldn't load embedded code file '{}': {}",
                                        file.name(),
                                        e
                                    );
                                }
                            }
                        }
                    }

                    if let Some(main_code) = main_file {
                        if !no_print_info {
                            print_info();
                        }

                        match ctx.eval(&main_code) {
                            Ok(v) => {
                                let repl_flag = ctx
                                    .get_global_var("_ENABLE_WLAMBDA_REPL_")
                                    .unwrap_or(VVal::None);
                                if !repl_flag.b() {
                                    std::process::exit(v.i() as i32);
                                }
                            }
                            Err(EvalError::ExecError(StackAction::Break(v))) => {
                                std::process::exit(v.i() as i32);
                            }
                            Err(EvalError::ExecError(StackAction::Return(v))) => {
                                std::process::exit(v.1.i() as i32);
                            }
                            Err(e) => {
                                eprintln!("Appended main.wl ERROR: {}", e);
                                std::process::exit(1);
                            }
                        }
                    }
                }
            }

            if !handled_zip {
                let code = std::str::from_utf8(&tail).expect("Tail is proper UTF-8");
                if !no_print_info {
                    print_info();
                }

                match ctx.eval(code) {
                    Ok(v) => {
                        let repl_flag =
                            ctx.get_global_var("_ENABLE_WLAMBDA_REPL_").unwrap_or(VVal::None);
                        if !repl_flag.b() {
                            std::process::exit(v.i() as i32);
                        }
                    }
                    Err(EvalError::ExecError(StackAction::Break(v))) => {
                        std::process::exit(v.i() as i32);
                    }
                    Err(EvalError::ExecError(StackAction::Return(v))) => {
                        std::process::exit(v.1.i() as i32);
                    }
                    Err(e) => {
                        eprintln!("APPENDED CODE ERROR: {}", e);
                        std::process::exit(1);
                    }
                }
            }
        }
    }

    let repl_flag = ctx.get_global_var("_ENABLE_WLAMBDA_REPL_").unwrap_or(VVal::None).b();

    if !repl_flag && argv.len() > 1 {
        if argv[1] == "-parse" {
            let contents = std::fs::read_to_string(&argv[2]).unwrap();
            wlambda::parser::parse(&contents, &argv[2]).expect("successful parse");
        } else if argv.len() > 2 && argv[1] == "-e" {
            v_argv.delete_key(&VVal::Int(0)).expect("-e argument");
            v_argv.delete_key(&VVal::Int(0)).expect("script argument");

            match ctx.eval(&argv[2]) {
                Ok(v) => {
                    v.with_s_ref(|s| println!("{}", s));
                }
                Err(e) => {
                    println!("*** {}", e);
                }
            }
        } else {
            v_argv.delete_key(&VVal::Int(0)).expect("file argument");

            match ctx.eval_file(&argv[1]) {
                Ok(v) => {
                    std::process::exit(v.i() as i32);
                }
                Err(EvalError::ExecError(StackAction::Break(v))) => {
                    std::process::exit(v.i() as i32);
                }
                Err(EvalError::ExecError(StackAction::Return(v))) => {
                    std::process::exit(v.1.i() as i32);
                }
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
                    doc.print_sections_by_title(&cmd[1..], cmd[0] == "?#");
                    return true;
                }
                "?#*" | "?*" => {
                    doc.print_sections_by_title_or_body(&cmd[1..], cmd[0] == "?#");
                    return true;
                }
                _ => {}
            }
        } else if cmd.len() == 1 && cmd[0] == "?" {
            println!("REPL Usage:");
            println!("?   <term1> <term2> ... - Search in section headers, display only headers");
            println!("?#  <term1> <term2> ... - Search in section headers");
            println!(
                "?*  <term1> <term2> ... - Search in section bodies too, display only headers"
            );
            println!("?#* <term1> <term2> ... - Search in section bodies too");
            return true;
        }

        false
    }

    #[cfg(feature = "rustyline")]
    {
        let mut rl = rustyline::Editor::<()>::new();
        if rl.load_history("wlambda.history").is_ok() {
            println!("Loaded history from 'wlambda.history' file.");
        }

        print_info();
        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());

                    if handle_doc_commands(&doc, &line) {
                        continue;
                    }

                    match ctx.eval(&line) {
                        Ok(v) => {
                            println!("> {}", v.s());
                            ctx.set_global_var("@@", &v);
                        }
                        Err(e) => {
                            println!("*** {}", e);
                        }
                    }
                }
                Err(_) => {
                    break;
                }
            }
        }
        if rl.save_history("wlambda.history").is_ok() {
            println!("Saved history to 'wlambda.history'");
        }
    }

    #[cfg(not(feature = "rustyline"))]
    {
        print_info();
        loop {
            use std::io::{self, BufRead};
            for line in io::stdin().lock().lines() {
                let l = line.unwrap();

                if handle_doc_commands(&doc, &l) {
                    continue;
                }

                match ctx.eval(&l) {
                    Ok(v) => {
                        println!("> {}", v.s());
                        ctx.set_global_var("@@", &v);
                    }
                    Err(e) => {
                        println!("*** {}", e);
                    }
                }
            }
        }
    }
}
