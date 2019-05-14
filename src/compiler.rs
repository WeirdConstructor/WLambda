// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::parser::*; // ParseState;
use crate::prelude::*;
use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::Env;
use crate::vval::VValFun;
use crate::vval::EvalNode;
use crate::vval::StackAction;
use crate::vval::VarPos;
use std::rc::Rc;
use std::cell::RefCell;
use std::time::Instant;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
struct CompileLocal {
    is_upvalue: bool,
    index:      usize,
    name:       String,
}

//pub struct GUA<'a> {
//    e: &'a mut Env,
//}
//
//impl<'a> Drop for GUA<'a> {
//    fn drop(&mut self) {
//        println!("DROPENV!");
//    }
//}
//
#[derive(Debug, Clone)]
pub struct GlobalEnv {
    env: std::collections::HashMap<String, VVal>,
}

pub type GlobalEnvRef = Rc<RefCell<GlobalEnv>>;

impl GlobalEnv {
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=7011ec14ebade14fe62e438a0db52c98
    // 'static Lifetime != program length: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=74450a3fbb85f3baa6e667b85621152a
    //  simplified: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=45f376a6ef06d81ebbc11f08ec55d918
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=94811153fadd511effa306e5369e5b19
    // FnMut: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=ef42feadce57ac61b63ec7c6dc274b2b
    pub fn add_func<T>(&mut self, fnname: &str, fun: T)
        where T: 'static + Fn(&mut Env, usize) -> Result<VVal,StackAction> {
        self.env.insert(
            String::from(fnname),
            VValFun::new(Rc::new(RefCell::new(fun)), Vec::new(), 0));
    }

    pub fn new() -> GlobalEnvRef {
        Rc::new(RefCell::new(GlobalEnv {
            env: std::collections::HashMap::new()
        }))
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct CompileEnv {
    parent:    Option<Rc<RefCell<CompileEnv>>>,
    global:    GlobalEnvRef,
    local_map: std::collections::HashMap<String, VarPos>,
    locals:    std::vec::Vec<CompileLocal>,
    upvals:    std::vec::Vec<VarPos>,
}

type CompileEnvRef = Rc<RefCell<CompileEnv>>;

impl CompileEnv {

    fn create_env(parent: Option<CompileEnvRef>) -> Rc<RefCell<CompileEnv>> {
        let global = if let Some(p) = &parent {
            p.borrow_mut().global.clone()
        } else {
            GlobalEnv::new()
        };
        Rc::new(RefCell::new(CompileEnv {
            parent:    parent,
            global:    global,
            local_map: std::collections::HashMap::new(),
            locals:    Vec::new(),
            upvals:    Vec::new(),
        }))
    }

    fn def_up(&mut self, s: &str, pos: VarPos) -> usize {
        let next_index = self.upvals.len();
        //d// println!("NEW UPVALUE {:?} => {}", pos, next_index);
        self.upvals.push(pos);
        self.local_map.insert(String::from(s), VarPos::UpValue(next_index));
        next_index
    }

    fn def(&mut self, s: &str) -> usize {
        let next_index = self.locals.len();
        self.locals.push(CompileLocal {
            is_upvalue: false,
            index:      next_index,
            name:       String::from(s),
        });
        self.local_map.insert(String::from(s), VarPos::Local(next_index));
        next_index
    }

    fn copy_upvals(&self, e: &mut Env, upvalues: &mut std::vec::Vec<VVal>) {
        for p in self.upvals.iter() {
            let mut v = match p {
                VarPos::UpValue(i) => e.get_up_raw(*i),
                VarPos::Local(i)   => e.get_local_raw(*i),
                VarPos::NoPos      => VVal::Nul,
            };
            //d// println!("COPY AN UPVAL! {:?}({})", p, v.s());
            if let VVal::WRef(_) = v {
                v = v.downgrade().unwrap_or(VVal::Nul);
            }
            upvalues.push(v.clone());
        }
    }

    fn local_env_size(ce: &CompileEnvRef) -> usize {
        ce.borrow().locals.len()
    }

    fn mark_upvalue(&mut self, idx: usize) {
        self.locals[idx].is_upvalue = true;
    }

/*

    { !x = 10;      // local upvalue Rc
                    // copies local upv to upvx in func
        { x         // access upvalue of func
                    // copies upvalue of func to upvc in func
            { x     // access upvalue of func


*/

    fn get(&mut self, s: &str) -> VarPos {
        let pos = self.local_map.get(s);
        match pos {
            None => {
                let opt_p = self.parent.as_mut();
                if let None = opt_p { return VarPos::NoPos; }
                let parent = opt_p.unwrap().clone();
                let mut par_mut = parent.borrow_mut();

                let pos = par_mut.local_map.get(s).cloned();
                let par_var_pos = if let Some(pp) = pos {
                    pp
                } else {
                    par_mut.get(s)
                };
                match par_var_pos {
                    VarPos::Local(i) => {
                        par_mut.mark_upvalue(i);
                        VarPos::UpValue(self.def_up(s, par_var_pos))
                    },
                    VarPos::UpValue(_) => VarPos::UpValue(self.def_up(s, par_var_pos)),
                    VarPos::NoPos      => VarPos::NoPos
                }
            }
            Some(p) => { p.clone() },
        }
    }
}

fn compile_block(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, String> {
    let exprs : Vec<EvalNode> = ast.map_skip(|e| compile(e, ce).unwrap(), 1);

    #[allow(unused_assignments)]
    Ok(Box::new(move |e: &mut Env| {
        let mut res = VVal::Nul;
        for x in exprs.iter() {
            res = VVal::Nul;
            res = x(e)?;
        }
        Ok(res)
    }))
}

fn compile_var(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, String> {
    let var = ast.at(1).unwrap();

    let s = var.s_raw();
    match &s[..] {
        "_"  => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(0).clone()) })) },
        "_1" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(1).clone()) })) },
        "_2" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(2).clone()) })) },
        "_3" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(3).clone()) })) },
        "_4" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(4).clone()) })) },
        "_5" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(5).clone()) })) },
        "_6" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(6).clone()) })) },
        "_7" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(7).clone()) })) },
        "_8" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(8).clone()) })) },
        "_9" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(9).clone()) })) },
        "@"  => { Ok(Box::new(move |e: &mut Env| { Ok(e.argv()) })) },
        _ => {
            let pos = ce.borrow_mut().get(&s);
            //d// println!("ACCESS LOCAL: {} => {:?}", s, pos);
            match pos {
                VarPos::UpValue(i) =>
                    Ok(Box::new(move |e: &mut Env| { Ok(e.get_up(i)) })),
                VarPos::Local(i) =>
                    Ok(Box::new(move |e: &mut Env| { Ok(e.get_local(i)) })),
                VarPos::NoPos => {
                    match ce.borrow_mut().global.borrow().env.get(&s) {
                        Some(v) => {
                            let val = v.clone();
                            Ok(Box::new(move |_: &mut Env| { Ok(val.clone()) }))
                        },
                        None =>
                            Err(format!(
                                "Variable '{}' undefined", var.s_raw())),
                    }
                }
            }
        }
    }
}

fn compile_def(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_ref: bool, weak_ref: bool) -> Result<EvalNode, String> {
    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let cv      = compile(&value, ce)?;
    let idx     = ce.borrow_mut().def(&vars.at(0).unwrap().s_raw());

    if is_ref {
        if weak_ref {
            Ok(Box::new(move |e: &mut Env| {
                let mut v = cv(e)?;
                e.set_consume(idx, (&mut v).to_wref());
                Ok(v)
            }))
        } else {
            Ok(Box::new(move |e: &mut Env| {
                let mut v = cv(e)?;
                e.set_consume(idx, (&mut v).to_ref());
                Ok(v)
            }))
        }
    } else {
        Ok(Box::new(move |e: &mut Env| {
            let v = cv(e)?;
            e.set_local(idx, &v);
            Ok(v)
        }))
    }
}

fn compile_assign(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, String> {
    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let cv      = compile(&value, ce)?;
    let s       = &vars.at(0).unwrap().s_raw();
    let pos     = ce.borrow_mut().get(s);

    match pos {
        VarPos::UpValue(i) => {
            Ok(Box::new(move |e: &mut Env| {
                let v = cv(e)?;
                e.set_up(i, &v);
                Ok(v)
            }))
        },
        VarPos::Local(i) => {
            Ok(Box::new(move |e: &mut Env| {
                let v = cv(e)?;
                e.set_local(i, &v);
                Ok(v)
            }))
        },
        VarPos::NoPos => panic!(format!("assigning without definition of '{}'", s)),
    }
}

fn compile(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<EvalNode, String> {
//    println!("compile {}", ast.s());
    match ast {
        VVal::Lst(_l) => {
            let syn = if let VVal::Syn(s) = ast.at(0).unwrap() {
                s.syn
            } else {
                return Err(format!("Bad AST {:?}", ast));
            };

            match syn {
                Syntax::Block  => { compile_block(ast, ce)       },
                Syntax::Var    => { compile_var(ast, ce)         },
                Syntax::Def    => { compile_def(ast, ce, false, false)  },
                Syntax::DefRef => { compile_def(ast, ce, true, false)   },
                Syntax::DefWRef=> { compile_def(ast, ce, true, true)    },
                Syntax::Assign => { compile_assign(ast, ce)      },
                Syntax::Key    => {
                    let sym = ast.at(1).unwrap();
                    Ok(Box::new(move |_: &mut Env| { Ok(sym.clone()) }))
                },
                Syntax::SetKey => {
                    let map = compile(&ast.at(1).unwrap(), ce)?;
                    let sym = compile(&ast.at(2).unwrap(), ce)?;
                    let val = compile(&ast.at(3).unwrap(), ce)?;
                    Ok(Box::new(move |e: &mut Env| {
                        let m = map(e)?;
                        let s = sym(e)?;
                        let v = val(e)?;
                        let ks = s.s_raw();
                        m.set_key(&ks, v.clone());
                        Ok(v)
                    }))
                },
                Syntax::Lst    => {
                    let list_elems : Vec<EvalNode> = ast.map_skip(|e| compile(e, ce).unwrap(), 1);

                    Ok(Box::new(move |e: &mut Env| {
                        let v = VVal::vec();
                        for x in list_elems.iter() { v.push(x(e)?); }
                        Ok(v)
                    }))
                },
                Syntax::Map    => {
                    let list_elems : Vec<(EvalNode,EvalNode)> =
                        ast.map_skip(|e| {
                            let k = e.at(0).unwrap();
                            let v = e.at(1).unwrap();
                            (compile(&k, ce).unwrap(), compile(&v, ce).unwrap())
                        }, 1);

                    Ok(Box::new(move |e: &mut Env| {
                        let v = VVal::map();
                        for x in list_elems.iter() {
                            let ke = x.0(e)?;
                            let ks = ke.s_raw();
                            v.set_key(&ks, x.1(e)?);
                        }
                        Ok(v)
                    }))
                },
                Syntax::Call => {
                    let mut call_args : Vec<EvalNode> = ast.map_skip(|e| compile(e, ce).unwrap(), 1);
                    call_args.reverse();
                    let func = call_args.pop().expect("function in evaluation args list");

                    Ok(Box::new(move |e: &mut Env| {
                        let f    = func(e)?;
                        let argc = call_args.len();
                        e.with_pushed_sp(argc, |e: &mut Env| {
                            let mut i = 0;
                            for x in call_args.iter() {
                                let v = x(e)?;
                                e.set_arg(argc - (i + 1), v);
                                i = i + 1;
                            }
                            f.call(e, argc)
                        })
                    }))
                },
                Syntax::Func => {
                    let mut ce_sub = CompileEnv::create_env(Some(ce.clone()));

                    let stmts : Vec<EvalNode> = ast.map_skip(|e| compile(e, &mut ce_sub).unwrap(), 1);

                    #[allow(unused_assignments)]
                    let fun_ref = Rc::new(RefCell::new(move |env: &mut Env, _argc: usize| {
                        //d// println!("FFFFFFFFFFFFFFUUUUUUUUUUUUUUNCALLL");
                        //d// env.dump_stack();
                        let mut res = VVal::Nul;
                        for s in stmts.iter() {
                            res = VVal::Nul;
                            res = s(env)?;
                        }
                        Ok(res)
                    }));

                    let env_size = CompileEnv::local_env_size(&ce_sub);
                    Ok(Box::new(move |e: &mut Env| {
                        let mut v = Vec::new();
                        ce_sub.borrow_mut().copy_upvals(e, &mut v);
                        Ok(VValFun::new(fun_ref.clone(), v, env_size))
                    }))
                },
                _ => { Err(format!("bad input: {}", ast.s())) }
            }
        },
        _ => {
            let am = ast.clone();
            Ok(Box::new(move |_e: &mut Env| Ok(am.clone())))
        }
    }
}

fn mk(s: &str) -> ParseState {
    ParseState::new(s, 1)
}

pub fn parse(s: &str) -> VVal {
    let mut ps = mk(s);
    match parse_block(&mut ps, false) {
        Ok(v) => v,
        Err(e) => { panic!(format!("PARSE ERROR: {:?} at '{}' with input '{}'", e, ps.rest(), s)); },
    }
}

pub fn eval_tree(v: VVal, g: GlobalEnvRef, runs: u32) -> String {
    let mut ce = Rc::new(RefCell::new(CompileEnv {
        parent:    None,
        global:    g,
        local_map: std::collections::HashMap::new(),
        locals:    Vec::new(),
        upvals:    Vec::new(),
    }));

    let prog = compile(&v, &mut ce);
    match prog {
        Ok(r) => {
            let mut e = Env::new_s();
            e.push(VVal::Flt(42.42)); // 2nd arg
            e.push(VVal::Int(13));    // 1st arg
            e.argc = 2;
            e.set_bp(CompileEnv::local_env_size(&ce));

            if runs > 1 {
                let mut ret = String::from("");
                let mut rts = 0.0;
                let mut cnt = 0;
                for _ in 0..runs {
                    let now = Instant::now();
                    match r(&mut e) {
                        Ok(v)   => { ret = v.s() },
                        Err(je) => { panic!(format!("EXEC ERR: JUMPED {:?}", je)); }
                    }
                    rts = rts + (now.elapsed().as_millis() as f64);
                    cnt = cnt + 1;
                }
                println!("*** runtime: {} ({} runs)", rts / (cnt as f64), cnt);
                ret
            } else {
                match r(&mut e) {
                    Ok(v)   => { v.s() },
                    Err(je) => { panic!(format!("EXEC ERR: JUMPED {:?}", je)); }
                }
            }
        },
        Err(re) => { panic!(format!("COMPILE ERROR: {}", re)); },
    }
}

pub fn eval(s: &str) -> String {
    let global = create_wlamba_prelude();
    let v = parse(s);
    eval_tree(v, global, 1)
}


#[cfg(test)]
mod tests {
    use super::*;

//    fn parse_error(s: &str) -> String {
//        let mut ps = mk(s);
//        match parse_block(&mut ps, false) {
//            Ok(v)  => panic!(format!("Expected error but got result: {} for input '{}'",
//                                     v.s(), s)),
//            Err(e) => { format!("{}", e) },
//        }
//    }

    #[test]
    fn check_trivial() {
        assert_eq!(eval("_"),                       "13");          // XXX: in test env
        assert_eq!(eval("_1"),                      "42.42");       // XXX: in test env
        assert_eq!(eval("@"),                       "[13,42.42]");  // XXX: in test env

        assert_eq!(eval("$n"), "$n");
        assert_eq!(eval("10"), "10");
        assert_eq!(eval("10; 20; 30"),              "30");
        assert_eq!(eval("!x = 10; x"),              "10");
        assert_eq!(eval("!x = $true; x"),           "$true");
        assert_eq!(eval("{ 10 }"),                  "&VValFun");
        assert_eq!(eval("{ 10 }()"),                "10");
        assert_eq!(eval("{ 10; 20 }()"),            "20");
        assert_eq!(eval("!:ref x = 11; { 12; x }()"),                   "11");
        assert_eq!(eval("!x = 11; { 12; x }()"),                        "11");
        assert_eq!(eval("!x = 13; { .x = 12 }(); { x }() "),            "13");
        assert_eq!(eval("!:ref x = 13; { .x = 12 }(); $[{ x }(), x]"),  "[12,12]");
        assert_eq!(eval("!:ref x = 13; { .x = 12 }(); $[{ x }(), { .x = 15 }(), x]"), "[12,15,15]");
        assert_eq!(eval("{ _ } 10"),                        "10");
        assert_eq!(eval("!:ref y = 0; { .y = _ } 10; y"),   "10");
        assert_eq!(eval("${:a: 10, :b: 20}"),               "{a:10,b:20}");
        assert_eq!(eval("${:b: 20, :a: 10}"),               "{a:10,b:20}");
        assert_eq!(eval("!x = ${:b: 20, :a: 10}; x"),       "{a:10,b:20}");
        assert_eq!(eval("!x = ${:b: 20, :a: 10}; x.a"),     "10");
        assert_eq!(eval("!x = ${:b: 20, :a: 11}; :a x"),    "11");
        assert_eq!(eval("!x = ${}; x.a = 12; x.a"),         "12");
        assert_eq!(eval("!x = ${}; x.a = 12; x"),           "{a:12}");
    }

    #[test]
    fn check_ref_closures() {
        assert_eq!(eval("!c1 = { !:ref a = 1.2; { a } }; c1()()"),  "1.2");
        assert_eq!(eval("!c1 = { !:wref a = 1.2; { a } }; c1()()"), "$n");
        assert_eq!(eval("!c1 = { !:wref a = 1.2; { a }() }; c1()"), "$n");
        assert_eq!(eval("!:wref outer_a = 2.3; !c1 = { !:ref a = 1.2; { a + outer_a } }; c1()()"), "3.5");
        assert_eq!(eval("!:wref outer_a = 2.3; !c1 = { !:wref a = 1.2; { outer_a + a } }; c1()()"), "2.3");
    }

    #[test]
    fn check_arithmetics() {
        assert_eq!(eval("12 + 23"),         "35");
        assert_eq!(eval("+(12, 23)"),       "35");
        assert_eq!(eval("+ 12 23"),         "35");
        assert_eq!(eval("+ 12 ~ - 24 23"),  "13");
        assert_eq!(eval("[+ 12 ~ - 24 23] + 1"),    "14");
        assert_eq!(eval("[12 + 1] == 13"),          "$true");
        assert_eq!(eval("[+ 12 ~ - 24 23] == 13"),  "$true");
        assert_eq!(eval("[+ 12 ~ - 24 23] == 14"),  "$false");
        assert_eq!(eval("12.12 + 23.23"),           "35.35");

        // coertion of strings and keys to numbers:
        assert_eq!(eval(":10 + :20"),       "30");
        assert_eq!(eval(":-10 + :20"),      "10");

        assert_eq!(eval("12 - 23"),         "-11");
        assert_eq!(eval("5 * 4"),           "20");
        assert_eq!(eval("20 / 5"),          "4");

        assert_eq!(eval("6 - 3 * 2"),       "0");
        assert_eq!(eval("12 / 6 - 3 * 2"),  "-4");
    }

    #[test]
    fn check_compile_env() {
        let ce = CompileEnv::create_env(None);

        assert_eq!(ce.borrow_mut().def("x"), 0);
        assert_eq!(ce.borrow_mut().def("y"), 1);
        assert_eq!(ce.borrow_mut().def("z"), 2);

        let ce2 = CompileEnv::create_env(Some(ce.clone()));
        assert_eq!(ce2.borrow_mut().def("a"), 0);
        assert_eq!(ce2.borrow_mut().get("y"), VarPos::UpValue(0));
        assert_eq!(ce2.borrow_mut().get("z"), VarPos::UpValue(1));
        assert_eq!(ce2.borrow_mut().get("a"), VarPos::Local(0));

        let ce3 = CompileEnv::create_env(Some(ce2.clone()));
        assert_eq!(ce3.borrow_mut().get("a"), VarPos::UpValue(0));
        assert_eq!(ce3.borrow_mut().get("z"), VarPos::UpValue(1));

        assert_eq!(ce2.borrow_mut().get("x"), VarPos::UpValue(2));
    }

    #[test]
    fn check_bool() {
        assert_eq!(eval("!:ref a = 0; $t { .a = 1 } { .a = 2 }; a"), "1");
        assert_eq!(eval("!:ref a = 0; $f { .a = 1 } { .a = 2 }; a"), "2");
    }

    #[test]
    fn check_range() {
        assert_eq!(eval("!:ref x = 10; { .x = x + _ } 5"),                    "15");
        assert_eq!(eval("!:ref x = 10; { .x = { x + 11 + _ }(2) + _ } 5"),    "28");
        assert_eq!(eval("!:ref x = 10;   range 1 3 1     { .x = x + _ }; x"), "16");
        assert_eq!(eval("!:ref x = 10.0; range 1.0 3 0.5 { .x = x + _ }; x"), "20");
    }

    #[test]
    fn check_push() {
        assert_eq!(eval("!a = 10; !x = $[1]; !y = 20; x"), "[1]");
        assert_eq!(eval("!:ref x = $[]; push x 12; x"), "[12]");
        assert_eq!(eval("!a = 10; !x = $[]; !y = 20; push x 10; push x 30; x"), "[10,30]");
        assert_eq!(eval("!:ref x = $[]; push x 10; push x 20; x"), "[10,20]");
    }

    #[test]
    fn check_range_break() {
        assert_eq!(eval("4 == 4"), "$true");
        assert_eq!(eval("range 0 10 1 { break 14 }"), "14");
        assert_eq!(eval("range 0 10 1 { !i = _; [i == 4] { break ~ i + 10 } }"), "14");
    }

    #[test]
    fn check_range_next() {
        assert_eq!(eval("!:ref x = 0; range 0 10 1 { [_ == 4] { next() }; .x = x + _; }; x"), "51");
        assert_eq!(eval("!:ref x = 0; range 0 10 1 { next(); .x = x + _; }; x"), "0");
    }

    #[test]
    fn check_while() {
        assert_eq!(eval(r#"
            !:ref x = 0;
            while { x == 0 } {
                .x = x + 1;
            };
            x
        "#),
        "1");

        assert_eq!(eval(r#"
            !:ref x = 0;
            while { x == 0 } {
                break 10;
                .x = x + 1;
            }
        "#),
        "10");

        assert_eq!(eval(r#"
            !:ref x = 0;
            while { x == 0 } {
                next;
                .x = x + 1;
            };
            x
        "#),
        "1");
    }

    #[test]
    fn check_args() {
        assert_eq!(eval("{ $[_, _1, _2] }(1, 2, 3)"),       "[1,2,3]");
        assert_eq!(eval("{ @ }(1, 2, 3)"),                  "[1,2,3]");
        assert_eq!(eval("{ $[_, _1, _2, _3] }(1, 2, 3)"),   "[1,2,3,$n]");
    }

    #[test]
    fn check_to_drop() {
        assert_eq!(eval("!:ref x = 1; { .x = 2; }(); x"), "2");
        assert_eq!(eval("!:ref x = 1; { !d = { .x = 2; }; d }()(); x"), "2");
        assert_eq!(eval(r#"
            !:ref x = 0;
            { !d = to_drop 10 { .x = 17; } }();
            x
        "#),
        "17");
    }

    #[test]
    fn check_ops() {
        assert_eq!(eval("10 < 20"),     "$true");
        assert_eq!(eval("11 < 10"),     "$false");
        assert_eq!(eval("10 < 10"),     "$false");
        assert_eq!(eval("10 > 20"),     "$false");
        assert_eq!(eval("11 > 10"),     "$true");
        assert_eq!(eval("10 > 10"),     "$false");
        assert_eq!(eval("10 <= 20"),    "$true");
        assert_eq!(eval("11 <= 10"),    "$false");
        assert_eq!(eval("10 <= 10"),    "$true");
        assert_eq!(eval("10 >= 20"),    "$false");
        assert_eq!(eval("11 >= 10"),    "$true");
        assert_eq!(eval("10 >= 10"),    "$true");
        assert_eq!(eval("10.1 < 20.4"), "$true");
        assert_eq!(eval("11.2 < 10.2"), "$false");
        assert_eq!(eval("10.3 < 10.4"), "$true");
        assert_eq!(eval("22 == 22"),    "$true");
        assert_eq!(eval("22 == 23"),    "$false");
        assert_eq!(eval("22 != 22"),    "$false");
        assert_eq!(eval("21 != 22"),    "$true");

        assert_eq!(eval("2 ^ 2"),       "4");
        assert_eq!(eval("2 ^ 3"),       "8");
        assert_eq!(eval("2.1 ^ 2"),     "4.41");
        assert_eq!(eval("4 ^ 0.5"),     "1");
        assert_eq!(eval("4.0 ^ 0.5"),   "2");

        assert_eq!(eval("4 % 5"),       "4");
        assert_eq!(eval("6 % 5"),       "1");
        assert_eq!(eval("4.4 % 5.5"),   "4.4");
        assert_eq!(eval("5.5 % 5.5"),   "0");

        assert_eq!(eval("neg 0xFF"),    "-256");
        assert_eq!(eval("uneg 0xFF"),   "4294967040");
        assert_eq!(eval("uneg 0x1"),    "4294967294");
        assert_eq!(eval("uneg 0x0"),    "4294967295");

        assert_eq!(eval("[0x10 &| 0x01] == 0x11"), "$true");
        assert_eq!(eval("[0x0f &  0x33] == 0x3"),  "$true");
        assert_eq!(eval("[0x11 &^ 0x01] == 0x10"), "$true");
        assert_eq!(eval("[0b1 << 1] == 0b10"),     "$true");
        assert_eq!(eval("[0b1 << 2] == 0b100"),    "$true");
        assert_eq!(eval("[0b1 >> 1] == 0x0"),      "$true");
    }

}
