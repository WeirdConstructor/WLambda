// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::parser::*; // ParseState;
use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::Env;
use crate::vval::VValFun;
use crate::vval::ClosNode;
use std::rc::Rc;
use std::cell::RefCell;

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
struct CompileLocal {
    is_upvalue: bool,
    is_inherit: bool,
    index:      usize,
    name:       String,
}

#[derive(Debug, Clone)]
struct GlobalEnv {
    env: std::collections::HashMap<String, VVal>,
}

impl GlobalEnv {
    // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=7011ec14ebade14fe62e438a0db52c98
    fn add_func<T>(&mut self, fnname: &str, fun: T)
        where T: Fn(&std::vec::Vec<(usize, VVal)>, std::vec::Vec<VVal>) -> Result<VVal,u32> {
        self.env.insert(
            String::from(fnname),
            VValFun::new(Rc::new(RefCell::new(fun)),
                         Vec::new()));
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
struct CompileEnv {
    parent:    Option<Rc<RefCell<CompileEnv>>>,
    global:    Rc<RefCell<GlobalEnv>>,
    local_map: std::collections::HashMap<String, usize>,
    locals:    std::vec::Vec<CompileLocal>,
    upvals:    std::vec::Vec<(usize,usize)>,
}

type CompileEnvRef = Rc<RefCell<CompileEnv>>;

impl CompileEnv {

    fn create_env(parent: Option<CompileEnvRef>) -> Rc<RefCell<CompileEnv>> {
        let global = if let Some(p) = &parent {
            p.borrow_mut().global.clone()
        } else {
            Rc::new(RefCell::new(GlobalEnv { env: std::collections::HashMap::new() }))
        };
        Rc::new(RefCell::new(CompileEnv {
            parent:    parent,
            global:    global,
            local_map: std::collections::HashMap::new(),
            locals:    Vec::new(),
            upvals:    Vec::new(),
        }))
    }

    fn def_up(&mut self, s: &str, is_up: bool, is_inherit: bool) -> usize {
        let next_index = self.locals.len();
        self.locals.push(CompileLocal {
            is_upvalue: is_up,
            is_inherit: is_inherit,
            index:      next_index,
            name:       String::from(s),
        });
        self.local_map.insert(String::from(s), next_index);
        println!("CE DEF '{}' => {}", s, next_index);
        next_index
    }

    fn copy_upvals(&self, e: &mut Env, upvalues: &mut std::vec::Vec<(usize,VVal)>) {
        for u in self.upvals.iter() {
            match e.locals[u.0].downgrade() {
                Some(v) => { upvalues.push((u.1, v));             },
                None    => { upvalues.push((u.1, e.locals[u.0].clone())); },
            }
            println!("COPY UPV: {}, {}, {} => {}", u.0, u.1, e.locals[u.0].s(), upvalues[upvalues.len() - 1].1.s());
        }
    }

    fn local_env_size(ce: &CompileEnvRef) -> usize {
        ce.borrow().locals.len()
    }

    fn def(&mut self, s: &str) -> usize {
        self.def_up(s, false, false)
    }

    fn mark_upvalue(&mut self, idx: usize) {
        self.locals[idx].is_upvalue = true;
    }

    fn get(&mut self, s: &str) -> Option<usize> {
        println!("CE GET '{}'", s);
        let opt_idx = self.local_map.get(s);

        if let Some(&i) = opt_idx {
            return Some(i);
        }

        // We now handle the case, where we access a variable
        // from the parent scope. The invariant later must be,
        // that all local variables are present in the current
        // `local_map` and `locals`.
        // Calling a function will create the corresponding
        // local environments by copying the values from the
        // environment above.
        let parent = self.parent.as_mut()?.clone();
        let mut par_mut = parent.borrow_mut();

        let opt_i = par_mut.local_map.get(s).cloned();
        match opt_i {
            Some(i) => {
                par_mut.mark_upvalue(i);
                let my_i = self.def_up(s, true, true);
                println!("NEW UPVALUE {} => {}", i, my_i);
                self.upvals.push((i, my_i));
                Some(my_i)
            },
            None => { par_mut.get(s) }
        }
    }
}

fn compile_block(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ClosNode, String> {
    let exprs : Vec<ClosNode> = ast.map_skip(|e| compile(e, ce).unwrap(), 1);

    Ok(Box::new(move |e: &mut Env| {
        let mut res = VVal::Nul;
        for x in exprs.iter() {
            res = x(e)?;
        }
        Ok(res)
    }))
}

fn compile_var(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ClosNode, String> {
    let var = ast.at(1).unwrap();

    let s = var.s_raw();
    match &s[..] {
        "_"  => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(0)) })) },
        "_1" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(1)) })) },
        "_2" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(2)) })) },
        "_3" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(3)) })) },
        "_4" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(4)) })) },
        "_5" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(5)) })) },
        "_6" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(6)) })) },
        "_7" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(7)) })) },
        "_8" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(8)) })) },
        "_9" => { Ok(Box::new(move |e: &mut Env| { Ok(e.arg(9)) })) },
        "@"  => { Ok(Box::new(move |e: &mut Env| { Ok(e.argv()) })) },
        _ => {
            let maybe_idx = ce.borrow_mut().get(&s);
            match maybe_idx {
                Some(idx) =>
                    Ok(Box::new(move |e: &mut Env| { Ok(e.get(idx)) })),
                None => {
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

fn compile_def(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_ref: bool) -> Result<ClosNode, String> {
    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let cv      = compile(&value, ce)?;
    let idx     = ce.borrow_mut().def(&vars.at(0).unwrap().s_raw());

    if is_ref {
        Ok(Box::new(move |e: &mut Env| {
            let mut v = cv(e)?;
            e.set_consume(idx, (&mut v).to_ref());
            Ok(v)
        }))
    } else {
        Ok(Box::new(move |e: &mut Env| {
            let v = cv(e)?;
            e.set(idx, &v);
            Ok(v)
        }))
    }
}

fn compile_assign(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ClosNode, String> {
    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let cv      = compile(&value, ce)?;
    let idx     = ce.borrow_mut().get(&vars.at(0).unwrap().s_raw()).unwrap();

    Ok(Box::new(move |e: &mut Env| {
        let v = cv(e)?;
        println!("ASSIGN {} = {}", idx, v.s());
        e.set(idx, &v);
        Ok(v)
    }))
}

fn compile(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ClosNode, String> {
    println!("compile {}", ast.s());
    match ast {
        VVal::Lst(_l) => {
            match ast.at(0).unwrap() {
                VVal::Syn(Syntax::Block)  => { compile_block(ast, ce)       },
                VVal::Syn(Syntax::Var)    => { compile_var(ast, ce)         },
                VVal::Syn(Syntax::Def)    => { compile_def(ast, ce, false)  },
                VVal::Syn(Syntax::DefRef) => { compile_def(ast, ce, true)   },
                VVal::Syn(Syntax::Assign) => { compile_assign(ast, ce)      },
                VVal::Syn(Syntax::Key)    => {
                    let sym = ast.at(1).unwrap();
                    Ok(Box::new(move |_: &mut Env| { Ok(sym.clone()) }))
                },
                VVal::Syn(Syntax::SetKey)    => {
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
                VVal::Syn(Syntax::Lst)    => {
                    let list_elems : Vec<ClosNode> = ast.map_skip(|e| compile(e, ce).unwrap(), 1);

                    Ok(Box::new(move |e: &mut Env| {
                        let v = VVal::vec();
                        for x in list_elems.iter() { v.push(x(e)?); }
                        Ok(v)
                    }))
                },
                VVal::Syn(Syntax::Map)    => {
                    let list_elems : Vec<(ClosNode,ClosNode)> =
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
                VVal::Syn(Syntax::Call) => {
                    let mut call_args : Vec<ClosNode> = ast.map_skip(|e| compile(e, ce).unwrap(), 1);
                    let func = call_args.remove(0);

                    Ok(Box::new(move |e: &mut Env| {
                        let f = func(e)?;
                        let mut args = Vec::new();
                        for x in call_args.iter() { args.push(x(e)?); }

                        match f {
                            VVal::Fun(fu) => {
                                (((*fu).fun.borrow()))(&fu.upvalues, args)
                            },
                            VVal::Sym(sym) => {
                                if args.len() > 0 {
                                    match args[0] {
                                        VVal::Map(_) => {
                                            let v = args[0].get_key(&sym);
                                            Ok(if v.is_some() { v.unwrap() } else { VVal::Nul })
                                        },
                                        _ => Ok(VVal::Nul)
                                    }
                                } else { Ok(VVal::Nul) }
                            },
                            _ => { Ok(VVal::Nul) },
                        }
                    }))
                },
                VVal::Syn(Syntax::Func) => {
                    let mut ce_sub = CompileEnv::create_env(Some(ce.clone()));

                    let stmts : Vec<ClosNode> = ast.map_skip(|e| compile(e, &mut ce_sub).unwrap(), 1);

                    let env_size = CompileEnv::local_env_size(&ce_sub);
                    let fun_ref = Rc::new(RefCell::new(move |upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
                        let mut e = Env::new(env_size, args);
                        for u in upv.iter() {
                            if let VVal::WRef(_) = u.1 {
                                let mut v = u.1.clone();
                                e.locals[u.0] = v.upgrade().unwrap();
                            } else {
                                e.locals[u.0] = u.1.clone();
                            }
                        }
                        let mut res = VVal::Nul;
                        for s in stmts.iter() {
                            res = s(&mut e)?;
                        }
                        Ok(res)
                    }));

                    Ok(Box::new(move |e: &mut Env| {
                        let mut v = Vec::new();
                        ce_sub.borrow_mut().copy_upvals(e, &mut v);
                        Ok(VValFun::new(fun_ref.clone(), v))
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

#[cfg(test)]
mod tests {
    use super::*;

    fn mk(s: &str) -> ParseState {
        ParseState::new(s, "<testinput>")
    }

    fn eval(s: &str) -> String {
        let args = vec![VVal::Int(13), VVal::Flt(42.42)];
        let global = Rc::new(RefCell::new(GlobalEnv {
            env: std::collections::HashMap::new()
        }));

        global.borrow_mut().add_func(
            "+",
            |_upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
                if args.len() <= 0 { return Ok(VVal::Nul); }
                if let VVal::Flt(_) = args[0] {
                    Ok(VVal::Flt(args.iter().map(|v| v.f()).sum()))
                } else {
                    Ok(VVal::Int(args.iter().map(|v| v.i()).sum()))
                }
            });

        let mut ps = mk(s);
        match parse_block(&mut ps, false) {
            Ok(v)  =>  {
                let mut ce = Rc::new(RefCell::new(CompileEnv {
                    parent:    None,
                    global:    global,
                    local_map: std::collections::HashMap::new(),
                    locals:    Vec::new(),
                    upvals:    Vec::new(),
                }));
                let prog = compile(&v, &mut ce);
                match prog {
                    Ok(r) => {
                        let mut e = Env::new(CompileEnv::local_env_size(&ce), args);
                        match r(&mut e) {
                            Ok(v) => { v.s() },
                            Err(je) => { panic!(format!("EXEC ERR: JUMPED {}", je)); }
                        }
                    },
                    Err(re) => { panic!(format!("COMPILE ERROR: {}", re)); },
                }
            }
            Err(e) => { panic!(format!("PARSE ERROR: {} at '{}' with input '{}'", e, ps.rest(), s)); },
        }
    }

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
    fn check_arithmetics() {
        assert_eq!(eval("12 + 23"),         "35");
        assert_eq!(eval("12.12 + 23.23"),   "35.35");

        // coertion of strings and keys to numbers:
        assert_eq!(eval(":10 + :20"),       "30");
        assert_eq!(eval(":-10 + :20"),      "10");

        assert_eq!(eval("12 - 23"),         "-11");
        assert_eq!(eval("5 * 4"),           "20");
        assert_eq!(eval("20 / 5"),          "4");
    }

    #[test]
    fn check_compile_env() {
        let ce = CompileEnv::create_env(None);

        assert_eq!(ce.borrow_mut().def("x"), 0);
        assert_eq!(ce.borrow_mut().def("y"), 1);
        assert_eq!(ce.borrow_mut().def("z"), 2);

        let ce2 = CompileEnv::create_env(Some(ce.clone()));
        assert_eq!(ce2.borrow_mut().def("a"), 0);
        assert_eq!(ce2.borrow_mut().get("y"), Some(1));
        assert_eq!(ce2.borrow_mut().get("z"), Some(2));

        let ce3 = CompileEnv::create_env(Some(ce2.clone()));
        assert_eq!(ce3.borrow_mut().get("a"), Some(0));
        assert_eq!(ce3.borrow_mut().get("z"), Some(1));

        assert_eq!(ce2.borrow_mut().get("x"), Some(3));
    }
}
