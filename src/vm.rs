use crate::parser::{self};
use crate::compiler::*;
use crate::vval::*;

use std::rc::Rc;
use std::cell::RefCell;

struct Prog {
    results: i64,
    debug:  std::vec::Vec<Option<SynPos>>,
    clos:   std::vec::Vec<EvalNode>,
    data:   std::vec::Vec<VVal>,
    ops:    std::vec::Vec<Op>,
    nxt_debug: Option<SynPos>,
}

impl Prog {
    fn append(&mut self, mut prog: Prog) {
        let mut self_data_next_idx : u32 = self.data.len() as u32;

        for o in prog.ops.iter_mut() {
            match o {
                Op::Push(ref mut data_idx) => {
                    *data_idx = *data_idx + self_data_next_idx;
                },
                Op::PushRef(ref mut data_idx) => {
                    *data_idx = *data_idx + self_data_next_idx;
                },
                Op::SetGlobal(ref mut data_idx) => {
                    *data_idx = *data_idx + self_data_next_idx;
                },
                Op::GetGlobal(ref mut data_idx) => {
                    *data_idx = *data_idx + self_data_next_idx;
                },
                Op::GetGlobalRef(ref mut data_idx) => {
                    *data_idx = *data_idx + self_data_next_idx;
                },
                Op::NewPair
                | Op::Ret(_)
                | Op::Call(_)
                | Op::Pop
                | Op::Add
                | Op::Sub
                | Op::Div
                | Op::Mul
                | Op::Mod
                | Op::Lt
                | Op::Le
                | Op::Gt
                | Op::Ge
                | Op::Eq
                | Op::ResvLocals(_)
                | Op::SetLocal(_)
                | Op::GetLocal(_)
                | Op::GetLocalRef(_)
                | Op::GetUp(_)
                | Op::GetUpRef(_)
                | Op::Arg(_)
                | Op::ArgRef(_)
                | Op::Argv
                | Op::ArgvRef
                => (),
            }
        }

        self.results += prog.results;
        self.debug.append(&mut prog.debug);
        self.clos.append(&mut prog.clos);
        self.data.append(&mut prog.data);
        self.ops.append(&mut prog.ops);
    }

    fn new() -> Self {
        Self {
            clos:       vec![],
            data:       vec![],
            ops:        vec![],
            debug:      vec![],
            nxt_debug:  None,
            results:    0,
        }
    }

    fn consume(mut self, n: usize) -> Self {
        self.results -= n as i64;
        self
    }

    fn result(mut self) -> Self {
        self.results += 1;
        self
    }

    fn pop_result(&mut self) {
        if self.results < 0 {
            panic!("Can't pop from negative results!");
        }
        for i in 0..self.results {
            self.push_op(Op::Pop);
        }
        self.results = 0;
    }

    fn push_data(&mut self, v: VVal) -> usize {
        self.data.push(v);
        self.data.len() - 1
    }

    fn unshift_op(&mut self, o: Op) -> &mut Self {
        self.ops.insert(0, o);
        self.debug.insert(0, std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    fn push_op(&mut self, o: Op) -> &mut Self {
        self.ops.push(o);
        self.debug.push(std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    fn data_op(mut self, o: Op, v: VVal) -> Self {
        let idx = self.push_data(v);
        self.ops.push(
            match o {
                Op::SetGlobal(_) => Op::SetGlobal(idx as u32),
                Op::Push(_)      => Op::Push(idx as u32),
                Op::PushRef(_)      => Op::PushRef(idx as u32),
                Op::GetGlobalRef(_) => Op::GetGlobalRef(idx as u32),
                Op::GetGlobal(_) => Op::GetGlobal(idx as u32),
                Op::NewPair
                | Op::Ret(_)
                | Op::Call(_)
                | Op::Pop
                | Op::Add
                | Op::Sub
                | Op::Div
                | Op::Mul
                | Op::Mod
                | Op::Lt
                | Op::Le
                | Op::Gt
                | Op::Ge
                | Op::Eq
                | Op::ResvLocals(_)
                | Op::SetLocal(_)
                | Op::GetLocal(_)
                | Op::GetLocalRef(_)
                | Op::GetUp(_)
                | Op::GetUpRef(_)
                | Op::Arg(_)
                | Op::ArgRef(_)
                | Op::Argv
                | Op::ArgvRef
                => panic!("data_op for non data Op called!"),
            });
        self.debug.push(std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    fn op(mut self, o: Op) -> Self {
        self.ops.push(o);
        self.debug.push(std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    fn debug(mut self, sp: SynPos) -> Self {
        self.nxt_debug = Some(sp);
        self
    }
}

#[derive(Debug,Copy,Clone)]
enum Op {
    Call(u32),
    Push(u32),
    PushRef(u32),
    ResvLocals(u32),
    SetGlobal(u32),
    GetGlobal(u32),
    GetGlobalRef(u32),
    SetLocal(u32),
    GetLocal(u32),
    GetLocalRef(u32),
    GetUp(u32),
    GetUpRef(u32),
    Arg(u32),
    ArgRef(u32),
    Argv,
    ArgvRef,
    NewPair,
    Ret(u32),
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
}

fn vm(prog: &Prog, env: &mut Env) -> Result<VVal, StackAction> {
    let old_sp = env.sp;
    let mut pc : usize = 0;
    println!("-- START -------------------------------------");
    let mut ret = VVal::Nul;
    loop {
        let op = prog.ops[pc];
        let syn =
            if let Some(sp) = &prog.debug[pc] { format!("{}", sp) }
            else { "".to_string() };
        println!("OP: {:<15}      | sp: {:>3}, bp: {:>3} | {}", format!("{:?}", op), env.sp, env.bp, syn);
        match op {
            Op::Push(data_idx) => {
                env.push(prog.data[data_idx as usize].clone());
            },
            Op::PushRef(data_idx) => {
                env.push(prog.data[data_idx as usize].to_ref());
            },
            Op::NewPair => {
                let b = env.pop();
                let a = env.pop();
                env.push(VVal::Pair(Box::new((a, b))));
            },
            Op::ResvLocals(count)      => env.push_sp(count as usize),
            Op::Pop                    => { env.pop(); },
            Op::Ret(count)             => {
                let value = env.pop();
                env.popn(count as usize);
                ret = value;
                break;
            },
            Op::SetLocal(idx)          => {
                let v = env.pop();
                env.set_consume(idx as usize, v);
            },
            Op::SetGlobal(data_idx)    => { prog.data[data_idx as usize].set_ref(env.pop()); },
            Op::GetGlobal(data_idx)    => { env.push(prog.data[data_idx as usize].deref()); },
            Op::GetGlobalRef(data_idx) => { env.push(prog.data[data_idx as usize].clone()); },
            Op::GetUp(idx)             => { env.push(env.get_up(idx as usize)); },
            Op::GetUpRef(idx)          => { env.push(env.get_up_captured_ref(idx as usize)); },
            Op::GetLocal(idx)          => { env.push(env.get_local(idx as usize)); },
            Op::GetLocalRef(idx)       => { env.push(env.get_local_captured_ref(idx as usize)); },
            Op::Arg(arg_idx)           => { env.push(env.arg(arg_idx as usize)); },
            Op::ArgRef(arg_idx)        => { env.push(env.arg(arg_idx as usize).to_ref()); },
            Op::Argv                   => { env.push(env.argv()); },
            Op::ArgvRef                => { env.push(env.argv().to_ref()); },
            Op::Add => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    match (a, b) {
                        (VVal::IVec(ln), VVal::IVec(rn)) => VVal::IVec(ln + rn),
                        (VVal::FVec(ln), VVal::FVec(rn)) => VVal::FVec(ln + rn),
                        (VVal::Flt(f), re)               => VVal::Flt(f + re.f()),
                        (le, re)                         => VVal::Int(le.i().wrapping_add(re.i()))
                    };
                env.push(res);
            },
            Op::Sub => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a { VVal::Flt(f - b.f()) }
                    else { VVal::Int(a.i().wrapping_sub(b.i())) };
                env.push(res);
            },
            Op::Div => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a {
                        VVal::Flt(f / b.f())

                    } else if b.i() == 0 {
                        return
                            Err(StackAction::panic_str(
                                format!("Division by 0: {}/{}", a.i(), b.i()),
                                prog.debug[pc].clone()))

                    } else {
                        VVal::Int(a.i().wrapping_div(b.i()))
                    };
                env.push(res);
            },
            Op::Mul => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a { VVal::Flt(f * b.f()) }
                    else { VVal::Int(a.i().wrapping_mul(b.i())) };
                env.push(res);
            },
            Op::Mod => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a {
                        VVal::Flt(f % b.f())
                    } else {
                        VVal::Int(a.i().wrapping_rem(b.i()))
                    };
                env.push(res);
            },
            Op::Le => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a { VVal::Bol(f <= b.f()) }
                    else { VVal::Bol(a.i() <= b.i()) };
                env.push(res);
            },
            Op::Lt => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a { VVal::Bol(f < b.f()) }
                    else { VVal::Bol(a.i() < b.i()) };
                env.push(res);
            },
            Op::Ge => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a { VVal::Bol(f >= b.f()) }
                    else { VVal::Bol(a.i() >= b.i()) };
                env.push(res);
            },
            Op::Gt => {
                let b = env.pop();
                let a = env.pop();
                let res =
                    if let VVal::Flt(f) = a { VVal::Bol(f > b.f()) }
                    else { VVal::Bol(a.i() > b.i()) };
                env.push(res);
            },
            Op::Eq => {
                let b = env.pop();
                let a = env.pop();
                let res = VVal::Bol(a.eqv(&b));
                env.push(res);
            },
            Op::Call(argc) => {
                let argc = argc as usize;
                let f = env.pop();
                let ret = f.call_internal(env, argc);
                env.popn(argc);
                match ret {
                    Ok(v) => { env.push(v); },
                    Err(sa) =>
                        return Err(sa.wrap_panic(prog.debug[pc].clone())),
                }
            },
        }
        env.dump_stack();
        pc += 1;
    }

    if env.sp != old_sp {
        env.dump_stack();
        panic!("leaked or consumed stack space unevenly!");
    }

    Ok(ret)
}

fn vm_compile_def(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_global: bool) -> Result<Prog, CompileError> {
    let prev_max_arity = ce.borrow().implicit_arity.clone();

    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let destr   = ast.at(3).unwrap_or(VVal::Nul);

    //d// println!("COMP DEF: {:?} global={}, destr={}", vars, is_global, destr.b());

    if destr.b() {
        return Err(ast.compile_err("UNIMPL: DESTR!".to_string()));
//        let cv = compile(&value, ce)?;
//
//        check_for_at_arity(prev_max_arity, ast, ce, &vars);
//
//        let poses =
//            vars.map_ok_skip(
//                |v| ce.borrow_mut().def(&v.s_raw(), is_global),
//                0);
//
//        Ok(Box::new(move |e: &mut Env| {
//            let v = cv(e)?;
//            match v {
//                VVal::Lst(l) => {
//                    for (i, vi) in poses.iter().enumerate() {
//                        if l.borrow().len() <= i {
//                            if let VarPos::Local(vip) = vi {
//                                e.set_consume(*vip, VVal::Nul);
//                            }
//                        } else {
//                            let val = &mut l.borrow_mut()[i];
//
//                            let set_val = val.clone();
//                            match vi {
//                                VarPos::Local(vip) => {
//                                    e.set_consume(*vip, set_val);
//                                },
//                                VarPos::Global(r) => {
//                                    if let VVal::Ref(gr) = r {
//                                        gr.replace(set_val);
//                                    }
//                                },
//                                _ => {}
//                            }
//                        }
//                    }
//                },
//                VVal::Map(m) => {
//                    for (i, vi) in poses.iter().enumerate() {
//                        let vname = vars.at(i).unwrap().s_raw();
//                        let val = m.borrow().get(&vname).cloned().unwrap_or(VVal::Nul);
//
//                        match vi {
//                            VarPos::Local(vip) => e.set_consume(*vip, val),
//                            VarPos::Global(r) => {
//                                if let VVal::Ref(gr) = r {
//                                    gr.replace(val);
//                                }
//                            },
//                            _ => {}
//                        }
//                    }
//                },
//                _ => {
//                    for vi in poses.iter() {
//                        match vi {
//                            VarPos::Local(vip) => e.set_consume(*vip, v.clone()),
//                            VarPos::Global(r) => {
//                                if let VVal::Ref(gr) = r {
//                                    gr.replace(v.clone());
//                                }
//                            },
//                            _ => {},
//                        }
//                    }
//                }
//            }
//
//            Ok(VVal::Nul)
//        }))
    } else {
        let varname = vars.at(0).unwrap().s_raw();
        ce.borrow_mut().recent_var = varname.clone();
        let mut val_prog = vm_compile(&value, ce)?;

        let pos = ce.borrow_mut().def(&varname, is_global);

        match pos {
            VarPos::Local(vip) => {
                val_prog.append(Prog::new().op(Op::SetLocal(vip as u32)).consume(1));
                Ok(val_prog)
            },
            VarPos::Global(r) => {
                val_prog.append(Prog::new().data_op(Op::SetGlobal(0), r).consume(1));
                Ok(val_prog)
            },
            _ => Err(ast.compile_err(
                    "Can't define badly positioned variable!".to_string())),
        }
    }
}

fn vm_compile_var(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, to_ref: bool) -> Result<Prog, CompileError> {
    let var = ast.at(1).unwrap();
    var.with_s_ref(|var_s: &str| -> Result<Prog, CompileError> {
        if to_ref {
            match var_s {
                "_"  => { set_impl_arity(1,  ce); Ok(Prog::new().op(Op::ArgRef(0)).result()) },
                "_1" => { set_impl_arity(2,  ce); Ok(Prog::new().op(Op::ArgRef(1)).result()) },
                "_2" => { set_impl_arity(3,  ce); Ok(Prog::new().op(Op::ArgRef(2)).result()) },
                "_3" => { set_impl_arity(4,  ce); Ok(Prog::new().op(Op::ArgRef(3)).result()) },
                "_4" => { set_impl_arity(5,  ce); Ok(Prog::new().op(Op::ArgRef(4)).result()) },
                "_5" => { set_impl_arity(6,  ce); Ok(Prog::new().op(Op::ArgRef(5)).result()) },
                "_6" => { set_impl_arity(7,  ce); Ok(Prog::new().op(Op::ArgRef(6)).result()) },
                "_7" => { set_impl_arity(8,  ce); Ok(Prog::new().op(Op::ArgRef(7)).result()) },
                "_8" => { set_impl_arity(9,  ce); Ok(Prog::new().op(Op::ArgRef(8)).result()) },
                "_9" => { set_impl_arity(10, ce); Ok(Prog::new().op(Op::ArgRef(9)).result()) },
                "@"  => {
                    ce.borrow_mut().implicit_arity.1 = ArityParam::Infinite;
                    Ok(Prog::new().op(Op::ArgvRef).result())
                },
                _ => {
                    let pos = ce.borrow_mut().get(var_s);
                    match pos {
                        VarPos::UpValue(i) =>
                            Ok(Prog::new().op(Op::GetUpRef(i as u32)).result()),
                        VarPos::Local(i) =>
                            Ok(Prog::new().op(Op::GetLocalRef(i as u32)).result()),
                        VarPos::Global(v) =>
                            Ok(Prog::new().data_op(Op::GetGlobalRef(0), v.clone()).result()),
                        VarPos::Const(v) =>
                            Ok(Prog::new().data_op(Op::PushRef(0), v.clone()).result()),
                        VarPos::NoPos => {
                            Err(ast.compile_err(
                                format!("Variable '{}' undefined", var_s)))
                        }
                    }
                }
            }
        } else {
            match var_s {
                "_"  => { set_impl_arity(1,  ce); Ok(Prog::new().op(Op::Arg(0)).result()) },
                "_1" => { set_impl_arity(2,  ce); Ok(Prog::new().op(Op::Arg(1)).result()) },
                "_2" => { set_impl_arity(3,  ce); Ok(Prog::new().op(Op::Arg(2)).result()) },
                "_3" => { set_impl_arity(4,  ce); Ok(Prog::new().op(Op::Arg(3)).result()) },
                "_4" => { set_impl_arity(5,  ce); Ok(Prog::new().op(Op::Arg(4)).result()) },
                "_5" => { set_impl_arity(6,  ce); Ok(Prog::new().op(Op::Arg(5)).result()) },
                "_6" => { set_impl_arity(7,  ce); Ok(Prog::new().op(Op::Arg(6)).result()) },
                "_7" => { set_impl_arity(8,  ce); Ok(Prog::new().op(Op::Arg(7)).result()) },
                "_8" => { set_impl_arity(9,  ce); Ok(Prog::new().op(Op::Arg(8)).result()) },
                "_9" => { set_impl_arity(10, ce); Ok(Prog::new().op(Op::Arg(9)).result()) },
                "@"  => {
                    ce.borrow_mut().implicit_arity.1 = ArityParam::Infinite;
                    Ok(Prog::new().op(Op::Argv).result())
                },
                _ => {
                    let pos = ce.borrow_mut().get(var_s);
                    match pos {
                        VarPos::UpValue(i) =>
                            Ok(Prog::new().op(Op::GetUp(i as u32)).result()),
                        VarPos::Local(i) =>
                            Ok(Prog::new().op(Op::GetLocal(i as u32)).result()),
                        VarPos::Global(v) =>
                            Ok(Prog::new().data_op(Op::GetGlobal(0), v.clone()).result()),
                        VarPos::Const(v) =>
                            Ok(Prog::new().data_op(Op::Push(0), v.clone()).result()),
                        VarPos::NoPos => {
                            Err(ast.compile_err(
                                format!("Variable '{}' undefined", var_s)))
                        }
                    }
                }
            }
        }
    })
}


fn vm_compile(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<Prog, CompileError> {
    match ast {
        VVal::Lst(l) => {
            let syn  = ast.at(0).unwrap_or(VVal::Nul);
            let spos = syn.get_syn_pos();
            let syn  = syn.get_syn();

            match syn {
                Syntax::Block => {
                    ce.borrow_mut().push_block_env();
                    let exprs : Vec<Prog> = ast.map_skip(|e| vm_compile(e, ce), 1)?;

                    let mut p = Prog::new().debug(spos);
                    for e in exprs.into_iter() {
                        p.pop_result();
                        p.append(e);
                    }
                    let block_env_var_count = ce.borrow_mut().pop_block_env();
                    p.unshift_op(Op::ResvLocals(block_env_var_count as u32));
                    p.push_op(Op::Ret(block_env_var_count as u32));
                    Ok(p.result())
                },
//                Syntax::Assign      => { compile_assign(ast, ce, false) },
//                Syntax::AssignRef   => { compile_assign(ast, ce, true)  },
                Syntax::Var        => vm_compile_var(ast, ce, false).map(|p| p.debug(spos)),
                Syntax::CaptureRef => vm_compile_var(ast, ce, true).map(|p| p.debug(spos)),
                Syntax::Def        => vm_compile_def(ast, ce, false).map(|p| p.debug(spos)),
                Syntax::DefGlobRef => vm_compile_def(ast, ce, true).map(|p| p.debug(spos)),
                Syntax::BinOpAdd => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Add).consume(2).result())
                },
                Syntax::BinOpSub => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Sub).consume(2).result())
                },
                Syntax::BinOpDiv => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Div).consume(2).result())
                },
                Syntax::BinOpMul => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Mul).consume(2).result())
                },
                Syntax::BinOpMod => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Mod).consume(2).result())
                },
                Syntax::BinOpGe => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Ge).consume(2).result())
                },
                Syntax::BinOpGt => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Gt).consume(2).result())
                },
                Syntax::BinOpLe => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Le).consume(2).result())
                },
                Syntax::BinOpLt => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Lt).consume(2).result())
                },
                Syntax::BinOpEq => {
                    let left  = vm_compile(&ast.at(1).unwrap(), ce)?;
                    let right = vm_compile(&ast.at(2).unwrap(), ce)?;
                    let mut prog = Prog::new().debug(spos);
                    prog.append(left);
                    prog.append(right);
                    Ok(prog.op(Op::Eq).consume(2).result())
                },
                Syntax::Call => {
                    let mut call_args : Vec<Prog> =
                        ast.map_skip(|e: &VVal| vm_compile(e, ce), 1)?;
                    call_args.reverse();

                    let argc = call_args.len();
                    let mut prog = Prog::new().debug(spos);
                    for a in call_args.into_iter() {
                        prog.append(a);
                    }
                    Ok(prog.op(Op::Call(argc as u32 - 1)).consume(argc).result())
                },
                _ => { Err(ast.compile_err(format!("bad input: {}", ast.s()))) },
            }
        },
        VVal::Pair(bx) => {
            let a = vm_compile(&bx.0, ce)?;
            let b = vm_compile(&bx.1, ce)?;
            let mut p = Prog::new();
            p.append(a);
            p.append(b);
            Ok(p.op(Op::NewPair).consume(2).result())
        },
        _ => Ok(Prog::new().data_op(Op::Push(0), ast.clone()).result()),
    }
}

fn gen(s: &str) -> String {
    let global = GlobalEnv::new_default();
    match parser::parse(s, "<compiler:s_eval>") {
        Ok(ast) => {
            let mut ce = CompileEnv::new(global.clone());
            match vm_compile(&ast, &mut ce) {
                Ok(mut prog) => {
                    let mut e = Env::new(global);
                    e.push(VVal::Int(10));
                    e.push(VVal::Flt(14.4));
                    e.argc = 2;
                    e.set_bp(0);
                    match vm(&prog, &mut e) {
                        Ok(v) => v.s(),
                        Err(je) => {
                            format!("EXEC ERR: Caught {:?}", je)
                        }
                    }
                },
                Err(re) => format!("COMPILE ERROR: {}", re),
            }
        }
        Err(e)  => format!("PARSE ERROR: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn vm_pair() {
        assert_eq!(gen("$p(1,2)"),          "$p(1,2)");
        assert_eq!(gen("$p(1,$p(2,3))"),    "$p(1,$p(2,3))");
        assert_eq!(gen("$p($p(1,2),$p($p(3,4),3))"),
                   "$p($p(1,2),$p($p(3,4),3))");
    }

    #[test]
    fn vm_vars() {
        assert_eq!(gen("!:global x = 10; x"),                         "10");
        assert_eq!(gen("!:global x = 10; !:global y = 11; $p(x, y)"), "$p(10,11)");
        assert_eq!(gen("!x = 10; !y = 11; $p(x, y)"),                 "$p(10,11)");
        assert_eq!(gen("!x = 10; !y = 11; x; y; $p(x, y)"),           "$p(10,11)");
        assert_eq!(gen("!x = 10; !y = 11; $:x"),                      "$&&10");
        assert_eq!(gen("!:global x = 10; !y = 11; $:x"),              "$&&10");
        assert_eq!(gen("!:global x = 10; !y = 11; _"),                "14.4");
        assert_eq!(gen("!:global x = 10; !y = 11; $:_"),              "$&&14.4");
        assert_eq!(gen("!:global x = 10; !y = 11; $p(_, $p(_1, $p(_2, y)))"),
                   "$p(14.4,$p(10,$p($n,11)))");
    }

    #[test]
    fn vm_call() {
        assert_eq!(gen("1 + 2"),         "3");
        assert_eq!(gen("1 + 2 + 3 + 4"), "10");
        assert_eq!(gen("1 > 2"),         "$false");
        assert_eq!(gen("2 > 1"),         "$true");
        assert_eq!(gen("1 < 2"),         "$true");
        assert_eq!(gen("2 < 1"),         "$false");
        assert_eq!(gen("1 >= 2"),        "$false");
        assert_eq!(gen("2 >= 1"),        "$true");
        assert_eq!(gen("1 <= 2"),        "$true");
        assert_eq!(gen("2 <= 1"),        "$false");

        assert_eq!(gen("10 < 20"),     "$true");
        assert_eq!(gen("11 < 10"),     "$false");
        assert_eq!(gen("10 < 10"),     "$false");
        assert_eq!(gen("10 > 20"),     "$false");
        assert_eq!(gen("11 > 10"),     "$true");
        assert_eq!(gen("10 > 10"),     "$false");
        assert_eq!(gen("10 <= 20"),    "$true");
        assert_eq!(gen("11 <= 10"),    "$false");
        assert_eq!(gen("10 <= 10"),    "$true");
        assert_eq!(gen("10 >= 20"),    "$false");
        assert_eq!(gen("11 >= 10"),    "$true");
        assert_eq!(gen("10 >= 10"),    "$true");
        assert_eq!(gen("10.1 < 20.4"), "$true");
        assert_eq!(gen("11.2 < 10.2"), "$false");
        assert_eq!(gen("10.3 < 10.4"), "$true");
        assert_eq!(gen("22 == 22"),    "$true");
        assert_eq!(gen("22 == 23"),    "$false");
        assert_eq!(gen("22 != 22"),    "$false");
        assert_eq!(gen("21 != 22"),    "$true");
    }
}
