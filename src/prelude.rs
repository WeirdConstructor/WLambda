use crate::compiler::*;
use crate::vval::*;
use std::rc::Rc;

pub fn create_wlamba_prelude() -> GlobalEnvRef {
    let g = GlobalEnv::new();

    g.borrow_mut().add_func(
        "+",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            let args = env.slice(argc);
            if let VVal::Flt(_) = args[args.len() - 1] {
                Ok(VVal::Flt(args.iter().rev().map(|v| v.f()).sum()))
            } else {
                Ok(VVal::Int(args.iter().rev().map(|v| v.i()).sum()))
            }
        });

    g.borrow_mut().add_func(
        "-",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            let args = env.slice(argc);
            let f = &args[args.len() - 1];
            if let VVal::Flt(_) = f {
                Ok(VVal::Flt(args.iter().rev().skip(1).fold(f.f(), |a, v| a - v.f())))
            } else {
                Ok(VVal::Int(args.iter().rev().skip(1).fold(f.i(), |a, v| a - v.i())))
            }
        });

    g.borrow_mut().add_func(
        "*",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            let args = env.slice(argc);
            let f = &args[args.len() - 1];
            if let VVal::Flt(_) = f {
                Ok(VVal::Flt(args.iter().rev().skip(1).fold(f.f(), |a, v| a * v.f())))
            } else {
                Ok(VVal::Int(args.iter().rev().skip(1).fold(f.i(), |a, v| a * v.i())))
            }
        });

    g.borrow_mut().add_func(
        "/",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            let args = env.slice(argc);
            let f = &args[args.len() - 1];
            if let VVal::Flt(_) = f {
                Ok(VVal::Flt(args.iter().rev().skip(1).fold(f.f(), |a, v| a / v.f())))
            } else {
                Ok(VVal::Int(args.iter().rev().skip(1).fold(f.i(), |a, v| a / v.i())))
            }
        });

    g.borrow_mut().add_func(
        "==",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            let args = env.slice(argc);
            Ok(VVal::Bol(args[1].eq(&args[0])))
        });

    g.borrow_mut().add_func(
        "break",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(VVal::Nul)); }
            let args = env.slice(argc);
            Err(StackAction::Break(args[0].clone()))
        });

    g.borrow_mut().add_func(
        "push",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let args = env.slice(argc);
            let a = &args[0];
            let v = args[1].clone();
            v.push(a.clone());
            Ok(v)
        });

    g.borrow_mut().add_func(
        "range",
        |_: &Rc<VValFun>, env: &mut Env, argc: usize| {
            if argc <= 3 { return Ok(VVal::Nul); }
            let args = env.slice(argc);

            let from     = args[3].clone();
            let to       = args[2].clone();
            let step     = args[1].clone();
            let f        = args[0].clone();

            if let VVal::Flt(_) = from {
                let mut from = from.f();
                let to       = to.f();
                let step     = step.f();

                let mut ret = VVal::Nul;
                while from <= to {
                    match f.call(env, 0) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { return Ok(v); },
                        //e                          => { return e; },
                    }
                    from += step;
                }
                Ok(ret)
            } else {
                let mut from = from.i();
                let to       = to.i();
                let step     = step.i();

                let mut ret = VVal::Nul;
                while from <= to {
                    match f.call(env, 0) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { return Ok(v); },
                        // e                          => { return e; },
                    }
                    from += step;
                }
                Ok(ret)
            }
        });

    g
}
