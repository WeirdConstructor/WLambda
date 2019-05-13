use crate::compiler::*;
use crate::vval::*;

pub fn create_wlamba_prelude() -> GlobalEnvRef {
    let g = GlobalEnv::new();

    g.borrow_mut().add_func(
        "+",
        |env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(_) = env.arg(0) {
                let mut sum = 0.0;
                for i in 0..argc { sum = sum + env.arg(i).f() }
                Ok(VVal::Flt(sum))
            } else {
                let mut sum = 0;
                for i in 0..argc { sum = sum + env.arg(i).i() }
                Ok(VVal::Int(sum))
            }
        });

    g.borrow_mut().add_func(
        "-",
        |env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(_) = env.arg(0) {
                let mut sum = env.arg(0).f();
                for i in 1..argc { sum = sum - env.arg(i).f() }
                Ok(VVal::Flt(sum))
            } else {
                let mut sum = env.arg(0).i();
                for i in 1..argc { sum = sum - env.arg(i).i() }
                Ok(VVal::Int(sum))
            }
        });

    g.borrow_mut().add_func(
        "*",
        |env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(_) = env.arg(0) {
                let mut sum = env.arg(0).f();
                for i in 1..argc { sum = sum * env.arg(i).f() }
                Ok(VVal::Flt(sum))
            } else {
                let mut sum = env.arg(0).i();
                for i in 1..argc { sum = sum * env.arg(i).i() }
                Ok(VVal::Int(sum))
            }
        });

    g.borrow_mut().add_func(
        "/",
        |env: &mut Env, argc: usize| {
            if argc <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(_) = env.arg(0) {
                let mut sum = env.arg(0).f();
                for i in 1..argc { sum = sum / env.arg(i).f() }
                Ok(VVal::Flt(sum))
            } else {
                let mut sum = env.arg(0).i();
                for i in 1..argc { sum = sum / env.arg(i).i() }
                Ok(VVal::Int(sum))
            }
        });

    g.borrow_mut().add_func(
        "==",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            println!("EQ: #{} {} {}", argc, env.arg(0).s(), env.arg(1).s());
            env.dump_stack();
            Ok(VVal::Bol(env.arg(1).eq(&env.arg(0))))
        });

    g.borrow_mut().add_func(
        "break",
        |env: &mut Env, argc: usize| {
            if argc < 1 { return Err(StackAction::Break(VVal::Nul)); }
            Err(StackAction::Break(env.arg(0).clone()))
        });

    g.borrow_mut().add_func(
        "push",
        |env: &mut Env, argc: usize| {
            if argc < 2 { return Ok(VVal::Nul); }
            let v = env.arg(0);
            // println!("PUSH:");
            // env.dump_stack();
            v.push(env.arg(1).clone());
            Ok(v.clone())
        });

    g.borrow_mut().add_func(
        "range",
        |env: &mut Env, argc: usize| {
            if argc <= 3 { return Ok(VVal::Nul); }
            let from     = env.arg(0);
            let to       = env.arg(1);
            let step     = env.arg(2);
            let f        = env.arg(3);
            println!("RAGEN from={} to={} f={}", from.s(), to.s(), f.s());

            if let VVal::Flt(_) = from {
                let mut from = from.f();
                let to       = to.f();
                let step     = step.f();

                let mut ret = VVal::Nul;
                while from <= to {
                    env.push(VVal::Flt(from));
                    match f.call(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); return Ok(v); },
                        //e                          => { return e; },
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            } else {
                let mut from = from.i();
                let to       = to.i();
                let step     = step.i();

                let mut ret = VVal::Nul;
                while from <= to {
                    env.push(VVal::Int(from));
                    match f.call(env, 1) {
                        Ok(v)                      => { ret = v; },
                        Err(StackAction::Break(v)) => { env.popn(1); println!("BREAK {}", v.s()); return Ok(v); },
                        // e                          => { return e; },
                    }
                    from += step;
                    env.popn(1);
                }
                Ok(ret)
            }
        });

    g
}
