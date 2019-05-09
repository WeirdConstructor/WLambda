use crate::compiler::*;
use crate::vval::*;

pub fn create_wlamba_prelude() -> GlobalEnvRef {
    let g = GlobalEnv::new();

    g.borrow_mut().add_func(
        "+",
        |_upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
            if args.len() <= 0 { return Ok(VVal::Nul); }
            if let VVal::Flt(_) = args[0] {
                Ok(VVal::Flt(args.iter().map(|v| v.f()).sum()))
            } else {
                Ok(VVal::Int(args.iter().map(|v| v.i()).sum()))
            }
        });

    g.borrow_mut().add_func(
        "-",
        |_upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
            if args.len() <= 0 { return Ok(VVal::Nul); }
            let f = &args[0];
            if let VVal::Flt(_) = f {
                Ok(VVal::Flt(args.iter().skip(1).fold(f.f(), |a, v| a - v.f())))
            } else {
                Ok(VVal::Int(args.iter().skip(1).fold(f.i(), |a, v| a - v.i())))
            }
        });

    g.borrow_mut().add_func(
        "*",
        |_upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
            if args.len() <= 0 { return Ok(VVal::Nul); }
            let f = &args[0];
            if let VVal::Flt(_) = f {
                Ok(VVal::Flt(args.iter().skip(1).fold(f.f(), |a, v| a * v.f())))
            } else {
                Ok(VVal::Int(args.iter().skip(1).fold(f.i(), |a, v| a * v.i())))
            }
        });

    g.borrow_mut().add_func(
        "/",
        |_upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
            if args.len() <= 0 { return Ok(VVal::Nul); }
            let f = &args[0];
            if let VVal::Flt(_) = f {
                Ok(VVal::Flt(args.iter().skip(1).fold(f.f(), |a, v| a / v.f())))
            } else {
                Ok(VVal::Int(args.iter().skip(1).fold(f.i(), |a, v| a / v.i())))
            }
        });

    g.borrow_mut().add_func(
        "range",
        |_upv: &std::vec::Vec<(usize, VVal)>, args: std::vec::Vec<VVal>| {
            if args.len() <= 3 { return Ok(VVal::Nul); }

            if let VVal::Flt(_) = args[0] {
                let mut from = args[0].f();
                let to       = args[1].f();
                let step     = args[2].f();
                let f        = &args[3];

                let mut ret = VVal::Nul;
                while from <= to {
                    let a = vec![VVal::Flt(from)];
                    match f.call(a) {
                        Ok(v) => { ret = v; },
                        e     => { return e; },
                    }
                    from += step;
                }
                Ok(ret)
            } else {
                let mut from = args[0].i();
                let to       = args[1].i();
                let step     = args[2].i();
                let f        = &args[3];

                let mut ret = VVal::Nul;
                while from <= to {
                    let a = vec![VVal::Int(from)];
                    match f.call(a) {
                        Ok(v) => { ret = v; },
                        e     => { return e; },
                    }
                    from += step;
                }
                Ok(ret)
            }
        });

    g
}
