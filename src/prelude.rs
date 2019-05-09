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

    g
}
