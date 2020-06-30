use crate::vval::{VVal, Env, StackAction};

pub fn print_value(env: &mut Env, argc: usize, raw: bool) -> Result<VVal, StackAction> {
    let mut write = env.stdio.write.borrow_mut();

    for i in 0..argc {
        if raw {
            env.arg_ref(i).unwrap().with_s_ref(|s: &str| {
                if i == (argc - 1) {
                    if i > 0 { write!(write, " ").ok(); }
                    writeln!(write, "{}", s).ok();
                } else {
                    if i > 0 { write!(write, " ").ok(); }
                    write!(write, "{}", s).ok();
                }
            });
        } else {
            let s = env.arg_ref(i).unwrap().s();

            if i == (argc - 1) {
                if i > 0 { write!(write, " ").ok(); }
                writeln!(write, "{}", s).ok();
            } else {
                if i > 0 { write!(write, " ").ok(); }
                write!(write, "{}", s).ok();
            }
        }
    }
    if argc == 0 {
        writeln!(write).ok();
    }
    if argc > 0 {
        Ok(env.arg(argc - 1))
    } else {
        Ok(VVal::None)
    }
}
pub fn debug_print_value(env: &mut Env, argc: usize, pos_str: &str) -> Result<VVal, StackAction> {
    let mut write = env.stdio.write.borrow_mut();

    write!(write, "{} DEBUG: ", pos_str);

    let mut direct_print = false;
    for i in 0..argc {
        let t = env.arg_ref(i).unwrap().type_name();

        if t == "symbol" && !direct_print {
            env.arg_ref(i).unwrap().with_s_ref(|s: &str| {
                if s == "\\" {
                    direct_print = true;
                }
            });
            if direct_print {
                continue;
            }
        }

        if direct_print {
            direct_print = false;

            env.arg_ref(i).unwrap().with_s_ref(|s: &str| {
                if i == (argc - 1) {
                    if i > 1 { write!(write, " ").ok(); }
                    writeln!(write, "{}", s).ok();
                } else {
                    if i > 1 { write!(write, " ").ok(); }
                    write!(write, "{}", s).ok();
                }
            })
        } else {
            let s = env.arg_ref(i).unwrap().s();

            if i == (argc - 1) {
                if i > 0 { write!(write, " ").ok(); }
                writeln!(write, "{}({})", s, t).ok();
            } else {
                if i > 0 { write!(write, " ").ok(); }
                write!(write, "{}({})", s, t).ok();
            }
        }
    }
    if argc == 0 {
        writeln!(write).ok();
    }
    if argc > 0 {
        Ok(env.arg(argc - 1))
    } else {
        Ok(VVal::None)
    }
}

