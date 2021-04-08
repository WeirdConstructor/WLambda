use crate::compiler::*;
use crate::vval::*;
use std::process::{Command, Child, Stdio};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone)]
struct VChildProcess {
    child: Rc<RefCell<Child>>,
    id: u32,
}

impl VValUserData for VChildProcess {
    fn s(&self) -> String { format!("$<ChildProcess:pid={}>", self.id) }
    fn i(&self) -> i64 { self.id as i64 }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

pub fn add_to_symtable(st: &mut SymbolTable) {
    st.fun("process:spawn", |env: &mut Env, argc: usize| {
        let cmd_exe = env.arg(0).deref();

        let mut cmd =
            cmd_exe.with_s_ref(|s| Command::new(s));

        if argc > 1 {
            let args = env.arg(1).deref();
            for a in args.iter() {
                a.0.with_s_ref(|s| cmd.arg(s));
            }
        }

        if env.arg(2).with_s_ref(|s| s == "inherit_out") {
            cmd.stdin(Stdio::null());

        } else if env.arg(2).with_s_ref(|s| s == "inherit_all") {
            // nop

        } else {
            cmd.stdin(Stdio::null());
            cmd.stdout(Stdio::null());
            cmd.stderr(Stdio::null());
        }

        let child =
            match cmd.spawn() {
                Ok(child) => child,
                Err(e) => {
                    return Ok(env.new_err(
                        format!("Error executing '{}': {}", cmd_exe.s(), e)));
                },
            };

        let id = child.id();
        let child = Rc::new(RefCell::new(child));
        Ok(VVal::new_usr(VChildProcess { child, id }))
    }, Some(1), Some(3), false);

    st.fun("process:try_wait", |env: &mut Env, _argc: usize| {
        let mut chld = env.arg(0);
        chld.with_usr_ref(|vts: &mut VChildProcess| {
            match vts.child.borrow_mut().try_wait() {
                Ok(None) => Ok(VVal::None),
                Ok(Some(st)) => {
                    let ret = VVal::map();
                    ret.set_key_str("status", VVal::Int(st.code().unwrap_or(-1) as i64))
                        .expect("single use");
                    ret.set_key_str("success", VVal::Bol(st.success()))
                        .expect("single use");
                    Ok(ret)
                },
                Err(e) => {
                    Ok(env.new_err(
                        format!("Error wait pid={}: {}",
                            vts.id, e)))
                }
            }
        }).unwrap_or(Ok(VVal::None))
    }, Some(1), Some(1), false);

    st.fun("process:wait", |env: &mut Env, _argc: usize| {
        let mut chld = env.arg(0);
        chld.with_usr_ref(|vts: &mut VChildProcess| {
            match vts.child.borrow_mut().wait() {
                Ok(st) => {
                    let ret = VVal::map();
                    ret.set_key_str("status", VVal::Int(st.code().unwrap_or(-1) as i64))
                        .expect("single use");
                    ret.set_key_str("success", VVal::Bol(st.success()))
                        .expect("single use");
                    Ok(ret)
                },
                Err(e) => {
                    Ok(env.new_err(
                        format!("Error wait pid={}: {}",
                            vts.id, e)))
                }
            }
        }).unwrap_or(Ok(VVal::None))
    }, Some(1), Some(1), false);

    st.fun("process:kill_wait", |env: &mut Env, _argc: usize| {
        let mut chld = env.arg(0);
        chld.with_usr_ref(|vts: &mut VChildProcess| {
            let kill_res = vts.child.borrow_mut().kill();
            match kill_res {
                Ok(_) => {
                    match vts.child.borrow_mut().wait() {
                        Ok(st) => {
                            let ret = VVal::map();
                            ret.set_key_str("status", VVal::Int(st.code().unwrap_or(-1) as i64))
                                .expect("single use");
                            ret.set_key_str("success", VVal::Bol(st.success()))
                                .expect("single use");
                            Ok(ret)
                        },
                        Err(e) => {
                            Ok(env.new_err(
                                format!("Error killing & wait pid={}: {}",
                                    vts.id, e)))
                        }
                    }
                },
                Err(e) =>
                    Ok(env.new_err(
                        format!("Error killing pid={}: {}",
                            vts.id, e))),
            }
        }).unwrap_or(Ok(VVal::None))
    }, Some(1), Some(1), false);

    st.fun("process:run", |env: &mut Env, argc: usize| {
        let cmd_exe = env.arg(0).deref();

        let mut cmd =
            cmd_exe.with_s_ref(|s| Command::new(s));

        if argc > 1 {
            let args = env.arg(1).deref();
            for a in args.iter() {
                a.0.with_s_ref(|s| cmd.arg(s));
            }
        }

        cmd.stdin(Stdio::null());

        match cmd.output() {
            Ok(out) => {
                let ret = VVal::map();
                ret.set_key_str("status", VVal::Int(out.status.code().unwrap_or(-1) as i64))
                    .expect("single use");
                ret.set_key_str("success", VVal::Bol(out.status.success()))
                    .expect("single use");
                ret.set_key_str("stdout", VVal::new_byt(out.stdout))
                    .expect("single use");
                ret.set_key_str("stderr", VVal::new_byt(out.stderr))
                    .expect("single use");

                Ok(ret)
            },
            Err(e) => {
                return Ok(env.new_err(
                    format!("Error executing '{}': {}", cmd_exe.s(), e)));
            },
        }

    }, Some(1), Some(2), false);
}
