use crate::compiler::*;
use crate::stdlib::io_types::*;
use crate::vval::*;
use std::cell::RefCell;
use std::process::{Child, Command, Stdio};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
struct VChildProcess {
    child: Rc<RefCell<Child>>,
    id: u32,
}

impl VValUserData for VChildProcess {
    fn s(&self) -> String {
        format!("$<ChildProcess:pid={}>", self.id)
    }
    fn i(&self) -> i64 {
        self.id as i64
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

pub fn add_to_symtable(st: &mut SymbolTable) {
    st.fun(
        "process:spawn",
        |env: &mut Env, argc: usize| {
            let cmd_exe = env.arg(0).deref();

            let mut cmd = cmd_exe.with_s_ref(|s| Command::new(s));

            if argc > 1 {
                let args = env.arg(1).deref();
                let mut argv = vec![];
                for a in args.iter() {
                    argv.push(a.0.s_raw());
                }
                cmd.args(argv);
            }

            if env.arg(2).with_s_ref(|s| s == "inherit_out") {
                cmd.stdin(Stdio::null());
            } else if env.arg(2).with_s_ref(|s| s == "ioe") {
                cmd.stdin(Stdio::piped());
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::piped());
            } else if env.arg(2).with_s_ref(|s| s == "ie") {
                cmd.stdin(Stdio::piped());
                cmd.stdout(Stdio::null());
                cmd.stderr(Stdio::piped());
            } else if env.arg(2).with_s_ref(|s| s == "io") {
                cmd.stdin(Stdio::piped());
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::null());
            } else if env.arg(2).with_s_ref(|s| s == "i") {
                cmd.stdin(Stdio::piped());
                cmd.stdout(Stdio::null());
                cmd.stderr(Stdio::null());
            } else if env.arg(2).with_s_ref(|s| s == "oe") {
                cmd.stdin(Stdio::null());
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::piped());
            } else if env.arg(2).with_s_ref(|s| s == "o") {
                cmd.stdin(Stdio::null());
                cmd.stdout(Stdio::piped());
                cmd.stderr(Stdio::null());
            } else if env.arg(2).with_s_ref(|s| s == "e") {
                cmd.stdin(Stdio::null());
                cmd.stdout(Stdio::null());
                cmd.stderr(Stdio::piped());
            } else if env.arg(2).with_s_ref(|s| s == "inherit_all") {
                // nop
            } else {
                cmd.stdin(Stdio::null());
                cmd.stdout(Stdio::null());
                cmd.stderr(Stdio::null());
            }

            let child = match cmd.spawn() {
                Ok(child) => child,
                Err(e) => {
                    return Ok(env.new_err(format!("Error executing '{}': {}", cmd_exe.s(), e)));
                }
            };

            let id = child.id();
            let child = Rc::new(RefCell::new(child));
            Ok(VVal::new_usr(VChildProcess { child, id }))
        },
        Some(1),
        Some(3),
        false,
    );

    st.fun(
        "process:take_pipes",
        |env: &mut Env, _argc: usize| {
            let mut chld = env.arg(0);
            chld.with_usr_ref(|vts: &mut VChildProcess| {
                let stdin = vts.child.borrow_mut().stdin.take();
                let stdout = vts.child.borrow_mut().stdout.take();
                let stderr = vts.child.borrow_mut().stderr.take();
                let ret = VVal::map();
                if let Some(stdin) = stdin {
                    let stdin = VVal::new_usr(VIOHandle(Arc::new(Mutex::new(
                        IOHandle::ChildStdin(stdin)
                    ))));
                    ret.set_key_str("stdin", stdin).expect("single use");
                }
                if let Some(stdout) = stdout {
                    let stdout = VVal::new_usr(VIOHandle(Arc::new(Mutex::new(
                        IOHandle::ChildStdout(stdout)
                    ))));
                    ret.set_key_str("stdout", stdout).expect("single use");
                }
                if let Some(stderr) = stderr {
                    let stderr = VVal::new_usr(VIOHandle(Arc::new(Mutex::new(
                        IOHandle::ChildStderr(stderr)
                    ))));
                    ret.set_key_str("stderr", stderr).expect("single use");
                }
                Ok(ret)
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "std:process:take_pipes: First argument not a child process handle! {}",
                    chld.s()
                )))
            })
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "process:try_wait",
        |env: &mut Env, _argc: usize| {
            let mut chld = env.arg(0);
            chld.with_usr_ref(|vts: &mut VChildProcess| match vts.child.borrow_mut().try_wait() {
                Ok(Some(st)) => {
                    let ret = VVal::map();
                    ret.set_key_str("status", VVal::Int(st.code().unwrap_or(-1) as i64))
                        .expect("single use");
                    ret.set_key_str("success", VVal::Bol(st.success())).expect("single use");
                    Ok(ret)
                }
                Ok(None) => Ok(VVal::None),
                Err(e) => Ok(env.new_err(format!("Error try_wait pid={}: {}", vts.id, e))),
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "std:process:try_wait: First argument not a child process handle! {}",
                    chld.s()
                )))
            })
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "process:wait",
        |env: &mut Env, _argc: usize| {
            let mut chld = env.arg(0);
            chld.with_usr_ref(|vts: &mut VChildProcess| match vts.child.borrow_mut().wait() {
                Ok(st) => {
                    let ret = VVal::map();
                    ret.set_key_str("status", VVal::Int(st.code().unwrap_or(-1) as i64))
                        .expect("single use");
                    ret.set_key_str("success", VVal::Bol(st.success())).expect("single use");
                    Ok(ret)
                }
                Err(e) => Ok(env.new_err(format!("Error wait pid={}: {}", vts.id, e))),
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "std:process:wait: First argument not a child process handle! {}",
                    chld.s()
                )))
            })
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "process:kill_wait",
        |env: &mut Env, _argc: usize| {
            let mut chld = env.arg(0);
            chld.with_usr_ref(|vts: &mut VChildProcess| {
                let kill_res = vts.child.borrow_mut().kill();
                match kill_res {
                    Ok(_) => match vts.child.borrow_mut().wait() {
                        Ok(st) => {
                            let ret = VVal::map();
                            ret.set_key_str("status", VVal::Int(st.code().unwrap_or(-1) as i64))
                                .expect("single use");
                            ret.set_key_str("success", VVal::Bol(st.success()))
                                .expect("single use");
                            Ok(ret)
                        }
                        Err(e) => {
                            Ok(env.new_err(format!("Error killing & wait pid={}: {}", vts.id, e)))
                        }
                    },
                    Err(e) => Ok(env.new_err(format!("Error killing pid={}: {}", vts.id, e))),
                }
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "std:process:kill_wait: First argument not a child process handle! {}",
                    chld.s()
                )))
            })
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "process:run",
        |env: &mut Env, argc: usize| {
            let cmd_exe = env.arg(0).deref();

            let mut cmd = cmd_exe.with_s_ref(|s| Command::new(s));

            if argc > 1 {
                let args = env.arg(1).deref();
                let mut argv = vec![];
                for a in args.iter() {
                    argv.push(a.0.s_raw());
                }
                cmd.args(argv);
            }

            cmd.stdin(Stdio::null());

            match cmd.output() {
                Ok(out) => {
                    let ret = VVal::map();
                    ret.set_key_str("status", VVal::Int(out.status.code().unwrap_or(-1) as i64))
                        .expect("single use");
                    ret.set_key_str("success", VVal::Bol(out.status.success()))
                        .expect("single use");
                    ret.set_key_str("stdout", VVal::new_byt(out.stdout)).expect("single use");
                    ret.set_key_str("stderr", VVal::new_byt(out.stderr)).expect("single use");

                    Ok(ret)
                }
                Err(e) => {
                    return Ok(env.new_err(format!("Error executing '{}': {}", cmd_exe.s(), e)));
                }
            }
        },
        Some(1),
        Some(2),
        false,
    );
}
