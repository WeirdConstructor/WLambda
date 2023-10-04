use crate::compiler::*;
use crate::vval::*;
use std::cell::RefCell;
use std::net::{TcpStream, UdpSocket};
use std::process::{ChildStderr, ChildStdin, ChildStdout};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct RWHandleError(String);

#[derive(Debug, Clone)]
pub enum ChildHandle {
    Stdin(Arc<Mutex<ChildStdin>>),
    Stdout(Arc<Mutex<ChildStdout>>),
    Stderr(Arc<Mutex<ChildStderr>>),
}

#[derive(Debug, Clone)]
pub struct VChildHandle {
    pub handle: ChildHandle,
}

impl crate::threads::ThreadSafeUsr for VChildHandle {
    fn to_vval(&self) -> VVal {
        VVal::Usr(Box::new(VChildHandle { handle: self.handle.clone() }))
    }
}

impl VValUserData for VChildHandle {
    fn s(&self) -> String {
        format!(
            "$<ChildHandle:{}>",
            match self.handle {
                ChildHandle::Stdin(_) => "stdin",
                ChildHandle::Stdout(_) => "stdout",
                ChildHandle::Stderr(_) => "stderr",
            }
        )
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        Some(Box::new(VChildHandle { handle: self.handle.clone() }))
    }
}

#[derive(Debug, Clone)]
pub struct VTcpStream {
    pub stream: Rc<RefCell<TcpStream>>,
}

pub struct VTcpStreamThreadSafe {
    pub stream: TcpStream,
}

impl crate::threads::ThreadSafeUsr for VTcpStreamThreadSafe {
    fn to_vval(&self) -> VVal {
        match self.stream.try_clone() {
            Ok(stream) => VVal::Usr(Box::new(VTcpStream { stream: Rc::new(RefCell::new(stream)) })),
            Err(e) => VVal::err_msg(&format!("Can't clone TcpStream: {}", e)),
        }
    }
}

impl VValUserData for VTcpStream {
    fn s(&self) -> String {
        format!(
            "$<TcpStream:local={}/remote={}>",
            self.stream
                .borrow()
                .local_addr()
                .map(|a| a.to_string())
                .map_or_else(|_| String::from("?"), |a| a),
            self.stream.borrow().peer_addr().map_or_else(|_| String::from("?"), |a| a.to_string())
        )
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        match self.stream.borrow().try_clone() {
            Ok(stream) => Some(Box::new(VTcpStreamThreadSafe { stream })),
            Err(_e) => None,
        }
    }
}

pub fn with_read_usr<R, F: FnMut(&mut dyn std::io::Read) -> R>(
    mut fd: VVal,
    mut f: F,
) -> Result<R, RWHandleError> {
    if fd.is_usr::<VTcpStream>() {
        return Ok(fd
            .with_usr_ref(|vts: &mut VTcpStream| f(&mut *vts.stream.borrow_mut()))
            .expect("It should really be a VTcpStream here"));
    } else if fd.is_usr::<VChildHandle>() {
        return fd
            .with_usr_ref(|vch: &mut VChildHandle| match &mut vch.handle {
                ChildHandle::Stdout(stdout) => {
                    let mut lock = match stdout.lock() {
                        Ok(lock) => lock,
                        Err(e) => {
                            return Err(RWHandleError(format!(
                                "Can't lock stdout for reading: {}",
                                e
                            )));
                        }
                    };
                    Ok(f(&mut *lock))
                }
                ChildHandle::Stderr(stderr) => {
                    let mut lock = match stderr.lock() {
                        Ok(lock) => lock,
                        Err(e) => {
                            return Err(RWHandleError(format!(
                                "Can't lock stderr for reading: {}",
                                e
                            )));
                        }
                    };
                    Ok(f(&mut *lock))
                }
                _ => Err(RWHandleError(
                    "Can't use for reading, not an stdout or stderr handle".to_string(),
                )),
            })
            .expect("It should really be a VChildHandle here");
    }

    Err(RWHandleError("not a readable handle".to_string()))
}

pub fn with_write_usr<R, F: FnMut(&mut dyn std::io::Write) -> R>(
    mut fd: VVal,
    mut f: F,
) -> Result<R, RWHandleError> {
    if fd.is_usr::<VTcpStream>() {
        return Ok(fd
            .with_usr_ref(|vts: &mut VTcpStream| f(&mut *vts.stream.borrow_mut()))
            .expect("It should really bea VTcpStream here"));
    } else if fd.is_usr::<VChildHandle>() {
        return fd
            .with_usr_ref(|vch: &mut VChildHandle| match &mut vch.handle {
                ChildHandle::Stdin(stdin) => {
                    let mut lock = match stdin.lock() {
                        Ok(lock) => lock,
                        Err(e) => {
                            return Err(RWHandleError(format!(
                                "Can't lock stdin for reading: {}",
                                e
                            )));
                        }
                    };
                    Ok(f(&mut *lock))
                }
                _ => Err(RWHandleError("Can't use for writing, not an stdin handle".to_string())),
            })
            .expect("It should really be a VChildHandle here");
    }

    Err(RWHandleError("not a writable handle".to_string()))

    //            Ok(fd
    //                .with_usr_ref(|vts: &mut VTcpStream| {
    //                    use std::io::Write;
    //
    //                    data.with_bv_ref(|bytes| {
    //                        if offs >= bytes.len() {
    //                            return env.new_err("std:io:write_some: bad buffer offset".to_string());
    //                        }
    //
    //                        let r = vts.stream.borrow_mut().write_all(&bytes[offs..]);
    //                        match r {
    //                            Ok(()) => VVal::Int(bytes.len() as i64),
    //                            Err(e) => match e.kind() {
    //                                std::io::ErrorKind::Interrupted => VVal::None,
    //                                _ => env.new_err(format!("std:io:write_some: {}", e)),
    //                            },
    //                        }
    //                    })
    //                })
    //                .unwrap_or_else(|| {
    //                    env.new_err(format!(
    //                        "std:io:write: First argument not an IO handle! {}",
    //                        fd.s()
    //                    ))
    //                }))
}

#[derive(Debug, Clone)]
pub struct VUdpSocket {
    pub socket: Rc<RefCell<UdpSocket>>,
}

pub struct VUdpSocketThreadSafe {
    pub socket: UdpSocket,
}

impl crate::threads::ThreadSafeUsr for VUdpSocketThreadSafe {
    fn to_vval(&self) -> VVal {
        match self.socket.try_clone() {
            Ok(socket) => VVal::Usr(Box::new(VUdpSocket { socket: Rc::new(RefCell::new(socket)) })),
            Err(e) => VVal::err_msg(&format!("Can't clone VUdpSocket: {}", e)),
        }
    }
}

impl VValUserData for VUdpSocket {
    fn s(&self) -> String {
        format!(
            "$<UdpSocket:local={}/remote={}>",
            self.socket
                .borrow()
                .local_addr()
                .map(|a| a.to_string())
                .map_or_else(|_| String::from("?"), |a| a),
            self.socket.borrow().peer_addr().map_or_else(|_| String::from("?"), |a| a.to_string())
        )
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        match self.socket.borrow().try_clone() {
            Ok(socket) => Some(Box::new(VUdpSocketThreadSafe { socket })),
            Err(_e) => None,
        }
    }
}

pub fn io_add_to_symtable(st: &mut SymbolTable) {
    st.fun(
        "io:flush",
        |env: &mut Env, _argc: usize| {
            use std::io::Write;

            let mut fd = env.arg(0);

            Ok(with_write_usr(fd.clone(), |wr: &mut dyn std::io::Write| match wr.flush() {
                Ok(_) => VVal::Bol(true),
                Err(e) => env.new_err(format!("std:io:flush: {}", e)),
            })
            .unwrap_or_else(|err| {
                env.new_err(format!("std:io:flush: RWHandleError on {}: {:?}", fd.s(), err))
            }))
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "io:write",
        |env: &mut Env, _argc: usize| {
            let fd = env.arg(0);
            let data = env.arg(1);
            let offs = env.arg(2).i() as usize;

            Ok(with_write_usr(fd.clone(), |wr: &mut dyn std::io::Write| {
                data.with_bv_ref(|bytes| {
                    if offs >= bytes.len() {
                        return env.new_err("std:io:write: bad buffer offset".to_string());
                    }

                    let r = wr.write_all(&bytes[offs..]);
                    match r {
                        Ok(()) => VVal::Int(bytes.len() as i64),
                        Err(e) => match e.kind() {
                            std::io::ErrorKind::Interrupted => VVal::None,
                            _ => env.new_err(format!("std:io:write: {}", e)),
                        },
                    }
                })
            })
            .unwrap_or_else(|err| {
                env.new_err(format!("std:io:write: RWHandleError on {}: {:?}", fd.s(), err))
            }))
        },
        Some(2),
        Some(3),
        false,
    );

    st.fun(
        "io:write_some",
        |env: &mut Env, _argc: usize| {
            let mut fd = env.arg(0);
            let data = env.arg(1);
            let offs = env.arg(2).i() as usize;

            Ok(with_write_usr(fd.clone(), |wr: &mut dyn std::io::Write| {
                    data.with_bv_ref(|bytes| {
                        if offs >= bytes.len() {
                            return env.new_err("std:io:write_some: bad buffer offset".to_string());
                        }

                        let r = wr.write(&bytes[offs..]);
                        match r {
                            Ok(n) => {
                                if n == 0 {
                                    VVal::opt_none()
                                } else {
                                    VVal::opt(VVal::Int(n as i64))
                                }
                            }
                            Err(e) => match e.kind() {
                                std::io::ErrorKind::WouldBlock
                                | std::io::ErrorKind::TimedOut
                                | std::io::ErrorKind::Interrupted => VVal::None,
                                _ => env.new_err(format!("std:io:write_some: {}", e)),
                            },
                        }
                    })
                })
                .unwrap_or_else(|err| {
                    env.new_err(format!("std:io:write_some: RWHandleError on {}: {:?}", fd.s(), err))
                }))
        },
        Some(2),
        Some(3),
        false,
    );

    st.fun(
        "io:read_some",
        |env: &mut Env, _argc: usize| {
            let fd = env.arg(0);
            Ok(with_read_usr(fd.clone(), |rd: &mut dyn std::io::Read| {
                let mut buf: [u8; 4096] = [0; 4096];
                let r = rd.read(&mut buf[..]);
                match r {
                    Ok(n) => {
                        if n == 0 {
                            VVal::opt_none()
                        } else {
                            VVal::opt(VVal::new_byt(buf[0..n].to_vec()))
                        }
                    }
                    Err(e) => match e.kind() {
                        std::io::ErrorKind::WouldBlock
                        | std::io::ErrorKind::TimedOut
                        | std::io::ErrorKind::Interrupted => VVal::None,
                        _ => env.new_err(format!("std:io:read_some: {}", e)),
                    },
                }
            })
            .unwrap_or_else(|err| {
                env.new_err(format!("std:io:read_some: RWHandleError on {}: {:?}", fd.s(), err))
            }))
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "io:read_all",
        |env: &mut Env, _argc: usize| {
            let fd = env.arg(0);
            Ok(with_read_usr(fd.clone(), |rd: &mut dyn std::io::Read| {
                let mut buf = Vec::new();
                let r = rd.read_to_end(&mut buf);
                match r {
                    Ok(n) => VVal::new_byt(buf),
                    Err(e) => env.new_err(format!("std:io:read_all: {}", e)),
                }
            })
            .unwrap_or_else(|err| {
                env.new_err(format!("std:io:read_all: RWHandleError on {}: {:?}", fd.s(), err))
            }))
        },
        Some(1),
        Some(1),
        false,
    );
}
