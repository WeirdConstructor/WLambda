use crate::compiler::*;
use crate::vval::*;
use std::cell::RefCell;
use std::io::{BufReader, BufWriter};
use std::net::{TcpStream, UdpSocket};
use std::process::{ChildStderr, ChildStdin, ChildStdout};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum IOHandle {
    TcpStream(TcpStream),
    TcpStreamBufWr(BufWriter<TcpStream>),
    TcpStreamBufRd(BufReader<TcpStream>),
    ChildStdin(ChildStdin),
    ChildStdout(ChildStdout),
    ChildStderr(ChildStderr),
    ChildStdinBufWr(BufWriter<ChildStdin>),
    ChildStdoutBufRd(BufReader<ChildStdout>),
    ChildStderrBufRd(BufReader<ChildStderr>),
}

impl IOHandle {
    pub fn with_tcp_stream<R, F: FnMut(&mut TcpStream) -> R>(
        &mut self,
        mut f: F,
    ) -> Result<R, IOHandleError> {
        match self {
            IOHandle::TcpStream(stream) => Ok(f(stream)),
            IOHandle::TcpStreamBufRd(buf) => Ok(f(buf.get_mut())),
            IOHandle::TcpStreamBufWr(buf) => Ok(f(buf.get_mut())),
            hdl => Err(IOHandleError(format!("TcpStream required, got: {:?}", hdl))),
        }
    }

    pub fn with_read_usr<R, F: FnMut(&mut dyn std::io::Read) -> R>(
        &mut self,
        mut f: F,
    ) -> Result<R, IOHandleError> {
        match self {
            IOHandle::TcpStream(stream) => Ok(f(&mut stream)),
            IOHandle::TcpStreamBufRd(buf) => Ok(f(buf.get_mut())),
            IOHandle::ChildStdout(c) => Ok(f(&mut c)),
            IOHandle::ChildStderr(c) => Ok(f(&mut c)),
            IOHandle::ChildStdoutBufRd(buf) => Ok(f(buf.get_mut())),
            IOHandle::ChildStderrBufRd(buf) => Ok(f(buf.get_mut())),
            _ => Err(IOHandleError(format!("{:?} is not a readable IOHandle", self))),
        }
    }

    pub fn with_write_usr<R, F: FnMut(&mut dyn std::io::Write) -> R>(
        &mut self,
        mut f: F,
    ) -> Result<R, IOHandleError> {
        match self {
            IOHandle::TcpStream(stream) => Ok(f(&mut stream)),
            IOHandle::TcpStreamBufWr(buf) => Ok(f(buf.get_mut())),
            IOHandle::ChildStdin(c) => Ok(f(&mut c)),
            IOHandle::ChildStdinBufWr(buf) => Ok(f(buf.get_mut())),
            _ => Err(IOHandleError(format!("{:?} is not a writable IOHandle", self))),
        }
    }

    fn s(&self) -> String {
        match self {
            IOHandle::TcpStream(_) | IOHandle::TcpStreamBufRd(_) | IOHandle::TcpStreamBufWr(_) => {
                self.with_tcp_stream(|stream_ref| {
                    format!(
                        "$<IOHandle:TcpStream:local={}/remote={}>",
                        stream_ref
                            .local_addr()
                            .map(|a| a.to_string())
                            .map_or_else(|_| String::from("?"), |a| a),
                        stream_ref
                            .peer_addr()
                            .map_or_else(|_| String::from("?"), |a| a.to_string())
                    )
                })
                .expect("This is a TcpStream, alright")
            }
            IOHandle::ChildStdin(s) => format!("$<IOHandle:ChildStdin>"),
            IOHandle::ChildStdout(s) => format!("$<IOHandle:ChildStdout>"),
            IOHandle::ChildStderr(s) => format!("$<IOHandle:ChildStderr>"),
            IOHandle::ChildStdinBufWr(s) => format!("$<IOHandle:ChildStdin:Buffered>"),
            IOHandle::ChildStdoutBufRd(s) => format!("$<IOHandle:ChildStdout:Buffered>"),
            IOHandle::ChildStderrBufRd(s) => format!("$<IOHandle:ChildStderr:Buffered>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IOHandleError(String);

#[derive(Debug, Clone)]
pub struct VIOHandle(pub Arc<Mutex<IOHandle>>);

impl crate::threads::ThreadSafeUsr for VIOHandle {
    fn to_vval(&self) -> VVal {
        VVal::new_usr(self.clone())
    }
}

impl crate::threads::ThreadSafeUsr for IOHandleError {
    fn to_vval(&self) -> VVal {
        VVal::err_msg(&self.0)
    }
}

impl VValUserData for VIOHandle {
    fn s(&self) -> String {
        match self.0.lock() {
            Ok(h) => h.s(),
            Err(e) => {
                format!("$<IOHandle:BadLock:{}>", e)
            }
        }
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        let clone = self.0.clone();
        match clone.lock() {
            Ok(h) => match *h {
                IOHandle::TcpStream(stream_ref) => match stream_ref.try_clone() {
                    Ok(stream) => Some(Box::new(VIOHandle(Arc::new(Mutex::new(
                        IOHandle::TcpStream(stream),
                    ))))),
                    Err(e) => {
                        Some(Box::new(IOHandleError(format!("Can't clone TcpStream: {}", e))))
                    }
                },
                _ => Some(clone),
            },
            Err(e) => Some(Box::new(IOHandleError(format!(
                "Can't clone this IOHandle, error on mutex lock: {}",
                e
            )))),
        }
    }
}

//#[derive(Debug, Clone)]
//pub enum ChildHandle {
//    Stdin(Arc<Mutex<ChildStdin>>),
//    Stdout(Arc<Mutex<ChildStdout>>),
//    Stderr(Arc<Mutex<ChildStderr>>),
//}
//
//#[derive(Debug, Clone)]
//pub struct VChildHandle {
//    pub handle: ChildHandle,
//}
//
//impl crate::threads::ThreadSafeUsr for VChildHandle {
//    fn to_vval(&self) -> VVal {
//        VVal::Usr(Box::new(VChildHandle { handle: self.handle.clone() }))
//    }
//}
//
//impl VValUserData for VChildHandle {
//    fn s(&self) -> String {
//        format!(
//            "$<ChildHandle:{}>",
//            match self.handle {
//                ChildHandle::Stdin(_) => "stdin",
//                ChildHandle::Stdout(_) => "stdout",
//                ChildHandle::Stderr(_) => "stderr",
//            }
//        )
//    }
//    fn as_any(&mut self) -> &mut dyn std::any::Any {
//        self
//    }
//    fn clone_ud(&self) -> Box<dyn VValUserData> {
//        Box::new(self.clone())
//    }
//
//    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
//        Some(Box::new(VChildHandle { handle: self.handle.clone() }))
//    }
//}
//
//#[derive(Debug, Clone)]
//pub struct VTcpStream {
//    pub stream: Rc<RefCell<TcpStream>>,
//}
//
//pub struct VTcpStreamThreadSafe {
//    pub stream: TcpStream,
//}
//
//impl crate::threads::ThreadSafeUsr for VTcpStreamThreadSafe {
//    fn to_vval(&self) -> VVal {
//        match self.stream.try_clone() {
//            Ok(stream) => VVal::Usr(Box::new(VTcpStream { stream: Rc::new(RefCell::new(stream)) })),
//            Err(e) => VVal::err_msg(&format!("Can't clone TcpStream: {}", e)),
//        }
//    }
//}
//
//impl VValUserData for VTcpStream {
//    fn s(&self) -> String {
//        format!(
//            "$<TcpStream:local={}/remote={}>",
//            self.stream
//                .borrow()
//                .local_addr()
//                .map(|a| a.to_string())
//                .map_or_else(|_| String::from("?"), |a| a),
//            self.stream.borrow().peer_addr().map_or_else(|_| String::from("?"), |a| a.to_string())
//        )
//    }
//    fn as_any(&mut self) -> &mut dyn std::any::Any {
//        self
//    }
//    fn clone_ud(&self) -> Box<dyn VValUserData> {
//        Box::new(self.clone())
//    }
//
//    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
//        match self.stream.borrow().try_clone() {
//            Ok(stream) => Some(Box::new(VTcpStreamThreadSafe { stream })),
//            Err(_e) => None,
//        }
//    }
//}

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

macro_rules! get_io_handle {
    ($fun: expr, $env: expr, $vv: expr, $name: ident, $code: block) => {
        $vv.with_usr_ref(|ioh: &mut VIOHandle| match ioh.0.lock() {
            Ok($name) => $code,
            Err(e) => Ok($env.new_err(format!("{}: Can't lock IOHandle: {}", $fun, e))),
        })
        .unwrap_or_else(|| {
            Ok($env.new_err(format!("{}: Argument is not an IOHandle: {}", $fun, $vv.s())))
        })
    };
}

pub fn io_add_to_symtable(st: &mut SymbolTable) {
    st.fun(
        "io:flush",
        |env: &mut Env, _argc: usize| {
            get_io_handle!("std:io:flush", env, env.arg(0), ioh, {
                ioh.with_write_usr(|wr: &mut dyn std::io::Write| match wr.flush() {
                    Ok(_) => Ok(VVal::Bol(true)),
                    Err(e) => Ok(env.new_err(format!("std:io:flush: {}", e))),
                })
            })
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
                env.new_err(format!("std:io:write: IOHandleError on {}: {:?}", fd.s(), err))
            }))
        },
        Some(2),
        Some(3),
        false,
    );

    st.fun(
        "io:write_some",
        |env: &mut Env, _argc: usize| {
            let fd = env.arg(0);
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
                env.new_err(format!("std:io:write_some: IOHandleError on {}: {:?}", fd.s(), err))
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
                env.new_err(format!("std:io:read_some: IOHandleError on {}: {:?}", fd.s(), err))
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
                    Ok(_n) => VVal::new_byt(buf),
                    Err(e) => env.new_err(format!("std:io:read_all: {}", e)),
                }
            })
            .unwrap_or_else(|err| {
                env.new_err(format!("std:io:read_all: IOHandleError on {}: {:?}", fd.s(), err))
            }))
        },
        Some(1),
        Some(1),
        false,
    );

    st.fun(
        "net:tcp:set_timeouts",
        |env: &mut Env, _argc: usize| {
            let mut socket = env.arg(0);
            let read_t = env.arg(1);
            let write_t = env.arg(2);

            let read_t = if read_t.is_some() {
                match read_t.to_duration() {
                    Ok(dur) => Some(dur),
                    Err(e) => return Ok(e),
                }
            } else {
                None
            };

            let write_t = if write_t.is_some() {
                match write_t.to_duration() {
                    Ok(dur) => Some(dur),
                    Err(e) => return Ok(e),
                }
            } else {
                None
            };

            get_io_handle!("std:net:tcp:set_timeouts", env, env.arg(0), ioh, {
                ioh.with_tcp_stream(|stream| {
                    let _ = stream.borrow_mut().set_read_timeout(read_t);
                    let _ = stream.borrow_mut().set_write_timeout(write_t);
                    Ok(VVal::Bol(true))
                })
            })
        },
        Some(2),
        Some(3),
        false,
    );
}
