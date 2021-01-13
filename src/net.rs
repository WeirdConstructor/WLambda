// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.
use crate::compiler::*;
use crate::vval::*;
use std::rc::Rc;
use std::cell::RefCell;

use std::net::{SocketAddr, ToSocketAddrs};

fn vv2socketaddr(vv: &VVal) -> Result<Box<dyn std::iter::Iterator<Item = SocketAddr>>, String> {
    let addr =
        if vv.is_pair() {
            let port = vv.v_i(1) as u16;
            vv.at(0)
              .unwrap_or(VVal::None)
              .with_s_ref(|host| { (host, port).to_socket_addrs() })
        } else {
            vv.with_s_ref(|addr| addr.to_socket_addrs())
        };

    match addr {
        Ok(it) => Ok(Box::new(it)),
        Err(e) => { Err(format!("Couldn't resolve '{}': {}", vv.s(), e)) }
    }
}

#[derive(Debug, Clone)]
struct VTcpStream {
    stream: Rc<RefCell<std::net::TcpStream>>,
}

impl VValUserData for VTcpStream {
    fn s(&self) -> String {
        format!(
            "$<TcpStream:local={}/remote={}>",
            self.stream.borrow().local_addr()
                .map(|a| a.to_string())
                .map_or_else(|_| String::from("?"), |a| a.to_string()),
            self.stream.borrow().peer_addr()
                .map_or_else(|_| String::from("?"), |a| a.to_string()))
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

pub fn add_to_symtable(st: &mut SymbolTable) {
    st.fun("net:tcp:listen", |env: &mut Env, _argc: usize| {
        let vaddr = env.arg(0);
        let fun   = env.arg(1);

        let mut addr =
            match vv2socketaddr(&vaddr) {
                Ok(addr) => addr,
                Err(e) => { return Ok(env.new_err(e)) },
            };

        let first = addr.next();
        if first.is_none() {
            return Ok(env.new_err(format!(
                "Couldn't get socket address to bind to from '{}'",
                vaddr.s_raw())));
        }

        match std::net::TcpListener::bind(first.unwrap()) {
            Ok(listener) => {
                #[cfg(feature="socket2")]
                let listener = {
                    use socket2::Socket;
                    let socket = Socket::from(listener);
                    socket.set_reuse_address(true);
                    #[cfg(unix)]
                    socket.set_reuse_port(true);
                    println!("TETET");
                    socket.into_tcp_listener()
                };

                for stream in listener.incoming() {
                    match stream {
                        Err(e) => {
                            return Ok(
                                env.new_err(format!(
                                    "Couldn't get client: {}", e)));
                        },
                        Ok(stream) => {
                            let stream = VVal::Usr(Box::new(VTcpStream {
                                stream: Rc::new(RefCell::new(stream)),
                            }));
                            env.push(stream);
                            match fun.call_internal(env, 1) {
                                Ok(_) => {
                                    env.popn(1);
                                },
                                Err(e) => {
                                    env.popn(1);
                                    return Err(e);
                                },
                            }
                        },
                    }
                }

                Ok(VVal::None)
            },
            Err(e) => {
                Ok(env.new_err(format!(
                    "Couldn't create listener for '{}': {}",
                    vaddr.s_raw(), e)))
            },
        }
    }, Some(2), Some(2), false);

    st.fun("net:tcp:connect", |env: &mut Env, argc: usize| {
        let mut vaddr = env.arg(0);
        let mut adr_mode = 0;

        if vaddr.is_pair() && vaddr.v_(0).is_sym()
           && vaddr.v_(0).with_s_ref(|s| s == "v4") {

            adr_mode = 4;
            vaddr = vaddr.v_(1);
        } else if vaddr.is_pair() && vaddr.v_(0).is_sym()
                  && vaddr.v_(0).with_s_ref(|s| s == "v6") {

            adr_mode = 6;
            vaddr = vaddr.v_(1);
        }

        let mut addr =
            match vv2socketaddr(&vaddr) {
                Ok(addr) => addr,
                Err(e) => { return Ok(env.new_err(e)) },
            };

        let mut last_err = String::new();
        while let Some(ep) = addr.next() {
            match adr_mode {
                4 => { if !ep.is_ipv4() { continue } },
                6 => { if !ep.is_ipv6() { continue } },
                _ => (),
            }

            let stream =
                if argc == 2 {
                    let duration =
                        match env.arg(1).to_duration() {
                            Ok(dur) => dur,
                            Err(e)  => return Ok(e),
                        };
                    std::net::TcpStream::connect_timeout(&ep, duration)
                } else {
                    std::net::TcpStream::connect(&ep)
                };

            match stream {
                Ok(stream) => {
//                    stream.set_read_timeout(Some(std::time::Duration::from_secs(10)));
//                    stream.set_write_timeout(Some(std::time::Duration::from_secs(10)));
//
                    return Ok(VVal::Usr(Box::new(VTcpStream {
                        stream: Rc::new(RefCell::new(stream)),
                    })));
                },
                Err(e) => {
                    last_err = format!("Couldn't connect to {}: {}", ep, e);
                },
            }
        }

        Ok(env.new_err(last_err))
    }, Some(1), Some(2), false);

    st.fun("io:write", |env: &mut Env, _argc: usize| {
        let mut fd = env.arg(0);
        let data = env.arg(1);
        let offs = env.arg(2).i() as usize;

        Ok(fd.with_usr_ref(|vts: &mut VTcpStream| {
            use std::io::Write;

            data.with_bv_ref(|bytes| {
                if offs >= bytes.len() {
                    return env.new_err(
                        format!("std:io:write_some: bad buffer offset"));
                }

                let r = vts.stream.borrow_mut().write_all(&bytes[offs..]);
                match r {
                    Ok(()) => {
                        VVal::Int(bytes.len() as i64)
                    },
                    Err(e) => {
                        match e.kind() {
                            std::io::ErrorKind::Interrupted => { VVal::None },
                            _ =>
                                env.new_err(
                                    format!("std:io:write_some: {}", e)),
                        }
                    }
                }
            })
        }).unwrap_or(VVal::None))
    }, Some(2), Some(3), false);

    st.fun("io:write_some", |env: &mut Env, _argc: usize| {
        let mut fd = env.arg(0);
        let data = env.arg(1);
        let offs = env.arg(2).i() as usize;

        Ok(fd.with_usr_ref(|vts: &mut VTcpStream| {
            use std::io::Write;

            data.with_bv_ref(|bytes| {
                if offs >= bytes.len() {
                    return env.new_err(
                        format!("std:io:write_some: bad buffer offset"));
                }

                let r = vts.stream.borrow_mut().write(&bytes[offs..]);
                match r {
                    Ok(n) => {
                        if n == 0 {
                            VVal::opt_none()
                        } else {
                            VVal::opt(VVal::Int(n as i64))
                        }
                    },
                    Err(e) => {
                        match e.kind() {
                              std::io::ErrorKind::WouldBlock
                            | std::io::ErrorKind::TimedOut
                            | std::io::ErrorKind::Interrupted => {
                                VVal::None
                            },
                            _ =>
                                env.new_err(
                                    format!("std:io:write_some: {}", e)),
                        }
                    }
                }
            })
        }).unwrap_or(VVal::None))
    }, Some(2), Some(3), false);

    st.fun("io:read_some", |env: &mut Env, _argc: usize| {
        let mut fd = env.arg(0);
        Ok(fd.with_usr_ref(|vts: &mut VTcpStream| {
            use std::io::Read;

            let mut buf : [u8; 10] = [0; 10];
            let r = vts.stream.borrow_mut().read(&mut buf[..]);
            match r {
                Ok(n) => {
                    if n == 0 {
                        VVal::opt_none()
                    } else {
                        VVal::opt(VVal::new_byt(buf[0..n].to_vec()))
                    }
                },
                Err(e) => {
                    match e.kind() {
                          std::io::ErrorKind::WouldBlock
                        | std::io::ErrorKind::TimedOut
                        | std::io::ErrorKind::Interrupted => {
                            VVal::None
                        },
                        _ =>
                            env.new_err(
                                format!("std:io:read_some: {}", e)),
                    }
                }
            }
        }).unwrap_or(VVal::None))
    }, Some(1), Some(1), false);
}

