// Copyright (c) 2021-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.
use crate::compiler::*;
use crate::vval::*;
use std::cell::RefCell;
use std::rc::Rc;
use crate::stdlib::io_types::*;

use std::net::{SocketAddr, ToSocketAddrs, UdpSocket};

pub fn vv2socketaddr(vv: &VVal) -> Result<Box<dyn std::iter::Iterator<Item = SocketAddr>>, String> {
    let addr = if vv.is_pair() {
        let port = vv.v_i(1) as u16;
        vv.at(0).unwrap_or(VVal::None).with_s_ref(|host| (host, port).to_socket_addrs())
    } else {
        vv.with_s_ref(|addr| addr.to_socket_addrs())
    };

    match addr {
        Ok(it) => Ok(Box::new(it)),
        Err(e) => Err(format!("Couldn't resolve '{}': {}", vv.s(), e)),
    }
}

macro_rules! try_addresses {
    ($arg: expr, $env: expr, $ep: ident, $block: tt) => {
        let mut vaddr = $arg;
        let mut adr_mode = 0;

        if vaddr.is_pair() && vaddr.v_(0).is_sym() && vaddr.v_(0).with_s_ref(|s| s == "v4") {
            adr_mode = 4;
            vaddr = vaddr.v_(1);
        } else if vaddr.is_pair() && vaddr.v_(0).is_sym() && vaddr.v_(0).with_s_ref(|s| s == "v6") {
            adr_mode = 6;
            vaddr = vaddr.v_(1);
        }

        let mut addr = match vv2socketaddr(&vaddr) {
            Ok(addr) => addr,
            Err(e) => return Ok($env.new_err(e)),
        };

        while let Some($ep) = addr.next() {
            match adr_mode {
                4 => {
                    if !$ep.is_ipv4() {
                        continue;
                    }
                }
                6 => {
                    if !$ep.is_ipv6() {
                        continue;
                    }
                }
                _ => (),
            }

            $block
        }
    };
}

#[macro_export]
macro_rules! first_addr {
    ($arg: expr, $env: expr) => {{
        let vaddr = $arg;

        let mut addr = match crate::stdlib::net::vv2socketaddr(&vaddr) {
            Ok(addr) => addr,
            Err(e) => return Err($env.new_err(e)),
        };

        let first = addr.next();
        if first.is_none() {
            return Err(
                $env.new_err(format!("Couldn't get socket address from '{}'", vaddr.s_raw()))
            );
        }

        Ok(first.unwrap())
    }};
}

#[macro_export]
macro_rules! with_first_addr {
    ($arg: expr, $env: expr, $addr: ident, $block: tt) => {{
        let vaddr = $arg;

        let mut addr = match crate::stdlib::net::vv2socketaddr(&vaddr) {
            Ok(addr) => addr,
            Err(e) => return Ok($env.new_err(e)),
        };

        let first = addr.next();
        if first.is_none() {
            return Ok(
                $env.new_err(format!("Couldn't get socket address from '{}'", vaddr.s_raw()))
            );
        }
        let $addr = first.unwrap();
        $block
    }};
}

pub fn add_to_symtable(st: &mut SymbolTable) {
    st.fun(
        "net:tcp:listen",
        |env: &mut Env, _argc: usize| {
            let vaddr = env.arg(0);
            let fun = env.arg(1);

            with_first_addr!(env.arg(0), env, first, {
                match std::net::TcpListener::bind(first) {
                    Ok(listener) => {
                        #[cfg(feature = "socket2")]
                        let listener = {
                            use socket2::Socket;
                            let socket = Socket::from(listener);
                            let _ = socket.set_reuse_address(true);
                            #[cfg(unix)]
                            let _ = socket.set_reuse_port(true);
                            let listener: std::net::TcpListener = socket.into();
                            listener
                        };

                        for stream in listener.incoming() {
                            match stream {
                                Err(e) => {
                                    return Ok(env.new_err(format!("Couldn't get client: {}", e)));
                                }
                                Ok(stream) => {
                                    let stream = VVal::new_usr(VTcpStream {
                                        stream: Rc::new(RefCell::new(stream)),
                                    });
                                    env.push(stream);
                                    match fun.call_internal(env, 1) {
                                        Ok(_) => {
                                            env.popn(1);
                                        }
                                        Err(e) => {
                                            env.popn(1);
                                            return Err(e);
                                        }
                                    }
                                }
                            }
                        }

                        Ok(VVal::None)
                    }
                    Err(e) => Ok(env.new_err(format!(
                        "Couldn't create listener for '{}': {}",
                        vaddr.s_raw(),
                        e
                    ))),
                }
            })
        },
        Some(2),
        Some(2),
        false,
    );

    st.fun(
        "net:tcp:connect",
        |env: &mut Env, argc: usize| {
            let mut last_err = String::new();

            try_addresses!(env.arg(0), env, ep, {
                let stream = if argc == 2 {
                    let duration = match env.arg(1).to_duration() {
                        Ok(dur) => dur,
                        Err(e) => return Ok(e),
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
                    }
                    Err(e) => {
                        last_err = format!("Couldn't connect to {}: {}", ep, e);
                    }
                }
            });

            Ok(env.new_err(last_err))
        },
        Some(1),
        Some(2),
        false,
    );

    st.fun(
        "net:udp:new",
        |env: &mut Env, argc: usize| {
            let vaddr = env.arg(0);

            with_first_addr!(env.arg(0), env, first, {
                let socket = match UdpSocket::bind(first) {
                    Ok(socket) => socket,
                    Err(e) => {
                        return Ok(env.new_err(format!(
                            "Couldn't bind socket to address '{}': {}",
                            vaddr.s_raw(),
                            e
                        )));
                    }
                };

                if argc == 2 {
                    with_first_addr!(env.arg(1), env, caddr, {
                        if let Err(e) = socket.connect(caddr) {
                            return Ok(env.new_err(format!(
                                "Couldn't connect socket to address '{}': {}",
                                env.arg(1).s_raw(),
                                e
                            )));
                        }
                    })
                }

                Ok(VVal::Usr(Box::new(VUdpSocket { socket: Rc::new(RefCell::new(socket)) })))
            })
        },
        Some(1),
        Some(2),
        false,
    );

    st.fun(
        "net:udp:send",
        |env: &mut Env, argc: usize| {
            let mut socket = env.arg(0);
            let data = env.arg(1);
            let dest = env.arg(2);

            socket
                .with_usr_ref(|vus: &mut VUdpSocket| {
                    data.with_bv_ref(|bytes| {
                        if argc == 3 {
                            with_first_addr!(dest, env, daddr, {
                                match vus.socket.borrow_mut().send_to(bytes, daddr) {
                                    Ok(n) => Ok(VVal::Int(n as i64)),
                                    Err(e) => {
                                        Ok(env.new_err(format!("std:net:udp:send_to: {}", e)))
                                    }
                                }
                            })
                        } else {
                            match vus.socket.borrow_mut().send(bytes) {
                                Ok(n) => Ok(VVal::Int(n as i64)),
                                Err(e) => Ok(env.new_err(format!("std:net:udp:send: {}", e))),
                            }
                        }
                    })
                })
                .unwrap_or_else(|| {
                    Ok(env.new_err(format!(
                        "std:net:udp:send: First argument not a socket! {}",
                        socket.s()
                    )))
                })
        },
        Some(2),
        Some(3),
        false,
    );

    st.fun(
        "net:udp:recv",
        |env: &mut Env, argc: usize| {
            let mut socket = env.arg(0);
            let len = env.arg(1);

            let len = if argc < 2 || len.is_none() { 512 } else { len.i() as usize };

            Ok(socket
                .with_usr_ref(|vus: &mut VUdpSocket| {
                    let mut buf = vec![0; len];

                    match vus.socket.borrow_mut().recv_from(&mut buf) {
                        Ok((n, from_addr)) => {
                            buf.truncate(n);
                            VVal::pair(VVal::new_byt(buf), VVal::new_str_mv(from_addr.to_string()))
                        }
                        Err(e) => env.new_err(format!("std:net:udp:recv: {}", e)),
                    }
                })
                .unwrap_or_else(|| {
                    env.new_err(format!(
                        "std:net:udp:recv: First argument not a socket! {}",
                        socket.s()
                    ))
                }))
        },
        Some(1),
        Some(2),
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

            socket
                .with_usr_ref(|vts: &mut VTcpStream| {
                    let _ = vts.stream.borrow_mut().set_read_timeout(read_t);
                    let _ = vts.stream.borrow_mut().set_write_timeout(write_t);
                    Ok(VVal::Bol(true))
                })
                .unwrap_or_else(|| {
                    Ok(env.new_err(format!(
                        "std:net:tcp:set_timeouts: First argument not a socket! {}",
                        socket.s()
                    )))
                })
        },
        Some(2),
        Some(3),
        false,
    );

    io_add_to_symtable(st);
}
