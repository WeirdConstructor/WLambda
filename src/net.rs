// Copyright (c) 2021 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.
use crate::compiler::*;
use crate::vval::*;
use std::rc::Rc;
use std::cell::RefCell;

use std::net::{SocketAddr, ToSocketAddrs};

fn vv2socketaddr(vv: VVal) -> Result<Box<dyn std::iter::Iterator<Item = SocketAddr>>, String> {
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
            match vv2socketaddr(vaddr) {
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
                    stream.set_read_timeout(Some(std::time::Duration::from_secs(10)));
                    stream.set_write_timeout(Some(std::time::Duration::from_secs(10)));

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

    st.fun("io:read_some", |env: &mut Env, argc: usize| {
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

