// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::compiler::*;
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[cfg(feature = "reqwest")]
use crate::VVal;
#[allow(unused_imports)]
use crate::{Env, StackAction};
#[cfg(feature = "reqwest")]
use reqwest::blocking::Client;
#[cfg(feature = "reqwest")]
use reqwest::header::{HeaderName, HeaderValue};
#[cfg(feature = "reqwest")]
use std::cell::RefCell;
#[cfg(feature = "reqwest")]
use std::rc::Rc;

#[cfg(feature = "rouille")]
use super::PendingResult;
#[cfg(feature = "rouille")]
use rouille::{Response, Server};
#[cfg(feature = "rouille")]
use std::sync::mpsc::{Receiver, Sender};
#[cfg(feature = "rouille")]
use std::sync::{Arc, Mutex};
#[cfg(feature = "rouille")]
use std::thread::JoinHandle;

#[cfg(feature = "reqwest")]
#[derive(Debug, Clone)]
struct VHttpClient {
    cl: Rc<RefCell<Client>>,
}

#[cfg(feature = "reqwest")]
#[derive(Debug)]
enum VHttpClError {
    ReqwestError(reqwest::Error),
    InvalidHeaderName,
    InvalidHeaderValue,
    // InvalidQueryValue,
}

#[cfg(feature = "reqwest")]
impl std::fmt::Display for VHttpClError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VHttpClError::ReqwestError(e) => write!(f, "{}", e),
            VHttpClError::InvalidHeaderName => write!(f, "InvalidHeaderName"),
            VHttpClError::InvalidHeaderValue => write!(f, "InvalidHeaderValue"),
            // VHttpClError::InvalidQueryValue  => write!(f, "InvalidQueryValue"),
        }
    }
}

#[cfg(feature = "reqwest")]
impl VHttpClient {
    pub fn new() -> Self {
        Self { cl: Rc::new(RefCell::new(Client::new())) }
    }

    pub fn request(
        &self,
        method: &str,
        url: &str,
        body: Option<&[u8]>,
        headers: VVal,
        dump_output: &mut VVal,
    ) -> Result<VVal, VHttpClError> {
        let method =
            reqwest::Method::from_bytes(method.as_bytes()).unwrap_or(reqwest::Method::POST);

        let mut rq_build = self.cl.borrow_mut().request(method, url);

        let mut dump = false;

        if headers.is_some() {
            for (v, k) in headers.iter() {
                let name = k.unwrap_or(VVal::None).s_raw();

                if name.starts_with("@") {
                    match &name.as_str()[..] {
                        "@basic_auth" => {
                            let user = v.v_s_raw(0);

                            let mut password = None;
                            if v.v_(1).is_some() {
                                password = Some(v.v_s_raw(1));
                            }

                            rq_build = rq_build.basic_auth(user, password);
                        }
                        "@bearer_auth" => {
                            let token = v.s_raw();

                            rq_build = rq_build.bearer_auth(token);
                        }
                        "@timeout" => {
                            if let Ok(dur) = v.to_duration() {
                                rq_build = rq_build.timeout(dur);
                            }
                        }
                        "@query" => {
                            for (v, k) in v.iter() {
                                let name = k.unwrap_or(VVal::None);
                                rq_build = name.with_s_ref(|name| {
                                    v.with_s_ref(|value| rq_build.query(&[[name, value]]))
                                });
                            }
                        }
                        "@dump" => {
                            dump = true;
                        }
                        _ => {
                            return Err(VHttpClError::InvalidHeaderName);
                        }
                    }
                } else {
                    let name: HeaderName =
                        name.try_into().map_err(|_| VHttpClError::InvalidHeaderName)?;
                    let value: HeaderValue =
                        v.s_raw().try_into().map_err(|_| VHttpClError::InvalidHeaderValue)?;

                    rq_build = rq_build.header(name, value);
                }
            }
        }

        if let Some(body) = body {
            rq_build = rq_build.body(body.to_vec());
        }

        if dump {
            let rq = rq_build.build().map_err(|e| VHttpClError::ReqwestError(e))?;
            *dump_output = req2vv(&rq);
            let resp =
                self.cl.borrow_mut().execute(rq).map_err(|e| VHttpClError::ReqwestError(e))?;

            let resp = resp2vv(resp);
            *dump_output = VVal::pair((*dump_output).clone(), resp.clone());

            Ok(resp)
        } else {
            Ok(resp2vv(rq_build.send().map_err(|e| VHttpClError::ReqwestError(e))?))
        }
    }
}

#[cfg(feature = "reqwest")]
impl VValUserData for VHttpClient {
    fn s(&self) -> String {
        format!("$<HttpClient>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

#[cfg(feature = "reqwest")]
pub fn req2vv(req: &reqwest::blocking::Request) -> VVal {
    let headers = VVal::map();
    for (key, value) in req.headers().iter() {
        let _ = if let Ok(s) = value.to_str() {
            headers.set_key_str(key.as_str(), VVal::new_str(s))
        } else {
            headers.set_key_str(key.as_str(), VVal::new_byt(value.as_bytes().as_ref().to_vec()))
        };
    }

    let out = VVal::map();

    let _ = out.set_key_str("method", VVal::new_str(req.method().as_str()));
    let _ = out.set_key_str("url", VVal::new_str(req.url().as_str()));
    let _ = out.set_key_str("headers", headers);
    if let Some(body) = req.body() {
        if let Some(bytes) = body.as_bytes() {
            let _ = out.set_key_str("body", VVal::new_byt(bytes.to_vec()));
        }
    }

    out
}

#[cfg(feature = "reqwest")]
pub fn resp2vv(resp: reqwest::blocking::Response) -> VVal {
    let headers = VVal::map();
    for (key, value) in resp.headers().iter() {
        let _ = if let Ok(s) = value.to_str() {
            headers.set_key_str(key.as_str(), VVal::new_str(s))
        } else {
            headers.set_key_str(key.as_str(), VVal::new_byt(value.as_bytes().as_ref().to_vec()))
        };
    }

    let status = resp.status();

    match resp.bytes() {
        Err(e) => VVal::err_msg(&format!("HTTP Body Error: {}", e)),
        Ok(body) => {
            let map = VVal::map3(
                "body",
                VVal::new_byt(body.as_ref().to_vec()),
                "status",
                VVal::Int(status.as_u16() as i64),
                "headers",
                headers,
            );
            map.set_key_str(
                "reason",
                VVal::new_str(status.canonical_reason().unwrap_or_else(|| "")),
            )
            .expect("Unshared map to not generate borrowing errors");
            map
        }
    }
}

#[cfg(feature = "reqwest")]
fn add_dump(out: VVal, dump: VVal) -> VVal {
    if dump.is_some() {
        VVal::pair(out, dump)
    } else {
        out
    }
}

#[cfg(feature = "rouille")]
#[derive(Debug, Clone)]
struct PendingHttpRequest {
    url: String,
    method: String,
    body: Vec<u8>,
}

#[cfg(feature = "rouille")]
struct HttpServer {
    request_receiver: Receiver<(PendingHttpRequest, Arc<PendingResult>)>,
    quit_sender: Sender<()>,
    thread_join: Option<JoinHandle<()>>,
}

#[cfg(feature = "rouille")]
impl HttpServer {
    pub fn start(listen_ip_port: &str) -> Result<Self, String> {
        let (sender, receiver) = std::sync::mpsc::channel();
        let sender = Arc::new(Mutex::new(sender));

        match Server::new(listen_ip_port, move |request| {
            if request.url() == "/favicon.ico" {
                Response::from_data(
                    "image/x-icon",
                    include_bytes!("../../res/wlambda_logo_60.ico").to_vec(),
                )
            } else {
                use std::io::Read;

                let pending = Arc::new(PendingResult::new());

                let mut data = match request.data() {
                    Some(data) => data,
                    None => {
                        return Response::text("Failed to get body data").with_status_code(500);
                    }
                };

                let mut buf = Vec::new();
                match data.read_to_end(&mut buf) {
                    Ok(_) => (),
                    Err(_) => return Response::text("Failed to read body").with_status_code(500),
                };
                let p_request = PendingHttpRequest {
                    body: buf,
                    method: request.method().to_string(),
                    url: request.url().to_string(),
                };

                match sender.lock() {
                    Ok(locked_sender) => match locked_sender.send((p_request, pending.clone())) {
                        Ok(_) => (),
                        Err(e) => {
                            return Response::text(format!(
                                "Failed to send request to WLambda: {}",
                                e
                            ))
                            .with_status_code(500)
                        }
                    },
                    Err(e) => {
                        return Response::text(format!("Internal Mutex Error: {}", e))
                            .with_status_code(500)
                    }
                }

                match pending.wait() {
                    Ok(res) => {
                        let resp_type = res.v_s_raw(0);
                        match &resp_type[..] {
                            "file" => {
                                let prefix = res.v_(1).s_raw();
                                let path = res.v_(2).s_raw();

                                if let Some(req) = request.remove_prefix(&prefix) {
                                    rouille::match_assets(&req, &path)
                                } else {
                                    Response::text(format!("Invalid file response: {}", res.s()))
                                        .with_status_code(500)
                                }
                            }
                            "redirect" => Response::redirect_303(res.v_(1).s_raw()),
                            "data" => res.v_(1).with_s_ref(|conttype| {
                                res.v_(2)
                                    .with_bv_ref(|bv| Response::from_data(conttype.to_string(), bv))
                            }),
                            "error" => Response::text(res.v_s_raw(2)).with_status_code(res.v_i(1) as u16),
                            "internal_error" => Response::text(res.v_s_raw(1)).with_status_code(500),
                            "no_file" => Response::text("Not Found").with_status_code(404),
                            _ => Response::text(format!("Unknown Response Type: {}", res.s())),
                        }
                    }
                    Err(e) => Response::text(format!("Pending request not answered: {}", e))
                        .with_status_code(500),
                }
            }
        }) {
            Ok(server) => {
                let (handle, sender) = server.stoppable();

                Ok(Self {
                    request_receiver: receiver,
                    thread_join: Some(handle),
                    quit_sender: sender,
                })
            }
            Err(e) => Err(format!("{}", e)),
        }
    }
}

#[cfg(feature = "rouille")]
impl Drop for HttpServer {
    #[allow(unused_must_use)]
    fn drop(&mut self) {
        if let Some(handle) = self.thread_join.take() {
            self.quit_sender.send(()).expect("Sending http:server end signal works on drop");
            handle.join().expect("Joining the http:server thread works on drop");
        }
    }
}

#[cfg(feature = "rouille")]
#[derive(Clone)]
struct VHttpServer {
    srv: Rc<RefCell<HttpServer>>,
}

#[cfg(feature = "rouille")]
macro_rules! assert_arg_count {
    ($self: expr, $argv: expr, $count: expr, $function: expr, $env: ident) => {
        if $argv.len() != $count {
            return Err(StackAction::panic_str(
                format!("{}.{} expects {} arguments", $self, $function, $count),
                None,
                $env.argv(),
            ));
        }
    };
}

#[cfg(feature = "rouille")]
fn handle_request(
    env: &mut Env,
    fun: VVal,
    pend_req: PendingHttpRequest,
    pending_response: Arc<PendingResult>,
) -> Result<VVal, StackAction> {
    let req = VVal::map3(
        "url",
        VVal::new_str_mv(pend_req.url),
        "method",
        VVal::new_str_mv(pend_req.method),
        "body",
        VVal::new_byt(pend_req.body),
    );

    match fun.call(env, &[req.clone()]) {
        Ok(val) => match pending_response.send(&val) {
            Ok(()) => Ok(req),
            Err(e) => {
                Ok(env.new_err(format!("http:server:try_respond error on responding: {}", e)))
            }
        },
        Err(StackAction::Return(val)) => match pending_response.send(&val.1) {
            Ok(()) => Ok(req),
            Err(e) => {
                Ok(env.new_err(format!("http:server:try_respond error on responding: {}", e)))
            }
        },
        Err(StackAction::Break(val)) => {
            let _ = pending_response.send(&VVal::None);
            Ok(val.as_ref().clone())
        }
        Err(StackAction::Next) => {
            let _ = pending_response.send(&VVal::None);
            Ok(VVal::None)
        }
        Err(panic) => {
            let _ = pending_response.send(
                &VVal::vec2(
                    VVal::new_sym("internal_error"),
                    VVal::new_str_mv(format!("Internal Server Error:\n{}", panic))));
            Ok(VVal::None)
        }
    }
}

#[cfg(feature = "rouille")]
impl VValUserData for VHttpServer {
    fn s(&self) -> String {
        format!("$<HttpServer>")
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();

        match key {
            "wait_respond" => {
                assert_arg_count!(
                    "$<HttpServer>",
                    argv,
                    1,
                    "wait_respond[responder_function]",
                    env
                );
                match self.srv.borrow_mut().request_receiver.recv() {
                    Ok((pend_req, pending_response)) => {
                        return handle_request(env, argv.v_(0), pend_req, pending_response);
                    }
                    Err(e) => Err(StackAction::panic_str(
                        format!("$<HttpServer> error waiting for request: {}", e),
                        None,
                        env.argv(),
                    )),
                }
            }
            "timeout_respond" => {
                assert_arg_count!(
                    "$<HttpServer>",
                    argv,
                    2,
                    "timeout_respond[duration, responder_function]",
                    env
                );
                let dur = match argv.v_(0).to_duration() {
                    Ok(dur) => dur,
                    Err(e) => return Ok(e),
                };

                match self.srv.borrow_mut().request_receiver.recv_timeout(dur) {
                    Ok((pend_req, pending_response)) => {
                        return handle_request(env, argv.v_(1), pend_req, pending_response);
                    }
                    Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                        Err(StackAction::panic_str(
                            format!("$<HttpServer> server request thread not running anymore!"),
                            None,
                            env.argv(),
                        ))
                    }
                    Err(std::sync::mpsc::RecvTimeoutError::Timeout) => Ok(VVal::None),
                }
            }
            "try_respond" => {
                assert_arg_count!("$<HttpServer>", argv, 1, "try_respond[responder_function]", env);
                match self.srv.borrow_mut().request_receiver.try_recv() {
                    Ok((pend_req, pending_response)) => {
                        return handle_request(env, argv.v_(0), pend_req, pending_response);
                    }
                    Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                        Err(StackAction::panic_str(
                            format!("$<HttpServer> server request thread not running anymore!"),
                            None,
                            env.argv(),
                        ))
                    }
                    Err(std::sync::mpsc::TryRecvError::Empty) => Ok(VVal::None),
                }
            }
            _ => Err(StackAction::panic_str(
                format!("$<HttpServer> unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }

    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "reqwest")]
    st.fun(
        "http:client:new",
        |_env: &mut Env, _argc: usize| Ok(VVal::new_usr(VHttpClient::new())),
        Some(0),
        Some(0),
        false,
    );

    #[cfg(feature = "rouille")]
    st.fun(
        "http:server:new",
        |env: &mut Env, _argc: usize| match HttpServer::start(&env.arg(0).s_raw()) {
            Ok(srv) => Ok(VVal::new_usr(VHttpServer { srv: Rc::new(RefCell::new(srv)) })),
            Err(e) => Ok(env.new_err(format!("http:server:new Error: {}", e))),
        },
        Some(1),
        Some(1),
        false,
    );

    #[cfg(feature = "reqwest")]
    st.fun(
        "http:get",
        |env: &mut Env, _argc: usize| {
            let mut cl = env.arg(0);
            let url = env.arg(1);
            let headers = env.arg(2);

            let mut dump_out = VVal::None;

            cl.with_usr_ref(|cl: &mut VHttpClient| {
                url.with_s_ref(|url| match cl.request("GET", url, None, headers, &mut dump_out) {
                    Err(e) => Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                    Ok(resp) => Ok(add_dump(resp, dump_out)),
                })
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "http:get: First argument not a HttpClient handle! {}",
                    cl.s()
                )))
            })
        },
        Some(2),
        Some(3),
        false,
    );

    #[cfg(feature = "reqwest")]
    st.fun(
        "http:post",
        |env: &mut Env, _argc: usize| {
            let mut cl = env.arg(0);
            let url = env.arg(1);
            let body = env.arg(2);
            let headers = env.arg(3);

            let mut dump_out = VVal::None;

            cl.with_usr_ref(|cl: &mut VHttpClient| {
                body.with_bv_ref(|body| {
                    url.with_s_ref(|url| {
                        match cl.request("POST", url, Some(body), headers, &mut dump_out) {
                            Err(e) => Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                            Ok(resp) => Ok(add_dump(resp, dump_out)),
                        }
                    })
                })
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "http:post: First argument not a HttpClient handle! {}",
                    cl.s()
                )))
            })
        },
        Some(3),
        Some(4),
        false,
    );

    #[cfg(feature = "reqwest")]
    st.fun(
        "http:request",
        |env: &mut Env, _argc: usize| {
            let mut cl = env.arg(0);
            let method = env.arg(1);
            let url = env.arg(2);
            let body = env.arg(3);
            let headers = env.arg(4);

            let mut dump_out = VVal::None;

            cl.with_usr_ref(|cl: &mut VHttpClient| {
                method.with_s_ref(|method| {
                    if body.is_none() {
                        url.with_s_ref(|url| {
                            match cl.request(method, url, None, headers, &mut dump_out) {
                                Err(e) => Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                                Ok(resp) => Ok(add_dump(resp, dump_out)),
                            }
                        })
                    } else {
                        body.with_bv_ref(|body| {
                            url.with_s_ref(|url| {
                                match cl.request(method, url, Some(body), headers, &mut dump_out) {
                                    Err(e) => Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                                    Ok(resp) => Ok(add_dump(resp, dump_out)),
                                }
                            })
                        })
                    }
                })
            })
            .unwrap_or_else(|| {
                Ok(env.new_err(format!(
                    "http:request: First argument not a HttpClient handle! {}",
                    cl.s()
                )))
            })
        },
        Some(3),
        Some(5),
        false,
    );
}
