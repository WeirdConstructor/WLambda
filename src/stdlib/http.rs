// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#[cfg(feature="reqwest")]
use crate::VVal;
#[allow(unused_imports)]
use crate::{Env, StackAction};
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[cfg(feature="reqwest")]
use reqwest::blocking::Client;
#[cfg(feature="reqwest")]
use reqwest::header::{HeaderName, HeaderValue};
use crate::compiler::*;
#[cfg(feature="reqwest")]
use std::rc::Rc;
#[cfg(feature="reqwest")]
use std::cell::RefCell;

#[cfg(feature="reqwest")]
#[derive(Debug, Clone)]
struct VHttpClient {
    cl: Rc<RefCell<Client>>,
}

#[cfg(feature="reqwest")]
#[derive(Debug)]
enum VHttpClError {
    ReqwestError(reqwest::Error),
    InvalidHeaderName,
    InvalidHeaderValue,
    // InvalidQueryValue,
}

#[cfg(feature="reqwest")]
impl std::fmt::Display for VHttpClError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VHttpClError::ReqwestError(e)    => write!(f, "{}", e),
            VHttpClError::InvalidHeaderName  => write!(f, "InvalidHeaderName"),
            VHttpClError::InvalidHeaderValue => write!(f, "InvalidHeaderValue"),
            // VHttpClError::InvalidQueryValue  => write!(f, "InvalidQueryValue"),
        }
    }
}

#[cfg(feature="reqwest")]
impl VHttpClient {
    pub fn new() -> Self {
        Self {
            cl: Rc::new(RefCell::new(Client::new())),
        }
    }

    pub fn request(
        &self, method: &str, url: &str, body: Option<&[u8]>,
        headers: VVal, dump_output: &mut VVal)
        -> Result<VVal, VHttpClError>
    {
        let method =
            reqwest::Method::from_bytes(method.as_bytes())
                .unwrap_or(reqwest::Method::POST);

        let mut rq_build =
            self.cl.borrow_mut()
                .request(method, url);

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
                        },
                        "@bearer_auth" => {
                            let token = v.s_raw();

                            rq_build = rq_build.bearer_auth(token);
                        },
                        "@timeout" => {
                            if let Ok(dur) = v.to_duration() {
                                rq_build = rq_build.timeout(dur);
                            }
                        },
                        "@query" => {
                            for (v, k) in v.iter() {
                                let name = k.unwrap_or(VVal::None);
                                rq_build =
                                    name.with_s_ref(|name|
                                        v.with_s_ref(|value| {
                                            rq_build.query(&[[name, value]])
                                        }));
                            }
                        },
                        "@dump" => {
                            dump = true;
                        },
                        _ => {
                            return Err(VHttpClError::InvalidHeaderName);
                        },
                    }
                } else {
                    let name : HeaderName =
                        name.try_into()
                            .map_err(|_| VHttpClError::InvalidHeaderName)?;
                    let value : HeaderValue =
                        v.s_raw()
                          .try_into()
                          .map_err(|_| VHttpClError::InvalidHeaderValue)?;

                    rq_build = rq_build.header(name, value);
                }
            }
        }

        if let Some(body) = body {
            rq_build = rq_build.body(body.to_vec());
        }

        if dump {
            let rq =
                rq_build.build().map_err(|e| VHttpClError::ReqwestError(e))?;
            *dump_output = req2vv(&rq);
            let resp =
                self.cl.borrow_mut().execute(rq)
                    .map_err(|e| VHttpClError::ReqwestError(e))?;

            let resp = resp2vv(resp);
            *dump_output = VVal::pair((*dump_output).clone(), resp.clone());

            Ok(resp)
        } else {
            Ok(resp2vv(rq_build.send().map_err(|e| VHttpClError::ReqwestError(e))?))
        }
    }
}

#[cfg(feature="reqwest")]
impl VValUserData for VHttpClient {
    fn s(&self) -> String {
        format!("$<HttpClient>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
}

#[cfg(feature="reqwest")]
pub fn req2vv(req: &reqwest::blocking::Request) -> VVal {
    let headers = VVal::map();
    for (key, value) in req.headers().iter() {
        let _ =
            if let Ok(s) = value.to_str() {
                headers.set_key_str(
                    key.as_str(), VVal::new_str(s))
            } else {
                headers.set_key_str(
                    key.as_str(),
                    VVal::new_byt(
                        value.as_bytes().as_ref().to_vec()))
            };
    }

    let out = VVal::map();

    let _ = out.set_key_str("method",  VVal::new_str(req.method().as_str()));
    let _ = out.set_key_str("url",     VVal::new_str(req.url().as_str()));
    let _ = out.set_key_str("headers", headers);
    if let Some(body) = req.body() {
        if let Some(bytes) = body.as_bytes() {
            let _ = out.set_key_str("body", VVal::new_byt(bytes.to_vec()));
        }
    }

    out
}

#[cfg(feature="reqwest")]
pub fn resp2vv(resp: reqwest::blocking::Response) -> VVal {
    let headers = VVal::map();
    for (key, value) in resp.headers().iter() {
        let _ =
            if let Ok(s) = value.to_str() {
                headers.set_key_str(
                    key.as_str(), VVal::new_str(s))
            } else {
                headers.set_key_str(
                    key.as_str(),
                    VVal::new_byt(
                        value.as_bytes().as_ref().to_vec()))
            };
    }

    let status = resp.status();

    match resp.bytes() {
        Err(e) => {
            VVal::err_msg(&format!("HTTP Body Error: {}", e))
        },
        Ok(body) => {
            let map =
                VVal::map3(
                    "body",    VVal::new_byt(body.as_ref().to_vec()),
                    "status",  VVal::Int(status.as_u16() as i64),
                    "headers", headers);
            map.set_key_str("reason",
                VVal::new_str(
                    status
                        .canonical_reason()
                        .unwrap_or_else(|| "")))
                .expect("Unshared map to not generate borrowing errors");
            map
        },
    }
}

#[cfg(feature="reqwest")]
fn add_dump(out: VVal, dump: VVal) -> VVal {
    if dump.is_some() {
        VVal::pair(out, dump)
    } else {
        out
    }
}


#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature="reqwest")]
    st.fun("http:client:new", |_env: &mut Env, _argc: usize| {
        Ok(VVal::new_usr(VHttpClient::new()))
    }, Some(0), Some(0), false);

    #[cfg(feature="reqwest")]
    st.fun("http:get", |env: &mut Env, _argc: usize| {
        let mut cl  = env.arg(0);
        let url     = env.arg(1);
        let headers = env.arg(2);

        let mut dump_out = VVal::None;

        cl.with_usr_ref(|cl: &mut VHttpClient| {
            url.with_s_ref(|url| {
                match cl.request("GET", url, None, headers, &mut dump_out) {
                    Err(e) =>
                        Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                    Ok(resp) => { Ok(add_dump(resp, dump_out)) },
                }
            })
        }).unwrap_or_else(||
            Ok(env.new_err(format!(
                "http:get: First argument not a HttpClient handle! {}",
                cl.s()))))

    }, Some(2), Some(3), false);

    #[cfg(feature="reqwest")]
    st.fun("http:post", |env: &mut Env, _argc: usize| {
        let mut cl  = env.arg(0);
        let url     = env.arg(1);
        let body    = env.arg(2);
        let headers = env.arg(3);

        let mut dump_out = VVal::None;

        cl.with_usr_ref(|cl: &mut VHttpClient| {
            body.with_bv_ref(|body| {
                url.with_s_ref(|url| {
                    match cl.request("POST", url, Some(body), headers, &mut dump_out) {
                        Err(e) =>
                            Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                        Ok(resp) => { Ok(add_dump(resp, dump_out)) },
                    }
                })
            })
        }).unwrap_or_else(||
            Ok(env.new_err(format!(
                "http:post: First argument not a HttpClient handle! {}",
                cl.s()))))

    }, Some(3), Some(4), false);

    #[cfg(feature="reqwest")]
    st.fun("http:request", |env: &mut Env, _argc: usize| {
        let mut cl  = env.arg(0);
        let method  = env.arg(1);
        let url     = env.arg(2);
        let body    = env.arg(3);
        let headers = env.arg(4);

        let mut dump_out = VVal::None;

        cl.with_usr_ref(|cl: &mut VHttpClient| {
            method.with_s_ref(|method| {
                if body.is_none() {
                    url.with_s_ref(|url| {
                        match cl.request(method, url, None, headers, &mut dump_out) {
                            Err(e) =>
                                Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                            Ok(resp) => { Ok(add_dump(resp, dump_out)) },
                        }
                    })
                } else {
                    body.with_bv_ref(|body| {
                        url.with_s_ref(|url| {
                            match cl.request(method, url, Some(body), headers, &mut dump_out) {
                                Err(e) =>
                                    Ok(env.new_err(format!("HTTP Request Error: {}", e))),
                                Ok(resp) => { Ok(add_dump(resp, dump_out)) },
                            }
                        })
                    })
                }
            })
        }).unwrap_or_else(||
            Ok(env.new_err(format!(
                "http:request: First argument not a HttpClient handle! {}",
                cl.s()))))

    }, Some(3), Some(5), false);
}
