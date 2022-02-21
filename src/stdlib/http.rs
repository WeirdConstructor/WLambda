// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#[cfg(feature="reqwest")]
use crate::VVal;
#[allow(unused_imports)]
use crate::{Env, StackAction};
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[cfg(feature="reqwest")]
use reqwest::blocking::{Client, Response};
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
}

#[cfg(feature="reqwest")]
impl std::fmt::Display for VHttpClError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VHttpClError::ReqwestError(e)    => write!(f, "{}", e),
            VHttpClError::InvalidHeaderName  => write!(f, "InvalidHeaderName"),
            VHttpClError::InvalidHeaderValue => write!(f, "InvalidHeaderValue"),
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

    pub fn get(&self, url: &str, headers: VVal) -> Result<Response, VHttpClError> {
        let mut rq_build = self.cl.borrow_mut().get(url);

        if headers.is_some() {
            use std::convert::TryInto;
            for (v, k) in headers.iter() {
                let name  : HeaderName =
                    k.unwrap_or(VVal::None)
                     .s_raw()
                     .try_into()
                     .map_err(|_| VHttpClError::InvalidHeaderName)?;
                let value : HeaderValue =
                    v.s_raw()
                      .try_into()
                      .map_err(|_| VHttpClError::InvalidHeaderValue)?;

                rq_build = rq_build.header(name, value);
            }
        }

        rq_build.send().map_err(|e| VHttpClError::ReqwestError(e))
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
        cl.with_usr_ref(|cl: &mut VHttpClient| {
            url.with_s_ref(|url| {
                match cl.get(url, headers) {
                    Err(e) => {
                        Ok(env.new_err(format!("HTTP Request Error: {}", e)))
                    },
                    Ok(resp) => {
                        let headers = VVal::map();
                        for (key, value) in resp.headers().iter() {
                            let res =
                                if let Ok(s) = value.to_str() {
                                    headers.set_key_str(key.as_str(), VVal::new_str(s))
                                } else {
                                    headers.set_key_str(
                                        key.as_str(),
                                        VVal::new_byt(
                                            value.as_bytes().as_ref().to_vec()))
                                };

                            if let Err(e) = res {
                                return Ok(
                                    env.new_err(
                                        format!("HTTP Header Set Error: {}", e)));
                            }
                        }

                        let status = resp.status();

                        match resp.bytes() {
                            Err(e) => {
                                Ok(env.new_err(
                                    format!("HTTP Body Error: {}", e)))
                            },
                            Ok(body) => {

                                Ok(VVal::map3(
                                    "body",    VVal::new_byt(body.as_ref().to_vec()),
                                    "status",  VVal::Int(status.as_u16() as i64),
                                    "headers", headers
                                ))
                            },
                        }
                    },
                }
            })
        }).unwrap_or_else(||
            Ok(env.new_err(format!(
                "http:get: First argument not a HttpClient handle! {}",
                cl.s()))))

    }, Some(2), Some(3), false);
}
