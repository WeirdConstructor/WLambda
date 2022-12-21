// Copyright (c) 2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#![allow(unused_macros)]

#[allow(unused_imports)]
use crate::compiler::*;
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[allow(unused_imports)]
use crate::{threads::AVal, Env, StackAction, VVal};
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use std::collections::HashMap;
#[allow(unused_imports)]
use std::rc::Rc;
#[allow(unused_imports)]
use std::sync::{Arc, Condvar, Mutex};

#[cfg(feature = "odbc")]
use odbc_api::{
    buffers::Indicator, buffers::TextRowSet, handles::DataType, Connection, Cursor, Environment,
    ResultSetMetadata,
};

struct PendingResult {
    lock: Mutex<(bool, AVal)>,
    cvar: Condvar,
}

impl PendingResult {
    pub fn new() -> Self {
        Self { lock: Mutex::new((true, AVal::None)), cvar: Condvar::new() }
    }

    pub fn send(&self, res: &VVal) -> Result<(), String> {
        match self.lock.lock() {
            Ok(mut pend) => {
                pend.0 = false;
                pend.1 = AVal::from_vval(&res);
                self.cvar.notify_one();
                Ok(())
            }
            Err(e) => Err(format!("ODBC thread send error: {}", e)),
        }
    }

    pub fn wait(&self) -> Result<VVal, String> {
        let lock = match self.lock.lock() {
            Ok(lock) => lock,
            Err(e) => {
                return Err(format!("ODBC thread lock error: {}", e));
            }
        };

        match self.cvar.wait_while(lock, |pend| pend.0) {
            Ok(pend) => Ok(pend.1.to_vval()),
            Err(e) => {
                return Err(format!("ODBC thread wait error: {}", e));
            }
        }
    }
}

enum OdbcThreadRequest {
    Connect(String),
    Retrieve(String),
}

struct OdbcHandle {
    //    sender: std::sync::mpsc::Sender<OdbcThreadRequest>,
    //    connection: Option<HDbc>,
}

impl OdbcHandle {
    pub fn connect(con_str: String) -> Result<Self, String> {
        let res = Arc::new(PendingResult::new());
        let res_thrd = res.clone();

        std::thread::spawn(move || {
            let odbc_env = Environment::new();
            let env = match odbc_env {
                Ok(env) => env,
                Err(e) => {
                    res_thrd.send(&VVal::err_msg(&format!("ODBC environment setup error: {}", e)));
                    return;
                }
            };

            let con = match env.connect_with_connection_string(&con_str) {
                Err(e) => {
                    res_thrd.send(&VVal::err_msg(&format!(
                        "ODBC connect error (string={}): {}",
                        con_str, e
                    )));
                    return;
                }
                Ok(con) => con,
            };

            let res: Result<VVal, String> = match con.prepare("SELECT * FROM system") {
                Ok(mut prep) => {
                    match prep.execute(()) {
                        Ok(Some(mut cursor)) => {
                            let cnt = match cursor.num_result_cols() {
                                Ok(cnt) => cnt,
                                Err(e) => {
                                    res_thrd.send(&VVal::err_msg(&format!(
                                        "ODBC num_result_cols error: {}",
                                        e
                                    )));
                                    return;
                                }
                            };
                            println!("RESCOLS: {}", cnt);
                            let mut names = vec![];
                            for i in 1..=cnt {
                                let r = match cursor.col_name(i as u16) {
                                    Ok(r) => r,
                                    Err(e) => {
                                        res_thrd.send(&VVal::err_msg(&format!(
                                            "ODBC col_data_type error: {}",
                                            e
                                        )));
                                        return;
                                    }
                                };

                                names.push(r.to_string());
                                println!("COLNAME: {}={}", i, r);
                            }
                            let mut types = vec![];
                            for i in 1..=cnt {
                                let r = match cursor.col_data_type(i as u16) {
                                    Ok(r) => r,
                                    Err(e) => {
                                        res_thrd.send(&VVal::err_msg(&format!(
                                            "ODBC col_data_type error: {}",
                                            e
                                        )));
                                        return;
                                    }
                                };
                                types.push(r);
                                println!("COLDT: {}={:?}", i, r);
                            }
                            let lst = VVal::vec();

                            let batch_size = 1000;
                            let mut row_set =
                                match TextRowSet::for_cursor(batch_size, &mut cursor, Some(4096)) {
                                    Ok(rs) => rs,
                                    Err(e) => {
                                        res_thrd.send(&VVal::err_msg(&format!(
                                            "ODBC buffer prep error: {}",
                                            e
                                        )));
                                        return;
                                    }
                                };
                            let mut row_set_cursor = match cursor.bind_buffer(&mut row_set) {
                                Ok(cur) => cur,
                                Err(e) => {
                                    res_thrd.send(&VVal::err_msg(&format!(
                                        "ODBC bind_buffer error: {}",
                                        e
                                    )));
                                    return;
                                }
                            };

                            let mut batch = row_set_cursor.fetch();
                            let mut in_batch = match batch {
                                Ok(batch) => batch,
                                Err(e) => {
                                    res_thrd
                                        .send(&VVal::err_msg(&format!("ODBC fetch error: {}", e)));
                                    return;
                                }
                            };

                            while let Some(batch) = in_batch {
                                for row_index in 0..batch.num_rows() {
                                    let row = VVal::map();

                                    for col_i in 0..batch.num_cols() {
                                        let data = batch.at(col_i, row_index).unwrap_or(&[]);

                                        let v = match batch.indicator_at(col_i, row_index) {
                                            Indicator::Null => VVal::None,
                                            _ => {
                                                if let Ok(s) = std::str::from_utf8(data) {
                                                    match types.get(col_i) {
                                                        Some(&DataType::Integer) => VVal::Int(s.parse::<i64>().unwrap_or(0)),
                                                        Some(&DataType::SmallInt) => VVal::Int(s.parse::<i64>().unwrap_or(0)),
                                                        Some(&DataType::TinyInt) => VVal::Int(s.parse::<i64>().unwrap_or(0)),
                                                        Some(&DataType::Bit) => VVal::Bol(s.parse::<i64>().unwrap_or(0) == 1),
                                                        Some(&DataType::Varbinary { .. }) => VVal::new_byt(data.to_vec()),
                                                        Some(&DataType::LongVarbinary { .. }) => VVal::new_byt(data.to_vec()),
                                                        Some(&DataType::Binary { .. }) => VVal::new_byt(data.to_vec()),
                                                        _ => VVal::new_str(s),
                                                    }
                                                } else {
                                                    VVal::new_byt(data.to_vec())
                                                }
                                            }
                                        };

                                        if let Some(key) = names.get(col_i) {
                                            row.set_key_str(key, v);
                                        } else {
                                            row.set_key_str("?", v);
                                        }
                                    }

                                    lst.push(row);
                                }

                                let mut batch = row_set_cursor.fetch();
                                in_batch = match batch {
                                    Ok(batch) => batch,
                                    Err(e) => {
                                        res_thrd.send(&VVal::err_msg(&format!(
                                            "ODBC fetch error: {}",
                                            e
                                        )));
                                        return;
                                    }
                                };
                            }

                            res_thrd.send(&lst);
                            return;
                        }
                        Ok(None) => Ok(VVal::None),
                        Err(e) => {
                            Ok(VVal::None)
                            //                            return Ok(env.new_err(format!("$<Odbc>.retrieve execute error: {}", e)));
                        }
                    }
                }
                Err(e) => {
                    Ok(VVal::None)
                    //                    return Ok(env.new_err(format!("$<Odbc>.retrieve prepare error: {}", e)));
                }
            };
            res_thrd.send(&VVal::Bol(true));
        });

        let res = res.wait().unwrap();
        println!("WAITED: {}", res.s());

        Ok(Self {})
    }
}

impl OdbcHandle {
    //    pub fn connect(&mut self, env: &mut Env, con_str: &str) -> Result<VVal, StackAction> {
    //        let res = self.env.connect_with_connection_string(&con_str);
    //        match res {
    //            Ok(con) => {
    //                self.connection = Some(con.into_sys());
    //                Ok(VVal::Bol(true))
    //            }
    //            Err(e) => Ok(env.new_err(format!("$<Odbc>.connect error: {}", e)))
    //        }
    //    }
    //
    //    pub fn retrieve(&mut self, env: &mut Env, sql: &str) -> Result<VVal, StackAction> {
    //        if let Some(con) = self.connection.as_mut() {
    //            let con = odbc_api::handles::Connection::new(*con);
    //            let con = odbc_api::Connection::new(con);
    //
    //            let res = match con.prepare(sql) {
    //                Ok(prep) => {
    //                    match prep.execute(()) {
    //                        Ok(Some(cursor)) => {
    //                            Ok(VVal::None)
    //                        },
    //                        Ok(None) => {
    //                            Ok(VVal::None)
    //                        },
    //                        Err(e) => {
    //                            return Ok(env.new_err(format!("$<Odbc>.retrieve execute error: {}", e)));
    //                        }
    //                    }
    //
    //                },
    //                Err(e) => {
    //                    return Ok(env.new_err(format!("$<Odbc>.retrieve prepare error: {}", e)));
    //                }
    //            };
    //
    //            con.into_sys();
    //
    //            res
    //        } else {
    //            Ok(env.new_err(format!("$<Odbc>.retrieve not connected!")))
    //        }
    //    }
}

#[derive(Clone)]
struct Odbc {
    handle: Rc<RefCell<OdbcHandle>>,
}

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

#[cfg(feature = "cursive")]
impl VValUserData for Odbc {
    fn s(&self) -> String {
        format!("$<Odbc>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();

        match key {
            //            "connect" => {
            //                assert_arg_count!("$<Odbc>", argv, 1, "connect[odbc_connect_string]", env);
            //                let mut hdl = self.handle.borrow_mut();
            //                hdl.connect(env, &argv.v_s_raw(0))
            //            }
            //            "exec" => {
            //                assert_arg_count!("$<Odbc>", argv, 1, "exec[sql]", env);
            //
            //                let mut hdl = self.handle.borrow_mut();
            //                hdl.execute(env, &argv.v_s_raw(0))
            //            }
            _ => Err(StackAction::panic_str(
                format!("$<Odbc> unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "odbc")]
    st.fun(
        "odbc:connect",
        |env: &mut Env, _argc: usize| {
            let hdl = OdbcHandle::connect(env.argv().v_s_raw(0));
            match hdl {
                Err(e) => Ok(env.new_err(e)),
                Ok(hdl) => Ok(VVal::new_usr(Odbc { handle: Rc::new(RefCell::new(hdl)) })),
            }
        },
        Some(1),
        Some(1),
        false,
    );
    // driver={%1%};uid=%2%;pwd=%3%;server=%4%;port=%6%;database=%5%
}
