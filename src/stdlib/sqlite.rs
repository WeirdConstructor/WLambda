#[allow(unused_imports)]
use crate::{Env, StackAction, SymbolTable, VVal, VValUserData};
#[allow(unused_imports)]
use std::sync::{Arc, Mutex};

#[cfg(feature = "sqlite")]
#[derive(Clone)]
struct VSqliteConnection {
    con: Arc<Mutex<sqlite::Connection>>,
}

#[cfg(feature = "sqlite")]
impl VSqliteConnection {
    fn exec_sql_stmt(&self, stmt_str: String, binds: &Vec<VVal>) -> VVal {
        let con = match self.con.lock() {
            Ok(con) => con,
            Err(e) => {
                return VVal::err_msg(&format!("sqlite mutex error: {}", e));
            }
        };

        let stmt = con.prepare(stmt_str.clone());
        if let Err(e) = stmt {
            return VVal::err_msg(&format!("SQL parse error '{}': {}", stmt_str, e));
        }
        let mut stmt = stmt.unwrap();

        for (i, b) in binds.iter().enumerate() {
            if b.is_float() {
                stmt.bind((i + 1, &sqlite::Value::Float(b.f()))).unwrap();
            } else if b.is_int() {
                stmt.bind((i + 1, &sqlite::Value::Integer(b.i()))).unwrap();
            } else if let VVal::Byt(u) = b {
                stmt.bind((i + 1, &sqlite::Value::Binary(u.as_ref().clone()))).unwrap();
            } else if let VVal::None = b {
                stmt.bind((i + 1, &sqlite::Value::Null)).unwrap();
            } else {
                stmt.bind((i + 1, &sqlite::Value::String(b.s_raw()))).unwrap();
            }
        }

        let mut ret = VVal::None;
        loop {
            match stmt.next() {
                Err(e) => {
                    return VVal::err_msg(&format!("SQL exec error on '{}': {}", stmt_str, e));
                }
                Ok(sqlite::State::Row) => {
                    if let VVal::None = ret {
                        ret = VVal::vec();
                    };

                    let row_vv = VVal::map();
                    for i in 0..stmt.column_count() {
                        let name = match stmt.column_name(i) {
                            Ok(name) => name,
                            Err(e) => {
                                return VVal::err_msg(&format!(
                                    "SQL exec error on fetching column name '{}': {}",
                                    stmt_str, e
                                ));
                            }
                        };
                        let col_type = match stmt.column_type(i) {
                            Ok(col_type) => col_type,
                            Err(e) => {
                                return VVal::err_msg(&format!(
                                    "SQL exec error on fetching column type '{}': {}",
                                    stmt_str, e
                                ));
                            }
                        };
                        row_vv
                            .set_key_str(
                                name,
                                match col_type {
                                    sqlite::Type::Integer => {
                                        VVal::Int(stmt.read::<i64, usize>(i).unwrap())
                                    }
                                    sqlite::Type::Float => {
                                        VVal::Flt(stmt.read::<f64, usize>(i).unwrap())
                                    }
                                    sqlite::Type::Binary => {
                                        VVal::new_byt(stmt.read::<Vec<u8>, usize>(i).unwrap())
                                    }
                                    sqlite::Type::String => {
                                        VVal::new_str_mv(stmt.read::<String, usize>(i).unwrap())
                                    }
                                    sqlite::Type::Null => VVal::None,
                                },
                            )
                            .expect("no double usage of row_vv");
                    }

                    ret.push(row_vv);
                }
                Ok(sqlite::State::Done) => {
                    break;
                }
            };
        }

        ret
    }
}

#[cfg(feature = "sqlite")]
impl VValUserData for VSqliteConnection {
    fn s(&self) -> String {
        format!("$<Sqlite>")
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
            "exec" => {
                let argc = argv.len();
                if argc < 1 {
                    return Err(StackAction::panic_str(
                        format!(
                            "{} expects at least 1 arguments",
                            "$<Sqlite>.exec[sql_string, [param1, [param2, ...]]]"
                        ),
                        None,
                        env.argv(),
                    ));
                }

                let stmt_str = env.arg(0).s_raw();
                let binds = if env.arg(1).is_vec() {
                    env.arg(1).to_vec()
                } else {
                    let mut binds = vec![];
                    for i in 1..argc {
                        binds.push(env.arg(i).clone())
                    }
                    binds
                };

                Ok(self.exec_sql_stmt(stmt_str, &binds))
            }
            _ => Err(StackAction::panic_str(
                format!("$<Sqlite> unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        Some(Box::new(self.clone()))
    }
}

#[cfg(feature = "sqlite")]
impl crate::threads::ThreadSafeUsr for VSqliteConnection {
    fn to_vval(&self) -> VVal {
        VVal::new_usr(VSqliteConnection { con: self.con.clone() })
    }
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "sqlite")]
    st.fun(
        "sqlite:connect",
        |env: &mut Env, _argc: usize| {
            let open_str = env.arg(0).s_raw();
            match sqlite::open(open_str.clone()) {
                Ok(con) => Ok(VVal::new_usr(VSqliteConnection { con: Arc::new(Mutex::new(con)) })),
                Err(e) => {
                    Ok(VVal::err_msg(&format!("Couldn't open sqlite db '{}': {}", open_str, e)))
                }
            }
        },
        Some(1),
        Some(1),
        false,
    );
}
