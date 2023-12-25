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
use std::rc::Rc;

#[cfg(feature = "clipboard")]
use copypasta::{ClipboardContext, ClipboardProvider};

#[cfg(feature = "clipboard")]
thread_local! {
    pub static CLIP_CTX: RefCell<ClipboardContext> = RefCell::new(ClipboardContext::new().unwrap());
}


#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "clipboard")]
    st.fun(
        "os:get_clipboard_text",
        |env: &mut Env, _argc: usize| {
            CLIP_CTX.with(|ctx| match ctx.borrow_mut().get_contents() {
                Ok(txt) => Ok(VVal::new_str_mv(txt)),
                Err(e) => Ok(env.new_err(format!("std:os:get_clipboard_text error: {}", e))),
            })
        },
        Some(0),
        Some(0),
        false,
    );

    #[cfg(feature = "clipboard")]
    st.fun(
        "os:set_clipboard_text",
        |env: &mut Env, _argc: usize| {
            CLIP_CTX.with(|ctx| match ctx.borrow_mut().set_contents(env.arg(0).s_raw()) {
                Ok(()) => Ok(VVal::Bol(true)),
                Err(e) => Ok(env.new_err(format!("std:os:set_clipboard_text error: {}", e))),
            })
        },
        Some(1),
        Some(1),
        false,
    );

    #[cfg(feature = "html")]
    st.fun(
        "html:parse_simple",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_s_ref(|s| Ok(super::html2vval::parse_simplified(s)))
        },
        Some(1),
        Some(1),
        false,
    );

    #[cfg(feature = "html")]
    st.fun(
        "html:parse_complex",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_s_ref(|s| Ok(super::html2vval::parse(s, true)))
        },
        Some(1),
        Some(1),
        false,
    );


    #[cfg(feature = "toml")]

    #[cfg(feature = "clap")]
    st.fun(
        "app:simple_cli",
        |env: &mut Env, argc: usize| {
            use clap::{Command, Arg, ArgAction};
            let name = env.arg(0).s_raw();
            let version = env.arg(1).s_raw();
            let about = env.arg(2).s_raw();
            let mut m = Command::new(name.clone());
            m = m.version(version);

            let cfg = VVal::map();

            let mut arg_ids = Vec::new();

            for i in 3..argc {
                let arg = env.arg(i);
                if arg.is_sym() {
                    arg.with_s_ref(|s| {
                        if s == "$toml_config" {
                            Ok(())
                        } else {
                            return Err(StackAction::panic_msg(
                                format!("Unknown symbolic CLI arg: {}", s)));
                        }
                    })?;
                } else if arg.is_pair() {
                } else if arg.is_vec() {
                    let build_arg = arg.with_iter(|iter| {
                        let mut build_arg = None;
                        let mut arg_type_set = false;
                        let mut arg_id = String::from("");
                        let mut arg_typ = 0;

                        for (i, _) in iter {
                            if i.is_str() {
                                let s = i.s_raw();

                                if s == "--" {
                                    build_arg = Some(
                                        Arg::new(s.to_string()).trailing_var_arg(true).num_args(0..));
                                    arg_id = s.to_string();
                                    arg_type_set = true;
                                    arg_typ = 3;

                                } else if s == "--1" {
                                    build_arg = Some(
                                        Arg::new(s.to_string()).trailing_var_arg(true).required(true)
                                            .num_args(1..));
                                    arg_id = s.to_string();
                                    arg_type_set = true;
                                    arg_typ = 3;

                                } else if s.starts_with("--") {
                                    if build_arg.is_none() {
                                        let sn : String = s.chars().skip(2).collect();
                                        arg_id = sn.clone();
                                        build_arg = Some(Arg::new(sn));
                                    }

                                    build_arg = build_arg.map(
                                        |a| a.long(s.chars().skip(2).collect::<String>()));

                                } else if s.starts_with("-") {
                                    if build_arg.is_none() {
                                        let sn : String = s.chars().skip(1).collect();
                                        arg_id = sn.clone();
                                        build_arg = Some(Arg::new(sn));
                                    }

                                    build_arg = build_arg.map(
                                        |a| a.short(s.chars().skip(1).next().unwrap_or('x')));

                                } else {
                                    build_arg = build_arg.map(|a| a.help(s));
                                }
                            } else if !arg_type_set && i.is_bool() {
                                arg_type_set = true;

                                if i.b() {
                                    build_arg = build_arg.map(|a| a.action(ArgAction::SetTrue));
                                } else {
                                    build_arg = build_arg.map(|a| a.action(ArgAction::SetFalse));
                                }
                            } else if !arg_type_set && i.is_sym() && !i.s_raw().starts_with("$") {
                                arg_type_set = true;

                                arg_typ = 2;

                                build_arg = build_arg.map(
                                    |a| a.action(ArgAction::Set).value_name(i.s_raw()));

                            } else if i.is_sym() && i.s_raw() == "$required" {
                                build_arg = build_arg.map(|a| a.required(true));

                            } else if i.is_optional() {
                                build_arg = build_arg.map(
                                    |a| a.default_missing_value(
                                            i.unwrap_opt().s_raw()).num_args(0..=1));
                            }
                        }

                        if !arg_type_set {
                            build_arg = build_arg.map(|a| a.action(ArgAction::SetTrue));
                            arg_typ = 0;
                        }

                        arg_ids.push((arg_id, arg_typ));

                        build_arg
                    });

                    if let Some(ba) = build_arg {
                        m = m.arg(ba);
                    }
                }
            }

            let args = env.global.borrow_mut().get_var_ref("@@").unwrap_or(VVal::None);
            if args.is_none() {
                return Ok(cfg);
            }
            let mut arg_vec = Vec::new();
            arg_vec.push(name);
            args.with_iter(|it| {
                for (s, _) in it {
                    arg_vec.push(s.s_raw());
                }
            });

            let matches = m.get_matches_from(arg_vec);

            for (id, typ) in arg_ids {
                if typ == 0 || typ == 1 {
                    if let Some(v) = matches.get_one::<bool>(&id) {
                        let _ = cfg.set_key_str(&id, VVal::Bol(*v));
                    }
                } else if typ == 2 {
                    if let Some(v) = matches.get_one::<String>(&id) {
                        let _ = cfg.set_key_str(&id, VVal::new_str_mv(v.clone()));
                    }
                } else if typ == 3 {
                    if let Some(v) = matches.get_many::<String>(&id) {
                        let xargs = VVal::vec();
                        for sarg in v {
                            xargs.push(VVal::new_str(sarg.as_str()));
                        }
                        let _ = cfg.set_key_str(&id, xargs);
                    }
                }
            }

            Ok(cfg)
        },
        Some(3),
        None,
        false,
    );
}
