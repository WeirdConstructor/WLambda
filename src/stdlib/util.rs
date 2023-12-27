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

#[derive(Clone)]
enum ClapCmdArg {
    Bool(String, bool),
    Str(String, bool),
    Number(String, bool),
    Subcmd(String, Vec<ClapCmdArg>),
}

impl ClapCmdArg {
    #[allow(unused)]
    fn set_id(&self, id: String) -> Self {
        match self {
            ClapCmdArg::Bool(_, b) => ClapCmdArg::Bool(id, *b),
            ClapCmdArg::Str(_, b) => ClapCmdArg::Str(id, *b),
            ClapCmdArg::Number(_, b) => ClapCmdArg::Number(id, *b),
            ClapCmdArg::Subcmd(a, b) => ClapCmdArg::Subcmd(a.clone(), b.clone()),
        }
    }

    fn set_many(&self) -> Self {
        match self {
            ClapCmdArg::Bool(id, _) => ClapCmdArg::Bool(id.to_string(), true),
            ClapCmdArg::Str(id, _) => ClapCmdArg::Str(id.to_string(), true),
            ClapCmdArg::Number(id, _) => ClapCmdArg::Number(id.to_string(), true),
            ClapCmdArg::Subcmd(a, b) => ClapCmdArg::Subcmd(a.clone(), b.clone()),
        }
    }

    #[allow(unused)]
    fn many(&self) -> bool {
        match self {
            ClapCmdArg::Bool(_, b) => *b,
            ClapCmdArg::Str(_, b) => *b,
            ClapCmdArg::Number(_, b) => *b,
            ClapCmdArg::Subcmd(_, _) => false,
        }
    }

    fn id(&self) -> &str {
        match self {
            ClapCmdArg::Bool(id, _) => &id,
            ClapCmdArg::Str(id, _) => &id,
            ClapCmdArg::Number(id, _) => &id,
            ClapCmdArg::Subcmd(id, _) => &id,
        }
    }
}

#[cfg(feature = "clap")]
fn vv2command(
    name: String,
    v_list: &[VVal],
    cfg: VVal,
) -> Result<(clap::Command, Vec<ClapCmdArg>), StackAction> {
    use clap::{Arg, ArgAction, Command};

    let mut m = Command::new(name);

    let mut arg_typs = Vec::new();

    for arg in v_list.iter() {
        if arg.is_sym() {
            arg.with_s_ref(|s| {
                if s == "$toml_config" {
                    Ok(())
                } else {
                    return Err(StackAction::panic_msg(format!("Unknown symbolic CLI arg: {}", s)));
                }
            })?;
        } else if arg.is_pair() {
            let mut cmdvec = Vec::new();
            arg.v_(1).with_iter(|it| {
                for (s, _) in it {
                    cmdvec.push(s);
                }
            });

            let (cmdname, about) = if arg.v_(0).is_kind_of_string() {
                (arg.v_s_raw(0), None)
            } else {
                (arg.v_(0).v_s_raw(0), Some(arg.v_(0).v_s_raw(1)))
            };

            let (subcmd, subcmd_args) = vv2command(cmdname, &cmdvec[..], cfg.clone())?;
            let subcmd = if let Some(about) = about { subcmd.about(about) } else { subcmd };
            m = m.subcommand(subcmd);
            arg_typs.push(ClapCmdArg::Subcmd(arg.v_s_raw(0), subcmd_args));
        } else if arg.is_vec() {
            let build_arg = arg.with_iter(|iter| {
                let mut build_arg = None;
                let mut arg_type_set = false;
                let mut arg_typ = ClapCmdArg::Bool(String::from(""), false);

                for (i, _) in iter {
                    if i.is_str() {
                        let s = i.s_raw();

                        if s == "--" {
                            build_arg =
                                Some(Arg::new(s.to_string()).trailing_var_arg(true).num_args(0..));
                            arg_type_set = true;
                            arg_typ = ClapCmdArg::Str(s.to_string(), true);
                        } else if s == "--1" {
                            build_arg = Some(
                                Arg::new(s.to_string())
                                    .trailing_var_arg(true)
                                    .required(true)
                                    .num_args(1..),
                            );
                            arg_type_set = true;
                            arg_typ = ClapCmdArg::Str(s.to_string(), true);
                        } else if s.starts_with("--") {
                            if build_arg.is_none() {
                                let sn: String = s.chars().skip(2).collect();
                                arg_typ = ClapCmdArg::Bool(sn.clone(), false);
                                build_arg = Some(Arg::new(sn));
                            }

                            build_arg =
                                build_arg.map(|a| a.long(s.chars().skip(2).collect::<String>()));
                        } else if s.starts_with("-") {
                            if build_arg.is_none() {
                                let sn: String = s.chars().skip(1).collect();
                                arg_typ = ClapCmdArg::Bool(sn.clone(), false);
                                build_arg = Some(Arg::new(sn));
                            }

                            build_arg =
                                build_arg.map(|a| a.short(s.chars().skip(1).next().unwrap_or('x')));
                        } else {
                            build_arg = build_arg.map(|a| a.help(s));
                        }
                    } else if i.is_bool() {
                        arg_type_set = true;

                        if i.b() {
                            build_arg = build_arg.map(|a| a.action(ArgAction::SetTrue));
                        } else {
                            build_arg = build_arg.map(|a| a.action(ArgAction::SetFalse));
                        }
                    } else if i.is_sym() && !i.s_raw().starts_with("$") {
                        if !arg_type_set {
                            arg_typ = ClapCmdArg::Str(arg_typ.id().to_string(), false);
                        }

                        arg_type_set = true;

                        build_arg =
                            build_arg.map(|a| a.action(ArgAction::Set).value_name(i.s_raw()));
                    } else if i.is_sym() && i.s_raw() == "$required" {
                        build_arg = build_arg.map(|a| a.required(true));
                    } else if i.is_sym() && i.s_raw() == "$append" {
                        if !arg_type_set {
                            arg_typ = ClapCmdArg::Str(arg_typ.id().to_string(), true);
                        } else {
                            arg_typ = arg_typ.set_many();
                        }

                        arg_type_set = true;

                        build_arg = build_arg.map(|a| a.action(ArgAction::Append));
                    } else if i.is_pair() {
                        if i.v_(0).is_sym() && i.v_s_raw(0) == "$default" {
                            build_arg = build_arg
                                .map(|a| a.default_missing_value(i.v_s_raw(1)).num_args(0..=1));
                        } else if i.v_(0).is_sym() && i.v_s_raw(0) == "$env" {
                            build_arg = build_arg.map(|a| a.env(i.v_s_raw(1)));
                        } else if i.v_(0).is_int() {
                            arg_type_set = true;
                            arg_typ = ClapCmdArg::Number(arg_typ.id().to_string(), false);

                            build_arg = build_arg.map(|a| a.action(ArgAction::Set));

                            if i.v_i(0) < 0 || i.v_i(1) < 0 {
                                build_arg = build_arg.map(|a| {
                                    a.allow_hyphen_values(true).allow_negative_numbers(true)
                                });
                            }

                            if i.v_(1).is_some() {
                                build_arg = build_arg.map(|a| {
                                    a.value_parser(
                                        clap::builder::RangedI64ValueParser::<i64>::new().range(
                                            std::ops::Range { start: i.v_i(0), end: i.v_i(1) + 1 },
                                        ),
                                    )
                                });
                            } else {
                                build_arg = build_arg.map(|a| {
                                    a.value_parser(
                                        clap::builder::RangedI64ValueParser::<i64>::new().range(
                                            std::ops::Range { start: i.v_i(0), end: std::i64::MAX },
                                        ),
                                    )
                                });
                            }
                        }
                    } else if i.is_vec() {
                        let mut values = Vec::new();
                        i.with_iter(|it| {
                            for (v, _) in it {
                                values.push(v.s_raw());
                            }
                        });

                        build_arg = build_arg.map(|a| {
                            a.value_parser(clap::builder::PossibleValuesParser::new(values))
                        });
                    } else if i.is_optional() {
                        build_arg = build_arg.map(|a| {
                            a.default_missing_value(i.unwrap_opt().s_raw())
                                .default_value(i.unwrap_opt().s_raw())
                                .num_args(0..=1)
                        });
                    }
                }

                if !arg_type_set {
                    build_arg = build_arg.map(|a| a.action(ArgAction::SetTrue));
                    arg_typ = ClapCmdArg::Bool(arg_typ.id().to_string(), false);
                }

                arg_typs.push(arg_typ);

                build_arg
            });

            if let Some(ba) = build_arg {
                m = m.arg(ba);
            }
        }
    }

    Ok((m, arg_typs))
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
            let mut i = 0;

            let cfg = VVal::map();
            let mut app_mode = true;

            let args = if env.arg(0).is_vec() {
                i += 1;
                app_mode = false;
                env.arg(0)
            } else {
                let args = env.global.borrow_mut().get_var_ref("@@").unwrap_or(VVal::None);
                if args.is_none() {
                    return Ok(cfg);
                }
                args
            };

            let name = env.arg(i).s_raw();

            let mut arg_vec = Vec::new();
            arg_vec.push(name.clone());
            args.with_iter(|it| {
                for (s, _) in it {
                    arg_vec.push(s.s_raw());
                }
            });

            let version = env.arg(i + 1).s_raw();
            let about = env.arg(i + 2).s_raw();

            let (mut m, arg_typs) = vv2command(name, &env.argv_ref()[(i + 3)..], cfg.clone())?;
            m = m.version(version).about(about);

            let matches = if app_mode {
                m.get_matches_from(arg_vec)
            } else {
                let matches = m.try_get_matches_from(arg_vec);
                match matches {
                    Err(err) => {
                        return Ok(env.new_err(format!("std:app:simple_cli error: {}", err)));
                    }
                    Ok(m) => m,
                }
            };

            let (matches, arg_typs) = if let Some((subcmd, sc_matches)) = matches.subcommand() {
                let mut ret_matches = matches.clone();
                let mut ret_args = arg_typs.clone();
                for at in arg_typs {
                    if let ClapCmdArg::Subcmd(cmdname, args) = at {
                        if cmdname == subcmd {
                            let _ = cfg.set_key_str("_cmd", VVal::new_str(subcmd));
                            ret_matches = sc_matches.clone();
                            ret_args = args.clone();
                        }
                    }
                }
                (ret_matches, ret_args)
            } else {
                (matches, arg_typs)
            };

            for typ in arg_typs {
                match typ {
                    ClapCmdArg::Bool(id, many) => {
                        if many {
                            if let Some(v) = matches.get_many::<bool>(&id) {
                                let xargs = VVal::vec();
                                for sarg in v {
                                    xargs.push(VVal::Bol(*sarg));
                                }
                                let _ = cfg.set_key_str(&id, xargs);
                            }
                        } else {
                            if let Some(v) = matches.get_one::<bool>(&id) {
                                let _ = cfg.set_key_str(&id, VVal::Bol(*v));
                            }
                        }
                    }
                    ClapCmdArg::Str(id, many) => {
                        if many {
                            if let Some(v) = matches.get_many::<String>(&id) {
                                let xargs = VVal::vec();
                                for sarg in v {
                                    xargs.push(VVal::new_str(sarg.as_str()));
                                }
                                let _ = cfg.set_key_str(&id, xargs);
                            }
                        } else {
                            if let Some(v) = matches.get_one::<String>(&id) {
                                let _ = cfg.set_key_str(&id, VVal::new_str_mv(v.clone()));
                            }
                        }
                    }
                    ClapCmdArg::Number(id, many) => {
                        if many {
                            if let Some(v) = matches.get_many::<i64>(&id) {
                                let xargs = VVal::vec();
                                for sarg in v {
                                    xargs.push(VVal::Int(*sarg));
                                }
                                let _ = cfg.set_key_str(&id, xargs);
                            }
                        } else {
                            if let Some(v) = matches.get_one::<i64>(&id) {
                                let _ = cfg.set_key_str(&id, VVal::Int(*v));
                            }
                        }
                    }
                    ClapCmdArg::Subcmd(cmd, args) => {}
                }
            }

            Ok(cfg)
        },
        Some(4),
        None,
        false,
    );
}
