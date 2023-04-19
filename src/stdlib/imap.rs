#[cfg(feature = "mail")]
use imap::{types::Fetch, Session};
#[cfg(feature = "mail")]
use imap_proto::types::{BodyContentCommon, BodyStructure, MessageSection, SectionPath};
#[cfg(feature = "mail")]
use mailparse;
#[cfg(feature = "mail")]
use mailparse::{MailHeaderMap, ParsedMail};
#[cfg(feature = "mail")]
use native_tls;

#[allow(unused_imports)]
use crate::{Env, StackAction, SymbolTable, VVal, VValUserData};
#[allow(unused_imports)]
use std::sync::{Arc, Mutex};

#[cfg(feature = "imap")]
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

#[cfg(feature = "imap")]
#[derive(Clone)]
struct VImapSession {
    session: Arc<Mutex<Session<native_tls::TlsStream<std::net::TcpStream>>>>,
}

impl crate::threads::ThreadSafeUsr for VImapSession {
    fn to_vval(&self) -> VVal {
        VVal::new_usr(VImapSession { session: self.session.clone() })
    }
}

fn body_cont2vv<'a>(bcc: &'a BodyContentCommon) -> VVal {
    let mut params = VVal::None;
    if let Some(p) = &bcc.ty.params {
        params = VVal::map();
        for (key, val) in p.iter() {
            let _ = params.set_key_str(key, VVal::new_str(val));
        }
    }

    VVal::pair(VVal::new_str_mv(format!("{}/{}", bcc.ty.ty, bcc.ty.subtype)), params)
}

fn bodystructure2vv<'a>(bs: &'a BodyStructure) -> VVal {
    match bs {
        BodyStructure::Basic { common, .. } => {
            VVal::pair(VVal::new_sym("basic"), body_cont2vv(common))
        }
        BodyStructure::Text { common, .. } => {
            VVal::pair(VVal::new_sym("text"), body_cont2vv(common))
        }
        BodyStructure::Message { common, body, .. } => {
            let sub = body_cont2vv(common);
            let _ = sub.v_(1).set_key_str("body", bodystructure2vv(body));
            VVal::pair(VVal::new_sym("message"), sub)
        }
        BodyStructure::Multipart { common, bodies, .. } => {
            let bods = VVal::vec();
            for body in bodies.iter() {
                bods.push(bodystructure2vv(body));
            }

            let sub = body_cont2vv(common);
            let _ = sub.v_(1).set_key_str("bodies", bods);

            VVal::pair(VVal::new_sym("multipart"), sub)
        }
    }
}

fn vv2section_path(vv: &VVal) -> Option<SectionPath> {
    if vv.is_kind_of_string() {
        vv.with_s_ref(|s| match s {
            "header" => Some(SectionPath::Full(MessageSection::Header)),
            "mime" => Some(SectionPath::Full(MessageSection::Mime)),
            "text" => Some(SectionPath::Full(MessageSection::Text)),
            _ => None,
        })
    } else if vv.v_(0).is_int() || vv.v_(0).is_float() {
        let mut path = vec![];
        vv.with_iter(|it| {
            for (elem, _) in it {
                path.push(elem.i() as u32);
            }
        });

        Some(SectionPath::Part(path, None))
    } else {
        let mut path = vec![];
        vv.v_(0).with_iter(|it| {
            for (elem, _) in it {
                path.push(elem.i() as u32);
            }
        });

        if vv.v_(1).is_none() {
            Some(SectionPath::Part(path, None))
        } else {
            vv.v_(1).with_s_ref(|s| match s {
                "header" => Some(SectionPath::Part(path, Some(MessageSection::Header))),
                "mime" => Some(SectionPath::Part(path, Some(MessageSection::Mime))),
                "text" => Some(SectionPath::Part(path, Some(MessageSection::Text))),
                _ => None,
            })
        }
    }
}

fn fetch2vv(env: &mut Env, f: &Fetch, arg: VVal) -> VVal {
    let uid = if let Some(uid) = f.uid { VVal::Int(uid.into()) } else { VVal::None };

    let ret = VVal::vec2(VVal::Int(f.message.into()), uid);

    if arg.is_some() {
        if let Some(sp) = vv2section_path(&arg) {
            println!("SEC:ÜP {:?}", sp);
            if let Some(body) = f.section(&sp) {
                ret.push(VVal::pair(VVal::new_sym("section"), VVal::new_byt(body.to_vec())));
            } else {
                ret.push(VVal::pair(
                    VVal::new_sym("section_not_found"),
                    VVal::new_str_mv(format!("{:?}", f)),
                ));
            }

            return ret;
        } else {
            return env.new_err(format!("$<IMAPSession>.fetch bad section path: {}", arg.s()));
        }
    }

    if let Some(bs) = f.bodystructure() {
        //        println!("BS: {:#?}", bs);
        ret.push(bodystructure2vv(bs));
    } else if let Some(body) = f.body() {
        ret.push(VVal::pair(VVal::new_sym("body"), VVal::new_byt(body.to_vec())));
    } else if let Some(body) = f.text() {
        ret.push(VVal::pair(VVal::new_sym("text"), VVal::new_byt(body.to_vec())));
    } else {
        ret.push(VVal::pair(VVal::new_sym("unimplemented"), VVal::new_str_mv(format!("{:?}", f))));
    }

    ret
}

#[cfg(feature = "mail")]
impl VValUserData for VImapSession {
    fn s(&self) -> String {
        format!("$<IMAPSession>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();
        let argc = argv.len();

        let mut session = match self.session.lock() {
            Ok(session) => session,
            Err(e) => return Ok(env.new_err(format!("$<IMAPSession> can't get lock: {}", e))),
        };

        match key {
            "examine" => {
                assert_arg_count!("$<IMAPSession>", argv, 1, "examine[mailbox_string]", env);
                match session.examine(argv.v_s_raw(0)) {
                    Ok(mailbox) => Ok(VVal::map3(
                        "exists",
                        VVal::Int(mailbox.exists.into()),
                        "recent",
                        VVal::Int(mailbox.recent.into()),
                        "unseen",
                        mailbox.unseen.map(|u| VVal::Int(u.into())).unwrap_or(VVal::None),
                    )),
                    Err(e) => Ok(env.new_err(format!(
                        "$<IMAPSession>.examine[{}] error: {}",
                        argv.v_s_raw(0),
                        e
                    ))),
                }
            }
            "select" => {
                assert_arg_count!("$<IMAPSession>", argv, 1, "select[mailbox_string]", env);
                match session.select(argv.v_s_raw(0)) {
                    Ok(mailbox) => Ok(VVal::map3(
                        "exists",
                        VVal::Int(mailbox.exists.into()),
                        "recent",
                        VVal::Int(mailbox.recent.into()),
                        "unseen",
                        mailbox.unseen.map(|u| VVal::Int(u.into())).unwrap_or(VVal::None),
                    )),
                    Err(e) => Ok(env.new_err(format!(
                        "$<IMAPSession>.select[{}] error: {}",
                        argv.v_s_raw(0),
                        e
                    ))),
                }
            }
            "fetch" => {
                if argc < 2 {
                    return Err(StackAction::panic_str(
                        format!(
                            "{} expects at least 2 arguments",
                            "$<IMAPSession>.fetch[sequence_set, query, [section]]"
                        ),
                        None,
                        env.argv(),
                    ));
                } else if argc > 3 {
                    return Err(StackAction::panic_str(
                        format!(
                            "{} expects at most 3 arguments",
                            "$<IMAPSession>.fetch[sequence_set, query, [section]]"
                        ),
                        None,
                        env.argv(),
                    ));
                }

                let messages = match session.fetch(argv.v_s_raw(0), argv.v_s_raw(1)) {
                    Ok(msgs) => msgs,
                    Err(e) => return Ok(env.new_err(format!("$<IMAP>.fetch error: {}", e))),
                };

                let ret = VVal::vec();

                for msg in messages.iter() {
                    ret.push(fetch2vv(env, msg, argv.v_(2)));
                }

                return Ok(ret);
            }
            "logout" => {
                assert_arg_count!("$<IMAPSession>", argv, 0, "logout[]", env);

                if let Err(e) = session.logout() {
                    Ok(env.new_err(format!("$<IMAP>.logout error: {}", e)))
                } else {
                    Ok(VVal::Bol(true))
                }
            }
            _ => Err(StackAction::panic_str(
                format!("$<Odbc> unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
}

fn mail_part2vv(env: &mut Env, part: &ParsedMail) -> Result<VVal, VVal> {
    let m = VVal::map();
    for header in part.get_headers().into_iter() {
        let _ = m.set_key_str(&header.get_key()[..], VVal::new_str_mv(header.get_value()));
    }

    let body = if let Some(ct) = part.get_headers().get_first_value("Content-Type") {
        if ct.contains("text/") {
            match part.get_body() {
                Ok(text) => VVal::new_str_mv(text),
                Err(e) => {
                    return Err(
                        env.new_err(format!("std:mail:parse raw body parse error: {:?}", e))
                    );
                }
            }
        } else {
            match part.get_body_raw() {
                Ok(raw) => VVal::new_byt(raw),
                Err(e) => {
                    return Err(
                        env.new_err(format!("std:mail:parse raw body parse error: {:?}", e))
                    );
                }
            }
        }
    } else {
        match part.get_body_raw() {
            Ok(raw) => VVal::new_byt(raw),
            Err(e) => {
                return Err(env.new_err(format!("std:mail:parse raw body parse error: {:?}", e)));
            }
        }
    };

    Ok(VVal::vec2(m, body))
}

fn mail2vv(env: &mut Env, mail: &ParsedMail) -> Result<VVal, VVal> {
    let parts = VVal::vec();

    let top = mail_part2vv(env, mail)?;

    for part in mail.parts().skip(1) {
        parts.push(mail2vv(env, part)?);
    }

    top.push(parts);

    Ok(top)
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "mailparse")]
    st.fun(
        "mail:parse",
        |env: &mut Env, _argc: usize| {
            env.arg(0).with_bv_ref(|s| match mailparse::parse_mail(s) {
                Ok(mail) => match mail2vv(env, &mail) {
                    Ok(mail) => Ok(mail),
                    Err(err) => Ok(err),
                },
                Err(e) => Ok(env.new_err(format!("std:mail:parse error: {}", e))),
            })
        },
        Some(1),
        Some(1),
        false,
    );

    #[cfg(feature = "imap")]
    st.fun(
        "imap:connect_and_login_tls",
        |env: &mut Env, _argc: usize| {
            let domain = env.arg(0).s_raw();
            let port = env.arg(1).i();
            let user = env.arg(2).s_raw();
            let pass = env.arg(3).s_raw();

            let tls = native_tls::TlsConnector::builder().build().unwrap();

            // we pass in the domain twice to check that the server's TLS
            // certificate is valid for the domain we're connecting to.
            let client = imap::connect((&domain[..], port as u16), &domain, &tls).unwrap();

            // the client we have here is unauthenticated.
            // to do anything useful with the e-mails, we need to log in
            let imap_session = client.login(&user, &pass).map_err(|e| e.0);

            let imap_session = match imap_session {
                Ok(imap_session) => imap_session,
                Err(e) => {
                    return Ok(env.new_err(format!("std:imap:connect_and_login error: {}", e)))
                }
            };

            Ok(VVal::new_usr(VImapSession { session: Arc::new(Mutex::new(imap_session)) }))
        },
        Some(4),
        Some(4),
        false,
    );
}
