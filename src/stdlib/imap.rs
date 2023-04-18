#[cfg(feature = "mail")]
use imap::{types::Fetch, Session};
#[cfg(feature = "mail")]
use imap_proto::types::{BodyStructure, BodyContentCommon, ContentType};
#[cfg(feature = "mail")]
use mailparse;
#[cfg(feature = "mail")]
use mailparse::MailHeaderMap;
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
        BodyStructure::Message { common, envelope, body, .. } => {
            let sub = body_cont2vv(common);
            let _ = sub.v_(1).set_key_str("body", bodystructure2vv(body));
            VVal::pair(VVal::new_sym("message"), sub)
        }
        BodyStructure::Multipart { common, bodies, .. } => {
            let bods = VVal::vec();
//                println!("BODIES MULTIPART {}!!!", bodies.len());
            for body in bodies.iter() {
//                println!("BODY EXT!!!");
                bods.push(bodystructure2vv(body));
            }

            let sub = body_cont2vv(common);
            let _ = sub.v_(1).set_key_str("bodies", bods);

            VVal::pair(VVal::new_sym("multipart"), sub)
        }
    }
}

fn fetch2vv(env: &mut Env, f: &Fetch) -> VVal {
    let uid = if let Some(uid) = f.uid { VVal::Int(uid.into()) } else { VVal::None };

    let ret = VVal::vec2(VVal::Int(f.message.into()), uid);

    if let Some(bs) = f.bodystructure() {
//        println!("BS: {:#?}", bs);
        ret.push(bodystructure2vv(bs));
    } else {
        println!("UNIMPL: {:#?}", f);
        return env.new_err(format!("$<IMAPSession>.fetch unimplemented return value"));
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

        let mut session = match self.session.lock() {
            Ok(session) => session,
            Err(e) => return Ok(env.new_err(format!("$<IMAPSession> can't get lock: {}", e))),
        };

        match key {
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

            //            if let Err(e) = imap_session.select("INBOX") {
            //                return Ok(env.new_err(format!("$<IMAP>.select error: {}", e)))
            //            }
            "fetch" => {
                assert_arg_count!("$<IMAPSession>", argv, 2, "fetch[sequence_set, query]", env);

                let messages = match session.fetch(argv.v_s_raw(0), argv.v_s_raw(1)) {
                    Ok(msgs) => msgs,
                    Err(e) => return Ok(env.new_err(format!("$<IMAP>.fetch error: {}", e))),
                };

                let ret = VVal::vec();

                for msg in messages.iter() {
                    ret.push(fetch2vv(env, msg));
                }

                return Ok(ret);

                //            let message = if let Some(m) = messages.iter().next() {
                //
                //                if let Some(bs) = message.bodystructure() {
                //                    println!("STRUCT: {:#?}", bs);
                //                }
                //
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

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
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

            //            // we want to fetch the first email in the INBOX mailbox
            //            if let Err(e) = imap_session.select("INBOX") {
            //                return Ok(env.new_err(format!("$<IMAP>.select error: {}", e)))
            //            }
            //
            //            // fetch message number 1 in this mailbox, along with its RFC822 field.
            //            // RFC 822 dictates the format of the body of e-mails
            //            let messages = match imap_session.fetch("2", "BODYSTRUCTURE") {
            //                Ok(msgs) => msgs,
            //                Err(e) => {
            //                    return Ok(env.new_err(format!("$<IMAP>.fetch error: {}", e)))
            //                }
            //            };
            //            let message = if let Some(m) = messages.iter().next() {
            //                m
            //            } else {
            //                return Ok(VVal::None);
            //            };
            ////
            //            if let Some(bs) = message.bodystructure() {
            //                println!("STRUCT: {:#?}", bs);
            //
            //            } else if let Some(body) = message.body() {
            //                match mailparse::parse_mail(body) {
            //                    Ok(mail) => {
            //                        println!("OO: {:?}", mail.get_headers());
            //                        for part in mail.parts() {
            //    //                        println!("> {:#?}", part.get_headers());
            //    //                        println!("> parts: {}", part.parts().count());
            //                        if let Some(v) = part.get_headers().get_first_value("Content-Type")  {
            //                            println!("CONTENT TYPE: {}", v);
            //                            if v.contains("text/plain") {
            //                                println!("text: {}", part.get_body().unwrap());
            //                            } else if v.contains("text/html") {
            //                                println!("html: {}", part.get_body().unwrap());
            //                            }
            //                        }
            //    //                        for part2 in part.parts() {
            //    //                            if let Some(v) = part2.get_headers().get_first_value("Content-Type")  {
            //    //                            }
            //    ////                            println!(">> {:#?}", part2.get_headers());
            //    ////                            println!(">> parts: {}", part2.parts().count());
            //    //                        }
            //    //                        println!("{}", part.get_body().unwrap());
            //                        }
            //                    }
            //                    Err(e) => {
            //                        return Ok(env.new_err(format!("$<IMAP>.fetch parse mail error: {}", e)))
            //                    }
            //                }
            //            }
            //
            // be nice to the server and log out
            //            if let Err(e) = imap_session.logout() {
            //                return Ok(env.new_err(format!("$<IMAP>.logout error: {}", e)))
            //            }

            //            Ok(VVal::None)
        },
        Some(4),
        Some(4),
        false,
    );
}
