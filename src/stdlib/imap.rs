#[cfg(feature = "mail")]
use imap;
#[cfg(feature = "mail")]
use native_tls;
#[cfg(feature = "mail")]
use mailparse;
#[cfg(feature = "mail")]
use mailparse::MailHeaderMap;

#[allow(unused_imports)]
use crate::{StackAction, SymbolTable, Env, VValUserData, VVal};
#[allow(unused_imports)]
use std::rc::Rc;
#[allow(unused_imports)]
use std::cell::RefCell;

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "imap")]
    st.fun(
        "imap:connect_and_login",
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
            let imap_session = client
                .login(&user, &pass)
                .map_err(|e| e.0);

            let mut imap_session = match imap_session {
                Ok(imap_session) => imap_session,
                Err(e) => {
                    return Ok(env.new_err(format!("std:imap:connect_and_login error: {}", e)))
                }
            };

            // we want to fetch the first email in the INBOX mailbox
            if let Err(e) = imap_session.select("INBOX") {
                return Ok(env.new_err(format!("$<IMAP>.select error: {}", e)))
            }

            // fetch message number 1 in this mailbox, along with its RFC822 field.
            // RFC 822 dictates the format of the body of e-mails
            let messages = match imap_session.fetch("2", "RFC822") {
                Ok(msgs) => msgs,
                Err(e) => {
                    return Ok(env.new_err(format!("$<IMAP>.fetch error: {}", e)))
                }
            };
            let message = if let Some(m) = messages.iter().next() {
                m
            } else {
                return Ok(VVal::None);
            };

            // extract the message's body
            let body = message.body().expect("message did not have a body!");


            match mailparse::parse_mail(body) {
                Ok(mail) => {
                    println!("OO: {:?}", mail.get_headers());
                    for part in mail.parts() {
//                        println!("> {:#?}", part.get_headers());
//                        println!("> parts: {}", part.parts().count());
                    if let Some(v) = part.get_headers().get_first_value("Content-Type")  {
                        println!("CONTENT TYPE: {}", v);
                        if v.contains("text/plain") {
                            println!("text: {}", part.get_body().unwrap());
                        } else if v.contains("text/html") {
                            println!("html: {}", part.get_body().unwrap());
                        }
                    }
//                        for part2 in part.parts() {
//                            if let Some(v) = part2.get_headers().get_first_value("Content-Type")  {
//                            }
////                            println!(">> {:#?}", part2.get_headers());
////                            println!(">> parts: {}", part2.parts().count());
//                        }
//                        println!("{}", part.get_body().unwrap());
                    }
                }
                Err(e) => {
                    return Ok(env.new_err(format!("$<IMAP>.fetch parse mail error: {}", e)))
                }
            }

            // be nice to the server and log out
            if let Err(e) = imap_session.logout() {
                return Ok(env.new_err(format!("$<IMAP>.logout error: {}", e)))
            }

            Ok(VVal::None)

        }, Some(4), Some(4), false);
}
