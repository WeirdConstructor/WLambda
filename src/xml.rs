use crate::{Env, VVal, StackAction};
use quick_xml::Reader;
use quick_xml::events::Event;

macro_rules! unesc_val_to_vval_or_err {
    ($env: ident, $rd: ident, $val: expr) => {
        match $val.unescape_and_decode_value(&$rd) {
            Ok(text) => VVal::new_str_mv(text),
            Err(e) => {
                return
                    Ok($env.new_err(
                        format!("XML error at {}: {}",
                                $rd.buffer_position(), e)));
            }
        }
    }
}

macro_rules! unesc_to_vval_or_err {
    ($env: ident, $rd: ident, $val: expr) => {
        match $val.unescape_and_decode(&$rd) {
            Ok(text) => VVal::new_str_mv(text),
            Err(e) => {
                return
                    Ok($env.new_err(
                        format!("XML error at {}: {}",
                                $rd.buffer_position(), e)));
            }
        }
    }
}

#[cfg(feature="quick-xml")]
fn xml_start_attrs<'a, B>(
    env: &mut Env,
    rd: &Reader<B>,
    e: &quick_xml::events::BytesStart<'a>)
    -> Result<VVal, StackAction>
    where B: std::io::BufRead {

    let mut attrs = VVal::None;
    for attr in e.attributes() {
        if attrs.is_none() {
            attrs = VVal::map();
        }

        match attr {
            Ok(attr) => {
                attrs.set_key_str(
                    &String::from_utf8_lossy(attr.key),
                    unesc_val_to_vval_or_err!(env, rd, attr))
                    .expect("single use");
            },
            Err(e) => {
                return
                    Ok(env.new_err(
                        format!("XML parse attribute error at {}: {}",
                                rd.buffer_position(),
                                e)));
            }
        }
    }

    Ok(attrs)
}

#[cfg(feature="quick-xml")]
pub(crate) fn parse_sax(env: &mut Env, input: VVal, event_func: VVal)
    -> Result<VVal, StackAction> {

    input.with_s_ref(|xml| {
        let mut buf = Vec::new();
        let mut rd  = Reader::from_str(xml);
        let mut ret = VVal::None;
        rd.trim_text(true);

        #[allow(unused_assignments)]
        loop {
            let mut call_arg = None;

            match rd.read_event(&mut buf) {
                Ok(Event::Start(ref e)) => {
                    let attrs = xml_start_attrs(env, &rd, e)?;

                    call_arg = Some(
                        VVal::vec3(
                            VVal::sym("start"),
                            VVal::new_str_mv(
                                String::from_utf8_lossy(
                                    e.local_name()).to_string()),
                            attrs));
                },
                Ok(Event::Empty(ref e)) => {
                    let attrs = xml_start_attrs(env, &rd, e)?;

                    call_arg = Some(
                        VVal::vec3(
                            VVal::sym("empty"),
                            VVal::new_str_mv(
                                String::from_utf8_lossy(
                                    e.local_name()).to_string()),
                            attrs));
                },
                Ok(Event::Comment(ref t)) => {
                    call_arg = Some(
                        VVal::vec2(
                            VVal::sym("comment"),
                            unesc_to_vval_or_err!(env, rd, t)));
                },
                Ok(Event::Decl(ref e)) => {
                    let mut version    = VVal::None;
                    let mut encoding   = VVal::None;
                    let mut standalone = VVal::None;

                    match e.version() {
                        Ok(ref v) => {
                            version =
                                VVal::new_str_mv(
                                    String::from_utf8_lossy(v).to_string());
                        },
                        Err(e) => {
                            return
                                Ok(env.new_err(
                                    format!("XML error at {}: {}",
                                            rd.buffer_position(),
                                            e)));
                        },
                    }

                    match e.encoding() {
                        Some(Ok(ref v)) => {
                            encoding =
                                VVal::new_str_mv(
                                    String::from_utf8_lossy(v).to_string());
                        },
                        Some(Err(e)) => {
                            return
                                Ok(env.new_err(
                                    format!("XML error at {}: {}",
                                            rd.buffer_position(),
                                            e)));
                        },
                        _ => ()
                    }

                    match e.standalone() {
                        Some(Ok(ref v)) => {
                            standalone =
                                VVal::new_str_mv(
                                    String::from_utf8_lossy(v).to_string());
                        },
                        Some(Err(e)) => {
                            return
                                Ok(env.new_err(
                                    format!("XML error at {}: {}",
                                            rd.buffer_position(),
                                            e)));
                        },
                        _ => ()
                    }

                    call_arg = Some(
                        VVal::vec4(
                            VVal::sym("decl"),
                            version,
                            encoding,
                            standalone));
                },
                Ok(Event::PI(ref t)) => {
                    call_arg = Some(
                        VVal::vec2(
                            VVal::sym("pi"),
                            unesc_to_vval_or_err!(env, rd, t)));
                },
                Ok(Event::DocType(ref t)) => {
                    call_arg = Some(
                        VVal::vec2(
                            VVal::sym("doctype"),
                            unesc_to_vval_or_err!(env, rd, t)));
                },
                Ok(Event::CData(ref t)) => {
                    call_arg = Some(
                        VVal::vec2(
                            VVal::sym("cdata"),
                            unesc_to_vval_or_err!(env, rd, t)));
                },
                Ok(Event::Text(t)) => {
                    call_arg = Some(
                        VVal::vec2(
                            VVal::sym("text"),
                            unesc_to_vval_or_err!(env, rd,  t)));
                },
                Ok(Event::End(ref e)) => {
                    call_arg = Some(
                        VVal::vec2(
                            VVal::sym("end"),
                            VVal::new_str_mv(
                                String::from_utf8_lossy(
                                    e.local_name()).to_string())));
                },
                Ok(Event::Eof) => break,
                Err(e) => {
                    return
                        Ok(env.new_err(
                            format!("XML parse error at {}: {}",
                                    rd.buffer_position(),
                                    e)));
                }
            }

            if let Some(call_arg) = call_arg {
                env.push(call_arg);
                match event_func.call_internal(env, 1) {
                    Ok(v)                      => { ret = v; },
                    Err(StackAction::Break(v)) => { ret = *v; break; },
                    Err(StackAction::Next)     => { },
                    Err(e)                     => { return Err(e); },
                };
                env.popn(1);
            }

            buf.clear();
        }

        Ok(ret)
    })
}
