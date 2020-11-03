use crate::{Env, VVal, StackAction};
use crate::vval::VValFun;
use quick_xml::Reader;
use quick_xml::Writer;
use quick_xml::events::{Event, BytesStart, BytesEnd, BytesText, BytesDecl};

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
    e: &BytesStart<'a>)
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
pub(crate) fn read_sax(env: &mut Env, input: VVal, event_func: VVal, trim: bool)
    -> Result<VVal, StackAction> {

    input.with_s_ref(|xml| {
        let mut buf = Vec::new();
        let mut rd  = Reader::from_str(xml);
        let mut ret = VVal::None;

        if trim {
            rd.trim_text(true);
        }

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
                                    e.name()).to_string()),
                            attrs));
                },
                Ok(Event::Empty(ref e)) => {
                    let attrs = xml_start_attrs(env, &rd, e)?;

                    call_arg = Some(
                        VVal::vec3(
                            VVal::sym("empty"),
                            VVal::new_str_mv(
                                String::from_utf8_lossy(
                                    e.name()).to_string()),
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
                                    e.name()).to_string())));
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

macro_rules! write_event {
    ($env: ident, $writer: ident, $event: expr) => {
        if let Err(e) = $writer.borrow_mut().write_event($event) {
            return Ok($env.new_err(format!("XML writer error: {}", e)));
        } else {
            Ok(VVal::None)
        }
    }
}

pub(crate) fn create_sax_writer(indent_depth: Option<usize>) -> VVal {
    let writer =
        if let Some(indent_depth) = indent_depth {
            Writer::new_with_indent(
                std::io::Cursor::new(Vec::new()),
                b' ', indent_depth)
        } else {
            Writer::new(std::io::Cursor::new(Vec::new()))
        };

    let writer = std::rc::Rc::new(std::cell::RefCell::new(writer));

    VValFun::new_fun(
        move |env: &mut Env, argc: usize| {
            if argc == 0 {
                let writer = writer.replace(
                    if let Some(indent_depth) = indent_depth {
                        Writer::new_with_indent(
                            std::io::Cursor::new(Vec::new()),
                            b' ', indent_depth)
                    } else {
                        Writer::new(std::io::Cursor::new(Vec::new()))
                    });
                let res = writer.into_inner().into_inner();
                return Ok(VVal::new_str_mv(
                    String::from_utf8(res)
                        .expect("XML writer should output correct UTF-8")));
            }

            let elem = env.arg(0);
            elem.v_with_s_ref(0, |s| {
                match &s[..] {
                    "start" => {
                        elem.v_with_s_ref(1, |name| {
                            let mut bytes =
                                BytesStart::borrowed(
                                    name.as_bytes(),
                                    name.as_bytes().len());

                            for (v, k) in elem.v_(2).iter() {
                                if let Some(k) = k {
                                    v.with_s_ref(|v|
                                        k.with_s_ref(|k|
                                            bytes.push_attribute((k, v))));
                                }
                            }

                            write_event!(env, writer, Event::Start(bytes))
                        })
                    },
                    "empty" => {
                        elem.v_with_s_ref(1, |name| {
                            let mut bytes =
                                BytesStart::borrowed(
                                    name.as_bytes(),
                                    name.as_bytes().len());

                            for (v, k) in elem.v_(2).iter() {
                                if let Some(k) = k {
                                    v.with_s_ref(|v|
                                        k.with_s_ref(|k|
                                            bytes.push_attribute((k, v))));
                                }
                            }

                            write_event!(env, writer, Event::Empty(bytes))
                        })
                    },
                    "end" => {
                        elem.v_with_s_ref(1, |name|
                            write_event!(
                                env, writer,
                                Event::End(BytesEnd::borrowed(name.as_bytes()))))
                    },
                    "text" => {
                        elem.v_with_s_ref(1, |text|
                            write_event!(
                                env, writer,
                                Event::Text(BytesText::from_plain_str(text))))
                    },
                    "comment" => {
                        elem.v_with_s_ref(1, |text|
                            write_event!(
                                env, writer,
                                Event::Comment(BytesText::from_plain_str(text))))
                    },
                    "pi" => {
                        elem.v_with_s_ref(1, |text|
                            write_event!(
                                env, writer,
                                Event::PI(BytesText::from_plain_str(text))))
                    },
                    "doctype" => {
                        elem.v_with_s_ref(1, |text|
                            write_event!(
                                env, writer,
                                Event::DocType(BytesText::from_plain_str(text))))
                    },
                    "cdata" => {
                        elem.v_with_s_ref(1, |text|
                            write_event!(
                                env, writer,
                                Event::CData(BytesText::from_plain_str(text))))
                    },
                    "decl" => {
                        elem.v_with_s_ref(1, |version| {
                            let encoding =
                                if elem.v_(2).is_some() {
                                    Some(elem.v_s_raw(2))
                                } else {
                                    None
                                };
                            let standalone =
                                if elem.v_(3).is_some() {
                                    Some(elem.v_s_raw(3))
                                } else {
                                    None
                                };

                            write_event!(
                                env, writer,
                                Event::Decl(BytesDecl::new(
                                    version.as_bytes(),
                                    encoding.as_ref().map(|e| e.as_bytes()),
                                    standalone.as_ref().map(|s| s.as_bytes()))))
                        })
                    },
                    _ => Ok(VVal::None),
                }
            })
        }, Some(0), Some(1), false)
}

#[derive(Clone, Debug)]
struct VValBuilder {
    stack:       std::vec::Vec<VVal>,
    cur:         VVal,
    sym_decl:    VVal,
    sym_comment: VVal,
    sym_text:    VVal,
    sym_doctype: VVal,
    sym_cdata:   VVal,
    sym_pi:      VVal,
    sym_elem:    VVal,
}

impl VValBuilder {
    pub fn new() -> Self {
        Self {
            stack:       vec![],
            cur:         VVal::vec4(VVal::sym("root"), VVal::None, VVal::None, VVal::vec()),
            sym_decl:    VVal::sym("decl"),
            sym_comment: VVal::sym("comment"),
            sym_text:    VVal::sym("text"),
            sym_doctype: VVal::sym("doctype"),
            sym_cdata:   VVal::sym("cdata"),
            sym_pi:      VVal::sym("pi"),
            sym_elem:    VVal::sym("elem"),
        }
    }

    //
    // $[:decl,    ${ version=..., encoding=..., standalone=... }]
    // $[:elem,    name, ${ attrs }, $[childs]]
    // $[:comment, text]
    // $[:pi,      text]
    // $[:text,    text]
    // $[:doctype, text]
    // $[:cdata,   text]
    //
    pub fn event(&mut self, elem: &VVal) {
        elem.v_with_s_ref(0, |s| {
            match &s[..] {
                "start" => {
                    self.stack.push(std::mem::replace(&mut self.cur,
                        VVal::vec4(
                            self.sym_elem.clone(),
                            elem.v_(1),
                            elem.v_(2),
                            VVal::vec())));
                },
                "end" => {
                    let elem = self.stack.pop().unwrap_or(VVal::None);
                    elem.v_(3).push(self.cur.clone());
                    self.cur = elem;
                },
                "empty" => {
                    self.cur.v_(3).push(
                        VVal::vec4(
                            self.sym_elem.clone(),
                            elem.v_(1),
                            elem.v_(2),
                            VVal::None));
                },
                "text" => {
                    self.cur.v_(3).push(
                        VVal::vec2(
                            self.sym_text.clone(),
                            elem.v_(1)));
                },
                "comment" => {
                    self.cur.v_(3).push(
                        VVal::vec2(
                            self.sym_comment.clone(),
                            elem.v_(1)));
                },
                "pi" => {
                    self.cur.v_(3).push(
                        VVal::vec2(
                            self.sym_pi.clone(),
                            elem.v_(1)));
                },
                "doctype" => {
                    self.cur.v_(3).push(
                        VVal::vec2(
                            self.sym_doctype.clone(),
                            elem.v_(1)));
                },
                "cdata" => {
                    self.cur.v_(3).push(
                        VVal::vec2(
                            self.sym_cdata.clone(),
                            elem.v_(1)));
                },
                "decl" => {
                    self.cur.v_(3).push(
                        VVal::vec2(
                            self.sym_decl.clone(),
                            VVal::map3(
                                "version",    elem.v_(1),
                                "encoding",   elem.v_(2),
                                "standalone", elem.v_(3))));
                },
                _ => {
                },
            }
        })
    }

    pub fn result(&self) -> VVal { self.cur.clone() }
}
