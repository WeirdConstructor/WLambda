use crate::vval::{VVal, Env, VValFun};

use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};
use crate::ops::DirectFun;

use std::rc::Rc;
use std::cell::RefCell;

use crate::str_int::s2sym;

use std::fmt::Write;

fn parse_argument(ps: &mut State) -> Result<VVal, ParseError> {
    let mut is_integer = true;
    let mut identifier = String::new();
    let mut index      = String::new();

    while !ps.lookahead_one_of(":}") {
        match ps.expect_some(ps.peek())? {
            c if c.is_digit(10) => {
                ps.consume();
                if is_integer { index.push(c); }
                else          { identifier.push(c); }
            },
            c => {
                if !index.is_empty() {
                    identifier = index.clone();
                    is_integer = false;
                }
                ps.consume();

                identifier.push(c);
            }
        }
    }

    if is_integer {
        Ok(VVal::vec2(VVal::new_sym("index"), VVal::new_str_mv(index)))
    } else {
        Ok(VVal::vec2(VVal::new_sym("key"),   VVal::new_str_mv(identifier)))
    }
}

fn parse_format_spec(ps: &mut State) -> Result<VVal, ParseError> {
    Ok(VVal::None)
}

fn parse_format(ps: &mut State, implicit_index: &mut usize) -> Result<VVal, ParseError> {
    let mut arg = VVal::None;
    let mut fmt = VVal::None;

    let c = ps.expect_some(ps.peek())?;
    if c != ':' && c != '}' {
        arg = parse_argument(ps)?;
    } else {
        arg = VVal::vec2(VVal::new_sym("index"),
                         VVal::Int(*implicit_index as i64));
        *implicit_index = *implicit_index + 1;
    }

    let c = ps.expect_some(ps.peek())?;
    if c == ':' {
        ps.consume();
        fmt = parse_format_spec(ps)?;
    }

    if !ps.consume_if_eq('}') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken('}', "format end")));
    }

    Ok(VVal::vec3(VVal::new_sym("format"), arg, fmt))
}

fn parse_formatter(s: &str) -> Result<VVal, ParseError> {
    let mut ps = State::new(s, "<selector>");

    let mut fmt = VVal::vec();

    let mut cur_text = String::new();

    let mut impl_idx = 0;

    while !ps.at_end() {
        match ps.peek().unwrap() {
            '{' => {
                if ps.consume_lookahead("{{") {
                    cur_text.push('{');
                } else {
                    ps.consume();

                    if cur_text.len() > 0 {
                        fmt.push(VVal::vec2(VVal::new_sym("text"),
                                            VVal::new_str(&cur_text)));
                        cur_text = String::new();
                    }

                    fmt.push(parse_format(&mut ps, &mut impl_idx)?);
                }
            },
            '}' => {
                if ps.consume_lookahead("}}") {
                    cur_text.push('}');
                } else {
                    return Err(ps.err(
                        ParseErrorKind::UnexpectedToken('}', "format end")));
                }
            },
            c => { ps.consume(); cur_text.push(c); },
        }
    }

    if cur_text.len() > 0 {
        fmt.push(VVal::vec2(VVal::new_sym("text"), VVal::new_str_mv(cur_text)));
    }

    Ok(fmt)
}

#[derive(Debug, Clone)]
pub struct FormatState {
    str_data:  Option<String>,
    byte_data: Option<Vec<u8>>,
}

impl core::fmt::Write for FormatState {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        if let Some(sd) = &mut self.str_data.as_mut() {
            sd.push_str(s);
        } else if let Some(bd) = &mut self.byte_data.as_mut() {
            let r : &[u8] = s.as_ref();
            for b in r.iter() {
                bd.push(*b);
            }
        }
        Ok(())
    }
}

impl FormatState {
    fn add_char(&mut self, c: char) {
        if let Some(sd) = &mut self.str_data.as_mut() {
            sd.push(c);
        } else if let Some(bd) = &mut self.byte_data.as_mut() {
            let mut b = [0; 4];
            for cb in c.encode_utf8(&mut b).as_bytes().iter() {
                bd.push(*cb);
            }
        }
    }

    fn add_byte(&mut self, u: u8) {
        if let Some(sd) = &mut self.str_data.as_mut() {
            sd.push(std::char::from_u32(u as u32).unwrap());
        } else if let Some(bd) = &mut self.byte_data.as_mut() {
            bd.push(u);
        }
    }
}

pub type FormatNode = Box<dyn Fn(&mut FormatState)>;

pub fn compile_formatter(fmt: &VVal) -> (FormatNode, usize) {
    println!("COMPFMT[ {:?} ]", fmt);

    let mut fmts : Vec<FormatNode> = vec![];

    for (item, _) in fmt.iter() {
        let arg = item.at(1).unwrap_or_else(|| VVal::None);
        item.at(0).unwrap().with_s_ref(|syn| {
            match &syn[..] {
                "text" => {
                    fmts.push(Box::new(move |fs: &mut FormatState| {
                        arg.with_s_ref(|s| fs.write_str(s).unwrap());
                    }));
                },
                _ => {
                    panic!(format!("Unknown format spec: {}", item.s()));
//                    fmts.push(Box::new(|fs: &mut FormatState| {
//                        fs.add_char('?');
//                    }));
                }
            }
        })
    }
    (Box::new(move |fs: &mut FormatState| {
        for f in fmts.iter() {
            (*f)(fs);
        }
    }), 0)
}

pub fn create_formatter_fun(fmt: &VVal) -> Result<VVal, ParseError> {
    let fmt = fmt.at(1).unwrap();

    let (is_bytes, fmt_str) =
        if let VVal::Byt(bytes) = fmt {
            let mut s = String::new();
            for b in bytes.iter() {
                let c = std::char::from_u32(*b as u32).unwrap();
                s.push(c);
            }
            (true, s)
        } else {
            (false, fmt.s_raw())
        };

    let fmt = parse_formatter(&fmt_str)?;
    let (fun, argc) = compile_formatter(&fmt);

    Ok(if is_bytes {
        VValFun::new_fun(
            move |env: &mut Env, _argc: usize| {
                let mut out : Vec<u8> = Vec::new();
                let mut fs = FormatState {
                    str_data:   None,
                    byte_data:  Some(out),
                };
                (fun)(&mut fs);
                Ok(VVal::new_byt(fs.byte_data.take().unwrap()))
            }, None, None, false)
//            }, Some(argc), Some(argc), false)
    } else {
        VValFun::new_fun(
            move |env: &mut Env, _argc: usize| {
                let mut out = String::new();
                let mut fs = FormatState {
                    str_data:   Some(out),
                    byte_data:  None,
                };
                (fun)(&mut fs);
                println!("STR: {:?}", fs);
                Ok(VVal::new_str_mv(fs.str_data.take().unwrap()))
            }, None, None, false)
//            }, Some(argc), Some(argc), false)
    })
}
