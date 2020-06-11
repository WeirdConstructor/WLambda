use crate::vval::{VVal, Env, VValFun};

use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};
use crate::ops::DirectFun;

use std::rc::Rc;
use std::cell::RefCell;

use crate::str_int::s2sym;

fn parse_argument(ps: &mut State) -> Result<VVal, ParseError> {
    let mut is_integer = true;
    let mut identifier = String::new();
    let mut index      = String::new();

    while !ps.lookahead_one_of(":}") {
        match ps.expect_some(ps.peek())? {
            c if c.is_digit(10) => {
                if is_integer { index.push(c); }
                else          { identifier.push(c); }
            },
            c => {
                if !index.is_empty() {
                    identifier = index.clone();
                    is_integer = false;
                }

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

fn parse_format(ps: &mut State, implicit_index: usize) -> Result<VVal, ParseError> {
    let mut arg = VVal::None;
    let mut fmt = VVal::None;

    let c = ps.expect_some(ps.peek())?;
    if c != ':' {
        arg = parse_argument(ps)?;
    }

    let c = ps.expect_some(ps.peek())?;
    if c == ':' {
        ps.consume();
        fmt = parse_format_spec(ps)?;
    }

    Ok(VVal::vec3(VVal::new_sym("format"), arg, fmt))
}

fn parse_formatter(s: &str) -> Result<VVal, ParseError> {
    let mut ps = State::new(s, "<selector>");

    let mut fmt = VVal::vec();

    let mut cur_text = String::new();

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

                }
            },
            '}' => {
                if ps.consume_lookahead("}}") {
                    cur_text.push('}');
                } else {
                    ps.consume();

                    if cur_text.len() > 0 {
                        fmt.push(VVal::vec2(VVal::new_sym("text"),
                                            VVal::new_str(&cur_text)));
                        cur_text = String::new();
                    }

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
    (Box::new(|fs: &mut FormatState| { fs.add_char('X'); }), 0)
}

pub fn create_formatter_fun(fmt: &VVal) -> Result<VVal, ParseError> {
    let (is_bytes, fmt_str) =
        if fmt.is_bytes() {
            let mut s = String::new();
            for c in fmt.s().chars().map(|u| std::char::from_u32(u as u32).unwrap()) {
                s.push(c);
            }
            (true, s)
        } else {
            (false, fmt.s())
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
            }, Some(argc), Some(argc), false)
    } else {
        VValFun::new_fun(
            move |env: &mut Env, _argc: usize| {
                let mut out = String::new();
                let mut fs = FormatState {
                    str_data:   Some(out),
                    byte_data:  None,
                };
                (fun)(&mut fs);
                Ok(VVal::new_str_mv(fs.str_data.take().unwrap()))
            }, Some(argc), Some(argc), false)
    })
}
