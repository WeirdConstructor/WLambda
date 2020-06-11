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
            c => { cur_text.push(c); },
        }
    }

    if cur_text.len() > 0 {
        fmt.push(VVal::vec2(VVal::new_sym("text"), VVal::new_str_mv(cur_text)));
    }

    Ok(fmt)
}

pub type FormatPush = dyn Fn(char, Option<&mut String>, Option<&mut Vec<u8>>);
pub type FormatNode = Box<dyn Fn(&FormatPush, Option<&mut String>, Option<&mut Vec<u8>>)>;

pub fn compile_formatter(fmt: &VVal) -> FormatNode {
    Box::new(|add: &FormatPush, rs: Option<&mut String>, rb: Option<&mut Vec<u8>>| {
        add('X', rs, rb);
    })
}

pub fn create_formatter_direct_fun(fmt: &VVal)
    -> Result<DirectFun, ParseError>
{
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
    let fun = compile_formatter(&fmt);

    Ok(if is_bytes {
        DirectFun::new(Rc::new(move |v: VVal, _env: &mut Env| {
            let mut out : Vec<u8> = Vec::new();
            (fun)(&|c, _rs, rb| { (*rb.as_ref().unwrap()).push(c as u32 as u8); }, None, Some(&mut out));
            VVal::new_byt(out)
        }))
    } else {
        DirectFun::new(Rc::new(move |v: VVal, _env: &mut Env| {
            let mut out = String::new();
            (fun)(&|c, rs, _rb| { (*rs.as_ref().unwrap()).push(c); }, Some(&mut out), None);
            VVal::new_str_mv(out)
        }))
    })
}
