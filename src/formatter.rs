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

    while !ps.lookahead_one_of(":$}") {
        match ps.expect_some(ps.peek())? {
            c if c.is_digit(10) => {
                ps.consume();
                if is_integer { index.push(c); }
                else          { identifier.push(c); }
            },
            c => {
                if !index.is_empty() {
                    identifier = index.clone();
                }
                ps.consume();

                identifier.push(c);

                is_integer = false;
            }
        }
    }

    if is_integer {
        Ok(VVal::vec2(VVal::new_sym("index"), VVal::new_str_mv(index)))
    } else {
        Ok(VVal::vec2(VVal::new_sym("key"),   VVal::new_str_mv(identifier)))
    }
}

fn parse_count(ps: &mut State) -> Result<VVal, ParseError> {
    if let Some(_) = ps.find_char('$') {
        parse_argument(ps)
    } else {
        let integer = ps.take_while(|c| c.is_digit(10));
        if let Ok(idx) = u32::from_str_radix(&integer.to_string(), 10) {
            Ok(VVal::vec2(VVal::new_sym("count"), VVal::Int(idx as i64)))
        } else {
            Err(ps.err(
                ParseErrorKind::BadFormat(
                    "expected proper integer".to_string())))
        }
    }
}

fn parse_format_spec(ps: &mut State, arg: &VVal) -> Result<VVal, ParseError> {
    let mut fill       : Option<char> = None;
    let mut align_char : Option<char> = None;

    let align = ps.peek2();
    if let Some(align) = align {
        match align.at(1) {
              c @ '<'
            | c @ '^'
            | c @ '>' => {
                fill       = Some(align.at(0));
                align_char = Some(c);
                ps.consume();
                ps.consume();
            },
            _ => { },
        }
    }

    if align_char .is_none() {
        match ps.expect_some(ps.peek())? {
              c @ '<'
            | c @ '^'
            | c @ '>' => {
                align_char  = Some(c);
                ps.consume();
            },
            _ => { },
        }
    }

    let mut sign : Option<char> = None;
    match ps.expect_some(ps.peek())? {
          c @ '-'
        | c @ '+' => {
            sign = Some(c);
            ps.consume();
        },
        _ => { },
    }

    let alternate = ps.consume_if_eq('#');
    let int_pad0  = ps.consume_if_eq('0');

    let width = parse_count(ps)?;

    let precision =
        if ps.consume_if_eq('.') {
            if ps.consume_if_eq('*') {
                if arg.at(0).unwrap().with_s_ref(|s| s == "index") {
                    let idx = arg.at(1).unwrap();
                    arg.set(1, VVal::Int(idx.i() + 1));

                    VVal::vec2(VVal::new_sym("index"), idx)
                } else {
                    return Err(ps.err(
                        ParseErrorKind::BadFormat(
                            "can't use * in combination with named arg".to_string())));
                }
            } else {
                parse_count(ps)?
            }
        } else {
            VVal::None
        };

    let m = VVal::map();
    if let Some(fill) = fill {
        m.set_key_str("fill",   VVal::new_str_mv(fill.to_string()));
    }
    match align_char {
        Some('<') => { m.set_key_str("align", VVal::Int(-1)); },
        Some('>') => { m.set_key_str("align", VVal::Int( 1)); },
        Some('^') => { m.set_key_str("align", VVal::Int( 2)); },
        _         => (),
    }
    match sign {
        Some('-') => { m.set_key_str("sign", VVal::Int(-1)); },
        Some('+') => { m.set_key_str("sign", VVal::Int( 1)); },
        _         => (),
    }
    m.set_key_str("alternate",  VVal::Bol(alternate));
    m.set_key_str("int_pad0",   VVal::Bol(int_pad0));
    m.set_key_str("precision",  precision);
    m.set_key_str("width",      width);

    Ok(m)
}

fn parse_format(ps: &mut State, implicit_index: &mut usize) -> Result<VVal, ParseError> {
    let mut arg = VVal::None;
    let mut fmt = VVal::None;

    let mut was_implicit_idx = false;
    let c = ps.expect_some(ps.peek())?;
    if c != ':' && c != '}' {
        arg = parse_argument(ps)?;
    } else {
        arg = VVal::vec2(VVal::new_sym("index"),
                         VVal::Int(*implicit_index as i64));
        *implicit_index = *implicit_index + 1;
        was_implicit_idx = true;
    }


    let c = ps.expect_some(ps.peek())?;
    if c == ':' {
        ps.consume();

        let arg_idx_prev = arg.at(1).unwrap_or_else(|| VVal::None);
        fmt = parse_format_spec(ps, &arg)?;
        let arg_idx_after = arg.at(1).unwrap_or_else(|| VVal::None);

        // There is some magic patching going on in parse_format_spec.
        // Not entirely clean. But it might get the job done for now.
        if was_implicit_idx && !arg_idx_prev.eqv(&arg_idx_after) {
            *implicit_index = *implicit_index + 1;
        }
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

pub enum FormatArg {
    Index(usize),
    Key(VVal),
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

pub type FormatNode = Box<dyn Fn(&mut FormatState, &[VVal])>;

pub fn compile_format(arg: FormatArg, fmt: &VVal) -> FormatNode {
    Box::new(move |fs: &mut FormatState, args: &[VVal]| {
        match &arg {
            FormatArg::Index(i) => {
                args[*i].with_s_ref(|s| fs.write_str(s));
            },
            FormatArg::Key(k) => {
                let val =
                    k.with_s_ref(|ks|
                        args[0].get_key(ks).unwrap_or_else(|| VVal::None));
                val.with_s_ref(|s| fs.write_str(s));
            },
        }
        // For any align: let the lower node write, and check
        // how the buffer changed.
        // then do something like: fs.pad_from_idx_with(last_idx, field_width)
//        fs.write_str(s).unwrap()
    })
}

pub fn compile_formatter(fmt: &VVal) -> (FormatNode, usize) {
    println!("COMPFMT[ {} ]", fmt.s());

    let mut fmts : Vec<FormatNode> = vec![];

    for (item, _) in fmt.iter() {
        let arg = item.at(1).unwrap_or_else(|| VVal::None);
        item.at(0).unwrap().with_s_ref(|syn| {
            match &syn[..] {
                "text" => {
                    fmts.push(Box::new(move |fs: &mut FormatState, _args: &[VVal]| {
                        arg.with_s_ref(|s| fs.write_str(s).unwrap());
                    }));
                },
                _ => {
                    let arg =
                        arg.at(0).unwrap().with_s_ref(|arg_syn| {
                            match &arg_syn[..] {
                                "index" => FormatArg::Index(arg.v_i(1) as usize),
                                _       => FormatArg::Key(arg.v_(1)),
                            }
                        });
                    fmts.push(
                        compile_format(
                            arg,
                            &item.at(2).unwrap_or_else(|| VVal::None)));
//                    panic!(format!("Unknown format spec: {}", item.s()));
//                    fmts.push(Box::new(|fs: &mut FormatState| {
//                        fs.add_char('?');
//                    }));
                }
            }
        })
    }
    (Box::new(move |fs: &mut FormatState, args: &[VVal]| {
        for f in fmts.iter() {
            (*f)(fs, args);
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
                (fun)(&mut fs, env.argv_ref());
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
                (fun)(&mut fs, env.argv_ref());
                println!("STR: {:?}", fs);
                Ok(VVal::new_str_mv(fs.str_data.take().unwrap()))
            }, None, None, false)
//            }, Some(argc), Some(argc), false)
    })
}
