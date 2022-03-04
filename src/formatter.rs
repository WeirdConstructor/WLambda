// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This is a string formatter for WLambda.

It provides the [create_formatter_fun]. It's syntax supports most of the
Rust string formatting functionality.
*/

use crate::vval::{VVal, Env, VValFun};

use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};

use std::fmt::Write;

fn parse_argument(ps: &mut State) -> Result<VVal, ParseError> {
    let mut is_integer = true;
    let mut identifier = String::new();
    let mut index      = String::new();

    while !ps.lookahead_one_of(":$.}") {
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
    if ps.find_char_not_of('$', ".!").is_some() {
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum CastType {
    Str     = 0,
    Int     = 1,
    Flt     = 2,
    Written = 4,
}

impl CastType {
    fn from_vval(v: &VVal) -> Self {
        match v.i() {
            1 => CastType::Int,
            2 => CastType::Flt,
            4 => CastType::Written,
            _ => CastType::Str,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum FormatType {
    Unknown = 0,
    Hex     = 1,
    Oct     = 2,
    Bin     = 3,
}

impl FormatType {
    fn from_vval(v: &VVal) -> Self {
        match v.i() {
            1 => FormatType::Hex,
            2 => FormatType::Oct,
            3 => FormatType::Bin,
            _ => FormatType::Unknown,
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
                align_char = Some(c);
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
    let pad0      = ps.consume_if_eq('0');

    let width =
        if    ps.find_char_not_of('$', ".").is_some()
           || (   ps.peek().is_some()
               && ps.lookahead_one_of("0123456789")) {
            parse_count(ps)?
        } else {
            VVal::None
        };

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

    let mut cast_type =
        if ps.consume_if_eq('!') {
            match ps.expect_some(ps.peek())? {
                'i' => { ps.consume(); Some(CastType::Int) },
                'f' => { ps.consume(); Some(CastType::Flt) },
                'w' => { ps.consume(); Some(CastType::Written) },
                c => {
                    return Err(ps.err(
                        ParseErrorKind::BadFormat(
                            format!("Unknown cast type: {}", c))));
                }
            }
        } else {
            None
        };

    let m = VVal::map();

    match ps.expect_some(ps.peek())? {
        'x' => {
            ps.consume();
            m.set_key_str("type", VVal::Int(FormatType::Hex as i64)).unwrap();
        },
        'o' => {
            ps.consume();
            m.set_key_str("type", VVal::Int(FormatType::Oct as i64)).unwrap();
        },
        'b' => {
            ps.consume();
            m.set_key_str("type", VVal::Int(FormatType::Bin as i64)).unwrap();
        },
        _ => {
            m.set_key_str("type", VVal::Int(FormatType::Unknown as i64)).unwrap();
        }
    }

    if let Some(fill) = fill {
        m.set_key_str("fill", VVal::new_str_mv(fill.to_string())).unwrap();
    } else {
        m.set_key_str("fill", VVal::new_str(" ")).unwrap();
    }
    match align_char {
        Some('<') => { m.set_key_str("align", VVal::Int(-1)).unwrap(); },
        Some('>') => { m.set_key_str("align", VVal::Int( 1)).unwrap(); },
        Some('^') => { m.set_key_str("align", VVal::Int( 2)).unwrap(); },
        _         => (),
    }
    match sign {
        Some('-') => { m.set_key_str("sign", VVal::Int(-1)).unwrap(); },
        Some('+') => { m.set_key_str("sign", VVal::Int( 1)).unwrap(); },
        _         => (),
    }
    m.set_key_str("alternate",  VVal::Bol(alternate)).unwrap();
    m.set_key_str("pad0",       VVal::Bol(pad0)).unwrap();
    if width.is_some() {
        m.set_key_str("width", width).unwrap();
    }
    if precision.is_some() {
        m.set_key_str("precision", precision).unwrap();
        if cast_type.is_none() {
            cast_type = Some(CastType::Flt);
        }
    }
    if let Some(ct) = cast_type {
        m.set_key_str("cast_type", VVal::Int(ct as i64)).unwrap();
    } else {
        m.set_key_str("cast_type", VVal::Int(CastType::Str as i64)).unwrap();
    }

    Ok(m)
}

fn parse_format(ps: &mut State, implicit_index: &mut usize) -> Result<VVal, ParseError> {
    let mut fmt = VVal::None;

    let mut was_implicit_idx = false;
    let c = ps.expect_some(ps.peek())?;
    let arg =
        if c != ':' && c != '}' {
            parse_argument(ps)?
        } else {
            let arg = VVal::vec2(VVal::new_sym("index"),
                             VVal::Int(*implicit_index as i64));
            *implicit_index += 1;
            was_implicit_idx = true;
            arg
        };


    let c = ps.expect_some(ps.peek())?;
    if c == ':' {
        ps.consume();

        let arg_idx_prev = arg.at(1).unwrap_or_else(|| VVal::None);
        fmt = parse_format_spec(ps, &arg)?;
        let arg_idx_after = arg.at(1).unwrap_or_else(|| VVal::None);

        // There is some magic patching going on in parse_format_spec.
        // Not entirely clean. But it might get the job done for now.
        if was_implicit_idx && !arg_idx_prev.eqv(&arg_idx_after) {
            *implicit_index += 1;
        }
    }

    if !ps.consume_if_eq('}') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken('}', "format end")));
    }

    Ok(VVal::vec3(VVal::new_sym("format"), arg, fmt))
}

fn parse_formatter(s: &str) -> Result<VVal, ParseError> {
    let mut ps = State::new_verbatim(s, "<selector>");

    let fmt = VVal::vec();

    let mut cur_text = String::new();

    let mut impl_idx = 0;

    while !ps.at_end() {
        match ps.peek().unwrap() {
            '{' => {
                if ps.consume_lookahead("{{") {
                    cur_text.push('{');
                } else {
                    ps.consume();

                    if !cur_text.is_empty() {
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

    if !cur_text.is_empty() {
        fmt.push(VVal::vec2(VVal::new_sym("text"), VVal::new_str_mv(cur_text)));
    }

    Ok(fmt)
}

#[derive(Debug, Clone)]
pub(crate) struct FormatState {
    str_data:  Option<String>,
    byte_data: Option<Vec<u8>>,
}

pub(crate) enum FormatArg {
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
//    fn add_char(&mut self, c: char) {
//        if let Some(sd) = &mut self.str_data.as_mut() {
//            sd.push(c);
//        } else if let Some(bd) = &mut self.byte_data.as_mut() {
//            let mut b = [0; 4];
//            for cb in c.encode_utf8(&mut b).as_bytes().iter() {
//                bd.push(*cb);
//            }
//        }
//    }

    fn cur_len(&self) -> usize {
        if let Some(sd) = &self.str_data.as_ref() {
            sd.len()
        } else if let Some(bd) = &self.byte_data.as_ref() {
            bd.len()
        } else {
            0
        }
    }

    fn insert_at(&mut self, idx: usize, s: &str) {
        if let Some(sd) = &mut self.str_data.as_mut() {
            sd.insert_str(idx, s)
        } else if let Some(bd) = &mut self.byte_data.as_mut() {
            for c in s.chars() {
                let mut b = [0; 4];
                for cb in c.encode_utf8(&mut b).as_bytes().iter() {
                    bd.insert(idx, *cb);
                }
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

pub(crate) type FormatNode = Box<dyn Fn(&mut FormatState, &[VVal]) -> std::fmt::Result>;
pub(crate) type CountNode  = Box<dyn Fn(&mut FormatState, &[VVal]) -> usize>;

pub(crate) fn compile_count(count: &VVal) -> CountNode {
    if count.v_with_s_ref(0, |s| s == "count") {
        let count = count.v_i(1);
        Box::new(move |_fs: &mut FormatState, _args: &[VVal]| -> usize {
            count as usize
        })
    } else {
        let count = count.clone();
        Box::new(move |_fs: &mut FormatState, _args: &[VVal]| -> usize {
            panic!("other count not yet supported! {}", count.s());
        })
    }
}

#[allow(clippy::many_single_char_names)]
pub(crate) fn write_vval<F>(arg: &VVal, fs: &mut FormatState, ct: CastType, mut f: F) -> std::fmt::Result
    where F: FnMut(&mut FormatState, &VVal) -> std::fmt::Result
{
    use crate::nvec::NVec;

    if let CastType::Written = ct {
        return f(fs, &arg);
    }

    match arg {
        VVal::IVec(v) => {
            write!(fs, "(")?;
            match v.as_ref() {
                NVec::Vec2(a, b) => {
                    f(fs, &VVal::Int(*a))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Int(*b))?;
                },
                NVec::Vec3(a, b, c) => {
                    f(fs, &VVal::Int(*a))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Int(*b))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Int(*c))?;
                },
                NVec::Vec4(a, b, c, d) => {
                    f(fs, &VVal::Int(*a))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Int(*b))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Int(*c))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Int(*d))?;
                },
            }
            write!(fs, ")")?;
        },
        VVal::FVec(v) => {
            write!(fs, "(")?;
            match v.as_ref() {
                NVec::Vec2(a, b) => {
                    f(fs, &VVal::Flt(*a))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Flt(*b))?;
                },
                NVec::Vec3(a, b, c) => {
                    f(fs, &VVal::Flt(*a))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Flt(*b))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Flt(*c))?;
                },
                NVec::Vec4(a, b, c, d) => {
                    f(fs, &VVal::Flt(*a))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Flt(*b))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Flt(*c))?;
                    write!(fs, ",")?;
                    f(fs, &VVal::Flt(*d))?;
                },
            }
            write!(fs, ")")?;
        },
        VVal::Pair(ab) => {
            let (a, b) = ab.as_ref();
            write!(fs, "(")?;
            f(fs, a)?;
            write!(fs, ",")?;
            f(fs, b)?;
            write!(fs, ")")?;
        },
        VVal::Lst(_) => {
            write!(fs, "[")?;
            let mut first = true;
            for (a, _) in arg.iter() {
                if first { first = false; }
                else     { write!(fs, ",")?; }

                f(fs, &a)?;
            }
            write!(fs, "]")?;
        },
        VVal::Map(m) => {
            use crate::str_int::*;
            write!(fs, "{{")?;
            let hm = m.borrow();
            let mut keys : Vec<&Symbol> = hm.keys().collect();
            keys.sort();

            let mut first = true;
            for k in keys {
                if first { first = false; }
                else     { write!(fs, ", ")?; }

                let v = hm.get(k).unwrap();
                write!(fs, "{}:", k.as_ref())?;
                f(fs, &v)?;
            }

            write!(fs, "}}")?;
        },
        v => f(fs, v)?,
    }
    Ok(())
}

pub(crate) fn with_format_arg_write<F>(arg: &FormatArg, args: &[VVal], fs: &mut FormatState, ct: CastType, f: F) -> std::fmt::Result
    where F: FnMut(&mut FormatState, &VVal) -> std::fmt::Result
{
    match arg {
        FormatArg::Index(i) => { write_vval(&args[*i], fs, ct, f) },
        FormatArg::Key(k) => {
            let val =
                k.with_s_ref(|ks|
                    args[0].get_key(ks).unwrap_or_else(|| VVal::None));
            write_vval(&val, fs, ct, f)
        },
    }
}

macro_rules! ft_align_write {
    ($ftype: ident, $align: ident, $fs: ident, $prefix: literal, $postfix: expr, $arg0: expr, $arg1: expr) => {
        match $ftype {
            FormatType::Bin =>
                align_write!($align, $fs, $prefix, concat!($postfix, "b"), $arg0, $arg1),
            FormatType::Oct =>
                align_write!($align, $fs, $prefix, concat!($postfix, "o"), $arg0, $arg1),
            FormatType::Hex =>
                align_write!($align, $fs, $prefix, concat!($postfix, "x"), $arg0, $arg1),
            FormatType::Unknown =>
                align_write!($align, $fs, $prefix, $postfix, $arg0, $arg1),
        }
    };
}


macro_rules! align_write {
    ($align: ident, $fs: ident, $prefix: literal, $postfix: expr, $arg0: expr, $arg1: expr) => {
        match $align {
            1 => { write!($fs, concat!("{", $prefix, ">", $postfix, "}"), $arg0, $arg1) },
            2 => { write!($fs, concat!("{", $prefix, "^", $postfix, "}"), $arg0, $arg1) },
            _ => { write!($fs, concat!("{", $prefix, "<", $postfix, "}"), $arg0, $arg1) },
        }
    };
    ($align: ident, $fs: ident, $prefix: literal, $postfix: expr, $arg0: expr, $arg1: expr, $arg2: expr) => {
        match $align {
            1 => { write!($fs, concat!("{", $prefix, ">", $postfix, "}"), $arg0, $arg1, $arg2) },
            2 => { write!($fs, concat!("{", $prefix, "^", $postfix, "}"), $arg0, $arg1, $arg2) },
            _ => { write!($fs, concat!("{", $prefix, "<", $postfix, "}"), $arg0, $arg1, $arg2) },
        }
    };
}

pub(crate) type AlignNode  = Box<dyn Fn(&mut FormatState, &[VVal], usize) -> std::fmt::Result>;

pub(crate) fn compile_align_fun(fill: String, width: Option<CountNode>, align: i64) -> AlignNode {
    if let Some(width) = width {
        Box::new(move |fs: &mut FormatState, args: &[VVal], len: usize| -> std::fmt::Result {
            let align = if align == 0 { -1 } else { align };

            let width = (*width)(fs, args);
            if width > len {
                let pad_len = width - len;

                match align {
                    1  => {
                        let idx = fs.cur_len() - len;
                        let pad = fill.repeat(pad_len);
                        fs.insert_at(idx, &pad);
                    },
                    2  => {
                        let first_half_pad  = pad_len / 2;
                        let second_half_pad =
                            if first_half_pad <= pad_len {
                                pad_len - first_half_pad
                            } else {
                                0
                            };
                        let idx = fs.cur_len() - len;
                        let pad_l = fill.repeat(first_half_pad);
                        let pad_r = fill.repeat(second_half_pad);
                        fs.insert_at(idx, &pad_l);
                        fs.insert_at(fs.cur_len(), &pad_r);
                    },
                    -1 => {
                        let pad = fill.repeat(pad_len);
                        fs.insert_at(fs.cur_len(), &pad);
                    },
                    _  => (),
                }
            }

            Ok(())
        })
    } else {
        Box::new(move |_fs: &mut FormatState, _args: &[VVal], _len: usize| -> std::fmt::Result {
            Ok(())
        })
    }
}

#[allow(clippy::collapsible_if)]
#[allow(clippy::collapsible_else_if)]
pub(crate) fn compile_format(arg: FormatArg, fmt: &VVal) -> FormatNode {
    let width = fmt.get_key("width");
    let width : Option<CountNode> =
        if let Some(width) = width {
            Some(compile_count(&width))
        } else {
            None
        };

    let prec = fmt.get_key("precision");
    let prec : Option<CountNode> =
        if let Some(prec) = prec {
            Some(compile_count(&prec))
        } else {
            None
        };

    let align = fmt.v_ik("align");
    let mut fill  = fmt.v_s_rawk("fill");
    if fill == "" {
        fill = String::from(" ");
    }

    let ftype = FormatType::from_vval(&fmt.v_k("type"));

    let cast_type = CastType::from_vval(&fmt.v_k("cast_type"));

    match cast_type {
        CastType::Int => {
            if let Some(width) = width {
                let align =
                    if align == 0 { 1 } else { align };

                if fmt.v_ik("pad0") > 0 {
                    Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                        let width = (*width)(fs, args);

                        with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                            ft_align_write!(ftype, align, fs, "0:", "01$", v.i(), width)
                        })
                    })

                } else {
                    Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                        let width = (*width)(fs, args);

                        with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                            ft_align_write!(ftype, align, fs, "0:", "1$", v.i(), width)
                        })
                    })
                }

            } else {
                match ftype {
                    FormatType::Bin =>
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                write!(fs, concat!("{:b}"), v.i())
                            })
                        }),
                    FormatType::Oct =>
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                write!(fs, concat!("{:o}"), v.i())
                            })
                        }),
                    FormatType::Hex =>
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                write!(fs, concat!("{:x}"), v.i())
                            })
                        }),
                    FormatType::Unknown =>
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                write!(fs, concat!("{}"), v.i())
                            })
                        })
                }
            }
        },
        CastType::Flt => {
            if let Some(width) = width {
                let align =
                    if align == 0 { 1 } else { align };

                if let Some(prec) = prec {
                    if fmt.v_ik("pad0") > 0 {
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            let width = (*width)(fs, args);
                            let prec  = (*prec)(fs, args);

                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                align_write!(align, fs, "0:", "01$.2$", v.f(), width, prec)
                            })
                        })

                    } else {
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            let width = (*width)(fs, args);
                            let prec  = (*prec)(fs, args);

                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                align_write!(align, fs, "0:", "1$.2$", v.f(), width, prec)
                            })
                        })
                    }
                } else {
                    if fmt.v_ik("pad0") > 0 {
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            let width = (*width)(fs, args);

                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                align_write!(align, fs, "0:", "01$", v.f(), width)
                            })
                        })

                    } else {
                        Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                            let width = (*width)(fs, args);

                            with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                                align_write!(align, fs, "0:", "1$", v.f(), width)
                            })
                        })
                    }
                }

            } else if let Some(prec) = prec {
                Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                    let prec = (*prec)(fs, args);

                    with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                        write!(fs, "{0:.1$}", v.f(), prec)
                    })
                })
            } else {
                Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                    with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                        write!(fs, "{}", v.f())
                    })
                })
            }
        }
        CastType::Written => {
            let align_fun = compile_align_fun(fill, width, align);

            Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                let mut len = 0;
                with_format_arg_write(&arg, args, fs, cast_type, |fs, v| {
                    let s = v.s();
                    len = s.len();
                    fs.write_str(&s)?;

                    (*align_fun)(fs, args, len)?;

                    Ok(())
                })
            })
        }
        CastType::Str => {
            let align_fun = compile_align_fun(fill, width, align);

            Box::new(move |fs: &mut FormatState, args: &[VVal]| {
                let mut len = 0;
                with_format_arg_write(&arg, args, fs, cast_type,|fs, v| {
                    if v.is_bytes() {
                        v.with_bv_ref(|bv| {
                            len = bv.len();
                            for b in bv {
                                fs.add_byte(*b)
                            }
                        });
                    } else {
                        v.with_s_ref(|s| {
                            fs.write_str(s)?;
                            len = s.len();
                            Ok(()) as std::fmt::Result
                        })?;
                    }

                    (*align_fun)(fs, args, len)?;

                    Ok(())
                })
            })
        }
    }
}

pub(crate) fn compile_formatter(fmt: &VVal) -> (FormatNode, usize) {
    let mut fmts : Vec<FormatNode> = vec![];

    let mut max_argc = 0;
    for (item, _) in fmt.iter() {
        let arg = item.at(1).unwrap_or_else(|| VVal::None);
        item.at(0).unwrap().with_s_ref(|syn| {
            match &syn[..] {
                "text" => {
                    fmts.push(Box::new(move |fs: &mut FormatState, _args: &[VVal]| {
                        arg.with_s_ref(|s| fs.write_str(s))
                    }));
                },
                _ => {
                    let arg =
                        arg.at(0).unwrap().with_s_ref(|arg_syn| {
                            match &arg_syn[..] {
                                "index" => {
                                    if max_argc < arg.v_i(1) + 1 {
                                        max_argc = arg.v_i(1) + 1;
                                    }

                                    FormatArg::Index(arg.v_i(1) as usize)
                                },
                                _ => {
                                    if max_argc < 1 {
                                        max_argc = 1;
                                    }
                                    FormatArg::Key(arg.v_(1))
                                }
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
            (*f)(fs, args)?
        }
        Ok(())
    }), max_argc as usize)
}

/// This function takes a VVal::Str or VVal::Byt and parses it as formatting
/// template. The return value is a VVal with a function that takes
/// as many arguments as specified in the formatting template.
///
///```
/// use wlambda::{VVal, EvalContext};
/// use wlambda::formatter::create_formatter_fun;
///
/// let mut ctx = EvalContext::new_default();
///
/// let fmt_src = VVal::new_str("a {} in {:8.3}");
/// match create_formatter_fun(&fmt_src) {
///     Ok(fun) => {
///         let ret =
///             ctx.call(
///                 &fun,
///                 &[VVal::new_str("b"),
///                   VVal::Flt(43.29432)])
///                .unwrap();
///         assert_eq!(ret.s_raw(), "a b in   43.294");
///     },
///     Err(e) => { panic!(format!("Bad formatter: {}", e)); }
/// }
///```
pub fn create_formatter_fun(fmt: &VVal) -> Result<VVal, ParseError> {
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
                let out : Vec<u8> = Vec::new();
                let mut fs = FormatState {
                    str_data:   None,
                    byte_data:  Some(out),
                };
                match (fun)(&mut fs, env.argv_ref()) {
                    Err(e) =>
                        Ok(env.new_err(format!("Formatter error: {}", e))),
                    _ => Ok(VVal::new_byt(fs.byte_data.take().unwrap()))
                }
            }, Some(argc), Some(argc), false)
    } else {
        VValFun::new_fun(
            move |env: &mut Env, _argc: usize| {
                let out = String::new();
                let mut fs = FormatState {
                    str_data:   Some(out),
                    byte_data:  None,
                };
                match (fun)(&mut fs, env.argv_ref()) {
                    Err(e) =>
                        Ok(env.new_err(format!("Formatter error: {}", e))),
                    _ => Ok(VVal::new_str_mv(fs.str_data.take().unwrap()))
                }
            }, Some(argc), Some(argc), false)
    })
}
