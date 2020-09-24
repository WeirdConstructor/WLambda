use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};
use crate::vval::VVal;

enum Endian {
    NA,
    LE,
    BE,
}

macro_rules! endian_bytes {
    ($u8vec: ident, $endian: ident, $v: expr) => {
        match $endian {
            Endian::LE => $u8vec.extend_from_slice(&($v).to_le_bytes()),
            Endian::BE => $u8vec.extend_from_slice(&($v).to_be_bytes()),
            Endian::NA => $u8vec.extend_from_slice(&($v).to_ne_bytes()),
        }
    }
}

fn parse_size(ps: &mut State) -> Result<usize, ParseError> {
    let uh = ps.take_while(|c| c.is_digit(10));

    if let Ok(cn) = usize::from_str_radix(&uh.to_string(), 10) {
        ps.skip_ws();
        Ok(cn)
    } else {
        Err(ps.err(ParseErrorKind::BadEscape("Bad number as size")))
    }
}

pub(crate) fn do_pack(pak_str: &str, data: &VVal) -> Result<VVal, ParseError> {
    let mut ps = State::new(pak_str, "<pack string>");
    ps.skip_ws();

    let mut byts : Vec<u8> = Vec::new();

    let mut endian = Endian::NA;

    let mut i = 0;

    while !ps.at_end() {
        let v = if data.is_vec() {
            data.v_(i)
        } else {
            if i == 0 { data.clone() } else { VVal::None }
        };

        match ps.expect_some(ps.peek())? {
            '<' => { ps.consume(); endian = Endian::LE; }
            '>' => { ps.consume(); endian = Endian::BE; }
            '=' => { ps.consume(); endian = Endian::NA; }
            'x' => { ps.consume(); byts.push(0x00); },
            'z' => {
                ps.consume();

                i += 1;
                v.with_bv_ref(|s| byts.extend_from_slice(&s[..]));
                byts.push(0x00);
            },
            'y' => {
                ps.consume();

                i += 1;
                v.with_bv_ref(|s| byts.extend_from_slice(&s[..]));
            },
            'c' => {
                ps.consume();

                let len = parse_size(&mut ps)?;
                i += 1;
                v.with_bv_ref(|s| {
                    if len > s.len() {
                        byts.extend_from_slice(&s[..]);
                        for _ in 0..(len - s.len()) {
                            byts.push(0x00);
                        }
                    } else {
                        byts.extend_from_slice(&s[..len]);
                    }
                })
            },
            'b' => {
                ps.consume();
                i += 1;
                v.with_bv_ref(|s| {
                    if s.len() == 0 {
                        byts.push(0x00)
                    } else {
                        byts.push(s[0])
                    }
                });
            },
            's' => {
                ps.consume();
                i += 1;
                v.with_bv_ref(|s| {
                    let size = s.len();

                    if ps.consume_lookahead("8") {
                        byts.extend_from_slice(&(size as u8).to_ne_bytes());

                    } else if ps.consume_lookahead("16") {
                        endian_bytes!(byts, endian, size as u16);

                    } else if ps.consume_lookahead("32") {
                        endian_bytes!(byts, endian, size as u32);

                    } else if ps.consume_lookahead("64") {
                        endian_bytes!(byts, endian, size as u64);

                    } else if ps.consume_lookahead("128") {
                        endian_bytes!(byts, endian, size as u128);

                    } else {
                        return Err(ps.err(
                            ParseErrorKind::BadPack(
                                "unknown size in s<n> pack string".to_string())));
                    }

                    byts.extend_from_slice(&s[..]);
                    Ok(())
                })?;
            },
            'u' => {
                ps.consume();
                i += 1;
                let i : i64 = v.i();

                if ps.consume_lookahead("8") {
                    byts.extend_from_slice(&(i as u8).to_ne_bytes());

                } else if ps.consume_lookahead("16") {
                    endian_bytes!(byts, endian, i as u16);

                } else if ps.consume_lookahead("32") {
                    endian_bytes!(byts, endian, i as u32);

                } else if ps.consume_lookahead("64") {
                    endian_bytes!(byts, endian, i as u64);

                } else if ps.consume_lookahead("128") {
                    endian_bytes!(byts, endian, i as u128);

                } else {
                    return Err(ps.err(
                        ParseErrorKind::BadPack(
                            "unknown size in u<n> pack string".to_string())));
                }
            },
            'i' => {
                ps.consume();
                i += 1;
                let i : i64 = v.i();

                if ps.consume_lookahead("8") {
                    byts.extend_from_slice(&(i as i8).to_ne_bytes());

                } else if ps.consume_lookahead("16") {
                    endian_bytes!(byts, endian, i as i16);

                } else if ps.consume_lookahead("32") {
                    endian_bytes!(byts, endian, i as i32);

                } else if ps.consume_lookahead("64") {
                    endian_bytes!(byts, endian, i as i64);

                } else if ps.consume_lookahead("128") {
                    endian_bytes!(byts, endian, i as i128);

                } else {
                    return Err(ps.err(
                        ParseErrorKind::BadPack(
                            "unknown size in i<n> pack string".to_string())));
                }
            },
            'f' => {
                ps.consume();
                i += 1;
                endian_bytes!(byts, endian, v.f() as f32);
            },
            'd' => {
                ps.consume();
                i += 1;
                endian_bytes!(byts, endian, v.f());
            },
            c => {
                return Err(ps.err(ParseErrorKind::UnexpectedToken(c, "in pack string")));
            },
        }
        ps.skip_ws();
    }

    Ok(VVal::new_byt(byts))
}

pub(crate) fn do_unpack(pak_str: &str, data: &VVal) -> Result<VVal, ParseError> {
    let mut ps = State::new(pak_str, "<pack string>");
    ps.skip_ws();

    data.with_bv_ref(|b| {
        let mut endian = Endian::NA;

        let mut i = 0;

        let out = VVal::vec();

        while !ps.at_end() && i < b.len() {
            match ps.expect_some(ps.peek())? {
                '<' => { ps.consume(); endian = Endian::LE; }
                '>' => { ps.consume(); endian = Endian::BE; }
                '=' => { ps.consume(); endian = Endian::NA; }
                'x' => { ps.consume(); i += 1; },
                'z' => {
                    ps.consume();

                    let mut found = false;
                    for o in i..b.len() {
                        if b[o] == 0x00 {
                            out.push(VVal::new_byt(b[i..o].to_vec()));
                            found = true;
                            i = o + 1;
                            break;
                        }
                    }

                    if !found {
                        out.push(VVal::new_byt(b[i..].to_vec()));
                        i = b.len();
                    }
                },
                'y' => {
                    ps.consume();

                    out.push(VVal::new_byt(b[i..].to_vec()));
                    i = b.len();
                },
                'c' => {
                    ps.consume();

                    let len = parse_size(&mut ps)?;
                    if (i + len) > b.len() {
                        return Err(ps.err(
                            ParseErrorKind::BadPack(
                                "'c<len>' data to small".to_string())));
                    }

                    out.push(VVal::new_byt(b[i..(i + len)].to_vec()));
                    i += len;
                },
                'b' => {
                    ps.consume();

                    if (i + 1) > b.len() {
                        return Err(ps.err(
                            ParseErrorKind::BadPack(
                                "'b' data to small".to_string())));
                    }

                    out.push(VVal::new_byt(b[i..(i + 1)].to_vec()));
                    i += 1;
                },
//                's' => {
//                    ps.consume();
//                    i += 1;
//                    v.with_bv_ref(|s| {
//                        let size = s.len();
//
//                        if ps.consume_lookahead("8") {
//                            byts.extend_from_slice(&(size as u8).to_ne_bytes());
//
//                        } else if ps.consume_lookahead("16") {
//                            endian_bytes!(byts, endian, size as u16);
//
//                        } else if ps.consume_lookahead("32") {
//                            endian_bytes!(byts, endian, size as u32);
//
//                        } else if ps.consume_lookahead("64") {
//                            endian_bytes!(byts, endian, size as u64);
//
//                        } else if ps.consume_lookahead("128") {
//                            endian_bytes!(byts, endian, size as u128);
//
//                        } else {
//                            return Err(ps.err(
//                                ParseErrorKind::BadPack(
//                                    "unknown size in s<n> pack string".to_string())));
//                        }
//
//                        byts.extend_from_slice(&s[..]);
//                        Ok(())
//                    })?;
//                },
//                'u' => {
//                    ps.consume();
//                    i += 1;
//                    let i : i64 = v.i();
//
//                    if ps.consume_lookahead("8") {
//                        byts.extend_from_slice(&(i as u8).to_ne_bytes());
//
//                    } else if ps.consume_lookahead("16") {
//                        endian_bytes!(byts, endian, i as u16);
//
//                    } else if ps.consume_lookahead("32") {
//                        endian_bytes!(byts, endian, i as u32);
//
//                    } else if ps.consume_lookahead("64") {
//                        endian_bytes!(byts, endian, i as u64);
//
//                    } else if ps.consume_lookahead("128") {
//                        endian_bytes!(byts, endian, i as u128);
//
//                    } else {
//                        return Err(ps.err(
//                            ParseErrorKind::BadPack(
//                                "unknown size in u<n> pack string".to_string())));
//                    }
//                },
//                'i' => {
//                    ps.consume();
//                    i += 1;
//                    let i : i64 = v.i();
//
//                    if ps.consume_lookahead("8") {
//                        byts.extend_from_slice(&(i as i8).to_ne_bytes());
//
//                    } else if ps.consume_lookahead("16") {
//                        endian_bytes!(byts, endian, i as i16);
//
//                    } else if ps.consume_lookahead("32") {
//                        endian_bytes!(byts, endian, i as i32);
//
//                    } else if ps.consume_lookahead("64") {
//                        endian_bytes!(byts, endian, i as i64);
//
//                    } else if ps.consume_lookahead("128") {
//                        endian_bytes!(byts, endian, i as i128);
//
//                    } else {
//                        return Err(ps.err(
//                            ParseErrorKind::BadPack(
//                                "unknown size in i<n> pack string".to_string())));
//                    }
//                },
//                'f' => {
//                    ps.consume();
//                    i += 1;
//                    endian_bytes!(byts, endian, v.f() as f32);
//                },
//                'd' => {
//                    ps.consume();
//                    i += 1;
//                    endian_bytes!(byts, endian, v.f());
//                },
                c => {
                    return Err(ps.err(ParseErrorKind::UnexpectedToken(c, "in pack string")));
                },
            }
            ps.skip_ws();
        }

        Ok(out)
    })
}
