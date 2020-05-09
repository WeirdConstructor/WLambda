/*!
Selector Syntax:


```ebnf

    class_char  = { ?any character except "]"? }
                  (* special sequence: "\^" => "^" and "\\" => "\"
                     and "\]" => "]" *)
                ;

    ident_char  = { ?any character except whitespace,
                    "!", "?", "/", "\", "|", "^", ",",
                    "'", "&", ":", ";", "$", "(", ")",
                    "{", "}", "[", "]", "*" or "="? }
                  (* allows the usual backslash escaping! *)
                ;

    ident       = ident_char, { ident_char }
                ;

    index       = digit, { digit }
                ;

    pat_regex   = "*", pat_glob_rx (* matches sub pattern 0 or N times *)
                | "+", pat_glob_rx (* matches sub pattern 1 or N times *)
                | "?", pat_glob_rx (* matches sub pattern 0 or 1 times *)
                | "!", pat_glob_rx (* matches (zero width) if next pattern does not match *)
                | "=", pat_glob_rx (* matches (zero width) if next pattern does match *)
                | "^"              (* matches (zero width) start of string *)
                | "$"              (* matches (zero width) end of string *)
                ;

    pat_glob_rx = pat_glob
                | ident_char
                ;

    pat_glob    = "*"           (* 0 or N any characters *)
                | "?"           (* 0 or 1 any character *)
                | "[", { class_char }, "]"
                | "[^", { class_char }, "]"
                | "(", "^", pattern, ")"   (* no capturing sub group *)
                | "(", pattern, ")"   (* capturing sub group *)
                | "$", pat_regex
                ;

    pat_s       = pat_glob, { pat_s }
                | ident
                ;

    pattern     = pat_s, { "|", pat_s }
                ;

    key         = index | pattern
                ;

    kv          = key, "=", pattern
                ;

    kv_item     = "{", kv, { ",", kv }, "}"
                ;

    kv_match    = kv_item, "&", kv_match
                | kv_item, "|", kv_match
                | kv_item
                ;

    node        = key, [":", ["!"], [ selector ], kv_match]
                | ":", ["!"], [ selector ], kv_match
                | "^", node (* marks it for referencing it in the result set *)
                | "**"      (* deep expensive recursion *)
                ;

    selector    = node, { "/", node }
                ;
```

*/

use crate::vval::VVal;
//use crate::vval::Syntax;

pub use crate::parser::state::State;
pub use crate::parser::state::{ParseValueError, ParseNumberError, ParseError, ParseErrorKind};
//use crate::parser::state::StrPart;

use crate::str_int::s2sym;


fn parse_ident(ps: &mut State) -> Result<VVal, ParseError> {
    let uh = ps.take_while(|c|
        match c {
           '!' | '?' | '/' | '\\' | '|' | '{' | '}'
         | '[' | ']' | '(' | ')' | '\'' | '^'
         | '&' | '$' | ':' | ';' | '*' | '=' | ','
                => false,
            _   => !c.is_whitespace()
    });

    let r =
        VVal::pair(
            VVal::new_sym("I"),
            VVal::new_sym(&uh.to_string()));

    ps.skip_ws();

    Ok(r)
}

fn parse_char_class(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq('[') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('[', "in char class")));
    }

    let neg = ps.consume_if_eq('^');

    let mut chars = String::new();

    let mut c = ps.expect_some(ps.peek())?;
    while c != ']' {
        ps.consume();
        if c == '\\' {
            let c = ps.expect_some(ps.peek())?;
            ps.consume();
            // TODO: Factor out the \-Parsing of parse_string() in parser.rs!
            chars.push(c);
        } else {
            chars.push(c);
        }
        c = ps.expect_some(ps.peek())?;
    }

    if !ps.consume_if_eq_ws(']') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken(']', "in char class")));
    }

    Ok(VVal::pair(
        VVal::new_sym(
            if neg { "NCCls" }
            else   { "CCls" }),
        VVal::new_str_mv(chars)))
}

fn parse_capture(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('(') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('(', "in sub pattern")));
    }

    let (no_capture, p) =
        if ps.consume_if_eq_ws('{') {
            let ctrl =
                match ps.expect_some(ps.peek())? {
                    '*' => { ps.consume_ws(); Some(VVal::new_sym("N0")) },
                    '+' => { ps.consume_ws(); Some(VVal::new_sym("N1")) },
                    '?' => { ps.consume_ws(); Some(VVal::new_sym("Opt")) },
                    '!' => { ps.consume_ws(); Some(VVal::new_sym("ZwNegLA")) },
                    '=' => { ps.consume_ws(); Some(VVal::new_sym("ZwLA")) },
                    _   => None,
                };

            if !ps.consume_if_eq_ws('}') {
                return Err(ps.err(
                    ParseErrorKind::UnexpectedToken('}', "in sub pattern")));
            }

            if let Some(ctrl) = ctrl {
                (true, VVal::pair(ctrl, parse_pattern(ps)?))
            } else {
                (true, parse_pattern(ps)?)
            }
        } else {
            (false, parse_pattern(ps)?)
        };

    if !ps.consume_if_eq_ws(')') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken(')', "in sub pattern")));
    }

    if no_capture {
        Ok(VVal::pair(VVal::new_sym("PatSub"), p))
    } else {
        Ok(VVal::pair(VVal::new_sym("PatCap"), p))
    }
}

fn parse_pattern_s(ps: &mut State) -> Result<VVal, ParseError> {
    let pat = VVal::vec();

    while !ps.at_end() && !ps.lookahead_one_of("'&:^;$)]}=/|,") {
        let element =
            match ps.expect_some(ps.peek())? {
                '*' => { ps.consume_ws(); VVal::new_sym("Glob") },
                '?' => { ps.consume_ws(); VVal::new_sym("Any") },
                '^' => { ps.consume_ws(); VVal::new_sym("Start") },
                '$' => { ps.consume_ws(); VVal::new_sym("End") },
                '[' => parse_char_class(ps)?,
                '(' => parse_capture(ps)?,
                '\\' => {
                    ps.consume();
                    let next = ps.expect_some(ps.peek())?;
                    ps.consume_ws();
                    let mut b = [0; 4];
                    VVal::pair(
                        VVal::new_sym("I"),
                        VVal::new_sym(next.encode_utf8(&mut b)))
                },
                _   => parse_ident(ps)?
            };

        pat.push(element);
    }

    Ok(pat)
}

fn parse_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let mut pat = parse_pattern_s(ps)?;

    let mut append = false;
    while ps.consume_if_eq_ws('|') {
        let next_opt_pat = parse_pattern_s(ps)?;

        if append {
            pat.push(next_opt_pat);
        } else {
            pat = VVal::vec3(VVal::new_sym("Opt"), pat, next_opt_pat);
            append = true;
        }
    }

    Ok(pat)
}

fn parse_index(ps: &mut State) -> Result<VVal, ParseError> {
    let uh = ps.take_while(|c| c.is_digit(10));

    if let Ok(cn) = i64::from_str_radix(&uh.to_string(), 10) {
        ps.skip_ws();
        Ok(VVal::Int(cn as i64))
    } else {
        Err(ps.err(ParseErrorKind::BadEscape("Bad number as index")))
    }
}

fn parse_key(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
          '0' | '1' | '2' | '3' | '4'
        | '5' | '6' | '7' | '8' | '9'
            => parse_index(ps),
        _   => parse_pattern(ps),
    }
}

fn parse_kv(ps: &mut State) -> Result<VVal, ParseError> {
    let key = parse_key(ps)?;

    if !ps.consume_if_eq_ws('=') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('=', "in key/value")));
    }

    let val = parse_pattern(ps)?;

    Ok(VVal::vec2(key, val))
}

fn parse_kv_item(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('{') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('{', "in key/value node pattern start")));
    }

    let kv = parse_kv(ps)?;

    let v = VVal::vec2(VVal::new_sym("KV"), kv);

    while ps.expect_some(ps.peek())? == ',' {
        ps.consume_ws();
        let kv = parse_kv(ps)?;
        v.push(kv);
    }

    if !ps.consume_if_eq_ws('}') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('}', "in key/value node pattern end")));
    }

    Ok(v)
}

fn parse_kv_match(ps: &mut State) -> Result<VVal, ParseError> {
    let item = parse_kv_item(ps)?;

    match ps.peek().unwrap_or('\0') {
        '&' => {
            ps.consume_ws();

            let v = VVal::vec();
            v.push(VVal::new_sym("And"));
            v.push(item);
            v.push(parse_kv_item(ps)?);
            Ok(v)
        },
        '|' => {
            ps.consume_ws();

            let v = VVal::vec();
            v.push(VVal::new_sym("Or"));
            v.push(item);
            v.push(parse_kv_item(ps)?);
            Ok(v)
        },
        _ => Ok(item),
    }
}

fn parse_node(ps: &mut State) -> Result<VVal, ParseError> {
    let c = ps.expect_some(ps.peek())?;

    if c == '*' && ps.lookahead("**") {
        ps.consume_lookahead("**");
        ps.skip_ws();
        Ok(VVal::vec1(VVal::new_sym("RecGlob")))

    } else {
        match c {
            ':' => {
                ps.consume_ws();
                // TODO: FIXME: Parse "!" here for negated kv matches!
                // TODO: FIXME: Parse selector here, that will
                //              act as recursive self-select on the current value
                Ok(VVal::vec2(
                    VVal::new_sym("NKVM"),
                    parse_kv_match(ps)?))
            },
            '^' => {
                ps.consume_ws();
                Ok(VVal::vec2(
                    VVal::new_sym("NCap"),
                    parse_node(ps)?))
            },
            _ => {
                let key = parse_key(ps)?;

                match ps.peek().unwrap_or('\0') {
                    ':' => {
                        ps.consume_ws();
                        let kvm = parse_kv_match(ps)?;
                        Ok(VVal::vec3(
                            VVal::new_sym("NKLA_KVM"),
                            key, kvm))
                    },
                    _ =>
                        Ok(VVal::vec2(
                            VVal::new_sym("NK"),
                            key))
                }
            }
        }
    }
}

fn parse_selector_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let selector = VVal::vec1(VVal::new_sym("Path"));

    let node = parse_node(ps)?;
    selector.push(node);

    while ps.consume_if_eq_ws('/') {
        let node = parse_node(ps)?;
        selector.push(node);
    }

    ps.skip_ws();

    Ok(selector)
}

fn parse_selector(s: &str) -> Result<VVal, String> {
    let mut ps = State::new(s, "<selector>");
    ps.skip_ws();
    parse_selector_pattern(&mut ps).map_err(|e| format!("{}", e))
}

pub struct SelectorState {
}

impl SelectorState {
    fn new() -> Self {
        Self {
        }
    }
}

pub type PatternNode = Box<dyn Fn(&str, &mut SelectorState) -> (VVal, usize)>;
pub type SelNode     = Box<dyn Fn(&VVal, &mut SelectorState, &VVal)>;

fn compile_sub_pattern(pat: &VVal, capture: bool, next: Option<PatternNode>) -> PatternNode {
    println!("COMP SUB PAT {}", pat.s());
    if pat.is_pair() {
        let sub_type = pat.at(0).expect("proper pattern").to_sym();
        let sub      = pat.at(1).expect("sub pattern");
        let sub_pat  = compile_pattern(&sub);

        if sub_type == s2sym("ZwNegLA") {
            Box::new(move |s: &str, st: &mut SelectorState| {
                let (m, _) = (*sub_pat)(s, st);
                if m.b() {
                    return (VVal::None, 0);
                }

                if let Some(n) = &next {
                    (*n)(&s, st)
                } else {
                    (VVal::Bol(true), s.len())
                }
            })

        } else if sub_type == s2sym("ZwLA") {
            Box::new(move |s: &str, st: &mut SelectorState| {
                let (m, len) = (*sub_pat)(s, st);
                if !m.b() {
                    return (VVal::None, 0);
                }

                if let Some(n) = &next {
                    (*n)(&s, st)
                } else {
                    (VVal::Bol(true), s.len())
                }
            })

        } else if sub_type == s2sym("N1") {
            Box::new(move |s: &str, st: &mut SelectorState| {
                let (m, len) = (*sub_pat)(s, st);
                if !m.b() {
                    return (VVal::None, 0);
                }

                let mut matched     = true;
                let mut match_len   = len;

                while matched {
                    let (m, len) = (*sub_pat)(&s[match_len..], st);
                    matched = m.b();
                    if matched {
                        if len == 0 { break; }
                        match_len += len;
                    }
                }

                if let Some(n) = &next {
                    let (m, len) = (*n)(&s[match_len..], st);
                    if m.b() {
                        (m, match_len + len)
                    } else {
                        (VVal::None, 0)
                    }
                } else {
                    (VVal::Bol(true), match_len)
                }
            })

        } else {
            Box::new(move |s: &str, st: &mut SelectorState| {
                panic!("NOT IMPLEMENTED: {}", sub_type.to_string())
            })
        }

    } else {
        println!("COMPILE SUB PATTERN: [{}]", pat.s());
        let p = compile_pattern(&pat);
        Box::new(move |s: &str, st: &mut SelectorState| {
            let (r, len1) = (*p)(s, st);
            if !r.b() {
                return (VVal::None, 0);
            }

            if let Some(n) = &next {
                let (r, len2) = (*n)(&s[len1..], st);
                (r, len1 + len2)
            } else {
                (r, len1)
            }
        })
    }
}

fn compile_pattern(pat: &VVal) -> PatternNode {
    println!("COMPILE PATTERN [{}]", pat.s());

    let mut next : Option<PatternNode> = None;
    for i in 0..pat.len() {
        let p = pat.at(pat.len() - (i + 1)).expect("pattern item");

        println!("PAT COMP: {}", p.s());

        if p.is_pair() {
            let pat_pair_type = p.at(0).unwrap().to_sym();

            if pat_pair_type == s2sym("I") {
                let key_str = p.at(1).unwrap().clone();

                let mn = std::mem::replace(&mut next, None);
                next = Some(Box::new(move |s: &str, st: &mut SelectorState| {
                    key_str.with_s_ref(|y| {
                        println!("I: [{}]", s);
                        let y_len = y.len();

                        if s.starts_with(y) {
                            if let Some(n) = &mn {
                                let (m, len) = (*n)(&s[y_len..], st);
                                (m, y_len + len)
                            } else {
                                (VVal::Bol(true), y_len)
                            }
                        } else {
                            (VVal::None, 0)
                        }
                    })
                }));
            } else if pat_pair_type == s2sym("CCls") {
                let chars = p.at(1).unwrap().clone();

                let mn = std::mem::replace(&mut next, None);
                next = Some(Box::new(move |s: &str, st: &mut SelectorState| {
                    chars.with_s_ref(|chrs| {
                        if let Some(c) = s.chars().nth(0) {
                            let c_len = c.len_utf8();

                            for mc in chrs.chars() {
                                if c == mc {
                                    if let Some(n) = &mn {
                                        let (m, len) = (*n)(&s[c_len..], st);
                                        return (m, c_len + len);
                                    } else {
                                        return (VVal::Bol(true), c_len);
                                    }
                                }
                            }
                        }

                        (VVal::None, 0)
                    })
                }));

            } else if pat_pair_type == s2sym("NCCls") {
                let chars = p.at(1).unwrap().clone();

                let mn = std::mem::replace(&mut next, None);
                next = Some(Box::new(move |s: &str, st: &mut SelectorState| {
                    chars.with_s_ref(|chrs| {
                        if let Some(c) = s.chars().nth(0) {
                            for mc in chrs.chars() {
                                if c == mc {
                                    return (VVal::None, 0);
                                }
                            }

                            let c_len = c.len_utf8();

                            if let Some(n) = &mn {
                                let (m, len) = (*n)(&s[c_len..], st);
                                return (m, c_len + len);
                            } else {
                                return (VVal::Bol(true), c_len);
                            }
                        }
                        (VVal::None, 0)
                    })
                }));

            } else if pat_pair_type == s2sym("PatSub") {
                let mn = std::mem::replace(&mut next, None);
                next = Some(
                    compile_sub_pattern(
                        &p.at(1).expect("sub pattern"), false, mn))

            } else if pat_pair_type == s2sym("PatCap") {
                let mn = std::mem::replace(&mut next, None);
                // TODO: Make some capture closure, that pushes the captures
                //       on some capture stack?
                next = Some(
                    compile_sub_pattern(
                        &p.at(1).expect("sub pattern"), true, mn));

            } else {
                next = Some(Box::new(|_s: &str, _st: &mut SelectorState| {
                    (VVal::None, 0)
                }));
            }

        } else if p.to_sym() == s2sym("Any") {
            let mn = std::mem::replace(&mut next, None);
            next = Some(Box::new(move |s: &str, st: &mut SelectorState| {

                if let Some(c) = s.chars().nth(0) {
                    let c_len = c.len_utf8();

                    if let Some(n) = &mn {
                        let (m, len) = (*n)(&s[c_len..], st);
                        (m, c.len_utf8() + len)
                    } else {
                        (VVal::Bol(true), c_len)
                    }
                } else {
                    (VVal::None, 0)
                }
            }));

        } else if p.to_sym() == s2sym("Glob") {
            let mn = std::mem::replace(&mut next, None);
            next = Some(Box::new(move |s: &str, st: &mut SelectorState| {

                if let Some(n) = &mn {
                    let s_len = s.len();
                    let mut c_try_len = s_len;
                    while c_try_len > 0 {
                        c_try_len -= 1;
                        while c_try_len > 0 && !s.is_char_boundary(c_try_len) {
                            c_try_len -= 1;
                        }

                        let (m, len) = (*n)(&s[c_try_len..], st);
                        if m.b() {
                            return (m, c_try_len + len);
                        }
                    }

                    (VVal::Bol(true), 0)
                } else {
                    (VVal::Bol(true), s.len())
                }
            }));

        } else {
            next = Some(Box::new(move |_s: &str, _st: &mut SelectorState| {
                panic!("unimplemented pattern type: {}", p.s());
                (VVal::None, 0)
            }));
        }
    }

    if let Some(n) = next {
        n
    } else {
        Box::new(|_s: &str, _st: &mut SelectorState| { (VVal::None, 0) })
    }
}

fn compile_key(k: &VVal, sn: SelNode) -> SelNode {
    if k.is_int() {
        let i = k.i();

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            if let Some(v) = v.at(i as usize) {
                (*sn)(&v, st, capts);
            }
        })
    } else {
        let pat = k.at(0).unwrap_or_else(|| VVal::None);
        let pat_type = k.at(0).unwrap_or_else(|| VVal::None).at(0).unwrap_or_else(|| VVal::None);

        if k.len() == 1 && pat.is_pair() && pat_type.to_sym() == s2sym("I") {
            let key = pat.at(1).unwrap_or_else(|| VVal::None).to_sym();

            return
                Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                    if let Some(v) = v.get_key_sym(&key) {
                        (*sn)(&v, st, capts);
                    }
                });

        } else if k.len() == 1 && pat.to_sym() == s2sym("Glob") {
            return
                Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                    for (v, _) in v.iter() {
                        (*sn)(&v, st, capts);
                    }
                });
        }

        let pat = compile_pattern(k);

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            for (i, (v, k)) in v.iter().enumerate() {
                if let Some(k) = k {
                    k.with_s_ref(|s| {
                        let (r, len) = (*pat)(&s[..], st);
                        if r.b() && len == s.len() {
                            (*sn)(&v, st, capts);
                        }
                    });
                } else {
                    let idx_str = format!("{}", i);

                    let (r, len) = (*pat)(&idx_str[..], st);
                    if r.b() && len == idx_str.len() {
                        (*sn)(&v, st, capts);
                    }
                }
            }
        })
    }
}

fn compile_node(n: &VVal, sn: SelNode) -> SelNode {
    let node_type = n.at(0).expect("proper node").to_sym();

    if node_type == s2sym("NK") {
        compile_key(&n.at(1).unwrap_or_else(|| VVal::None), sn)
    } else {
        Box::new(move |_v: &VVal, _st: &mut SelectorState, _capts: &VVal| {
            panic!("Unimplemented node type: {}", node_type);
        })
    }
}

fn compile_selector(sel: &VVal) -> SelNode {
    println!("***** COM SELECTOR: {}", sel.s());
    if let VVal::Lst(_) = sel {

        let first = sel.at(0).unwrap_or_else(|| VVal::None);
        if first.to_sym() == s2sym("Path") {
            let mut next : Option<SelNode> = Some(Box::new(
                |v: &VVal, _st: &mut SelectorState, capts: &VVal| {
                    capts.push(v.clone());
                }));

            for i in 1..sel.len() {
                let nod = sel.at(sel.len() - i).expect("proper path");
                let n = std::mem::replace(&mut next, None).unwrap();
                let cur = compile_node(&nod, n);
                next = Some(cur);
            }

            next.unwrap()
        } else {
            Box::new(move |_v: &VVal, _st: &mut SelectorState, _capts: &VVal| {
                panic!("unimplemented selector type: {}", first.s());
            })
        }
    } else {
        let sel = sel.clone();
        Box::new(move |_v: &VVal, _st: &mut SelectorState, _capts: &VVal| {
            panic!("unimplemented selector type?: {}", sel.s());
        })
    }
}

fn compile_single_pattern(v: &VVal) -> PatternNode {
    panic!("implement a pattern that matches either at the \
            start only or anywhere in the string if the first \
            element is or is not a \"Start\" (^)");
}

//fn tree_select(slct: &VVal, tree: &VVal) -> VVal {
//    let path = VVal::vec();
//    slct.with_s_ref(|s| parse_select(&s.chars().collect(), path));
//}

#[cfg(test)]
mod tests {
    use super::*;

    fn pat(pat: &str, st: &str) -> String {
        let mut ps = State::new(pat, "<pattern>");
        ps.skip_ws();
        match parse_pattern(&mut ps) {
            Ok(v) => {
                let pn = compile_single_pattern(&v);
                let mut ss = SelectorState::new();
                let (r, len) = (*pn)(st, &mut ss);
                if len == st.len() {
                    r.s()
                } else {
                    "-nomatch-".to_string()
                }
            },
            Err(e) => format!("Error: {}", e),
        }
    }

    fn p(s: &str) -> String {
        match parse_selector(s) {
            Ok(v)  => v.s(),
            Err(e) => format!("Error: {}", e),
        }
    }

    fn pev(s: &str, v: &VVal) -> String {
        let sel_ast =
            match parse_selector(s) {
                Ok(v)  => v,
                Err(e) => { return format!("Error: {}", e); },
            };
        let sn = compile_selector(&sel_ast);
        let mut state = SelectorState::new();
        let capts = VVal::vec();
        (*sn)(v, &mut state, &capts);
        capts.s()
    }

    #[test]
    fn check_selector_match_path() {
        let v1 =
            VVal::map3("a",
                VVal::vec3(
                    VVal::Int(20),
                    VVal::pair(VVal::Int(2), VVal::Int(4)),
                    VVal::new_str("F0O")),
                "ab",
                VVal::vec2(
                    VVal::Int(33),
                    VVal::Int(44)),
                "xyab",
                VVal::vec3(
                    VVal::Int(8),
                    VVal::Int(9),
                    VVal::map2("X", VVal::Int(10), "Y", VVal::Int(20))));

        assert_eq!(pev("a",         &v1), "$[$[20,$p(2,4),\"F0O\"]]");
        assert_eq!(pev("a/2/2",     &v1), "$[\"O\"]");
        assert_eq!(pev("a/2/1",     &v1), "$[\"0\"]");
        assert_eq!(pev("ab/0",      &v1), "$[33]");

        assert_eq!(pev("a/?",       &v1), "$[20,$p(2,4),\"F0O\"]");
        assert_eq!(pev("a/?/1",     &v1), "$[4,\"0\"]");

        assert_eq!(pev("?/1",       &v1), "$[$p(2,4)]");
        assert_eq!(pev("?/2",       &v1), "$[\"F0O\"]");

        assert_eq!(pev("?b/1",      &v1), "$[44]");
        assert_eq!(pev("a?/1",      &v1), "$[44]");
        assert_eq!(pev("??ab/1",    &v1), "$[9]");

        assert_eq!(pev("*/X",       &v1), "$[]");
        assert_eq!(pev("*/?/X",     &v1), "$[10]");
        assert_eq!(pev("*/*/X",     &v1), "$[10]");
        assert_eq!(pev("*/2/2",     &v1), "$[\"O\"]");

        assert_eq!(pev("*ab/*/X",   &v1), "$[10]");

        assert_eq!(pev("[xy][xy]*/[01]",    &v1), "$[8,9]");
        assert_eq!(pev("[^xy][^xy]/[01]",   &v1), "$[33,44]");
        assert_eq!(pev("a/[^01]",           &v1), "$[\"F0O\"]");

        assert_eq!(pev("(ab)/[01]",         &v1), "$[33,44]");
        assert_eq!(pev("(x)y(a)b/[01]",     &v1), "$[8,9]");
        assert_eq!(pev("$!(a)*/[01]",       &v1), "$[8,9]");
        assert_eq!(pev("a/$![01]",          &v1), "$[\"F0O\"]");

        assert_eq!(pev("$=}x*/[01]",        &v1), "$[8,9]");
        assert_eq!(pev("$=}(ab)*/[01]",     &v1), "$[33,44]");
        assert_eq!(pev("a$=b*/[01]",        &v1), "$[33,44]");
        assert_eq!(pev("$!x*$=b/[01]",      &v1), "$[33,44]");

        assert_eq!(pev("$+[xy]ab/0",     &v1), "$[8]");
        assert_eq!(pev("a$+b/0",         &v1), "$[33]");
        assert_eq!(pev("$*[xy]ab/0",     &v1), "");
        assert_eq!(pev("$?[xy][xy]ab/0", &v1), "");
    }

    #[test]
    fn check_selector_match_esc() {
        let v1 =
            VVal::map3("\\",
                VVal::vec3(
                    VVal::Int(20),
                    VVal::pair(VVal::Int(2), VVal::Int(4)),
                    VVal::new_str("F0O%/{}[]")),
                "//",
                VVal::vec2(
                    VVal::Int(33),
                    VVal::Int(44)),
                "?*",
                VVal::vec3(
                    VVal::Int(8),
                    VVal::Int(9),
                    VVal::map2("*", VVal::Int(10), "|", VVal::Int(20))));

        assert_eq!(pev("*/*/\\*", &v1),     "$[10]");
        assert_eq!(pev("*/*/\\|", &v1),     "$[20]");

        assert_eq!(pev("[\\\\]*/1", &v1),   "$[$p(2,4)]");
        assert_eq!(pev("[\\/]*/0", &v1),    "$[33]");
        assert_eq!(pev("\\/\\//0", &v1),    "$[33]");
    }

    #[test]
    fn check_selector_node_path() {
        assert_eq!(p("a"),     "$[:Path,$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(p("a/0/2"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NK,0],$[:NK,2]]");
        assert_eq!(p("a/b/c"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NK,$[$p(:I,:b)]],$[:NK,$[$p(:I,:c)]]]");

        assert_eq!(p("a/^b/c/^"), "Error: error[1,9:<selector>] EOF while parsing: Unexpected EOF at code \'\'");
        assert_eq!(p("a/^b/c/^*"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NCap,$[:NK,$[$p(:I,:b)]]],$[:NK,$[$p(:I,:c)]],$[:NCap,$[:NK,$[:Glob]]]]");
        assert_eq!(p("a/^b/^c"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NCap,$[:NK,$[$p(:I,:b)]]],$[:NCap,$[:NK,$[$p(:I,:c)]]]]");
    }

    #[test]
    fn check_selector_globs() {
        assert_eq!(p("*"),      "$[:Path,$[:NK,$[:Glob]]]");
        assert_eq!(p("**"),     "$[:Path,$[:RecGlob]]");
        assert_eq!(p("^**"),    "$[:Path,$[:NCap,$[:RecGlob]]]");
        assert_eq!(p("^*"),     "$[:Path,$[:NCap,$[:NK,$[:Glob]]]]");

        assert_eq!(p("(*|a?)"), "$[:Path,$[:NK,$[$p(:PatCap,$[:Opt,$[:Glob],$[$p(:I,:a),:Any]])]]]");

        assert_eq!(p("*/*/a"),   "$[:Path,$[:NK,$[:Glob]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(p("*  /  *  /   a   "),   "$[:Path,$[:NK,$[:Glob]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(p("**/^a/**"), "$[:Path,$[:RecGlob],$[:NCap,$[:NK,$[$p(:I,:a)]]],$[:RecGlob]]");

        assert_eq!(p("?a"),    "$[:Path,$[:NK,$[:Any,$p(:I,:a)]]]");
    }

    #[test]
    fn check_selector_kvmatch() {
        assert_eq!(p(":{b=a,a=20}"),                                "$[:Path,$[:NKVM,$[:KV,$[$[$p(:I,:b)],$[$p(:I,:a)]],$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]");
        assert_eq!(p("a : { a = 20 }"),                             "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]");
        assert_eq!(p("a : { a = 20, b=a(a?)[^ABC]cc*f}"),           "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]],$[$[$p(:I,:b)],$[$p(:I,:a),$p(:PatCap,$[$p(:I,:a),:Any]),$p(:NCCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20, b=a(a?)[ABC]cc*f}"),            "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]],$[$[$p(:I,:b)],$[$p(:I,:a),$p(:PatCap,$[$p(:I,:a),:Any]),$p(:CCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20 } | { b = 20 }"),                "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:Or,$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]],$[:KV,$[$[$p(:I,:b)],$[$p(:I,:20)]]]]]]");
        assert_eq!(p("a : { a = 20 } | { b = 20 } & { x = 10}"),    "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:Or,$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]],$[:KV,$[$[$p(:I,:b)],$[$p(:I,:20)]]]]]]");
    }

    #[test]
    fn check_selector_subpat() {
        assert_eq!(p("(^abc$$)"),               "");
        assert_eq!(p("(\\^abc$$)"),             "");

        assert_eq!(p("(abc)"),                  "");
        assert_eq!(p("$!(abc)"),                "");
        assert_eq!(p("$*(abc)"),                "");
        assert_eq!(p("$+(abc)"),                "");
        assert_eq!(p("$?(abc)"),                "");
        assert_eq!(p("$=(abc)"),                "");
        assert_eq!(p("$^abc$$"),                "");
        assert_eq!(p("(\\$abc)"),               "");
    }

    #[test]
    fn check_patterns() {
        assert_eq!(pat("^A(B)C$",           "ABC"),         "B");
        assert_eq!(pat("(BC)",              "ABC"),         "BC");
        assert_eq!(pat("^[ ]$",             " "),           "BC");
        assert_eq!(pat("^$*[ ]$",        "   "),         "BC");
        assert_eq!(pat("[\\t\\0\\u0101]",   "\0"),          "");
        assert_eq!(pat("[\\t\\0\\u0101]",   "\t"),          "");
        assert_eq!(pat("[\\t\\0\\u0101]",   "ā"),           "");
    }
}
