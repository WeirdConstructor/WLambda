/*!
Selector Syntax:


```ebnf

    class_char  = { ?any character except "]"? }
                  (* special sequence: "\\" => "\" and "\]" => "]" *)
                ;

    ident       = { ?any character except whitespace,
                    "?", "/", "\", "|", "^", ",",
                    "'", "&", ":", ";", "$", "(", ")",
                    "{", "}", "[", "]", "*" or "="? }
                  (* allows the usual backslash escaping! *)
                ;

    index       = digit, { digit }
                ;

    pattern     = "*", [ pattern ]
                | "?", [ pattern ]
                | "[", { class_char }, "]"
                | "[^", { class_char }, "]"
                | "(", pattern, ")"         (* maybe for later: captures *)
                | ident, [ pattern ]
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

    node        = key, [":", kv_match]
                | ":", kv_match
                | "^", node (* marks it for referencing it in the result set *)
                | "**"
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



fn parse_ident(ps: &mut State) -> Result<VVal, ParseError> {
    let uh = ps.take_while(|c|
        match c {
           '?' | '/' | '\\' | '|' | '{' | '}'
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
    if !ps.consume_if_eq_ws('[') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('[', "in char class")));
    }

    let neg = ps.consume_if_eq_ws('^');

    let mut chars = String::new();

    let mut c = ps.expect_some(ps.peek())?;
    while c != ']' {
        ps.consume();
        chars.push(c);
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
            ParseErrorKind::UnexpectedToken('(', "in capture")));
    }

    let p = parse_pattern(ps)?;

    if !ps.consume_if_eq_ws(')') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken(')', "in capture")));
    }

    Ok(VVal::pair(VVal::new_sym("PatCap"), p))
}

fn parse_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let pat = VVal::vec();

    while !ps.at_end() && !ps.lookahead_one_of("'&:^;$)]}=/|,") {
        let element =
            match ps.expect_some(ps.peek())? {
                '*' => { ps.consume(); VVal::new_sym("Glob") },
                '?' => { ps.consume(); VVal::new_sym("Any") },
                '[' => parse_char_class(ps)?,
                '(' => parse_capture(ps)?,
                '\\' => {
                    ps.consume();
                    let next = ps.expect_some(ps.peek())?;
                    ps.consume();
                    let mut b = [0; 4];
                    VVal::pair(
                        VVal::new_sym("S"),
                        VVal::new_sym(next.encode_utf8(&mut b)))
                },
                _   => parse_ident(ps)?
            };

        pat.push(element);
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

    let c = ps.peek().unwrap_or('\0');

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

    } else if c == '*' {
        ps.consume_ws();
        Ok(VVal::vec1(VVal::new_sym("Glob")))

    } else {
        match c {
            ':' => {
                ps.consume_ws();
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

pub type PatternNode = Box<dyn Fn(&[char], &mut SelectorState) -> (VVal, usize)>;
pub type SelNode     = Box<dyn Fn(&VVal, &mut SelectorState, &VVal)>;

fn compile_pattern(pat: &VVal, sn: SelNode) -> PatternNode {
    let pattern = n.at(0).expect("proper pattern").to_sym();

    let mut next : Option<PatternNode> = None;

    if pattern == VVal::new_sym("Pat") {

        for i in 0..(n.len() - 1) {
            let p = n.at(n.len() - i).expect("pattern item");

            if p.is_pair() && p.at(0).unwrap() == VVal::new_sym("I") {
                let key_str = p.at(1).unwrap().clone();

                let mn = std::mem::replace(&mut next, None);
                next = Box::new(move |s: &[char], st: &mut SelectorState| {
                    key_str.with_s_ref(|y| {
                        let y_len = y.len();

                        if s.len() >= y_len && s[0..y_len] == y {
                            if let Some(n) = mn {
                                if y_len == s.len() {
                                    (VVal::Bol(true), y_len)
                                } else {
                                    let (m, len) = (*n)(s[y_len..], st);
                                    (m, y_len + len)
                                }
                            } else {
                                (VVal::Bol(true), y_len)
                            }
                        } else {
                            (VVal::None, 0)
                        }
                    })
                });

            } else {
                next = Box::new(|s: &[char], st: &mut SelectorState| {
                    (VVal::None, 0)
                });
            }
        }
//
//        let mut full_str =
//            Box::new(move |v: &VVal, st: &mut SelectorState| {
//                let (r, _) = v.with_s_ref(|s| { next(&s[..], st) });
//                r
//            });
    }

    if let Some(n) = next {
        next
    } else {
        Box::new(|s: &[char], st: &mut SelectorState| { (VVal::None, 0) })
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
        let pat = compile_pattern(k, sn);

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            let mut captures = VVal::None;

            for (v, k) in v.iter() {
                if let Some(k) = k {
                    k.with_s_ref(|s| {
                        let (r, _) = (*pat)(&s[..], st);
                        if r.b() {
                            (*sn)(&v, st, capts);
                        }
                    });
                } else {
                    panic!("not implemented yet");
                }
            }
        })
    }
}

fn compile_node(n: &VVal, sn: SelNode) -> SelNode {
    let node_type = n.at(0).expect("proper node").to_sym();

    if node_type == VVal::new_sym("NK") {
        compile_key(&n.at(1).unwrap_or_else(|| VVal::None), sn)
    } else {
        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| { })
    }
}

fn compile_selector(sel: &VVal) -> SelNode {
    if let VVal::Lst(_) = sel {
        println!("SELE {}", sel.s());

        let first = sel.at(0).unwrap_or_else(|| VVal::None);
        if first == VVal::new_sym("Path") {
            for i in 1..sel.len() {
                let nod = sel.at(i).expect("proper path");
            }
        }

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| { })
    } else {
        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| { })
    }
}

//fn tree_select(slct: &VVal, tree: &VVal) -> VVal {
//    let path = VVal::vec();
//    slct.with_s_ref(|s| parse_select(&s.chars().collect(), path));
//}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> String {
        match parse_selector(s) {
            Ok(v)  => v.s(),
            Err(e) => format!("Error: {}", e),
        }
    }

    fn pev(s: &str, v: VVal) -> String {
        let sel_ast =
            match parse_selector(s) {
                Ok(v)  => v,
                Err(e) => { return format!("Error: {}", e); },
            };
        let sn = compile_selector(&sel_ast);
        let mut state = SelectorState::new();
        let ret = (*sn)(&v, &mut state);
        ret.s()
    }

    #[test]
    fn check_selector_match_path() {
        assert_eq!(
            pev("a", VVal::map1("a", VVal::vec1(VVal::Int(20)))),
            "");
    }

    #[test]
    fn check_selector_node_path() {
        assert_eq!(p("a"),     "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]]]");
        assert_eq!(p("a/0/2"), "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]],$[:NK,0],$[:NK,2]]");
        assert_eq!(p("a/b/c"), "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]],$[:NK,$[:Pat,$p(:I,:b)]],$[:NK,$[:Pat,$p(:I,:c)]]]");

        assert_eq!(p("a/^b/c/^"), "Error: error[1,9:<selector>] EOF while parsing: Unexpected EOF at code \'\'");
        assert_eq!(p("a/^b/c/^*"), "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]],$[:NCap,$[:NK,$[:Pat,$p(:I,:b)]]],$[:NK,$[:Pat,$p(:I,:c)]],$[:NCap,$[:Glob]]]");
        assert_eq!(p("a/^b/^c"), "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]],$[:NCap,$[:NK,$[:Pat,$p(:I,:b)]]],$[:NCap,$[:NK,$[:Pat,$p(:I,:c)]]]]");
    }

    #[test]
    fn check_selector_globs() {
        assert_eq!(p("*"),      "$[:Path,$[:Glob]]");
        assert_eq!(p("**"),     "$[:Path,$[:RecGlob]]");
        assert_eq!(p("^**"),    "$[:Path,$[:NCap,$[:RecGlob]]]");
        assert_eq!(p("^*"),     "$[:Path,$[:NCap,$[:Glob]]]");

        assert_eq!(p("*/*/a"),   "$[:Path,$[:Glob],$[:Glob],$[:NK,$[:Pat,$p(:I,:a)]]]");
        assert_eq!(p("*  /  *  /   a   "),   "$[:Path,$[:Glob],$[:Glob],$[:NK,$[:Pat,$p(:I,:a)]]]");
        assert_eq!(p("**/^a/**"), "$[:Path,$[:RecGlob],$[:NCap,$[:NK,$[:Pat,$p(:I,:a)]]],$[:RecGlob]]");
    }

    #[test]
    fn check_selector_kvmatch() {
        assert_eq!(p(":{b=a,a=20}"),                                "$[:Path,$[:NKVM,$[:KV,$[$[:Pat,$p(:I,:b)],$[:Pat,$p(:I,:a)]],$[$[:Pat,$p(:I,:a)],$[:Pat,$p(:I,:20)]]]]]");
        assert_eq!(p("a : { a = 20 }"),                             "$[:Path,$[:NKLA_KVM,$[:Pat,$p(:I,:a)],$[:KV,$[$[:Pat,$p(:I,:a)],$[:Pat,$p(:I,:20)]]]]]");
        assert_eq!(p("a : { a = 20, b=a(a?)[^ABC]cc*f}"),           "$[:Path,$[:NKLA_KVM,$[:Pat,$p(:I,:a)],$[:KV,$[$[:Pat,$p(:I,:a)],$[:Pat,$p(:I,:20)]],$[$[:Pat,$p(:I,:b)],$[:Pat,$p(:I,:a),$p(:PatCap,$[:Pat,$p(:I,:a),:Any]),$p(:NCCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20, b=a(a?)[ABC]cc*f}"),            "$[:Path,$[:NKLA_KVM,$[:Pat,$p(:I,:a)],$[:KV,$[$[:Pat,$p(:I,:a)],$[:Pat,$p(:I,:20)]],$[$[:Pat,$p(:I,:b)],$[:Pat,$p(:I,:a),$p(:PatCap,$[:Pat,$p(:I,:a),:Any]),$p(:CCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20 } | { b = 20 }"),                "$[:Path,$[:NKLA_KVM,$[:Pat,$p(:I,:a)],$[:Or,$[:KV,$[$[:Pat,$p(:I,:a)],$[:Pat,$p(:I,:20)]]],$[:KV,$[$[:Pat,$p(:I,:b)],$[:Pat,$p(:I,:20)]]]]]]");
        assert_eq!(p("a : { a = 20 } | { b = 20 } & { x = 10}"),    "$[:Path,$[:NKLA_KVM,$[:Pat,$p(:I,:a)],$[:Or,$[:KV,$[$[:Pat,$p(:I,:a)],$[:Pat,$p(:I,:20)]]],$[:KV,$[$[:Pat,$p(:I,:b)],$[:Pat,$p(:I,:20)]]]]]]");
    }
}
