/*!
Selector Syntax:


```ebnf

    class_char  = { ?any character except "]"? }
                  (* special sequence: "\\" => "\" and "\]" => "]" *)
                ;

    ident       = { ?any character except whitespace,
                    "?", "/", "\", "|", "^",
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
         | '&' | '$' | ':' | ';' | '*' | '='
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
            if neg { "CCls" }
            else   { "NCCls" }),
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
    let pat = VVal::vec1(VVal::new_sym("Pat"));

    while !ps.at_end() && !ps.lookahead_one_of("'&:^;$)]}=/|") {
        let element =
            match ps.expect_some(ps.peek())? {
                '*' => VVal::new_sym("Glob"),
                '?' => VVal::new_sym("Any"),
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
            ParseErrorKind::UnexpectedToken('=', "in key/value node pattern")));
    }

    let val = parse_pattern(ps)?;

    Ok(VVal::vec2(key, val))
}

fn parse_kv_item(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('{') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('{', "in key/value node pattern")));
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
            ParseErrorKind::UnexpectedToken('}', "in key/value node pattern")));
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
        Ok(VVal::vec1(VVal::new_sym("RecGlob")))
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

    #[test]
    fn check_selector_node_path() {
        assert_eq!(p("a"),     "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]]]");
        assert_eq!(p("a/b/c"), "$[:Path,$[:NK,$[:Pat,$p(:I,:a)]],$[:NK,$[:Pat,$p(:I,:b)]],$[:NK,$[:Pat,$p(:I,:c)]]]");
    }
}
