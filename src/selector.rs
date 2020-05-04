use crate::vval::VVal;
use crate::vval::Syntax;

mod state;

pub use state::State;
pub use state::{ParseValueError, ParseNumberError, ParseError, ParseErrorKind};
use state::StrPart;


/*!
Selector Syntax:


```ebnf

    class_char  = { ?any character except "]"? }
                  (* special sequence: "\\" => "\" and "\]" => "]" *)
                ;

    ident       = { ?any character except whitespace,
                    "?", "/", "\", "|",
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
                | "**"
                ;

    selector    = node, { "/", node }
                ;
```

*/

fn parse_ident(ps: &mut State) -> Result<VVal, ParseError> {
    let uh = ps.take_while(|c|
        match c {
           '?' | '/' | '\\' | '|' | '{' | '}'
         | '[' | ']' | '(' | ')' | '\''
         | '&' | '$' | ':' | ';' | '*' | '='
                => false,
            _   => !c.is_whitespace()
    });

    let r =
        VVal::pair(
            VVal::new_sym("Ident"),
            VVal::new_sym(&uh.to_string()));

    ps.skip_ws();

    Ok(r)
}

fn parse_char_class(ps: &mut State) -> Result<VVal, ParseError> {
}

fn parse_capture(ps: &mut State) -> Result<VVal, ParseError> {
}

fn parse_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let pat = VVal::vec1(VVal::new_sym("Pattern"));

    while !ps.at_end() && !ps.lookahead_one_of("'&:;$)=/|") {
        let element =
            match ps.expect_some(ps.peek())? {
                '*' => VVal::new_sym("Glob"),
                '?' => VVal::new_sym("AnyChar"),
                '[' => parse_char_class(ps)?,
                '(' => parse_capture(ps)?,
                '\\' => {
                    ps.consume();
                    let next = ps.expect_some(ps.peek())?;
                    ps.consume();
                    let mut b = [0; 4];
                    VVal::pair(
                        VVal::new_sym("Ident"),
                        VVal::new_sym(next.encode_utf8(&mut b)))
                },
                _   => parse_ident(ps)?
            };
        pat.push(element)
    }

    Ok(pat)
}

fn parse_index(ps: &mut State) -> Result<VVal, ParseError> {
    let uh = ps.take_while(|c| c.is_digit(10));
    ps.skip_ws();

    if let Ok(cn) = i64::from_str_radix(&uh.to_string(), 10) {
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

    if !ps.consume_if_eq_es('}') {
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
            v.push(parse_kv_item()?);
            Ok(v)
        },
        '|' => {
            ps.consume_ws();

            let v = VVal::vec();
            v.push(VVal::new_sym("Or"));
            v.push(item);
            v.push(parse_kv_item()?);
            Ok(v)
        },
        _ => item,
    }
}

fn parse_node(ps: &mut State) -> Result<VVal, ParseError> {
    let c = ps.expect_some(ps.peek())?;

    if c == '*' && ps.lookahead("**") {
        Ok(VVal::vec1(VVal::new_sym("RecursiveGlob")))
    } else {
        match c {
            ':' => {
                ps.consume_ws();
                Ok(VVal::vec2(
                    VVal::new_sym("NodeKVM"),
                    parse_kv_match(ps)?))
            },
            _ => {
                let key = parse_key(ps)?;

                while let Some(c) = ps.peek() {
                    match c {
                        ':' => {
                            ps.consume_ws();
                            let kvm = parse_kv_match(ps)?;
                            Ok(VVal::vec3(
                                VVal::new_sym("NodeKeyLA_KVM"),
                                key, kvm))
                        },
                        _ => return
                            Ok(VVal::vec2(
                                VVal::new_sym("NodeKey"),
                                key))
                    }
                }
            }
        }
    }
}

fn parse_selector_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let selector = VVal::vec();

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

fn tree_select(slct: &VVal, tree: &VVal) -> VVal {
    let path = VVal::vec();
    slct.with_s_ref(|s| parse_select(&s.chars().collect(), path));
}
