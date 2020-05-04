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

    ident       = { ?any character except whitespace, "?", "/", "\", "|", "'", "&", ":", "[", "*" or "="? }
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

    kv_item     = "{", { key, "=", pattern }, "}"
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

fn parse_kv_item(ps: &mut State) -> Result<VVal, ParseError> {
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

    while ps.consume_if_eq('/') {
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
