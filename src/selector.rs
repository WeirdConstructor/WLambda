/*!
Selector Syntax:


```ebnf
    (* NOTE: Whitespace is not part of a pattern in most places. This means
             if you want to match whitespace, you will have to escape
             it either with a '\', with a [ ] character class or match
             one whitespace char with $s. *)

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

    rx_atom     = pat_glob
                | ident_char
                ;

    glob_atom   = pat_glob
                | ident
                ;

    pat_regex   = "*", rx_atom     (* matches sub pattern 0 or N times *)
                | "+", rx_atom     (* matches sub pattern 1 or N times *)
                | "<", [ ("*" | "+") ], rx_atom
                                   (* non greedy version of the above *)
                | "?", rx_atom     (* matches sub pattern 0 or 1 times *)
                | "!", rx_atom     (* matches (zero width) if next pattern does not match *)
                | "=", rx_atom     (* matches (zero width) if next pattern does match *)
                | "^"              (* matches (zero width) start of string *)
                | "$"              (* matches (zero width) end of string *)
                | "s"              (* matches one whitespace character *)
                ;

    glob_group  = "(", "^", pattern, ")"    (* capturing sub group *)
                | "(", pattern, ")"         (* sub group *)
                ;

    glob_cclass = "[", { class_char }, "]"  (* character class match for 1 char *)
                | "[^", { class_char }, "]" (* negated character class match for 1 char *)
                ;

    pat_glob    = "*"                       (* 0 or N any characters *)
                | "?"                       (* any character *)
                | "$", pat_regex
                | glob_cclass
                | glob_group
                ;

    pat_branch  = { glob_atom }
                ;

    pattern     = pat_branch, { "|", pat_branch }
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

fn is_ident_char(c: char) -> bool {
    match c {
          '!' | '?' | '/' | '\\' | '|' | '{' | '}'
        | '[' | ']' | '(' | ')' | '\'' | '^'
        | '&' | '$' | ':' | ';' | '*' | '=' | ','
               => false,
           _   => !c.is_whitespace(),
    }
}

fn parse_ident_char(ps: &mut State) -> Result<Option<char>, ParseError> {
    if let Some(c) = ps.peek() {
        match c {
            '\\' => {
                ps.consume();
                let c = ps.expect_some(ps.peek())?;
                ps.consume_ws();
                Ok(Some(c))
            },
            c if is_ident_char(c) => {
                ps.consume_ws();
                Ok(Some(c))
            },
            _ => Ok(None),
        }
    } else {
        Ok(None)
    }
}

fn parse_ident(ps: &mut State, one_char: bool) -> Result<VVal, ParseError> {
    let mut uh = String::new();

    if one_char {
        if let Some(c) = parse_ident_char(ps)? {
            uh.push(c);
        }
    } else {
        while let Some(c) = parse_ident_char(ps)? {
            uh.push(c);
        }
    }

    if uh.len() == 0 {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken(
                ps.peek().unwrap_or(' '),
                "Expected identifier character")));
    }

    let r = VVal::pair(VVal::new_sym("I"), VVal::new_sym(&uh));

    Ok(r)
}

fn parse_glob_cclass(ps: &mut State) -> Result<VVal, ParseError> {
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

fn parse_pat_regex(ps: &mut State) -> Result<VVal, ParseError> {
    let c = ps.expect_some(ps.peek())?;
    match c {
        '*' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("N0"), parse_rx_atom(ps)?))
        },
        '+' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("N1"), parse_rx_atom(ps)?))
        },
        '<' => {
            ps.consume_ws();
            let c = ps.expect_some(ps.peek())?;
            match c {
                '*' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("N0-"), parse_rx_atom(ps)?))
                },
                '+' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("N1-"), parse_rx_atom(ps)?))
                },
                _ =>
                    Err(ps.err(
                        ParseErrorKind::UnexpectedToken(
                            c, "in non-greedy regex pattern"))),
            }
        }
        '?' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("Opt"), parse_rx_atom(ps)?))
        },
        '!' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("ZwNegLA"), parse_rx_atom(ps)?))
        },
        '=' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("ZwLA"), parse_rx_atom(ps)?))
        },
        '^' => { ps.consume_ws(); Ok(VVal::new_sym("Start")) },
        '$' => { ps.consume_ws(); Ok(VVal::new_sym("End")) },
        's' => { ps.consume_ws(); Ok(VVal::new_sym("WsChar")) },
        'S' => { ps.consume_ws(); Ok(VVal::new_sym("NWsChar")) },
        _ =>
            Err(ps.err(
                ParseErrorKind::UnexpectedToken(c, "in regex pattern"))),
    }
}

fn parse_glob_group(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('(') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken('(', "in sub pattern")));
    }

    let capture = ps.consume_if_eq_ws('^');
    let p       = parse_pattern(ps)?;

    if !ps.consume_if_eq_ws(')') {
        return Err(ps.err(
            ParseErrorKind::UnexpectedToken(')', "in sub pattern")));
    }

    if capture {
        Ok(VVal::pair(VVal::new_sym("PatCap"), p))
    } else {
        Ok(VVal::pair(VVal::new_sym("PatSub"), p))
    }
}

fn parse_pat_glob(ps: &mut State) -> Result<VVal, ParseError> {
    let c = ps.expect_some(ps.peek())?;
    match c {
        '*' => { ps.consume_ws(); Ok(VVal::new_sym("Glob")) },
        '?' => { ps.consume_ws(); Ok(VVal::new_sym("Any")) },
        '$' => { ps.consume_ws(); parse_pat_regex(ps) },
        '[' => parse_glob_cclass(ps),
        '(' => parse_glob_group(ps),
        _ =>
            Err(ps.err(
                ParseErrorKind::UnexpectedToken(c, "in glob pattern"))),
    }
}

fn parse_rx_atom(ps: &mut State) ->  Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' | '?' | '[' | '(' | '$'
            => parse_pat_glob(ps),
        _   => parse_ident(ps, true)
    }
}

fn parse_glob_atom(ps: &mut State) ->  Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' | '?' | '[' | '(' | '$'
            => parse_pat_glob(ps),
        _   => parse_ident(ps, false)
    }
}

fn parse_pat_branch(ps: &mut State) -> Result<VVal, ParseError> {
    let pat_branch = VVal::vec();

    while !ps.at_end() && !ps.lookahead_one_of("|:&=)]}/,") {
        println!("GO {}", ps.rest().to_string());
        pat_branch.push(parse_glob_atom(ps)?);
    }

    Ok(pat_branch)
}

fn parse_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let mut pat = parse_pat_branch(ps)?;

    let mut append = false;
    while ps.consume_if_eq_ws('|') {
        let next_opt_pat = parse_pat_branch(ps)?;

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
        println!("PARSE NODE: {}", ps.rest().to_string());
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

#[derive(Debug, Clone)]
pub struct SelectorState {
    orig_string_len: usize,
    captures:        Vec<(usize, usize)>,
}

impl SelectorState {
    fn new() -> Self {
        Self {
            orig_string_len: 0,
            captures:        Vec::new(),
        }
    }

    fn push_capture(&mut self, s: &str, len: usize) {
        let offs = self.orig_string_len - s.len();
        self.captures.push((offs, offs + len));
    }

    fn pop_capture(&mut self) {
        self.captures.pop();
    }

    fn is_str_start(&self, s: &str) -> bool {
        self.orig_string_len == s.len()
    }

    fn set_str(&mut self, s: &str) -> usize {
        std::mem::replace(&mut self.orig_string_len, s.len())
    }

    fn restore_str(&mut self, os_len: usize) {
        self.orig_string_len = os_len;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RxBuf<'a> {
    s:          &'a str,
    offs:       usize,
    orig_len:   usize,
}

impl std::fmt::Display for RxBuf<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.s)
    }
}

impl<'a> RxBuf<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            s,
            offs: 0,
            orig_len: s.len(),
        }
    }

    fn is_at_start(&self) -> bool {
        self.offs == 0
    }

    fn is_at_end(&self) -> bool {
        self.offs == self.orig_len
    }

    fn offs(&self, offs: usize) -> Self {
        Self {
            s:        &self.s[offs..],
            offs:     self.offs + offs,
            orig_len: self.orig_len,
        }
    }

    fn limit_len(&self, len: usize) -> Self {
        Self {
            s:        &self.s[..len],
            offs:     self.offs,
            orig_len: self.orig_len,
        }
    }

    fn sub(&self, offs: usize, len: usize) -> Self {
        Self {
            s:        &self.s[offs..len],
            offs:     self.offs + offs,
            orig_len: self.orig_len,
        }
    }
}

pub type PatternNode = Box<dyn Fn(RxBuf, &mut SelectorState) -> (VVal, usize)>;
pub type SelNode     = Box<dyn Fn(&VVal, &mut SelectorState, &VVal)>;

//fn compile_sub_pattern(pat: &VVal, capture: bool, mut next: PatternNode) -> PatternNode {
//    println!("COMP SUB PAT {}", pat.s());
//
//    if capture {
//        next =
//            Box::new(move |s: &str, st: &mut SelectorState| {
//                let ret = (*next)(s, st);
//                st.pop_capture();
//                ret
//            });
//    }
//
//    if pat.is_pair() {
//        let sub_type    = pat.at(0).expect("proper pattern").to_sym();
//        let sub         = pat.at(1).expect("sub pattern");
//        let mut sub_pat = compile_pattern_branch(&sub);
//
//        if capture {
//            sub_pat =
//                Box::new(move |s: &str, st: &mut SelectorState| {
//                    let (m, l) = (*sub_pat)(s, st);
//                    st.push_capture(s, l);
//                    (m, l)
//                });
//        }
//
//
//        } else {
//            Box::new(move |s: &str, st: &mut SelectorState| {
//                panic!("NOT IMPLEMENTED: {}", sub_type.to_string())
//            })
//        }
//
//    } else {
//
//        println!("COMPILE SUB PATTERN: [{}]", pat.s());
//        let mut p = compile_pattern_branch(&pat);
//
//        if capture {
//            p =
//                Box::new(move |s: &str, st: &mut SelectorState| {
//                    let (m, l) = (*p)(s, st);
//                    st.push_capture(s, l);
//                    (m, l)
//                });
//        }
//
//        Box::new(move |s: &str, st: &mut SelectorState| {
//            let (r, len1) = (*p)(s, st);
//            if !r.b() {
//                return (VVal::None, 0);
//            }
//
//            let (r, len2) = (*next)(&s[len1..], st);
//            (r, len1 + len2)
//        })
//    }
//}

macro_rules! while_lengthen_str {
    ($s: ident, $try_len: ident, $b: block) => {
        let mut $try_len = 0;

        while $try_len <= $s.s.len() {
            println!("WLS-TRY {} [{}]", $try_len, &$s.limit_len($try_len).s);
            $b;

            $try_len += 1;
            while $try_len <= $s.s.len() && !$s.s.is_char_boundary($try_len) {
                $try_len += 1;
            }
        }
    }
}

macro_rules! while_shorten_str {
    ($s: ident, $try_len: ident, $b: block) => {
        let mut $try_len = $s.s.len();

        //d// println!("WSS-TRY0 [{}]", &$s[$try_len..]);
        $b;

        while $try_len > 0 {
            $try_len -= 1;
            while $try_len > 0 && !$s.s.is_char_boundary($try_len) {
                $try_len -= 1;
            }

            //d// println!("WSS-TRY [{}]", &$s[$try_len..]);
            $b;
        }
    }
}

macro_rules! vari_str_glob {
    ($s: ident, $try_len: ident, $greedy: expr, $b: block) => {
        if $greedy {
            while_shorten_str!($s, $try_len, $b);
        } else {
            while_lengthen_str!($s, $try_len, $b);
        }
    }
}

fn compile_atom(p: &VVal, next: PatternNode) -> PatternNode {
    println!("COMPILE ATOM {}", p.s());

    if p.is_pair() {
        let pair_type = p.at(0).unwrap().to_sym();
        let pair_val  = p.at(1).unwrap_or_else(|| VVal::None);

        if pair_type == s2sym("I") {
            let key_str = pair_val.clone();

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                key_str.with_s_ref(|y| {
                    println!("I: [{}]<=>[{}]", s, y);
                    let y_len = y.len();

                    if s.s.starts_with(y) {
                        let (m, len) = (*next)(s.offs(y_len), st);
                        (m, y_len + len)
                    } else {
                        (VVal::None, 0)
                    }
                })
            })

        } else if pair_type == s2sym("CCls") {
            let chars = pair_val.clone();

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                chars.with_s_ref(|chrs| {
                    if let Some(c) = s.s.chars().nth(0) {
                        let c_len = c.len_utf8();

                        for mc in chrs.chars() {
                            if c == mc {
                                let (m, len) = (*next)(s.offs(c_len), st);
                                return (m, c_len + len);
                            }
                        }
                    }

                    (VVal::None, 0)
                })
            })

        } else if pair_type == s2sym("NCCls") {
            let chars = pair_val.clone();

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                chars.with_s_ref(|chrs| {
                    if let Some(c) = s.s.chars().nth(0) {
                        for mc in chrs.chars() {
                            if c == mc {
                                return (VVal::None, 0);
                            }
                        }

                        let c_len = c.len_utf8();

                        let (m, len) = (*next)(s.offs(c_len), st);
                        (m, c_len + len)
                    } else {
                        (VVal::None, 0)
                    }
                })
            })

        } else if pair_type == s2sym("PatSub") {
            compile_pattern_branch(&pair_val, next)

        } else if pair_type == s2sym("PatCap") {
            let sub = compile_pattern_branch(&pair_val, next); //, true, next);
            // TODO: Arrange capturing!
            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let (m, l) = (*sub)(s, st);
                (m, l)
//                if m.b() {
//                    let (m2, l2) = (*next)(&s[l..], st);
//                    (m2, l + l2)
//                } else {
//                    (VVal::None, 0)
//                }
            })

        } else if pair_type == s2sym("ZwNegLA") {
            let mut sub_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState|
                        (VVal::Bol(true), 0)));

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let (m, _) = (*sub_pat)(s, st);
                if m.b() {
                    return (VVal::None, 0);
                }

                (*next)(s, st)
            })

        } else if pair_type == s2sym("ZwLA") {
            let mut sub_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState|
                        (VVal::Bol(true), 0)));

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let (m, len) = (*sub_pat)(s, st);
                if !m.b() {
                    return (VVal::None, 0);
                }

                (*next)(s, st)
            })

        } else if pair_type == s2sym("N1") || pair_type == s2sym("N1-") {
            let sub_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState|
                        (VVal::Bol(true), 0)));

            let sub_pat =
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    let (m, len) = (*sub_pat)(s, st);
                    if !m.b() {
                        return (VVal::None, 0);
                    }

                    let mut matched     = true;
                    let mut match_len   = len;

                    while matched {
                        let (m, len) = (*sub_pat)(s.offs(match_len), st);
                        matched = m.b();
                        if matched {
                            if len == 0 { break; }
                            match_len += len;
                        }
                    }

                    (VVal::Bol(true), match_len)
                });

            let mut greedy = pair_type == s2sym("N1");

            if greedy {
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    while_shorten_str!(s, try_len, {
                        let (m_sp, len_sp) = (*sub_pat)(s.limit_len(try_len), st);
                        if m_sp.b() && len_sp == try_len {
                            let (m, len) = (*next)(s.offs(len_sp), st);
                            if m.b() {
                                return (m, try_len + len);
                            }
                        }
                    });

                    (VVal::None, 0)
                })
            } else {
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    while_lengthen_str!(s, try_len, {
                        println!("NONG s=[{}]", s.limit_len(try_len));
                        let (m_sp, len_sp) = (*sub_pat)(s.limit_len(try_len), st);
                        if m_sp.b() {
                            println!("NONG len_sp={} r=[{}]", len_sp, s.offs(len_sp).s);
                            let (m, len) = (*next)(s.offs(len_sp), st);
                            if m.b() {
                                println!("NONG len={}", len);
                                return (m, len_sp + len);
                            }
                        }
                    });

                    (VVal::None, 0)
                })
            }
        } else {
            panic!("Unknown pair atom: {}", p.s());
        }

    } else if p.to_sym() == s2sym("Any") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().nth(0) {
                let c_len = c.len_utf8();

                let (m, len) = (*next)(s.offs(c_len), st);
                (m, c_len + len)
            } else {
                (VVal::None, 0)
            }
        })

    } else if p.to_sym() == s2sym("Glob") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            while_shorten_str!(s, try_len, {
                let (m, len) = (*next)(s.offs(try_len), st);
                if m.b() {
                    return (m, try_len + len);
                }
            });

            (VVal::None, 0)
        })

    } else if p.to_sym() == s2sym("Start") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if s.is_at_start() {
                let (m, len) = (*next)(s, st);
                (m, len)
            } else {
                (VVal::None, 0)
            }
        })

    } else if p.to_sym() == s2sym("End") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            println!("MATCH END: [{}] atend={}", s, s.is_at_end());
            if s.is_at_end() {
                let (m, len) = (*next)(s, st);
                (m, len)
            } else {
                (VVal::None, 0)
            }
        })

    } else {
        panic!("UNKNOWN ATOM: {}", p.s());
    }
}

fn compile_pattern_branch(pat: &VVal, next: PatternNode) -> PatternNode {
    println!("COMPILE PATTERN [{}]", pat.s());

    let mut next : Option<PatternNode> = Some(next);

    for i in 0..pat.len() {
        let p = pat.at(pat.len() - (i + 1)).expect("pattern item");
        println!("PAT COMP: {}", p.s());

        let my_next = std::mem::replace(&mut next, None);
        next = Some(compile_atom(&p, my_next.unwrap()));
    }

    next.unwrap()
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

        let pat = compile_pattern_branch(k,
            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                // TODO: Needs to be done by the caller of compile_pattern_branch!
                //       And passed like usual as continuation.
                println!("*** BRANCH LEAF PATTERN MATCH {}| {:?}", s, st.captures);
                (VVal::Bol(true), 0)
            }));

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            for (i, (v, k)) in v.iter().enumerate() {
                if let Some(k) = k {
                    k.with_s_ref(|s| {
                        let old_str = st.set_str(&s[..]);
                        let rb = RxBuf::new(&s[..]);
                        let (r, len) = (*pat)(rb, st);
                        st.restore_str(old_str);

                        if r.b() && len == s.len() {
                            (*sn)(&v, st, capts);
                        }
                    });
                } else {
                    let idx_str = format!("{}", i);

                    let old_str = st.set_str(&idx_str[..]);
                    let rb = RxBuf::new(&idx_str[..]);
                    let (r, len) = (*pat)(rb, st);
                    st.restore_str(old_str);

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
    let pat = compile_pattern_branch(v,
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            // TODO: Needs to be done by the caller of compile_pattern_branch!
            //       And passed like usual as continuation.
            println!("*** BRANCH LEAF SINGLE PATTERN MATCH {}| {:?}", s, st.captures);
            (VVal::Bol(true), 0)
        }));

    Box::new(move |s: RxBuf, st: &mut SelectorState| {
        let mut i = 0;
        while i <= s.s.len() {
            let (r, len) = (*pat)(s.offs(i), st);
            if r.b() {
                return (VVal::new_str(s.sub(i, len).s), len);
            }

            i += 1;
            while i <= s.s.len() && !s.s.is_char_boundary(i) {
                i += 1;
            }
        }

        (VVal::None, 0)
    })
}

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
                ss.set_str(st);
                let rb = RxBuf::new(st);
                let (r, len) = (*pn)(rb, &mut ss);
                if r.is_some() {
                    r.s_raw()
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

//    #[test]
//    fn check_selector_match_path() {
//        let v1 =
//            VVal::map3("a",
//                VVal::vec3(
//                    VVal::Int(20),
//                    VVal::pair(VVal::Int(2), VVal::Int(4)),
//                    VVal::new_str("F0O")),
//                "ab",
//                VVal::vec2(
//                    VVal::Int(33),
//                    VVal::Int(44)),
//                "xyab",
//                VVal::vec3(
//                    VVal::Int(8),
//                    VVal::Int(9),
//                    VVal::map2("X", VVal::Int(10), "Y", VVal::Int(20))));
//
//        assert_eq!(pev("a",         &v1), "$[$[20,$p(2,4),\"F0O\"]]");
//        assert_eq!(pev("a/2/2",     &v1), "$[\"O\"]");
//        assert_eq!(pev("a/2/1",     &v1), "$[\"0\"]");
//        assert_eq!(pev("ab/0",      &v1), "$[33]");
//
//        assert_eq!(pev("a/?",       &v1), "$[20,$p(2,4),\"F0O\"]");
//        assert_eq!(pev("a/?/1",     &v1), "$[4,\"0\"]");
//
//        assert_eq!(pev("?/1",       &v1), "$[$p(2,4)]");
//        assert_eq!(pev("?/2",       &v1), "$[\"F0O\"]");
//
//        assert_eq!(pev("?b/1",      &v1), "$[44]");
//        assert_eq!(pev("a?/1",      &v1), "$[44]");
//        assert_eq!(pev("??ab/1",    &v1), "$[9]");
//
//        assert_eq!(pev("*/X",       &v1), "$[]");
//        assert_eq!(pev("*/?/X",     &v1), "$[10]");
//        assert_eq!(pev("*/*/X",     &v1), "$[10]");
//        assert_eq!(pev("*/2/2",     &v1), "$[\"O\"]");
//
//        assert_eq!(pev("*ab/*/X",   &v1), "$[10]");
//
//        assert_eq!(pev("[xy][xy]*/[01]",    &v1), "$[8,9]");
//        assert_eq!(pev("[^xy][^xy]/[01]",   &v1), "$[33,44]");
//        assert_eq!(pev("a/[^01]",           &v1), "$[\"F0O\"]");
//
//        assert_eq!(pev("(ab)/[01]",         &v1), "$[33,44]");
//        assert_eq!(pev("(x)y(a)b/[01]",     &v1), "$[8,9]");
//        assert_eq!(pev("$!(a)*/[01]",       &v1), "$[8,9]");
//        assert_eq!(pev("a/$![01]",          &v1), "$[\"F0O\"]");
//
//        assert_eq!(pev("$=}x*/[01]",        &v1), "$[8,9]");
//        assert_eq!(pev("$=}(ab)*/[01]",     &v1), "$[33,44]");
//        assert_eq!(pev("a$=b*/[01]",        &v1), "$[33,44]");
//        assert_eq!(pev("$!x*$=b/[01]",      &v1), "$[33,44]");
//
//        assert_eq!(pev("$+[xy]ab/0",     &v1), "$[8]");
//        assert_eq!(pev("a$+b/0",         &v1), "$[33]");
//        assert_eq!(pev("$*[xy]ab/0",     &v1), "");
//        assert_eq!(pev("$?[xy][xy]ab/0", &v1), "");
//    }
//
//    #[test]
//    fn check_selector_match_esc() {
//        let v1 =
//            VVal::map3("\\",
//                VVal::vec3(
//                    VVal::Int(20),
//                    VVal::pair(VVal::Int(2), VVal::Int(4)),
//                    VVal::new_str("F0O%/{}[]")),
//                "//",
//                VVal::vec2(
//                    VVal::Int(33),
//                    VVal::Int(44)),
//                "?*",
//                VVal::vec3(
//                    VVal::Int(8),
//                    VVal::Int(9),
//                    VVal::map2("*", VVal::Int(10), "|", VVal::Int(20))));
//
//        assert_eq!(pev("*/*/\\*", &v1),     "$[10]");
//        assert_eq!(pev("*/*/\\|", &v1),     "$[20]");
//
//        assert_eq!(pev("[\\\\]*/1", &v1),   "$[$p(2,4)]");
//        assert_eq!(pev("[\\/]*/0", &v1),    "$[33]");
//        assert_eq!(pev("\\/\\//0", &v1),    "$[33]");
//    }

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

        assert_eq!(p("(a)"),    "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:a)])]]]");
        assert_eq!(p("(^a)"),   "$[:Path,$[:NK,$[$p(:PatCap,$[$p(:I,:a)])]]]");
        assert_eq!(p("^(^a)"),  "$[:Path,$[:NCap,$[:NK,$[$p(:PatCap,$[$p(:I,:a)])]]]]");

        assert_eq!(p("(*|a?)"), "$[:Path,$[:NK,$[$p(:PatSub,$[:Opt,$[:Glob],$[$p(:I,:a),:Any]])]]]");

        assert_eq!(p("*/*/a"),   "$[:Path,$[:NK,$[:Glob]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(p("*  /  *  /   a   "),   "$[:Path,$[:NK,$[:Glob]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(p("**/^a/**"), "$[:Path,$[:RecGlob],$[:NCap,$[:NK,$[$p(:I,:a)]]],$[:RecGlob]]");

        assert_eq!(p("?a"),    "$[:Path,$[:NK,$[:Any,$p(:I,:a)]]]");
    }

    #[test]
    fn check_selector_kvmatch() {
        assert_eq!(p(":{b=a,a=20}"),                                "$[:Path,$[:NKVM,$[:KV,$[$[$p(:I,:b)],$[$p(:I,:a)]],$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]");
        assert_eq!(p("a : { a = 20 }"),                             "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]");
        assert_eq!(p("a : { a = 20, b=a(a?)[^ABC]cc*f}"),           "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]],$[$[$p(:I,:b)],$[$p(:I,:a),$p(:PatSub,$[$p(:I,:a),:Any]),$p(:NCCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20, b=a(a?)[ABC]cc*f}"),            "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]],$[$[$p(:I,:b)],$[$p(:I,:a),$p(:PatSub,$[$p(:I,:a),:Any]),$p(:CCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20 } | { b = 20 }"),                "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:Or,$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]],$[:KV,$[$[$p(:I,:b)],$[$p(:I,:20)]]]]]]");
        assert_eq!(p("a : { a = 20 } | { b = 20 } & { x = 10}"),    "$[:Path,$[:NKLA_KVM,$[$p(:I,:a)],$[:Or,$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]],$[:KV,$[$[$p(:I,:b)],$[$p(:I,:20)]]]]]]");
    }

    #[test]
    fn check_selector_subpat() {
        assert_eq!(p("(^abc$$)"),               "$[:Path,$[:NK,$[$p(:PatCap,$[$p(:I,:abc),:End])]]]");
        assert_eq!(p("(\\^abc$$)"),             "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:\"^abc\"),:End])]]]");

        assert_eq!(p("(abc)"),                  "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:abc)])]]]");
        assert_eq!(p("$!abc"),                  "$[:Path,$[:NK,$[$p(:ZwNegLA,$p(:I,:a)),$p(:I,:bc)]]]");
        assert_eq!(p("$!(abc)"),                "$[:Path,$[:NK,$[$p(:ZwNegLA,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$*(abc)"),                "$[:Path,$[:NK,$[$p(:N0,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$+(abc)"),                "$[:Path,$[:NK,$[$p(:N1,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$?(abc)"),                "$[:Path,$[:NK,$[$p(:Opt,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$=(abc)"),                "$[:Path,$[:NK,$[$p(:ZwLA,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$^abc$$"),                "$[:Path,$[:NK,$[:Start,$p(:I,:abc),:End]]]");
        assert_eq!(p("(\\$abc)"),               "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:\"$abc\")])]]]");
    }

    #[test]
    fn check_patterns() {
        assert_eq!(pat("A($<+B)",         "ABB"),         "AB");
        assert_eq!(pat("A($<+B)",         "AB"),          "AB");
        assert_eq!(pat("A($<+B)",         "ABBB"),        "AB");

        assert_eq!(pat("A($<+B)$$",       "ABB"),         "ABB");

        assert_eq!(pat("$^A($<+B)$$",     "ABB"),         "ABB");
        assert_eq!(pat("$^A($<+B)$$",     "ABBB"),        "ABBB");
        assert_eq!(pat("$^$+A($<+B)$$",   "ABBB"),        "ABBB");
        assert_eq!(pat("$^$<+A($<+B)$$",  "ABBB"),        "ABBB");
        assert_eq!(pat("$^$<+A($<+B)$$",  "ABBB"),        "ABBB");
        assert_eq!(pat("$^$<+A($<+B)C$$", "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($<+B)$$",     "AB"),          "AB");
        assert_eq!(pat("$^A($<+B)",       "ABB"),         "AB");
        assert_eq!(pat("$^A($<+B)",       "ABBB"),        "AB");
        assert_eq!(pat("$^$+A($<+B)",     "AABBB"),        "AAB");
        assert_eq!(pat("$^$<+A($<+B)",    "AABBB"),        "AAB");
        assert_eq!(pat("$^$<+A($<+B)",    "AABBB"),        "AAB");
        assert_eq!(pat("$^$<+A($<+B)C",   "AABBBC"),       "AABBBC");
        assert_eq!(pat("$^A($<+B)",       "AB"),           "AB");

        assert_eq!(pat("$^ABC$$",               "ABC"),         "ABC");
        assert_eq!(pat("$^AB$$C",               "ABC"),         "-nomatch-");
        assert_eq!(pat("A$^ABC$$",              "ABC"),         "-nomatch-");

        assert_eq!(pat("$^A($+BB)C$$",            "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($+(B)B)C$$",          "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($+($+B)B)C$$",        "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$+A($+($+B)B)C$$",      "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$+A$+(B)$+BC$$",        "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$+A((B)$+B)$$",         "ABB"),         "ABB");
        assert_eq!(pat("$^$+BC$$",                "BC"),          "BC");

        assert_eq!(pat("$^$+C$$",                 "C"),           "C");
        assert_eq!(pat("$^ABBB$+C$$",             "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$+A($+($+B)$+B)$+C$$",  "ABBBC"),       "ABBBC");

        assert_eq!(pat("$^($<+B)C$$",                   "BC"),          "BC");
        assert_eq!(pat("$^$<+A($<+B)C$$",               "ABC"),         "ABC");
        assert_eq!(pat("$^$<+A((B)$<+B)C$$",            "ABBC"),        "ABBC");
        assert_eq!(pat("$^$<+BB$$",         "BB"),       "BB");
        assert_eq!(pat("$<+BB$$",         "BB"),       "BB");
        assert_eq!(pat("$+BB$$",         "BB"),       "BB");
        assert_eq!(pat("$^$<+BB$$",         "BB"),       "BB");
        assert_eq!(pat("$^$<+B$$",         "B"),       "B");
        assert_eq!(pat("$^$<+BB$$",         "BBB"),       "BBB");
        assert_eq!(pat("$^A$<+BB$$",         "ABBB"),       "ABBB");
        assert_eq!(pat("$^A$<+BBC$$",         "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($<+B$<+B)C$$",         "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+B$<+B)C$$",         "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+(B)$<+B)C$$",         "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)C$$",      "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)C$$",      "ABBBC"),       "ABBBC");

        assert_eq!(pat("$^$<+C$$",                      "C"),           "C");
        assert_eq!(pat("$^ABBB$<+C$$",                  "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)$<+C$$",   "ABBBC"),       "ABBBC");

        assert_eq!(pat("$^A($*BB)C$$",            "ABBBC"),       "B");
        assert_eq!(pat("$^A(^B)C$$",              "ABC"),         "B");
        assert_eq!(pat("$^A(^$*B)C$$",            "ABBBC"),       "B");
//        assert_eq!(pat("(^BC)",                 "ABC"),         "BC");
//        assert_eq!(pat("$^[ ]$$",               " "),           "BC");
//        assert_eq!(pat("$^$*[ ]$$",             "   "),         "BC");
//        assert_eq!(pat("[\\t\\0\\u0101]",   "\0"),          "");
//        assert_eq!(pat("[\\t\\0\\u0101]",   "\t"),          "");
//        assert_eq!(pat("[\\t\\0\\u0101]",   "ā"),           "");
    }
}
