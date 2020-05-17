/*!
Selector and WLambda-Regex Syntax:

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

    rx_match_mod = "L"             (* transforms the input string from the match
                                      position on to lower case. *)
                 | "U"             (* transforms the input string from the match
                                      position on to upper case. *)
                 ;

    pat_regex   = "*", rx_atom     (* matches sub pattern 0 or N times *)
                | "+", rx_atom     (* matches sub pattern 1 or N times *)
                | "<", [ ("*" | "+" | "?") ], rx_atom
                                   (* non greedy version of the above *)
                | "?", rx_atom     (* matches sub pattern 0 or 1 times *)
                | "!", rx_atom     (* matches (zero width) if next pattern does not match *)
                | "=", rx_atom     (* matches (zero width) if next pattern does match *)
                | "^"              (* matches (zero width) start of string *)
                | "$"              (* matches (zero width) end of string *)
                | "s"              (* matches one whitespace character *)
                | "&", rx_match_mod
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

    pattern     = pat_branch, [ "|", pattern ]
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

pub use crate::parser::state::State;
pub use crate::parser::state::{ParseValueError, ParseNumberError, ParseError, ParseErrorKind};
pub use crate::parser::{parse_str_backslash, EscSeqValue};

use std::rc::Rc;
use std::cell::RefCell;

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
                let c =
                    match parse_str_backslash(ps)? {
                        EscSeqValue::Char(c) => c,
                        EscSeqValue::Byte(b) => b as char,
                    };
                ps.skip_ws();
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
                "identifier character")));
    }

    let r = VVal::pair(VVal::new_sym("I"), VVal::new_sym(&uh));

    Ok(r)
}

fn parse_glob_cclass(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq('[') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken('[', "char class start")));
    }

    let neg = ps.consume_if_eq('^');

    let mut chars = String::new();

    let mut c = ps.expect_some(ps.peek())?;
    while c != ']' {
        ps.consume();
        if c == '\\' {
            let c =
                match parse_str_backslash(ps)? {
                    EscSeqValue::Char(c) => c,
                    EscSeqValue::Byte(b) => b as char,
                };
            chars.push(c);
        } else {
            chars.push(c);
        }
        c = ps.expect_some(ps.peek())?;
    }

    if !ps.consume_if_eq_ws(']') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken(']', "char class end")));
    }

    Ok(VVal::pair(
        VVal::new_sym(
            if neg { "NCCls" }
            else   { "CCls" }),
        VVal::new_str_mv(chars)))
}

fn parse_pat_regex(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
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
            match ps.expect_some(ps.peek())? {
                '*' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("N0-"), parse_rx_atom(ps)?))
                },
                '+' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("N1-"), parse_rx_atom(ps)?))
                },
                '?' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("Opt-"), parse_rx_atom(ps)?))
                },
                c =>
                    Err(ps.err(
                        ParseErrorKind::UnexpectedToken(
                            c, "non-greedy regex pattern"))),
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
        '&' => { ps.consume_ws();
            match ps.expect_some(ps.peek())? {
                'L' => { ps.consume_ws(); Ok(VVal::new_sym("ToLowercase")) },
                'U' => { ps.consume_ws(); Ok(VVal::new_sym("ToUppercase")) },
                c =>
                    Err(ps.err(
                        ParseErrorKind::UnexpectedToken(c, "match modifier"))),
            }
        },
        's' => { ps.consume_ws(); Ok(VVal::new_sym("WsChar")) },
        'S' => { ps.consume_ws(); Ok(VVal::new_sym("NWsChar")) },
        c =>
            Err(ps.err(
                ParseErrorKind::UnexpectedToken(c, "regex pattern"))),
    }
}

fn parse_glob_group(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('(') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken('(', "sub pattern start")));
    }

    let capture = ps.consume_if_eq_ws('^');
    let p       = parse_pattern(ps)?;

    if !ps.consume_if_eq_ws(')') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken(')', "sub pattern end")));
    }

    if capture {
        Ok(VVal::pair(VVal::new_sym("PatCap"), p))
    } else {
        Ok(VVal::pair(VVal::new_sym("PatSub"), p))
    }
}

fn parse_pat_glob(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' => { ps.consume_ws(); Ok(VVal::new_sym("Glob")) },
        '?' => { ps.consume_ws(); Ok(VVal::new_sym("Any")) },
        '$' => { ps.consume_ws(); parse_pat_regex(ps) },
        '[' => parse_glob_cclass(ps),
        '(' => parse_glob_group(ps),
        c =>
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

    if ps.consume_if_eq_ws('|') {
        let pat_alt = parse_pattern(ps)?;
        pat = VVal::vec3(VVal::new_sym("Alt"), pat, pat_alt);
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
            ParseErrorKind::ExpectedToken('=', "key/value pattern")));
    }

    let val = parse_pattern(ps)?;

    Ok(VVal::vec2(key, val))
}

fn parse_kv_item(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('{') {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken('{', "key/value node pattern start")));
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
            ParseErrorKind::ExpectedToken('}', "in key/value node pattern end")));
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

    fn push_capture_start(&mut self, s: &RxBuf) {
        self.captures.push((s.offs, 0));
    }

    fn set_capture_end(&mut self, s: &RxBuf) -> (usize, (usize, usize)) {
        let idx = self.captures.len() - 1;
        let offs = self.captures[idx].0;
        self.captures[idx].1 = s.offs - offs;
        (idx, self.captures[idx].clone())
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
        write!(f, "{}[{},{}]", self.s, self.offs, self.orig_len)
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
            s:        &self.s[offs..(len + offs)],
            offs:     self.offs + offs,
            orig_len: self.orig_len,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CaptureNode {
    idx:  usize,
    len:  usize,
    next: Option<Box<CaptureNode>>,
}

impl CaptureNode {
    fn add_capture_to(&self, idx: usize, res: &mut PatResult) {
        if let Some(n) = &self.next {
            n.add_capture_to(idx, res);
        }

        res.add_capture((idx, (self.idx, self.len)));
    }

    fn to_test_string(&self, input: &str) -> String {
        if let Some(n) = &self.next {
            input[self.idx..(self.idx + self.len)].to_string()
            + "/"
            + &n.to_test_string(input)
        } else {
            input[self.idx..(self.idx + self.len)].to_string()
        }
    }
}

fn append_capture(cap_idx: usize, v: &mut Vec<Option<Box<CaptureNode>>>, cap: &(usize, usize)) {
    let pos = v.get_mut(cap_idx).unwrap();
    let tail = std::mem::replace(pos, None);
    *pos = Some(Box::new(CaptureNode {
        idx: cap.0,
        len: cap.1,
        next: tail
    }));
}

#[derive(Debug, Clone)]
pub struct PatResult {
    matched:        bool,
    match_len:      usize,
    offs:           usize,
    captures:       Option<Vec<Option<Box<CaptureNode>>>>,
}

impl PatResult {
    fn matched() -> Self {
        Self {
            matched:    true,
            match_len:  0,
            offs:       0,
            captures:   None,
        }
    }

    fn len(mut self, l: usize) -> Self {
        self.match_len += l;
        self
    }

    fn b(&self) -> bool { self.matched }

    fn add_len(&mut self, l: usize) -> &mut Self {
        self.match_len += l;
        self
    }

    fn merge_captures(mut self, res: &PatResult) -> Self {
        if let Some(c) = &res.captures {
            for (idx, cap) in c.iter().enumerate() {
                if let Some(cap) = cap {
                    cap.add_capture_to(idx, &mut self);
                }
            }
        }
        self
    }

    fn add_capture(&mut self, cap: (usize, (usize, usize))) {
        if self.captures.is_none() {
            self.captures = Some(vec![]);
        }
        if let Some(c) = &mut self.captures {
            let idx = cap.0;
            let cap = cap.1;
            if idx >= c.len() {
                c.resize(idx + 1, None);
            }
            append_capture(idx, c, &cap);
        }
    }

    fn capture(mut self, cap: (usize, (usize, usize))) -> Self {
        self.add_capture(cap);
        self
    }

    fn fail() -> Self {
        Self {
            matched:   false,
            match_len: 0,
            offs:      0,
            captures:  None,
        }
    }

    fn to_test_string(&self, input: &str) -> String {
        if !self.matched { return "-nomatch-".to_string() }

        //d// println!("TOTEST {:?} [{}]", self, input);

        let mut s = input[self.offs..(self.offs + self.match_len)].to_string();

        if let Some(c) = &self.captures {
            for cap in c.iter().rev() {
                if let Some(cap) = cap {
                    s += "-";
                    s += &cap.to_test_string(input);
                } else {
                    s += "$n";
                }
            }
        }

        s
    }
}

pub type PatternNode = Box<dyn Fn(RxBuf, &mut SelectorState) -> PatResult>;
pub type SelNode     = Box<dyn Fn(&VVal, &mut SelectorState, &VVal)>;

//macro_rules! while_lengthen_str {
//    ($s: ident, $try_len: ident, $b: block) => {
//        let mut $try_len = 0;
//
//        while $try_len <= $s.s.len() {
//            println!("WLS-TRY {} [{}]", $try_len, &$s.limit_len($try_len).s);
//            $b;
//
//            $try_len += 1;
//            while $try_len <= $s.s.len() && !$s.s.is_char_boundary($try_len) {
//                $try_len += 1;
//            }
//        }
//    }
//}

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
                        (*next)(s.offs(y_len), st).len(y_len)
                    } else {
                        PatResult::fail()
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
                                return (*next)(s.offs(c_len), st).len(c_len);
                            }
                        }
                    }

                    PatResult::fail()
                })
            })

        } else if pair_type == s2sym("NCCls") {
            let chars = pair_val.clone();

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                chars.with_s_ref(|chrs| {
                    if let Some(c) = s.s.chars().nth(0) {
                        for mc in chrs.chars() {
                            if c == mc {
                                return PatResult::fail();
                            }
                        }

                        let c_len = c.len_utf8();
                        (*next)(s.offs(c_len), st).len(c_len)
                    } else {
                        PatResult::fail()
                    }
                })
            })

        } else if pair_type == s2sym("PatSub") {
            compile_pattern(&pair_val, next)

        } else if pair_type == s2sym("PatCap") {
            let sub =
                compile_pattern(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState| {
                        let cap = st.set_capture_end(&s);
                        (*next)(s, st).capture(cap)
                    }));

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                st.push_capture_start(&s);
                let res = (*sub)(s, st);
                st.pop_capture();
                res
            })

        } else if pair_type == s2sym("ZwNegLA") {
            let mut sub_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState|
                        PatResult::matched()));

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let res = (*sub_pat)(s, st);
                if res.b() {
                    return PatResult::fail();
                }

                (*next)(s, st)
            })

        } else if pair_type == s2sym("ZwLA") {
            let mut sub_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState|
                        PatResult::matched()));

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let res = (*sub_pat)(s, st);
                if !res.b() {
                    return PatResult::fail();
                }

                (*next)(s, st)
            })

        } else if pair_type == s2sym("Opt")
               || pair_type == s2sym("Opt-")
        {
            let greedy = pair_type == s2sym("Opt");

            let next     : Rc<PatternNode> = Rc::from(next);
            let next_opt : Rc<PatternNode> = next.clone();

            let opt_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState|
                        (*next)(s, st)));

            if greedy {
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    let opt_res = (*opt_pat)(s, st);
                    if opt_res.b() {
                        opt_res
                    } else {
                        (*next_opt)(s, st)
                    }
                })
            } else {
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    let res = (*next_opt)(s, st);
                    if res.b() {
                        res
                    } else {
                        (*opt_pat)(s, st)
                    }
                })
            }

        } else if pair_type == s2sym("N1")
               || pair_type == s2sym("N1-")
               || pair_type == s2sym("N0")
               || pair_type == s2sym("N0-")
        {
            let next    : Rc<PatternNode> = Rc::from(next);
            let next_n0 : Rc<PatternNode> = next.clone();

            let sub_match_offs   = Rc::new(RefCell::new(None));
            let sub_match_offs_n = sub_match_offs.clone();
            let sub_pat_str      = pair_val.s();
            let sub_pat_str2     = sub_pat_str.clone();

            let sub_pat =
                compile_atom(&pair_val,
                    Box::new(move |s: RxBuf, st: &mut SelectorState| {
                        (*sub_match_offs.borrow_mut()) = Some(s.offs);
                        let r = (*next)(s, st);
                        println!("Nx PATTERN SUB(offs {})/NEXT RET({:?}) [{}] {:?} (for {})",
                                 s.offs, r, s, st.captures, sub_pat_str2);
                        r
                    }));

            let n0 =
                   pair_type == s2sym("N0")
                || pair_type == s2sym("N0-");
            let greedy =
                   pair_type == s2sym("N1")
                || pair_type == s2sym("N0");

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let mut res =
                    if n0 {
                        let mut res = (*next_n0)(s, st);
                        if !greedy && res.b() {
                            return res;
                        }
                        res
                    } else {
                        PatResult::fail()
                    };

                let mut match_offs = 0;
                while match_offs <= s.s.len() {
                    (*sub_match_offs_n.borrow_mut()) = None;
                    let next_res = (*sub_pat)(s.offs(match_offs), st);

                    if next_res.b() {
                        res = next_res.len(match_offs).merge_captures(&res);
                        if !greedy { break; }
                    }

                    if let Some(sub_pat_offs) = *sub_match_offs_n.borrow() {
                        let next_offs = sub_pat_offs - s.offs;
                        if next_offs == match_offs {
                            break;
                        }
                        match_offs = next_offs;
                    } else {
                        break;
                    }
                }

                println!("EXIT n1({})", sub_pat_str);
                res
            })
        } else {
            panic!("Unknown pair atom: {}", p.s());
        }

    } else if p.to_sym() == s2sym("WsChar") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().nth(0) {
                let c_len = c.len_utf8();
                if c.is_whitespace() {
                    return (*next)(s.offs(c_len), st).len(c_len);
                }
            }

            PatResult::fail()
        })

    } else if p.to_sym() == s2sym("NWsChar") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().nth(0) {
                let c_len = c.len_utf8();

                if !c.is_whitespace() {
                    return (*next)(s.offs(c_len), st).len(c_len);
                }
            }

            PatResult::fail()
        })

    } else if p.to_sym() == s2sym("Any") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().nth(0) {
                let c_len = c.len_utf8();

                (*next)(s.offs(c_len), st).len(c_len)
            } else {
                PatResult::fail()
            }
        })

    } else if p.to_sym() == s2sym("Glob") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            while_shorten_str!(s, try_len, {
                let res = (*next)(s.offs(try_len), st);
                if res.b() {
                    return res.len(try_len);
                }
            });

            PatResult::fail()
        })

    } else if p.to_sym() == s2sym("Start") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if s.is_at_start() {
                (*next)(s, st)
            } else {
                PatResult::fail()
            }
        })

    } else if p.to_sym() == s2sym("End") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            println!("MATCH END: [{}] atend={} [CAP {:?}]", s, s.is_at_end(), st.captures);
            if s.is_at_end() {
                (*next)(s, st)
            } else {
                PatResult::fail()
            }
        })

    } else if p.to_sym() == s2sym("ToLowercase") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            let s_lower = s.s.to_lowercase();
            let rx = RxBuf {
                s:              &s_lower[..],
                offs:           s.offs,
                orig_len:       s.orig_len,
            };
            (*next)(rx, st)
        })

    } else if p.to_sym() == s2sym("ToUppercase") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            let s_upper = s.s.to_uppercase();
            let rx = RxBuf {
                s:              &s_upper[..],
                offs:           s.offs,
                orig_len:       s.orig_len,
            };
            (*next)(rx, st)
        })

    } else if p.is_vec() {
        if p.len() == 0 {
            Box::new(move |s: RxBuf, st: &mut SelectorState| { PatResult::matched() })
        } else {
            panic!("UNKNOWN ATOM: {}", p.s());
        }

    } else {
        panic!("UNKNOWN ATOM: {}", p.s());
    }
}

fn compile_pattern_branch(pat: &VVal, next: PatternNode) -> PatternNode {
    println!("COMPILE PATTERN BRANCH [{}]", pat.s());

    let mut next : Option<PatternNode> = Some(next);

    for i in 0..pat.len() {
        let p = pat.at(pat.len() - (i + 1)).expect("pattern item");
        println!("PAT COMP: {}", p.s());

        let my_next = std::mem::replace(&mut next, None);
        next = Some(compile_atom(&p, my_next.unwrap()));
    }

    next.unwrap()
}

fn compile_pattern(pat: &VVal, next: PatternNode) -> PatternNode {
    println!("COMPILE PATTERN [{}]", pat.s());

    let first = pat.at(0).unwrap_or_else(|| VVal::None);
    if first.is_sym() && first.to_sym() == s2sym("Alt") {
        let next_a : Rc<PatternNode> = Rc::from(next);
        let next_b : Rc<PatternNode> = next_a.clone();

        let branch_a =
            compile_pattern_branch(
                &pat.at(1).expect("left hand side alt branch"),
                Box::new(move |s: RxBuf, st: &mut SelectorState|
                    (*next_a)(s, st)));

        let branch_b =
            compile_pattern(
                &pat.at(2).expect("right hand side alt branch"),
                Box::new(move |s: RxBuf, st: &mut SelectorState|
                    (*next_b)(s, st)));

        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            let res = (branch_a)(s, st);
            if res.b() {
                res
            } else {
                (branch_b)(s, st)
            }
        })
    } else {
        compile_pattern_branch(pat, next)
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

        let pat = compile_pattern(k,
            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                println!("*** BRANCH LEAF PATTERN MATCH {}| {:?}", s, st.captures);
                PatResult::matched()
            }));

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            for (i, (v, k)) in v.iter().enumerate() {
                if let Some(k) = k {
                    k.with_s_ref(|s| {
                        let old_str = st.set_str(&s[..]);
                        let rb = RxBuf::new(&s[..]);
                        let res = (*pat)(rb, st);
                        st.restore_str(old_str);

                        if res.b() && res.match_len == s.len() {
                            (*sn)(&v, st, capts);
                        }
                    });

                } else {
                    let idx_str = format!("{}", i);

                    let old_str = st.set_str(&idx_str[..]);
                    let rb = RxBuf::new(&idx_str[..]);
                    let res = (*pat)(rb, st);
                    st.restore_str(old_str);

                    if res.b() && res.match_len == idx_str.len() {
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
                next = Some(compile_node(&nod, n));
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
    let pat = compile_pattern(v,
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            println!("*** BRANCH LEAF SINGLE PATTERN MATCH {}| {:?}", s, st.captures);
            PatResult::matched()
        }));

    Box::new(move |s: RxBuf, st: &mut SelectorState| {
        let mut i = 0;
        while i <= s.s.len() {
            let mut res = (*pat)(s.offs(i), st);
            if res.b() {
                res.offs += i;
                return res;
            }

            i += 1;
            while i <= s.s.len() && !s.s.is_char_boundary(i) {
                i += 1;
            }
        }

        PatResult::fail()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex_syntax::ast::Ast;
//    use regex_syntax::ast::Literal;
    use regex_syntax::ast::LiteralKind;
//    use regex_syntax::ast::SpecialLiteralKind;
    use regex_syntax::ast::RepetitionKind;

    pub fn re_ast2wlpat(a: &Ast) -> String {
        match a {
            Ast::Empty(_) => "".to_string(),
            Ast::Flags(f) => {
                panic!("Got flags: {:?}", f);
            },
            Ast::Literal(l) => {
                match &l.kind {
                    LiteralKind::Verbatim                    => l.c.to_string(),
                    LiteralKind::Punctuation                 => l.c.to_string(),
                    LiteralKind::Octal                       => l.c.to_string(),
                    LiteralKind::HexFixed(HexLiteralKind)    => l.c.to_string(),
                    LiteralKind::HexBrace(HexLiteralKind)    => l.c.to_string(),
                    LiteralKind::Special(slk) => {
                        match &slk {
                            Bell                => "\\b".to_string(),
                            FormFeed            => "\\f".to_string(),
                            Tab                 => "\\t".to_string(),
                            LineFeed            => "\\n".to_string(),
                            CarriageReturn      => "\\r".to_string(),
                            VerticalTab         => "\\v".to_string(),
                            Space               => "[ ]".to_string(),
                        }
                    },
                }
            },
            Ast::Dot(_) => "?".to_string(),
            Ast::Assertion(a) => {
                panic!("Assertion: {:?}", a);
            },
            Ast::Class(cl) => {
                panic!("Class: {:?}", cl);
            },
            Ast::Repetition(rep) => {
                match &rep.op.kind {
                    RepetitionKind::ZeroOrOne => {
                        if rep.greedy {
                            "$?".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                        } else {
                            "$<?".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                        }
                    },
                    RepetitionKind::OneOrMore => {
                        if rep.greedy {
                            "$+".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                        } else {
                            "$<+".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                        }
                    },
                    RepetitionKind::ZeroOrMore => {
                        if rep.greedy {
                            "$*".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                        } else {
                            "$<*".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                        }
                    },
                    _ => panic!("Unimplemented rep op: {:?}", rep),
                }
            },
            Ast::Group(g) => {
                panic!("Grp: {:?}", g);
            },
            Ast::Alternation(alt) => {
                panic!("alt: {:?}", alt);
            },
            Ast::Concat(cat) => {
                let mut ret : String =
                    cat.asts.iter().map(|a| re_ast2wlpat(a)).collect::<Vec<String>>().join("");
                ret
            },
        }
    }

    pub fn re2wlpat(s: &str) -> String {
        use regex_syntax::ast::parse::Parser;
        let ast = Parser::new().parse(s).unwrap();
        println!("***\nIN:  {}\n***", s);
        println!("***\nAST: {:?}\n***", ast);
        re_ast2wlpat(&ast)
    }

    pub fn v(s: &str) -> VVal {
        let mut ctx = crate::EvalContext::new_default();
        ctx.eval(s).expect("correct compilation")
    }

    fn rep(re: &str, st: &str) -> String {
        let wlre = re2wlpat(re);
        println!("######## WLambda Regex: [ {} ]", wlre);
        pat(&wlre, st)
    }

    fn pat(pat: &str, st: &str) -> String {
        let mut ps = State::new(pat, "<pattern>");
        ps.skip_ws();
        match parse_pattern(&mut ps) {
            Ok(v) => {
                let pn = compile_single_pattern(&v);
                let mut ss = SelectorState::new();
                ss.set_str(st);
                let rb = RxBuf::new(st);
                (*pn)(rb, &mut ss).to_test_string(st)
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
        capts.sort(|a: &VVal, b: &VVal| {
            if a.is_int() || a.is_float() {
                a.compare_num(b)
            } else {
                a.compare_str(b)
            }
        });
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

        assert_eq!(pev("a/?",       &v1), "$[$p(2,4),20,\"F0O\"]");
        assert_eq!(pev("a/?/1",     &v1), "$[\"0\",4]");

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
        assert_eq!(pev("a/$![01]?",         &v1), "$[\"F0O\"]");

        assert_eq!(pev("$=x*/[01]",         &v1), "$[8,9]");
        assert_eq!(pev("$=(ab)*/[01]",      &v1), "$[33,44]");
        assert_eq!(pev("a$=b*/[01]",        &v1), "$[33,44]");
        assert_eq!(pev("$!x*$=b?/[01]",     &v1), "$[33,44]");

        assert_eq!(pev("$+[xy]ab/0",        &v1), "$[8]");
        assert_eq!(pev("a$+b/0",            &v1), "$[33]");
        assert_eq!(pev("$*[xy]ab/0",        &v1), "$[8,33]");
        assert_eq!(pev("$?[xy][xy]ab/0",    &v1), "$[8]");

        let v2 = VVal::map1("\t", VVal::Int(12));
        assert_eq!(pev("\\t",               &v2), "$[12]");
        assert_eq!(pev("[\\t]",             &v2), "$[12]");
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
    fn check_selector_kv_match() {
        let v1 = v(r#"
            $[
                ${ a = :test, x = 10 },
                ${ b = :test, x = 11 },
                ${ a = :test, x = 12 },
                ${ c = :test, y = 15, x = 22 },
                ${ a = :test, y = 16, x = 23 },
            ]
        "#);

        assert_eq!(pev("*/[xy]",        &v1), "$[10,11,12,15,16,22,23]");
        assert_eq!(pev("*:{a = test}",  &v1), "");
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

        assert_eq!(p("(a)"),    "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:a)])]]]");
        assert_eq!(p("(^a)"),   "$[:Path,$[:NK,$[$p(:PatCap,$[$p(:I,:a)])]]]");
        assert_eq!(p("^(^a)"),  "$[:Path,$[:NCap,$[:NK,$[$p(:PatCap,$[$p(:I,:a)])]]]]");

        assert_eq!(p("(*|a?)"), "$[:Path,$[:NK,$[$p(:PatSub,$[:Alt,$[:Glob],$[$p(:I,:a),:Any]])]]]");

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
        assert_eq!(pat("A",               "XXAYY"),       "A");
        assert_eq!(pat("AY",              "XXAYY"),       "AY");

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
        assert_eq!(pat("$^A($+(($+B)B)B)C$$",        "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($+(($+B))B)C$$",        "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($+(($+B)B))C$$",        "ABBBC"),       "ABBBC");
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
        assert_eq!(pat("$^$<+BB$$",                     "BB"),       "BB");
        assert_eq!(pat("$<+BB$$",                       "BB"),       "BB");
        assert_eq!(pat("$+BB$$",                        "BB"),       "BB");
        assert_eq!(pat("$^$<+BB$$",                     "BB"),       "BB");
        assert_eq!(pat("$^$<+B$$",                      "B"),       "B");
        assert_eq!(pat("$^$<+BB$$",                     "BBB"),       "BBB");
        assert_eq!(pat("$^A$<+BB$$",                    "ABBB"),       "ABBB");
        assert_eq!(pat("$^A$<+BBC$$",                   "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A($<+B$<+B)C$$",              "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+B$<+B)C$$",           "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+(B)$<+B)C$$",         "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)C$$",      "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)C$$",      "ABBBC"),       "ABBBC");

        assert_eq!(pat("$^$<+C$$",                      "C"),           "C");
        assert_eq!(pat("$^ABBB$<+C$$",                  "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)$<+C$$",   "ABBBC"),       "ABBBC");

        assert_eq!(pat("$^A($*BB)C$$",                  "ABBBC"),       "ABBBC");
        assert_eq!(pat("$^A(^B)C$$",                    "ABC"),         "ABC-B");
        assert_eq!(pat("$^A(^$*B)C$$",                  "ABBBC"),       "ABBBC-BBB");
        assert_eq!(pat("BC",                            "ABC"),         "BC");
        assert_eq!(pat("(BC)",                          "ABC"),         "BC");
        assert_eq!(pat("(^BC)",                         "ABC"),         "BC-BC");
        assert_eq!(pat("$^[ ]$$",                       " "),           " ");
        assert_eq!(pat("$^$*[ ]$$",                     "   "),         "   ");

        assert_eq!(pat("$^ $!x*$=b? $$",                "ab"),          "ab");
        assert_eq!(pat("$^ $!x*$=b? $$",                "xyab"),        "-nomatch-");

        assert_eq!(pat("$<?a",                            "a"),        "");
        assert_eq!(pat("$?a",                             "a"),        "a");
        assert_eq!(pat("$^ (^$<?a)(^$+a) $$",             "aaa"),      "aaa--aaa");
        assert_eq!(pat("$^ (^$?a)(^$+a) $$",              "aaa"),      "aaa-a-aa");

        assert_eq!(pat("$+($?abab)",                      "abbbxababb"),   "abab");
        assert_eq!(pat("$*($?abab)",                      "abbbxababb"),   "");
        assert_eq!(pat("$+(x$?abab)",                     "abbbxababb"),        "xabab");
        assert_eq!(pat("$+(x$?abab)",                     "abbbxababxababb"),   "xababxabab");
        assert_eq!(pat("$<+(x$?abab)",                    "abbbxababxababb"),   "xabab");
        assert_eq!(pat("bbb$*(x$?abab)",                  "abbbxababxababb"),   "bbbxababxabab");
        assert_eq!(pat("bbb$<*(x$?abab)x",                "abbbxababxababb"),   "bbbx");
        assert_eq!(pat("bbb$<*?ba",                       "abbbxababxababb"),   "bbbxaba");
        assert_eq!(pat("bbb$*?ba",                        "abbbxababxababb"),   "bbbxababxaba");
        assert_eq!(pat("bbb$<*(x$?abab)",                 "abbbxababxababb"),   "bbb");
        assert_eq!(pat("$*(a$?b)",                        "abbbababb"),   "ab");
        assert_eq!(pat("$*($?ab)",                        "abbbababb"),   "abbbababb");
        assert_eq!(pat("$<*($?ab)",                       "abbbababb"),   "");

        assert_eq!(pat("[\\t\\0\\u{0101}]",               "\0"),          "\u{0}");
        assert_eq!(pat("[\\t\\0\\u{0101}]",               "\t"),          "\t");
        assert_eq!(pat("[\\t\\0\\u{0101}]",               "ā"),           "ā");

        assert_eq!(pat("a$?[xy]a",                        "aa"),         "aa");
        assert_eq!(pat("$^$?[xy]$$",                      "a"),          "-nomatch-");
        assert_eq!(pat("$?[xy]",                          "x"),          "x");
        assert_eq!(pat("$?[xy]a",                         "xa"),         "xa");
        assert_eq!(pat("$?[xy][xy]abc$$",                 "xyabc"),      "xyabc");
        assert_eq!(pat("$?[xy][xy]ab",                    "xyab"),       "xyab");
        assert_eq!(pat("$?[xy][xy]ab$$",                  "xyab"),       "xyab");

        assert_eq!(pat("xyab|ab",                         "xyab"),       "xyab");
        assert_eq!(pat("xyab|ab",                         "jjab"),       "ab");
        assert_eq!(pat("(x|y|z)(y|x)(ab|)",               "xyab"),       "xyab");
        assert_eq!(pat("$+(x|y|z)(y|x)(ab|)",             "zxyab"),      "zxyab");
        assert_eq!(pat("$^ $*(x|y|z)(ab|) $$",            "zxyab"),      "zxyab");

        assert_eq!(pat("$^ $+$s $$",        "  \t\n\r  "),              "  \t\n\r  ");
        assert_eq!(pat(" $+ $S ",           "  \t\nXXJFJF\r  "),        "XXJFJF");

        assert_eq!(pat("AB $&L $+b $&U C",  " ABbbBbc "),               "ABbbBbc");
        assert_eq!(pat("$&U A$+BC",         " abbbbbc "),               "abbbbbc");
        assert_eq!(pat("$&L a$+bc",         " ABBBBBC "),               "ABBBBBC");
    }

    #[test]
    fn check_patterns_py_tests() {
        assert_eq!(pat("(ab|cd)e",                        "cde"),        "cde");
        assert_eq!(pat("$*($+a|b)",                       "ab"),        "ab");
        assert_eq!(pat("$+($+a|b)",                       "ab"),        "ab");
        assert_eq!(pat("$?($+a|b)",                       "ab"),        "a");

        assert_eq!(re2wlpat("a.b"), "a?b");
        assert_eq!(rep("a.b",                             "acb"),         "acb");
        assert_eq!(rep("a.*b",                            "acc\nccb"),    "acc\nccb");

////assert_eq!(rep("(?P<foo_123>a)(?P=foo_123",  "aa"),                        "-nomatch-");
////assert_eq!(rep("(?P<foo_123>a)(?P=1)",       "aa"),                        "-nomatch-");
////assert_eq!(rep("(?P<foo_123>a)(?P=!)",       "aa"),                        "-nomatch-");
////assert_eq!(rep("(?P<foo_123>a)(?P=foo_124",  "aa"),                        "-nomatch-");
////assert_eq!(rep("(?P<foo_123>a)",             "a"),                         "a");
////assert_eq!(rep("(?P<foo_123>a)(?P=foo_123)", "aa"),                        "a");
////assert_eq!(rep("\\1",                        "a"),                         "-nomatch-");
////assert_eq!(rep("[\\1]",                      "\x01"),                      "\x01");
//assert_eq!(rep("\\x61",                      "a"),                         "a");
//assert_eq!(rep("(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)9","abcdefghijkl9"),            "abcdefghijkl9");
//assert_eq!(rep(r"\0",                        "\0"),                        "\0");
//assert_eq!(rep(r"[\0a]",                     "\0"),                        "\0");
//assert_eq!(rep(r"[a\0]",                     "\0"),                        "\0");
//assert_eq!(rep(r"[^a\0]",                    "\0"),                        "-nomatch-");
////assert_eq!(rep(r"\a[\b]\f\n\r\t\v",          "\a\b\f\n\r\t\v"),            "\a\b\f\n\r\t\v");
////assert_eq!(rep(r"[\a][\b][\f][\n][\r][\t][\v]","\a\b\f\n\r\t\v"),            "\a\b\f\n\r\t\v");
//assert_eq!(rep(r"\c\e\g\h\i\j\k\m\o\p\q\y\z","ceghijkmopqyz"),             "ceghijkmopqyz");
//assert_eq!(rep("a.b",                        "acb"),                       "acb");
//assert_eq!(rep("a.b",                        "a\nb"),                      "-nomatch-");
//assert_eq!(rep("a.*b",                       "acc\nccb"),                  "-nomatch-");
//assert_eq!(rep("a.{4,5}b",                   "acc\nccb"),                  "-nomatch-");
//assert_eq!(rep("a.b",                        "a\rb"),                      "a\rb");
//assert_eq!(rep("a.b(?s)",                    "a\nb"),                      "a\nb");
//assert_eq!(rep("a.*(?s)b",                   "acc\nccb"),                  "acc\nccb");
//assert_eq!(rep("(?s)a.{4,5}b",               "acc\nccb"),                  "acc\nccb");
//assert_eq!(rep("(?s)a.b",                    "a\nb"),                      "a\nb");
//assert_eq!(rep("abc",                        "abc"),                       "abc");
//assert_eq!(rep("abc",                        "xbc"),                       "-nomatch-");
//assert_eq!(rep("abc",                        "axc"),                       "-nomatch-");
//assert_eq!(rep("abc",                        "abx"),                       "-nomatch-");
//assert_eq!(rep("abc",                        "xabcy"),                     "abc");
//assert_eq!(rep("abc",                        "ababc"),                     "abc");
//assert_eq!(rep("ab*c",                       "abc"),                       "abc");
//assert_eq!(rep("ab*bc",                      "abc"),                       "abc");
//assert_eq!(rep("ab*bc",                      "abbc"),                      "abbc");
//assert_eq!(rep("ab*bc",                      "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab+bc",                      "abbc"),                      "abbc");
//assert_eq!(rep("ab+bc",                      "abc"),                       "-nomatch-");
//assert_eq!(rep("ab+bc",                      "abq"),                       "-nomatch-");
//assert_eq!(rep("ab+bc",                      "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab?bc",                      "abbc"),                      "abbc");
//assert_eq!(rep("ab?bc",                      "abc"),                       "abc");
//assert_eq!(rep("ab?bc",                      "abbbbc"),                    "-nomatch-");
//assert_eq!(rep("ab?c",                       "abc"),                       "abc");
//assert_eq!(rep("^abc$",                      "abc"),                       "abc");
//assert_eq!(rep("^abc$",                      "abcc"),                      "-nomatch-");
//assert_eq!(rep("^abc",                       "abcc"),                      "abc");
//assert_eq!(rep("^abc$",                      "aabc"),                      "-nomatch-");
//assert_eq!(rep("abc$",                       "aabc"),                      "abc");
//assert_eq!(rep("^",                          "abc"),                       "-");
//assert_eq!(rep("$",                          "abc"),                       "-");
//assert_eq!(rep("a.c",                        "abc"),                       "abc");
//assert_eq!(rep("a.c",                        "axc"),                       "axc");
//assert_eq!(rep("a.*c",                       "axyzc"),                     "axyzc");
//assert_eq!(rep("a.*c",                       "axyzd"),                     "-nomatch-");
//assert_eq!(rep("a[bc]d",                     "abc"),                       "-nomatch-");
//assert_eq!(rep("a[bc]d",                     "abd"),                       "abd");
//assert_eq!(rep("a[b-d]e",                    "abd"),                       "-nomatch-");
//assert_eq!(rep("a[b-d]e",                    "ace"),                       "ace");
//assert_eq!(rep("a[b-d]",                     "aac"),                       "ac");
//assert_eq!(rep("a[-b]",                      "a-"),                        "a-");
//assert_eq!(rep("a[\\-b]",                    "a-"),                        "a-");
//assert_eq!(rep("a[b-]",                      "a-"),                        "-nomatch-");
//assert_eq!(rep("a[]b",                       "-"),                         "-nomatch-");
//assert_eq!(rep("a[",                         "-"),                         "-nomatch-");
//assert_eq!(rep("a\\",                        "-"),                         "-nomatch-");
//assert_eq!(rep("abc)",                       "-"),                         "-nomatch-");
//assert_eq!(rep("(abc",                       "-"),                         "-nomatch-");
//assert_eq!(rep("a]",                         "a]"),                        "a]");
//assert_eq!(rep("a[]]b",                      "a]b"),                       "a]b");
//assert_eq!(rep("a[\\]]b",                     "a]b"),                       "a]b");
//assert_eq!(rep("a[^bc]d",                    "aed"),                       "aed");
//assert_eq!(rep("a[^bc]d",                    "abd"),                       "-nomatch-");
//assert_eq!(rep("a[^-b]c",                    "adc"),                       "adc");
//assert_eq!(rep("a[^-b]c",                    "a-c"),                       "-nomatch-");
//assert_eq!(rep("a[^]b]c",                    "a]c"),                       "-nomatch-");
//assert_eq!(rep("a[^]b]c",                    "adc"),                       "adc");
//assert_eq!(rep("\\ba\\b",                    "a-"),                        "-");
//assert_eq!(rep("\\ba\\b",                    "-a"),                        "-");
//assert_eq!(rep("\\ba\\b",                    "-a-"),                       "-");
//assert_eq!(rep("\\by\\b",                    "xy"),                        "-nomatch-");
//assert_eq!(rep("\\by\\b",                    "yz"),                        "-nomatch-");
//assert_eq!(rep("\\by\\b",                    "xyz"),                       "-nomatch-");
//assert_eq!(rep("x\\b",                       "xyz"),                       "-nomatch-");
//assert_eq!(rep("x\\B",                       "xyz"),                       "-");
//assert_eq!(rep("\\Bz",                       "xyz"),                       "-");
//assert_eq!(rep("z\\B",                       "xyz"),                       "-nomatch-");
//assert_eq!(rep("\\Bx",                       "xyz"),                       "-nomatch-");
//assert_eq!(rep("\\Ba\\B",                    "a-"),                        "-nomatch-");
//assert_eq!(rep("\\Ba\\B",                    "-a"),                        "-nomatch-");
//assert_eq!(rep("\\Ba\\B",                    "-a-"),                       "-nomatch-");
//assert_eq!(rep("\\By\\B",                    "xy"),                        "-nomatch-");
//assert_eq!(rep("\\By\\B",                    "yz"),                        "-nomatch-");
//assert_eq!(rep("\\By\\b",                    "xy"),                        "-");
//assert_eq!(rep("\\by\\B",                    "yz"),                        "-");
//assert_eq!(rep("\\By\\B",                    "xyz"),                       "-");
//assert_eq!(rep("ab|cd",                      "abc"),                       "ab");
//assert_eq!(rep("ab|cd",                      "abcd"),                      "ab");
//assert_eq!(rep("()ef",                       "def"),                       "ef-");
//assert_eq!(rep("$b",                         "b"),                         "-nomatch-");
//assert_eq!(rep("a\\(b",                      "a(b"),                       "a(b-Error");
//assert_eq!(rep("a\\(*b",                     "ab"),                        "ab");
//assert_eq!(rep("a\\(*b",                     "a((b"),                      "a((b");
//assert_eq!(rep("a\\\\b",                     "a\\b"),                      "a\\b");
//assert_eq!(rep("((a))",                      "abc"),                       "a-a-a");
//assert_eq!(rep("(a)b(c)",                    "abc"),                       "abc-a-c");
//assert_eq!(rep("a+b+c",                      "aabbabc"),                   "abc");
//assert_eq!(rep("(a+|b)*",                    "ab"),                        "ab-b");
//assert_eq!(rep("(a+|b)+",                    "ab"),                        "ab-b");
//assert_eq!(rep("(a+|b)?",                    "ab"),                        "a-a");
//assert_eq!(rep(")(",                         "-"),                         "-nomatch-");
//assert_eq!(rep("[^ab]*",                     "cde"),                       "cde");
//assert_eq!(rep("a|b|c|d|e",                  "e"),                         "e");
//assert_eq!(rep("(a|b|c|d|e)f",               "ef"),                        "ef-e");
//assert_eq!(rep("abcd*efg",                   "abcdefg"),                   "abcdefg");
//assert_eq!(rep("ab*",                        "xabyabbbz"),                 "ab");
//assert_eq!(rep("ab*",                        "xayabbbz"),                  "a");
//assert_eq!(rep("(ab|cd)e",                   "abcde"),                     "cde-cd");
//assert_eq!(rep("[abhgefdc]ij",               "hij"),                       "hij");
//assert_eq!(rep("^(ab|cd)e",                  "abcde"),                     "-nomatch-");
//assert_eq!(rep("(abc|)ef",                   "abcdef"),                    "ef-");
//assert_eq!(rep("(a|b)c*d",                   "abcd"),                      "bcd-b");
//assert_eq!(rep("(ab|ab*)bc",                 "abc"),                       "abc-a");
//assert_eq!(rep("a([bc]*)c*",                 "abc"),                       "abc-bc");
//assert_eq!(rep("a([bc]*)(c*d)",              "abcd"),                      "abcd-bc-d");
//assert_eq!(rep("a([bc]+)(c*d)",              "abcd"),                      "abcd-bc-d");
//assert_eq!(rep("a([bc]*)(c+d)",              "abcd"),                      "abcd-b-cd");
//assert_eq!(rep("a[bcd]*dcdcde",              "adcdcde"),                   "adcdcde");
//assert_eq!(rep("a[bcd]+dcdcde",              "adcdcde"),                   "-nomatch-");
//assert_eq!(rep("(ab|a)b*c",                  "abc"),                       "abc-ab");
//assert_eq!(rep("((a)(b)c)(d)",               "abcd"),                      "abc-a-b-d");
//assert_eq!(rep("[a-zA-Z_][a-zA-Z0-9_]*",     "alpha"),                     "alpha");
//assert_eq!(rep("^a(bc+|b[eh])g|.h$",         "abh"),                       "bh-None");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "effgz"),                     "effgz-effgz-None");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "ij"),                        "ij-ij-j");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "effg"),                      "-nomatch-");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "bcdd"),                      "-nomatch-");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "reffgz"),                    "effgz-effgz-None");
//assert_eq!(rep("(((((((((a)))))))))",        "a"),                         "a");
//assert_eq!(rep("multiple words of text",     "uh-uh"),                     "-nomatch-");
//assert_eq!(rep("multiple words",             "multiple words, yeah"),      "multiple words");
//assert_eq!(rep("(.*)c(.*)",                  "abcde"),                     "abcde-ab-de");
//assert_eq!(rep("\\((.*), (.*)\\)",           "(a, b)"),                    "b-a");
//assert_eq!(rep("[k]",                        "ab"),                        "-nomatch-");
//assert_eq!(rep("a[-]?c",                     "ac"),                        "ac");
//assert_eq!(rep("(abc)\\1",                   "abcabc"),                    "abc");
//assert_eq!(rep("([a-c]*)\\1",                "abcabc"),                    "abc");
//assert_eq!(rep("^(.+)?B",                    "AB"),                        "A");
//assert_eq!(rep("(a+).\\1$",                  "aaaaa"),                     "aaaaa-aa");
//assert_eq!(rep("^(a+).\\1$",                 "aaaa"),                      "-nomatch-");
//assert_eq!(rep("(abc)\\1",                   "abcabc"),                    "abcabc-abc");
//assert_eq!(rep("([a-c]+)\\1",                "abcabc"),                    "abcabc-abc");
//assert_eq!(rep("(a)\\1",                     "aa"),                        "aa-a");
//assert_eq!(rep("(a+)\\1",                    "aa"),                        "aa-a");
//assert_eq!(rep("(a+)+\\1",                   "aa"),                        "aa-a");
//assert_eq!(rep("(a).+\\1",                   "aba"),                       "aba-a");
//assert_eq!(rep("(a)ba*\\1",                  "aba"),                       "aba-a");
//assert_eq!(rep("(aa|a)a\\1$",                "aaa"),                       "aaa-a");
//assert_eq!(rep("(a|aa)a\\1$",                "aaa"),                       "aaa-a");
//assert_eq!(rep("(a+)a\\1$",                  "aaa"),                       "aaa-a");
//assert_eq!(rep("([abc]*)\\1",                "abcabc"),                    "abcabc-abc");
//assert_eq!(rep("(a)(b)c|ab",                 "ab"),                        "ab-None-None");
//assert_eq!(rep("(a)+x",                      "aaax"),                      "aaax-a");
//assert_eq!(rep("([ac])+x",                   "aacx"),                      "aacx-c");
//assert_eq!(rep("([^/]*/)*sub1/",             "d:msgs/tdir/sub1/trial/away.cpp"),"d:msgs/tdir/sub1/-tdir/");
//assert_eq!(rep("([^.]*)\\.([^:]*):[T ]+(.*)","track1.title:TBlah blah blah"),"track1.title:TBlah blah blah-track1-title-Blah blah blah");
//assert_eq!(rep("([^N]*N)+",                  "abNNxyzN"),                  "abNNxyzN-xyzN");
//assert_eq!(rep("([^N]*N)+",                  "abNNxyz"),                   "abNN-N");
//assert_eq!(rep("([abc]*)x",                  "abcx"),                      "abcx-abc");
//assert_eq!(rep("([abc]*)x",                  "abc"),                       "-nomatch-");
//assert_eq!(rep("([xyz]*)x",                  "abcx"),                      "x-");
//assert_eq!(rep("(a)+b|aac",                  "aac"),                       "aac-None");
//assert_eq!(rep("(?P<i d>aaa)a",              "aaaa"),                      "-nomatch-");
//assert_eq!(rep("(?P<id>aaa)a",               "aaaa"),                      "aaaa-aaa");
//assert_eq!(rep("(?P<id>aa)(?P=id)",          "aaaa"),                      "aaaa-aa");
//assert_eq!(rep("(?P<id>aa)(?P=xd)",          "aaaa"),                      "-nomatch-");
//assert_eq!(rep("\\1",                        "a"),                         "-nomatch-");
//assert_eq!(rep("\\141",                      "a"),                         "a");
//assert_eq!(rep("(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\119","abcdefghijklk9"),            "abcdefghijklk9-k");
//assert_eq!(rep("abc",                        "abc"),                       "abc");
//assert_eq!(rep("abc",                        "xbc"),                       "-nomatch-");
//assert_eq!(rep("abc",                        "axc"),                       "-nomatch-");
//assert_eq!(rep("abc",                        "abx"),                       "-nomatch-");
//assert_eq!(rep("abc",                        "xabcy"),                     "abc");
//assert_eq!(rep("abc",                        "ababc"),                     "abc");
//assert_eq!(rep("ab*c",                       "abc"),                       "abc");
//assert_eq!(rep("ab*bc",                      "abc"),                       "abc");
//assert_eq!(rep("ab*bc",                      "abbc"),                      "abbc");
//assert_eq!(rep("ab*bc",                      "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab{0,}bc",                   "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab+bc",                      "abbc"),                      "abbc");
//assert_eq!(rep("ab+bc",                      "abc"),                       "-nomatch-");
//assert_eq!(rep("ab+bc",                      "abq"),                       "-nomatch-");
//assert_eq!(rep("ab{1,}bc",                   "abq"),                       "-nomatch-");
//assert_eq!(rep("ab+bc",                      "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab{1,}bc",                   "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab{1,3}bc",                  "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab{3,4}bc",                  "abbbbc"),                    "abbbbc");
//assert_eq!(rep("ab{4,5}bc",                  "abbbbc"),                    "-nomatch-");
//assert_eq!(rep("ab?bc",                      "abbc"),                      "abbc");
//assert_eq!(rep("ab?bc",                      "abc"),                       "abc");
//assert_eq!(rep("ab{0,1}bc",                  "abc"),                       "abc");
//assert_eq!(rep("ab?bc",                      "abbbbc"),                    "-nomatch-");
//assert_eq!(rep("ab?c",                       "abc"),                       "abc");
//assert_eq!(rep("ab{0,1}c",                   "abc"),                       "abc");
//assert_eq!(rep("^abc$",                      "abc"),                       "abc");
//assert_eq!(rep("^abc$",                      "abcc"),                      "-nomatch-");
//assert_eq!(rep("^abc",                       "abcc"),                      "abc");
//assert_eq!(rep("^abc$",                      "aabc"),                      "-nomatch-");
//assert_eq!(rep("abc$",                       "aabc"),                      "abc");
//assert_eq!(rep("^",                          "abc"),                       "");
//assert_eq!(rep("$",                          "abc"),                       "");
//assert_eq!(rep("a.c",                        "abc"),                       "abc");
//assert_eq!(rep("a.c",                        "axc"),                       "axc");
//assert_eq!(rep("a.*c",                       "axyzc"),                     "axyzc");
//assert_eq!(rep("a.*c",                       "axyzd"),                     "-nomatch-");
//assert_eq!(rep("a[bc]d",                     "abc"),                       "-nomatch-");
//assert_eq!(rep("a[bc]d",                     "abd"),                       "abd");
//assert_eq!(rep("a[b-d]e",                    "abd"),                       "-nomatch-");
//assert_eq!(rep("a[b-d]e",                    "ace"),                       "ace");
//assert_eq!(rep("a[b-d]",                     "aac"),                       "ac");
//assert_eq!(rep("a[-b]",                      "a-"),                        "a-");
//assert_eq!(rep("a[b-]",                      "a-"),                        "a-");
//assert_eq!(rep("a[b-a]",                     "-"),                         "-nomatch-");
//assert_eq!(rep("a[]b",                       "-"),                         "-nomatch-");
//assert_eq!(rep("a[",                         "-"),                         "-nomatch-");
//assert_eq!(rep("a]",                         "a]"),                        "a]");
//assert_eq!(rep("a[]]b",                      "a]b"),                       "a]b");
//assert_eq!(rep("a[^bc]d",                    "aed"),                       "aed");
//assert_eq!(rep("a[^bc]d",                    "abd"),                       "-nomatch-");
//assert_eq!(rep("a[^-b]c",                    "adc"),                       "adc");
//assert_eq!(rep("a[^-b]c",                    "a-c"),                       "-nomatch-");
//assert_eq!(rep("a[^]b]c",                    "a]c"),                       "-nomatch-");
//assert_eq!(rep("a[^]b]c",                    "adc"),                       "adc");
//assert_eq!(rep("ab|cd",                      "abc"),                       "ab");
//assert_eq!(rep("ab|cd",                      "abcd"),                      "ab");
//assert_eq!(rep("()ef",                       "def"),                       "ef-");
//assert_eq!(rep("*a",                         "-"),                         "-nomatch-");
//assert_eq!(rep("(*)b",                       "-"),                         "-nomatch-");
//assert_eq!(rep("$b",                         "b"),                         "-nomatch-");
//assert_eq!(rep("a\\",                        "-"),                         "-nomatch-");
//assert_eq!(rep("a\\(b",                      "a(b"),                       "a(b-Error");
//assert_eq!(rep("a\\(*b",                     "ab"),                        "ab");
//assert_eq!(rep("a\\(*b",                     "a((b"),                      "a((b");
//assert_eq!(rep("a\\\\b",                     "a\\b"),                      "a\\b");
//assert_eq!(rep("abc)",                       "-"),                         "-nomatch-");
//assert_eq!(rep("(abc",                       "-"),                         "-nomatch-");
//assert_eq!(rep("((a))",                      "abc"),                       "a-a-a");
//assert_eq!(rep("(a)b(c)",                    "abc"),                       "abc-a-c");
//assert_eq!(rep("a+b+c",                      "aabbabc"),                   "abc");
//assert_eq!(rep("a{1,}b{1,}c",                "aabbabc"),                   "abc");
//assert_eq!(rep("a**",                        "-"),                         "-nomatch-");
//assert_eq!(rep("a.+?c",                      "abcabc"),                    "abc");
//assert_eq!(rep("(a+|b)*",                    "ab"),                        "ab-b");
//assert_eq!(rep("(a+|b){0,}",                 "ab"),                        "ab-b");
//assert_eq!(rep("(a+|b)+",                    "ab"),                        "ab-b");
//assert_eq!(rep("(a+|b){1,}",                 "ab"),                        "ab-b");
//assert_eq!(rep("(a+|b)?",                    "ab"),                        "a-a");
//assert_eq!(rep("(a+|b){0,1}",                "ab"),                        "a-a");
//assert_eq!(rep(")(",                         "-"),                         "-nomatch-");
//assert_eq!(rep("[^ab]*",                     "cde"),                       "cde");
//assert_eq!(rep("([abc])*d",                  "abbbcd"),                    "abbbcd-c");
//assert_eq!(rep("([abc])*bcd",                "abcd"),                      "abcd-a");
//assert_eq!(rep("a|b|c|d|e",                  "e"),                         "e");
//assert_eq!(rep("(a|b|c|d|e)f",               "ef"),                        "ef-e");
//assert_eq!(rep("abcd*efg",                   "abcdefg"),                   "abcdefg");
//assert_eq!(rep("ab*",                        "xabyabbbz"),                 "ab");
//assert_eq!(rep("ab*",                        "xayabbbz"),                  "a");
//assert_eq!(rep("(ab|cd)e",                   "abcde"),                     "cde-cd");
//assert_eq!(rep("[abhgefdc]ij",               "hij"),                       "hij");
//assert_eq!(rep("^(ab|cd)e",                  "abcde"),                     "-nomatch-");
//assert_eq!(rep("(abc|)ef",                   "abcdef"),                    "ef-");
//assert_eq!(rep("(a|b)c*d",                   "abcd"),                      "bcd-b");
//assert_eq!(rep("(ab|ab*)bc",                 "abc"),                       "abc-a");
//assert_eq!(rep("a([bc]*)c*",                 "abc"),                       "abc-bc");
//assert_eq!(rep("a([bc]*)(c*d)",              "abcd"),                      "abcd-bc-d");
//assert_eq!(rep("a([bc]+)(c*d)",              "abcd"),                      "abcd-bc-d");
//assert_eq!(rep("a([bc]*)(c+d)",              "abcd"),                      "abcd-b-cd");
//assert_eq!(rep("a[bcd]*dcdcde",              "adcdcde"),                   "adcdcde");
//assert_eq!(rep("a[bcd]+dcdcde",              "adcdcde"),                   "-nomatch-");
//assert_eq!(rep("(ab|a)b*c",                  "abc"),                       "abc-ab");
//assert_eq!(rep("((a)(b)c)(d)",               "abcd"),                      "abc-a-b-d");
//assert_eq!(rep("[a-zA-Z_][a-zA-Z0-9_]*",     "alpha"),                     "alpha");
//assert_eq!(rep("^a(bc+|b[eh])g|.h$",         "abh"),                       "bh-None");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "effgz"),                     "effgz-effgz-None");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "ij"),                        "ij-ij-j");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "effg"),                      "-nomatch-");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "bcdd"),                      "-nomatch-");
//assert_eq!(rep("(bc+d$|ef*g.|h?i(j|k))",     "reffgz"),                    "effgz-effgz-None");
//assert_eq!(rep("((((((((((a))))))))))",      "a"),                         "a");
//assert_eq!(rep("((((((((((a))))))))))\\10",  "aa"),                        "aa");
//assert_eq!(rep("((((((((((a))))))))))\\41",  "aa"),                        "-nomatch-");
//assert_eq!(rep("((((((((((a))))))))))\\41",  "a!"),                        "a!");
//assert_eq!(rep("(((((((((a)))))))))",        "a"),                         "a");
//assert_eq!(rep("multiple words of text",     "uh-uh"),                     "-nomatch-");
//assert_eq!(rep("multiple words",             "multiple words, yeah"),      "multiple words");
//assert_eq!(rep("(.*)c(.*)",                  "abcde"),                     "abcde-ab-de");
//assert_eq!(rep("\\((.*), (.*)\\)",           "(a, b)"),                    "b-a");
//assert_eq!(rep("[k]",                        "ab"),                        "-nomatch-");
//assert_eq!(rep("a[-]?c",                     "ac"),                        "ac");
//assert_eq!(rep("(abc)\\1",                   "abcabc"),                    "abc");
//assert_eq!(rep("([a-c]*)\\1",                "abcabc"),                    "abc");
//assert_eq!(rep("(?i)abc",                    "ABC"),                       "ABC");
//assert_eq!(rep("(?i)abc",                    "XBC"),                       "-nomatch-");
//assert_eq!(rep("(?i)abc",                    "AXC"),                       "-nomatch-");
//assert_eq!(rep("(?i)abc",                    "ABX"),                       "-nomatch-");
//assert_eq!(rep("(?i)abc",                    "XABCY"),                     "ABC");
//assert_eq!(rep("(?i)abc",                    "ABABC"),                     "ABC");
//assert_eq!(rep("(?i)ab*c",                   "ABC"),                       "ABC");
//assert_eq!(rep("(?i)ab*bc",                  "ABC"),                       "ABC");
//assert_eq!(rep("(?i)ab*bc",                  "ABBC"),                      "ABBC");
//assert_eq!(rep("(?i)ab*?bc",                 "ABBBBC"),                    "ABBBBC");
//assert_eq!(rep("(?i)ab{0,}?bc",              "ABBBBC"),                    "ABBBBC");
//assert_eq!(rep("(?i)ab+?bc",                 "ABBC"),                      "ABBC");
//assert_eq!(rep("(?i)ab+bc",                  "ABC"),                       "-nomatch-");
//assert_eq!(rep("(?i)ab+bc",                  "ABQ"),                       "-nomatch-");
//assert_eq!(rep("(?i)ab{1,}bc",               "ABQ"),                       "-nomatch-");
//assert_eq!(rep("(?i)ab+bc",                  "ABBBBC"),                    "ABBBBC");
//assert_eq!(rep("(?i)ab{1,}?bc",              "ABBBBC"),                    "ABBBBC");
//assert_eq!(rep("(?i)ab{1,3}?bc",             "ABBBBC"),                    "ABBBBC");
//assert_eq!(rep("(?i)ab{3,4}?bc",             "ABBBBC"),                    "ABBBBC");
//assert_eq!(rep("(?i)ab{4,5}?bc",             "ABBBBC"),                    "-nomatch-");
//assert_eq!(rep("(?i)ab??bc",                 "ABBC"),                      "ABBC");
//assert_eq!(rep("(?i)ab??bc",                 "ABC"),                       "ABC");
//assert_eq!(rep("(?i)ab{0,1}?bc",             "ABC"),                       "ABC");
//assert_eq!(rep("(?i)ab??bc",                 "ABBBBC"),                    "-nomatch-");
//assert_eq!(rep("(?i)ab??c",                  "ABC"),                       "ABC");
//assert_eq!(rep("(?i)ab{0,1}?c",              "ABC"),                       "ABC");
//assert_eq!(rep("(?i)^abc$",                  "ABC"),                       "ABC");
//assert_eq!(rep("(?i)^abc$",                  "ABCC"),                      "-nomatch-");
//assert_eq!(rep("(?i)^abc",                   "ABCC"),                      "ABC");
//assert_eq!(rep("(?i)^abc$",                  "AABC"),                      "-nomatch-");
//assert_eq!(rep("(?i)abc$",                   "AABC"),                      "ABC");
//assert_eq!(rep("(?i)^",                      "ABC"),                       "");
//assert_eq!(rep("(?i)$",                      "ABC"),                       "");
//assert_eq!(rep("(?i)a.c",                    "ABC"),                       "ABC");
//assert_eq!(rep("(?i)a.c",                    "AXC"),                       "AXC");
//assert_eq!(rep("(?i)a.*?c",                  "AXYZC"),                     "AXYZC");
//assert_eq!(rep("(?i)a.*c",                   "AXYZD"),                     "-nomatch-");
//assert_eq!(rep("(?i)a[bc]d",                 "ABC"),                       "-nomatch-");
//assert_eq!(rep("(?i)a[bc]d",                 "ABD"),                       "ABD");
//assert_eq!(rep("(?i)a[b-d]e",                "ABD"),                       "-nomatch-");
//assert_eq!(rep("(?i)a[b-d]e",                "ACE"),                       "ACE");
//assert_eq!(rep("(?i)a[b-d]",                 "AAC"),                       "AC");
//assert_eq!(rep("(?i)a[-b]",                  "A-"),                        "A-");
//assert_eq!(rep("(?i)a[b-]",                  "A-"),                        "A-");
//assert_eq!(rep("(?i)a[b-a]",                 "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)a[]b",                   "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)a[",                     "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)a]",                     "A]"),                        "A]");
//assert_eq!(rep("(?i)a[]]b",                  "A]B"),                       "A]B");
//assert_eq!(rep("(?i)a[^bc]d",                "AED"),                       "AED");
//assert_eq!(rep("(?i)a[^bc]d",                "ABD"),                       "-nomatch-");
//assert_eq!(rep("(?i)a[^-b]c",                "ADC"),                       "ADC");
//assert_eq!(rep("(?i)a[^-b]c",                "A-C"),                       "-nomatch-");
//assert_eq!(rep("(?i)a[^]b]c",                "A]C"),                       "-nomatch-");
//assert_eq!(rep("(?i)a[^]b]c",                "ADC"),                       "ADC");
//assert_eq!(rep("(?i)ab|cd",                  "ABC"),                       "AB");
//assert_eq!(rep("(?i)ab|cd",                  "ABCD"),                      "AB");
//assert_eq!(rep("(?i)()ef",                   "DEF"),                       "EF-");
//assert_eq!(rep("(?i)*a",                     "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)(*)b",                   "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)$b",                     "B"),                         "-nomatch-");
//assert_eq!(rep("(?i)a\\",                    "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)a\\(b",                  "A(B"),                       "A(B-Error");
//assert_eq!(rep("(?i)a\\(*b",                 "AB"),                        "AB");
//assert_eq!(rep("(?i)a\\(*b",                 "A((B"),                      "A((B");
//assert_eq!(rep("(?i)a\\\\b",                 "A\\B"),                      "A\\B");
//assert_eq!(rep("(?i)abc)",                   "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)(abc",                   "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)((a))",                  "ABC"),                       "A-A-A");
//assert_eq!(rep("(?i)(a)b(c)",                "ABC"),                       "ABC-A-C");
//assert_eq!(rep("(?i)a+b+c",                  "AABBABC"),                   "ABC");
//assert_eq!(rep("(?i)a{1,}b{1,}c",            "AABBABC"),                   "ABC");
//assert_eq!(rep("(?i)a**",                    "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)a.+?c",                  "ABCABC"),                    "ABC");
//assert_eq!(rep("(?i)a.*?c",                  "ABCABC"),                    "ABC");
//assert_eq!(rep("(?i)a.{0,5}?c",              "ABCABC"),                    "ABC");
//assert_eq!(rep("(?i)(a+|b)*",                "AB"),                        "AB-B");
//assert_eq!(rep("(?i)(a+|b){0,}",             "AB"),                        "AB-B");
//assert_eq!(rep("(?i)(a+|b)+",                "AB"),                        "AB-B");
//assert_eq!(rep("(?i)(a+|b){1,}",             "AB"),                        "AB-B");
//assert_eq!(rep("(?i)(a+|b)?",                "AB"),                        "A-A");
//assert_eq!(rep("(?i)(a+|b){0,1}",            "AB"),                        "A-A");
//assert_eq!(rep("(?i)(a+|b){0,1}?",           "AB"),                        "-None");
//assert_eq!(rep("(?i))(",                     "-"),                         "-nomatch-");
//assert_eq!(rep("(?i)[^ab]*",                 "CDE"),                       "CDE");
//assert_eq!(rep("(?i)([abc])*d",              "ABBBCD"),                    "ABBBCD-C");
//assert_eq!(rep("(?i)([abc])*bcd",            "ABCD"),                      "ABCD-A");
//assert_eq!(rep("(?i)a|b|c|d|e",              "E"),                         "E");
//assert_eq!(rep("(?i)(a|b|c|d|e)f",           "EF"),                        "EF-E");
//assert_eq!(rep("(?i)abcd*efg",               "ABCDEFG"),                   "ABCDEFG");
//assert_eq!(rep("(?i)ab*",                    "XABYABBBZ"),                 "AB");
//assert_eq!(rep("(?i)ab*",                    "XAYABBBZ"),                  "A");
//assert_eq!(rep("(?i)(ab|cd)e",               "ABCDE"),                     "CDE-CD");
//assert_eq!(rep("(?i)[abhgefdc]ij",           "HIJ"),                       "HIJ");
//assert_eq!(rep("(?i)^(ab|cd)e",              "ABCDE"),                     "-nomatch-");
//assert_eq!(rep("(?i)(abc|)ef",               "ABCDEF"),                    "EF-");
//assert_eq!(rep("(?i)(a|b)c*d",               "ABCD"),                      "BCD-B");
//assert_eq!(rep("(?i)(ab|ab*)bc",             "ABC"),                       "ABC-A");
//assert_eq!(rep("(?i)a([bc]*)c*",             "ABC"),                       "ABC-BC");
//assert_eq!(rep("(?i)a([bc]*)(c*d)",          "ABCD"),                      "ABCD-BC-D");
//assert_eq!(rep("(?i)a([bc]+)(c*d)",          "ABCD"),                      "ABCD-BC-D");
//assert_eq!(rep("(?i)a([bc]*)(c+d)",          "ABCD"),                      "ABCD-B-CD");
//assert_eq!(rep("(?i)a[bcd]*dcdcde",          "ADCDCDE"),                   "ADCDCDE");
//assert_eq!(rep("(?i)a[bcd]+dcdcde",          "ADCDCDE"),                   "-nomatch-");
//assert_eq!(rep("(?i)(ab|a)b*c",              "ABC"),                       "ABC-AB");
//assert_eq!(rep("(?i)((a)(b)c)(d)",           "ABCD"),                      "ABC-A-B-D");
//assert_eq!(rep("(?i)[a-zA-Z_][a-zA-Z0-9_]*", "ALPHA"),                     "ALPHA");
//assert_eq!(rep("(?i)^a(bc+|b[eh])g|.h$",     "ABH"),                       "BH-None");
//assert_eq!(rep("(?i)(bc+d$|ef*g.|h?i(j|k))", "EFFGZ"),                     "EFFGZ-EFFGZ-None");
//assert_eq!(rep("(?i)(bc+d$|ef*g.|h?i(j|k))", "IJ"),                        "IJ-IJ-J");
//assert_eq!(rep("(?i)(bc+d$|ef*g.|h?i(j|k))", "EFFG"),                      "-nomatch-");
//assert_eq!(rep("(?i)(bc+d$|ef*g.|h?i(j|k))", "BCDD"),                      "-nomatch-");
//assert_eq!(rep("(?i)(bc+d$|ef*g.|h?i(j|k))", "REFFGZ"),                    "EFFGZ-EFFGZ-None");
//assert_eq!(rep("(?i)((((((((((a))))))))))",  "A"),                         "A");
//assert_eq!(rep("(?i)((((((((((a))))))))))\\10","AA"),                        "AA");
//assert_eq!(rep("(?i)((((((((((a))))))))))\\41","AA"),                        "-nomatch-");
//assert_eq!(rep("(?i)((((((((((a))))))))))\\41","A!"),                        "A!");
//assert_eq!(rep("(?i)(((((((((a)))))))))",    "A"),                         "A");
//assert_eq!(rep("(?i)(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))","A"),                         "A");
//assert_eq!(rep("(?i)(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))","C"),                         "C");
//assert_eq!(rep("(?i)multiple words of text", "UH-UH"),                     "-nomatch-");
//assert_eq!(rep("(?i)multiple words",         "MULTIPLE WORDS, YEAH"),      "MULTIPLE WORDS");
//assert_eq!(rep("(?i)(.*)c(.*)",              "ABCDE"),                     "ABCDE-AB-DE");
//assert_eq!(rep("(?i)\\((.*), (.*)\\)",       "(A, B)"),                    "B-A");
//assert_eq!(rep("(?i)[k]",                    "AB"),                        "-nomatch-");
//assert_eq!(rep("(?i)abcd",                   "ABCD"),                      "ABCD-$&-\\ABCD");
//assert_eq!(rep("(?i)a(bc)d",                 "ABCD"),                      "BC-$1-\\BC");
//assert_eq!(rep("(?i)a[-]?c",                 "AC"),                        "AC");
//assert_eq!(rep("(?i)(abc)\\1",               "ABCABC"),                    "ABC");
//assert_eq!(rep("(?i)([a-c]*)\\1",            "ABCABC"),                    "ABC");
//assert_eq!(rep("a(?!b).",                    "abad"),                      "ad");
//assert_eq!(rep("a(?=d).",                    "abad"),                      "ad");
//assert_eq!(rep("a(?=c|d).",                  "abad"),                      "ad");
//assert_eq!(rep("a(?:b|c|d)(.)",              "ace"),                       "e");
//assert_eq!(rep("a(?:b|c|d)*(.)",             "ace"),                       "e");
//assert_eq!(rep("a(?:b|c|d)+?(.)",            "ace"),                       "e");
//assert_eq!(rep("a(?:b|(c|e){1,2}?|d)+?(.)",  "ace"),                       "ce");
//assert_eq!(rep("^(.+)?B",                    "AB"),                        "A");
//assert_eq!(rep("w(?# comment",               "w"),                         "-nomatch-");
//assert_eq!(rep("w(?# comment 1)xy(?# comment 2)z","wxyz"),                      "wxyz");
//assert_eq!(rep("w(?i)",                      "W"),                         "W");
//assert_eq!(rep("w(?i)",                      "W"),                         "-nomatch-");
//assert_eq!(rep("a.b",                        "a\nb"),                      "-nomatch-");
//assert_eq!(rep("(?s)a.b",                    "a\nb"),                      "a\nb");
//assert_eq!(rep("\\w+",                       "--ab_cd0123--"),             "ab_cd0123");
//assert_eq!(rep("[\\w]+",                     "--ab_cd0123--"),             "ab_cd0123");
//assert_eq!(rep("\\D+",                       "1234abc5678"),               "abc");
//assert_eq!(rep("[\\D]+",                     "1234abc5678"),               "abc");
//assert_eq!(rep("[\\da-fA-F]+",               "123abc"),                    "123abc");
//assert_eq!(rep("[\\d-x]",                    "-"),                         "-nomatch-");
//assert_eq!(rep(r"([\s]*)([\S]*)([\s]*)",     " testing!1972"),             "testing!1972 ");
//assert_eq!(rep(r"(\s*)(\S*)(\s*)",           " testing!1972"),             "testing!1972 ");
////assert_eq!(rep(r"\x00ff",                    "\377"),                      "-nomatch-");
////assert_eq!(rep(r"\t\n\v\r\f\a\g",            "\t\n\v\r\f\ag"),             "\t\n\v\r\f\ag");
////assert_eq!(rep("\t\n\v\r\f\a\g",             "\t\n\v\r\f\ag"),             "\t\n\v\r\f\ag");
////assert_eq!(rep(r"[\t][\n][\v][\r][\f][\b]",  "\t\n\v\r\f\b"),              "\t\n\v\r\f\b");
//assert_eq!(rep(r"(([a-z]+):)?([a-z]+)$",     "smil"),                      "None-None-smil");
//assert_eq!(rep(r".*d",                       "abc\nabd"),                  "abd");
//assert_eq!(rep(r"[\41]",                     "!"),                         "!");
//assert_eq!(rep(r"(x?)?",                     "x"),                         "x");
//assert_eq!(rep(r" (?x)foo ",                 "foo"),                       "foo");
//assert_eq!(rep(r"(?<!abc)(d.f)",             "abcdefdof"),                 "dof");
//assert_eq!(rep(r"[\w-]+",                    "laser_beam"),                "laser_beam");
//assert_eq!(rep(r".*?\S *:",                  "xx:"),                       "xx:");
//assert_eq!(rep(r"a[ ]*?\ (\d+).*",           "a   10"),                    "a   10");
//assert_eq!(rep(r"a[ ]*?\ (\d+).*",           "a    10"),                   "a    10");
//assert_eq!(rep(r"(?ms).*?x\s*\Z(.*)",        "xx\nx\n"),                   "");
//assert_eq!(rep(r"(?i)M+",                    "MMM"),                       "MMM");
//assert_eq!(rep(r"(?i)m+",                    "MMM"),                       "MMM");
//assert_eq!(rep(r"(?i)[M]+",                  "MMM"),                       "MMM");
//assert_eq!(rep(r"(?i)[m]+",                  "MMM"),                       "MMM");
////assert_eq!(rep(r""(?:\\"|[^"])*?"",          r""\"""),                     r""\""");
//assert_eq!(rep(r"^.*?$",                     "one\ntwo\nthree\n"),         "-nomatch-");
//assert_eq!(rep(r"a[^>]*?b",                  "a>b"),                       "-nomatch-");
//assert_eq!(rep(r"^a*?$",                     "foo"),                       "-nomatch-");
//assert_eq!(rep(r"^((a)c)?(ab)$",             "ab"),                        "None-None-ab");
//assert_eq!(rep("^([ab]*?)(?=(b)?)c",         "abc"),                       "ab-None");
//assert_eq!(rep("^([ab]*?)(?!(b))c",          "abc"),                       "ab-None");
//assert_eq!(rep("^([ab]*?)(?<!(a))c",         "abc"),                       "ab-None");
//assert_eq!(rep(r"\b.\b",                     "a"),                         "a");
    }

    #[test]
    fn check_pattern_capture() {
        assert_eq!(pat("(^ab|cd)e",                    "cde"),        "cde-cd");
        assert_eq!(pat("$+(^AA|BB|XX)",                "AABBBXX"),     "AABB-AA/BB");
        assert_eq!(pat("$+(^A|B|X)",                   "AABBBXX"),     "AABBBXX-A/A/B/B/B/X/X");
        assert_eq!(pat("(^$+A|$+B|$+X)",               "AABBBXX"),     "AA-AA");
        assert_eq!(pat("(^$+A)(^$+B)$+(^X)$$",         "AABBBXX"),     "");
        assert_eq!(pat("(^$+A)(^$?L)(^$+B)$+(^X)$$",   "AABBBXX"),     "");
    }
}
