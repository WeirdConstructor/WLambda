/*!
Implements a data querying algorithm that is used
by the WLambda `$S(...)` syntax and by WLambda `std:selector`. It's a recursive
searching, matching and capturing algorithm.
*/

use crate::vval::{Env, StackAction, VVal, VValFun};

use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};
use crate::parser::{parse_str_backslash, EscSeqValue};

use std::cell::RefCell;
use std::rc::Rc;

use crate::str_int::s2sym;

fn is_ident_pattern_char(c: char) -> bool {
    match c {
        '?' | '|' | '[' | ']' | '(' | ')' | '$' | '*' => false,
        _ => !c.is_whitespace(),
    }
}

fn is_ident_selector_char(c: char) -> bool {
    match c {
        '!' | '?' | '/' | '\\' | '|' | '{' | '}' | '[' | ']' | '(' | ')' | '\'' | '^' | '&'
        | '$' | ':' | ';' | '*' | '=' | ',' => false,
        _ => !c.is_whitespace(),
    }
}

fn parse_ident_char(ps: &mut State) -> Result<Option<char>, ParseError> {
    if let Some(c) = ps.peek() {
        match c {
            '\\' => {
                ps.consume();
                let c = match parse_str_backslash(ps)? {
                    EscSeqValue::Char(c) => c,
                    EscSeqValue::Byte(b) => b as char,
                };
                ps.skip_ws();
                Ok(Some(c))
            }
            c if ps.is_pattern_ident_mode() && is_ident_pattern_char(c) => {
                ps.consume_ws();
                Ok(Some(c))
            }
            c if !ps.is_pattern_ident_mode() && is_ident_selector_char(c) => {
                ps.consume_ws();
                Ok(Some(c))
            }
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

    if uh.is_empty() {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(
            ps.peek().unwrap_or(' '),
            "identifier character",
        )));
    }

    let r = VVal::pair(VVal::new_sym("I"), VVal::new_sym(&uh));

    Ok(r)
}

fn parse_glob_cclass(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq('[') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('[', "char class start")));
    }

    let neg = ps.consume_if_eq('^');

    let mut chars = String::new();

    let mut c = ps.expect_some(ps.peek())?;
    let mut last: Option<char> = None;
    let mut find_range_end = false;

    while c != ']' {
        ps.consume();

        if last.is_some() && c == '-' {
            find_range_end = true;
            c = ps.expect_some(ps.peek())?;
            continue;
        }

        c = if c == '\\' {
            match parse_str_backslash(ps)? {
                EscSeqValue::Char(c) => c,
                EscSeqValue::Byte(b) => b as char,
            }
        } else {
            c
        };

        if find_range_end {
            let start = last.unwrap() as u32 + 1;
            let end = c as u32;
            for c_idx in start..end {
                if let Ok(c) = char::try_from(c_idx) {
                    chars.push(c);
                }
            }
            chars.push(c);

            last = None;
            find_range_end = false;
        } else {
            chars.push(c);
            last = Some(c);
        }

        c = ps.expect_some(ps.peek())?;
    }

    if find_range_end {
        chars.push('-');
    }

    if !ps.consume_if_eq_ws(']') {
        return Err(ps.err(ParseErrorKind::ExpectedToken(']', "char class end")));
    }

    Ok(VVal::pair(VVal::new_sym(if neg { "NCCls" } else { "CCls" }), VVal::new_str_mv(chars)))
}

fn parse_pat_regex(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("N0"), parse_rx_atom(ps)?))
        }
        '+' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("N1"), parse_rx_atom(ps)?))
        }
        '<' => {
            ps.consume_ws();
            match ps.expect_some(ps.peek())? {
                '*' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("N0-"), parse_rx_atom(ps)?))
                }
                '+' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("N1-"), parse_rx_atom(ps)?))
                }
                '?' => {
                    ps.consume_ws();
                    Ok(VVal::pair(VVal::new_sym("Opt-"), parse_rx_atom(ps)?))
                }
                c => Err(ps.err(ParseErrorKind::UnexpectedToken(c, "non-greedy regex pattern"))),
            }
        }
        '?' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("Opt"), parse_rx_atom(ps)?))
        }
        '!' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("ZwNegLA"), parse_rx_atom(ps)?))
        }
        '=' => {
            ps.consume_ws();
            Ok(VVal::pair(VVal::new_sym("ZwLA"), parse_rx_atom(ps)?))
        }
        '^' => {
            ps.consume_ws();
            Ok(VVal::new_sym("Start"))
        }
        '$' => {
            ps.consume_ws();
            Ok(VVal::new_sym("End"))
        }
        '&' => {
            ps.consume_ws();
            match ps.expect_some(ps.peek())? {
                'L' => {
                    ps.consume_ws();
                    Ok(VVal::new_sym("ToLowercase"))
                }
                'U' => {
                    ps.consume_ws();
                    Ok(VVal::new_sym("ToUppercase"))
                }
                c => Err(ps.err(ParseErrorKind::UnexpectedToken(c, "match modifier"))),
            }
        }
        's' => {
            ps.consume_ws();
            Ok(VVal::new_sym("WsChar"))
        }
        'S' => {
            ps.consume_ws();
            Ok(VVal::new_sym("NWsChar"))
        }
        c => Err(ps.err(ParseErrorKind::UnexpectedToken(c, "regex pattern"))),
    }
}

fn parse_glob_group(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('(') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('(', "sub pattern start")));
    }

    let capture = ps.consume_if_eq_ws('^');
    let p = parse_pattern(ps)?;

    if !ps.consume_if_eq_ws(')') {
        return Err(ps.err(ParseErrorKind::ExpectedToken(')', "sub pattern end")));
    }

    if capture {
        Ok(VVal::pair(VVal::new_sym("PatCap"), p))
    } else {
        Ok(VVal::pair(VVal::new_sym("PatSub"), p))
    }
}

fn parse_pat_glob(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' => {
            ps.consume_ws();
            Ok(VVal::new_sym("Glob"))
        }
        '?' => {
            ps.consume_ws();
            Ok(VVal::new_sym("Any"))
        }
        '$' => {
            ps.consume_ws();
            parse_pat_regex(ps)
        }
        '[' => parse_glob_cclass(ps),
        '(' => parse_glob_group(ps),
        c => Err(ps.err(ParseErrorKind::UnexpectedToken(c, "in glob pattern"))),
    }
}

fn parse_rx_atom(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' | '?' | '[' | '(' | '$' => parse_pat_glob(ps),
        _ => parse_ident(ps, true),
    }
}

fn parse_glob_atom(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '*' | '?' | '[' | '(' | '$' => parse_pat_glob(ps),
        _ => parse_ident(ps, false),
    }
}

fn parse_pat_branch(ps: &mut State) -> Result<VVal, ParseError> {
    let pat_branch = VVal::vec();

    while !ps.at_end()
        && ((ps.is_pattern_ident_mode() && !ps.lookahead_one_of("|)]"))
            || (!ps.is_pattern_ident_mode() && !ps.lookahead_one_of("|:&=)]}/,")))
    {
        //d// println!("GO {}", ps.rest().to_string());
        pat_branch.push(parse_glob_atom(ps)?);
    }

    Ok(pat_branch)
}

/// Parses a regex pattern from a parser State and returns
/// the VVal data structure describing the parsed pattern.
pub fn parse_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let mut pat = parse_pat_branch(ps)?;

    if ps.consume_if_eq_ws('|') {
        let pat_alt = parse_pattern(ps)?;
        pat = VVal::vec3(VVal::new_sym("Alt"), pat, pat_alt);
    }

    Ok(pat)
}

fn parse_index(ps: &mut State) -> Result<VVal, ParseError> {
    let uh = ps.take_while(|c| c.is_digit(10));

    if let Ok(cn) = uh.to_string().parse::<i64>() {
        ps.skip_ws();
        Ok(VVal::Int(cn as i64))
    } else {
        Err(ps.err(ParseErrorKind::BadEscape("Bad number as index")))
    }
}

fn parse_key(ps: &mut State) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parse_index(ps),
        _ => parse_pattern(ps),
    }
}

fn parse_kv(ps: &mut State) -> Result<VVal, ParseError> {
    let key = parse_key(ps)?;

    if !ps.consume_if_eq_ws('=') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('=', "key/value pattern")));
    }

    let val = parse_pattern(ps)?;

    Ok(VVal::vec2(key, val))
}

fn parse_kv_item(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws('{') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('{', "key/value node pattern start")));
    }

    let kv = parse_kv(ps)?;

    let v = VVal::vec2(VVal::new_sym("KV"), kv);

    while ps.expect_some(ps.peek())? == ',' {
        ps.consume_ws();
        let kv = parse_kv(ps)?;
        v.push(kv);
    }

    if !ps.consume_if_eq_ws('}') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('}', "in key/value node pattern end")));
    }

    Ok(v)
}

fn parse_node_match(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_ws(':') {
        return Err(ps.err(ParseErrorKind::ExpectedToken(':', "start of node match")));
    }

    let negated = ps.consume_if_eq_ws('!');

    let mut ret = match ps.expect_some(ps.peek())? {
        '(' => {
            ps.consume_ws();
            let ret = VVal::vec2(VVal::new_sym("LA"), parse_selector_pattern(ps)?);

            if !ps.consume_if_eq_ws(')') {
                return Err(
                    ps.err(ParseErrorKind::ExpectedToken(')', "at end of look-ahead selector"))
                );
            }

            ret
        }
        's' if ps.consume_lookahead("str") => {
            ps.skip_ws();
            if !ps.consume_if_eq_ws('=') {
                return Err(ps.err(ParseErrorKind::ExpectedToken('=', "str node match")));
            }

            VVal::vec2(VVal::new_sym("Str"), parse_pattern(ps)?)
        }
        't' if ps.consume_lookahead("type") => {
            ps.skip_ws();
            if !ps.consume_if_eq_ws('=') {
                return Err(ps.err(ParseErrorKind::ExpectedToken('=', "type node match")));
            }

            VVal::vec2(VVal::new_sym("Type"), parse_pattern(ps)?)
        }
        '{' => parse_kv_item(ps)?,
        c => {
            return Err(ps.err(ParseErrorKind::UnexpectedToken(c, "node match")));
        }
    };

    if negated {
        ret = VVal::vec2(VVal::new_sym("Not"), ret);
    }

    Ok(ret)
}

fn parse_node_cond(ps: &mut State) -> Result<VVal, ParseError> {
    let nm = parse_node_match(ps)?;

    match ps.peek().unwrap_or('\0') {
        '&' => {
            ps.consume_ws();

            let v = VVal::vec();
            v.push(VVal::new_sym("And"));
            v.push(nm);
            v.push(parse_node_cond(ps)?);
            Ok(v)
        }
        '|' => {
            ps.consume_ws();

            let v = VVal::vec();
            v.push(VVal::new_sym("Or"));
            v.push(nm);
            v.push(parse_node_cond(ps)?);
            Ok(v)
        }
        _ => Ok(nm),
    }
}

fn parse_rec_cond(ps: &mut State) -> Result<VVal, ParseError> {
    if ps.consume_lookahead("key") {
        ps.skip_ws();
        if ps.consume_if_eq_ws('=') {
            ps.skip_ws();
            Ok(VVal::vec2(VVal::new_sym("NotKey"), parse_pattern(ps)?))
        } else {
            Err(ps.err(ParseErrorKind::ExpectedToken('=', "`key =` in recursion condition")))
        }
    } else {
        Err(ps.err(ParseErrorKind::UnexpectedToken(ps.peek().unwrap(), "recursion condition")))
    }
}

fn parse_node(ps: &mut State) -> Result<VVal, ParseError> {
    let c = ps.expect_some(ps.peek())?;

    match c {
        '*' if ps.consume_lookahead("**") => {
            ps.skip_ws();

            let rec_cond = if ps.peek().unwrap_or('\0') == '!' {
                ps.consume_ws();
                parse_rec_cond(ps)?
            } else {
                VVal::None
            };

            let recval_cond = if ps.peek().unwrap_or('\0') == '=' {
                ps.consume_ws();
                parse_node_cond(ps)?
            } else {
                VVal::None
            };

            let rg = if ps.peek().unwrap_or('\0') == ':' {
                VVal::vec3(VVal::new_sym("RecGlob"), rec_cond, parse_node_cond(ps)?)
            } else {
                VVal::vec3(VVal::new_sym("RecGlob"), rec_cond, VVal::None)
            };

            rg.push(recval_cond);

            Ok(rg)
        }
        '^' => {
            ps.consume_ws();
            Ok(VVal::vec2(VVal::new_sym("NCap"), parse_node(ps)?))
        }
        _ => {
            let key = parse_key(ps)?;
            if ps.peek().unwrap_or('\0') == ':' {
                Ok(VVal::vec3(VVal::new_sym("NK"), key, parse_node_cond(ps)?))
            } else {
                Ok(VVal::vec2(VVal::new_sym("NK"), key))
            }
        }
    }
}

fn parse_selector_pattern(ps: &mut State) -> Result<VVal, ParseError> {
    let selector = VVal::vec1(VVal::new_sym("Path"));

    let node = parse_node(ps)?;
    selector.push(node);

    while ps.consume_if_eq_ws('/') {
        //d// println!("PARSE NODE: {}", ps.rest().to_string());
        let node = parse_node(ps)?;
        selector.push(node);
    }

    Ok(selector)
}

fn parse_selector(s: &str) -> Result<VVal, ParseError> {
    let mut ps = State::new(s, "<selector>");
    ps.skip_ws();

    let ret = parse_selector_pattern(&mut ps)?;

    ps.skip_ws();

    if !ps.at_end() {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(ps.peek().unwrap(), "end of selector")));
    }

    Ok(ret)
}

/// State for evaluating patterns and selectors.
#[derive(Debug, Clone)]
pub(crate) struct SelectorState {
    orig_string_len: usize,
    captures: Vec<(usize, usize)>,
    selector_captures: Vec<VVal>,
}

impl SelectorState {
    fn new() -> Self {
        Self { orig_string_len: 0, captures: Vec::new(), selector_captures: Vec::new() }
    }

    fn push_sel_capture(&mut self, v: VVal) {
        self.selector_captures.push(v);
    }

    fn has_captures(&self) -> bool {
        !self.selector_captures.is_empty()
    }

    fn get_sel_captures(&self) -> VVal {
        VVal::vec_from(&self.selector_captures[..])
    }

    fn pop_sel_caputure(&mut self) {
        self.selector_captures.pop();
    }

    fn push_capture_start(&mut self, s: &RxBuf) -> usize {
        //d// println!("###### PUSH CAP START [{}] => ({}, 0) ({:?})", s, s.offs, self.captures);
        self.captures.push((s.offs, 0));
        self.captures.len() - 1
    }

    fn set_capture_end(&mut self, idx: usize, s: &RxBuf) -> (usize, (usize, usize)) {
        //d// println!("###### SET CAP END [{}] => ({:?})", s, self.captures);
        let offs = self.captures[idx].0;
        self.captures[idx].1 = s.offs - offs;
        (idx, self.captures[idx])
    }

    fn pop_capture(&mut self) {
        //d// println!("###### POP CAP ({:?})", self.captures);
        self.captures.pop();
    }

    fn set_str(&mut self, s: &str) -> usize {
        std::mem::replace(&mut self.orig_string_len, s.len())
    }

    fn restore_str(&mut self, os_len: usize) {
        self.orig_string_len = os_len;
    }
}

/// A parse buffer, storing the current offset into the input string.
#[derive(Debug, Clone, Copy)]
pub(crate) struct RxBuf<'a> {
    s: &'a str,
    offs: usize,
    orig_len: usize,
}

impl std::fmt::Display for RxBuf<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}[{},{}]", self.s, self.offs, self.orig_len)
    }
}

impl<'a> RxBuf<'a> {
    fn new(s: &'a str) -> Self {
        Self { s, offs: 0, orig_len: s.len() }
    }

    fn is_at_start(&self) -> bool {
        self.offs == 0
    }

    fn is_at_end(&self) -> bool {
        self.offs == self.orig_len
    }

    fn offs(&self, offs: usize) -> Self {
        Self { s: &self.s[offs..], offs: self.offs + offs, orig_len: self.orig_len }
    }
}

/// Stores the position of a captured part of the input string of a pattern.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(crate) struct CaptureNode {
    idx: usize,
    len: usize,
    next: Option<Box<CaptureNode>>,
}

impl CaptureNode {
    // fn add_capture_to(&self, idx: usize, res: &mut PatResult) {
    //     if let Some(n) = &self.next {
    //         n.add_capture_to(idx, res);
    //     }
    //
    //     res.add_capture((idx, (self.idx, self.len)));
    // }

    fn to_string(&self, input: &str) -> String {
        input[self.idx..(self.idx + self.len)].to_string()
    }

    #[allow(dead_code)]
    fn to_test_string(&self, input: &str) -> String {
        if let Some(n) = &self.next {
            input[self.idx..(self.idx + self.len)].to_string() + "/" + &n.to_test_string(input)
        } else {
            input[self.idx..(self.idx + self.len)].to_string()
        }
    }
}

fn append_capture(cap_idx: usize, v: &mut Vec<Option<Box<CaptureNode>>>, cap: &(usize, usize)) {
    let pos = v.get_mut(cap_idx).unwrap();
    let tail = pos.take();
    *pos = Some(Box::new(CaptureNode { idx: cap.0, len: cap.1, next: tail }));
}

/// Stores the result of a pattern match, including the captured parts of the
/// input string.
#[derive(Debug, Clone)]
pub(crate) struct PatResult {
    matched: bool,
    match_len: usize,
    offs: usize,
    captures: Option<Vec<Option<Box<CaptureNode>>>>,
}

impl PatResult {
    fn matched() -> Self {
        Self { matched: true, match_len: 0, offs: 0, captures: None }
    }

    pub fn len(mut self, l: usize) -> Self {
        self.match_len += l;
        self
    }

    pub fn b(&self) -> bool {
        self.matched
    }

    // fn merge_captures(mut self, res: &PatResult) -> Self {
    //     if let Some(c) = &res.captures {
    //         for (idx, cap) in c.iter().enumerate() {
    //             if let Some(cap) = cap {
    //                 cap.add_capture_to(idx, &mut self);
    //             }
    //         }
    //     }
    //     self
    // }

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
        Self { matched: false, match_len: 0, offs: 0, captures: None }
    }

    pub fn pos(&self) -> Option<(usize, usize)> {
        if !self.matched {
            return None;
        }

        Some((self.offs, self.match_len))
    }

    pub fn to_vval(&self, input: &str) -> VVal {
        if !self.matched {
            return VVal::None;
        }

        let v = VVal::vec();
        v.push(VVal::new_str_mv(input[self.offs..(self.offs + self.match_len)].to_string()));

        if let Some(c) = &self.captures {
            for cap in c.iter() {
                if let Some(cap) = cap {
                    v.push(VVal::new_str_mv(cap.to_string(input)));
                } else {
                    v.push(VVal::None);
                }
            }
        }

        v
    }

    #[allow(dead_code)]
    pub fn to_test_string(&self, input: &str) -> String {
        if !self.matched {
            return "-nomatch-".to_string();
        }

        //d// println!("TOTEST {:?} [{}]", self, input);

        let mut s = input[self.offs..(self.offs + self.match_len)].to_string();

        if let Some(c) = &self.captures {
            for cap in c.iter() {
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

/// A function type for the evaluation node of a regex pattern.
pub(crate) type PatternNode = Box<dyn Fn(RxBuf, &mut SelectorState) -> PatResult>;
/// A function type for the evaluation node of a data structure selector.
pub(crate) type SelNode = Box<dyn Fn(&VVal, &mut SelectorState, &VVal) -> bool>;

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
    };
}

fn compile_atom(p: &VVal, next: PatternNode) -> PatternNode {
    //d// println!("COMPILE ATOM {}", p.s());

    if p.is_pair() {
        let pair_type = p.at(0).unwrap().to_sym();
        let pair_val = p.at(1).unwrap_or(VVal::None);

        if pair_type == s2sym("I") {
            let key_str = pair_val.clone();

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                key_str.with_s_ref(|y| {
                    //d// println!("I: [{}]<=>[{}]", s, y);
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
                    if let Some(c) = s.s.chars().next() {
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
                    if let Some(c) = s.s.chars().next() {
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
            let cap_idx = Rc::new(RefCell::new(0));
            let cap_idx_b = cap_idx.clone();

            let sub = compile_pattern(
                &pair_val,
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    let cap = st.set_capture_end(*cap_idx.borrow(), &s);
                    (*next)(s, st).capture(cap)
                }),
            );

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                (*cap_idx_b.borrow_mut()) = st.push_capture_start(&s);
                let res = (*sub)(s, st);
                st.pop_capture();
                res
            })
        } else if pair_type == s2sym("ZwNegLA") {
            let sub_pat = compile_atom(
                &pair_val,
                Box::new(move |_s: RxBuf, _st: &mut SelectorState| PatResult::matched()),
            );

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let res = (*sub_pat)(s, st);
                if res.b() {
                    return PatResult::fail();
                }

                (*next)(s, st)
            })
        } else if pair_type == s2sym("ZwLA") {
            let sub_pat = compile_atom(
                &pair_val,
                Box::new(move |_s: RxBuf, _st: &mut SelectorState| PatResult::matched()),
            );

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let res = (*sub_pat)(s, st);
                if !res.b() {
                    return PatResult::fail();
                }

                (*next)(s, st)
            })
        } else if pair_type == s2sym("Opt") || pair_type == s2sym("Opt-") {
            let greedy = pair_type == s2sym("Opt");

            let next: Rc<PatternNode> = Rc::from(next);
            let next_opt: Rc<PatternNode> = next.clone();

            let opt_pat = compile_atom(
                &pair_val,
                Box::new(move |s: RxBuf, st: &mut SelectorState| (*next)(s, st)),
            );

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
            let next: Rc<PatternNode> = Rc::from(next);
            let next_n0: Rc<PatternNode> = next.clone();

            let sub_match_offs = Rc::new(RefCell::new(None));
            let sub_match_offs_n = sub_match_offs.clone();
            //d// let sub_pat_str      = pair_val.s();
            //d// let sub_pat_str2     = sub_pat_str.clone();

            let sub_pat = compile_atom(
                &pair_val,
                Box::new(move |s: RxBuf, st: &mut SelectorState| {
                    (*sub_match_offs.borrow_mut()) = Some(s.offs);
                    (*next)(s, st)
                    //d// println!("Nx PATTERN SUB(offs {})/NEXT RET({:?}) [{}] {:?} (for {})",
                    //d//         s.offs, r, s, st.captures, sub_pat_str2);
                }),
            );

            let n0 = pair_type == s2sym("N0") || pair_type == s2sym("N0-");
            let greedy = pair_type == s2sym("N1") || pair_type == s2sym("N0");

            Box::new(move |s: RxBuf, st: &mut SelectorState| {
                let mut res = if n0 {
                    let res = (*next_n0)(s, st);
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
                        res = next_res.len(match_offs);
                        if !greedy {
                            break;
                        }
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

                //d// println!("EXIT n1({})", sub_pat_str);
                res
            })
        } else {
            panic!("Unknown pair atom: {}", p.s());
        }
    } else if p.to_sym() == s2sym("WsChar") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().next() {
                let c_len = c.len_utf8();
                if c.is_whitespace() {
                    return (*next)(s.offs(c_len), st).len(c_len);
                }
            }

            PatResult::fail()
        })
    } else if p.to_sym() == s2sym("NWsChar") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().next() {
                let c_len = c.len_utf8();

                if !c.is_whitespace() {
                    return (*next)(s.offs(c_len), st).len(c_len);
                }
            }

            PatResult::fail()
        })
    } else if p.to_sym() == s2sym("Any") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            if let Some(c) = s.s.chars().next() {
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
        Box::new(
            move |s: RxBuf, st: &mut SelectorState| {
                if s.is_at_start() {
                    (*next)(s, st)
                } else {
                    PatResult::fail()
                }
            },
        )
    } else if p.to_sym() == s2sym("End") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            //d// println!("MATCH END: [{}] atend={} [CAP {:?}]", s, s.is_at_end(), st.captures);
            if s.is_at_end() {
                (*next)(s, st)
            } else {
                PatResult::fail()
            }
        })
    } else if p.to_sym() == s2sym("ToLowercase") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            let s_lower = s.s.to_lowercase();

            //d// println!("s={}[{}] s_l={}[{}]", s.s.len(), s, s_lower.len(), s_lower);

            let rx = RxBuf { s: &s_lower[..], offs: s.offs, orig_len: s.orig_len };
            (*next)(rx, st)
        })
    } else if p.to_sym() == s2sym("ToUppercase") {
        Box::new(move |s: RxBuf, st: &mut SelectorState| {
            let s_upper = s.s.to_uppercase();
            let rx = RxBuf { s: &s_upper[..], offs: s.offs, orig_len: s.orig_len };
            (*next)(rx, st)
        })
    } else if p.is_vec() {
        if p.is_empty() {
            Box::new(move |_s: RxBuf, _st: &mut SelectorState| PatResult::matched())
        } else {
            panic!("UNKNOWN ATOM: {}", p.s());
        }
    } else {
        panic!("UNKNOWN ATOM: {}", p.s());
    }
}

fn compile_pattern_branch(pat: &VVal, next: PatternNode) -> PatternNode {
    //d// println!("COMPILE PATTERN BRANCH [{}]", pat.s());

    let mut next: Option<PatternNode> = Some(next);

    for i in 0..pat.len() {
        let p = pat.at(pat.len() - (i + 1)).expect("pattern item");
        //d// println!("PAT COMP: {}", p.s());

        let my_next = next.take();
        next = Some(compile_atom(&p, my_next.unwrap()));
    }

    next.unwrap()
}

fn compile_pattern(pat: &VVal, next: PatternNode) -> PatternNode {
    //d// println!("COMPILE PATTERN [{}]", pat.s());

    let first = pat.at(0).unwrap_or(VVal::None);
    if first.is_sym() && first.to_sym() == s2sym("Alt") {
        let next_a: Rc<PatternNode> = Rc::from(next);
        let next_b: Rc<PatternNode> = next_a.clone();

        let branch_a = compile_pattern_branch(
            &pat.at(1).expect("left hand side alt branch"),
            Box::new(move |s: RxBuf, st: &mut SelectorState| (*next_a)(s, st)),
        );

        let branch_b = compile_pattern(
            &pat.at(2).expect("right hand side alt branch"),
            Box::new(move |s: RxBuf, st: &mut SelectorState| (*next_b)(s, st)),
        );

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

fn match_pattern(pat: &PatternNode, s: &str, st: &mut SelectorState) -> bool {
    let old_str = st.set_str(s);

    let rb = RxBuf::new(s);
    let res = (*pat)(rb, st);

    st.restore_str(old_str);

    res.b() && res.match_len == s.len()
}

fn compile_key(k: &VVal, sn: SelNode) -> SelNode {
    if k.is_int() {
        let i = k.i();

        Box::new(
            move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                if let Some(v) = v.at(i as usize) {
                    (*sn)(&v, st, capts)
                } else {
                    false
                }
            },
        )
    } else {
        let pat = k.at(0).unwrap_or(VVal::None);
        let pat_type = k.at(0).unwrap_or(VVal::None).at(0).unwrap_or(VVal::None);

        if k.len() == 1 && pat.is_pair() && pat_type.to_sym() == s2sym("I") {
            let key = pat.at(1).unwrap_or(VVal::None).to_sym();

            return Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                if let Some(v) = v.get_key_sym(&key) {
                    (*sn)(&v, st, capts)
                } else {
                    false
                }
            });
        } else if k.len() == 1 && pat.to_sym() == s2sym("Glob") {
            return Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                if !v.iter_over_vvals() {
                    return false;
                }

                let mut found = false;
                for (v, _) in v.iter() {
                    if (*sn)(&v, st, capts) {
                        found = true;
                    }
                }

                found
            });
        }

        let pat = compile_pattern(
            k,
            Box::new(move |_s: RxBuf, _st: &mut SelectorState| {
                //d// println!("*** BRANCH LEAF PATTERN MATCH {}| {:?}", s, st.captures);
                PatResult::matched()
            }),
        );

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            if !v.iter_over_vvals() {
                return false;
            }

            let mut found = false;

            for (i, (v, k)) in v.iter().enumerate() {
                if let Some(k) = k {
                    k.with_s_ref(|s| {
                        if match_pattern(&pat, s, st) && (*sn)(&v, st, capts) {
                            found = true;
                        }
                    });
                } else {
                    let idx_str = format!("{}", i);

                    if match_pattern(&pat, &idx_str[..], st) && (*sn)(&v, st, capts) {
                        found = true;
                    }
                }
            }

            found
        })
    }
}

fn compile_kv(kv: &VVal) -> SelNode {
    let pat = compile_pattern(
        &kv.at(1).expect("pattern in kv"),
        Box::new(move |_s: RxBuf, _st: &mut SelectorState| {
            //d// println!("*** BRANCH LEAF PATTERN MATCH {}| {:?}", s, st.captures);
            PatResult::matched()
        }),
    );

    compile_key(
        &kv.at(0).expect("key in kv"),
        Box::new(move |v: &VVal, st: &mut SelectorState, _capts: &VVal| {
            v.with_s_ref(|s| match_pattern(&pat, s, st))
        }),
    )
}

fn compile_kv_match(kvm: &VVal) -> SelNode {
    let mut kv_conds = vec![];
    for i in 1..kvm.len() {
        kv_conds.push(compile_kv(&kvm.at(i).unwrap()));
    }

    Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
        for kv in kv_conds.iter() {
            if !(*kv)(v, st, capts) {
                return false;
            }
        }

        true
    })
}

fn compile_node_cond(n: &VVal) -> SelNode {
    let node_type = n.at(0).expect("proper node condition").to_sym();

    if node_type == s2sym("KV") {
        compile_kv_match(n)
    } else if node_type == s2sym("LA") {
        let subsel = compile_selector(&n.at(1).expect("sub selector"), true);
        Box::new(move |v: &VVal, st: &mut SelectorState, _capts: &VVal| {
            let nocaps = VVal::None;
            (*subsel)(v, st, &nocaps)
        })
    } else if node_type == s2sym("And") {
        let a = compile_node_cond(&n.at(1).expect("node condition a"));
        let b = compile_node_cond(&n.at(2).expect("node condition b"));
        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            (*a)(v, st, capts) && (*b)(v, st, capts)
        })
    } else if node_type == s2sym("Or") {
        let a = compile_node_cond(&n.at(1).expect("node condition a"));
        let b = compile_node_cond(&n.at(2).expect("node condition b"));
        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            (*a)(v, st, capts) || (*b)(v, st, capts)
        })
    } else if node_type == s2sym("Not") {
        let nc = compile_node_cond(&n.at(1).expect("node condition"));
        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| !(*nc)(v, st, capts))
    } else if node_type == s2sym("Type") {
        let pat = compile_pattern(
            &n.at(1).expect("node type pattern"),
            Box::new(move |_s: RxBuf, _st: &mut SelectorState| {
                //d// println!("*** BRANCH LEAF TYPE PATTERN MATCH {}| {:?}", s, st.captures);
                PatResult::matched()
            }),
        );
        Box::new(move |v: &VVal, st: &mut SelectorState, _capts: &VVal| {
            match_pattern(&pat, v.type_name(), st)
        })
    } else if node_type == s2sym("Str") {
        let pat = compile_pattern(
            &n.at(1).expect("node type pattern"),
            Box::new(move |_s: RxBuf, _st: &mut SelectorState| {
                //d// println!("*** BRANCH LEAF TYPE PATTERN MATCH {}| {:?}", s, st.captures);
                PatResult::matched()
            }),
        );
        Box::new(move |v: &VVal, st: &mut SelectorState, _capts: &VVal| {
            v.with_s_ref(|s| match_pattern(&pat, s, st))
        })
    } else {
        panic!("Unsupported node cond: {}", node_type);
    }
}

fn compile_node(n: &VVal, sn: SelNode) -> SelNode {
    let node_type = n.at(0).expect("proper node").to_sym();

    let sn = if let Some(node_cond) = n.at(2) {
        // Fixed 2, for RecGlob and NK
        if node_cond.is_none() {
            sn
        } else {
            let cond = compile_node_cond(&node_cond);
            Box::new(
                move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                    if (*cond)(v, st, capts) {
                        (*sn)(v, st, capts)
                    } else {
                        false
                    }
                },
            )
        }
    } else {
        sn
    };

    if node_type == s2sym("NK") {
        compile_key(&n.at(1).unwrap_or(VVal::None), sn)
    } else if node_type == s2sym("RecGlob") {
        let rec_cond = n.at(1).unwrap_or(VVal::None);
        let recval_cond = n.at(3).unwrap_or(VVal::None);

        let key_cond = if let Some(cond) = rec_cond.at(0) {
            if cond.to_sym() == s2sym("NotKey") {
                let pat = compile_pattern(
                    &rec_cond.at(1).expect("pattern"),
                    Box::new(move |_s: RxBuf, _st: &mut SelectorState| PatResult::matched()),
                );

                Some(Box::new(move |k: &VVal, st: &mut SelectorState| -> bool {
                    !k.with_s_ref(|s| match_pattern(&pat, s, st))
                }))
            } else {
                None
            }
        } else {
            None
        };

        let val_cond =
            if recval_cond.is_some() { Some(compile_node_cond(&recval_cond)) } else { None };

        Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
            if !v.iter_over_vvals() {
                return false;
            }

            let mut found = false;

            let mut stack = vec![v.clone()];
            while let Some(v) = stack.pop() {
                //d// println!("STACK POP: {}", v.s());
                if (*sn)(&v, st, capts) {
                    found = true;
                }

                for (v, k) in v.iter() {
                    if let Some(k) = k {
                        if let Some(key_cond) = &key_cond {
                            if !(*key_cond)(&k, st) {
                                continue;
                            }
                        }
                    }

                    if let Some(val_cond) = &val_cond {
                        if !(*val_cond)(&v, st, &VVal::None) {
                            continue;
                        }
                    }

                    if v.iter_over_vvals() {
                        stack.push(v);
                    }
                }
            }

            found
        })
    } else if node_type == s2sym("NCap") {
        compile_node(
            &n.at(1).expect("capture node"),
            Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                st.push_sel_capture(v.clone());
                let ret = (*sn)(v, st, capts);
                st.pop_sel_caputure();
                ret
            }),
        )
    } else {
        panic!("Unsupported node type: {}", node_type);
    }
}

fn compile_selector(sel: &VVal, no_capture: bool) -> SelNode {
    //d// println!("***** COM SELECTOR: {}", sel.s());
    if let VVal::Lst(_) = sel {
        let first = sel.at(0).unwrap_or(VVal::None);
        if first.to_sym() == s2sym("Path") {
            let mut next: Option<SelNode> =
                Some(Box::new(move |v: &VVal, st: &mut SelectorState, capts: &VVal| {
                    if !no_capture {
                        if st.has_captures() {
                            capts.push(st.get_sel_captures());
                        } else {
                            capts.push(v.clone());
                        }
                    }
                    true
                }));

            for i in 1..sel.len() {
                let nod = sel.at(sel.len() - i).expect("proper path");
                let n = next.take().unwrap();
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

fn compile_find_pattern(v: &VVal) -> PatternNode {
    let pat = compile_pattern(
        v,
        Box::new(move |_s: RxBuf, _st: &mut SelectorState| {
            //d// println!("*** BRANCH LEAF SINGLE PATTERN MATCH {}| {:?}", s, st.captures);
            PatResult::matched()
        }),
    );

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

fn compile_match_pattern(v: &VVal) -> PatternNode {
    let pat = compile_pattern(
        v,
        Box::new(move |_s: RxBuf, _st: &mut SelectorState| {
            //d// println!("*** BRANCH LEAF SINGLE PATTERN MATCH {}| {:?}", s, st.captures);
            PatResult::matched()
        }),
    );

    Box::new(move |s: RxBuf, st: &mut SelectorState| (*pat)(s, st))
}

fn check_pattern_start_anchor(pattern: &VVal) -> bool {
    if let Some(first) = pattern.at(0) {
        if first.is_sym() && first.to_sym() == s2sym("Start") {
            return true;
        } else if first.is_pair() {
            if let Some(pair_type) = first.at(0) {
                let pair_val = first.at(1).unwrap_or(VVal::None);
                let branch_first = pair_val.at(0).unwrap_or(VVal::None);

                if pair_type.to_sym() == s2sym("PatSub") || pair_type.to_sym() == s2sym("PatCap") {
                    return branch_first.is_sym() && branch_first.to_sym() == s2sym("Start");
                }
            }
        }
    }

    false
}

/// Creates a function that takes a VVal data structure
/// and runs the given selector expression on it.
/// The returned function then returns a list of captured nodes
/// or `$none` if nothing was found.
pub fn create_selector(
    sel: &str,
    result_ref: VVal,
) -> Result<Box<dyn Fn(&VVal) -> VVal>, ParseError> {
    let selector = parse_selector(sel)?;
    let comp_sel = compile_selector(&selector, false);

    Ok(Box::new(move |v: &VVal| {
        let mut state = SelectorState::new();
        let capts = VVal::vec();
        (*comp_sel)(v, &mut state, &capts);

        if !capts.is_empty() {
            result_ref.set_ref(capts.clone());
            capts
        } else {
            result_ref.set_ref(VVal::None);
            VVal::None
        }
    }))
}

/// Creates a WLambda function that takes a VVal data structure
/// and runs the given selector expression on it.
/// The returned function then returns a list of captured nodes
/// or `$none` if nothing was found.
pub fn create_selector_function(sel: &str, result_ref: VVal) -> Result<VVal, ParseError> {
    let rref2 = result_ref.clone();
    let sel_fun = create_selector(sel, result_ref)?;

    Ok(VValFun::new_fun(
        move |env: &mut Env, _argc: usize| {
            if let Some(v) = env.arg_ref(0) {
                Ok(sel_fun(v))
            } else {
                rref2.set_ref(VVal::None);
                Ok(VVal::None)
            }
        },
        Some(1),
        Some(1),
        false,
    ))
}

fn parse_and_compile_regex(pat: &str) -> Result<PatternNode, ParseError> {
    let mut ps = State::new(pat, "<pattern>");
    ps.set_pattern_ident_mode();
    ps.skip_ws();
    let pattern = parse_pattern(&mut ps)?;

    ps.skip_ws();

    if !ps.at_end() {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(ps.peek().unwrap(), "end of pattern")));
    }

    let not_find = check_pattern_start_anchor(&pattern);
    let comp_pat =
        if not_find { compile_match_pattern(&pattern) } else { compile_find_pattern(&pattern) };

    Ok(comp_pat)
}

/// Creates a function that takes a string slice and tries to
/// find the compiled regular expression in it.
/// The returned function then returns a `PatResult` which stores
/// the captures and whether the pattern matched.
pub fn create_regex_find(
    pat: &str,
    result_ref: VVal,
) -> Result<Box<dyn Fn(&VVal) -> VVal>, ParseError> {
    let comp_pat = parse_and_compile_regex(pat)?;

    Ok(Box::new(move |v: &VVal| {
        v.with_s_ref(|s| {
            let mut ss = SelectorState::new();
            ss.set_str(s);
            let pat_res = (*comp_pat)(RxBuf::new(s), &mut ss);
            let r = pat_res.to_vval(s);
            result_ref.set_ref(r.clone());
            r
        })
    }))
}

struct FindAllState<'a, 'b> {
    ss: SelectorState,
    s: &'a str,
    comp_pat: &'b PatternNode,
    cur_offs: usize,
}

impl<'a, 'b> FindAllState<'a, 'b> {
    fn new(s: &'a str, comp_pat: &'b PatternNode) -> Self {
        Self { ss: SelectorState::new(), cur_offs: 0, comp_pat, s }
    }

    fn next(&mut self) -> Option<(VVal, (usize, usize))> {
        let rxb = RxBuf::new(&self.s[self.cur_offs..]);

        let pat_res = (*self.comp_pat)(rxb, &mut self.ss);
        if let Some(pos) = pat_res.pos() {
            let v = pat_res.to_vval(&self.s[self.cur_offs..]);
            let out_pos = (self.cur_offs + pos.0, pos.1);
            self.cur_offs += pos.0 + pos.1;
            Some((v, out_pos))
        } else {
            None
        }
    }
}

pub type RegexFindAllFunc = Box<dyn Fn(&VVal, Box<dyn Fn(VVal, (usize, usize))>)>;

/// Creates a function that takes a string slice and tries to
/// find the compiled regular expression in it.
/// The returned function then returns a `PatResult` which stores
/// the captures and whether the pattern matched.
pub fn create_regex_find_all(pat: &str, result_ref: VVal) -> Result<RegexFindAllFunc, ParseError> {
    let comp_pat = parse_and_compile_regex(pat)?;

    Ok(Box::new(move |v: &VVal, fun: Box<dyn Fn(VVal, (usize, usize))>| {
        v.with_s_ref(|s| {
            let mut fs = FindAllState::new(s, &comp_pat);

            while let Some((v, pos)) = fs.next() {
                result_ref.set_ref(v.clone());
                fun(v, pos);
            }
        })
    }))
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum RegexMode {
    Find,
    FindAll,
    Substitute,
}

#[allow(clippy::should_implement_trait)]
impl RegexMode {
    pub fn from_str(s: &str) -> Self {
        match s {
            "g" => RegexMode::FindAll,
            "s" => RegexMode::Substitute,
            _ => RegexMode::Find,
        }
    }
}

/// Creates a WLambda function that takes a string slice and tries to
/// find the compiled regular expression in it.
/// The returned function then returns a `PatResult` which stores
/// the captures and whether the pattern matched.
pub fn create_regex_find_function(
    pat: &str,
    result_ref: VVal,
    mode: RegexMode,
) -> Result<VVal, ParseError> {
    let rref2 = result_ref.clone();
    match mode {
        RegexMode::FindAll => {
            let comp_pat = parse_and_compile_regex(pat)?;
            Ok(VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    let s = env.arg(0);
                    let fun = env.arg(1).disable_function_arity();

                    s.with_s_ref(|s| {
                        let mut fs = FindAllState::new(s, &comp_pat);
                        let mut ret = Ok(VVal::None);

                        while let Some((v, pos)) = fs.next() {
                            result_ref.set_ref(v.clone());
                            env.push(v);
                            env.push(VVal::Int(pos.0 as i64));
                            env.push(VVal::Int(pos.1 as i64));
                            match fun.call_internal(env, 3) {
                                Ok(r) => {
                                    ret = Ok(r);
                                }
                                Err(StackAction::Break(v)) => {
                                    ret = Ok(v.as_ref().clone());
                                    env.popn(3);
                                    break;
                                }
                                Err(StackAction::Next) => {}
                                Err(e) => {
                                    ret = Err(e);
                                    env.popn(3);
                                    break;
                                }
                            }
                            env.popn(3);
                        }

                        ret
                    })
                },
                Some(2),
                Some(2),
                false,
            ))
        }
        RegexMode::Substitute => {
            let comp_pat = parse_and_compile_regex(pat)?;
            Ok(VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    let s = env.arg(0);
                    let fun = env.arg(1).disable_function_arity();

                    let mut out = String::new();
                    let mut last_offs = 0;

                    s.with_s_ref(|s| {
                        let mut fs = FindAllState::new(s, &comp_pat);
                        let mut ret = Ok(VVal::None);

                        while let Some((v, pos)) = fs.next() {
                            if last_offs < pos.0 {
                                out.push_str(&s[last_offs..pos.0]);
                                last_offs = pos.0 + pos.1;
                            }

                            result_ref.set_ref(v.clone());
                            env.push(v);
                            env.push(VVal::Int(pos.0 as i64));
                            env.push(VVal::Int(pos.1 as i64));
                            match fun.call_internal(env, 3) {
                                Ok(r) => {
                                    r.with_s_ref(|r| {
                                        out.push_str(r);
                                        last_offs = pos.0 + pos.1;
                                    });
                                }
                                Err(StackAction::Break(v)) => {
                                    v.with_s_ref(|r| {
                                        out.push_str(r);
                                        last_offs = pos.0 + pos.1;
                                    });
                                    env.popn(3);
                                    break;
                                }
                                Err(StackAction::Next) => {}
                                Err(e) => {
                                    ret = Err(e);
                                    env.popn(3);
                                    break;
                                }
                            }
                            env.popn(3);
                        }

                        if last_offs < s.len() {
                            out.push_str(&s[last_offs..]);
                        }

                        if let Ok(VVal::None) = ret {
                            ret = Ok(VVal::new_str_mv(out));
                        }

                        ret
                    })
                },
                Some(2),
                Some(2),
                false,
            ))
        }
        RegexMode::Find => {
            let match_fun = create_regex_find(pat, result_ref)?;
            Ok(VValFun::new_fun(
                move |env: &mut Env, _argc: usize| {
                    if let Some(s) = env.arg_ref(0) {
                        Ok(match_fun(s))
                    } else {
                        rref2.set_ref(VVal::None);
                        Ok(VVal::None)
                    }
                },
                Some(1),
                Some(1),
                false,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex_syntax::ast::Ast;
    //    use regex_syntax::ast::Literal;
    use regex_syntax::ast::LiteralKind;
    use regex_syntax::ast::RepetitionKind;
    use regex_syntax::ast::SpecialLiteralKind;

    pub fn re_ast2wlpat(a: &Ast) -> String {
        match a {
            Ast::Empty(_) => "".to_string(),
            Ast::Flags(f) => {
                panic!("Got flags: {:?}", f);
            }
            Ast::Literal(l) => match &l.kind {
                LiteralKind::Verbatim => l.c.to_string(),
                LiteralKind::Punctuation => l.c.to_string(),
                LiteralKind::Octal => l.c.to_string(),
                LiteralKind::HexFixed(_) => l.c.to_string(),
                LiteralKind::HexBrace(_) => l.c.to_string(),
                LiteralKind::Special(slk) => match &slk {
                    SpecialLiteralKind::Bell => "\\b".to_string(),
                    SpecialLiteralKind::FormFeed => "\\f".to_string(),
                    SpecialLiteralKind::Tab => "\\t".to_string(),
                    SpecialLiteralKind::LineFeed => "\\n".to_string(),
                    SpecialLiteralKind::CarriageReturn => "\\r".to_string(),
                    SpecialLiteralKind::VerticalTab => "\\v".to_string(),
                    SpecialLiteralKind::Space => "[ ]".to_string(),
                },
            },
            Ast::Dot(_) => "?".to_string(),
            Ast::Assertion(a) => {
                panic!("Assertion: {:?}", a);
            }
            Ast::Class(cl) => {
                panic!("Class: {:?}", cl);
            }
            Ast::Repetition(rep) => match &rep.op.kind {
                RepetitionKind::ZeroOrOne => {
                    if rep.greedy {
                        "$?".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                    } else {
                        "$<?".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                    }
                }
                RepetitionKind::OneOrMore => {
                    if rep.greedy {
                        "$+".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                    } else {
                        "$<+".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                    }
                }
                RepetitionKind::ZeroOrMore => {
                    if rep.greedy {
                        "$*".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                    } else {
                        "$<*".to_string() + &re_ast2wlpat(rep.ast.as_ref())
                    }
                }
                _ => panic!("Unimplemented rep op: {:?}", rep),
            },
            Ast::Group(g) => {
                panic!("Grp: {:?}", g);
            }
            Ast::Alternation(alt) => {
                panic!("alt: {:?}", alt);
            }
            Ast::Concat(cat) => {
                cat.asts.iter().map(|a| re_ast2wlpat(a)).collect::<Vec<String>>().join("")
            }
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
        ps.set_pattern_ident_mode();
        ps.skip_ws();
        match parse_pattern(&mut ps) {
            Ok(v) => {
                //d// println!("PAT: {}", v.s());
                let pn = compile_find_pattern(&v);
                let mut ss = SelectorState::new();
                ss.set_str(st);
                let rb = RxBuf::new(st);
                (*pn)(rb, &mut ss).to_test_string(st)
            }
            Err(e) => format!("Error: {}", e),
        }
    }

    fn p(s: &str) -> String {
        match parse_selector(s) {
            Ok(v) => v.s(),
            Err(e) => format!("Error: {}", e),
        }
    }

    fn pev(s: &str, v: &VVal) -> VVal {
        let sel_ast = match parse_selector(s) {
            Ok(v) => v,
            Err(e) => {
                return VVal::new_str_mv(format!("Error: {}", e));
            }
        };
        let sn = compile_selector(&sel_ast, false);
        let mut state = SelectorState::new();
        let capts = VVal::vec();
        (*sn)(v, &mut state, &capts);
        capts.sort(|a: &VVal, b: &VVal| a.compare_str(b));
        capts
    }

    fn pes(s: &str, v: &VVal) -> String {
        pev(s, v).s()
    }

    #[test]
    fn check_selector_match_path() {
        let v1 = VVal::map3(
            "a",
            VVal::vec3(VVal::Int(20), VVal::pair(VVal::Int(2), VVal::Int(4)), VVal::new_str("F0O")),
            "ab",
            VVal::vec2(VVal::Int(33), VVal::Int(44)),
            "xyab",
            VVal::vec3(
                VVal::Int(8),
                VVal::Int(9),
                VVal::map2("X", VVal::Int(10), "Y", VVal::Int(20)),
            ),
        );

        assert_eq!(pes("a", &v1), "$[$[20,$p(2,4),\"F0O\"]]");
        assert_eq!(pes("a/2/2", &v1), "$['O']");
        assert_eq!(pes("a/2/1", &v1), "$['0']");
        assert_eq!(pes("ab/0", &v1), "$[33]");

        assert_eq!(pes("a/?", &v1), "$[$p(2,4),20,\"F0O\"]");
        assert_eq!(pes("a/?/1", &v1), "$['0',4]");

        assert_eq!(pes("?/1", &v1), "$[$p(2,4)]");
        assert_eq!(pes("?/2", &v1), "$[\"F0O\"]");

        assert_eq!(pes("?b/1", &v1), "$[44]");
        assert_eq!(pes("a?/1", &v1), "$[44]");
        assert_eq!(pes("??ab/1", &v1), "$[9]");

        assert_eq!(pes("*/X", &v1), "$[]");
        assert_eq!(pes("*/?/X", &v1), "$[10]");
        assert_eq!(pes("*/*/X", &v1), "$[10]");
        assert_eq!(pes("*/2/2", &v1), "$['O']");

        assert_eq!(pes("*ab/*/X", &v1), "$[10]");

        assert_eq!(pes("[xy][xy]*/[01]", &v1), "$[8,9]");
        assert_eq!(pes("[^xy][^xy]/[01]", &v1), "$[33,44]");
        assert_eq!(pes("a/[^01]", &v1), "$[\"F0O\"]");

        assert_eq!(pes("(ab)/[01]", &v1), "$[33,44]");
        assert_eq!(pes("(x)y(a)b/[01]", &v1), "$[8,9]");
        assert_eq!(pes("$!(a)*/[01]", &v1), "$[8,9]");
        assert_eq!(pes("a/$![01]?", &v1), "$[\"F0O\"]");

        assert_eq!(pes("$=x*/[01]", &v1), "$[8,9]");
        assert_eq!(pes("$=(ab)*/[01]", &v1), "$[33,44]");
        assert_eq!(pes("a$=b*/[01]", &v1), "$[33,44]");
        assert_eq!(pes("$!x*$=b?/[01]", &v1), "$[33,44]");

        assert_eq!(pes("$+[xy]ab/0", &v1), "$[8]");
        assert_eq!(pes("a$+b/0", &v1), "$[33]");
        assert_eq!(pes("$*[xy]ab/0", &v1), "$[33,8]");
        assert_eq!(pes("$?[xy][xy]ab/0", &v1), "$[8]");

        let v2 = VVal::map1("\t", VVal::Int(12));
        assert_eq!(pes("\\t", &v2), "$[12]");
        assert_eq!(pes("[\\t]", &v2), "$[12]");
    }

    #[test]
    fn check_selector_match_esc() {
        let v1 = VVal::map3(
            "\\",
            VVal::vec3(
                VVal::Int(20),
                VVal::pair(VVal::Int(2), VVal::Int(4)),
                VVal::new_str("F0O%/{}[]"),
            ),
            "//",
            VVal::vec2(VVal::Int(33), VVal::Int(44)),
            "?*",
            VVal::vec3(
                VVal::Int(8),
                VVal::Int(9),
                VVal::map2("*", VVal::Int(10), "|", VVal::Int(20)),
            ),
        );

        assert_eq!(pes("*/*/\\*", &v1), "$[10]");
        assert_eq!(pes("*/*/\\|", &v1), "$[20]");

        assert_eq!(pes("[\\\\]*/1", &v1), "$[$p(2,4)]");
        assert_eq!(pes("[\\/]*/0", &v1), "$[33]");
        assert_eq!(pes("\\/\\//0", &v1), "$[33]");
    }

    #[test]
    fn check_selector_kv_match() {
        let v1 = v(r#"
            $[
                ${ a = :test, x = 10        , childs = $[10, 20], },
                ${ b = :test, x = 11        , childs = $[11, 21, 31], },
                ${ a = :test, x = 12        , childs = $[12, 22, 32, 42], },
                ${ c = :test, y = 15, x = 22, childs = $[13, 23, 33, 43, 53], },
                ${ a = :test, y = 16, x = 23, childs = $[14, 24, 34, 44, 54, 64], },
            ]
        "#);

        assert_eq!(pes("*:{a = test, x = 1* }/childs/1", &v1), "$[20,22]");

        assert_eq!(pes("*/[xy]", &v1), "$[10,11,12,15,16,22,23]");
        assert_eq!(pes("*/:{a = test, x = 1* }", &v1), "$[]");
        assert_eq!(
            pes("*:{a = test, x = 1* }", &v1),
            "$[${a=:test,childs=$[10,20],x=10},${a=:test,childs=$[12,22,32,42],x=12}]"
        );
        assert_eq!(pes("*:{a = test, x = 2* }/y", &v1), "$[16]");
        assert_eq!(
            pes("*:{[ab] = test }/childs/*", &v1),
            "$[10,11,12,14,20,21,22,24,31,32,34,42,44,54,64]"
        );
        assert_eq!(pes("*:{a = test}/[xy]", &v1), "$[10,12,16,23]");

        assert_eq!(pes("*:!{a = test}/[xy]", &v1), "$[11,15,22]");
        assert_eq!(pes("*:!{ * = * }/[xy]", &v1), "$[]");
        assert_eq!(pes("*:!{ x = 1* }/[xy]", &v1), "$[15,16,22,23]");
        assert_eq!(pes("*:!{[bc] = test} & :{ y = *6 }/childs/1", &v1), "$[24]");
        assert_eq!(
            pes(
                r"
            *:!{[bc] = test}
              & :{ y = *6 }
                | :{ x = *[12] }
            /childs/1",
                &v1
            ),
            "$[22,24]"
        );
        assert_eq!(pes("*:!{a = test} & :!{ c = test }/childs/1", &v1), "$[21]");
        assert_eq!(pes("*:{a = test} | :{ c = test }/childs/1", &v1), "$[20,22,23,24]");
    }

    #[test]
    fn check_selector_rec_glob() {
        let v1 = v(r#"
            !i = $[10, 20, 30];
            !j = $[40, 50];
            !k = $[90, 80];
            !d = ${ h = 10, w = 20, childs = $[ i, j ] };
            !e = ${ h = 20, w = 24, childs = $[ j, k ] };
            !f = ${ h = 12, w = 30, childs = $[ i, k ] };
            $[
                ${ a = :test, x = 10        , childs = $[d, e], },
                ${ b = :test, x = 11        , childs = $[d, e, f], },
                ${ a = :test, x = 12        , childs = $[f], },
                ${ c = :test, y = 15, x = 22, childs = $[e], },
                ${ a = :test, y = 16, x = 23, childs = $[f, e], },
            ]
        "#);

        assert_eq!(
            pes("*/childs/**/*:type = integer & :str=[89]*", &v1),
            "$[80,80,80,80,80,80,80,90,90,90,90,90,90,90]"
        );
        assert_eq!(pes("**/childs/*:{ w = 2* }/h", &v1), "$[10,10,20,20,20,20]");
        assert_eq!(pes("**/childs/*:type=vector/*:type = integer", &v1),
            "$[10,10,10,10,10,20,20,20,20,20,30,30,30,30,30,40,40,40,40,40,40,50,50,50,50,50,50,80,80,80,80,80,80,80,90,90,90,90,90,90,90]");

        // with capturing
        assert_eq!(
            pes("*/childs/^**/^*:type = integer & :str=[89]*", &v1),
            "$[$[$<1=>$[90,80],80],$[$<1>,80],$[$<1>,80],$[$<1>,80],$[$<1>,80],$[$<1>,80],$[$<1>,80],$[$<1>,90],$[$<1>,90],$[$<1>,90],$[$<1>,90],$[$<1>,90],$[$<1>,90],$[$<1>,90]]");
        assert_eq!(
            pes("*/0/[x]", &pev("**/^*:{a=*}|:{b=*}/childs/*:{h=12}/^h", &v1)),
            "$[11,12,23]"
        );

        assert_eq!(pes("**/*:str=1[^0]", &v1), "$[11,12,12,12,12,15,16]");
    }

    #[test]
    fn check_selector_la() {
        let v1 = v(r#"
            !i = $[10, 20, 30];
            !j = $[40, 50];
            !k = $[90, 80];
            !d = ${ h = 10, w = 20, childs = $[ i, j ] };
            !e = ${ h = 20, w = 24, childs = $[ j, k ] };
            !f = ${ h = 12, w = 30, childs = $[ i, k ] };
            $[
                ${ a = :test, x = 10        , childs = $[d, e], },
                ${ b = :test, x = 11        , childs = $[d, e, f], },
                ${ a = :test, x = 12        , childs = $[f], },
                ${ c = :test, y = 15, x = 22, childs = $[e], },
                ${ a = :test, y = 16, x = 23, childs = $[f, e], },
            ]
        "#);

        assert_eq!(pes("*:(childs/*:{h=12})/x", &v1), "$[11,12,23]");
    }

    #[test]
    fn check_selector_rec_cond() {
        let v1 = v(r#"
            $[
                ${ a = :test, x = 10        , childs = $[${ i = 90 }], },
                ${ b = :test, x = 11        , childs = $[${ i = 91 }], },
                ${ a = :test, x = 12        , childs = $[${ i = 92 }], },
                ${ c = :test, y = 15, x = 22, childs = $[${ i = 93 }], },
                ${ a = :test, y = 16, x = 23, childs = $[
                    ${ a = :test, x = 13, childs = $[${ i = 94 }] },
                ] }
            ]
        "#);

        assert_eq!(pes("**:{ x = 1* }/childs/*/i", &v1), "$[90,91,92,94]");
        assert_eq!(pes("** ! key = childs/childs/*/i", &v1), "$[90,91,92,93]");
        assert_eq!(pes("** ! key = childs :{ x = 1* }/childs/*/i", &v1), "$[90,91,92]");

        assert_eq!(pes("** =:{ a = test } /x", &v1), "$[10,12,23]");
        assert_eq!(pes("** =:{ a = test } | :type=vector /x", &v1), "$[10,12,13,23]");
        assert_eq!(pes("** =:{ a = test } | :type=vector :{y=16}/x|y|a", &v1), "$[16,23,:test]");
        assert_eq!(pes("** !key=childs =:{ a = test } | :type=vector /x", &v1), "$[10,12,23]");
    }

    #[test]
    fn check_selector_node_path() {
        assert_eq!(p("a"), "$[:Path,$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(p("a/0/2"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NK,0],$[:NK,2]]");
        assert_eq!(
            p("a/b/c"),
            "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NK,$[$p(:I,:b)]],$[:NK,$[$p(:I,:c)]]]"
        );

        assert_eq!(
            p("a/^b/c/^"),
            "Error: <selector>:1:9 EOF while parsing: Unexpected EOF\nat code:\n1   | \n"
        );
        assert_eq!(p("a/^b/c/^*"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NCap,$[:NK,$[$p(:I,:b)]]],$[:NK,$[$p(:I,:c)]],$[:NCap,$[:NK,$[:Glob]]]]");
        assert_eq!(p("a/^b/^c"), "$[:Path,$[:NK,$[$p(:I,:a)]],$[:NCap,$[:NK,$[$p(:I,:b)]]],$[:NCap,$[:NK,$[$p(:I,:c)]]]]");
    }

    #[test]
    fn check_selector_globs() {
        assert_eq!(p("*"), "$[:Path,$[:NK,$[:Glob]]]");
        assert_eq!(p("**"), "$[:Path,$[:RecGlob,$n,$n,$n]]");
        assert_eq!(p("^**"), "$[:Path,$[:NCap,$[:RecGlob,$n,$n,$n]]]");
        assert_eq!(p("^*"), "$[:Path,$[:NCap,$[:NK,$[:Glob]]]]");

        assert_eq!(p("(a)"), "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:a)])]]]");
        assert_eq!(p("(^a)"), "$[:Path,$[:NK,$[$p(:PatCap,$[$p(:I,:a)])]]]");
        assert_eq!(p("^(^a)"), "$[:Path,$[:NCap,$[:NK,$[$p(:PatCap,$[$p(:I,:a)])]]]]");

        assert_eq!(
            p("(*|a?)"),
            "$[:Path,$[:NK,$[$p(:PatSub,$[:Alt,$[:Glob],$[$p(:I,:a),:Any]])]]]"
        );

        assert_eq!(p("*/*/a"), "$[:Path,$[:NK,$[:Glob]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:a)]]]");
        assert_eq!(
            p("*  /  *  /   a   "),
            "$[:Path,$[:NK,$[:Glob]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:a)]]]"
        );
        assert_eq!(
            p("**/^a/**"),
            "$[:Path,$[:RecGlob,$n,$n,$n],$[:NCap,$[:NK,$[$p(:I,:a)]]],$[:RecGlob,$n,$n,$n]]"
        );

        assert_eq!(p("?a"), "$[:Path,$[:NK,$[:Any,$p(:I,:a)]]]");
    }

    #[test]
    fn check_selector_kvmatch_parse() {
        assert_eq!(p("*:{b=a}"), "$[:Path,$[:NK,$[:Glob],$[:KV,$[$[$p(:I,:b)],$[$p(:I,:a)]]]]]");
        assert_eq!(
            p(":{b=a,a=20}"),
            "$[:Path,$[:NK,$[],$[:KV,$[$[$p(:I,:b)],$[$p(:I,:a)]],$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]"
        );
        assert_eq!(
            p("a : { a = 20 }"),
            "$[:Path,$[:NK,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]"
        );
        assert_eq!(
            p("a :!{ a = 20 }"),
            "$[:Path,$[:NK,$[$p(:I,:a)],$[:Not,$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]]]]]"
        );
        assert_eq!(p("a : { a = 20, b=a(a?)[^ABC]cc*f}"),          "$[:Path,$[:NK,$[$p(:I,:a)],$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]],$[$[$p(:I,:b)],$[$p(:I,:a),$p(:PatSub,$[$p(:I,:a),:Any]),$p(:NCCls,\"ABC\"),$p(:I,:cc),:Glob,$p(:I,:f)]]]]]");
        assert_eq!(p("a : { a = 20 } | :{ b = 20 } & :{ x = 10}"), "$[:Path,$[:NK,$[$p(:I,:a)],$[:Or,$[:KV,$[$[$p(:I,:a)],$[$p(:I,:20)]]],$[:And,$[:KV,$[$[$p(:I,:b)],$[$p(:I,:20)]]],$[:KV,$[$[$p(:I,:x)],$[$p(:I,:10)]]]]]]]");
        assert_eq!(p("a : (b/*/c)"),   "$[:Path,$[:NK,$[$p(:I,:a)],$[:LA,$[:Path,$[:NK,$[$p(:I,:b)]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:c)]]]]]]");
        assert_eq!(p("a :!(b/*/c)"),   "$[:Path,$[:NK,$[$p(:I,:a)],$[:Not,$[:LA,$[:Path,$[:NK,$[$p(:I,:b)]],$[:NK,$[:Glob]],$[:NK,$[$p(:I,:c)]]]]]]]");
    }

    #[test]
    fn check_selector_subpat() {
        assert_eq!(p("(^abc$$)"), "$[:Path,$[:NK,$[$p(:PatCap,$[$p(:I,:abc),:End])]]]");
        assert_eq!(p("(\\^abc$$)"), "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:\"^abc\"),:End])]]]");

        assert_eq!(p("(abc)"), "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:abc)])]]]");
        assert_eq!(p("$!abc"), "$[:Path,$[:NK,$[$p(:ZwNegLA,$p(:I,:a)),$p(:I,:bc)]]]");
        assert_eq!(p("$!(abc)"), "$[:Path,$[:NK,$[$p(:ZwNegLA,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$*(abc)"), "$[:Path,$[:NK,$[$p(:N0,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$+(abc)"), "$[:Path,$[:NK,$[$p(:N1,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$?(abc)"), "$[:Path,$[:NK,$[$p(:Opt,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$=(abc)"), "$[:Path,$[:NK,$[$p(:ZwLA,$p(:PatSub,$[$p(:I,:abc)]))]]]");
        assert_eq!(p("$^abc$$"), "$[:Path,$[:NK,$[:Start,$p(:I,:abc),:End]]]");
        assert_eq!(p("(\\$abc)"), "$[:Path,$[:NK,$[$p(:PatSub,$[$p(:I,:\"$abc\")])]]]");
    }

    #[test]
    fn check_patterns() {
        assert_eq!(pat("A", "XXAYY"), "A");
        assert_eq!(pat("AY", "XXAYY"), "AY");

        assert_eq!(pat("A($<+B)", "ABB"), "AB");
        assert_eq!(pat("A($<+B)", "AB"), "AB");
        assert_eq!(pat("A($<+B)", "ABBB"), "AB");

        assert_eq!(pat("A($<+B)$$", "ABB"), "ABB");

        assert_eq!(pat("$^A($<+B)$$", "ABB"), "ABB");
        assert_eq!(pat("$^A($<+B)$$", "ABBB"), "ABBB");
        assert_eq!(pat("$^$+A($<+B)$$", "ABBB"), "ABBB");
        assert_eq!(pat("$^$<+A($<+B)$$", "ABBB"), "ABBB");
        assert_eq!(pat("$^$<+A($<+B)$$", "ABBB"), "ABBB");
        assert_eq!(pat("$^$<+A($<+B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($<+B)$$", "AB"), "AB");
        assert_eq!(pat("$^A($<+B)", "ABB"), "AB");
        assert_eq!(pat("$^A($<+B)", "ABBB"), "AB");
        assert_eq!(pat("$^$+A($<+B)", "AABBB"), "AAB");
        assert_eq!(pat("$^$<+A($<+B)", "AABBB"), "AAB");
        assert_eq!(pat("$^$<+A($<+B)", "AABBB"), "AAB");
        assert_eq!(pat("$^$<+A($<+B)C", "AABBBC"), "AABBBC");
        assert_eq!(pat("$^A($<+B)", "AB"), "AB");

        assert_eq!(pat("$^ABC$$", "ABC"), "ABC");
        assert_eq!(pat("$^AB$$C", "ABC"), "-nomatch-");
        assert_eq!(pat("A$^ABC$$", "ABC"), "-nomatch-");

        assert_eq!(pat("$^A($+BB)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($+(B)B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($+($+B)B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($+(($+B)B)B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($+(($+B))B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($+(($+B)B))C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$+A($+($+B)B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$+A$+(B)$+BC$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$+A((B)$+B)$$", "ABB"), "ABB");
        assert_eq!(pat("$^$+BC$$", "BC"), "BC");

        assert_eq!(pat("$^$+C$$", "C"), "C");
        assert_eq!(pat("$^ABBB$+C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$+A($+($+B)$+B)$+C$$", "ABBBC"), "ABBBC");

        assert_eq!(pat("$^($<+B)C$$", "BC"), "BC");
        assert_eq!(pat("$^$<+A($<+B)C$$", "ABC"), "ABC");
        assert_eq!(pat("$^$<+A((B)$<+B)C$$", "ABBC"), "ABBC");
        assert_eq!(pat("$^$<+BB$$", "BB"), "BB");
        assert_eq!(pat("$<+BB$$", "BB"), "BB");
        assert_eq!(pat("$+BB$$", "BB"), "BB");
        assert_eq!(pat("$^$<+BB$$", "BB"), "BB");
        assert_eq!(pat("$^$<+B$$", "B"), "B");
        assert_eq!(pat("$^$<+BB$$", "BBB"), "BBB");
        assert_eq!(pat("$^A$<+BB$$", "ABBB"), "ABBB");
        assert_eq!(pat("$^A$<+BBC$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A($<+B$<+B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$<+A($<+B$<+B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$<+A($<+(B)$<+B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)C$$", "ABBBC"), "ABBBC");

        assert_eq!(pat("$^$<+C$$", "C"), "C");
        assert_eq!(pat("$^ABBB$<+C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^$<+A($<+($<+B)$<+B)$<+C$$", "ABBBC"), "ABBBC");

        assert_eq!(pat("$^A($*BB)C$$", "ABBBC"), "ABBBC");
        assert_eq!(pat("$^A(^B)C$$", "ABC"), "ABC-B");
        assert_eq!(pat("$^A(^$*B)C$$", "ABBBC"), "ABBBC-BBB");
        assert_eq!(pat("BC", "ABC"), "BC");
        assert_eq!(pat("(BC)", "ABC"), "BC");
        assert_eq!(pat("(^BC)", "ABC"), "BC-BC");
        assert_eq!(pat("$^[ ]$$", " "), " ");
        assert_eq!(pat("$^$*[ ]$$", "   "), "   ");

        assert_eq!(pat("$^ $!x*$=b? $$", "ab"), "ab");
        assert_eq!(pat("$^ $!x*$=b? $$", "xyab"), "-nomatch-");

        assert_eq!(pat("$<?a", "a"), "");
        assert_eq!(pat("$?a", "a"), "a");
        assert_eq!(pat("$^ (^$<?a)(^$+a) $$", "aaa"), "aaa--aaa");
        assert_eq!(pat("$^ (^$?a)(^$+a) $$", "aaa"), "aaa-a-aa");

        assert_eq!(pat("$+($?abab)", "abbbxababb"), "abab");
        assert_eq!(pat("$*($?abab)", "abbbxababb"), "");
        assert_eq!(pat("$+(x$?abab)", "abbbxababb"), "xabab");
        assert_eq!(pat("$+(x$?abab)", "abbbxababxababb"), "xababxabab");
        assert_eq!(pat("$<+(x$?abab)", "abbbxababxababb"), "xabab");
        assert_eq!(pat("bbb$*(x$?abab)", "abbbxababxababb"), "bbbxababxabab");
        assert_eq!(pat("bbb$<*(x$?abab)x", "abbbxababxababb"), "bbbx");
        assert_eq!(pat("bbb$<*?ba", "abbbxababxababb"), "bbbxaba");
        assert_eq!(pat("bbb$*?ba", "abbbxababxababb"), "bbbxababxaba");
        assert_eq!(pat("bbb$<*(x$?abab)", "abbbxababxababb"), "bbb");
        assert_eq!(pat("$*(a$?b)", "abbbababb"), "ab");
        assert_eq!(pat("$*($?ab)", "abbbababb"), "abbbababb");
        assert_eq!(pat("$<*($?ab)", "abbbababb"), "");

        assert_eq!(pat("[\\t\\0\\u{0101}]", "\0"), "\u{0}");
        assert_eq!(pat("[\\t\\0\\u{0101}]", "\t"), "\t");
        assert_eq!(pat("[\\t\\0\\u{0101}]", "ā"), "ā");

        assert_eq!(pat("a$?[xy]a", "aa"), "aa");
        assert_eq!(pat("$^$?[xy]$$", "a"), "-nomatch-");
        assert_eq!(pat("$?[xy]", "x"), "x");
        assert_eq!(pat("$?[xy]a", "xa"), "xa");
        assert_eq!(pat("$?[xy][xy]abc$$", "xyabc"), "xyabc");
        assert_eq!(pat("$?[xy][xy]ab", "xyab"), "xyab");
        assert_eq!(pat("$?[xy][xy]ab$$", "xyab"), "xyab");

        assert_eq!(pat("xyab|ab", "xyab"), "xyab");
        assert_eq!(pat("xyab|ab", "jjab"), "ab");
        assert_eq!(pat("(x|y|z)(y|x)(ab|)", "xyab"), "xyab");
        assert_eq!(pat("$+(x|y|z)(y|x)(ab|)", "zxyab"), "zxyab");
        assert_eq!(pat("$^ $*(x|y|z)(ab|) $$", "zxyab"), "zxyab");

        assert_eq!(pat("$^ $+$s $$", "  \t\n\r  "), "  \t\n\r  ");
        assert_eq!(pat(" $+ $S ", "  \t\nXXJFJF\r  "), "XXJFJF");

        assert_eq!(pat("AB $&L $+b $&U C", " ABbbBbc "), "ABbbBbc");
        assert_eq!(pat("$&U A$+BC", " abbbbbc "), "abbbbbc");
        assert_eq!(pat("$&L a$+bc", " ABBBBBC "), "ABBBBBC");

        assert_eq!(pat("$+[a-z]", "ABabzXXZ"), "abz");
        assert_eq!(pat("$+[^a-z]", "ABabzXXZ"), "AB");
        assert_eq!(pat("$+[-z]", "ABab-z-XXZ"), "-z-");
        assert_eq!(pat("$+[z-]", "ABab-z-XXZ"), "-z-");

        assert_eq!(pat("$+\\f", "ABfffFO"), "fff");
        assert_eq!(pat("$+\\x41", "BAAAO"), "AAA");
        assert_eq!(pat("$+ \\$", "ABx$$$xxFO"), "$$$");
        assert_eq!(pat("x$* \\$", "ABx$$$xxFO"), "x$$$");
        assert_eq!(pat("\\u{2211}", "∑"), "∑");
    }

    #[test]
    fn check_pattern_ident_mode() {
        assert_eq!(pat("($*!/$*\\\\^$*,':;{}=)", "!!!/\\\\\\^':;{}="), "!!!/\\\\\\^':;{}=");
    }

    #[test]
    fn check_patterns_py_tests() {
        assert_eq!(pat("(ab|cd)e", "cde"), "cde");
        assert_eq!(pat("$*($+a|b)", "ab"), "ab");
        assert_eq!(pat("$+($+a|b)", "ab"), "ab");
        assert_eq!(pat("$?($+a|b)", "ab"), "a");

        assert_eq!(re2wlpat("a.b"), "a?b");
        assert_eq!(rep("a.b", "acb"), "acb");
        assert_eq!(rep("a.*b", "acc\nccb"), "acc\nccb");
    }

    #[test]
    fn check_pattern_capture() {
        assert_eq!(pat("(^ab|cd)e", "cde"), "cde-cd");

        assert_eq!(pat("(^(^AA)C)$$", "AAC"), "AAC-AAC-AA");
        assert_eq!(pat("$<+(^aa)$*(^a)$+(^AA)", "aaaaAAAA"), "aaaaAAAA-aa-a-AA");
        assert_eq!(pat("$+(^aa|bb)$+(^A(A)|B(B)|X(X))", "aabbAABBBXX"), "aabbAABB-bb-BB");
        assert_eq!(pat("$+(^AA|BB|XX)", "AABBBXX"), "AABB-BB");

        assert_eq!(pat("$+(^A|B|X)", "AABBBXX"), "AABBBXX-X");
        assert_eq!(pat("(^$+A|$+B|$+X)", "AABBBXX"), "AA-AA");
        assert_eq!(pat("(^$+A)(^$+B)$+(^X)$$", "AABBBXX"), "AABBBXX-AA-BBB-X");
        assert_eq!(pat("(^$+A)(^$?L)(^$+B)$+(^X)$$", "AABBBXX"), "AABBBXX-AA--BBB-X");

        assert_eq!(
            pat("(^$<+(^aa)$*(^a)$+(^AA) | $<+(^aa)$*(^a)$+(^AA)C)$$", "aaaaAAAAC"),
            "aaaaAAAAC-aaaaAAAAC-aa-a-AA"
        );

        assert_eq!(pat("(^a)$*?(^$+b)$*?c", "afoefoeeoobbbbfec"), "afoefoeeoobbbbfec-a-b");
        assert_eq!(pat("(^a)$<*?(^$+b)$*?c", "afoefoeeoobbbbfec"), "afoefoeeoobbbbfec-a-bbbb");
        assert_eq!(pat("(^a)$*[^b](^$+b)$*?c", "afoefoeeoobbbbfec"), "afoefoeeoobbbbfec-a-bbbb");
    }
}
