// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::SynPos;
use crate::vval::FileRef;
use std::fmt;

/// This is the parser state data structure. It holds the to be read source
/// code and keeps track of the parser head position.
///
/// Can be created using `parser::State::new`:
///
/// ```rust
/// use wlambda::parser::State;
///
/// let code    = "{ 123 }";
/// let mut ps  = State::new(code, "filenamehere");
///
/// // ...
/// ```
#[allow(dead_code)]
pub struct State {
    input:          Vec<char>,
    ch_ptr:         usize,
    line_no:        u32,
    col_no:         u16,
    indent:         Option<u32>,
    line_indent:    u32,
    last_tok_char:  char,
    file:           FileRef,
}

#[derive(Copy, Clone, Debug)]
pub struct IndentPos {
    line_no:        u32,
    indent:         Option<u32>,
    line_indent:    u32,
}

impl IndentPos {
    pub fn belongs_to(&self, call_ip: &IndentPos) -> bool {
        self.line_no == call_ip.line_no
        || (if let Some(i) = self.indent {
                if let Some(i2) = call_ip.indent { i > i2 }
                else { i > call_ip.line_indent } }
            else { true })
    }
}

/// The possible errors the parser can detect while reading code.
#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken(char, &'static str),
    ExpectedToken(char, &'static str),
    BadEscape(&'static str),
    BadIndent(&'static str),
    BadValue(ParseValueError),
    BadPattern(String),
    BadFormat(String),
    BadKeyword(String, &'static str),
    BadNumber(ParseNumberError),
    //BadCall(&'static str),
    EOF(&'static str),
}
impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseErrorKind::*;
        match self {
            UnexpectedToken(c, s) => write!(f, "Unexpected token '{}'. At {}", c, s),
            ExpectedToken(c, s)   => write!(f, "Expected token '{}'. At {}", c, s),
            BadEscape(s)          => write!(f, "{}", s),
            BadIndent(s)          => write!(f, "{}", s),
            BadValue(s)           => write!(f, "{}", s),
            BadPattern(s)         => write!(f, "{}", s),
            BadFormat(s)          => write!(f, "{}", s),
            BadKeyword(kw, s)     => write!(f, "Got '{}', expected {}", kw, s),
            BadNumber(s)          => write!(f, "{}", s),
            //BadCall(s)            => write!(f, "{}", s),
            EOF(s)                => write!(f, "EOF while parsing: {}", s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseValueError {
    Expected(&'static str),
    UnknownSpecialIdentifier(char),
    VectorLength,
    ExpectedAccumulator,
    ExpectedMaxArity,
    ExpectedMinArity,
}
impl fmt::Display for ParseValueError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseValueError::*;
        match self {
            UnknownSpecialIdentifier(c) => write!(
                f,
                "Expected special value, unknown special value identifier '{}'",
                c
            ),
            Expected(s) => write!(
                f,
                "Expected {}",
                s
            ),
            VectorLength        => write!(f, "Numerical Vectors must have 2 to 4 values, inclusive!"),
            ExpectedAccumulator => write!(f, "Expected accumulator value"),
            ExpectedMaxArity    => write!(f, "Expected integer value for min arity"),
            ExpectedMinArity    => write!(f, "Expected integer value for max arity"),
        }
    }
}
impl Into<ParseErrorKind> for ParseValueError {
    fn into(self) -> ParseErrorKind {
        ParseErrorKind::BadValue(self)
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseNumberError {
    InvalidIndexDigits(String),
    InvalidFractionalDigits(String),
    UnsupportedRadix(u8),
    UnsupportedRadixPrefix(char, String),
    InvalidRadix(String, u8, std::num::ParseIntError),
}
impl fmt::Display for ParseNumberError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseNumberError::*;
        match self {
            InvalidIndexDigits(r)      => write!(f, "Invalid radix: {}", r),
            InvalidFractionalDigits(s) => write!(f, "Invalid fractional digits: {}", s),
            UnsupportedRadix(r)        => write!(f, "Unsupported radix: {}", r),
            UnsupportedRadixPrefix(body, found) => write!(
                f,
                "Unsupported radix prefix. Must be '0{0}'. Found '{1}{0}'",
                body,
                found
            ),
            InvalidRadix(num, r, e) => write!(
                f,
                "'{}' can't be parsed with radix '{}': {}",
                num, r, e
            ),
        }
    }
}
impl Into<ParseErrorKind> for ParseNumberError {
    fn into(self) -> ParseErrorKind {
        ParseErrorKind::BadNumber(self)
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    kind: ParseErrorKind,
    /// A snip of the code that caused this error.
    snip: String,
    line: u32,
    col: u16,
    file: FileRef
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "error[{},{}:{}] {} at code '{}'",
            self.line,
            self.col,
            self.file.s(),
            self.kind,
            self.snip
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StrPart<'a> {
    slice: &'a [char],
}

impl<'a, 'b> PartialEq<&'a str> for StrPart<'a> {
    fn eq(&self, other: &&'a str) -> bool {
        if other.len() != self.slice.len() { return false; }

        let mut sc = other.chars();
        for c in self.slice.iter() {
            if let Some(oc) = sc.next() {
                if *c != oc { return false; }
            } else {
                return false;
            }
        }

        true
    }
}

#[allow(clippy::inherent_to_string)]
impl<'a> StrPart<'a> {
    pub fn to_string(&self) -> String { self.slice.iter().collect() }
    pub fn is_empty(&self) -> bool { self.slice.is_empty() }
    pub fn len(&self) -> usize { self.slice.len() }
    pub fn at(&self, i: usize) -> char { self.slice[i] }
}

#[allow(dead_code)]
impl State {
    pub fn err<E: Into<ParseErrorKind>>(&self, kind: E) -> ParseError {
        ParseError {
            kind: kind.into(),
            snip: self.rest().to_string(),
            line: self.line_no,
            col:  self.col_no,
            file: self.file.clone(),
        }
    }


    /// Creates a `VVal::Syn` annotated with the current parse head position.
    pub fn syn_raw(&self, s: Syntax) -> VVal {
        VVal::Syn(SynPos {
            syn:  s,
            line: self.line_no,
            col:  self.col_no,
            file: self.file.clone(),
            name: None,
        })
    }

    /// Creates an syntactic AST node.
    pub fn syn(&self, s: Syntax) -> VVal {
        let vec = VVal::vec();
        vec.push(self.syn_raw(s));
        vec
    }

    /// Returns the next character under the parse head.
    /// Returns `None` when the parse head is at EOF.
    pub fn peek(&self) -> Option<char> {
        if self.at_end() { None } else { Some(self.input[self.ch_ptr]) }
    }

    /// Returns if the end of the input was reached.
    #[inline]
    pub fn at_end(&self) -> bool { self.ch_ptr >= self.input.len() }

    /// Returns the remaining count of characters in the buffer.
    pub fn rest_len(&self) -> usize { self.input.len() - self.ch_ptr }

    /// Generates a StrPart slice:
    fn spart(&self, a: usize, b: usize) -> StrPart {
        StrPart { slice: &self.input[a..b] }
    }

    /// Generates a StrPart slice:
    fn spart_ptr(&self, offs: usize) -> StrPart {
        StrPart { slice: &self.input[self.ch_ptr..self.ch_ptr + offs] }
    }

    /// Returns the next 2 characters or `None` if EOF.
    pub fn peek2(&self) -> Option<StrPart> {
        if self.rest_len() > 1 {
            Some(self.spart_ptr(2))
        } else {
            None
        }
    }

    /// Returns the next 3 characters or `None` if EOF.
    pub fn peek3(&self) -> Option<StrPart> {
        if self.rest_len() > 2 {
            Some(self.spart_ptr(3))
        } else {
            None
        }
    }

    /// Returns the next 4 characters or `None` if EOF.
    pub fn peek4(&self) -> Option<StrPart> {
        if self.rest_len() > 3 {
            Some(self.spart_ptr(4))
        } else {
            None
        }
    }

    /// Tries to peek for an WLambda operator followed
    /// by the given look-ahead string.
    pub fn peek_op_ws_la(&self, la: &str) -> Option<StrPart> {
        let op = self.peek_op();
        if let Some(op) = op {
            if self.rest_len() >= (op.len() + la.len()) {
                let mut inp_ws_offs = 0;
                for (i, c) in la.chars().enumerate() {
                    let mut inp = self.input[self.ch_ptr + op.len() + i];
                    while inp.is_whitespace() {
                        inp_ws_offs += 1;
                        inp = self.input[self.ch_ptr + op.len() + inp_ws_offs + i];
                    }
                    if c != inp {
                        return None;
                    }
                }
                return Some(op);
            } else {
                return None;
            }
        }
        return op;
    }

    /// Tries to peek for an WLambda operator. Returns `None`
    /// either at EOF or if no operator could be found.
    pub fn peek_op(&self) -> Option<StrPart> {
        if self.at_end() { return None; }
        let ch = self.input[self.ch_ptr];
        match ch {
            '+' | '-'
                => {
                    if let Some(_s) = self.peek2() {
                        if self.rest_len() > 1
                           && self.input[self.ch_ptr + 1].is_digit(10)
                        {
                            return None;
                        }
                    }
                    Some(self.spart_ptr(1))
                },
            '*' | '/' | '%' | '^'
                => {
                    Some(self.spart_ptr(1))
                },
            '<' | '>' | '!' | '=' | '&' => {
                if let Some(s) = self.peek4() {
                    if s == "&and" { return Some(s); }
                }
                if let Some(s) = self.peek3() {
                    if    s == "&or"
                       || s == "&@>"
                       || s == "<@&"
                       { return Some(s); }
                }
                if let Some(s) = self.peek2() {
                    if   s == "<="
                      || s == ">="
                      || s == "!="
                      || s == "=="
                      || s == "<<"
                      || s == ">>"
                      || s == "&|"
                      || s == "&^"
                      || s == "=>"
                      || s == "<&"
                      || s == "&>"
                    { return Some(s); }
                }

                if ch != '=' {
                    Some(self.spart_ptr(1))
                } else {
                    None
                }
            },
            _ => { None }
        }
    }

    /// Returns the rest of the code after the parse head,
    /// including the current character under the parse head.
    pub fn rest(&self) -> StrPart {
        let len = if self.rest_len() > 50 { 50 } else { self.rest_len() };
        self.spart_ptr(len)
    }

    /// Consumes characters while `pred` returns a true value.
    /// Returns `true` if it matched and consumed at least once.
    pub fn consume_while<F>(&mut self, pred: F) -> bool
        where F: Fn(char) -> bool {

        let mut did_match_once = false;
        while let Some(c) = self.peek() {
            if pred(c) { self.consume(); did_match_once = true; }
            else { break; }
        }
        did_match_once
    }

    /// Consumes the `expected_char` and possibly following
    /// white space and comments following it. Returns true if
    /// `expected_char` was found.
    pub fn consume_if_eq_wsc(&mut self, expected_char: char) -> bool {
        let res = self.consume_if_eq(expected_char);
        self.skip_ws_and_comments();
        res
    }

    /// Consumes the `expected_char` and possibly following
    /// white space following it. Returns true if
    /// `expected_char` was found.
    pub fn consume_if_eq_ws(&mut self, expected_char: char) -> bool {
        let res = self.consume_if_eq(expected_char);
        self.skip_ws();
        res
    }

    pub fn consume_if_eq(&mut self, expected_char: char) -> bool {
        if let Some(c) = self.peek() {
            if c == expected_char {
                self.consume();
                return true;
            }
        }
        false
    }

    pub fn take_while_wsc<F>(&mut self, pred: F) -> StrPart
        where F: Fn(char) -> bool {
        let start = self.remember();
        self.take_while(pred);
        let end = self.remember();
        self.skip_ws_and_comments();
        self.spart(start, end)
    }

    pub fn take_while<F>(&mut self, pred: F) -> StrPart
        where F: Fn(char) -> bool {

        let start = self.ch_ptr;
        while let Some(c) = self.peek() {
            if !pred(c) { break; }
            self.consume();
        }
        self.spart(start, self.ch_ptr)
    }

    pub fn indent_pos(&self) -> IndentPos {
        IndentPos {
            line_no:     self.line_no,
            indent:      self.indent,
            line_indent: self.line_indent,
        }
    }

    pub fn last_token_char(&self) -> char {
        self.last_tok_char
    }

    pub fn find_char(&self, c: char) -> Option<usize> {
        let len = self.input.len();
        for i in self.ch_ptr..len {
            if self.input[i] == c {
                return Some(i);
            }
        }

        None
    }

    pub fn find_char_not_of(&self, c: char, not_of: &str) -> Option<usize> {
        let len = self.input.len();
        for i in self.ch_ptr..len {
            for nc in not_of.chars() {
                if self.input[i] == nc {
                    return None;
                }
            }
            if self.input[i] == c {
                return Some(i);
            }
        }

        None
    }

    pub fn consume_lookahead(&mut self, s: &str) -> bool {
        if self.lookahead(s) {
            self.ch_ptr += s.len();
            return true;
        }
        false
    }

    pub fn lookahead_one_of(&self, s: &str) -> bool {
        if self.at_end() { return false; }

        let pc = self.peek().unwrap();
        for c in s.chars() {
            if pc == c { return true; }
        }
        false
    }

    pub fn lookahead(&mut self, s: &str) -> bool {
        if self.rest_len() < s.len() {
            return false;
        }

        for (i, c) in s.chars().enumerate() {
            if self.input[self.ch_ptr + i] != c {
                return false;
            }
        }

        true
    }

    pub fn consume_wsc_n(&mut self, n: usize) {
        for _i in 0..n {
            self.consume();
        }
        self.skip_ws_and_comments();
    }

    pub fn consume_wsc(&mut self) {
        self.consume();
        self.skip_ws_and_comments();
    }

    pub fn consume_ws(&mut self) {
        self.consume();
        self.skip_ws();
    }

    pub fn consume(&mut self) {
        if self.at_end() { return }

        let c = self.peek().unwrap();
        self.col_no = self.col_no.wrapping_add(1);
        if c == '\n' {
            self.line_no += 1;
            self.indent      = Some(0);
            self.line_indent = 0;
            self.col_no      = 1;
        } else if c.is_whitespace() {
            if let Some(i) = self.indent {
                self.line_indent += 1;
                self.indent = Some(i + 1);
            }
        } else {
            self.indent = None;
            self.last_tok_char = c;
        }

        self.ch_ptr += 1;
    }

    pub fn skip_ws(&mut self) {
        self.consume_while(char::is_whitespace);
    }

    pub fn skip_ws_and_comments(&mut self) {
        self.skip_ws();
        while let Some(c) = self.peek() {
            if c == '#' {
                self.consume_while(|c| c != '\n');
                if !self.consume_if_eq('\n') {
                    return;
                }
                self.skip_ws();
            } else {
                break;
            }
        }
    }

    /// The constructor for the `parser::State`.
    ///
    /// ```rust
    /// use wlambda::parser::State;
    ///
    /// let code    = "{ 123 }";
    /// let mut ps  = State::new(code, "filenamehere");
    /// // ...
    /// wlambda::parser::parse_block(&mut ps, true);
    /// // ...
    /// ```
    pub fn new(code: &str, filename: &str) -> State {
        let mut ps = State {
            input:          code.chars().collect(),
            ch_ptr:         0,
            line_no:        1,
            col_no:         1,
            indent:         Some(0),
            line_indent:    0,
            last_tok_char:  ' ',
            file:           FileRef::new(filename),
        };
        ps.skip_ws_and_comments();
        ps
    }

    pub fn expect_some<T>(&self, o: Option<T>) -> Result<T, ParseError> {
        match o {
            None => Err(self.err(ParseErrorKind::EOF("Unexpected EOF"))),
            Some(r) => Ok(r)
        }
    }

    pub fn remember(&mut self) -> usize {
        self.ch_ptr
    }

    pub fn collect(&mut self, start: usize, end: usize) -> StrPart {
        self.spart(start, end)
    }
}
