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
        chars:      Vec<char>,
        peek_char:  char,
        line_no:    u32,
        col_no:     u32,
        file:       FileRef,
    pub at_eof:     bool,
}

/// The possible errors the parser can detect while reading code.
#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken(char, &'static str),
    BadEscape(&'static str),
    BadValue(ParseValueError),
    BadKeyword(String, &'static str),
    BadNumber(ParseNumberError),
    //BadCall(&'static str),
    EOF(&'static str),
}
impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseErrorKind::*;
        match self {
            UnexpectedToken(c, s) => write!(f, "Unexpected token '{}'. {}", c, s),
            BadEscape(s)          => write!(f, "{}", s),
            BadValue(s)           => write!(f, "{}", s),
            BadKeyword(kw, s)     => write!(f, "Got '{}', expected {}", kw, s),
            BadNumber(s)          => write!(f, "{}", s),
            //BadCall(s)            => write!(f, "{}", s),
            EOF(s)                => write!(f, "EOF while parsing {}", s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseValueError {
    UnknownSpecialIdentifier(char),
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
    col: u32,
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

#[allow(dead_code)]
impl State {
    pub fn err<E: Into<ParseErrorKind>>(&self, kind: E) -> ParseError {
        ParseError {
            kind: kind.into(),
            snip: self.rest(),
            line: self.line_no,
            col: self.col_no,
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
        if self.at_eof { None } else { Some(self.peek_char) }
    }

    /// Returns the next 2 characters or `None` if EOF.
    pub fn peek2(&self) -> Option<String> {
        if self.chars.len() > 1 {
            let s : String = self.chars[0..2].iter().collect();
            Some(s)
        } else {
            None
        }
    }

    /// Returns the next 3 characters or `None` if EOF.
    pub fn peek3(&self) -> Option<String> {
        if self.chars.len() > 2 {
            let s : String = self.chars[0..3].iter().collect();
            Some(s)
        } else {
            None
        }
    }

    /// Returns the next 4 characters or `None` if EOF.
    pub fn peek4(&self) -> Option<String> {
        if self.chars.len() > 3 {
            let s : String = self.chars[0..4].iter().collect();
            Some(s)
        } else {
            None
        }
    }

    /// Tries to peek for an WLambda operator. Returns `None`
    /// either at EOF or if no operator could be found.
    pub fn peek_op(&self) -> Option<String> {
        if self.at_eof { return None; }
        match self.peek_char {
            '+' | '-'
                => {
                    if let Some(s) = self.peek2() {
                        if s.chars().nth(1).unwrap_or(' ').is_digit(10) {
                            return None;
                        }
                    }
                    Some(self.peek_char.to_string())
                },
            '*' | '/' | '%' | '^'
                => { Some(self.peek_char.to_string()) },
            '<' | '>' | '!' | '=' | '&' => {
                if let Some(s) = self.peek4() {
                    if s == "&and" { return Some(s); }
                }
                if let Some(s) = self.peek3() {
                    if s == "&or" { return Some(s); }
                }
                if let Some(s) = self.peek2() {
                    match &s[0..2] {
                          "<=" | ">=" | "!=" | "==" | "<<" | ">>"
                        | "&|" | "&^" => { return Some(s); }
                        _ => { }
                    }
                }
                if self.peek_char != '=' {
                    Some(self.peek_char.to_string())
                } else {
                    None
                }
            },
            _ => { None }
        }
    }

    /// Returns the rest of the code after the parse head,
    /// including the current character under the parse head.
    pub fn rest(&self) -> String {
        let s : String = self.chars.iter().collect();
        let len = if s.len() > 50 { 50 } else { s.len() };
        String::from(&s[0..len])
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

    pub fn consume_if_eq(&mut self, expected_char: char) -> bool {
        if let Some(c) = self.peek() {
            if c == expected_char {
                self.consume();
                return true;
            }
        }
        false
    }

    pub fn take_while_wsc<F>(&mut self, pred: F) -> Vec<char>
        where F: Fn(char) -> bool {
        let ret = self.take_while(pred);
        self.skip_ws_and_comments();
        ret
    }

    pub fn take_while<F>(&mut self, pred: F) -> Vec<char>
        where F: Fn(char) -> bool {

        let mut ret = Vec::new();
        while let Some(c) = self.peek() {
            if !pred(c) { break; }
            ret.push(c);
            self.consume();
        }
        ret
    }

    pub fn consume_lookahead(&mut self, s: &str) -> bool {
        if self.lookahead(s) {
            for _ in s.chars() { self.chars.remove(0); }
            if !self.chars.is_empty() {
                self.peek_char = self.chars[0];
            } else {
                self.peek_char = ' ';
                self.at_eof = true;
            }
            return true;
        }
        false
    }

    pub fn lookahead_one_of(&self, s: &str) -> bool {
        if self.at_eof { return false; }

        for c in s.chars() {
            if self.peek_char == c {
                return true;
            }
        }
        false
    }

    pub fn lookahead(&mut self, s: &str) -> bool {
        if self.chars.len() < s.len() {
            return false;
        }

        for (i, c) in s.chars().enumerate() {
            if self.chars[i] != c {
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

    pub fn consume(&mut self) {
        if self.at_eof { return }

        let c = self.peek_char;
        self.col_no += 1;
        if c == '\n' {
            self.line_no += 1;
            self.col_no = 1;
        }

        if !self.chars.is_empty() {
            self.chars.remove(0);
        }

        if !self.chars.is_empty() {
            self.peek_char = self.chars[0];
        } else {
            self.at_eof = true;
        }
    }

    fn skip_ws(&mut self) {
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

    fn init(&mut self) {
        if !self.chars.is_empty() {
            self.peek_char = self.chars[0];
        } else {
            self.at_eof = true;
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
            chars:     code.chars().collect(),
            peek_char: ' ',
            at_eof:    false,
            line_no:   1,
            col_no:    1,
            file:      FileRef::new(filename),
        };
        ps.init();
        ps.skip_ws_and_comments();
        ps
    }

    pub fn expect_some<T>(&self, o: Option<T>) -> Result<T, ParseError> {
        match o {
            None => Err(self.err(ParseErrorKind::EOF("Unexpected EOF"))),
            Some(r) => Ok(r)
        }
    }
}

