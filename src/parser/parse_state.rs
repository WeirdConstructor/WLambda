// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::SynPos;
use std::fmt::{Display, Formatter};

/// This is the parser state data structure. It holds the to be read source
/// code and keeps track of the parser head position.
///
/// Can be created using `parser::State::new`:
///
/// ```rust
/// let code    = "{ 123 }";
/// let file_no = 400;
/// let mut ps  = State::new(code, file_no);
///
/// // ...
/// ```
#[allow(dead_code)]
pub struct State {
        chars:      Vec<char>,
        peek_char:  char,
        line_no:    u32,
        col_no:     u32,
        file_no:    u32,
    pub at_eof:     bool,
}

/// The possible errors the parser can detect while reading code.
///
/// The errors all take a tuple of 5 elements, with the following
/// semantics:
///
///     (<error message>,
///      <snipped of the following code>,
///      <line number>,
///      <column number>,
///      <file number>)
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken((String, String, u32, u32, u32)),
    BadEscape(      (String, String, u32, u32, u32)),
    BadValue(       (String, String, u32, u32, u32)),
    BadNumber(      (String, String, u32, u32, u32)),
    BadCall(        (String, String, u32, u32, u32)),
    EOF(            (String, String, u32, u32, u32)),
}

fn tuple2str(tp: &(String, String, u32, u32, u32)) -> String {
    format!("error[{}:{}] {} at code '{}'", tp.2, tp.3, tp.0, tp.1)
}

pub fn parse_error_to_string(pe: &ParseError) -> String {
    match pe {
        ParseError::UnexpectedToken(t) => tuple2str(t),
        ParseError::BadEscape(t)       => tuple2str(t),
        ParseError::BadValue(t)        => tuple2str(t),
        ParseError::BadNumber(t)       => tuple2str(t),
        ParseError::BadCall(t)         => tuple2str(t),
        ParseError::EOF(t)             => tuple2str(t),
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", parse_error_to_string(self))
    }
}

#[allow(dead_code)]
impl State {
    pub fn err_unexpected_token(&self, c: char, s: &str) -> Result<VVal,ParseError> {
        Err(ParseError::UnexpectedToken((format!("Unexpected token '{}'. {}", c, s), self.rest(), self.line_no, self.col_no, self.file_no)))
    }

    pub fn err_bad_value(&self, s: &str) -> Result<VVal,ParseError> {
        Err(ParseError::BadValue((String::from(s), self.rest(), self.line_no, self.col_no, self.file_no)))
    }

    pub fn err_bad_number(&self, s: &str) -> Result<VVal,ParseError> {
        Err(ParseError::BadNumber((String::from(s), self.rest(), self.line_no, self.col_no, self.file_no)))
    }

    pub fn err_bad_call(&self, s: &str) -> Result<VVal,ParseError> {
        Err(ParseError::BadCall((String::from(s), self.rest(), self.line_no, self.col_no, self.file_no)))
    }

    pub fn err_eof(&self, s: &str) -> Result<VVal,ParseError> {
        Err(ParseError::EOF((format!("EOF while parsing {}", s), self.rest(), self.line_no, self.col_no, self.file_no)))
    }

    pub fn err_bad_escape(&self, s: &str) -> Result<VVal,ParseError> {
        Err(ParseError::BadEscape((String::from(s), self.rest(), self.line_no, self.col_no, self.file_no)))
    }

    /// Creates a `VVal::Syn` annotated with the current parse head position.
    pub fn syn_raw(&self, s: Syntax) -> VVal {
        VVal::Syn(SynPos {
            syn:  s,
            line: self.line_no,
            col:  self.col_no,
            file: self.file_no
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
            '+' | '-' | '*' | '/' | '%' | '^'
                => { return Some(self.peek_char.to_string()); },
            '<' | '>' | '!' | '=' | '|' | '&' => {
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
                if self.peek_char != '=' && self.peek_char != '|' {
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
            if self.chars.len() > 0 {
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
        return false;
    }

    pub fn lookahead(&mut self, s: &str) -> bool {
        if self.chars.len() < s.len() {
            return false;
        }

        let mut i = 0;
        for c in s.chars() {
            if self.chars[i] != c {
                return false;
            }
            i = i + 1;
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
            self.line_no = self.line_no + 1;
            self.col_no = 1;
        }

        if self.chars.len() > 0 {
            self.chars.remove(0);
        }

        if self.chars.len() > 0 {
            self.peek_char = self.chars[0];
        } else {
            self.at_eof = true;
        }
    }

    fn skip_ws(&mut self) {
        self.consume_while(|c| c.is_whitespace());
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
        if self.chars.len() > 0 {
            self.peek_char = self.chars[0];
        } else {
            self.at_eof = true;
        }
    }

    /// The constructor for the `parser::State`.
    ///
    /// Instead of giving the filename you should provide the
    /// `file_no`. It's up to you to keep track of the
    /// mapping from `file_no` to an input file or input source.
    /// That is, because WLambda might not come from files but
    /// from other sources, that I can not anticipate here.
    ///
    /// ```rust
    /// let code    = "{ 123 }";
    /// let file_no = 400;
    /// let mut ps  = parser::State::new(code, file_no);
    /// // ...
    /// wlambda::parser::parse_block(&mut ps, true);
    /// // ...
    /// ```
    pub fn new(code: &str, file_no: u32) -> State {
        let mut ps = State {
            chars:     code.chars().collect(),
            peek_char: ' ',
            at_eof:    false,
            line_no:   1,
            col_no:    1,
            file_no:   file_no,
        };
        ps.init();
        ps.skip_ws_and_comments();
        ps
    }
}

