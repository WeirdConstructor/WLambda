// Copyright (c) 2019 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

use crate::vval::VVal;
use crate::vval::Syntax;
use crate::vval::SynPos;

/*

Bukclo

vector      := '$[' expr (',' expr)* ','? ']'
map         := '${' expr ':' expr (',' expr ':' expr)* ','? '}'
number      := float | int
string      := '"' ... '"'
none        := "$none" | "$n"
bool        := "$t" | "$f" | "$true" | "$false"
native_keys := "$if" | "$while" | "$break" | "$for"
key         := ':' identifier
primitive   := vector | map | number | key | string | none | bool
var         := identifier

block       := '{' stmt* '}'
func        := block
value       := primitive | func | var | '[' expr ']'
field       := value | identifier
arglist     := '(' (expr (',' expr)*)?)? ')'
field-acc   := value ('.' field)+          // field access
             | value ('.' field)+ '=' expr // assignment
             | value ('.' field)+ arglist  // method/field call
call        := call arglist
             | call '~' expr
             | field-acc
             | call (binop call)+ // where call is limited to arglist and field-acc
             | call call+ // field-acc and others have priority
             | value call+ // this is then always
expr        := call ('|' expr)*
             | value
def         := '!' key? assignment
assignment  := '.' identifier '=' expr
             | '.'? '(' (identifier (',' identifier)*)? ')' = expr
stmt        := def ';'
             | assignment ';'
             | expr ';'
             | ';'

special variables:
    arguments are in '@', first one is in '_' and _1
    others: _1 up to _9

special functions:
    range   - returning a range iterator
    iter    - called on vec/map it returns a collection iterator
    apply fn arg-list - calls fn with arguments from arg-list
                        makes stuff like `apply bla @` possible

pipe:

    fn a b | fn_b c | fn_c
    =>
    fn_c(fn_b (fn a b) c)

    allows:
        iter x | map { * _ 2 } | filter { even? _ }

    => filter({ even? _ }, map({ * _ 2 }, iter(x)))

tilde:

    fn a b ~ fn_b c ~ fn_c

    { * _ 2 } ~ { - _ 2 } ~ { pow _ 10 } ~ 20
    =>
    { * _ 2 }({ - _ 2 }({ pow _ 10 } 20))



dotcall:

    a.b c d
    =>
    [b a] c d

what calling means for primitive types:

    number  => access index in first arg
    key     => access field in first arg
    vec     => call first arg for every element
    map     => call first arg for every kv pair
    #true   => call first arg
    #false  => call second arg
    string  => call first arg for every character

=>

    let x = 0

    while { < x 10 } {
        x = + x 1;
        [== 0 $ % x 2] { break }
    }

    [range 0 10 1] {
        print "foo {}" _;
    }

    let doit = {
       let (a, b) = @;
       assert = _ a;
    }


Thoughts about cyclic referencing

    let new = {
        ! self = ${ // self on stack
            x: 10
        };
        ! y = $[1,2,3]; // y on stack
        ! :ref yhard = $[1,2,3]; // yhard puts upvalue on stack, any closure captures the upvalue by value so it references it strongly

        self.foo = {
            # think of this as:
            # let self  = shallow copy of outer_self;
            # let y     = shallow copy of outer_y;
            # yhard references yhard up value;

            # or: set! self :x ~ + 1 ~ :x self
            # or: mut! self :x { + _ 1 }; // self captures by value
            self->x = + self->x 1;
            y     = + y 1;      // y captured by value locally no change to outer y
            yhard = + yhard 1;  // y referenced strong now!
        };

        self.bar = {
            + [0 y] [0 yhard]
            # or:
            +(y->0, yhard->0)
            # or:
            + ~ y->0 ~ yhard->0
        }

        self
    }

    let obj = new(); // self is on stack here
    obj.foo;

Callable objects:

    !my_cond = {
        let :ref self = ${ inner_val: #t };
        { apply self->inner_val @ }
    }()

    my_cond { # if
        println "INNER VALUE IS TRUE!"
    } { # else
        println "INNER VALUE IS FALSE!"
    }

Prototyped inheritance:

    !proto = ${ print: { println _ }, };
    !o = to_obj { _proto_: proto };
    o.print(123);

    # MetaMap(Rc<RefCell<std::collections::HashMap<String, VVal>>>),
    # => invokes _proto_ lookup on field access (not write)

Tagged values:
    !tag = 123;
    !v = tag 10 tag;
    !fun = { println("not tagged!") };
    .fun = add_tag fun tag { println("tagged with 123"); }
    fun(v); # prints "tagged with 123"
    fun(10); # prints "not tagged!"

    # TagFun(Rc<RefCell<std::collections::HashMap<String, Rc<VValFun>>>>),
*/

#[allow(dead_code)]
pub struct ParseState {
//    contents:   String,
    chars:      Vec<char>,
    peek_char:  char,
    line_no:    u32,
    col_no:     u32,
    file_no:    u32,
    at_eof:     bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken((String, String, u32, u32, u32)),
    BadEscape(      (String, String, u32, u32, u32)),
    BadValue(       (String, String, u32, u32, u32)),
    BadNumber(      (String, String, u32, u32, u32)),
    EOF(            (String, String, u32, u32, u32)),
    BadCall(        (String, String, u32, u32, u32)),
}

#[allow(dead_code)]
impl ParseState {
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

    pub fn syn_raw(&self, s: Syntax) -> VVal {
        VVal::Syn(SynPos {
            syn:  s,
            line: self.line_no,
            col:  self.col_no,
            file: self.file_no
        })
    }

    pub fn syn(&self, s: Syntax) -> VVal {
        let vec = VVal::vec();
        vec.push(self.syn_raw(s));
        vec
    }

    pub fn peek(&self) -> Option<char> { if self.at_eof { None } else { Some(self.peek_char) } }

    pub fn peek3(&self) -> Option<String> {
        if self.chars.len() > 2 {
            let s : String = self.chars[0..3].iter().collect();
            Some(s)
        } else {
            None
        }
    }

    pub fn peek4(&self) -> Option<String> {
        if self.chars.len() > 3 {
            let s : String = self.chars[0..4].iter().collect();
            Some(s)
        } else {
            None
        }
    }

    pub fn peek2(&self) -> Option<String> {
        if self.chars.len() > 1 {
            let s : String = self.chars[0..2].iter().collect();
            Some(s)
        } else {
            None
        }
    }

    pub fn peek_op(&self) -> Option<String> {
        if self.at_eof { return None; }
        match self.peek_char {
            'a' => {
                if let Some(s) = self.peek3() {
                    if s == "and" { return Some(s); }
                }
                None
            },
            'o' => {
                if let Some(s) = self.peek2() {
                    if s == "or" { return Some(s); }
                }
                None
            },
            '+' | '-' | '*' | '/' | '%' | '^'
                => { return Some(self.peek_char.to_string()); },
            '<' | '>' | '!' | '=' | '|' | '&' => {
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

    pub fn rest(&self) -> String {
        let s : String = self.chars.iter().collect();
        let len = if s.len() > 50 { 50 } else { s.len() };
        String::from(&s[0..len])
    }

    pub fn expect_char(&mut self, expected_char: char) -> bool {
        if let Some(c) = self.peek() {
            if c == expected_char {
                self.consume();
                return true;
            }
        }

        false
    }

    pub fn consume_while<F>(&mut self, pred: F) -> bool
        where F: Fn(char) -> bool {

        let mut did_match_once = false;
        while let Some(c) = self.peek() {
            if pred(c) { self.consume(); did_match_once = true; }
            else { break; }
        }
        did_match_once
    }

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

    pub fn skip_ws(&mut self) {
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

    pub fn init(&mut self) {
        if self.chars.len() > 0 {
            self.peek_char = self.chars[0];
        } else {
            self.at_eof = true;
        }
    }

    pub fn new(content: &str, file_no: u32) -> ParseState {
        let mut ps = ParseState {
            chars:     content.chars().collect(),
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

fn add_c_to_vec(v: &mut Vec<u8>, c: char) {
    if c.is_ascii() {
        v.push((c as u32) as u8);
    } else {
        let mut b = [0; 4];
        for cb in c.encode_utf8(&mut b).as_bytes().iter() {
            v.push(*cb);
        }
    }
}

fn adchr(v: &mut Vec<u8>, s: &mut String, b: bool, c: char) {
    if b { add_c_to_vec(v, c); }
    else { s.push(c); }
}

//pub fn read_int(it: &mut TE) {
////    let k = it.peek().unwrap();
////    println!("FO: {:?}", k);
//}
pub fn parse_string(ps: &mut ParseState, byte_str: bool) -> Result<VVal, ParseError> {
    if ps.at_eof { return ps.err_eof("string"); }

    ps.consume_if_eq('"');

    let vec = ps.syn(Syntax::Str);

    let mut s = String::from("");
    let mut v : Vec<u8> = Vec::new();

    while ps.peek().unwrap_or('"') != '"' {
        let c = ps.peek().unwrap();
        match c {
            '\\' => {
                ps.consume();
                if let Some(c) = ps.peek() {
                    match c {
                        'x' => {
                            ps.consume();
                            let hex = ps.peek2();
                            if let Some(h) = hex {
                                ps.consume();
                                ps.consume();
                                if let Ok(cn) = u8::from_str_radix(&h, 16) {
                                    if byte_str { v.push(cn) }
                                    else { s.push(cn as char); }
                                } else {
                                    return ps.err_bad_escape("Bad hex escape in string");
                                }
                            } else {
                                return ps.err_eof("string hex escape");
                            }
                        },
                        'n'  => { ps.consume(); adchr(&mut v, &mut s, byte_str, '\n'); },
                        'r'  => { ps.consume(); adchr(&mut v, &mut s, byte_str, '\r'); },
                        't'  => { ps.consume(); adchr(&mut v, &mut s, byte_str, '\t'); },
                        '\\' => { ps.consume(); adchr(&mut v, &mut s, byte_str, '\\'); },
                        '0'  => { ps.consume(); adchr(&mut v, &mut s, byte_str, '\0'); },
                        '\'' => { ps.consume(); adchr(&mut v, &mut s, byte_str, '\''); },
                        '"'  => { ps.consume(); adchr(&mut v, &mut s, byte_str, '"'); },
                        'u' => {
                            ps.consume();
                            if !ps.consume_if_eq('{') {
                                return ps.err_unexpected_token('{', "After unicode escape.");
                            }

                            let uh : String = ps.take_while(|c| c.is_digit(16)).iter().collect();

                            if let Ok(cn) = u32::from_str_radix(&uh, 16) {
                                if let Some(c) = std::char::from_u32(cn) {
                                    adchr(&mut v, &mut s, byte_str, c);
                                } else {
                                    return ps.err_bad_escape(
                                        "Bad char in unicode escape in string");
                                }
                            } else {
                                return ps.err_bad_escape(
                                    "Bad unicode hex escape in string");
                            }

                            if !ps.consume_if_eq('}') {
                                return ps.err_unexpected_token('}', "After unicode escape.");
                            }
                        },
                        c => { ps.consume(); adchr(&mut v, &mut s, byte_str, c); },
                    }
                } else {
                    return ps.err_eof("string escape");
                }
            },
            _ => {
                ps.consume();
                adchr(&mut v, &mut s, byte_str, c);
            },
        }
    }

    if byte_str {
        vec.push(VVal::new_byt(v));
    } else {
        vec.push(VVal::new_str(&s));
    }

    if !ps.consume_if_eq('"') {
        return ps.err_unexpected_token('\"', "");
    }

    Ok(vec)
}

pub fn parse_num(ps: &mut ParseState) -> Result<VVal, ParseError> {
    if ps.at_eof { return ps.err_eof("number"); }

    let c = ps.peek().unwrap();
    let sign = match c {
        '-' => {
            ps.consume();
            if !ps.peek().unwrap_or(' ').is_digit(10) {
                ps.skip_ws_and_comments();
                return Ok(make_var(ps, "-"));
            }
            -1
        },
        '+' => {
            ps.consume();
            if !ps.peek().unwrap_or(' ').is_digit(10) {
                ps.skip_ws_and_comments();
                return Ok(make_var(ps, "+"));
            }
            1
        },
        _   => 1
    };

    let radix_or_num : String = ps.take_while(|c| c.is_digit(10)).iter().collect();

    let (radix, num) = if ps.consume_if_eq('r') {
        let radix = if let Ok(r) = u8::from_str_radix(&radix_or_num, 10) {
            r
        } else {
            10
        };

        if radix < 2 || radix > 36 {
            return ps.err_bad_number(&format!("Unsupported radix: {}", radix));
        }

        (radix, ps.take_while(|c| c.is_digit(radix as u32)).iter().collect())
    } else if ps.consume_if_eq('x') {
        if radix_or_num != "0" {
            return ps.err_bad_number(&format!("Unsupported radix prefix. \
                                Must be '0x'. Found '{}x'", radix_or_num));
        }
        (16, ps.take_while(|c| c.is_digit(16)).iter().collect())
    } else if ps.consume_if_eq('b') {
        if radix_or_num != "0" {
            return ps.err_bad_number(&format!("Unsupported radix prefix. \
                                Must be '0b'. Found '{}b'", radix_or_num));
        }
        (2, ps.take_while(|c| c.is_digit(2)).iter().collect())
    } else if ps.consume_if_eq('o') {
        if radix_or_num != "0" {
            return ps.err_bad_number(&format!("Unsupported radix prefix. \
                                Must be '0o'. Found '{}o'", radix_or_num));
        }
        (8, ps.take_while(|c| c.is_digit(8)).iter().collect())
    } else {
        (10, radix_or_num)
    };

    let (is_float, fract_num) = if ps.consume_if_eq('.') {
        let fract_digits : String = ps.take_while(|c| c.is_digit(radix as u32)).iter().collect();
        if let Ok(fract_num) = u64::from_str_radix(&fract_digits, radix as u32) {
            (true, (fract_num as f64) / (radix as f64).powf(fract_digits.len() as f64))
        } else {
            return ps.err_bad_number(&format!("Invalid fractional digits {}", fract_digits));
        }
    } else {
        (false, 0.0)
    };

    ps.skip_ws_and_comments();

    match u64::from_str_radix(&num, radix as u32) {
        Ok(num) => {
            if is_float {
                if sign == -1 {
                    Ok(VVal::Flt(-((num as f64) + fract_num)))
                } else {
                    Ok(VVal::Flt((num as f64)   + fract_num))
                }
            } else {
                if sign == -1 {
                    Ok(VVal::Int(-(num as i64)))
                } else {
                    Ok(VVal::Int(num as i64))
                }
            }
        },
        _ => ps.err_bad_number(&format!(
                "Couldn't parse number '{}' with radix={}", num, radix)),
    }
}

fn parse_vec(ps: &mut ParseState) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_wsc('[') {
        return ps.err_unexpected_token('[', "At vector.");
    }

    let vec = ps.syn(Syntax::Lst);

    while ps.peek().unwrap() != ']' {
        let atom = parse_expr(ps)?;
        vec.push(atom);
        if !ps.consume_if_eq_wsc(',') { break; }
    }

    if !ps.consume_if_eq_wsc(']') {
        return ps.err_unexpected_token(']', "At the end of vector");
    }

    Ok(vec)
}

fn parse_map(ps: &mut ParseState) -> Result<VVal, ParseError> {
    //println!("parse_map [{}]", ps.rest());
    if !ps.consume_if_eq_wsc('{') {
        return ps.err_unexpected_token('{', "At map");
    }

    let map = ps.syn(Syntax::Map);

    while ps.peek().unwrap() != '}' {
        let key = parse_expr(ps)?;
        if !ps.consume_if_eq_wsc(':') {
            return ps.err_unexpected_token(':', "After reading map key");
        }
        let value = parse_expr(ps)?;

        let elem = VVal::vec();
        elem.push(key);
        elem.push(value);
        map.push(elem);

        if !ps.consume_if_eq_wsc(',') { break; }
    }

    if !ps.consume_if_eq_wsc('}') {
        return ps.err_unexpected_token('}', "At the end of a map");
    }

    Ok(map)
}


fn parse_literal(ps: &mut ParseState) -> Result<VVal, ParseError> {
    if ps.at_eof { return ps.err_eof("literal value"); }
    let c = ps.peek().unwrap();

    match c {
        'b' => { ps.consume(); parse_string(ps, true) },
        '[' => parse_vec(ps),
        '{' => parse_map(ps),
        'n' => {
            if ps.consume_lookahead("none") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(VVal::Nul)
        },
        't' => {
            if ps.consume_lookahead("true") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(VVal::Bol(true))
        },
        'f' => {
            if ps.consume_lookahead("false") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(VVal::Bol(false))
        },
        _   => Ok(VVal::Flt(0.2)),
    }
}

#[allow(dead_code)]
fn is_var(expr: &VVal) -> bool {
    if let Some(ea) = expr.at(0) {
        if let VVal::Syn(s) = ea {
            return s.syn == Syntax::Var;
        }
    }
    return false;
}

fn is_call(expr: &VVal) -> bool {
    if let Some(ea) = expr.at(0) {
        if let VVal::Syn(s) = ea {
            return s.syn == Syntax::Call;
        }
    }
    return false;
}

fn make_to_call(ps: &ParseState, expr: VVal) -> VVal {
    let call = ps.syn(Syntax::Call);
    call.push(expr);
    call
}

fn make_var(ps: &ParseState, identifier: &str) -> VVal {
    let id = ps.syn(Syntax::Var);
    id.push(VVal::Sym(String::from(identifier)));
    return id;
}

fn make_key(ps: &ParseState, identifier: &str) -> VVal {
    let id = ps.syn(Syntax::Key);
    id.push(VVal::Sym(String::from(identifier)));
    return id;
}

fn make_binop(ps: &ParseState, op: &str, left: VVal, right: VVal) -> VVal {
    let call = make_to_call(ps, make_var(ps, op));
    call.push(left);
    call.push(right);
    return call;
}

pub fn parse_identifier(ps: &mut ParseState) -> String {
    let identifier : String =
        ps.take_while_wsc(|c| {
            match c {
               '.' | ',' | ':' | ';' | '{' | '}'
             | '[' | ']' | '(' | ')' | '~' | '|' | '='
                    => false,
                _   => !c.is_whitespace()
            }
        }).iter().collect();
    identifier
}

pub fn parse_value(ps: &mut ParseState) -> Result<VVal, ParseError> {
    //println!("parse_value [{}]", ps.rest());
    if let Some(c) = ps.peek() {
        match c {
            '0' ... '9' | '+' | '-' => parse_num(ps),
            '"' => parse_string(ps, false),
            '$' => { ps.consume_wsc(); parse_literal(ps) },
            '[' => {
                ps.consume_wsc();
                let expr = parse_expr(ps)?;
                if !ps.consume_if_eq_wsc(']') {
                    return ps.err_unexpected_token(']', "In sub expression.");
                }
                Ok(expr)
            },
            '{' => {
                let block = parse_block(ps, true)?;
                block.set_at(0, ps.syn_raw(Syntax::Func));
                Ok(block)
            },
            ':' => {
                ps.consume_wsc();
                let id = parse_identifier(ps);
                Ok(make_key(ps, &id))
            },
            _ if c.is_alphanumeric() || c == '_' || c == '@' => {
                let id = parse_identifier(ps);
                Ok(make_var(ps, &id))
            },
            _ => {
                ps.err_bad_value("Expected literal value, sub \
                                 expression, block, key or identifier.")
            }
        }
    } else {
        ps.err_eof("value.")
    }
}

pub fn parse_field_access(obj_val: VVal, ps: &mut ParseState) -> Result<VVal, ParseError> {
    let mut obj = obj_val;

    while let Some(c) = ps.peek() {
        if c != '.' { break; }

        ps.consume_wsc();
        let value = parse_value(ps)?;
        if let Some(ea) = value.at(0) {
            if let VVal::Syn(s) = ea {
                if s.syn == Syntax::Var {
                    value.set_at(0, ps.syn_raw(Syntax::Key));
                }
            }
        }

        if let Some(c) = ps.peek() {
            match c {
                '=' => {
                    ps.consume_wsc();
                    let field_set = ps.syn(Syntax::SetKey);
                    field_set.push(obj);
                    field_set.push(value);
                    field_set.push(parse_expr(ps)?);
                    return Ok(field_set);
                },
                '(' => {
                    let call = make_to_call(ps, value);
                    call.push(obj);
                    let mut field_call = make_to_call(ps, call);
                    match parse_arg_list(&mut field_call, ps) {
                        Ok(_)    => return Ok(field_call),
                        Err(err) => return Err(err),
                    }
                },
                _ => {
                    let call = make_to_call(ps, value);
                    call.push(obj);
                    obj = call;
                }
            }
        } else {
            let call = make_to_call(ps, value);
            call.push(obj);
            obj = call;
        }
    }

    Ok(obj)
}

pub fn parse_arg_list<'a>(call: &'a mut VVal, ps: &mut ParseState) -> Result<&'a mut VVal, ParseError> {
    if !ps.consume_if_eq_wsc('(') {
        return Err(ps.err_unexpected_token('(', "At start of call arguments.").unwrap_err());
    }

    while let Some(c) = ps.peek() {
        if c == ')' { break; }

        let call_arg = parse_expr(ps)?;
        call.push(call_arg);

        if !ps.consume_if_eq_wsc(',') {
            break;
        }
    }

    if ps.at_eof {
        return Err(ps.err_eof("call args").unwrap_err());
    }
    if !ps.consume_if_eq_wsc(')') {
        return Err(ps.err_unexpected_token(')',
            "While reading call arguments").unwrap_err());
    }

    return Ok(call);
}

pub fn get_op_prec(op: &str) -> i32 {
    match op {
        "^"                         => 15,
        "*"  | "/" | "%"            => 14,
        "-"  | "+"                  => 13,
        "<<" | ">>"                 => 12,
        "<"  | ">" | "<=" | ">="    => 11,
        "==" | "!="                 => 10,
        "&"                         => 9,
        "&^"                        => 8,
        "&|"                        => 7,
        "and"                       => 6,
        "or"                        => 5,
        _                           => 0
    }
}

pub fn parse_binop(mut left: VVal, ps: &mut ParseState, op: &str) -> Result<VVal, ParseError> {
    let prec = get_op_prec(op);
    let mut right = parse_call_expr(ps, true, true)?;

    while let Some(next_op) = ps.peek_op() {
        if next_op.len() == 2 {
            ps.consume_wsc();
            ps.consume_wsc();
        } else if next_op.len() == 3 {
            ps.consume_wsc();
            ps.consume_wsc();
            ps.consume_wsc();
        } else {
            ps.consume_wsc();
        }

        let next_prec = get_op_prec(&next_op);
        if prec < next_prec {
            right = parse_binop(right, ps, &next_op)?;
        } else {
            left = make_binop(ps, &op, left, right);
            return parse_binop(left, ps, &next_op);
        }
    }

    return Ok(make_binop(ps, op, left, right));
}

pub fn parse_call(mut value: VVal, ps: &mut ParseState, binop_mode: bool) -> Result<VVal, ParseError> {
    //println!("parse_call [{}]", ps.rest());
    let mut res_call = VVal::Nul;

    while let Some(c) = ps.peek() {
        //println!("PC c={}", c);
        let op = ps.peek_op();
        match c {
            '(' => {
                let mut call = make_to_call(ps, value);
                match parse_arg_list(&mut call, ps) {
                    Ok(_)    => { value = call; },
                    Err(err) => return Err(err),
                }
            },
            '.' => {
                value = parse_field_access(value, ps)?;
            },
            '~' => {
                ps.consume_wsc();
                if let VVal::Nul = res_call { res_call = make_to_call(ps, value); }
                else { res_call.push(value); }
                res_call.push(parse_expr(ps)?);
                // We don't set value here, because it will not be
                // used by '(' or '.' cases anymore!
                // Those will be covered by parse_expr() presumably.
                return Ok(res_call);
            },
            ';' | ')' | ',' | ']' | '|' | '}' | ':' => {
                break;
            },
            _ if op.is_some() => {
                if binop_mode { break; }
                let op = op.unwrap();
                if op.len() == 2 {
                    ps.consume_wsc();
                    ps.consume_wsc();
                } else if op.len() == 3 {
                    ps.consume_wsc();
                    ps.consume_wsc();
                    ps.consume_wsc();
                } else {
                    ps.consume_wsc();
                }
                value = parse_binop(value, ps, &op)?;
            },
            '=' => {
                return ps.err_bad_call("Unexpected '='");
            },
            _ => {
                if binop_mode { break; }

                if let VVal::Nul = res_call { res_call = make_to_call(ps, value); }
                else { res_call.push(value); }
                value = parse_value(ps)?;
            },
        }
    }

    if let VVal::Nul = res_call {
        res_call = value;
    } else {
        res_call.push(value);
    }

    Ok(res_call)
}

pub fn parse_expr(ps: &mut ParseState) -> Result<VVal, ParseError> {
    return parse_call_expr(ps, false, false);
}

pub fn parse_call_expr(ps: &mut ParseState, no_pipe: bool, binop_mode: bool) -> Result<VVal, ParseError> {
    //println!("parse_expr [{}] np={}", ps.rest(), no_pipe);
    let value = parse_value(ps)?;

    // look ahead, if we see an expression delimiter.
    // because then, this is not going to be a call!
    if ps.lookahead_one_of(";),]}:") {
        return Ok(value);

    } else if ps.at_eof {
        return Ok(value);

    } else if no_pipe && ps.peek().unwrap() == '|' {
        return Ok(value);
    }

    let mut call = value;
    if ps.peek().unwrap() != '|' {
        // if we have something left to read, and it's not a
        // delimiter, then we have to read a call:
        call = parse_call(call, ps, binop_mode)?;
    }

    if ps.at_eof || (no_pipe && ps.peek().unwrap() == '|') {
        return Ok(call);
    }

    while let Some(c) = ps.peek() {
        match c {
            '|' => {
                ps.consume_wsc();
                let mut fn_expr = parse_call_expr(ps, true, binop_mode)?;
                if !is_call(&fn_expr) {
                    fn_expr = make_to_call(ps, fn_expr);
                }
                fn_expr.push(call);
                call = fn_expr;
            },
            _ => {
                break;
            }
        }
    }

    Ok(call)
}

pub fn parse_assignment(ps: &mut ParseState, is_def: bool) -> Result<VVal, ParseError> {
    if ps.at_eof {
        return ps.err_eof("assignment");
    }

    let mut assign = VVal::vec();
    if is_def {
        assign.push(ps.syn_raw(Syntax::Def));
    } else {
        assign.push(ps.syn_raw(Syntax::Assign));
    }

    if is_def {
        if ps.consume_if_eq_wsc(':') {
            let key = parse_identifier(ps);
            if key == "ref" {
                assign = ps.syn(Syntax::DefRef);
            } else if key == "wref" {
                assign = ps.syn(Syntax::DefWRef);
            }
        }
    }

    let ids = VVal::vec();

    match ps.peek().unwrap() {
        '(' => {
            ps.consume_wsc();

            while let Some(c) = ps.peek() {
                if c == ')' { break; }
                ids.push(VVal::Sym(parse_identifier(ps)));
                if !ps.consume_if_eq_wsc(',') { break; }
            }

            if ps.at_eof {
                return ps.err_eof(
                    "destructuring assignment");
            }

            if !ps.consume_if_eq_wsc(')') {
                return ps.err_unexpected_token(
                    ')', "At the end of destructuring assignment.");
            }
        },
        _ => { ids.push(VVal::Sym(parse_identifier(ps))); }
    }

    assign.push(ids);

    if !ps.consume_if_eq_wsc('=') {
        return ps.err_unexpected_token('=', "In assignment");
    }
    assign.push(parse_expr(ps)?);

    return Ok(assign);
}

pub fn parse_stmt(ps: &mut ParseState) -> Result<VVal, ParseError> {
    //println!("parse_stmt [{}]", ps.rest());
    match ps.peek() {
        Some(c) => {
            match c {
                '!' => { ps.consume_wsc(); parse_assignment(ps, true) },
                '.' => { ps.consume_wsc(); parse_assignment(ps, false) },
                '(' => { parse_assignment(ps, false) },
                _   => { parse_expr(ps) },
            }
        },
        None => { ps.err_eof("statement") }
    }
}

pub fn parse_block(ps: &mut ParseState, is_delimited: bool) -> Result<VVal, ParseError> {
    //println!("parse_block [{}]", ps.rest());
    if is_delimited {
        if !ps.consume_if_eq_wsc('{') {
            return ps.err_unexpected_token('{', "When parsing a block.");
        }
    }

    let block = ps.syn(Syntax::Block);

    while let Some(c) = ps.peek() {
        if is_delimited { if c == '}' { break; } }

        let next_stmt = parse_stmt(ps)?;
        block.push(next_stmt);

        while ps.consume_if_eq_wsc(';') {
            while ps.consume_if_eq_wsc(';') { }
            if ps.at_eof || ps.consume_if_eq_wsc('}') { return Ok(block); }
            let next_stmt = parse_stmt(ps)?;
            block.push(next_stmt);
        }
    }

    if is_delimited {
        if ps.at_eof { return ps.err_eof("parsing block"); }
        if !ps.consume_if_eq_wsc('}') {
            return ps.err_unexpected_token('}', "When parsing a block.");
        }
    }

    Ok(block)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk(s: &str) -> ParseState {
        ParseState::new(s, 1)
    }

    fn parse(s: &str) -> String {
        let mut ps = mk(s);
        match parse_block(&mut ps, false) {
            Ok(v)  => v.s(),
            Err(e) => { panic!(format!("ERROR: {:?} at '{}' with input '{}'", e, ps.rest(), s)); },
        }
    }

    fn parse_error(s: &str) -> String {
        let mut ps = mk(s);
        match parse_block(&mut ps, false) {
            Ok(v)  => panic!(format!("Expected error but got result: {} for input '{}'",
                                     v.s(), s)),
            Err(e) => { format!("Parse error: {:?}", e) },
        }
    }

    #[test]
    fn check_parse_numbers() {
        assert_eq!(parse("#comment \n10;#fom \n"),  "[&Block,10]");
        assert_eq!(parse("10;"),       "[&Block,10]");
        assert_eq!(parse("10.123;"),   "[&Block,10.123]");
        assert_eq!(parse("-10;"),      "[&Block,-10]");
        assert_eq!(parse("-0xFF;"),    "[&Block,-255]");
        assert_eq!(parse("-0xFF.1;"),  "[&Block,-255.0625]");
        assert_eq!(parse("-0xFF.9;"),  "[&Block,-255.5625]");
        assert_eq!(parse("-0xFF.A;"),  "[&Block,-255.625]");
        assert_eq!(parse("-0xFF.F;"),  "[&Block,-255.9375]");
    }

    #[test]
    fn check_parse_vec() {
        assert_eq!(parse("$[10];"),
                   "[&Block,[&Lst,10]]");
        assert_eq!(parse("$[10, 11.23, -30, -0xFF];"),
                   "[&Block,[&Lst,10,11.23,-30,-255]]");
        assert_eq!(parse("$[10, $[1,2,3], 11.23, -30, -0xFF];"),
                   "[&Block,[&Lst,10,[&Lst,1,2,3],11.23,-30,-255]]");
    }

    #[test]
    fn check_calls() {
        assert_eq!(parse("10"),         "[&Block,10]");
        assert_eq!(parse("10;"),        "[&Block,10]");
        assert_eq!(parse("10; 20"),     "[&Block,10,20]");
        assert_eq!(parse("10;;; 20"),   "[&Block,10,20]");
        assert_eq!(parse("10;;; 20;"),  "[&Block,10,20]");
        assert_eq!(parse("10 20;"),     "[&Block,[&Call,10,20]]");
        assert_eq!(parse("[10] 20;"),   "[&Block,[&Call,10,20]]");
    }

    #[test]
    fn check_expr() {
        assert_eq!(parse("10 20 30"),
                   "[&Block,[&Call,10,20,30]]");
        assert_eq!(parse("10 20 30 40"),
                   "[&Block,[&Call,10,20,30,40]]");
        assert_eq!(parse("10 | 20 30"),
                   "[&Block,[&Call,20,30,10]]");
        assert_eq!(parse("10 20 | 30 40"),
                   "[&Block,[&Call,30,40,[&Call,10,20]]]");
        assert_eq!(parse("10 20 | 30 40 | 50"),
                   "[&Block,[&Call,50,[&Call,30,40,[&Call,10,20]]]]");
        assert_eq!(parse("10 | 20 | 30 | 40"),
                   "[&Block,[&Call,40,[&Call,30,[&Call,20,10]]]]");
        assert_eq!(parse("10() | 20 | 30 | 40"),
                   "[&Block,[&Call,40,[&Call,30,[&Call,20,[&Call,10]]]]]");
        assert_eq!(parse("10()() | 20 | 30 | 40"),
                   "[&Block,[&Call,40,[&Call,30,[&Call,20,[&Call,[&Call,10]]]]]]");
        assert_eq!(parse("[10 | 20] | [foo(bar)]"),
                   "[&Block,[&Call,[&Var,$\"foo\"],[&Var,$\"bar\"],[&Call,20,10]]]");
        assert_eq!(parse("10 ~ 20 ~ 30 ~ 40"),
                   "[&Block,[&Call,10,[&Call,20,[&Call,30,40]]]]");
        assert_eq!(parse("10 | 20"),                  "[&Block,[&Call,20,10]]");
        assert_eq!(parse("10 [1 2] [3 4 5] [6 [7]]"), "[&Block,[&Call,10,[&Call,1,2],[&Call,3,4,5],[&Call,6,7]]]");
        assert_eq!(parse("10()"),                     "[&Block,[&Call,10]]");
        assert_eq!(parse("10(20, 30)"),               "[&Block,[&Call,10,20,30]]");
        assert_eq!(parse("10 x(20, 30)"),             "[&Block,[&Call,10,[&Call,[&Var,$\"x\"],20,30]]]");
        assert_eq!(parse("10 x(20, 30) | 50"),        "[&Block,[&Call,50,[&Call,10,[&Call,[&Var,$\"x\"],20,30]]]]");
        assert_eq!(parse("[10].a"),                   "[&Block,[&Call,[&Key,$\"a\"],10]]");
        assert_eq!(parse("a.b"),                      "[&Block,[&Call,[&Key,$\"b\"],[&Var,$\"a\"]]]");
        assert_eq!(parse("10 a.b"),                   "[&Block,[&Call,10,[&Call,[&Key,$\"b\"],[&Var,$\"a\"]]]]");
        assert_eq!(parse("[10].[20]"),                "[&Block,[&Call,20,10]]");
        assert_eq!(parse("10.20 30"),                 "[&Block,[&Call,10.2,30]]");
        assert_eq!(parse("10 20 ~ 30 ~ 40 ~ 50"),     "[&Block,[&Call,10,20,[&Call,30,[&Call,40,50]]]]");
        assert_eq!(parse("10 20 ~ 30 40 ~ 40 1 2 3 ~ 50 60"),  "[&Block,[&Call,10,20,[&Call,30,40,[&Call,40,1,2,3,[&Call,50,60]]]]]");
        assert_eq!(parse("10[10(1,2,3 foo) ~ 4]"),    "[&Block,[&Call,10,[&Call,[&Call,10,1,2,[&Call,3,[&Var,$\"foo\"]]],4]]]");
        assert_eq!(parse("foo.b.c.d"),                "[&Block,[&Call,[&Key,$\"d\"],[&Call,[&Key,$\"c\"],[&Call,[&Key,$\"b\"],[&Var,$\"foo\"]]]]]");
        assert_eq!(parse("foo.b.c.d()"),              "[&Block,[&Call,[&Call,[&Key,$\"d\"],[&Call,[&Key,$\"c\"],[&Call,[&Key,$\"b\"],[&Var,$\"foo\"]]]]]]");
        assert_eq!(parse("foo.b.c.d(1,2,3)"),         "[&Block,[&Call,[&Call,[&Key,$\"d\"],[&Call,[&Key,$\"c\"],[&Call,[&Key,$\"b\"],[&Var,$\"foo\"]]]],1,2,3]]");
        assert_eq!(parse("foo.b.c.d 1 2 3"),          "[&Block,[&Call,[&Call,[&Key,$\"d\"],[&Call,[&Key,$\"c\"],[&Call,[&Key,$\"b\"],[&Var,$\"foo\"]]]],1,2,3]]");
        assert_eq!(parse("[foo.b.c.d] 1 2 3"),        "[&Block,[&Call,[&Call,[&Key,$\"d\"],[&Call,[&Key,$\"c\"],[&Call,[&Key,$\"b\"],[&Var,$\"foo\"]]]],1,2,3]]");
        assert_eq!(parse("foo.a = 10"),               "[&Block,[&SetKey,[&Var,$\"foo\"],[&Key,$\"a\"],10]]");
        assert_eq!(parse("foo.a = 10 | 20"),          "[&Block,[&SetKey,[&Var,$\"foo\"],[&Key,$\"a\"],[&Call,20,10]]]");
        assert_eq!(parse("foo.a = 10 ~ 20"),          "[&Block,[&SetKey,[&Var,$\"foo\"],[&Key,$\"a\"],[&Call,10,20]]]");
        assert_eq!(parse("4 == 5 ~ 10"),              "[&Block,[&Call,[&Var,$\"==\"],4,[&Call,5,10]]]");
    }

    #[test]
    fn check_expr_err() {
        assert_eq!(parse_error("foo.a() = 10"),       "Parse error: BadCall((\"Unexpected \\\'=\\\'\", \"= 10\", 1, 9, 1))");
    }

    #[test]
    fn check_identifier() {
        assert_eq!(parse("+"),          "[&Block,[&Var,$\"+\"]]");
        assert_eq!(parse("-"),          "[&Block,[&Var,$\"-\"]]");
        assert_eq!(parse("+ 10 20"),    "[&Block,[&Call,[&Var,$\"+\"],10,20]]");
        assert_eq!(parse("13 + 10 20"), "[&Block,[&Call,[&Call,[&Var,$\"+\"],13,10],20]]");
        assert_eq!(parse("13 + 10 == 23"),
                                        "[&Block,[&Call,[&Var,$\"==\"],[&Call,[&Var,$\"+\"],13,10],23]]");
        assert_eq!(parse("[+ 12 ~ - 24 23] == 13"),
           "[&Block,[&Call,[&Var,$\"==\"],[&Call,[&Var,$\"+\"],12,[&Call,[&Var,$\"-\"],24,23]],13]]");
        assert_eq!(parse("_"),          "[&Block,[&Var,$\"_\"]]");
        assert_eq!(parse("ten"),        "[&Block,[&Var,$\"ten\"]]");
        assert_eq!(parse("tenäß foo"),  "[&Block,[&Call,[&Var,$\"tenäß\"],[&Var,$\"foo\"]]]");
    }

    #[test]
    fn check_primitives() {
        assert_eq!(parse("$n"),         "[&Block,$n]");
        assert_eq!(parse("$none"),      "[&Block,$n]");
        assert_eq!(parse("$t"),         "[&Block,$true]");
        assert_eq!(parse("$true"),      "[&Block,$true]");
        assert_eq!(parse("$f"),         "[&Block,$false]");
        assert_eq!(parse("$false"),     "[&Block,$false]");
    }

    #[test]
    fn check_binops() {
        assert_eq!(parse("20 * 10"),                "[&Block,[&Call,[&Var,$\"*\"],20,10]]");
        assert_eq!(parse("40 20 * 10"),             "[&Block,[&Call,40,[&Call,[&Var,$\"*\"],20,10]]]");
        assert_eq!(parse("40 20 * 10 30"),          "[&Block,[&Call,40,[&Call,[&Var,$\"*\"],20,10],30]]");
        assert_eq!(parse("40 20 * 10()"),           "[&Block,[&Call,40,[&Call,[&Var,$\"*\"],20,[&Call,10]]]]");
        assert_eq!(parse("40 20() * 10()"),         "[&Block,[&Call,40,[&Call,[&Var,$\"*\"],[&Call,20],[&Call,10]]]]");
        assert_eq!(parse("20() * 10()"),            "[&Block,[&Call,[&Var,$\"*\"],[&Call,20],[&Call,10]]]");
        assert_eq!(parse("10 - 20 * 30"),           "[&Block,[&Call,[&Var,$\"-\"],10,[&Call,[&Var,$\"*\"],20,30]]]");
        assert_eq!(parse("10 * 20 - 30"),           "[&Block,[&Call,[&Var,$\"-\"],[&Call,[&Var,$\"*\"],10,20],30]]");
        assert_eq!(parse("10 * 20 - 30 * 2"),       "[&Block,[&Call,[&Var,$\"-\"],[&Call,[&Var,$\"*\"],10,20],[&Call,[&Var,$\"*\"],30,2]]]");
        assert_eq!(parse("10 * 20 * 30"),           "[&Block,[&Call,[&Var,$\"*\"],[&Call,[&Var,$\"*\"],10,20],30]]");
        assert_eq!(parse("10 - 20 - 30 - 40"),      "[&Block,[&Call,[&Var,$\"-\"],[&Call,[&Var,$\"-\"],[&Call,[&Var,$\"-\"],10,20],30],40]]");
        assert_eq!(parse("10 - 20 - [30 - 40]"),    "[&Block,[&Call,[&Var,$\"-\"],[&Call,[&Var,$\"-\"],10,20],[&Call,[&Var,$\"-\"],30,40]]]");

        assert_eq!(parse("$t and $f"),              "[&Block,[&Call,[&Var,$\"and\"],$true,$false]]");
        assert_eq!(parse("$t or $f"),               "[&Block,[&Call,[&Var,$\"or\"],$true,$false]]");
        assert_eq!(parse("$t and $f or $f and $f"), "[&Block,[&Call,[&Var,$\"or\"],[&Call,[&Var,$\"and\"],$true,$false],[&Call,[&Var,$\"and\"],$false,$false]]]");
    }

    #[test]
    fn check_assignments() {
        assert_eq!(parse("!x=10;"),              "[&Block,[&Def,[$\"x\"],10]]");
        assert_eq!(parse("! x = 10 ;"),          "[&Block,[&Def,[$\"x\"],10]]");
        assert_eq!(parse("! x = 10"),            "[&Block,[&Def,[$\"x\"],10]]");
        assert_eq!(parse("!:ref x = 10"),        "[&Block,[&DefRef,[$\"x\"],10]]");
        assert_eq!(parse("!:ref (a,b) = 10"),    "[&Block,[&DefRef,[$\"a\",$\"b\"],10]]");
        assert_eq!(parse(". (a,b) = 10"),        "[&Block,[&Assign,[$\"a\",$\"b\"],10]]");
        assert_eq!(parse("(a,b)=10"),            "[&Block,[&Assign,[$\"a\",$\"b\"],10]]");
    }

    #[test]
    fn check_func() {
        assert_eq!(parse("{}"),           "[&Block,[&Func]]");
        assert_eq!(parse("{10;}"),        "[&Block,[&Func,10]]");
        assert_eq!(parse("{10;;;}"),      "[&Block,[&Func,10]]");
        assert_eq!(parse("{10; 20}"),     "[&Block,[&Func,10,20]]");
        assert_eq!(parse("{ 10 } { }"),   "[&Block,[&Call,[&Func,10],[&Func]]]");
    }

    #[test]
    fn check_map() {
        assert_eq!(parse("${a:10}"),   "[&Block,[&Map,[[&Var,$\"a\"],10]]]");
        assert_eq!(parse("${:a:10}"),  "[&Block,[&Map,[[&Key,$\"a\"],10]]]");
    }

    #[test]
    fn check_str() {
        assert_eq!(parse("\"foo\""),       "[&Block,[&Str,\"foo\"]]");
        assert_eq!(parse("\"fo\0o\""),     "[&Block,[&Str,\"fo\\0o\"]]");
        assert_eq!(parse("\"fo\no\""),     "[&Block,[&Str,\"fo\\no\"]]");
        assert_eq!(parse("\"fo\ro\""),     "[&Block,[&Str,\"fo\\ro\"]]");
        assert_eq!(parse("\"fo\\\"o\""),   "[&Block,[&Str,\"fo\\\"o\"]]");
        assert_eq!(parse("\"fo\x05o\""),  "[&Block,[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{9f}\""),   "[&Block,[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{0009f}\""),   "[&Block,[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{09f}\""),   "[&Block,[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{2400}\""), "[&Block,[&Str,\"fo\\x05o␀\"]]");

        assert_eq!(parse("$b\"\\u{2400}\""), "[&Block,[&Str,$b\"\\xE2\\x90\\x80\"]]");
        assert_eq!(parse("$b\"\\x00\\xFF\\xEB\""), "[&Block,[&Str,$b\"\\0\\xFF\\xEB\"]]");
    }
}
