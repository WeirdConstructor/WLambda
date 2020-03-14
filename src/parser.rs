// Copyright (c) 2020 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.
#![allow(clippy::collapsible_if)]

/*!
This is the grammar parser for WLambda.

It produces an AST to be further
transformed by `wlambda::compiler::compile()` into an executable form
of the program.

The parser is a bit crufty as I did not go the extra step of writing
a lexer/tokenizer. One goal of WLambda is to have a rather uncomplicated
and small implementation, and I hope I could achieve that here.

The syntax of WLambda is in part also to make it a bit easier
to parse in this hand written parser.

## Full WLambda Lexical Syntax and Grammar

White space is everything that satisfies `std::char::is_whitespace`,
so unicode white space is respected. Comments have the following syntax:

```ebnf
    comment = "#" ?anything except "\n"? "\n"
```

In the following grammar, white space and comments are omitted:

```ebnf

    ident_start   = ( ?alphabetic? | "_" | "@" )
    ident_end     = { ?any character?
                     - ( ?white space?
                         | "." | "," | ";"
                         | "{" | "}" | "[" | "]" | "(" | ")"
                         | "~" | "|" | "=" ) }
                  ;
    qident        = ident_end
                  (* a quoted identifier can not appear anywhere,
                     it's usually delimited or follows something that
                     makes sure we are now expecting an identifier *)
                  | "`", { ?any character except '`'? }, "`" (* quoted identifier *)
                  ;
    ident         = ident_start, [ ident_end ]
                  | "`", { ?any character except '`'? }, "`" (* quoted identifier *)
                  ;
    ref_specifier = ":", qident
                  ;

    digit         = "0" | "1" | "2" | "3" | "4" | "5"
                  | "6" | "7" | "8" | "9"
                  ;
    integer       = digit, { digit }
                  ;
    radix         = integer
                  ;
    radix_digits  = (* digits in the radix specified
                       earlier in the number.
                       Default radix is of course 10. *)
    number        = [ "-" | "+" ],
                    [ ( radix, "r"
                      | "0", "x"
                      | "0", "b"
                      | "0", "o"
                      ) ],
                    radix_digits,
                    [ ".", radix_digits ]
                  ;
    hexdigit      = ?hexdigit, upper or lower case?
                  ;
    string_escape = "x", hexdigit, hexdigit  (* byte/ascii escape *)
                  | "n"                      (* newline *)
                  | "r"                      (* carriage return *)
                  | "t"                      (* horizontal tab *)
                  | "0"                      (* nul byte/char *)
                  | "u", hexdigit, { hexdigit }
                                             (* unicode char, or in byte strings
                                                their utf-8 encoded form *)
                  | "\""
                  | "\'"
                  | "\\"
                  ;
    string        = "\"", { "\\", string_escape | ?any character? - "\\" },"\""
                  ;
    byte_string   = "b", string
                  ;
    quote_string  = "q", ?any character as quote?, { ?any character? }, ?any character as quote?
                  | "Q", ?any character as quote?, { ?any character? }, ?any character as quote?
                    (* but Q generates a byte string instead! *)
                  ;
    list_expr     = "*", expr   (* splices the vector result of 'expr'
                                   into the currently parsed list *)
                  | expr
                  ;
    list          = "[", [ list_expr, { ",", list_expr }, [ "," ] ],"]"
                  ;
    map_expr      = (ident | expr), "=", expr
                  | "*" expr    (* splices the map result of 'expr'
                                   into the currently parsed map *)
                  ;
    map           = "{", [ map_expr, { ",", map_expr }, [ "," ] ], "}"
                  ;
    self          = "s" | "self"
                  ;
    true          = "t" | "true"
                  ;
    false         = "f" | "false"
                  ;
    none          = "n" | "none
                  ;
    pair          = "p", "(", expr, "," expr, ")"
                  ;
    err           = ("e" | "error"), expr
                  ;
    nvec          = ("i" | "f"), "(", expr, { ",", expr }, ")"
                  ;
    ref           = "&&", value
                  ;
    wref          = "&", value
                  ;
    accumulator   = "@", ("i" | "int"
                         |"s" | "string"
                         |"f" | "float"
                         |"b" | "bytes"
                         |"v" | "vec"
                         |"m" | "map" ), expr
                    (* defines a new accumulator context *)
                  | "@", ("a" | "accum")
                    (* returns the current accumulator value *)
                  | "+" (* resolves to the current accumulator function *)
                  ;
    capture_ref   = ":", var
                  ;
    deref         = "*", value
                  ;
    special_value = byte_string
                  | quote_string
                  | list
                  | map
                  | none
                  | true
                  | false
                  | self
                  | err
                  | nvec
                  | pair
                  | ref
                  | wref
                  | deref
                  | capture_ref
                  | accumulator
                  ;
    arity_def     = "|", number, "<", number, "|" (* set min/max *)
                  | "|", number, "|"              (* set min and max *)
                  | "|", "|"                      (* no enforcement *)
                  ;
    function      = [ "\:", ident ], "{", [ arity_def ], block, "}"
                  | "\", [ arity_def ], statement
                  ;
    var           = ident
                  ;
    symbol        = ":", qident
                  | ":", "\"", (? any char, quoted \\ and \" ?), "\""
                  (*
                     symbols are usually used to specify
                     fields in literal map definitions
                     and lots of other places as stringy sentinel values
                  *)
                  ;
    value         = number
                  | string
                  | "$", special_value
                  | "(", expr, ")"
                  | function
                  | symbol
                  | var
                  ;
    op            = (* here all operators are listed line by line regarding
                       their precedence, top to bottom *)
                    "^"
                  | "*" | "/" | "%"
                  | "-" | "+"
                  | "<<" | ">>"       (* binary shift *)
                  | "<" | ">" | "<=" | ">="
                  | "==" | "!="
                  | "&"               (* binary and *)
                  | "&^"              (* binary xor *)
                  | "&|"              (* binary or *)
                  | "&and"            (* logical and, short circuit *)
                  | "&or"             (* logical or, short circuit *)
                  ;
    bin_op        = call_no_ops, { op, bin_op } (* precedence parsing is done
                                                   in a Pratt parser style *)
                  ;
    arg_list      = "[", [ expr, { ",", expr }, [ "," ] ], "]"
                  | "[[", expr, "]]"  (* apply result vector of expr as argument list *)
                  ;
    field         = ".", ( integer | ident | value ), [ field ]
                  ;
    field_access  = field, "=", expr
                  | field, arg_list
                  | field
                  (* please note, that a field access like:
                     `obj.field` is equivalent to the call:
                     `field[obj]`. That also means that
                     `obj.field[...]` is transformed into
                     `field[obj][...]`.
                     The exception is "=" which assigns
                     the field as specified.
                     BUT: There is a special case, when you specify
                     an `indent` it is quoted and interpreted as symbol. *)
                  ;
    call_no_ops   = value, { arg_list | field_access }, [ "~", expr ]
                  ;
    call          = value,
                    { arg_list | field_access | bin_op | value },
                    [ "~", expr ] (* this is a tail argument, if present the
                                     expr is appended to the argument list *)
                  ;
    expr          = call, { "|", call }
                  | call, { "|>", call }
                  | call, { "||", call }
                  ;
    simple_assign = qident, "=", expr
                  ;
    destr_assign  = "(", [ qident, { ",", qident } ], ")", "=" expr
                  ;
    definition    = [ ref_specifier ], ( simple_assign | destr_assign )
                  ;
    import        = "!", "@import", symbol, [ "=" ], symbol
                  | "!", "@wlambda"
                  ;
    export        = "!", "@export", symbol, [ "=" ], expr
                  ;
    statement     = "!" definition
                  | "." simple_assign
                  | "." destr_assign
                  | import
                  | export
                  | expr
                  ;
    block         = "{", { statement, ";", {";"}}, [ statement, {";"} ], "}"
                  | { statement, ";", {";"} }, [ statement, {";"} ]
                  ;
    code          = block
                  ;
```

*/


use crate::vval::VVal;
use crate::vval::Syntax;

pub mod state;

pub use state::State;
pub use state::{ParseValueError, ParseNumberError, ParseError, ParseErrorKind};

/*

special variables:
    arguments are in '@', first one is in '_' and _1
    others: _1 up to _9

special functions:
    range   - returning a range iterator
    iter    - called on list/map it returns a collection iterator
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
    list    => call first arg for every element
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
            x = 10
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
        let :ref self = ${ inner_val = #t };
        { apply self->inner_val @ }
    }()

    my_cond { # if
        println "INNER VALUE IS TRUE!"
    } { # else
        println "INNER VALUE IS FALSE!"
    }

Prototyped inheritance:

    !proto = ${ print = { println _ }, };
    !o = to_obj { _proto_ = proto };
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

/// Helper function for recording characters into a byte buffer.
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

/// Helper function for recording a string character either
/// as byte for a byte buffer in `v` or as character for a `String` `s`.
fn adchr(v: &mut Vec<u8>, s: &mut String, b: bool, c: char) {
    if b { add_c_to_vec(v, c); }
    else { s.push(c); }
}

/// Parsers a WLambda special quote string or byte buffer.
fn parse_q_string(ps: &mut State, bytes: bool) -> Result<VVal, ParseError> {
    if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("string"))); }

    let quote_char = ps.expect_some(ps.peek())?;
    ps.consume();

    let quote_char = match quote_char {
        '[' => ']',
        '(' => ')',
        '{' => '}',
        '<' => '>',
        _ => quote_char
    };

    let vec = ps.syn(Syntax::Str);

    let mut s = String::from("");
    let mut v : Vec<u8> = Vec::new();

    while ps.peek().unwrap_or(quote_char) != quote_char {
        let c = ps.expect_some(ps.peek())?;
        ps.consume();
        adchr(&mut v, &mut s, bytes, c);
    }

    if bytes {
        vec.push(VVal::new_byt(v));
    } else {
        vec.push(VVal::new_str(&s));
    }

    if !ps.consume_if_eq(quote_char) {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(quote_char, "")));
    }

    ps.skip_ws_and_comments();

    Ok(vec)
}

enum NVecKind {
    Int,
    Flt,
}

/// Parses the part of a numerical vector that comes after the declaration,
/// in `$i(1, 2, 3)` `$i` is the declaration.
fn parse_nvec_body(ps: &mut State, kind: NVecKind) -> Result<VVal, ParseError> {
    match ps.peek() {
        Some('(') => {
            ps.consume_wsc();
            let vec = ps.syn(match kind {
                NVecKind::Int => Syntax::IVec,
                NVecKind::Flt => Syntax::FVec,
            });
            
            vec.push(parse_expr(ps)?);
            while ps.consume_if_eq_wsc(',') {
                vec.push(parse_expr(ps)?);
            }

            // how many dimensions are in the numerical vector we just parsed?
            let dim = vec.len() - 1;

            if !ps.consume_if_eq_wsc(')') {
                Err(ps.err(ParseErrorKind::UnexpectedToken(')', "")))
            } else if dim > 4 || dim < 1 {
                Err(ps.err(ParseValueError::VectorLength))
            } else {
                Ok(vec)
            }
        },
        Some(t) => Err(ps.err(ParseErrorKind::UnexpectedToken(
            t,
            "To make a numerical vector, Parenthesis must follow $i and $f",
        ))),
        None => Err(ps.err(ParseErrorKind::EOF("numerical vector body"))),
    }
}


/// Parsers a WLambda string or byte buffer.
fn parse_string(ps: &mut State, bytes: bool) -> Result<VVal, ParseError> {
    if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("string"))); }

    ps.consume_if_eq('"');

    let vec = ps.syn(Syntax::Str);

    let mut s = String::from("");
    let mut v : Vec<u8> = Vec::new();

    while ps.peek().unwrap_or('"') != '"' {
        let c = ps.expect_some(ps.peek())?;
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
                                    if bytes { v.push(cn) }
                                    else { s.push(cn as char); }
                                } else {
                                    return Err(ps.err(ParseErrorKind::BadEscape("Bad hex escape in string")));
                                }
                            } else {
                                return Err(ps.err(ParseErrorKind::EOF("string hex escape")));
                            }
                        },
                        'n'  => { ps.consume(); adchr(&mut v, &mut s, bytes, '\n'); },
                        'r'  => { ps.consume(); adchr(&mut v, &mut s, bytes, '\r'); },
                        't'  => { ps.consume(); adchr(&mut v, &mut s, bytes, '\t'); },
                        '\\' => { ps.consume(); adchr(&mut v, &mut s, bytes, '\\'); },
                        '0'  => { ps.consume(); adchr(&mut v, &mut s, bytes, '\0'); },
                        '\'' => { ps.consume(); adchr(&mut v, &mut s, bytes, '\''); },
                        '"'  => { ps.consume(); adchr(&mut v, &mut s, bytes, '"'); },
                        'u' => {
                            ps.consume();
                            if !ps.consume_if_eq('{') {
                                return Err(ps.err(ParseErrorKind::UnexpectedToken('{', "After unicode escape")));
                            }

                            let uh : String = ps.take_while(|c| c.is_digit(16)).iter().collect();

                            if let Ok(cn) = u32::from_str_radix(&uh, 16) {
                                if let Some(c) = std::char::from_u32(cn) {
                                    adchr(&mut v, &mut s, bytes, c);
                                } else {
                                    return Err(ps.err(ParseErrorKind::BadEscape(
                                        "Bad char in unicode escape in string"
                                    )));
                                }
                            } else {
                                return Err(ps.err(ParseErrorKind::BadEscape(
                                    "Bad unicode hex escape in string"
                                )));
                            }

                            if !ps.consume_if_eq('}') {
                                return Err(ps.err(ParseErrorKind::UnexpectedToken('}', "After unicode escape")));
                            }
                        },
                        c => { ps.consume(); adchr(&mut v, &mut s, bytes, c); },
                    }
                } else {
                    return Err(ps.err(ParseErrorKind::EOF("string escape")));
                }
            },
            _ => {
                ps.consume();
                adchr(&mut v, &mut s, bytes, c);
            },
        }
    }

    if bytes {
        vec.push(VVal::new_byt(v));
    } else {
        vec.push(VVal::new_str(&s));
    }

    if !ps.consume_if_eq('"') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('\"', "")));
    }

    ps.skip_ws_and_comments();

    Ok(vec)
}

#[allow(clippy::cast_lossless)]
fn parse_num(ps: &mut State) -> Result<VVal, ParseError> {
    if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("number"))); }

    let c = ps.expect_some(ps.peek())?;
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
            return Err(ps.err(ParseNumberError::UnsupportedRadix(radix)));
        }

        (radix, ps.take_while(|c| c.is_digit(radix as u32)).iter().collect())
    } else if ps.consume_if_eq('x') {
        if radix_or_num != "0" {
            return Err(ps.err(ParseNumberError::UnsupportedRadixPrefix('x', radix_or_num)));
        }
        (16, ps.take_while(|c| c.is_digit(16)).iter().collect())
    } else if ps.consume_if_eq('b') {
        if radix_or_num != "0" {
            return Err(ps.err(ParseNumberError::UnsupportedRadixPrefix('b', radix_or_num)));
        }
        (2, ps.take_while(|c| c.is_digit(2)).iter().collect())
    } else if ps.consume_if_eq('o') {
        if radix_or_num != "0" {
            return Err(ps.err(ParseNumberError::UnsupportedRadixPrefix('o', radix_or_num)));
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
            return Err(ps.err(ParseNumberError::InvalidFractionalDigits(fract_digits)));
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
                    Ok(VVal::Int((num as i64).wrapping_neg()))
                } else {
                    Ok(VVal::Int(num as i64))
                }
            }
        },
        Err(e) => Err(ps.err(ParseNumberError::InvalidRadix(num, radix, e)))
    }
}

fn parse_list(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_wsc('[') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('[', "At list")));
    }

    let list = ps.syn(Syntax::Lst);

    while ps.expect_some(ps.peek())? != ']' {
        if ps.consume_if_eq_wsc('*') {
            let r = ps.syn(Syntax::VecSplice);
            r.push(parse_expr(ps)?);
            list.push(r);
        } else {
            list.push(parse_expr(ps)?);
        }
        if !ps.consume_if_eq_wsc(',') { break; }
    }

    if !ps.consume_if_eq_wsc(']') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(']', "At the end of list")));
    }

    Ok(list)
}

fn parse_map(ps: &mut State) -> Result<VVal, ParseError> {
    //println!("parse_map [{}]", ps.rest());
    if !ps.consume_if_eq_wsc('{') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('{', "At map")));
    }

    let map = ps.syn(Syntax::Map);

    while ps.expect_some(ps.peek())? != '}' {
        let c = ps.expect_some(ps.peek())?;

        map.push(
            if ps.consume_if_eq_wsc('*') {
                let r = ps.syn(Syntax::MapSplice);
                r.push(parse_expr(ps)?);
                r

            } else {
                let key = if is_ident_start(c) {
                    VVal::new_sym_mv(parse_identifier(ps)?)
                } else {
                    parse_expr(ps)?
                };
                if !ps.consume_if_eq_wsc('=') {
                    return Err(ps.err(ParseErrorKind::UnexpectedToken('=', "After reading map key")));
                }

                let elem = VVal::vec();
                elem.push(key);
                elem.push(parse_expr(ps)?);
                elem
            });

        if !ps.consume_if_eq_wsc(',') { break; }
    }

    if !ps.consume_if_eq_wsc('}') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('}', "At the end of a map")));
    }

    Ok(map)
}


fn parse_special_value(ps: &mut State) -> Result<VVal, ParseError> {
    if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("literal value"))); }
    let c = ps.expect_some(ps.peek())?;

    match c {
        'b' => { ps.consume(); parse_string(ps, true) },
        'q' => { ps.consume(); parse_q_string(ps, false) },
        'Q' => { ps.consume(); parse_q_string(ps, true) },
        '[' => parse_list(ps),
        '{' => parse_map(ps),
        'n' => {
            if ps.consume_lookahead("none") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(VVal::Nul)
        },
        'i' => {
            ps.consume();
            parse_nvec_body(ps, NVecKind::Int)
        },
        'd' => {
            if ps.consume_lookahead("data") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(ps.syn(Syntax::SelfData))
        },
        's' => {
            if ps.consume_lookahead("self") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(ps.syn(Syntax::SelfObj))
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
                Ok(VVal::Bol(false))
            } else if ps.lookahead("f(") {
                ps.consume();
                parse_nvec_body(ps, NVecKind::Flt)
            } else {
                ps.consume_wsc();
                Ok(VVal::Bol(false))
            }
        },
        'e' => {
            if ps.consume_lookahead("error") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }

            let err = ps.syn(Syntax::Err);
            err.push(parse_expr(ps)?);
            Ok(err)
        },
        '*' => {
            ps.consume_wsc();
            let r = ps.syn(Syntax::Deref);
            r.push(parse_value(ps)?);
            Ok(r)
        },
        'p' => {
            ps.consume_wsc();
            if !ps.consume_if_eq_wsc('(') {
                return Err(ps.err(ParseErrorKind::UnexpectedToken(')', "At the end of a pair")))
            }
            let a = parse_expr(ps)?;
            let ret =
                if ps.consume_if_eq_wsc(',') {
                    let b = parse_expr(ps)?;
                    VVal::Pair(Box::new((a, b)))
                } else {
                    VVal::Pair(Box::new((a, VVal::Nul)))
                };

            if ps.consume_if_eq_wsc(')') {
                Ok(ret)
            } else {
                Err(ps.err(ParseErrorKind::UnexpectedToken(')', "At the end of a pair")))
            }
        },
        ':' => {
            ps.consume_wsc();
            let capture = ps.syn(Syntax::CaptureRef);
            capture.push(VVal::new_sym_mv(parse_identifier(ps)?));
            Ok(capture)
        },
        '&' => {
            if ps.consume_lookahead("&&") {
                ps.skip_ws_and_comments();
                let r = ps.syn(Syntax::Ref);
                r.push(parse_value(ps)?);
                Ok(r)
            } else {
                ps.consume_wsc();
                let r = ps.syn(Syntax::WRef);
                r.push(parse_value(ps)?);
                Ok(r)
            }
        },
        '@' => {
            let a = ps.syn(Syntax::Accum);
            if ps.consume_lookahead("@int") || ps.consume_lookahead("@i") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("int"));
                a.push(parse_expr(ps)?);
                Ok(a)

            } else if ps.consume_lookahead("@float") || ps.consume_lookahead("@f") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("float"));
                a.push(parse_expr(ps)?);
                Ok(a)

            } else if ps.consume_lookahead("@string") || ps.consume_lookahead("@s") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("string"));
                a.push(parse_expr(ps)?);
                Ok(a)

            } else if ps.consume_lookahead("@bytes") || ps.consume_lookahead("@b") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("bytes"));
                a.push(parse_expr(ps)?);
                Ok(a)

            } else if ps.consume_lookahead("@vec") || ps.consume_lookahead("@v") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("vec"));
                a.push(parse_expr(ps)?);
                Ok(a)

            } else if ps.consume_lookahead("@map") || ps.consume_lookahead("@m") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("map"));
                a.push(parse_expr(ps)?);
                Ok(a)

            } else if ps.consume_lookahead("@@") {
                ps.skip_ws_and_comments();
                a.push(VVal::new_sym("@"));
                Ok(a)

            } else {
                Err(ps.err(ParseValueError::ExpectedAccumulator))
            }
        },
        '+' => {
            ps.consume_wsc();
            Ok(ps.syn(Syntax::Accum))
        },
        c => {
            Err(ps.err(ParseValueError::UnknownSpecialIdentifier(c)))
        }
    }
}

#[allow(dead_code)]
fn is_var(expr: &VVal) -> bool {
    if let Some(ea) = expr.at(0) {
        if let VVal::Syn(s) = ea {
            return s.syn == Syntax::Var;
        }
    }
    false
}

fn is_call(expr: &VVal) -> bool {
    if let Some(ea) = expr.at(0) {
        if let VVal::Syn(s) = ea {
            return s.syn == Syntax::Call;
        }
    }
    false
}

fn make_to_call(ps: &State, expr: VVal) -> VVal {
    let call = ps.syn(Syntax::Call);
    call.push(expr);
    call
}

fn make_var(ps: &State, identifier: &str) -> VVal {
    let id = ps.syn(Syntax::Var);
    id.push(VVal::new_sym(identifier));
    id
}

fn make_sym(ps: &State, identifier: &str) -> VVal {
    let id = ps.syn(Syntax::Key);
    id.push(VVal::new_sym(identifier));
    id
}

fn make_binop(ps: &State, op: &str, left: VVal, right: VVal) -> VVal {
    if op == "&and" {
        let and = ps.syn(Syntax::And);
        and.push(left);
        and.push(right);
        and

    } else if op == "&or" {
        let or = ps.syn(Syntax::Or);
        or.push(left);
        or.push(right);
        or

    } else if op == "+" {
        let or = ps.syn(Syntax::BinOpAdd);
        or.push(left);
        or.push(right);
        or

    } else if op == "-" {
        let or = ps.syn(Syntax::BinOpSub);
        or.push(left);
        or.push(right);
        or

    } else if op == "*" {
        let or = ps.syn(Syntax::BinOpMul);
        or.push(left);
        or.push(right);
        or

    } else if op == "/" {
        let or = ps.syn(Syntax::BinOpDiv);
        or.push(left);
        or.push(right);
        or

    } else if op == "%" {
        let or = ps.syn(Syntax::BinOpMod);
        or.push(left);
        or.push(right);
        or

    } else if op == ">" {
        let or = ps.syn(Syntax::BinOpGt);
        or.push(left);
        or.push(right);
        or

    } else if op == "<" {
        let or = ps.syn(Syntax::BinOpLt);
        or.push(left);
        or.push(right);
        or

    } else if op == "<=" {
        let or = ps.syn(Syntax::BinOpLe);
        or.push(left);
        or.push(right);
        or

    } else if op == ">=" {
        let or = ps.syn(Syntax::BinOpGe);
        or.push(left);
        or.push(right);
        or

    } else {
        let call = make_to_call(ps, make_var(ps, op));
        call.push(left);
        call.push(right);
        call
    }
}

fn parse_identifier(ps: &mut State) -> Result<String, ParseError> {
    if ps.peek().unwrap() == '`' {
        let mut identifier = "".to_string();
        ps.consume();
        while ps.peek().unwrap_or('`') != '`' {
            let c = ps.expect_some(ps.peek())?;
            match c {
                '\\' => {
                    ps.consume();
                    if let Some(c) = ps.peek() {
                        ps.consume();
                        identifier.push(c);
                    } else {
                        return Err(ps.err(ParseErrorKind::EOF("identifier escape")));
                    }
                },
                _ => {
                    ps.consume();
                    identifier.push(c);
                },
            }
        }

        if !ps.consume_if_eq('`') {
            return Err(ps.err(ParseErrorKind::UnexpectedToken('`', "")));
        }

        ps.skip_ws_and_comments();

        Ok(identifier)
    } else {
        let identifier : String =
            ps.take_while_wsc(|c| {
                match c {
                   '.' | ',' | ';' | '{' | '}'
                 | '[' | ']' | '(' | ')' | '~' | '|' | '='
                        => false,
                    _   => !c.is_whitespace()
                }
            }).iter().collect();
        Ok(identifier)
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '@' || c == '`'
}

fn parse_value(ps: &mut State) -> Result<VVal, ParseError> {
    //println!("parse_value [{}]", ps.rest());
    if let Some(c) = ps.peek() {
        match c {
            '0' ..= '9' | '+' | '-' => parse_num(ps),
            '"' => parse_string(ps, false),
            '$' => { ps.consume_wsc(); parse_special_value(ps) },
            '(' => {
                ps.consume_wsc();
                let expr = parse_expr(ps)?;
                if !ps.consume_if_eq_wsc(')') {
                    return Err(ps.err(ParseErrorKind::UnexpectedToken(')', "In sub expression")));
                }
                Ok(expr)
            },
            '{' => {
                let syn = ps.syn_raw(Syntax::Func);
                let block = parse_block(ps, true)?;
                block.set_at(0, syn);
                block.insert_at(1, VVal::Nul);
                Ok(block)
            },
            '\\' => {
                ps.consume_wsc();

                if ps.consume_if_eq_wsc(':') {
                    let syn = ps.syn_raw(Syntax::Func);

                    let block_name = parse_identifier(ps)?;
                    ps.skip_ws_and_comments();
                    let block = parse_block(ps, true)?;

                    block.set_at(0, syn);
                    block.insert_at(1, VVal::new_sym_mv(block_name));
                    Ok(block)
                } else {
                    let block = ps.syn(Syntax::Func);

                    let arity =
                        if ps.lookahead("|") { parse_arity(ps)? }
                        else { VVal::Nul };

                    let next_stmt = parse_stmt(ps)?;
                    block.push(VVal::Nul);
                    block.push(arity);
                    block.push(next_stmt);
                    Ok(block)
                }
            },
            ':' => {
                ps.consume_wsc();
                if ps.lookahead("\"") {
                    let s = parse_string(ps, false)?;
                    s.at(1).unwrap().with_s_ref(|s: &str|
                        Ok(make_sym(ps, s)))
                } else {
                    let id = parse_identifier(ps)?;
                    Ok(make_sym(ps, &id))
                }
            },
            _ if is_ident_start(c) => {
                let id = parse_identifier(ps)?;
                Ok(make_var(ps, &id))
            },
            _ => {
                Err(ps.err(ParseValueError::Expected("literal value, sub \
                                 expression, block, key or identifier")))
            }
        }
    } else {
        Err(ps.err(ParseErrorKind::EOF("value")))
    }
}

fn optimize_get_key(ps: &mut State, obj: VVal, value: VVal) -> VVal {
    let mut first_syn = obj.v_(0);

    if first_syn.get_syn() == Syntax::GetKey {
        first_syn.set_syn(Syntax::GetKey2);
        obj.set_at(0, first_syn);
        obj.push(value);
        obj

    } else if first_syn.get_syn() == Syntax::GetKey2 {
        first_syn.set_syn(Syntax::GetKey3);
        obj.set_at(0, first_syn);
        obj.push(value);
        obj

    } else if first_syn.get_syn() == Syntax::GetIdx && value.is_int() {
        first_syn.set_syn(Syntax::GetIdx2);
        obj.set_at(0, first_syn);
        obj.push(value);
        obj

    } else if first_syn.get_syn() == Syntax::GetIdx2 && value.is_int() {
        first_syn.set_syn(Syntax::GetIdx3);
        obj.set_at(0, first_syn);
        obj.push(value);
        obj

    } else if first_syn.get_syn() == Syntax::GetSym && value.v_(0).get_syn() == Syntax::Key {
        first_syn.set_syn(Syntax::GetSym2);
        obj.set_at(0, first_syn);
        obj.push(value.v_(1));
        obj

    } else if first_syn.get_syn() == Syntax::GetSym2 && value.v_(0).get_syn() == Syntax::Key {
        first_syn.set_syn(Syntax::GetSym3);
        obj.set_at(0, first_syn);
        obj.push(value.v_(1));
        obj

    } else if first_syn.get_syn() == Syntax::GetSym && value.v_(0).get_syn() == Syntax::Str {
        first_syn.set_syn(Syntax::GetSym2);
        obj.set_at(0, first_syn);
        obj.push(VVal::new_str_mv(value.v_(1).s_raw()));
        obj

    } else if first_syn.get_syn() == Syntax::GetSym2 && value.v_(0).get_syn() == Syntax::Str {
        first_syn.set_syn(Syntax::GetSym3);
        obj.set_at(0, first_syn);
        obj.push(VVal::new_str_mv(value.v_(1).s_raw()));
        obj

    } else {
        if value.v_(0).get_syn() == Syntax::Key {
            let call = ps.syn(Syntax::GetSym);
            call.push(obj);
            call.push(value.v_(1));
            call

        } else if value.v_(0).get_syn() == Syntax::Str {
            let call = ps.syn(Syntax::GetSym);
            call.push(obj);
            call.push(VVal::new_str_mv(value.v_(1).s_raw()));
            call

        } else if value.is_int() {
            let call = ps.syn(Syntax::GetIdx);
            call.push(obj);
            call.push(value);
            call

        } else {
            let call = ps.syn(Syntax::GetKey);
            call.push(obj);
            call.push(value);
            call
        }
    }
}

fn parse_field_access(obj_val: VVal, ps: &mut State) -> Result<VVal, ParseError> {
    let mut obj = obj_val;

    while let Some(c) = ps.peek() {
        if c != '.' { break; }

        ps.consume_wsc();

        let c = if let Some(c) = ps.peek() {
            c
        } else {
            return Err(ps.err(ParseErrorKind::EOF("field access")));
        };

        let value =
            if c.is_digit(10) {
                let idx : String =
                    ps.take_while(|c| c.is_digit(10)).iter().collect();
                if let Ok(idx_num) = i64::from_str_radix(&idx, 10) {
                    ps.skip_ws_and_comments();
                    VVal::Int(idx_num)
                } else {
                    return Err(ps.err(ParseNumberError::InvalidIndexDigits(idx)));
                }

            } else if is_ident_start(c) {
                let id = ps.syn(Syntax::Key);
                id.push(VVal::new_sym_mv(parse_identifier(ps)?));
                id
            } else {
                parse_value(ps)?
            };

        if let Some(c) = ps.peek() {
            match c {
                '=' => {
                    if !ps.lookahead("==") {
                        ps.consume_wsc();
                        let field_set = ps.syn(Syntax::SetKey);
                        field_set.push(obj);
                        field_set.push(value);
                        field_set.push(parse_expr(ps)?);
                        return Ok(field_set);
                    }
                },
                '[' => {
                    let call = optimize_get_key(ps, obj, value);

                    let mut field_call = make_to_call(ps, call);
                    match parse_arg_list(&mut field_call, ps) {
                        Ok(_)    => return Ok(field_call),
                        Err(err) => return Err(err),
                    }
                },
                _ => { }
            }
        }

        obj = optimize_get_key(ps, obj, value);
    }

    Ok(obj)
}

fn parse_arg_list<'a, 'b>(call: &'a mut VVal, ps: &'b mut State) -> Result<&'a mut VVal, ParseError> {
    if !ps.consume_if_eq_wsc('[') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('[', "At start of call arguments")));
    }

    let is_apply = ps.consume_if_eq_wsc('[');

    if is_apply {
        if let VVal::Syn(mut sp) = call.at(0).unwrap_or(VVal::Nul) {
            sp.syn = Syntax::Apply;
            call.set_at(0, VVal::Syn(sp));
        }
        let call_argv = parse_expr(ps)?;
        call.push(call_argv);

    } else {
        while let Some(c) = ps.peek() {
            if c == ']' { break; }

            let call_arg = parse_expr(ps)?;
            call.push(call_arg);

            if !ps.consume_if_eq_wsc(',') {
                break;
            }
        }
    }

    if ps.at_eof {
        return Err(ps.err(ParseErrorKind::EOF("call args")));
    }

    if is_apply && !ps.consume_if_eq_wsc(']') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(']', "While reading apply arguments")));
    }

    if !ps.consume_if_eq_wsc(']') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(']', "While reading call arguments")));
    }

    Ok(call)
}

fn get_op_prec(op: &str) -> i32 {
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
        "&and"                      => 6,
        "&or"                       => 5,
        _                           => 0
    }
}

fn parse_binop(mut left: VVal, ps: &mut State, op: &str) -> Result<VVal, ParseError> {
    let prec = get_op_prec(op);
    let mut right = parse_call(ps, true)?;

    while let Some(next_op) = ps.peek_op() {
        ps.consume_wsc_n(next_op.len());

        let next_prec = get_op_prec(&next_op);
        if prec < next_prec {
            right = parse_binop(right, ps, &next_op)?;
        } else {
            left = make_binop(ps, &op, left, right);
            return parse_binop(left, ps, &next_op);
        }
    }

    Ok(make_binop(ps, op, left, right))
}

fn parse_call(ps: &mut State, binop_mode: bool) -> Result<VVal, ParseError> {
    //println!("parse_expr [{}] np={}", ps.rest(), no_pipe);
    let mut value = parse_value(ps)?;

    // look ahead, if we see an expression delimiter.
    // because then, this is not going to be a call!
    // Also exception to parse_expr, we are excluding the '|'.
    if ps.lookahead_one_of(";),]}|") || ps.at_eof {
        return Ok(value);
    }

    //println!("parse_call [{}]", ps.rest());
    let mut res_call = VVal::Nul;

    while let Some(c) = ps.peek() {
        //println!("PC c={}", c);
        let op = ps.peek_op();

//        if let Some(s) = self.peek2() {
//            match s.chars().nth(0).unwrap_or(' ') {
//                '+' | '-' => true,
//                _         => false,
//            } && (if s.chars().nth(1).unwrap_or(' ').is_digit(10) {
//                      true
//                  } else {
//                      false
//                  })
//        } else {
//
        match c {
            '[' => {
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
            ';' | ')' | ',' | ']' | '|' | '}' => {
                break;
            },
            _ if op.is_some() => {
                if binop_mode { break; }
                let op = op.unwrap();
                ps.consume_wsc_n(op.len());
                value = parse_binop(value, ps, &op)?;
            },
            '=' => { break; }, // '=' from parsing map keys
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

fn parse_expr(ps: &mut State) -> Result<VVal, ParseError> {
    let mut call = parse_call(ps, false)?;
    if ps.at_eof {
        return Ok(call);
    }

    while let Some(c) = ps.peek() {
        match c {
            '|' => {
                if ps.lookahead("|>") {
                    ps.consume();
                    ps.consume_wsc();

                    let call_right = parse_call(ps, false)?;

                    let new_call = make_to_call(ps, call);
                    new_call.push(call_right);
                    call = new_call;

                } else {
                    let push_front =
                        if ps.lookahead("||") { ps.consume(); true } else { false };
                    ps.consume_wsc();

                    let mut fn_expr = parse_call(ps, false)?;
                    if !is_call(&fn_expr) {
                        fn_expr = make_to_call(ps, fn_expr);
                    }

                    if push_front {
                        fn_expr.insert_at(2, call);
                    } else {
                        fn_expr.push(call);
                    }

                    call = fn_expr;
                }
            },
            _ => {
                break;
            }
        }
    }

    Ok(call)
}

fn parse_assignment(ps: &mut State, is_def: bool) -> Result<VVal, ParseError> {
    if ps.at_eof {
        return Err(ps.err(ParseErrorKind::EOF("assignment")));
    }

    let mut assign = VVal::vec();
    if is_def {
        assign.push(ps.syn_raw(Syntax::Def));
    } else {
        assign.push(ps.syn_raw(Syntax::Assign));
    }

    if is_def {
        if ps.consume_if_eq_wsc(':') {
            let key = parse_identifier(ps)?;
            if key == "global" {
                assign = ps.syn(Syntax::DefGlobRef);
            } else if key == "const" {
                assign = ps.syn(Syntax::DefConst);
            }
        }
    } else {
        if ps.consume_if_eq_wsc('*') {
            assign = ps.syn(Syntax::AssignRef);
        }
    }

    let mut destructuring = false;
    let ids = VVal::vec();

    match ps.expect_some(ps.peek())? {
        '(' => {
            ps.consume_wsc();
            destructuring = true;

            while let Some(c) = ps.peek() {
                if c == ')' { break; }
                ids.push(VVal::new_sym_mv(parse_identifier(ps)?));
                if !ps.consume_if_eq_wsc(',') { break; }
            }

            if ps.at_eof {
                return Err(ps.err(ParseErrorKind::EOF(
                    "destructuring assignment")));
            }

            if !ps.consume_if_eq_wsc(')') {
                return Err(ps.err(ParseErrorKind::UnexpectedToken(
                    ')', "At the end of destructuring assignment")));
            }
        },
        _ => { ids.push(VVal::new_sym_mv(parse_identifier(ps)?)); }
    }

    assign.push(ids);

    if !ps.consume_if_eq_wsc('=') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('=', "In assignment")));
    }

    assign.push(parse_expr(ps)?);

    if destructuring {
        assign.push(VVal::Bol(destructuring));
    }

    Ok(assign)
}

fn parse_stmt(ps: &mut State) -> Result<VVal, ParseError> {
    //println!("parse_stmt [{}]", ps.rest());
    match ps.peek() {
        Some(c) => {
            match c {
                '!' => {
                    ps.consume_wsc();
                    if ps.consume_if_eq_wsc('@') {
                        if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("special assignment"))); }
                        let id = parse_identifier(ps)?;
                        match &id[..] {
                            "wlambda" => {
                                let imp = ps.syn(Syntax::Import);
                                imp.push(VVal::Nul);
                                imp.push(VVal::new_sym("wlambda"));
                                Ok(imp)
                            },
                            "import" => {
                                let mut prefix =
                                    VVal::new_sym(&parse_identifier(ps)?);
                                ps.skip_ws_and_comments();
                                let name =
                                    if ps.peek().unwrap_or(';') == ';' {
                                        let p = prefix;
                                        prefix = VVal::Nul;
                                        p
                                    } else {
                                        ps.consume_if_eq_wsc('=');
                                        VVal::new_sym(&parse_identifier(ps)?)
                                    };

                                let imp = ps.syn(Syntax::Import);
                                imp.push(prefix);
                                imp.push(name);
                                Ok(imp)
                            },
                            "export" => {
                                let name = parse_identifier(ps)?;
                                ps.skip_ws_and_comments();
                                ps.consume_if_eq_wsc('=');
                                let expr = parse_expr(ps)?;
                                let exp = ps.syn(Syntax::Export);
                                exp.push(VVal::new_sym(&name));
                                exp.push(expr);
                                Ok(exp)
                            },
                            "dump_stack" => Ok(ps.syn(Syntax::DumpStack)),
                            _ => Err(ps.err(ParseErrorKind::BadKeyword(id.to_string(), "import or export"))),
                        }
                    } else {
                        parse_assignment(ps, true)
                    }
                },
                '.' => {
                    ps.consume_wsc();
                    parse_assignment(ps, false)
                }
                _   => parse_expr(ps),
            }
        },
        None => Err(ps.err(ParseErrorKind::EOF("statement")))
    }
}

/// Parses an arity definition for a function.
fn parse_arity(ps: &mut State) -> Result<VVal, ParseError> {
    if !ps.consume_if_eq_wsc('|') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('|', "When parsing an arity definition")));
    }

    if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("parsing arity definition"))); }

    let arity = if ps.expect_some(ps.peek())? != '|' {
        let min = parse_num(ps)?;
        if !min.is_int() {
            return Err(ps.err(ParseValueError::ExpectedMinArity));
        }

        let max = if ps.consume_if_eq_wsc('<') {
            let max = parse_num(ps)?;
            if !max.is_int() {
                return Err(ps.err(ParseValueError::ExpectedMaxArity));
            }
            max
        } else {
            min.clone()
        };

        let arity = VVal::vec();
        arity.push(min);
        arity.push(max);
        arity
    } else {
        let arity = VVal::vec();
        arity.push(VVal::Bol(true));
        arity.push(VVal::Bol(true));
        arity
    };

    if !ps.consume_if_eq_wsc('|') {
        return Err(ps.err(ParseErrorKind::UnexpectedToken('|', "When parsing an arity definition")));
    }

    Ok(arity)
}

/// This function parses the an optionally delimited block of WLambda statements.
///
/// ```rust
/// use wlambda::parser::{State, parse_block};
///
/// let code = "!a = 0; !b = 1; a + b";
/// let mut ps = State::new(&code, "somefilename");
///
/// // Parse a bare block without '{' ... '}' delimiters:
/// match parse_block(&mut ps, false) {
///     Ok(v)  => { println!("Result: {}", v.s()); },
///     Err(e) => { panic!(format!("ERROR: {}", e)); },
/// }
/// ```
///
/// The return value is a abstract syntax tree in a VVal data structure
/// that is ready for the `compiler` to be compiled. It consists mostly of
/// `VVal::Lst` and `VVal::Syn` nodes. The latter hold the position information
/// of the AST nodes.
pub fn parse_block(ps: &mut State, is_func: bool) -> Result<VVal, ParseError> {
    //println!("parse_block [{}]", ps.rest());
    if is_func {
        if !ps.consume_if_eq_wsc('{') {
            return Err(ps.err(ParseErrorKind::UnexpectedToken('{', "When parsing a block")));
        }
    }

    let block = ps.syn(Syntax::Block);

    if is_func && ps.lookahead("|") {
        block.push(parse_arity(ps)?);
    } else if is_func {
        block.push(VVal::Nul);
    }

    while let Some(c) = ps.peek() {
        if is_func { if c == '}' { break; } }

        let next_stmt = parse_stmt(ps)?;
        block.push(next_stmt);

        while ps.consume_if_eq_wsc(';') {
            while ps.consume_if_eq_wsc(';') { }
            if ps.at_eof || ps.consume_if_eq_wsc('}') { return Ok(block); }
            let next_stmt = parse_stmt(ps)?;
            block.push(next_stmt);
        }
    }

    if is_func {
        if ps.at_eof { return Err(ps.err(ParseErrorKind::EOF("parsing block"))); }
        if !ps.consume_if_eq_wsc('}') {
            return Err(ps.err(ParseErrorKind::UnexpectedToken('}', "When parsing a block")));
        }
    }

    Ok(block)
}

/// Facade function for an undelimited `parse_block`.
///
/// ```rust
/// use wlambda::parser::parse;
///
/// match parse("123; 456", "filenamehere") {
///     Ok(ast)  => println!("AST: {}", ast.s()),
///     Err(e) => { panic!(format!("ERROR: {}", e)); },
/// }
/// ```
pub fn parse<'a>(s: &str, filename: &str) -> Result<VVal, String> {
    let mut ps = State::new(s, filename);
    parse_block(&mut ps, false).map_err(|e| format!("{}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(s: &str) -> String {
        let mut ps = State::new(s, "<parser_test>");
        match parse_block(&mut ps, false) {
            Ok(v)  => v.s(),
            Err(e) => panic!(format!("Parse error: {}", e)),
        }
    }

    fn parse_error(s: &str) -> String {
        let mut ps = State::new(s,"<parser_test>");
        match parse_block(&mut ps, false) {
            Ok(v)  => panic!(format!("Expected error but got result: {} for input '{}'",
                                     v.s(), s)),
            Err(e) => format!("Parse error: {}", e),
        }
    }

    #[test]
    fn check_parse_numbers() {
        assert_eq!(parse("#comment \n10;#fom \n"),  "$[&Block,10]");
        assert_eq!(parse("10;"),       "$[&Block,10]");
        assert_eq!(parse("10.123;"),   "$[&Block,10.123]");
        assert_eq!(parse("-10;"),      "$[&Block,-10]");
        assert_eq!(parse("-0xFF;"),    "$[&Block,-255]");
        assert_eq!(parse("-0xFF.1;"),  "$[&Block,-255.0625]");
        assert_eq!(parse("-0xFF.9;"),  "$[&Block,-255.5625]");
        assert_eq!(parse("-0xFF.A;"),  "$[&Block,-255.625]");
        assert_eq!(parse("-0xFF.F;"),  "$[&Block,-255.9375]");
    }

    #[test]
    fn check_parse_sym() {
        assert_eq!(parse(":\"foo bar\""),
                   "$[&Block,$[&Key,:\"foo bar\"]]");
        assert_eq!(parse("foo :bar -2.3 2.3"),
                   "$[&Block,$[&Call,$[&Var,:\"foo\"],$[&Key,:\"bar\"],-2.3,2.3]]");
        assert_eq!(parse("foo :bar -x 2"),
                   "$[&Block,$[&Call,$[&Var,:\"foo\"],$[&BinOpSub,$[&Key,:\"bar\"],$[&Var,:\"x\"]],2]]");
        assert_eq!(parse("foo :bar -2 2"),
                   "$[&Block,$[&Call,$[&Var,:\"foo\"],$[&Key,:\"bar\"],-2,2]]");
    }

    #[test]
    fn check_parse_vec() {
        assert_eq!(parse("$[10];"),
                   "$[&Block,$[&Lst,10]]");
        assert_eq!(parse("$[10, 11.23, -30, -0xFF];"),
                   "$[&Block,$[&Lst,10,11.23,-30,-255]]");
        assert_eq!(parse("$[10, $[1,2,3], 11.23, -30, -0xFF];"),
                   "$[&Block,$[&Lst,10,$[&Lst,1,2,3],11.23,-30,-255]]");
    }

    #[test]
    fn check_parse_last_commas() {
        assert_eq!(parse("$[10,]"),         "$[&Block,$[&Lst,10]]");
        assert_eq!(parse("$[10,20,]"),      "$[&Block,$[&Lst,10,20]]");
        assert_eq!(parse("${a=1,b=2,}"),    "$[&Block,$[&Map,$[:\"a\",1],$[:\"b\",2]]]");
        assert_eq!(parse("${a=1,}"),        "$[&Block,$[&Map,$[:\"a\",1]]]");
        assert_eq!(parse("f[1,]"),          "$[&Block,$[&Call,$[&Var,:\"f\"],1]]");
    }

    #[test]
    fn check_calls() {
        assert_eq!(parse("10"),         "$[&Block,10]");
        assert_eq!(parse("10;"),        "$[&Block,10]");
        assert_eq!(parse("10; 20"),     "$[&Block,10,20]");
        assert_eq!(parse("10;;; 20"),   "$[&Block,10,20]");
        assert_eq!(parse("10;;; 20;"),  "$[&Block,10,20]");
        assert_eq!(parse("10 20;"),     "$[&Block,$[&Call,10,20]]");
        assert_eq!(parse("(10) 20;"),   "$[&Block,$[&Call,10,20]]");
    }

    #[test]
    fn check_expr() {
        assert_eq!(parse("10 20 30"),
                   "$[&Block,$[&Call,10,20,30]]");
        assert_eq!(parse("10 20 30 40"),
                   "$[&Block,$[&Call,10,20,30,40]]");
        assert_eq!(parse("10 || 20 30"),
                   "$[&Block,$[&Call,20,10,30]]");
        assert_eq!(parse("10 | 20 30"),
                   "$[&Block,$[&Call,20,30,10]]");
        assert_eq!(parse("10 20 | 30 40"),
                   "$[&Block,$[&Call,30,40,$[&Call,10,20]]]");
        assert_eq!(parse("10 20 || 30 40"),
                   "$[&Block,$[&Call,30,$[&Call,10,20],40]]");
        assert_eq!(parse("10 20 | 30 40 | 50"),
                   "$[&Block,$[&Call,50,$[&Call,30,40,$[&Call,10,20]]]]");
        assert_eq!(parse("10 | 20 | 30 | 40"),
                   "$[&Block,$[&Call,40,$[&Call,30,$[&Call,20,10]]]]");
        assert_eq!(parse("10[] | 20 | 30 | 40"),
                   "$[&Block,$[&Call,40,$[&Call,30,$[&Call,20,$[&Call,10]]]]]");
        assert_eq!(parse("10[][] | 20 | 30 | 40"),
                   "$[&Block,$[&Call,40,$[&Call,30,$[&Call,20,$[&Call,$[&Call,10]]]]]]");
        assert_eq!(parse("(10 | 20) | (foo(bar))"),
                   "$[&Block,$[&Call,$[&Var,:\"foo\"],$[&Var,:\"bar\"],$[&Call,20,10]]]");
        assert_eq!(parse("10 ~ 20 ~ 30 ~ 40"),
                   "$[&Block,$[&Call,10,$[&Call,20,$[&Call,30,40]]]]");
        assert_eq!(parse("10 | 20"),                  "$[&Block,$[&Call,20,10]]");
        assert_eq!(parse("10 (1 2) (3 4 5) (6 (7))"), "$[&Block,$[&Call,10,$[&Call,1,2],$[&Call,3,4,5],$[&Call,6,7]]]");
        assert_eq!(parse("10[]"),                     "$[&Block,$[&Call,10]]");
        assert_eq!(parse("10[20, 30]"),               "$[&Block,$[&Call,10,20,30]]");
        assert_eq!(parse("10 x[20, 30]"),             "$[&Block,$[&Call,10,$[&Call,$[&Var,:\"x\"],20,30]]]");
        assert_eq!(parse("10 x[20, 30] | 50"),        "$[&Block,$[&Call,50,$[&Call,10,$[&Call,$[&Var,:\"x\"],20,30]]]]");
        assert_eq!(parse("(10).(\"a\" \"b\")"),       "$[&Block,$[&GetKey,10,$[&Call,$[&Str,\"a\"],$[&Str,\"b\"]]]]");
        assert_eq!(parse("(10).(\"ab\")"),            "$[&Block,$[&GetSym,10,\"ab\"]]");
        assert_eq!(parse("(10).a"),                   "$[&Block,$[&GetSym,10,:\"a\"]]");
        assert_eq!(parse("a.b"),                      "$[&Block,$[&GetSym,$[&Var,:\"a\"],:\"b\"]]");
        assert_eq!(parse("10 a.b"),                   "$[&Block,$[&Call,10,$[&GetSym,$[&Var,:\"a\"],:\"b\"]]]");
        assert_eq!(parse("(10).(20)"),                "$[&Block,$[&GetIdx,10,20]]");
        assert_eq!(parse("10.20 30"),                 "$[&Block,$[&Call,10.2,30]]");
        assert_eq!(parse("10 20 ~ 30 ~ 40 ~ 50"),     "$[&Block,$[&Call,10,20,$[&Call,30,$[&Call,40,50]]]]");
        assert_eq!(parse("10 20 ~ 30 40 ~ 40 1 2 3 ~ 50 60"),  "$[&Block,$[&Call,10,20,$[&Call,30,40,$[&Call,40,1,2,3,$[&Call,50,60]]]]]");
        assert_eq!(parse("10[10[1,2,3 foo] ~ 4]"),    "$[&Block,$[&Call,10,$[&Call,$[&Call,10,1,2,$[&Call,3,$[&Var,:\"foo\"]]],4]]]");
        assert_eq!(parse("foo.b.c.d"),                "$[&Block,$[&GetSym3,$[&Var,:\"foo\"],:\"b\",:\"c\",:\"d\"]]");
        assert_eq!(parse("foo.b.c.d[]"),              "$[&Block,$[&Call,$[&GetSym3,$[&Var,:\"foo\"],:\"b\",:\"c\",:\"d\"]]]");
        assert_eq!(parse("foo.b.c.d[1,2,3]"),         "$[&Block,$[&Call,$[&GetSym3,$[&Var,:\"foo\"],:\"b\",:\"c\",:\"d\"],1,2,3]]");
        assert_eq!(parse("foo.b.c.d 1 2 3"),          "$[&Block,$[&Call,$[&GetSym3,$[&Var,:\"foo\"],:\"b\",:\"c\",:\"d\"],1,2,3]]");
        assert_eq!(parse("(foo.b.c.d) 1 2 3"),        "$[&Block,$[&Call,$[&GetSym3,$[&Var,:\"foo\"],:\"b\",:\"c\",:\"d\"],1,2,3]]");
        assert_eq!(parse("foo.a = 10"),               "$[&Block,$[&SetKey,$[&Var,:\"foo\"],$[&Key,:\"a\"],10]]");
        assert_eq!(parse("foo.a = 10 | 20"),          "$[&Block,$[&SetKey,$[&Var,:\"foo\"],$[&Key,:\"a\"],$[&Call,20,10]]]");
        assert_eq!(parse("foo.a = 10 ~ 20"),          "$[&Block,$[&SetKey,$[&Var,:\"foo\"],$[&Key,:\"a\"],$[&Call,10,20]]]");
        assert_eq!(parse("4 == 5 ~ 10"),              "$[&Block,$[&Call,$[&Var,:\"==\"],4,$[&Call,5,10]]]");
        assert_eq!(parse("foo.(i) = 10"),             "$[&Block,$[&SetKey,$[&Var,:\"foo\"],$[&Var,:\"i\"],10]]");
        assert_eq!(parse("foo :x :y 10"),             "$[&Block,$[&Call,$[&Var,:\"foo\"],$[&Key,:\"x\"],$[&Key,:\"y\"],10]]");
    }

    #[test]
    fn check_expr_err() {
        assert_eq!(parse_error("foo.a[] = 10"),
            "Parse error: error[1,9:<parser_test>] Expected literal value, sub expression, block, key or identifier at code \'= 10\'");
    }

    #[test]
    fn check_identifier() {
        assert_eq!(parse("+"),          "$[&Block,$[&Var,:\"+\"]]");
        assert_eq!(parse("-"),          "$[&Block,$[&Var,:\"-\"]]");
        assert_eq!(parse("+ 10 20"),    "$[&Block,$[&Call,$[&Var,:\"+\"],10,20]]");
        assert_eq!(parse("13 + 10 20"), "$[&Block,$[&Call,$[&BinOpAdd,13,10],20]]");
        assert_eq!(parse("13 + 10 == 23"),
                                        "$[&Block,$[&Call,$[&Var,:\"==\"],$[&BinOpAdd,13,10],23]]");
        assert_eq!(parse("(+ 12 ~ - 24 23) == 13"),
           "$[&Block,$[&Call,$[&Var,:\"==\"],$[&Call,$[&Var,:\"+\"],12,$[&Call,$[&Var,:\"-\"],24,23]],13]]");
        assert_eq!(parse("_"),          "$[&Block,$[&Var,:\"_\"]]");
        assert_eq!(parse("ten"),        "$[&Block,$[&Var,:\"ten\"]]");
        assert_eq!(parse("tenäß foo"),  "$[&Block,$[&Call,$[&Var,:\"tenäß\"],$[&Var,:\"foo\"]]]");
    }

    #[test]
    fn check_primitives() {
        assert_eq!(parse("$n"),         "$[&Block,$n]");
        assert_eq!(parse("$none"),      "$[&Block,$n]");
        assert_eq!(parse("$t"),         "$[&Block,$true]");
        assert_eq!(parse("$true"),      "$[&Block,$true]");
        assert_eq!(parse("$f"),         "$[&Block,$false]");
        assert_eq!(parse("$false"),     "$[&Block,$false]");
    }

    #[test]
    fn check_binops() {
        assert_eq!(parse("20 * 10"),                "$[&Block,$[&BinOpMul,20,10]]");
        assert_eq!(parse("20 + 10"),                "$[&Block,$[&BinOpAdd,20,10]]");
        assert_eq!(parse("20 - 10"),                "$[&Block,$[&BinOpSub,20,10]]");
        assert_eq!(parse("20 / 10"),                "$[&Block,$[&BinOpDiv,20,10]]");
        assert_eq!(parse("20 % 10"),                "$[&Block,$[&BinOpMod,20,10]]");
        assert_eq!(parse("20 > 10"),                "$[&Block,$[&BinOpGt,20,10]]");
        assert_eq!(parse("20 < 10"),                "$[&Block,$[&BinOpLt,20,10]]");
        assert_eq!(parse("20 <= 10"),               "$[&Block,$[&BinOpLe,20,10]]");
        assert_eq!(parse("20 >= 10"),               "$[&Block,$[&BinOpGe,20,10]]");
        assert_eq!(parse("40 20 * 10"),             "$[&Block,$[&Call,40,$[&BinOpMul,20,10]]]");
        assert_eq!(parse("40 20 * 10 30"),          "$[&Block,$[&Call,40,$[&BinOpMul,20,10],30]]");
        assert_eq!(parse("40 20 * 10[]"),           "$[&Block,$[&Call,40,$[&BinOpMul,20,$[&Call,10]]]]");
        assert_eq!(parse("40 20[] * 10[]"),         "$[&Block,$[&Call,40,$[&BinOpMul,$[&Call,20],$[&Call,10]]]]");
        assert_eq!(parse("20[] * 10[]"),            "$[&Block,$[&BinOpMul,$[&Call,20],$[&Call,10]]]");
        assert_eq!(parse("10 - 20 * 30"),           "$[&Block,$[&BinOpSub,10,$[&BinOpMul,20,30]]]");
        assert_eq!(parse("10 * 20 - 30"),           "$[&Block,$[&BinOpSub,$[&BinOpMul,10,20],30]]");
        assert_eq!(parse("10 * 20 - 30 * 2"),       "$[&Block,$[&BinOpSub,$[&BinOpMul,10,20],$[&BinOpMul,30,2]]]");
        assert_eq!(parse("10 * 20 * 30"),           "$[&Block,$[&BinOpMul,$[&BinOpMul,10,20],30]]");
        assert_eq!(parse("10 - 20 - 30 - 40"),      "$[&Block,$[&BinOpSub,$[&BinOpSub,$[&BinOpSub,10,20],30],40]]");
        assert_eq!(parse("10 - 20 - (30 - 40)"),    "$[&Block,$[&BinOpSub,$[&BinOpSub,10,20],$[&BinOpSub,30,40]]]");

        assert_eq!(parse("$t &and $f"),                "$[&Block,$[&And,$true,$false]]");
        assert_eq!(parse("1 &and 2 &and 3 &and 4"),    "$[&Block,$[&And,$[&And,$[&And,1,2],3],4]]");
        assert_eq!(parse("$t &or $f"),                 "$[&Block,$[&Or,$true,$false]]");
        assert_eq!(parse("$t &and $f &or $f &and $f"), "$[&Block,$[&Or,$[&And,$true,$false],$[&And,$false,$false]]]");
    }

    #[test]
    fn check_assignments() {
        assert_eq!(parse("!x=10;"),              "$[&Block,$[&Def,$[:\"x\"],10]]");
        assert_eq!(parse("! x = 10 ;"),          "$[&Block,$[&Def,$[:\"x\"],10]]");
        assert_eq!(parse("! x = 10"),            "$[&Block,$[&Def,$[:\"x\"],10]]");
        assert_eq!(parse("!:global (y,x) = @"),  "$[&Block,$[&DefGlobRef,$[:\"y\",:\"x\"],$[&Var,:\"@\"],$true]]");
        assert_eq!(parse(". (a,b) = 10"),        "$[&Block,$[&Assign,$[:\"a\",:\"b\"],10,$true]]");
    }

    #[test]
    fn check_func() {
        assert_eq!(parse("{}"),           "$[&Block,$[&Func,$n,$n]]");
        assert_eq!(parse("{10;}"),        "$[&Block,$[&Func,$n,$n,10]]");
        assert_eq!(parse("{10;;;}"),      "$[&Block,$[&Func,$n,$n,10]]");
        assert_eq!(parse("{10; 20}"),     "$[&Block,$[&Func,$n,$n,10,20]]");
        assert_eq!(parse("{ 10 } { }"),   "$[&Block,$[&Call,$[&Func,$n,$n,10],$[&Func,$n,$n]]]");
        assert_eq!(parse("\\:x { }"),     "$[&Block,$[&Func,:\"x\",$n]]");
        assert_eq!(parse("\\ p 1 | 20 ~ 30"),  "$[&Block,$[&Func,$n,$n,$[&Call,20,30,$[&Call,$[&Var,:\"p\"],1]]]]");
    }

    #[test]
    fn check_map() {
        assert_eq!(parse("${a=10}"),   "$[&Block,$[&Map,$[:\"a\",10]]]");
        assert_eq!(parse("${:a=10}"),  "$[&Block,$[&Map,$[$[&Key,:\"a\"],10]]]");
    }

    #[test]
    fn check_str() {
        assert_eq!(parse("\"foo\""),       "$[&Block,$[&Str,\"foo\"]]");
        assert_eq!(parse("$q$foo$"),       "$[&Block,$[&Str,\"foo\"]]");
        assert_eq!(parse("\"fo\0o\""),     "$[&Block,$[&Str,\"fo\\0o\"]]");
        assert_eq!(parse("\"fo\no\""),     "$[&Block,$[&Str,\"fo\\no\"]]");
        assert_eq!(parse("\"fo\ro\""),     "$[&Block,$[&Str,\"fo\\ro\"]]");
        assert_eq!(parse("\"fo\\\"o\""),   "$[&Block,$[&Str,\"fo\\\"o\"]]");
        assert_eq!(parse("\"fo\x05o\""),  "$[&Block,$[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{9f}\""),   "$[&Block,$[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{0009f}\""),   "$[&Block,$[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{09f}\""),   "$[&Block,$[&Str,\"fo\\x05o\\u{9f}\"]]");
        assert_eq!(parse("\"fo\x05o\\u{2400}\""), "$[&Block,$[&Str,\"fo\\x05o␀\"]]");
        assert_eq!(parse("$q foo "),        "$[&Block,$[&Str,\"foo\"]]");
        assert_eq!(parse("$q[foo]"),        "$[&Block,$[&Str,\"foo\"]]");
        assert_eq!(parse("$q(foo)"),        "$[&Block,$[&Str,\"foo\"]]");
        assert_eq!(parse("$q{foo}"),        "$[&Block,$[&Str,\"foo\"]]");
        assert_eq!(parse("$q<foo>"),        "$[&Block,$[&Str,\"foo\"]]");

        assert_eq!(parse("$b\"\\u{2400}\""),       "$[&Block,$[&Str,$b\"\\xE2\\x90\\x80\"]]");
        assert_eq!(parse("$Q'foo'"),               "$[&Block,$[&Str,$b\"foo\"]]");
        assert_eq!(parse("$b\"\\x00\\xFF\\xEB\""), "$[&Block,$[&Str,$b\"\\0\\xFF\\xEB\"]]");
    }

    #[test]
    fn check_parse_field_access() {
        assert_eq!(parse("foo.(bar) == 2019"), "$[&Block,$[&Call,$[&Var,:\"==\"],$[&GetKey,$[&Var,:\"foo\"],$[&Var,:\"bar\"]],2019]]");
        assert_eq!(parse("o.x[]"),             "$[&Block,$[&Call,$[&GetSym,$[&Var,:\"o\"],:\"x\"]]]");

        assert_eq!(parse("o.1"),                       "$[&Block,$[&GetIdx,$[&Var,:\"o\"],1]]");
        assert_eq!(parse("o.1.2"),                     "$[&Block,$[&GetIdx2,$[&Var,:\"o\"],1,2]]");
        assert_eq!(parse("o.1.2.3"),                   "$[&Block,$[&GetIdx3,$[&Var,:\"o\"],1,2,3]]");
        assert_eq!(parse("o.x"),                       "$[&Block,$[&GetSym,$[&Var,:\"o\"],:\"x\"]]");
        assert_eq!(parse("o.x.y"),                     "$[&Block,$[&GetSym2,$[&Var,:\"o\"],:\"x\",:\"y\"]]");
        assert_eq!(parse("o.x.y.z"),                   "$[&Block,$[&GetSym3,$[&Var,:\"o\"],:\"x\",:\"y\",:\"z\"]]");
        assert_eq!(parse("o.x.(\"y\").z"),             "$[&Block,$[&GetSym3,$[&Var,:\"o\"],:\"x\",\"y\",:\"z\"]]");
        assert_eq!(parse("o.(\"x\")"),                 "$[&Block,$[&GetSym,$[&Var,:\"o\"],\"x\"]]");
        assert_eq!(parse("o.(\"x\").(\"y\")"),         "$[&Block,$[&GetSym2,$[&Var,:\"o\"],\"x\",\"y\"]]");
        assert_eq!(parse("o.(\"x\").(\"y\").(\"z\")"), "$[&Block,$[&GetSym3,$[&Var,:\"o\"],\"x\",\"y\",\"z\"]]");
        assert_eq!(parse("o.(1 \"x\")"),                     "$[&Block,$[&GetKey,$[&Var,:\"o\"],$[&Call,1,$[&Str,\"x\"]]]]");
        assert_eq!(parse("o.(1 \"x\").(1 \"y\")"),           "$[&Block,$[&GetKey2,$[&Var,:\"o\"],$[&Call,1,$[&Str,\"x\"]],$[&Call,1,$[&Str,\"y\"]]]]");
        assert_eq!(parse("o.(1 \"x\").(1 \"y\").(1 \"z\")"), "$[&Block,$[&GetKey3,$[&Var,:\"o\"],$[&Call,1,$[&Str,\"x\"]],$[&Call,1,$[&Str,\"y\"]],$[&Call,1,$[&Str,\"z\"]]]]");
    }

    #[test]
    fn check_err_val() {
        assert_eq!(parse("$e 10 20 30"),        "$[&Block,$[&Err,$[&Call,10,20,30]]]");
        assert_eq!(parse("$e 10 20 30 | 20"),   "$[&Block,$[&Err,$[&Call,20,$[&Call,10,20,30]]]]");
    }

    #[test]
    fn check_parse_ref_deref() {
        assert_eq!(parse("$& 1"),         "$[&Block,$[&WRef,1]]");
        assert_eq!(parse("$&$[1,2]"),     "$[&Block,$[&WRef,$[&Lst,1,2]]]");
        assert_eq!(parse("$&${z=1}"),     "$[&Block,$[&WRef,$[&Map,$[:\"z\",1]]]]");
        assert_eq!(parse("$&& 1"),        "$[&Block,$[&Ref,1]]");
        assert_eq!(parse("$&&$[1,2]"),    "$[&Block,$[&Ref,$[&Lst,1,2]]]");
        assert_eq!(parse("$&&${z=1}"),    "$[&Block,$[&Ref,$[&Map,$[:\"z\",1]]]]");
        assert_eq!(parse("$*${z=1}.f=1"), "$[&Block,$[&SetKey,$[&Deref,$[&Map,$[:\"z\",1]]],$[&Key,:\"f\"],1]]");
        assert_eq!(parse("$*xxx.f=1"),    "$[&Block,$[&SetKey,$[&Deref,$[&Var,:\"xxx\"]],$[&Key,:\"f\"],1]]");
        assert_eq!(parse("$*xxx.f"),      "$[&Block,$[&GetSym,$[&Deref,$[&Var,:\"xxx\"]],:\"f\"]]");
    }

    #[test]
    fn check_self() {
        assert_eq!(parse("$s"),    "$[&Block,$[&SelfObj]]");
        assert_eq!(parse("$self"), "$[&Block,$[&SelfObj]]");
        assert_eq!(parse("$d"),    "$[&Block,$[&SelfData]]");
        assert_eq!(parse("$data"), "$[&Block,$[&SelfData]]");
    }

    #[test]
    fn check_backtick_ident() {
        assert_eq!(parse("` `"),        "$[&Block,$[&Var,:\" \"]]");
        assert_eq!(parse("`\\``"),      "$[&Block,$[&Var,:\"`\"]]");
        assert_eq!(parse("`\\\"`"),     "$[&Block,$[&Var,:\"\"\"]]");
        assert_eq!(parse("`\"`"),       "$[&Block,$[&Var,:\"\"\"]]");
        assert_eq!(parse("!` ` = 10;"), "$[&Block,$[&Def,$[:\" \"],10]]");
    }

    #[test]
    fn check_apply() {
        assert_eq!(parse("fo[[@]]"),            "$[&Block,$[&Apply,$[&Var,:\"fo\"],$[&Var,:\"@\"]]]");
        assert_eq!(parse("fo[[$[1,2,3]]]"),     "$[&Block,$[&Apply,$[&Var,:\"fo\"],$[&Lst,1,2,3]]]");
        assert_eq!(parse("obj.1.field[[_]]"),   "$[&Block,$[&Apply,$[&GetSym,$[&GetIdx,$[&Var,:\"obj\"],1],:\"field\"],$[&Var,:\"_\"]]]");
        assert_eq!(parse("obj.1.(\"field\")[[_]]"),   "$[&Block,$[&Apply,$[&GetSym,$[&GetIdx,$[&Var,:\"obj\"],1],\"field\"],$[&Var,:\"_\"]]]");
    }

    #[test]
    fn check_right_call() {
        assert_eq!(parse("10 |> 20"),                "$[&Block,$[&Call,10,20]]");
        assert_eq!(parse("10 20"),                   "$[&Block,$[&Call,10,20]]");
        assert_eq!(parse("10 |> 20 |> 30"),          "$[&Block,$[&Call,$[&Call,10,20],30]]");
        assert_eq!(parse("10 20 |> 30"),             "$[&Block,$[&Call,$[&Call,10,20],30]]");

        assert_eq!(parse("10 20 |> 20 30 40"),       "$[&Block,$[&Call,$[&Call,10,20],$[&Call,20,30,40]]]");

        assert_eq!(parse("10 11 |> 20"),             "$[&Block,$[&Call,$[&Call,10,11],20]]");
        assert_eq!(parse("10 11 |> 20 |> 30"),       "$[&Block,$[&Call,$[&Call,$[&Call,10,11],20],30]]");
        assert_eq!(parse("10 11 |> 20 21 |> 30"),    "$[&Block,$[&Call,$[&Call,$[&Call,10,11],$[&Call,20,21]],30]]");
        assert_eq!(parse("10 11 |> 20 21 |> 30 31"), "$[&Block,$[&Call,$[&Call,$[&Call,10,11],$[&Call,20,21]],$[&Call,30,31]]]");
    }

    #[test]
    fn check_const() {
        assert_eq!(parse("!:const X = 32;"),                "$[&Block,$[&DefConst,$[:\"X\"],32]]");
        assert_eq!(parse("!:const X = 32.4;"),              "$[&Block,$[&DefConst,$[:\"X\"],32.4]]");
        assert_eq!(parse("!:const X = :XX;"),               "$[&Block,$[&DefConst,$[:\"X\"],$[&Key,:\"XX\"]]]");
        assert_eq!(parse("!:const X = \"fo\";"),            "$[&Block,$[&DefConst,$[:\"X\"],$[&Str,\"fo\"]]]");
        assert_eq!(parse("!:const X = $[120];"),            "$[&Block,$[&DefConst,$[:\"X\"],$[&Lst,120]]]");
        assert_eq!(parse("!:const X = ${a=10};"),           "$[&Block,$[&DefConst,$[:\"X\"],$[&Map,$[:\"a\",10]]]]");
        assert_eq!(parse("!:const (A,B,X) = $[1,3,4];"),    "$[&Block,$[&DefConst,$[:\"A\",:\"B\",:\"X\"],$[&Lst,1,3,4],$true]]");
    }

    #[test]
    fn check_nvec() {
        assert_eq!(parse("$i(30, 18, 5)"),              "$[&Block,$[&IVec,30,18,5]]");
        assert_eq!(parse("$i(30,18,5)"),                "$[&Block,$[&IVec,30,18,5]]");
        assert_eq!(parse("$i(0b100,0xC,0o10)"),         "$[&Block,$[&IVec,4,12,8]]");
        assert_eq!(parse("$i(0b101 +  1,  0xF *  4)"),  "$[&Block,$[&IVec,$[&BinOpAdd,5,1],$[&BinOpMul,15,4]]]");
        //assert_eq!(parse("$i(0b100+2,0xC*4)"),          "$[&Block,$[&IVec,$[&BinOpAdd,4,2],$[&BinOpMul,12,4]]]");
        assert_eq!(parse("$f(1.2, 3.4, 5.6, 7.8)"),     "$[&Block,$[&FVec,1.2,3.4,5.6,7.8]]");
        assert_eq!(
            parse("$f(1/2, 1/3, 1/4, 1/5)"),
            "$[&Block,$[&FVec,$[&BinOpDiv,1,2],$[&BinOpDiv,1,3],$[&BinOpDiv,1,4],$[&BinOpDiv,1,5]]]"
        );
    }
}
