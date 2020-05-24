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

    ident_start   = ( ?alphabetic? | "_" | "@" | "?" )
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
    selector      = "S", ?any character as quote?, selector_rs_syntax, ?any character as quote?
                    (* parses substring like 'q', but constructs a
                       selector_rs_syntax matcher at compile time *)
                  ;
    pattern       = "r", ?any character as quote?, selector_rs_pattern_syntax, ?any character as quote?
                    (* parses substring like 'q', but constructs a
                       pattern matcher at compile time *)
                  ;
    struct_match  = "P", expr   (* compiles expr as structure matcher function.
                                   If called, it matches the first argument against
                                   the literal structure and returns a map of
                                   matched variables. If nothing matches $none
                                   is returned. *)
    list_expr     = "*", expr   (* splices the vector result of 'expr'
                                   into the currently parsed list *)
                  | expr
                  ;
    list          = "[", [ list_expr, { ",", list_expr }, [ "," ] ],"]"
                  ;
    map_expr      = (ident | expr), "=", expr
                  | "*", expr   (* splices the map result of 'expr'
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
    none          = "n" | "none"
                  ;
    code_string   = ("c" | "code" ), expr
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
    import        = "@import", ident, [ ident ]
                  ;
    export        = "@export", ident, [ "=" ], expr
                  ;
    capture_ref   = ":", var
                  ;
    deref         = "*", value
                  ;
    special_value = byte_string
                  | quote_string
                  | code_string
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
                  | selector
                  | pattern
                  | struct_match
                  | "\"             (* The global variable with the name "\" *)
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
                  | "=>"              (* pair constructor *)
                  | "&>"              (* call rhs with lhs operator *)
                  | "<&"              (* call lhs with rhs operator *)
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
    call_no_ops   = value, { arg_list | field_access }
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
    import        = "!", "@import", symbol, [ [ "=" ], symbol ]
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

## Special Forms

There are certain calls that are handled by the compiler differently.

- `? _condition_ _then-block-or-expr_ [_else-block-or-expr_]`
- `while _condition_ _block-or-expr_`
- `iter _var_ _value-expr_ _block-or-expr_`
- `next _x_`
- `break`
- `match _value-expr_ $p(structure_pattern, branch_block) ... [ branch_block ]

## Structure Pattern Syntax

This the the compiletime syntax that is understood by the
structure patterns that are used by `$P ...` and `match`.

- `$P`, `$P1`, `$P2`, ... in the following table stands for a structure pattern expression.
- All other tokens or values stand for themself.

| WLambda Value | Semantics |
|-|-|
| `x $P $P1 ... $Pn`     | Assign the value that matched $P, $P1 or $Pn to the variable `x`. |
| `_* $P`                | Matches 0 or N appearances of the pattern $P in a vector. |
| `_+ $P`                | Matches 1 or N appearances of the pattern $P in a vector. |
| `_? $P`                | Matches 0 or 1 appearances of the pattern $P in a vector. |
| `$P1 &or $P2`          | Matches if $P1 or $P2 matches. |
| `$P1 &and $P2`         | Matches if $P1 and $P2 matches. |
| `$[$P1, $P2, ...]`     | Matches a vector. |
| `${ key = $P1, ...}`   | Matches a map. |
| `$p($P1, $P2)`         | Matches a pair. |
| `$i($P1, ...)`         | Matches an integer vector. |
| `$f($P1, ...)`         | Matches a float vector. |
| `$o($P)`               | Matches an optional where the value matches $P. |
| `$e $P`                | Matches an error value that matches $P. |
| `x`                    | Matches any value and assigns it to the variable `x`. |
| `?`                    | Matches any value, but does not assign it. |
| literal values         | Literal values like booleans, strings, symbols and numbers match their value. |

*/


use crate::vval::VVal;
use crate::vval::Syntax;

pub mod state;

pub use state::State;
pub use state::{ParseValueError, ParseNumberError, ParseError, ParseErrorKind};
use state::StrPart;

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
fn addchr(v: &mut Vec<u8>, s: &mut String, b: bool, c: char) {
    if b { add_c_to_vec(v, c); }
    else { s.push(c); }
}

/// Parses a quoted byte vector or string.
fn parse_q_string(ps: &mut State, bytes: bool) -> Result<VVal, ParseError> {
    let vec = ps.syn(Syntax::Str);

    if bytes {
        vec.push(VVal::new_byt(
            parse_quoted(ps, Vec::new(), |v, c| add_c_to_vec(v, c))?));
    } else {
        vec.push(VVal::new_str_mv(
            parse_quoted(ps, String::new(), |v, c| v.push(c))?));
    }

    Ok(vec)
}

/// Parsers a WLambda special quoted part of code.
fn parse_quoted<F, V>(ps: &mut State, mut v: V, add_char: F) -> Result<V, ParseError>
    where F: Fn(&mut V, char)
{
    if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("string"))); }

    let quote_char = ps.expect_some(ps.peek())?;
    ps.consume();

    let quote_char = match quote_char {
        '[' => ']',
        '(' => ')',
        '{' => '}',
        '<' => '>',
        _ => quote_char
    };

    let mut cont_next = false;
    while ps.peek().unwrap_or(quote_char) != quote_char {
        let c = ps.expect_some(ps.peek())?;
        ps.consume();
        add_char(&mut v, c);
    }

    if !ps.consume_if_eq(quote_char) {
        return Err(ps.err(
            ParseErrorKind::ExpectedToken(quote_char, "quote string end")));
    }

    ps.skip_ws_and_comments();

    Ok(v)
}

enum NVecKind {
    Int,
    Flt,
}

/// Parses the part of a numerical vector that comes after the declaration,
/// in `$i(1, 2, 3)` `$i` is the declaration.
fn parse_nvec_body(ps: &mut State, kind: NVecKind) -> Result<VVal, ParseError> {
    match ps.expect_some(ps.peek())? {
        '(' => {
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
                Err(ps.err(ParseErrorKind::ExpectedToken(')', "numerical vector end")))
            } else if dim > 4 || dim < 1 {
                Err(ps.err(ParseValueError::VectorLength))
            } else {
                Ok(vec)
            }
        },
        _ => Err(ps.err(ParseErrorKind::ExpectedToken('(', "numerical vector start"))),
    }
}

pub fn parse_2hex(ps: &mut State) -> Result<u8, ParseError> {
    let hex = ps.peek2();
    if let Some(h) = hex {
        let h = h.to_string();
        ps.consume();
        ps.consume();
        if let Ok(cn) = u8::from_str_radix(&h, 16) {
            Ok(cn)
        } else {
            Err(ps.err(ParseErrorKind::BadEscape("Bad hex escape in string")))
        }
    } else {
        Err(ps.err(ParseErrorKind::EOF("string hex escape")))
    }
}

pub fn parse_unicode_hex(ps: &mut State) -> Result<char, ParseError> {
    if !ps.consume_if_eq('{') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('{', "unicode escape start")));
    }

    let uh = ps.take_while(|c| c.is_digit(16));

    let c =
        if let Ok(cn) = u32::from_str_radix(&uh.to_string(), 16) {
            if let Some(c) = std::char::from_u32(cn) {
                c
            } else {
                return Err(ps.err(ParseErrorKind::BadEscape(
                    "Bad char in unicode escape in string"
                )));
            }
        } else {
            return Err(ps.err(ParseErrorKind::BadEscape(
                "Bad unicode hex escape in string"
            )));
        };

    if !ps.consume_if_eq('}') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('}', "unicode escape end")));
    }

    Ok(c)
}

#[derive(Debug, Clone, Copy)]
pub enum EscSeqValue {
    Char(char),
    Byte(u8),
}

pub fn parse_str_backslash(ps: &mut State) -> Result<EscSeqValue, ParseError>
{
    match ps.expect_some(ps.peek())? {
        'n' => { ps.consume(); Ok(EscSeqValue::Char('\n')) },
        'r' => { ps.consume(); Ok(EscSeqValue::Char('\r')) },
        't' => { ps.consume(); Ok(EscSeqValue::Char('\t')) },
        '0' => { ps.consume(); Ok(EscSeqValue::Char('\0')) },
        'x' => { ps.consume(); Ok(EscSeqValue::Byte(parse_2hex(ps)?)) },
        'u' => { ps.consume(); Ok(EscSeqValue::Char(parse_unicode_hex(ps)?)) },
        c   => { ps.consume(); Ok(EscSeqValue::Char(c)) },
    }
}

/// Parsers a WLambda string or byte buffer.
fn parse_string(ps: &mut State, bytes: bool) -> Result<VVal, ParseError> {
    if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("string"))); }

    if !ps.consume_if_eq('"') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('\"', "string start")));
    }

    let vec = ps.syn(Syntax::Str);

    let mut s = String::from("");
    let mut v : Vec<u8> = Vec::new();

    while ps.peek().unwrap_or('"') != '"' {
        let c = ps.expect_some(ps.peek())?;
        match c {
            '\\' => {
                ps.consume();
                match parse_str_backslash(ps)? {
                    EscSeqValue::Char(c) => addchr(&mut v, &mut s, bytes, c),
                    EscSeqValue::Byte(b) =>
                        if bytes { v.push(b); }
                        else     { s.push(b as char); },
                }
            },
            _ => { ps.consume(); addchr(&mut v, &mut s, bytes, c); },
        }
    }

    if bytes {
        vec.push(VVal::new_byt(v));
    } else {
        vec.push(VVal::new_str(&s));
    }

    if !ps.consume_if_eq('"') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('\"', "string end")));
    }

    ps.skip_ws_and_comments();

    Ok(vec)
}

#[allow(clippy::cast_lossless)]
fn parse_num(ps: &mut State) -> Result<VVal, ParseError> {
    if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("number"))); }

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

    let radix_or_num = ps.take_while(|c| c.is_digit(10)).to_string();

    let (radix, num) = if ps.consume_if_eq('r') {
        let radix = if let Ok(r) = u8::from_str_radix(&radix_or_num, 10) {
            r
        } else {
            10
        };

        if radix < 2 || radix > 36 {
            return Err(ps.err(ParseNumberError::UnsupportedRadix(radix)));
        }

        (radix, ps.take_while(|c| c.is_digit(radix as u32)).to_string())
    } else if ps.consume_if_eq('x') {
        if radix_or_num != "0" {
            return Err(ps.err(ParseNumberError::UnsupportedRadixPrefix('x', radix_or_num)));
        }
        (16, ps.take_while(|c| c.is_digit(16)).to_string())
    } else if ps.consume_if_eq('b') {
        if radix_or_num != "0" {
            return Err(ps.err(ParseNumberError::UnsupportedRadixPrefix('b', radix_or_num)));
        }
        (2, ps.take_while(|c| c.is_digit(2)).to_string())
    } else if ps.consume_if_eq('o') {
        if radix_or_num != "0" {
            return Err(ps.err(ParseNumberError::UnsupportedRadixPrefix('o', radix_or_num)));
        }
        (8, ps.take_while(|c| c.is_digit(8)).to_string())
    } else {
        (10, radix_or_num)
    };

    let (is_float, fract_num) = if ps.consume_if_eq('.') {
        let fract_digits = ps.take_while(|c| c.is_digit(radix as u32)).to_string();
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
        return Err(ps.err(ParseErrorKind::ExpectedToken('[', "list start")));
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
        return Err(ps.err(ParseErrorKind::ExpectedToken(']', "list end")));
    }

    Ok(list)
}

fn parse_map(ps: &mut State) -> Result<VVal, ParseError> {
    //println!("parse_map [{}]", ps.rest());
    if !ps.consume_if_eq_wsc('{') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('{', "map start")));
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
                    return Err(ps.err(ParseErrorKind::ExpectedToken('=', "map key")));
                }

                let elem = VVal::vec();
                elem.push(key);
                elem.push(parse_expr(ps)?);
                elem
            });

        if !ps.consume_if_eq_wsc(',') { break; }
    }

    if !ps.consume_if_eq_wsc('}') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('}', "map end")));
    }

    Ok(map)
}


fn parse_special_value(ps: &mut State) -> Result<VVal, ParseError> {
    if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("literal value"))); }
    let c = ps.expect_some(ps.peek())?;

    match c {
        'b' => { ps.consume(); parse_string(ps, true) },
        'q' => { ps.consume(); parse_q_string(ps, false) },
        'Q' => { ps.consume(); parse_q_string(ps, true) },
        'r' => {
            ps.consume();
            let pattern_source = parse_quoted(ps, String::new(), |s, c| s.push(c))?;
            let vec = ps.syn(Syntax::Pattern);
            vec.push(VVal::new_str_mv(pattern_source));
            Ok(vec)
        },
        'S' => {
            ps.consume();
            let selector_source = parse_quoted(ps, String::new(), |s, c| s.push(c))?;
            let vec = ps.syn(Syntax::Selector);
            vec.push(VVal::new_str_mv(selector_source));
            Ok(vec)
        },
        'P' => {
            ps.consume_wsc();
            let pat_expr = parse_expr(ps)?;
            let vec = ps.syn(Syntax::StructPattern);
            vec.push(pat_expr);
            Ok(vec)
        },
        '\\' => { ps.consume_wsc(); Ok(make_var(ps, "\\")) },
        'c' => {
            if ps.consume_lookahead("code") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }

            let code_start_pos = ps.remember();
            parse_expr(ps)?;
            let code_end_pos = ps.remember();
            let code = ps.collect(code_start_pos, code_end_pos).to_string();

            let vec = ps.syn(Syntax::Str);
            vec.push(VVal::new_str_mv(code));
            Ok(vec)
        },
        '[' => parse_list(ps),
        '{' => parse_map(ps),
        'n' => {
            if ps.consume_lookahead("none") {
                ps.skip_ws_and_comments();
            } else {
                ps.consume_wsc();
            }
            Ok(VVal::None)
        },
        'i' => {
            if ps.consume_lookahead("iter") {
                ps.skip_ws_and_comments();

                let err = ps.syn(Syntax::Iter);
                err.push(parse_expr(ps)?);
                Ok(err)
            } else {
                ps.consume();
                parse_nvec_body(ps, NVecKind::Int)
            }
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
        'o' => {
            ps.consume_wsc();
            if !ps.consume_if_eq_wsc('(') {
                return Err(ps.err(ParseErrorKind::ExpectedToken('(', "start of optional value")))
            }

            if ps.consume_if_eq_wsc(')') {
                Ok(ps.syn(Syntax::Opt))
            } else {
                let a = parse_expr(ps)?;
                let opt_v = ps.syn(Syntax::Opt);
                opt_v.push(a);

                if ps.consume_if_eq_wsc(')') {
                    Ok(opt_v)
                } else {
                    Err(ps.err(ParseErrorKind::ExpectedToken(')', "end of optional value")))
                }
            }
        },
        'p' => {
            ps.consume_wsc();
            if !ps.consume_if_eq_wsc('(') {
                return Err(ps.err(ParseErrorKind::ExpectedToken('(', "pair start")))
            }
            let a = parse_expr(ps)?;
            let ret =
                if ps.consume_if_eq_wsc(',') {
                    let b = parse_expr(ps)?;
                    VVal::pair(a, b)
                } else {
                    VVal::pair(a, VVal::None)
                };

            if ps.consume_if_eq_wsc(')') {
                Ok(ret)
            } else {
                Err(ps.err(ParseErrorKind::ExpectedToken(')', "pair end")))
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

fn make_binop(ps: &State, op: StrPart) -> VVal {
    if op == "&and" {
        ps.syn(Syntax::And)

    } else if op == "&or" {
        ps.syn(Syntax::Or)

    } else if op == "+" {
        ps.syn(Syntax::BinOpAdd)

    } else if op == "-" {
        ps.syn(Syntax::BinOpSub)

    } else if op == "*" {
        ps.syn(Syntax::BinOpMul)

    } else if op == "/" {
        ps.syn(Syntax::BinOpDiv)

    } else if op == "%" {
        ps.syn(Syntax::BinOpMod)

    } else if op == ">" {
        ps.syn(Syntax::BinOpGt)

    } else if op == "<" {
        ps.syn(Syntax::BinOpLt)

    } else if op == "<=" {
        ps.syn(Syntax::BinOpLe)

    } else if op == ">=" {
        ps.syn(Syntax::BinOpGe)

    } else if op == "==" {
        ps.syn(Syntax::BinOpEq)

    } else if op == "=>" {
        ps.syn(Syntax::OpNewPair)

    } else if op == "&>" {
        ps.syn(Syntax::OpCallRwL)

    } else if op == "<&" {
        ps.syn(Syntax::OpCallLwR)

    } else {
        make_to_call(ps, make_var(ps, &op.to_string()))
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
            return Err(ps.err(ParseErrorKind::ExpectedToken('`', "quoted identifier end")));
        }

        ps.skip_ws_and_comments();

        Ok(identifier)
    } else {
        let identifier =
            ps.take_while_wsc(|c| {
                match c {
                   '.' | ',' | ';' | '{' | '}'
                 | '[' | ']' | '(' | ')' | '~' | '|' | '='
                        => false,
                    _   => !c.is_whitespace()
                }
            });
        Ok(identifier.to_string())
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '@' || c == '`' || c == '?'
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
                    return Err(ps.err(
                        ParseErrorKind::ExpectedToken(
                            ')', "sub expression end")));
                }
                Ok(expr)
            },
            '{' => {
                let syn = ps.syn_raw(Syntax::Func);
                let block = parse_block(ps, true)?;
                block.set_at(0, syn);
                block.insert_at(1, VVal::None);
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
                        else { VVal::None };

                    let next_stmt = parse_stmt(ps)?;
                    block.push(VVal::None);
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

    if first_syn.get_syn() == Syntax::GetIdx && value.is_int() {
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
                let idx = ps.take_while(|c| c.is_digit(10)).to_string();
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
        return Err(ps.err(ParseErrorKind::ExpectedToken('[', "call arguments start")));
    }

    let is_apply = ps.consume_if_eq_wsc('[');

    if is_apply {
        if let VVal::Syn(mut sp) = call.at(0).unwrap_or(VVal::None) {
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

    if ps.at_end() {
        return Err(ps.err(ParseErrorKind::EOF("call args")));
    }

    if is_apply && !ps.consume_if_eq_wsc(']') {
        return Err(ps.err(ParseErrorKind::ExpectedToken(']', "apply arguments end")));
    }

    if !ps.consume_if_eq_wsc(']') {
        return Err(ps.err(ParseErrorKind::ExpectedToken(']', "call arguments end")));
    }

    Ok(call)
}

fn get_op_binding_power(op: StrPart) -> (i32, i32) {
    if       op == "&>"   { (32, 33) }
    else if  op == "<&"   { (31, 30) }
    else if  op == "^"    { (25, 26) }
    else if  op == "*"
          || op == "/"
          || op == "%"    { (23, 24) }
    else if  op == "+"
          || op == "-"    { (21, 22) }
    else if  op == "<<"
          || op == ">>"   { (19, 20) }
    else if  op == "<"
          || op == ">"
          || op == ">="
          || op == "<="   { (17, 18) }
    else if  op == "=="
          || op == "!="   { (15, 16) }
    else if  op == "&"    { (13, 14) }
    else if  op == "&^"   { (11, 12) }
    else if  op == "&|"   { ( 9, 10) }
    else if  op == "&and" { ( 7,  8) }
    else if  op == "&or"  { ( 5,  6) }
    else if  op == "=>"   { ( 2,  1) }
    else { panic!("Bad op: {}", op.to_string()) }
}

fn construct_op(binop: VVal, left: VVal, right: VVal) -> VVal {
    match binop.at(0).unwrap().get_syn() {
        Syntax::OpNewPair => VVal::pair(left, right),
        Syntax::OpCallLwR => {
            binop.set_syn_at(0, Syntax::Call);
            binop.push(left);
            binop.push(right);
            binop
        },
        Syntax::OpCallRwL => {
            binop.set_syn_at(0, Syntax::Call);
            binop.push(right);
            binop.push(left);
            binop
        },
        _ => {
            binop.push(left);
            binop.push(right);
            binop
        }
    }
}

fn parse_binop(mut left: VVal, ps: &mut State, bind_pow: i32, binop: VVal)
    -> Result<VVal, ParseError>
{
    let mut right = parse_call(ps, true)?;

    while let Some(next_op) = ps.peek_op() {
        let (l_bp, r_bp) = get_op_binding_power(next_op);
        let next_binop = make_binop(ps, next_op);

        let op_len = next_op.len();
        ps.consume_wsc_n(op_len);

        if l_bp < bind_pow {
            left = construct_op(binop, left, right);
            return parse_binop(left, ps, r_bp, next_binop);
        } else {
            right = parse_binop(right, ps, r_bp, next_binop)?;
        }
    }

    Ok(construct_op(binop, left, right))
}

fn parse_call(ps: &mut State, binop_mode: bool) -> Result<VVal, ParseError> {
    //println!("parse_expr [{}] np={}", ps.rest(), no_pipe);
    let call_indent = ps.indent_pos();
    //d// println!("CALL INDENT: {:?} @'{}'", call_indent, ps.rest());
    let mut value = parse_value(ps)?;

    // look ahead, if we see an expression delimiter.
    // because then, this is not going to be a call!
    // Also exception to parse_expr, we are excluding the '|'.
    if ps.lookahead_one_of(";),]}|") || ps.at_end() {
        return Ok(value);
    }

    let mut res_call = VVal::None;

    while let Some(c) = ps.peek() {
        let op = ps.peek_op();

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
                if binop_mode { break; }

                ps.consume_wsc();
                if let VVal::None = res_call { res_call = make_to_call(ps, value); }
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

                let op        = op.unwrap();
                let binop     = make_binop(ps, op);
                let (_, r_bp) = get_op_binding_power(op);

                let op_len = op.len();
                ps.consume_wsc_n(op_len);

                value = parse_binop(value, ps, r_bp, binop)?;
            },
            '=' => { break; }, // '=' from parsing map keys
            _ => {
                if binop_mode { break; }

                if let VVal::None = res_call { res_call = make_to_call(ps, value); }
                else { res_call.push(value); }

                //d// println!("INDENT: {:?} VS {:?} '{}'", ps.indent_pos(), call_indent, ps.rest());
                if !ps.indent_pos().belongs_to(&call_indent) {
                    return Err(ps.err(ParseErrorKind::BadIndent(
                        "Call argument does not belong to call, it needs a higher indentation.")));
                }

                value = parse_value(ps)?;
            },
        }
    }

    if let VVal::None = res_call {
        res_call = value;
    } else {
        res_call.push(value);
    }

    Ok(res_call)
}

fn parse_expr(ps: &mut State) -> Result<VVal, ParseError> {
    let mut call = parse_call(ps, false)?;
    if ps.at_end() {
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
    if ps.at_end() {
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

            if ps.at_end() {
                return Err(ps.err(ParseErrorKind::EOF(
                    "destructuring assignment")));
            }

            if !ps.consume_if_eq_wsc(')') {
                return Err(ps.err(ParseErrorKind::ExpectedToken(
                    ')', "destructuring assignment end")));
            }
        },
        _ => { ids.push(VVal::new_sym_mv(parse_identifier(ps)?)); }
    }

    assign.push(ids);

    if !ps.consume_if_eq_wsc('=') {
        return Err(ps.err(ParseErrorKind::ExpectedToken('=', "assignment")));
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
                        if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("special assignment"))); }
                        let id = parse_identifier(ps)?;
                        match &id[..] {
                            "wlambda" => {
                                let imp = ps.syn(Syntax::Import);
                                imp.push(VVal::None);
                                imp.push(VVal::new_sym("wlambda"));
                                Ok(imp)
                            },
                            "import" => {
                                let prefix =
                                    VVal::new_sym_mv(parse_identifier(ps)?);
                                ps.skip_ws_and_comments();
                                let name =
                                    if ps.peek().unwrap_or(';') == ';' {
                                        prefix.clone()
                                    } else {
                                        ps.consume_if_eq_wsc('=');
                                        VVal::new_sym_mv(parse_identifier(ps)?)
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
                                exp.push(VVal::new_sym_mv(name));
                                exp.push(expr);
                                Ok(exp)
                            },
                            "dump_stack" => Ok(ps.syn(Syntax::DumpStack)),
                            "dump_vm" => Ok(ps.syn(Syntax::DumpVM)),
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
        return Err(ps.err(
            ParseErrorKind::ExpectedToken('|', "arity definition start")));
    }

    if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("parsing arity definition"))); }

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
        return Err(ps.err(ParseErrorKind::ExpectedToken(
            '|', "arity definition end")));
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
            return Err(ps.err(ParseErrorKind::ExpectedToken('{', "block start")));
        }
    }

    let block = ps.syn(Syntax::Block);

    if is_func && ps.lookahead("|") {
        block.push(parse_arity(ps)?);
    } else if is_func {
        block.push(VVal::None);
    }

    while let Some(c) = ps.peek() {
        if is_func { if c == '}' { break; } }

        let next_stmt = parse_stmt(ps)?;
        block.push(next_stmt);

        while ps.consume_if_eq_wsc(';') {
            while ps.consume_if_eq_wsc(';') { }
            if ps.at_end() || ps.consume_if_eq_wsc('}') { return Ok(block); }
            let next_stmt = parse_stmt(ps)?;
            block.push(next_stmt);
        }
    }

    if is_func {
        if ps.at_end() {
            return Err(ps.err(ParseErrorKind::EOF("parsing block")));
        }
        if !ps.consume_if_eq_wsc('}') {
            return Err(ps.err(ParseErrorKind::ExpectedToken('}', "block end")));
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
pub fn parse(s: &str, filename: &str) -> Result<VVal, String> {
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
                   "$[&Block,$[&Call,$[&Var,:foo],$[&Key,:bar],-2.3,2.3]]");
        assert_eq!(parse("foo :bar -x 2"),
                   "$[&Block,$[&Call,$[&Var,:foo],$[&BinOpSub,$[&Key,:bar],$[&Var,:x]],2]]");
        assert_eq!(parse("foo :bar -2 2"),
                   "$[&Block,$[&Call,$[&Var,:foo],$[&Key,:bar],-2,2]]");
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
        assert_eq!(parse("${a=1,b=2,}"),    "$[&Block,$[&Map,$[:a,1],$[:b,2]]]");
        assert_eq!(parse("${a=1,}"),        "$[&Block,$[&Map,$[:a,1]]]");
        assert_eq!(parse("f[1,]"),          "$[&Block,$[&Call,$[&Var,:f],1]]");
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
                   "$[&Block,$[&Call,$[&Var,:foo],$[&Var,:bar],$[&Call,20,10]]]");
        assert_eq!(parse("10 ~ 20 ~ 30 ~ 40"),
                   "$[&Block,$[&Call,10,$[&Call,20,$[&Call,30,40]]]]");
        assert_eq!(parse("10 | 20"),                  "$[&Block,$[&Call,20,10]]");
        assert_eq!(parse("10 (1 2) (3 4 5) (6 (7))"), "$[&Block,$[&Call,10,$[&Call,1,2],$[&Call,3,4,5],$[&Call,6,7]]]");
        assert_eq!(parse("10[]"),                     "$[&Block,$[&Call,10]]");
        assert_eq!(parse("10[20, 30]"),               "$[&Block,$[&Call,10,20,30]]");
        assert_eq!(parse("10 x[20, 30]"),             "$[&Block,$[&Call,10,$[&Call,$[&Var,:x],20,30]]]");
        assert_eq!(parse("10 x[20, 30] | 50"),        "$[&Block,$[&Call,50,$[&Call,10,$[&Call,$[&Var,:x],20,30]]]]");
        assert_eq!(parse("(10).(\"a\" \"b\")"),       "$[&Block,$[&GetKey,10,$[&Call,$[&Str,\"a\"],$[&Str,\"b\"]]]]");
        assert_eq!(parse("(10).(\"ab\")"),            "$[&Block,$[&GetSym,10,\"ab\"]]");
        assert_eq!(parse("(10).a"),                   "$[&Block,$[&GetSym,10,:a]]");
        assert_eq!(parse("a.b"),                      "$[&Block,$[&GetSym,$[&Var,:a],:b]]");
        assert_eq!(parse("10 a.b"),                   "$[&Block,$[&Call,10,$[&GetSym,$[&Var,:a],:b]]]");
        assert_eq!(parse("(10).(20)"),                "$[&Block,$[&GetIdx,10,20]]");
        assert_eq!(parse("10.20 30"),                 "$[&Block,$[&Call,10.2,30]]");
        assert_eq!(parse("10 20 ~ 30 ~ 40 ~ 50"),     "$[&Block,$[&Call,10,20,$[&Call,30,$[&Call,40,50]]]]");
        assert_eq!(parse("10 20 ~ 30 40 ~ 40 1 2 3 ~ 50 60"),  "$[&Block,$[&Call,10,20,$[&Call,30,40,$[&Call,40,1,2,3,$[&Call,50,60]]]]]");
        assert_eq!(parse("10[10[1,2,3 foo] ~ 4]"),    "$[&Block,$[&Call,10,$[&Call,$[&Call,10,1,2,$[&Call,3,$[&Var,:foo]]],4]]]");
        assert_eq!(parse("foo.b.c.d"),                "$[&Block,$[&GetSym3,$[&Var,:foo],:b,:c,:d]]");
        assert_eq!(parse("foo.b.c.d[]"),              "$[&Block,$[&Call,$[&GetSym3,$[&Var,:foo],:b,:c,:d]]]");
        assert_eq!(parse("foo.b.c.d[1,2,3]"),         "$[&Block,$[&Call,$[&GetSym3,$[&Var,:foo],:b,:c,:d],1,2,3]]");
        assert_eq!(parse("foo.b.c.d 1 2 3"),          "$[&Block,$[&Call,$[&GetSym3,$[&Var,:foo],:b,:c,:d],1,2,3]]");
        assert_eq!(parse("(foo.b.c.d) 1 2 3"),        "$[&Block,$[&Call,$[&GetSym3,$[&Var,:foo],:b,:c,:d],1,2,3]]");
        assert_eq!(parse("foo.a = 10"),               "$[&Block,$[&SetKey,$[&Var,:foo],$[&Key,:a],10]]");
        assert_eq!(parse("foo.a = 10 | 20"),          "$[&Block,$[&SetKey,$[&Var,:foo],$[&Key,:a],$[&Call,20,10]]]");
        assert_eq!(parse("foo.a = 10 ~ 20"),          "$[&Block,$[&SetKey,$[&Var,:foo],$[&Key,:a],$[&Call,10,20]]]");
        assert_eq!(parse("4 == 5 ~ 10"),              "$[&Block,$[&Call,$[&BinOpEq,4,5],10]]");
        assert_eq!(parse("foo.(i) = 10"),             "$[&Block,$[&SetKey,$[&Var,:foo],$[&Var,:i],10]]");
        assert_eq!(parse("foo :x :y 10"),             "$[&Block,$[&Call,$[&Var,:foo],$[&Key,:x],$[&Key,:y],10]]");
    }

    #[test]
    fn check_expr_err() {
        assert_eq!(parse_error("foo.a[] = 10"),
            "Parse error: error[1,9:<parser_test>] Expected literal value, sub expression, block, key or identifier at code \'= 10\'");
    }

    #[test]
    fn check_identifier() {
        assert_eq!(parse("+"),          "$[&Block,$[&Var,:+]]");
        assert_eq!(parse("-"),          "$[&Block,$[&Var,:-]]");
        assert_eq!(parse("+ 10 20"),    "$[&Block,$[&Call,$[&Var,:+],10,20]]");
        assert_eq!(parse("13 + 10 20"), "$[&Block,$[&Call,$[&BinOpAdd,13,10],20]]");
        assert_eq!(parse("13 + 10 == 23"),
                                        "$[&Block,$[&BinOpEq,$[&BinOpAdd,13,10],23]]");
        assert_eq!(parse("(+ 12 ~ - 24 23) == 13"),
           "$[&Block,$[&BinOpEq,$[&Call,$[&Var,:+],12,$[&Call,$[&Var,:-],24,23]],13]]");
        assert_eq!(parse("_"),          "$[&Block,$[&Var,:_]]");
        assert_eq!(parse("ten"),        "$[&Block,$[&Var,:ten]]");
        assert_eq!(parse("tenäß foo"),  "$[&Block,$[&Call,$[&Var,:tenäß],$[&Var,:foo]]]");
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

        assert_eq!(parse("20 & 10"),                "$[&Block,$[&Call,$[&Var,:&],20,10]]");
    }

    #[test]
    fn check_assignments() {
        assert_eq!(parse("!x=10;"),              "$[&Block,$[&Def,$[:x],10]]");
        assert_eq!(parse("! x = 10 ;"),          "$[&Block,$[&Def,$[:x],10]]");
        assert_eq!(parse("! x = 10"),            "$[&Block,$[&Def,$[:x],10]]");
        assert_eq!(parse("!:global (y,x) = @"),  "$[&Block,$[&DefGlobRef,$[:y,:x],$[&Var,:@],$true]]");
        assert_eq!(parse(". (a,b) = 10"),        "$[&Block,$[&Assign,$[:a,:b],10,$true]]");
    }

    #[test]
    fn check_func() {
        assert_eq!(parse("{}"),           "$[&Block,$[&Func,$n,$n]]");
        assert_eq!(parse("{10;}"),        "$[&Block,$[&Func,$n,$n,10]]");
        assert_eq!(parse("{10;;;}"),      "$[&Block,$[&Func,$n,$n,10]]");
        assert_eq!(parse("{10; 20}"),     "$[&Block,$[&Func,$n,$n,10,20]]");
        assert_eq!(parse("{ 10 } { }"),   "$[&Block,$[&Call,$[&Func,$n,$n,10],$[&Func,$n,$n]]]");
        assert_eq!(parse("\\:x { }"),     "$[&Block,$[&Func,:x,$n]]");
        assert_eq!(parse("\\ p 1 | 20 ~ 30"),  "$[&Block,$[&Func,$n,$n,$[&Call,20,30,$[&Call,$[&Var,:p],1]]]]");
    }

    #[test]
    fn check_map() {
        assert_eq!(parse("${a=10}"),   "$[&Block,$[&Map,$[:a,10]]]");
        assert_eq!(parse("${:a=10}"),  "$[&Block,$[&Map,$[$[&Key,:a],10]]]");
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
        assert_eq!(parse("foo.(bar) == 2019"), "$[&Block,$[&BinOpEq,$[&GetKey,$[&Var,:foo],$[&Var,:bar]],2019]]");
        assert_eq!(parse("o.x[]"),             "$[&Block,$[&Call,$[&GetSym,$[&Var,:o],:x]]]");

        assert_eq!(parse("o.1"),                       "$[&Block,$[&GetIdx,$[&Var,:o],1]]");
        assert_eq!(parse("o.1.2"),                     "$[&Block,$[&GetIdx2,$[&Var,:o],1,2]]");
        assert_eq!(parse("o.1.2.3"),                   "$[&Block,$[&GetIdx3,$[&Var,:o],1,2,3]]");
        assert_eq!(parse("o.x"),                       "$[&Block,$[&GetSym,$[&Var,:o],:x]]");
        assert_eq!(parse("o.x.y"),                     "$[&Block,$[&GetSym2,$[&Var,:o],:x,:y]]");
        assert_eq!(parse("o.x.y.z"),                   "$[&Block,$[&GetSym3,$[&Var,:o],:x,:y,:z]]");
        assert_eq!(parse("o.x.(\"y\").z"),             "$[&Block,$[&GetSym3,$[&Var,:o],:x,\"y\",:z]]");
        assert_eq!(parse("o.(\"x\")"),                 "$[&Block,$[&GetSym,$[&Var,:o],\"x\"]]");
        assert_eq!(parse("o.(\"x\").(\"y\")"),         "$[&Block,$[&GetSym2,$[&Var,:o],\"x\",\"y\"]]");
        assert_eq!(parse("o.(\"x\").(\"y\").(\"z\")"), "$[&Block,$[&GetSym3,$[&Var,:o],\"x\",\"y\",\"z\"]]");
        assert_eq!(parse("o.(1 \"x\")"),                     "$[&Block,$[&GetKey,$[&Var,:o],$[&Call,1,$[&Str,\"x\"]]]]");
        assert_eq!(parse("o.(1 \"x\").(1 \"y\")"),           "$[&Block,$[&GetKey,$[&GetKey,$[&Var,:o],$[&Call,1,$[&Str,\"x\"]]],$[&Call,1,$[&Str,\"y\"]]]]");
        assert_eq!(parse("o.(1 \"x\").(1 \"y\").(1 \"z\")"), "$[&Block,$[&GetKey,$[&GetKey,$[&GetKey,$[&Var,:o],$[&Call,1,$[&Str,\"x\"]]],$[&Call,1,$[&Str,\"y\"]]],$[&Call,1,$[&Str,\"z\"]]]]");
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
        assert_eq!(parse("$&${z=1}"),     "$[&Block,$[&WRef,$[&Map,$[:z,1]]]]");
        assert_eq!(parse("$&& 1"),        "$[&Block,$[&Ref,1]]");
        assert_eq!(parse("$&&$[1,2]"),    "$[&Block,$[&Ref,$[&Lst,1,2]]]");
        assert_eq!(parse("$&&${z=1}"),    "$[&Block,$[&Ref,$[&Map,$[:z,1]]]]");
        assert_eq!(parse("$*${z=1}.f=1"), "$[&Block,$[&SetKey,$[&Deref,$[&Map,$[:z,1]]],$[&Key,:f],1]]");
        assert_eq!(parse("$*xxx.f=1"),    "$[&Block,$[&SetKey,$[&Deref,$[&Var,:xxx]],$[&Key,:f],1]]");
        assert_eq!(parse("$*xxx.f"),      "$[&Block,$[&GetSym,$[&Deref,$[&Var,:xxx]],:f]]");
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
        assert_eq!(parse("fo[[@]]"),            "$[&Block,$[&Apply,$[&Var,:fo],$[&Var,:@]]]");
        assert_eq!(parse("fo[[$[1,2,3]]]"),     "$[&Block,$[&Apply,$[&Var,:fo],$[&Lst,1,2,3]]]");
        assert_eq!(parse("obj.1.field[[_]]"),   "$[&Block,$[&Apply,$[&GetSym,$[&GetIdx,$[&Var,:obj],1],:field],$[&Var,:_]]]");
        assert_eq!(parse("obj.1.(\"field\")[[_]]"),   "$[&Block,$[&Apply,$[&GetSym,$[&GetIdx,$[&Var,:obj],1],\"field\"],$[&Var,:_]]]");
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
        assert_eq!(parse("!:const X = 32;"),                "$[&Block,$[&DefConst,$[:X],32]]");
        assert_eq!(parse("!:const X = 32.4;"),              "$[&Block,$[&DefConst,$[:X],32.4]]");
        assert_eq!(parse("!:const X = :XX;"),               "$[&Block,$[&DefConst,$[:X],$[&Key,:XX]]]");
        assert_eq!(parse("!:const X = \"fo\";"),            "$[&Block,$[&DefConst,$[:X],$[&Str,\"fo\"]]]");
        assert_eq!(parse("!:const X = $[120];"),            "$[&Block,$[&DefConst,$[:X],$[&Lst,120]]]");
        assert_eq!(parse("!:const X = ${a=10};"),           "$[&Block,$[&DefConst,$[:X],$[&Map,$[:a,10]]]]");
        assert_eq!(parse("!:const (A,B,X) = $[1,3,4];"),    "$[&Block,$[&DefConst,$[:A,:B,:X],$[&Lst,1,3,4],$true]]");
    }

    #[test]
    fn check_indent_error() {
        assert_eq!(parse_error(" 10 11\n12"),
            "Parse error: error[2,1:<parser_test>] Call argument does not belong to call, it needs a higher indentation. at code \'12\'");
        assert_eq!(parse_error("10 11\n12"),
            "Parse error: error[2,1:<parser_test>] Call argument does not belong to call, it needs a higher indentation. at code \'12\'");
        assert_eq!(parse_error("!x = 10 11\n12"),
            "Parse error: error[2,1:<parser_test>] Call argument does not belong to call, it needs a higher indentation. at code \'12\'");
        assert_eq!(parse("10 11\n 12"),
            "$[&Block,$[&Call,10,11,12]]");
        assert_eq!(parse("10 11\n;12"),
            "$[&Block,$[&Call,10,11],12]");
        assert_eq!(parse("10 11\n;\n12"),
            "$[&Block,$[&Call,10,11],12]");
        assert_eq!(parse("10 11\n 12 13"),
            "$[&Block,$[&Call,10,11,12,13]]");
        assert_eq!(parse("!x = 10 11\n 12"),
            "$[&Block,$[&Def,$[:x],$[&Call,10,11,12]]]");
        assert_eq!(parse("!x = 10 11\n 12 13"),
            "$[&Block,$[&Def,$[:x],$[&Call,10,11,12,13]]]");
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

    #[test]
    fn check_pair_op() {
        assert_eq!(parse("match x a => b x => d 5"), "$[&Block,$[&Call,$[&Var,:match],$[&Var,:x],$p($[&Var,:a],$[&Var,:b]),$p($[&Var,:x],$[&Var,:d]),5]]");
        assert_eq!(parse("iter i 0 => 10 ~ 20"),     "$[&Block,$[&Call,$[&Var,:iter],$[&Var,:i],$p(0,10),20]]");
        assert_eq!(parse("iter i 0 => 10 \\20"),     "$[&Block,$[&Call,$[&Var,:iter],$[&Var,:i],$p(0,10),$[&Func,$n,$n,20]]]");
        assert_eq!(parse("iter i 0 => 10 { 20 }"),   "$[&Block,$[&Call,$[&Var,:iter],$[&Var,:i],$p(0,10),$[&Func,$n,$n,20]]]");
    }

    #[test]
    fn check_call_op() {
        assert_eq!(parse("a &> b x"),        "$[&Block,$[&Call,$[&Call,$[&Var,:b],$[&Var,:a]],$[&Var,:x]]]");
        assert_eq!(parse("x a &> b"),        "$[&Block,$[&Call,$[&Var,:x],$[&Call,$[&Var,:b],$[&Var,:a]]]]");
        assert_eq!(parse("x a &> b &> c"),   "$[&Block,$[&Call,$[&Var,:x],$[&Call,$[&Var,:c],$[&Call,$[&Var,:b],$[&Var,:a]]]]]");
        assert_eq!(parse("x a &> b <& c"),   "$[&Block,$[&Call,$[&Var,:x],$[&Call,$[&Call,$[&Var,:b],$[&Var,:a]],$[&Var,:c]]]]");
        assert_eq!(parse("x a <& b"),        "$[&Block,$[&Call,$[&Var,:x],$[&Call,$[&Var,:a],$[&Var,:b]]]]");
        assert_eq!(parse("x a <& b <& c"),   "$[&Block,$[&Call,$[&Var,:x],$[&Call,$[&Var,:a],$[&Call,$[&Var,:b],$[&Var,:c]]]]]");
        assert_eq!(parse("x a <& b &> c"),   "$[&Block,$[&Call,$[&Var,:x],$[&Call,$[&Var,:a],$[&Call,$[&Var,:c],$[&Var,:b]]]]]");
    }
}
