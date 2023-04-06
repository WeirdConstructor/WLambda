use crate::vval::{VVal, Env, VValFun};

use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};

use std::fmt::Write;

pub fn parse_chemical_sum_formula(s: &str) -> Result<VVal, ParseError> {
    let mut ps = State::new_verbatim(s, "<selector>");

    let res = VVal::map();

    while !ps.at_end() {
        match ps.peek().unwrap() {
            '0' ..= '9' => {
                let num = ps.take_while(|c| c.is_digit(10)).to_string();
                if ps.at_end() { return Err(ps.err(ParseErrorKind::EOF("mol-count"))); }
            }
        }
    }

    Ok(res)
}
