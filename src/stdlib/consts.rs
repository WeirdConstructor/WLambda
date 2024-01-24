#![allow(unused_macros)]

#[allow(unused_imports)]
use crate::compiler::*;
#[allow(unused_imports)]
use crate::vval::{VVal, VValFun, VValUserData};
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use std::rc::Rc;

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    st.set("const:PI", VVal::Flt(std::f64::consts::PI));
    st.set("const:TAU", VVal::Flt(std::f64::consts::TAU));
    st.set("const:SQRT_2", VVal::Flt(std::f64::consts::SQRT_2));
    st.set("const:LN_2", VVal::Flt(std::f64::consts::LN_2));
    st.set("const:LN_10", VVal::Flt(std::f64::consts::LN_10));
    st.set("const:LOG2_10", VVal::Flt(std::f64::consts::LOG2_10));
    st.set("const:LOG2_E", VVal::Flt(std::f64::consts::LOG2_E));
    st.set("const:LOG10_2", VVal::Flt(std::f64::consts::LOG10_2));
    st.set("const:LOG10_E", VVal::Flt(std::f64::consts::LOG10_E));
    st.set("const:FRAC_1_PI", VVal::Flt(std::f64::consts::FRAC_1_PI));
    st.set("const:FRAC_1_SQRT_2", VVal::Flt(std::f64::consts::FRAC_1_SQRT_2));
    st.set("const:FRAC_2_PI", VVal::Flt(std::f64::consts::FRAC_2_PI));
    st.set("const:FRAC_2_SQRT_PI", VVal::Flt(std::f64::consts::FRAC_2_SQRT_PI));
    st.set("const:FRAC_PI_2", VVal::Flt(std::f64::consts::FRAC_PI_2));
    st.set("const:FRAC_PI_3", VVal::Flt(std::f64::consts::FRAC_PI_3));
    st.set("const:FRAC_PI_4", VVal::Flt(std::f64::consts::FRAC_PI_4));
    st.set("const:FRAC_PI_6", VVal::Flt(std::f64::consts::FRAC_PI_6));
    st.set("const:FRAC_PI_8", VVal::Flt(std::f64::consts::FRAC_PI_8));
    st.set("const:E", VVal::Flt(std::f64::consts::E));
}
