// Copyright (c) 2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#![allow(unused_macros)]

#[allow(unused_imports)]
use crate::compiler::*;
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[allow(unused_imports)]
use crate::{threads::AVal, Env, StackAction, VVal};
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use std::rc::Rc;

#[cfg(feature = "clipboard")]
use copypasta::{ClipboardContext, ClipboardProvider};

#[cfg(feature = "clipboard")]
thread_local! {
    pub static ClipCtx: RefCell<ClipboardContext> = RefCell::new(ClipboardContext::new().unwrap());
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "clipboard")]
    st.fun(
        "os:get_clipboard_text",
        |env: &mut Env, _argc: usize| {
            ClipCtx.with(|ctx| {
                match ctx.borrow_mut().get_contents() {
                    Ok(txt) => Ok(VVal::new_str_mv(txt)),
                    Err(e) =>
                        Ok(env.new_err(format!("std:os:get_clipboard_text error: {}", e)))
                }
            })
        },
        Some(0),
        Some(0),
        false,
    );

    #[cfg(feature = "clipboard")]
    st.fun(
        "os:set_clipboard_text",
        |env: &mut Env, _argc: usize| {
            ClipCtx.with(|ctx| {
                match ctx.borrow_mut().set_contents(env.arg(0).s_raw()) {
                    Ok(()) => Ok(VVal::Bol(true)),
                    Err(e) =>
                        Ok(env.new_err(format!("std:os:set_clipboard_text error: {}", e)))
                }
            })
        },
        Some(1),
        Some(1),
        false,
    );
}
