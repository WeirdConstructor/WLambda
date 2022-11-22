// Copyright (c) 2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#[allow(unused_imports)]
use crate::{VVal, Env, StackAction};
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[allow(unused_imports)]
use std::rc::Rc;
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use crate::compiler::*;

#[cfg(feature="cursive")]
use cursive::{Cursive, CursiveExt};
#[cfg(feature="cursive")]
use cursive::view::{IntoBoxedView};
#[cfg(feature="cursive")]
use cursive::views::{BoxedView, Button};

#[cfg(feature="cursive")]
#[derive(Clone)]
struct CursiveHandle {
    cursive: Rc<RefCell<Cursive>>,
}

#[cfg(feature="cursive")]
impl CursiveHandle {
    pub fn new() -> Self {
        Self {
            cursive: Rc::new(RefCell::new(Cursive::new())),
        }
    }
}

#[cfg(feature="cursive")]
fn vv2view(v: &VVal) -> Result<Box<(dyn cursive::View + 'static)>, String> {
    let typ = v.v_(0);
    let define = v.v_(1);

    match &typ.s_raw()[..] {
        "button" => {
            Ok(Button::new(define.v_s_rawk("label"), |s| s.quit()).into_boxed_view())
        }
        _ => {
            Err(format!("Unknown view type: '{}'", typ.s()))
        }
    }
}


#[cfg(feature="cursive")]
impl VValUserData for CursiveHandle {
    fn s(&self) -> String {
        format!("$<Cursive>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any { self }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv_ref();

        match key {
            "add_layer" => {
                if argv.len() != 1 {
                    return
                        Err(StackAction::panic_str(
                            "$<Cursive>.add_layer(view) expects 1 argument".to_string(),
                            None,
                            env.argv()))
                }

                match vv2view(&argv[0]) {
                    Ok(v) => {
                        self.cursive.borrow_mut().add_layer(v);
                        Ok(VVal::None)
                    },
                    Err(e) => {
                        Err(StackAction::panic_str(
                            format!("$<Cursive>.add_layer(view) expects proper view definition: {}", e),
                            None,
                            env.argv()))
                    },
                }
            }
            "run" => {
                if argv.len() != 0 {
                    return
                        Err(StackAction::panic_str(
                            "$<Cursive>.run() expects 0 arguments".to_string(),
                            None,
                            env.argv()))
                }

                self.cursive.borrow_mut().run();
                Ok(VVal::None)
            },
            _ => {
                Err(StackAction::panic_str(
                    format!("$<Cursive> unknown method called: {}", key),
                    None,
                    env.argv()))
            },
        }
    }
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature="cursive")]
    st.fun("cursive:new", |env: &mut Env, _argc: usize| {
        Ok(VVal::new_usr(CursiveHandle::new()))
    }, Some(0), Some(0), false);
}
