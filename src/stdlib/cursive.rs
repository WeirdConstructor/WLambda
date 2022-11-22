// Copyright (c) 2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#[allow(unused_imports)]
use crate::compiler::*;
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[allow(unused_imports)]
use crate::{Env, StackAction, VVal};
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use std::rc::Rc;

#[cfg(feature = "cursive")]
use cursive::view::IntoBoxedView;
#[cfg(feature = "cursive")]
use cursive::views::{BoxedView, Button, Dialog, LinearLayout, Panel, TextView};
#[cfg(feature = "cursive")]
use cursive::{direction::Orientation, traits::Nameable, Cursive, CursiveExt};

macro_rules! call_callback {
    ($cursive: ident, $cb: ident, $env: ident) => {{
        let cursive_ptr: *mut Cursive = $cursive;

        let api = CursiveAPI::new(cursive_ptr);
        {
            if $cb.is_some() {
                match $cb.call(&mut $env.borrow_mut(), &[VVal::new_usr(api.clone())]) {
                    Ok(_) => (),
                    Err(e) => {
                        $cursive.add_layer(
                            Dialog::around(TextView::new(format!("Error in callback: {}", e)))
                                .button("Quit", |s| s.quit()),
                        );
                    }
                }
            }
        }

        if !api.this_is_the_only_strong_ref() {
            $cursive.add_layer(
                Dialog::around(TextView::new(format!(
                    "You must never store $<CursiveAPI> from a callback: {}",
                    $cb.s()
                )))
                .button("Quit", |s| s.quit()),
            );
        }
    }};
}

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct CursiveAPI {
    ptr: Rc<*mut Cursive>,
}

impl CursiveAPI {
    pub fn new(ptr: *mut Cursive) -> Self {
        Self { ptr: Rc::new(ptr) }
    }

    pub fn this_is_the_only_strong_ref(&self) -> bool {
        Rc::strong_count(&self.ptr) == 1
    }
}

#[cfg(feature = "cursive")]
impl VValUserData for CursiveAPI {
    fn s(&self) -> String {
        format!("$<CursiveAPI>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();

        match key {
            "msg" => {
                if argv.len() < 1 || argv.len() > 2 {
                    return Err(StackAction::panic_str(
                        "$<CursiveAPI>.msg([callback], text) expects 1 or 2 arguments".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                let cb = if argv.len() > 1 { argv.v_(0) } else { VVal::None };
                let text = if argv.len() > 1 { argv.v_(1) } else { argv.v_(0) };
                let denv = Rc::new(RefCell::new(env.derive()));

                let cursive: &mut Cursive = unsafe { &mut **self.ptr };
                cursive.add_layer(Dialog::around(TextView::new(&text.s_raw())).button("Ok", move |s| {
                    s.pop_layer();
                    call_callback!(s, cb, denv);
                }));
                Ok(VVal::None)
            }
            "quit" => {
                if argv.len() != 0 {
                    return Err(StackAction::panic_str(
                        "$<CursiveAPI>.quit() expects 0 arguments".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                unsafe { (**self.ptr).quit() }

                Ok(VVal::None)
            }
            _ => Err(StackAction::panic_str(
                format!("$<CursiveAPI> unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
}

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct CursiveHandle {
    cursive: Rc<RefCell<Cursive>>,
}

#[cfg(feature = "cursive")]
impl CursiveHandle {
    pub fn new() -> Self {
        Self { cursive: Rc::new(RefCell::new(Cursive::new())) }
    }
}

macro_rules! wrap_named {
    ($view: expr, $define: ident) => {
        if $define.v_k("name").is_some() {
            $view.with_name($define.v_s_rawk("name")).into_boxed_view()
        } else {
            $view.into_boxed_view()
        }
    };
}

#[cfg(feature = "cursive")]
fn vv2view(v: &VVal, env: &mut Env) -> Result<Box<(dyn cursive::View + 'static)>, String> {
    let typ = v.v_(0);
    let define = v.v_(1);

    match &typ.s_raw()[..] {
        "hbox" => {
            let mut ll = LinearLayout::new(Orientation::Horizontal);
            define.with_iter(|it| {
                for (v, _) in it {
                    ll.add_child(vv2view(&v, env)?);
                }

                Ok::<(), String>(())
            })?;

            Ok(ll.into_boxed_view())
        }
        "vbox" => {
            let mut ll = LinearLayout::new(Orientation::Vertical);
            define.with_iter(|it| {
                for (v, _) in it {
                    ll.add_child(vv2view(&v, env)?);
                }

                Ok::<(), String>(())
            })?;

            Ok(ll.into_boxed_view())
        }
        "panel" => {
            let (define, child) = (define.v_(0), define.v_(1));
            let mut pnl = Panel::new(BoxedView::new(vv2view(&child, env)?));
            if define.v_k("title").is_some() {
                pnl.set_title(define.v_s_rawk("title"));
            }
            Ok(pnl.into_boxed_view())
        }
        "button" => {
            let cb = define.v_k("cb");
            let denv = Rc::new(RefCell::new(env.derive()));

            let view = Button::new(define.v_s_rawk("label"), move |s| call_callback!(s, cb, denv));

            Ok(wrap_named!(view, define))
        }
        _ => Err(format!("Unknown view type: '{}'", typ.s())),
    }
}

#[cfg(feature = "cursive")]
impl VValUserData for CursiveHandle {
    fn s(&self) -> String {
        format!("$<Cursive>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();

        match key {
            "add_layer" => {
                if argv.len() != 1 {
                    return Err(StackAction::panic_str(
                        "$<Cursive>.add_layer(view) expects 1 argument".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                match vv2view(&argv.v_(0), env) {
                    Ok(v) => {
                        self.cursive.borrow_mut().add_layer(v);
                        Ok(VVal::None)
                    }
                    Err(e) => Err(StackAction::panic_str(
                        format!("$<Cursive>.add_layer(view) expects proper view definition: {}", e),
                        None,
                        env.argv(),
                    )),
                }
            }
            "run" => {
                if argv.len() != 0 {
                    return Err(StackAction::panic_str(
                        "$<Cursive>.run() expects 0 arguments".to_string(),
                        None,
                        env.argv(),
                    ));
                }

                self.cursive.borrow_mut().run();
                Ok(VVal::None)
            }
            _ => Err(StackAction::panic_str(
                format!("$<Cursive> unknown method called: {}", key),
                None,
                env.argv(),
            )),
        }
    }
}

#[allow(unused_variables)]
pub fn add_to_symtable(st: &mut SymbolTable) {
    #[cfg(feature = "cursive")]
    st.fun(
        "cursive:new",
        |env: &mut Env, _argc: usize| Ok(VVal::new_usr(CursiveHandle::new())),
        Some(0),
        Some(0),
        false,
    );
}
