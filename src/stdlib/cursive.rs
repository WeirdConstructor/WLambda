// Copyright (c) 2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

#![allow(unused_macros)]

#[allow(unused_imports)]
use crate::compiler::*;
#[allow(unused_imports)]
use crate::vval::{VValFun, VValUserData};
#[allow(unused_imports)]
use crate::{Env, StackAction, VVal};
#[allow(unused_imports)]
use std::cell::RefCell;
#[allow(unused_imports)]
use std::collections::HashMap;
#[allow(unused_imports)]
use std::rc::Rc;

#[cfg(feature = "cursive")]
use cursive::view::{IntoBoxedView, Resizable, ScrollStrategy, Scrollable, SizeConstraint};
#[cfg(feature = "cursive")]
use cursive::views::{
    BoxedView, Button, Checkbox, Dialog, EditView, LinearLayout, ListView, Panel, RadioButton,
    RadioGroup, SelectView, SliderView, StackView, TextContent, TextView, ViewRef,
};
#[cfg(feature = "cursive")]
use cursive::{direction::Orientation, traits::Nameable, Cursive, CursiveExt};

macro_rules! assert_arg_count {
    ($self: expr, $argv: expr, $count: expr, $function: expr, $env: ident) => {
        if $argv.len() != $count {
            return Err(StackAction::panic_str(
                format!("{}.{} expects {} arguments", $self, $function, $count),
                None,
                $env.argv(),
            ));
        }
    };
}

macro_rules! call_callback {
    ($cursive: ident, $cb: ident, $env: ident $(,$args: expr)*) => {{
        let cursive_ptr: *mut Cursive = $cursive;

        let api = CursiveAPI::new(cursive_ptr);
        {
            if $cb.is_some() {
                match $cb.call(&mut $env.borrow_mut(), &[VVal::new_usr(api.clone()) $(,$args)*]) {
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
                    "You must never store $<Cursive> from a callback: {}",
                    $cb.s()
                )))
                .button("Quit", |s| s.quit()),
            );
        }
    }};
}

thread_local! {
    static CURSIVE_STDOUT: RefCell<TextContent> = RefCell::new(TextContent::new(""));
}

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct CursiveAPI {
    ptr: Rc<*mut Cursive>,
}

#[cfg(feature = "cursive")]
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
        let cursive: &mut Cursive = unsafe { &mut **self.ptr };
        let ud = cursive
            .user_data::<WLambdaCursiveContext>()
            .expect("Userdata must be WLambdaCursiveContext!");
        let reg = ud.get_registry();

        handle_cursive_call_method(key, &argv, env, cursive, &reg)
    }
}

#[cfg(feature = "cursive")]
struct WLambdaCursiveContext {
    callbacks: HashMap<String, Option<Box<dyn FnMut(&mut Cursive, VVal)>>>,
    default_callbacks: HashMap<String, Option<Box<dyn FnMut(&mut Cursive, VVal)>>>,
    reg: ViewNameRegistry,
}

#[cfg(feature = "cursive")]
impl WLambdaCursiveContext {
    pub fn new() -> Self {
        let reg = Rc::new(RefCell::new(HashMap::new()));

        Self { callbacks: HashMap::new(), default_callbacks: HashMap::new(), reg }
    }

    pub fn get_registry(&self) -> ViewNameRegistry {
        self.reg.clone()
    }
}

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct CursiveHandle {
    cursive: Rc<RefCell<Cursive>>,
    reg: ViewNameRegistry,
}

#[cfg(feature = "cursive")]
impl CursiveHandle {
    pub fn new() -> Self {
        let cursive = Rc::new(RefCell::new(Cursive::new()));
        //        cursive.borrow_mut().add_global_callback('c', |s| {
        //            cursive::reexports::log::set_max_level(cursive::reexports::log::LevelFilter::Info);
        //            s.toggle_debug_console()
        //        });
        let ud = WLambdaCursiveContext::new();
        let reg = ud.get_registry();
        cursive.borrow_mut().set_user_data(ud);
        Self { cursive, reg }
    }
}

#[cfg(feature = "cursive")]
#[derive(Debug, Clone)]
enum ViewType {
    StackView,
    EditView,
    Button,
    Panel,
    TextView,
    RadioButton,
    Checkbox,
    Slider,
    SelectView,
}

#[cfg(feature = "cursive")]
type ViewNameRegistry = Rc<RefCell<HashMap<String, String>>>;

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct NamedViewHandle {
    ptr: Rc<*mut Cursive>,
    reg: ViewNameRegistry,
    name: String,
    typ: ViewType,
}

#[cfg(feature = "cursive")]
impl NamedViewHandle {
    pub fn new(ptr: Rc<*mut Cursive>, name: &str, typ: ViewType) -> Self {
        let cursive: &mut Cursive = unsafe { &mut **ptr };
        let ud = cursive
            .user_data::<WLambdaCursiveContext>()
            .expect("Userdata must be WLambdaCursiveContext!");
        let reg = ud.get_registry();

        Self { ptr, name: name.to_string(), typ, reg }
    }
}

macro_rules! access_named_view_ctx {
    ($self: ident, $cursive: ident, $type: ty, $name: ident, $env: ident, $block: tt) => {{
        let $cursive: &mut Cursive = unsafe { &mut **$self.ptr };
        let view: Option<ViewRef<$type>> = $cursive.find_name(&$self.name);
        #[allow(unused_mut)]
        match view {
            Some(mut $name) => $block,
            None => {
                return Err(StackAction::panic_str(
                    format!("$<NamedView:{}:{:?}> no such view", $self.name, $self.typ),
                    None,
                    $env.argv(),
                ));
            }
        }
    }};
}

macro_rules! access_named_view {
    ($self: ident, $type: ident, $name: ident, $env: ident, $block: tt) => {
        access_named_view_ctx!($self, cursive, $type, $name, $env, $block)
    };
}

macro_rules! expect_view {
    ($argv: expr, $what: expr, $name: ident, $reg: expr, $env: ident, $block: tt) => {
        match vv2view($argv, $env, &$reg) {
            Ok($name) => $block,
            Err(e) => {
                return Err(StackAction::panic_str(
                    format!("{} expects proper view definition: {}", $what, e),
                    None,
                    $env.argv(),
                ))
            }
        }
    };
}

macro_rules! named_view_error {
    ($self: ident, $env: ident, $key: ident) => {
        Err(StackAction::panic_str(
            format!("$<NamedView:{}:{:?}> unknown method called: {}", $self.name, $self.typ, $key),
            None,
            $env.argv(),
        ))
    };
}

#[cfg(feature = "cursive")]
impl VValUserData for NamedViewHandle {
    fn s(&self) -> String {
        format!("$<NamedView:{}:{:?}>", self.name, self.typ)
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();

        match self.typ {
            ViewType::Panel => match key {
                _ => named_view_error!(self, env, key),
            },
            ViewType::Button => match key {
                "set_label" => {
                    assert_arg_count!(self.s(), argv, 1, "set_label[text]", env);
                    access_named_view!(self, Button, view, env, {
                        view.set_label(argv.v_s_raw(0));
                        Ok(VVal::None)
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::EditView => match key {
                "set_content" => {
                    assert_arg_count!(self.s(), argv, 1, "set_content[new_text]", env);
                    access_named_view_ctx!(self, cursive, EditView, view, env, {
                        (view.set_content(argv.v_s_raw(0)))(cursive);
                        Ok(VVal::None)
                    })
                }
                "content" => {
                    assert_arg_count!(self.s(), argv, 0, "content[]", env);
                    access_named_view_ctx!(self, cursive, EditView, view, env, {
                        Ok(VVal::new_str(&view.get_content()))
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::TextView => match key {
                "set_content" => {
                    assert_arg_count!(self.s(), argv, 1, "set_content[new_text]", env);
                    access_named_view_ctx!(self, cursive, TextView, view, env, {
                        view.set_content(argv.v_s_raw(0));
                        Ok(VVal::None)
                    })
                }
                "append" => {
                    assert_arg_count!(self.s(), argv, 1, "append[text]", env);
                    access_named_view_ctx!(self, cursive, TextView, view, env, {
                        view.append(argv.v_s_raw(0));
                        Ok(VVal::None)
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::RadioButton => match key {
                "select" => {
                    assert_arg_count!(self.s(), argv, 0, "select[]", env);
                    access_named_view_ctx!(self, cursive, RadioButton<VVal>, view, env, {
                        view.select();
                        Ok(VVal::None)
                    })
                }
                "is_selected" => {
                    assert_arg_count!(self.s(), argv, 0, "is_selected[]", env);
                    access_named_view_ctx!(self, cursive, RadioButton<VVal>, view, env, {
                        Ok(VVal::Bol(view.is_selected()))
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::Checkbox => match key {
                "set_checked" => {
                    assert_arg_count!(self.s(), argv, 1, "set_checked[bool]", env);
                    access_named_view_ctx!(self, cursive, Checkbox, view, env, {
                        view.set_checked(argv.v_b(0));
                        Ok(VVal::None)
                    })
                }
                "is_checked" => {
                    assert_arg_count!(self.s(), argv, 0, "is_checked[]", env);
                    access_named_view_ctx!(self, cursive, Checkbox, view, env, {
                        Ok(VVal::Bol(view.is_checked()))
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::Slider => match key {
                "set_value" => {
                    assert_arg_count!(self.s(), argv, 1, "set_value[num]", env);
                    access_named_view_ctx!(self, cursive, SliderView, view, env, {
                        view.set_value(argv.v_i(0) as usize);
                        Ok(VVal::None)
                    })
                }
                "max" => {
                    assert_arg_count!(self.s(), argv, 0, "max[]", env);
                    access_named_view_ctx!(self, cursive, SliderView, view, env, {
                        Ok(VVal::Int(view.get_max_value() as i64))
                    })
                }
                "value" => {
                    assert_arg_count!(self.s(), argv, 0, "value[]", env);
                    access_named_view_ctx!(self, cursive, SliderView, view, env, {
                        Ok(VVal::Int(view.get_value() as i64))
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::SelectView => match key {
                "clear" => {
                    assert_arg_count!(self.s(), argv, 0, "clear[]", env);
                    access_named_view_ctx!(self, cursive, SelectView<VVal>, view, env, {
                        view.clear();
                        Ok(VVal::None)
                    })
                }
                "add_item" => {
                    assert_arg_count!(self.s(), argv, 2, "add_item[label, value]", env);
                    access_named_view_ctx!(self, cursive, SelectView<VVal>, view, env, {
                        view.add_item(argv.v_s_raw(0), argv.v_(1));
                        Ok(VVal::None)
                    })
                }
                "selected_id" => {
                    assert_arg_count!(self.s(), argv, 0, "selected_id[]", env);
                    access_named_view_ctx!(self, cursive, SelectView<VVal>, view, env, {
                        if let Some(id) = view.selected_id() {
                            Ok(VVal::Int(id as i64))
                        } else {
                            Ok(VVal::None)
                        }
                    })
                }
                "selected" => {
                    assert_arg_count!(self.s(), argv, 0, "selected[]", env);
                    access_named_view_ctx!(self, cursive, SelectView<VVal>, view, env, {
                        if let Some(val) = view.selection() {
                            Ok((*val).clone())
                        } else {
                            Ok(VVal::None)
                        }
                    })
                }
                "set_selection" => {
                    assert_arg_count!(self.s(), argv, 1, "set_selection[index]", env);
                    access_named_view_ctx!(self, cursive, SelectView<VVal>, view, env, {
                        (view.set_selection(argv.v_i(0) as usize))(cursive);
                        Ok(VVal::None)
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::StackView => match key {
                "pop_layer" => {
                    assert_arg_count!(self.s(), argv, 0, "pop_layer[]", env);
                    access_named_view!(self, StackView, view, env, {
                        view.pop_layer();
                        Ok(VVal::None)
                    })
                }
                "add_layer" => {
                    assert_arg_count!(self.s(), argv, 1, "add_layer[view]", env);
                    expect_view!(
                        &argv.v_(0),
                        "$<NamedViewHandle>.add_layer",
                        new_view,
                        self.reg,
                        env,
                        {
                            access_named_view!(self, StackView, view, env, {
                                view.add_layer(new_view);
                                Ok(VVal::None)
                            })
                        }
                    )
                }
                _ => named_view_error!(self, env, key),
            },
        }
    }
}

#[cfg(feature = "cursive")]
fn vv2size_const(v: &VVal) -> Option<SizeConstraint> {
    let topkind = v.s_raw();

    if topkind == "free" {
        return Some(SizeConstraint::Free);
    } else if topkind == "full" {
        return Some(SizeConstraint::Full);
    } else {
        let kind = v.v_s_raw(0);
        let size = v.v_i(1);
        match &kind[..] {
            "fixed" => {
                return Some(SizeConstraint::Fixed(size as usize));
            }
            "max" => {
                return Some(SizeConstraint::AtMost(size as usize));
            }
            "min" => {
                return Some(SizeConstraint::AtLeast(size as usize));
            }
            _ => {}
        }
    }

    None
}

macro_rules! wrap_scroll_view {
    ($view: expr, $define: ident, $reg: expr, $cursive: ident, $env: ident) => {
        {
            if $define.v_k("scroll_name").is_some() {
                $reg.borrow_mut().insert($define.v_s_rawk("scroll_name"), "scrollview".to_string());
            }

            let mut v = $view;

            add_cb_handler!(
                $cursive,
                $define,
                $env,
                v,
                set_on_scroll_change,
                "on_scroll_change",
                rect: VVal::ivec4(rect.left() as i64, rect.top() as i64, rect.right() as i64, rect.bottom() as i64),
            );

            v
        }
    }
}

macro_rules! auto_wrap_view {
    ($view: expr, $define: ident, $type: ident, $reg: expr, $cursive: ident, $env: ident) => {{
        let size_w = vv2size_const(&$define.v_k("width"));
        let size_h = vv2size_const(&$define.v_k("height"));

        let mut scroll_strat = None;
        if $define.v_k("scroll").is_some() {
            scroll_strat = Some(match &$define.v_s_rawk("scroll")[..] {
                "top" => ScrollStrategy::StickToTop,
                "bottom" => ScrollStrategy::StickToBottom,
                _ => ScrollStrategy::KeepRow,
            })
        }

        if $define.v_k("name").is_some() {
            $reg.borrow_mut().insert($define.v_s_rawk("name"), stringify!($type).to_string());

            if size_w.is_some() || size_h.is_some() {
                if let Some(scroll_strat) = scroll_strat {
                    $view
                        .with_name($define.v_s_rawk("name"))
                        .resized(
                            size_w.unwrap_or(SizeConstraint::Free),
                            size_h.unwrap_or(SizeConstraint::Free),
                        )
                        .scrollable()
                        .scroll_strategy(scroll_strat)
                        .into_boxed_view()
                } else {
                    $view
                        .with_name($define.v_s_rawk("name"))
                        .resized(
                            size_w.unwrap_or(SizeConstraint::Free),
                            size_h.unwrap_or(SizeConstraint::Free),
                        )
                        .into_boxed_view()
                }
            } else {
                if let Some(scroll_strat) = scroll_strat {
                    $view
                        .with_name($define.v_s_rawk("name"))
                        .scrollable()
                        .scroll_strategy(scroll_strat)
                        .into_boxed_view()
                } else {
                    $view.with_name($define.v_s_rawk("name")).into_boxed_view()
                }
            }
        } else {
            if size_w.is_some() || size_h.is_some() {
                if let Some(scroll_strat) = scroll_strat {
                    $view
                        .resized(
                            size_w.unwrap_or(SizeConstraint::Free),
                            size_h.unwrap_or(SizeConstraint::Free),
                        )
                        .scrollable()
                        .scroll_strategy(scroll_strat)
                        .into_boxed_view()
                } else {
                    $view
                        .resized(
                            size_w.unwrap_or(SizeConstraint::Free),
                            size_h.unwrap_or(SizeConstraint::Free),
                        )
                        .into_boxed_view()
                }
            } else {
                if let Some(scroll_strat) = scroll_strat {
                    $view.scrollable().scroll_strategy(scroll_strat).into_boxed_view()
                } else {
                    $view.into_boxed_view()
                }
            }
        }
    }};
}

macro_rules! add_cb_handler {
    ($cursive: ident, $define: ident, $env: ident, $view: ident, $func: ident, $event_str: literal $(,$args: ident)* : $($build: expr,)*) => {
        {
            if $define.v_k($event_str).is_some() {
                let cb = $define.v_k($event_str);
                let denv = Rc::new(RefCell::new($env.derive()));
                $view.$func(move |s $(,$args)*| call_callback!(s, cb, denv $(,$build)*))

            } else {
                add_default_cb!($cursive, $define, $env, $view, $func, $event_str $(,$args)* : $($build,)*)
            }
        }
    }
}

macro_rules! add_default_cb {
    ($cursive: ident, $define: ident, $env: ident, $view: ident, $func: ident, $event_str: literal $(,$args: ident)* : $($build: expr,)*) => {
        {
//            let denv = Rc::new(RefCell::new($env.derive()));
            let cb_name = if $define.v_k("name").is_some() {
                $define.v_s_rawk("name")
            } else {
                "*".to_string()
            };

            $view.$func(move |$cursive $(,$args)*| {
                let mut cb = if let Some(ud) = $cursive.user_data::<WLambdaCursiveContext>() {
                    ud.default_callbacks.get_mut(&cb_name).map(|t| t.take()).flatten()
                } else {
                    None
                };

                if let Some(cb) = cb.as_mut() {
                    let value = VVal::vec();

                    for arg in [VVal::new_str($event_str), $($build,)*] {
                        value.push(arg);
                    }

                    (cb)($cursive, value);
                }

                if let Some(ud) = $cursive.user_data::<WLambdaCursiveContext>() {
                    if let Some(map_cb) = ud.default_callbacks.get_mut(&cb_name) {
                        std::mem::swap(map_cb, &mut cb);
                    }
                }
            })
        }
    }
}

#[cfg(feature = "cursive")]
fn vv2view(
    v: &VVal,
    env: &mut Env,
    reg: &ViewNameRegistry,
) -> Result<Box<(dyn cursive::View + 'static)>, String> {
    let typ = v.v_(0);
    let define = v.v_(1);

    let typ_str = &typ.s_raw()[..];

    match typ_str {
        "hbox" => {
            let mut ll = LinearLayout::new(Orientation::Horizontal);
            define.with_iter(|it| {
                for (v, _) in it {
                    ll.add_child(vv2view(&v, env, reg)?);
                }

                Ok::<(), String>(())
            })?;

            Ok(auto_wrap_view!(ll, define, linearlayout, reg, cursive, env))
        }
        "vbox" => {
            let mut ll = LinearLayout::new(Orientation::Vertical);
            define.with_iter(|it| {
                for (v, _) in it {
                    ll.add_child(vv2view(&v, env, reg)?);
                }

                Ok::<(), String>(())
            })?;

            Ok(auto_wrap_view!(ll, define, linearlayout, reg, cursive, env))
        }
        "panel" => {
            let (define, child) = (define.v_(0), define.v_(1));
            let mut pnl = Panel::new(BoxedView::new(vv2view(&child, env, reg)?));
            if define.v_k("title").is_some() {
                pnl.set_title(define.v_s_rawk("title"));
            }
            Ok(auto_wrap_view!(pnl, define, panel, reg, cursive, env))
        }
        "edit" => {
            let mut edit = EditView::new();

            if define.v_k("maxlen").is_some() {
                edit.set_max_content_width(Some(define.v_ik("maxlen") as usize));
            }

            if define.v_bk("secret") {
                edit.set_secret(true);
            }

            if define.v_k("filler").is_some() {
                edit.set_filler(define.v_s_rawk("filler"));
            }

            add_cb_handler!(
                cursive,
                define,
                env,
                edit,
                set_on_edit,
                "on_edit",
                text,
                cursor: VVal::new_str(text),
                VVal::Int(cursor as i64),
            );
            add_cb_handler!(
                cursive,
                define,
                env,
                edit,
                set_on_submit,
                "on_submit",
                text: VVal::new_str(text),
            );

            Ok(auto_wrap_view!(edit, define, edit, reg, cursive, env))
        }
        "stack" => {
            let mut stk = StackView::new();
            define.v_k("layers").with_iter(|it| {
                for (v, _) in it {
                    stk.add_fullscreen_layer(vv2view(&v, env, reg)?);
                }
                Ok::<(), String>(())
            })?;

            Ok(auto_wrap_view!(stk, define, stack, reg, cursive, env))
        }
        "list" => {
            let mut lst = ListView::new();

            add_cb_handler!(
                cursive,
                define,
                env,
                lst,
                set_on_select,
                "on_select",
                item: VVal::new_str(item),
            );

            define.v_k("childs").with_iter(|it| {
                for (v, _) in it {
                    if v.is_none() {
                        lst.add_delimiter();
                    } else {
                        lst.add_child(&v.v_s_raw(0), vv2view(&v.v_(1), env, reg)?);
                    }
                }
                Ok::<(), String>(())
            })?;

            Ok(auto_wrap_view!(lst, define, list, reg, cursive, env))
        }
        "button" => {
            let mut view = Button::new(define.v_s_rawk("label"), |_| ());

            add_cb_handler!(cursive, define, env, view, set_callback, "on_press"  : );

            Ok(auto_wrap_view!(view, define, button, reg, cursive, env))
        }
        "radio" => {
            let orientation = if define.v_bk("horizontal") {
                Orientation::Horizontal
            } else {
                Orientation::Vertical
            };

            let mut view = LinearLayout::new(orientation);
            let mut group: RadioGroup<VVal> = RadioGroup::new();

            add_cb_handler!(cursive, define, env, group, set_on_change, "on_change", v : v.clone(),);

            let prefix =
                if define.v_k("name").is_some() { Some(define.v_s_rawk("name")) } else { None };

            define.v_k("buttons").with_iter(|it| {
                // v: Label => Value
                let mut i = 0;
                for (v, _) in it {
                    let sub_view = group.button(v.v_(1), v.v_s_raw(0));

                    if let Some(prefix) = &prefix {
                        let name = format!("{}_{}", prefix.to_string(), i);
                        i += 1;
                        reg.borrow_mut().insert(name.clone(), "radiobutton".to_string());

                        view.add_child(sub_view.with_name(name));
                    } else {
                        view.add_child(sub_view);
                    }
                }
            });

            Ok(auto_wrap_view!(view, define, linearlayout, reg, cursive, env))
        }
        "checkbox" => {
            let mut view = if define.v_bk("checked") {
                Checkbox::new().with_checked(true)
            } else {
                Checkbox::new().with_checked(false)
            };

            add_cb_handler!(
                cursive,
                define,
                env,
                view,
                set_on_change,
                "on_change",
                checked: VVal::Bol(checked),
            );

            Ok(auto_wrap_view!(view, define, checkbox, reg, cursive, env))
        }
        "hslider" | "vslider" => {
            let mut view = if typ_str == "vslider" {
                SliderView::vertical(define.v_ik("max") as usize)
            } else {
                SliderView::horizontal(define.v_ik("max") as usize)
            };

            view.set_value(define.v_ik("value") as usize);

            let view = add_cb_handler!(
                cursive,
                define,
                env,
                view,
                on_change,
                "on_change",
                value: VVal::Int(value as i64),
            );

            let view = add_cb_handler!(
                cursive,
                define,
                env,
                view,
                on_enter,
                "on_enter",
                value: VVal::Int(value as i64),
            );

            Ok(auto_wrap_view!(view, define, slider, reg, cursive, env))
        }
        "select" => {
            let mut view: SelectView<VVal> = SelectView::new();

            define.v_k("items").with_iter(|it| {
                for (v, _) in it {
                    view.add_item(v.v_s_raw(0), v.v_(1));
                }
            });

            if define.v_bk("popup") {
                view.set_popup(true);
            }

            if define.v_bk("autojump") {
                view.set_autojump(true);
            }

            add_cb_handler!(
                cursive,
                define,
                env,
                view,
                set_on_select,
                "on_select",
                v: v.clone(),
            );

            add_cb_handler!(
                cursive,
                define,
                env,
                view,
                set_on_submit,
                "on_submit",
                v: v.clone(),
            );

            let view = if define.v_k("h_align").is_some() {
                let halign = define.v_s_rawk("h_align");
                let halign = match &halign[..] {
                    "right" => cursive::align::HAlign::Right,
                    "center" => cursive::align::HAlign::Center,
                    _ => cursive::align::HAlign::Left,
                };
                view.h_align(halign)
            } else {
                view
            };

            let view = if define.v_k("v_align").is_some() {
                let valign = define.v_s_rawk("v_align");
                let valign = match &valign[..] {
                    "bottom" => cursive::align::VAlign::Bottom,
                    "center" => cursive::align::VAlign::Center,
                    _ => cursive::align::VAlign::Top,
                };
                view.v_align(valign)
            } else {
                view
            };

            let view = if define.v_k("select").is_some() {
                view.selected(define.v_ik("select") as usize)
            } else {
                view
            };

            Ok(auto_wrap_view!(view, define, select, reg, cursive, env))
        }
        "textview" => {
            let mut view = TextView::new(define.v_s_rawk("content"));
            if define.v_bk("no_wrap") {
                view.set_content_wrap(false);
            }

            let view = if define.v_k("h_align").is_some() {
                let halign = define.v_s_rawk("h_align");
                let halign = match &halign[..] {
                    "right" => cursive::align::HAlign::Right,
                    "center" => cursive::align::HAlign::Center,
                    _ => cursive::align::HAlign::Left,
                };
                view.h_align(halign)
            } else {
                view
            };

            let view = if define.v_k("v_align").is_some() {
                let valign = define.v_s_rawk("v_align");
                let valign = match &valign[..] {
                    "bottom" => cursive::align::VAlign::Bottom,
                    "center" => cursive::align::VAlign::Center,
                    _ => cursive::align::VAlign::Top,
                };
                view.v_align(valign)
            } else {
                view
            };

            Ok(auto_wrap_view!(view, define, textview, reg, cursive, env))
        }
        "stdout" => {
            let textcontent = CURSIVE_STDOUT.with(|cs| cs.borrow().clone());
            let view = TextView::new_with_content(textcontent);
            Ok(auto_wrap_view!(view, define, textview, reg, cursive, env))
        }
        _ => Err(format!("Unknown view type: '{}'", typ.s())),
    }
}

pub fn handle_cursive_call_method(
    method: &str,
    argv: &VVal,
    env: &mut Env,
    cursive: &mut Cursive,
    reg: &ViewNameRegistry,
) -> Result<VVal, StackAction> {
    let ptr: *mut Cursive = cursive;

    match method {
        "add_layer" => {
            assert_arg_count!("$<Cursive>", argv, 1, "add_layer[view]", env);
            expect_view!(&argv.v_(0), "$<NamedViewHandle>.add_layer", new_view, reg, env, {
                cursive.add_layer(new_view);
                Ok(VVal::None)
            })
        }
        "run" => {
            assert_arg_count!("$<Cursive>", argv, 0, "run[]", env);
            cursive.run();
            Ok(VVal::None)
        }
        "sender" => {
            assert_arg_count!("$<Cursive>", argv, 0, "sender[]", env);
            Ok(VVal::new_usr(SendMessageHandle { cb: cursive.cb_sink().clone() }))
        }
        "set_window_title" => {
            assert_arg_count!("$<Cursive>", argv, 1, "set_window_title[title_string]", env);
            argv.v_(0).with_s_ref(|s| cursive.set_window_title(s));
            Ok(VVal::None)
        }
        "init_console_logging" => {
            assert_arg_count!("$<Cursive>", argv, 0, "init_console_logging[]", env);
            cursive::logger::init();
            Ok(VVal::None)
        }
        "toggle_debug_console" => {
            assert_arg_count!("$<Cursive>", argv, 0, "toggle_debug_console[]", env);
            cursive.toggle_debug_console();
            Ok(VVal::None)
        }
        "show_debug_console" => {
            assert_arg_count!("$<Cursive>", argv, 0, "show_debug_console[]", env);
            cursive.show_debug_console();
            Ok(VVal::None)
        }
        "default_cb" => {
            assert_arg_count!(
                "$<Cursive>",
                argv,
                2,
                "view_default_cb[widget_name, callback_fun]",
                env
            );
            let name = argv.v_s_raw(0);
            let cb = argv.v_(1);
            let denv = Rc::new(RefCell::new(env.derive()));

            if let Some(ud) = cursive.user_data::<WLambdaCursiveContext>() {
                ud.default_callbacks.insert(
                    name,
                    Some(Box::new(move |s: &mut Cursive, args: VVal| {
                        if args.len() == 1 {
                            call_callback!(s, cb, denv, args.v_(0));
                        } else if args.len() == 2 {
                            call_callback!(s, cb, denv, args.v_(0), args.v_(1));
                        } else if args.len() == 3 {
                            call_callback!(s, cb, denv, args.v_(0), args.v_(1), args.v_(2));
                        } else if args.len() == 4 {
                            call_callback!(s, cb, denv, args.v_(0), args.v_(1), args.v_(3));
                        } else {
                            panic!("There are no default callbacks implemented for more than 3 arguments!");
                        }
                    })),
                );
            }

            Ok(VVal::None)
        }
        "send_cb" => {
            assert_arg_count!("$<Cursive>", argv, 2, "register_cb[event_tag, callback_fun]", env);
            let tag = argv.v_s_raw(0);
            let cb = argv.v_(1);
            let denv = Rc::new(RefCell::new(env.derive()));

            if let Some(ud) = cursive.user_data::<WLambdaCursiveContext>() {
                ud.callbacks.insert(
                    tag,
                    Some(Box::new(move |s: &mut Cursive, v: VVal| {
                        call_callback!(s, cb, denv, v);
                    })),
                );
            }

            Ok(VVal::None)
        }
        "msg" => {
            if argv.len() < 1 || argv.len() > 2 {
                return Err(StackAction::panic_str(
                    "$<Cursive>.msg([callback], text) expects 1 or 2 arguments".to_string(),
                    None,
                    env.argv(),
                ));
            }

            let cb = if argv.len() > 1 { argv.v_(0) } else { VVal::None };
            let text = if argv.len() > 1 { argv.v_(1) } else { argv.v_(0) };
            let denv = Rc::new(RefCell::new(env.derive()));

            cursive.add_layer(Dialog::around(TextView::new(&text.s_raw())).button(
                "Ok",
                move |s| {
                    s.pop_layer();
                    call_callback!(s, cb, denv);
                },
            ));
            Ok(VVal::None)
        }
        "focus" => {
            assert_arg_count!("$<Cursive>", argv, 1, "focus[name]", env);
            argv.v_(0).with_s_ref(|s| match cursive.focus_name(s) {
                Ok(_) => Ok(VVal::Bol(true)),
                Err(_) => Ok(VVal::Bol(false)),
            })
        }
        "get" => {
            assert_arg_count!("$<Cursive>", argv, 1, "get[name]", env);

            let name = argv.v_s_raw(0);

            let viewtype = if let Some(typ) = reg.borrow().get(&name) {
                match &typ[..] {
                    "stack" => ViewType::StackView,
                    "button" => ViewType::Button,
                    "edit" => ViewType::EditView,
                    "panel" => ViewType::Panel,
                    "textview" => ViewType::TextView,
                    "radiobutton" => ViewType::RadioButton,
                    "checkbox" => ViewType::Checkbox,
                    "slider" => ViewType::Slider,
                    "select" => ViewType::SelectView,
                    _ => {
                        panic!("Unknown viewtype encountered, fatal error in programming: {}", typ);
                    }
                }
            } else {
                return Err(StackAction::panic_str(
                    format!("$<Cursive>.get unknown name: '{}'", name),
                    None,
                    env.argv(),
                ));
            };

            Ok(VVal::new_usr(NamedViewHandle::new(Rc::new(ptr), &name[..], viewtype)))
        }
        "quit" => {
            assert_arg_count!("$<Cursive>", argv, 0, "quit[]", env);

            cursive.quit();

            Ok(VVal::None)
        }
        _ => Err(StackAction::panic_str(
            format!("$<Cursive> unknown method called: {}", method),
            None,
            env.argv(),
        )),
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
        handle_cursive_call_method(key, &argv, env, &mut self.cursive.borrow_mut(), &self.reg)
    }
}

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct SendMessageHandle {
    cb: cursive::CbSink,
}

#[cfg(feature = "cursive")]
impl crate::threads::ThreadSafeUsr for SendMessageHandle {
    fn to_vval(&self) -> VVal {
        VVal::Usr(Box::new(self.clone()))
    }
}

#[cfg(feature = "cursive")]
impl VValUserData for SendMessageHandle {
    fn s(&self) -> String {
        format!("$<Cursive:SendMsgCb>")
    }
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }

    fn call(&self, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv_ref();
        if argv.len() != 2 {
            return Err(StackAction::panic_str(
                "$<Cursive:SendMsgCb>[event_tag, value] called with incorrect number of arguments"
                    .to_string(),
                None,
                env.argv(),
            ));
        }

        let tag = argv[0].s_raw();
        let value = crate::AVal::from_vval(&argv[1]);

        let res = self.cb.send(Box::new(move |s: &mut Cursive| {
            let mut cb = if let Some(ud) = s.user_data::<WLambdaCursiveContext>() {
                ud.callbacks.get_mut(&tag).map(|t| t.take()).flatten()
            } else {
                None
            };

            if let Some(cb) = cb.as_mut() {
                (cb)(s, value.to_vval());
            }

            if let Some(ud) = s.user_data::<WLambdaCursiveContext>() {
                if let Some(map_cb) = ud.callbacks.get_mut(&tag) {
                    std::mem::swap(map_cb, &mut cb);
                }
            }
        }));

        match res {
            Ok(()) => Ok(VVal::Bol(true)),
            Err(e) => Ok(env.new_err(format!("$<Cursive:SendMsgCb error: {}", e))),
        }
    }

    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn crate::threads::ThreadSafeUsr>> {
        Some(Box::new(self.clone()))
    }
}

struct CursiveStdoutWriter();

impl std::io::Write for CursiveStdoutWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        CURSIVE_STDOUT.with(|cs| {
            let s = String::from_utf8_lossy(buf);
            cs.borrow_mut().append(s);
        });
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
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

    #[cfg(feature = "cursive")]
    st.fun(
        "cursive:install_cursive_stdio",
        |env: &mut Env, _argc: usize| {
            let stdio = crate::vval::Stdio {
                write: Rc::new(RefCell::new(CursiveStdoutWriter())),
                read: Rc::new(RefCell::new(std::io::BufReader::new(std::io::stdin()))),
            };
            env.set_stdio(stdio);
            Ok(VVal::None)
        },
        Some(0),
        Some(0),
        false,
    );
}
