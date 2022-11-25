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
use std::collections::HashMap;
#[allow(unused_imports)]
use std::rc::Rc;

#[cfg(feature = "cursive")]
use cursive::view::{IntoBoxedView, Resizable, Scrollable, SizeConstraint};
#[cfg(feature = "cursive")]
use cursive::views::{
    BoxedView, Button, Dialog, EditView, LinearLayout, Panel, StackView, TextView, ViewRef,
};
#[cfg(feature = "cursive")]
use cursive::{direction::Orientation, traits::Nameable, Cursive, CursiveExt};

macro_rules! assert_arg_count {
    ($self: ident, $argv: expr, $count: expr, $function: expr, $env: ident) => {
        if $argv.len() != $count {
            return Err(StackAction::panic_str(
                format!("{}.{} expects {} arguments", $self.s(), $function, $count),
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
                cursive.add_layer(Dialog::around(TextView::new(&text.s_raw())).button(
                    "Ok",
                    move |s| {
                        s.pop_layer();
                        call_callback!(s, cb, denv);
                    },
                ));
                Ok(VVal::None)
            }
            "get" => {
                assert_arg_count!(self, argv, 1, "get[name]", env);

                let cursive: &mut Cursive = unsafe { &mut **self.ptr };
                let ud = cursive
                    .user_data::<WLambdaCursiveContext>()
                    .expect("Userdata must be WLambdaCursiveContext!");
                let reg = ud.get_registry();

                let name = argv.v_s_raw(0);

                let viewtype = if let Some(typ) = reg.borrow().get(&name) {
                    match &typ[..] {
                        "stack" => ViewType::StackView,
                        "button" => ViewType::Button,
                        "edit" => ViewType::EditView,
                        "panel" => ViewType::Panel,
                        _ => {
                            panic!(
                                "Unknown viewtype encountered, fatal error in programming: {}",
                                typ
                            );
                        }
                    }
                } else {
                    return Err(StackAction::panic_str(
                        format!("$<CursiveAPI>.get unknown name: '{}'", name),
                        None,
                        env.argv(),
                    ));
                };

                Ok(VVal::new_usr(NamedViewHandle::new(self.ptr.clone(), &name[..], viewtype)))
            }
            "quit" => {
                assert_arg_count!(self, argv, 0, "quit[]", env);

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
struct WLambdaCursiveContext {
    callbacks: HashMap<String, Option<Box<dyn FnMut(&mut Cursive, VVal)>>>,
    reg: ViewNameRegistry,
}

impl WLambdaCursiveContext {
    pub fn new() -> Self {
        let reg = Rc::new(RefCell::new(HashMap::new()));

        Self { callbacks: HashMap::new(), reg }
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
        let ud = WLambdaCursiveContext::new();
        let reg = ud.get_registry();
        cursive.borrow_mut().set_user_data(ud);
        Self { cursive, reg }
    }
}

#[derive(Debug, Clone)]
enum ViewType {
    StackView,
    EditView,
    Button,
    Panel,
}

type ViewNameRegistry = Rc<RefCell<HashMap<String, String>>>;

#[cfg(feature = "cursive")]
#[derive(Clone)]
struct NamedViewHandle {
    ptr: Rc<*mut Cursive>,
    reg: ViewNameRegistry,
    name: String,
    typ: ViewType,
}

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
    ($self: ident, $cursive: ident, $type: ident, $name: ident, $env: ident, $block: tt) => {{
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
                    assert_arg_count!(self, argv, 1, "set_label[text]", env);
                    access_named_view!(self, Button, view, env, {
                        view.set_label(argv.v_s_raw(0));
                        Ok(VVal::None)
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::EditView => match key {
                "set_content" => {
                    assert_arg_count!(self, argv, 1, "set_content[new_text]", env);
                    access_named_view_ctx!(self, cursive, EditView, view, env, {
                        (view.set_content(argv.v_s_raw(0)))(cursive);
                        Ok(VVal::None)
                    })
                }
                "content" => {
                    assert_arg_count!(self, argv, 0, "content[]", env);
                    access_named_view_ctx!(self, cursive, EditView, view, env, {
                        Ok(VVal::new_str(&view.get_content()))
                    })
                }
                _ => named_view_error!(self, env, key),
            },
            ViewType::StackView => match key {
                "pop_layer" => {
                    assert_arg_count!(self, argv, 0, "pop_layer[]", env);
                    access_named_view!(self, StackView, view, env, {
                        view.pop_layer();
                        Ok(VVal::None)
                    })
                }
                "add_layer" => {
                    assert_arg_count!(self, argv, 1, "add_layer[view]", env);
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

macro_rules! auto_wrap_view {
    ($view: expr, $define: ident, $type: ident, $reg: expr) => {{
        let size_w = vv2size_const(&$define.v_k("width"));
        let size_h = vv2size_const(&$define.v_k("height"));

        let scrollable = $define.v_bk("scrollable");

        if $define.v_k("name").is_some() {
            $reg.borrow_mut().insert($define.v_s_rawk("name"), stringify!($type).to_string());

            if size_w.is_some() || size_h.is_some() {
                if scrollable {
                    $view
                        .with_name($define.v_s_rawk("name"))
                        .resized(
                            size_w.unwrap_or(SizeConstraint::Free),
                            size_h.unwrap_or(SizeConstraint::Free),
                        )
                        .scrollable()
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
                if scrollable {
                    $view.with_name($define.v_s_rawk("name")).scrollable().into_boxed_view()
                } else {
                    $view.with_name($define.v_s_rawk("name")).into_boxed_view()
                }
            }
        } else {
            if size_w.is_some() || size_h.is_some() {
                if scrollable {
                    $view
                        .resized(
                            size_w.unwrap_or(SizeConstraint::Free),
                            size_h.unwrap_or(SizeConstraint::Free),
                        )
                        .scrollable()
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
                if scrollable {
                    $view.scrollable().into_boxed_view()
                } else {
                    $view.into_boxed_view()
                }
            }
        }
    }};
}

#[cfg(feature = "cursive")]
fn vv2view(
    v: &VVal,
    env: &mut Env,
    reg: &ViewNameRegistry,
) -> Result<Box<(dyn cursive::View + 'static)>, String> {
    let typ = v.v_(0);
    let define = v.v_(1);

    match &typ.s_raw()[..] {
        "hbox" => {
            let mut ll = LinearLayout::new(Orientation::Horizontal);
            define.with_iter(|it| {
                for (v, _) in it {
                    ll.add_child(vv2view(&v, env, reg)?);
                }

                Ok::<(), String>(())
            })?;

            Ok(ll.into_boxed_view())
        }
        "vbox" => {
            let mut ll = LinearLayout::new(Orientation::Vertical);
            define.with_iter(|it| {
                for (v, _) in it {
                    ll.add_child(vv2view(&v, env, reg)?);
                }

                Ok::<(), String>(())
            })?;

            Ok(ll.into_boxed_view())
        }
        "panel" => {
            let (define, child) = (define.v_(0), define.v_(1));
            let mut pnl = Panel::new(BoxedView::new(vv2view(&child, env, reg)?));
            if define.v_k("title").is_some() {
                pnl.set_title(define.v_s_rawk("title"));
            }
            Ok(auto_wrap_view!(pnl, define, panel, reg))
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

            if define.v_k("on_edit").is_some() {
                let cb = define.v_k("on_edit");
                let denv = Rc::new(RefCell::new(env.derive()));
                edit.set_on_edit(move |s, text, cursor| {
                    call_callback!(s, cb, denv, VVal::new_str(text), VVal::Int(cursor as i64))
                });
            }

            if define.v_k("on_submit").is_some() {
                let cb = define.v_k("on_submit");
                let denv = Rc::new(RefCell::new(env.derive()));
                edit.set_on_submit(move |s, text| call_callback!(s, cb, denv, VVal::new_str(text)));
            }

            Ok(auto_wrap_view!(edit, define, edit, reg))
        }
        "stack" => {
            let mut stk = StackView::new();
            define.v_k("layers").with_iter(|it| {
                for (v, _) in it {
                    stk.add_fullscreen_layer(vv2view(&v, env, reg)?);
                }
                Ok::<(), String>(())
            })?;

            Ok(auto_wrap_view!(stk, define, stack, reg))
        }
        "button" => {
            let cb = define.v_k("cb");
            let denv = Rc::new(RefCell::new(env.derive()));

            let view = Button::new(define.v_s_rawk("label"), move |s| call_callback!(s, cb, denv));

            Ok(auto_wrap_view!(view, define, button, reg))
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
                assert_arg_count!(self, argv, 1, "add_layer[view]", env);
                expect_view!(
                    &argv.v_(0),
                    "$<NamedViewHandle>.add_layer",
                    new_view,
                    self.reg,
                    env,
                    {
                        self.cursive.borrow_mut().add_layer(new_view);
                        Ok(VVal::None)
                    }
                )
            }
            "run" => {
                assert_arg_count!(self, argv, 0, "run[]", env);
                self.cursive.borrow_mut().run();
                Ok(VVal::None)
            }
            "sender" => {
                assert_arg_count!(self, argv, 0, "sender[]", env);
                Ok(VVal::new_usr(SendMessageHandle {
                    cb: self.cursive.borrow_mut().cb_sink().clone(),
                }))
            }
            "register_cb" => {
                assert_arg_count!(self, argv, 2, "register_cb[event_tag, callback_fun]", env);
                let tag = argv.v_s_raw(0);
                let cb = argv.v_(1);
                let denv = Rc::new(RefCell::new(env.derive()));

                if let Some(ud) = self.cursive.borrow_mut().user_data::<WLambdaCursiveContext>() {
                    ud.callbacks.insert(
                        tag,
                        Some(Box::new(move |s: &mut Cursive, v: VVal| {
                            call_callback!(s, cb, denv, v);
                        })),
                    );
                }

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
