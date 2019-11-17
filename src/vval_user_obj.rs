#[macro_export]
/// This macro helps with exporting something with a more OO interface
/// instead of using a VValUserData trait implementation.
///
///```
/// use wlambda::*;
/// use wlambda::set_vval_method;
///
/// let mut ctx = EvalContext::new_default();
///
/// let some_ref = std::rc::Rc::new(std::cell::RefCell::new(10));
/// let obj = VVal::map();
///
/// set_vval_method!(obj, some_ref, get_it, None, None, _env, _argc, {
///     Ok(VVal::Int(*some_ref.borrow()))
/// });
///
/// ctx.set_global_var("I", &obj);
///
/// assert_eq!(ctx.eval("I.get_it[]").unwrap().s(), "10");
/// *some_ref.borrow_mut() += 11;
/// assert_eq!(ctx.eval("I.get_it[]").unwrap().s(), "21");
///```
macro_rules! set_vval_method {
    ($vv: expr, $obj: ident, $method: tt, $min: expr, $max: expr, $env: ident, $argc: ident, $b: block) => {
        {
            let $obj = $obj.clone();
            $vv.set_map_key_fun(stringify!($method).to_string(),
                move |$env: &mut Env, $argc: usize| $b, $min, $max, false);
        }
    }
}
