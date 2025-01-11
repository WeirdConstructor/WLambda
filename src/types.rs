// Copyright (c) 2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Implements a type checker for WLambda, which is an extra compile pass over the AST.
The AST as it comes directly from the Parser. The type checker will also augment the AST with
type information of course.
*/

use std::cell::RefCell;
use std::rc::Rc;

use crate::compiler::CompileEnv;
use crate::ops::BinOp;
use crate::vval::{resolve_type, CompileError, Syntax, Type, TypeResolveResult, VVal, VarPos};

#[derive(Debug, Clone)]
pub(crate) enum TypeHint {
    Infer,
    Expect(Rc<Type>),
}

impl TypeHint {
    fn from_type(ty: &Rc<Type>) -> Self {
        if ty.is_none() {
            TypeHint::Infer
        } else {
            TypeHint::Expect(ty.clone())
        }
    }

    fn expect(&self) -> Option<&Type> {
        if let TypeHint::Expect(ty) = self {
            Some(ty.as_ref())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct TypedVVal {
    pub vval: VVal,
    pub typ: Rc<Type>,
}

impl TypedVVal {
    pub fn new(typ: Rc<Type>, vval: VVal) -> Self {
        Self { vval, typ }
    }
}

impl std::fmt::Display for TypedVVal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypedVVal vval={}, typ={}", self.vval, self.typ)
    }
}

fn type_var(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    capt_ref: bool,
) -> Result<TypedVVal, CompileError> {
    let var = ast.at(1).unwrap();
    let mut typ_val = var.with_s_ref(|var_s: &str| -> Result<TypedVVal, CompileError> {
        match var_s {
            "_" => {
                ce.borrow_mut().set_impl_arity(1);
            }
            "_1" => {
                ce.borrow_mut().set_impl_arity(2);
            }
            "_2" => {
                ce.borrow_mut().set_impl_arity(3);
            }
            "_3" => {
                ce.borrow_mut().set_impl_arity(4);
            }
            "_4" => {
                ce.borrow_mut().set_impl_arity(5);
            }
            "_5" => {
                ce.borrow_mut().set_impl_arity(6);
            }
            "_6" => {
                ce.borrow_mut().set_impl_arity(7);
            }
            "_7" => {
                ce.borrow_mut().set_impl_arity(8);
            }
            "_8" => {
                ce.borrow_mut().set_impl_arity(9);
            }
            "_9" => {
                ce.borrow_mut().set_impl_arity(10);
            }
            "@" => {
                ce.borrow_mut().set_impl_arity_infinite();
                // TODO: @ type is a record/typed array, that is determined by the
                //       function this code resides in. We can set the arity here,
                //       implicitly, but to determine the type we need to check the function.
                // XXX: It might not be possible to do this without inference. We don't want to give
                //      an explicit function type to everything.
            }
            _ => {
                let (pos, typ) = ce.borrow_mut().get(var_s);
                if let VarPos::NoPos = pos {
                    return Err(ast.compile_err(format!("Variable '{}' undefined", var_s)));
                }
                println!("GET VAR TYPE {} => {}", var_s, typ.s());
                if capt_ref {
                    return Ok(TypedVVal::new(Type::ref_type(typ), ast.clone()));
                } else {
                    return Ok(TypedVVal::new(typ, ast.clone()));
                }
            }
        }
        Ok(TypedVVal::new(Type::rc_new_var(&format!("V{}_", var_s)), ast.clone()))
    })?;
    if capt_ref {
        typ_val.typ = Rc::new(Type::Ref(typ_val.typ));
    }
    Ok(typ_val)
}

fn type_binop(
    ast: &VVal,
    op: BinOp,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let (syn, a, b) = (ast.v_(0), ast.v_(1), ast.v_(2));
    //    println!("SYN: {:?}", ce.borrow_mut().get("+"));
    // TODO: Get Type of BinOp from Environment by `syn`.
    let op_type = match ce.borrow_mut().get_type(op.token()) {
        Some(t) => t,
        None => return Err(ast.compile_err(format!("Unknown type of operator: {}", ast.s()))),
    };
    //    if !op_type.is_resolved() {
    //        return Err(ast.compile_err(format!("Operator has unknown type: {}", op_type.s(),)));
    //    }
    //d// println!("TYPE: {:?}", op_type);

    // TODO: Get union type from first argument!
    let a_type = type_pass(&a, TypeHint::Infer, ce)?;

    // TODO: Get union type from second argument!
    let b_type = type_pass(&b, TypeHint::Infer, ce)?;

    let ret_type = if let TypeHint::Expect(t) = type_hint { t.as_type() } else { Type::Any };
    let chk_typ = Type::fun_2_ret("a", (*a_type.typ).clone(), "b", (*b_type.typ).clone(), ret_type);

    let operation_typ = resolve_and_check(&op_type, &chk_typ, ce, ast, || {
        format!("operator call '{}'", op.token())
    })?;

    if let Type::Function(_args, ret_type, _bound_vars) = operation_typ.as_ref() {
        eprintln!("RetType: {}", ret_type.s());
        Ok(TypedVVal::new(ret_type.clone(), ast.clone()))
    } else {
        Err(ast.compile_err(format!(
            "Expected function type from operator {}, got: {}",
            syn.s(),
            operation_typ.s()
        )))
    }
}

fn type_func(
    ast: &VVal,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    // let mut fun_spos = spos.clone();
    // fun_spos.set_name(&ce.borrow().recent_var);
    //
    // let mut func_ce = CompileEnv::create_env(Some(ce.clone()));
    // let ce_sub = func_ce.clone();
    //
    // let label = ast.at(1).unwrap();
    // let explicit_arity = ast.at(2).unwrap();
    //
    // let mut func_prog = Prog::new();
    //
    // let func_pw = compile_stmts(ast, 3, &mut func_ce)?;
    // func_pw.eval_to(&mut func_prog, ResPos::Value(ResValue::Ret));
    // func_prog.op_end();
    //
    // let func_prog = Rc::new(func_prog);
    //
    // let func_typ = if explicit_arity.is_type() {
    //     ce_sub.borrow_mut().explicit_arity.0 = ArityParam::Undefined;
    //     ce_sub.borrow_mut().explicit_arity.1 = ArityParam::Undefined;
    //     explicit_arity.t()

    Ok(TypedVVal::new(Type::none(), ast.clone()))
}

fn type_block(
    ast: &VVal,
    skip_cnt: usize,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    let mut last_type = Type::none();
    let stmts = ast.map_skip_vval(
        |e, is_last| {
            let tv = type_pass(
                e,
                if is_last { type_hint.clone() } else { TypeHint::Expect(Type::any()) },
                ce,
            )?;
            last_type = tv.typ;
            Ok(tv.vval)
        },
        skip_cnt,
    )?;
    Ok(TypedVVal::new(last_type, stmts))
}

fn type_def(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_global: bool,
) -> Result<TypedVVal, CompileError> {
    let (vars, value, destr, types) = (ast.v_(1), ast.v_(2), ast.v_(3), ast.v_(4));

    if destr.b() {
        // TODO // check_for_at_arity(prev_max_arity, ast, ce, &vars);

        if let VVal::Lst(b, _) = vars.clone() {
            for (i, v) in b.borrow().iter().enumerate() {
                ce.borrow_mut().def(&v.s_raw(), is_global, types.v_(i).t());
            }
        }
    } else {
        let varname = vars.v_s_raw(0);
        ce.borrow_mut().recent_var = varname.clone();

        let var_typ = types.v_(0).t();
        println!("VAR TYPE DEF: {} = {}", varname, var_typ.s());

        if is_global {
            let tv = type_pass(&value, TypeHint::from_type(&var_typ), ce)?;
            let _typ = tv.typ;

            // If typ == None, but var_typ != None         => Error
            // If var_typ != None and not(var_typ ISA typ) => Error
            // If var_typ == None and typ == None          => Error
            // If var_typ != None and var_typ ISA typ      => Ok

            if let VarPos::Global(_r) = ce.borrow_mut().def(&varname, true, var_typ) {
                // everything is fine!
            } else {
                panic!("Defining global did not return a global!");
            }
        } else {
            let next_local = ce.borrow_mut().next_local();
            let tv = type_pass(&value, TypeHint::from_type(&var_typ), ce)?;
            println!("TPPPP: {} <=> {}", tv, var_typ);
            let typ = tv.typ;

            let real_var_typ = resolve_and_check(&var_typ, &typ, ce, ast, || {
                format!("variable definition '{}'", &varname)
            })?;
            ce.borrow_mut().def_local(&varname, next_local, real_var_typ);
        }
    }

    println!("Vars: {}, Types: {}", vars.s(), types.s());

    Ok(TypedVVal::new(Type::any(), ast.clone()))
}

fn type_deftype(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_global: bool,
) -> Result<TypedVVal, CompileError> {
    let (typename, typ) = (ast.v_s_raw(1), ast.v_(2));

    let new_typ = if Some('@') == typename.chars().nth(0) {
        Type::aliased(&typename)
    } else {
        Type::named(&typename)
    };

    if is_global {
        if let VarPos::Global(r) = ce.borrow_mut().def(&typename, true, new_typ) {
            r.set_ref(typ);
        } else {
            panic!("Defining global did not return a global!");
        }
    } else {
        let next_local = ce.borrow_mut().next_local();
        ce.borrow_mut().def_local(&typename, next_local, new_typ);
        println!("DEFTYPE {} => {}", typename, typ);
        ce.borrow_mut().set_compiletime_value(&typename, &typ);
    }

    Ok(TypedVVal::new(Type::typ(), ast.clone()))
}

fn resolve_and_check<F>(
    typ: &Rc<Type>,
    chk_typ: &Rc<Type>,
    ce: &mut Rc<RefCell<CompileEnv>>,
    ast: &VVal,
    err_cb: F,
) -> Result<Rc<Type>, CompileError>
where
    F: Fn() -> String,
{
    let mut bound_vars = vec![];
    let res = resolve_type(typ, chk_typ, &mut bound_vars, &mut |name| {
        let (value, vartype) = ce.borrow_mut().get_compiletime_value(name)?;
        eprintln!("get comptime {}: {} => {} {:?}", name, value, vartype.s(), vartype);
        if vartype.is_alias() {
            eprintln!("IS ALIAS! {}", name);
            Some(value.t())
        } else if vartype.is_type() {
            Some(value.t())
        } else {
            None
        }
    });

    match res {
        TypeResolveResult::Match { typ } => Ok(typ),
        TypeResolveResult::Conflict { expected, got, reason } => {
            return Err(ast.compile_err(format!(
                "Type error, expected ({}), but got ({}); in {}\n=> reason: {}",
                expected.s(),
                got.s(),
                err_cb(),
                reason
            )));
        }
    }
}

fn type_assign(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    is_ref: bool,
) -> Result<TypedVVal, CompileError> {
    let (vars, value, destr) = (ast.v_(1), ast.v_(2), ast.v_(3));

    if destr.b() {
        // let val_pw = compile(&value, ce)?;

        // TODO // let prev_max_arity = ce.borrow().implicit_arity.clone();
        // TODO // check_for_at_arity(prev_max_arity, ast, ce, &vars);

        // 1. get types for vars
        // 2. get types for values
        // 3. check value types against var types.
        // let poses = vars.map_ok_skip(|v| ce.borrow_mut().get(&v.s_raw()).0, 0);
        Ok(TypedVVal::new(Type::none(), ast.clone()))
    } else {
        let varname = &vars.v_s_raw(0);
        let (pos, var_type) = ce.borrow_mut().get(varname);

        match pos {
            VarPos::Const(_) => {
                return Err(ast.compile_err(format!("Can't assign to constant '{}'", varname)))
            }
            VarPos::NoPos => {
                return Err(ast.compile_err(format!(
                    "Can't assign to undefined local variable '{}'",
                    varname
                )))
            }
            _ => (),
        }

        let value_type = type_pass(&value, TypeHint::from_type(&var_type), ce)?;
        let new_ast = VVal::vec4(ast.v_(0), vars, value_type.vval, destr);

        resolve_and_check(&var_type, &value_type.typ, ce, ast, || {
            format!("assignment to '{}'", &varname)
        })?;

        Ok(TypedVVal::new(Type::none(), new_ast))
    }
}

/// Runs the type checker pass over the AST.
/// TODO: Do we do preparative steps for the compiler/code generator?
#[allow(clippy::cognitive_complexity)]
pub(crate) fn type_pass(
    ast: &VVal,
    type_hint: TypeHint,
    ce: &mut Rc<RefCell<CompileEnv>>,
) -> Result<TypedVVal, CompileError> {
    match ast {
        VVal::Lst(_, _) => {
            let syn = ast.at(0).unwrap_or(VVal::None).get_syn();
            let v = match syn {
                Syntax::Key => TypedVVal::new(Type::sym(), ast.clone()),
                Syntax::Str => TypedVVal::new(Type::str(), ast.clone()),
                Syntax::Block => {
                    let tv = type_block(ast, 1, type_hint, ce)?;
                    tv.vval.unshift(ast.v_(0));
                    tv
                }
                Syntax::BinOpAdd => type_binop(ast, BinOp::Add, type_hint, ce)?,
                Syntax::BinOpSub => type_binop(ast, BinOp::Sub, type_hint, ce)?,
                Syntax::BinOpDiv => type_binop(ast, BinOp::Div, type_hint, ce)?,
                Syntax::BinOpMod => type_binop(ast, BinOp::Mod, type_hint, ce)?,
                Syntax::BinOpMul => type_binop(ast, BinOp::Mul, type_hint, ce)?,
                Syntax::BinOpGe => type_binop(ast, BinOp::Ge, type_hint, ce)?,
                Syntax::BinOpGt => type_binop(ast, BinOp::Gt, type_hint, ce)?,
                Syntax::BinOpLe => type_binop(ast, BinOp::Le, type_hint, ce)?,
                Syntax::BinOpLt => type_binop(ast, BinOp::Lt, type_hint, ce)?,
                Syntax::BinOpEq => type_binop(ast, BinOp::Eq, type_hint, ce)?,
                Syntax::Var => type_var(ast, ce, false)?,
                Syntax::Def => type_def(ast, ce, false)?,
                Syntax::DefType => type_deftype(ast, ce, false)?,
                Syntax::DefGlobType => type_deftype(ast, ce, true)?,
                Syntax::Assign => type_assign(ast, ce, false)?,
                Syntax::AssignRef => type_assign(ast, ce, true)?,
                Syntax::DumpVM => TypedVVal::new(Type::none(), ast.clone()),
                Syntax::DumpStack => TypedVVal::new(Type::none(), ast.clone()),
                Syntax::Func => type_func(ast, type_hint, ce)?,
                Syntax::TypeOf => {
                    let expr = ast.v_(1);
                    let res_type = type_pass(&expr, type_hint, ce)?;
                    let new_ast =
                        VVal::vec3(ast.v_(0), res_type.vval, VVal::typ(res_type.typ.clone()));
                    TypedVVal::new(res_type.typ, new_ast)
                }
                //                Syntax::DefGlobRef => compile_def(ast, ce, true),
                _ => {
                    return Err(
                        ast.compile_err(format!("type checker got unknown input: {}", ast.s()))
                    )
                }
            };
            Ok(v)
        }
        VVal::Pair(pair) => {
            // TODO: If type_hint is hinting a Pair, then deconstruct that!
            let (th_a, th_b) = if let Some(&Type::Pair(ref atype, ref btype)) = type_hint.expect() {
                (TypeHint::from_type(&atype), TypeHint::from_type(&btype))
            } else {
                (TypeHint::Infer, TypeHint::Infer)
            };

            let a = type_pass(&pair.0, th_a, ce)?;
            let b = type_pass(&pair.1, th_b, ce)?;

            Ok(TypedVVal::new(Type::pair(a.typ, b.typ), VVal::pair(a.vval, b.vval)))
        }
        _ => Ok(TypedVVal::new(ast.t(), ast.clone())),
    }
}

pub(crate) fn type_check(
    ast: &VVal,
    ce: &mut Rc<RefCell<CompileEnv>>,
    types_enabled: bool,
) -> Result<VVal, CompileError> {
    match type_pass(ast, TypeHint::Expect(Type::any()), ce) {
        Ok(tv) => Ok(tv.vval.clone()),
        Err(e) => {
            if types_enabled {
                Err(e)
            } else {
                eprintln!("TYPE ERROR: {}", e);
                Ok(ast.clone())
            }
        }
    }
}
