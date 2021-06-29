use crate::compiler::*;
use crate::vval::*;
use crate::nvec::NVec;
use crate::ops::*;

use std::rc::Rc;
use std::cell::RefCell;

const DEBUG_VM: bool = false;

macro_rules! in_reg {
    ($env: ident, $ret: ident, $data: ident, $respos_var: ident) => {
        let $respos_var =
            match $respos_var {
                ResPos::Local(o)    => $env.get_local(*o as usize),
                ResPos::LocalRef(o) => $env.get_local(*o as usize).deref(),
                ResPos::Up(i)       => $env.get_up(*i as usize),
                ResPos::UpRef(i)    => $env.get_up(*i as usize).deref(),
                ResPos::Global(i)   => $data[*i as usize].deref(),
                ResPos::GlobalRef(i)=> $data[*i as usize].deref().deref(),
                ResPos::Arg(o)      => $env.arg(*o as usize),
                ResPos::Data(i)     => $data[*i as usize].clone(),
                ResPos::Stack(_o)   => $env.pop(),
                ResPos::Value(ResValue::Ret)      => std::mem::replace(&mut $ret, VVal::None),
                ResPos::Value(ResValue::None)     => VVal::None,
                ResPos::Value(ResValue::OptNone)  => VVal::Opt(None),
                ResPos::Value(ResValue::SelfObj)  => $env.self_object(),
                ResPos::Value(ResValue::SelfData) => $env.self_object().proto_data(),
                ResPos::Value(ResValue::AccumVal) => $env.get_accum_value(),
                ResPos::Value(ResValue::AccumFun) => $env.get_accum_function(),
            };
    }
}

macro_rules! out_reg {
    ($env: ident, $ret: ident, $retv: ident, $data: ident, $respos_var: ident, $val: expr) => {
        match $respos_var {
            ResPos::Local(o)        => $env.set_consume(*o as usize, $val),
            ResPos::LocalRef(o)     => $env.assign_ref_local(*o as usize, $val),
            ResPos::Up(i)           => $env.set_up(*i as usize, $val),
            ResPos::UpRef(i)        => $env.assign_ref_up(*i as usize, $val),
            ResPos::Global(i)       => { $data[*i as usize].set_ref($val); },
            ResPos::GlobalRef(i)    => { $data[*i as usize].deref().set_ref($val); },
            ResPos::Data(i)         => { $data[*i as usize].set_ref($val); },
            ResPos::Stack(_)        => { $env.push($val); },
            ResPos::Value(ResValue::Ret) => { $ret = $val; },
            ResPos::Arg(_)          => (),
            ResPos::Value(ResValue::None) => {
                if let VVal::Err(ev) = $val {
                    $retv =
                        Err(StackAction::panic_str(
                            format!("Dropped error value: {}", ev.borrow().0.s()),
                            Some(ev.borrow().1.clone())));
                    break;
                }
            },
            ResPos::Value(_)        => (),
        };
    }
}

macro_rules! op_r {
    ($env: ident, $ret: ident, $retv: ident, $data: ident, $r: ident, $block: block) => {
        {
            let res = $block;
            out_reg!($env, $ret, $retv, $data, $r, res);
        }
    }
}

macro_rules! op_a_r {
    ($env: ident, $ret: ident, $retv: ident, $data: ident, $a: ident, $r: ident, $block: block) => {
        {
            in_reg!($env, $ret, $data, $a);
            let res = $block;
            out_reg!($env, $ret, $retv, $data, $r, res);
        }
    }
}

macro_rules! op_a_b_r {
    ($env: ident, $ret: ident, $retv: ident, $data: ident, $a: ident, $b: ident, $r: ident, $block: block) => {
        {
            in_reg!($env, $ret, $data, $a);
            in_reg!($env, $ret, $data, $b);
            let res = $block;
            out_reg!($env, $ret, $retv, $data, $r, res);
        }
    }
}

macro_rules! op_a_b_c_r {
    ($env: ident, $ret: ident, $retv: ident, $data: ident, $a: ident, $b: ident, $c: ident, $r: ident, $block: block) => {
        {
            in_reg!($env, $ret, $data, $a);
            in_reg!($env, $ret, $data, $b);
            in_reg!($env, $ret, $data, $c);
            let res = $block;
            out_reg!($env, $ret, $retv, $data, $r, res);
        }
    }
}

macro_rules! handle_err {
    ($v: ident, $msg: expr, $retv: ident) => {
        {
            match $v {
                VVal::Err(ev) => {
                    $retv =
                        Err(StackAction::panic_str(
                            format!("Error value in {}: {}",
                                    $msg, ev.borrow().0.s()),
                            Some(ev.borrow().1.clone())));
                    break;
                },
                v => v,
            }
        }
    }
}

macro_rules! handle_next {
    ($env: ident, $pc: ident, $uw_depth: ident, $retv: ident) => {
        if $env.loop_info.break_pc == 0 {
            $env.unwind_to_depth($uw_depth);
            $retv = Err(StackAction::Next);
            break;
        } else {
            $pc = $env.loop_info.pc;
            $env.cleanup_loop();
        }
    }
}

macro_rules! handle_break {
    ($env: ident, $pc: ident, $val: expr, $uw_depth: ident, $retv: ident) => {
        if $env.loop_info.break_pc == 0 {
            $env.unwind_to_depth($uw_depth);
            $retv = Err(StackAction::Break($val));
            break;
        } else {
            $pc = $env.loop_info.break_pc;
            $env.cleanup_loop();
        }
    }
}

macro_rules! call_ud_method {
    ($ud: ident, $key: ident, $argc: ident, $env: ident, $retv: ident, $uw_depth: ident, $prog: ident, $pc: ident, $call_ret: ident, $cont: block) => {
        {
            let call_ret =
                $env.with_local_call_info(
                    $argc, |env| $ud.call_method($key, env));
            $env.popn($argc);
            match call_ret {
                Ok($call_ret) => $cont,
                Err(StackAction::Return(ret)) => {
                    $env.unwind_to_depth($uw_depth);
                    $retv = Err(StackAction::Return(ret));
                    break;
                },
                Err(StackAction::Next) => {
                    handle_next!($env, $pc, $uw_depth, $retv);
                },
                Err(StackAction::Break(v)) => {
                    handle_break!($env, $pc, v, $uw_depth, $retv);
                },
                Err(sa) => {
                    $env.unwind_to_depth($uw_depth);
                    $retv = Err(sa.wrap_panic($prog.debug[$pc].clone()));
                    break;
                },
            }
        }
    }
}


macro_rules! call_func {
    ($f: ident, $argc: ident, $popc: expr, $env: ident, $retv: ident, $uw_depth: ident, $prog: ident, $pc: ident, $call_ret: ident, $cont: block) => {
        {
            let call_ret = $f.call_internal($env, $argc);
            $env.popn($popc); // + 1 for the function
            match call_ret {
                Ok($call_ret) => $cont,
                Err(StackAction::Return(ret)) => {
                    $env.unwind_to_depth($uw_depth);
                    $retv = Err(StackAction::Return(ret));
                    break;
                },
                Err(StackAction::Next) => {
                    handle_next!($env, $pc, $uw_depth, $retv);
                },
                Err(StackAction::Break(v)) => {
                    handle_break!($env, $pc, v, $uw_depth, $retv);
                },
                Err(sa) => {
                    $env.unwind_to_depth($uw_depth);
                    $retv = Err(sa.wrap_panic($prog.debug[$pc].clone()));
                    break;
                },
            }
        }
    }
}

macro_rules! get_key {
    ($o: ident, $k: ident, $method: ident, $env: ident, $retv: ident, $uw_depth: ident, $prog: ident, $pc: ident) => {
        match $k {
            VVal::Int(i)  => $o.at(i as usize).unwrap_or_else(|| VVal::None),
            VVal::Bol(b)  => $o.at(b as usize).unwrap_or_else(|| VVal::None),
            VVal::Sym(sy) => $o.$method(sy.as_ref()).unwrap_or_else(|| VVal::None),
            VVal::Str(sy) => $o.$method(sy.as_ref()).unwrap_or_else(|| VVal::None),
            _ => {
                $env.push($o.clone());
                let call_ret = $k.call_internal($env, 1);
                $env.pop();
                match call_ret {
                    Ok(v) => v,
                    Err(sa) => {
                        $env.unwind_to_depth($uw_depth);
                        $retv =
                            Err(sa.wrap_panic($prog.debug[$pc].clone()));
                        break;
                    },
                }
            }
        }
    }
}

#[allow(clippy::many_single_char_names)]
#[allow(clippy::cognitive_complexity)]
pub fn vm(prog: &Prog, env: &mut Env) -> Result<VVal, StackAction> {
    env.vm_nest += 1;

    let old_sp = env.sp;
    let mut pc : usize = 0;

    if DEBUG_VM {
        println!("# EXEC PROG:###################################");
        prog.dump();
        println!("-- START {:>3} -------------------------------",
                 env.vm_nest);
    }

    let mut retv : Result<(), StackAction> = Ok(());
    let mut ret = VVal::None;

    let uw_depth = env.unwind_depth();
    let old_loop_info =
        std::mem::replace(&mut env.loop_info, LoopInfo::new());

    let data = &prog.data;

    loop {
        let op = &prog.ops[pc];
        if DEBUG_VM {
            let syn =
                if let Some(sp) = &prog.debug[pc] { sp.s_short() }
                else { "".to_string() };
            println!("OP[{:<2} {:>3}]: {:<40}      | sp: {:>3}, bp: {:>3}, uws: {:>3} | {}",
                     env.vm_nest, pc, format!("{:?}", op), env.sp, env.bp, env.unwind_depth(), syn);
        }

        match op {
            Op::Mov(a, r) => op_a_r!(env, ret, retv, data, a, r, { a }),
            Op::NewPair(a, b, r) => op_a_b_r!(env, ret, retv, data, a, b, r, {
                VVal::pair(
                    handle_err!(b, "first pair element", retv),
                    handle_err!(a, "second pair element", retv))
            }),
            Op::NewOpt(a, r) => op_r!(env, ret, retv, data, r, {
                if let ResPos::Value(ResValue::OptNone) = a {
                    VVal::Opt(None)
                } else {
                    in_reg!(env, ret, data, a);
                    VVal::Opt(Some(Rc::new(a)))
                }
            }),
            Op::NewIter(a, r) => op_a_r!(env, ret, retv, data, a, r, {
                VVal::Iter(Rc::new(RefCell::new(a.iter())))
            }),
            Op::NewNVec(vp, r) => {
                match vp.as_ref() {
                    NVecPos::IVec2(a, b) => {
                        in_reg!(env, ret, data, b);
                        in_reg!(env, ret, data, a);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        out_reg!(env, ret, retv, data, r,
                            VVal::IVec(Box::new(NVec::Vec2(a.i(), b.i()))));
                    },
                    NVecPos::IVec3(a, b, c) => {
                        in_reg!(env, ret, data, c);
                        in_reg!(env, ret, data, b);
                        in_reg!(env, ret, data, a);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        out_reg!(env, ret, retv, data, r,
                            VVal::IVec(Box::new(NVec::Vec3(a.i(), b.i(), c.i()))));
                    },
                    NVecPos::IVec4(a, b, c, d) => {
                        in_reg!(env, ret, data, d);
                        in_reg!(env, ret, data, c);
                        in_reg!(env, ret, data, b);
                        in_reg!(env, ret, data, a);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        let d = handle_err!(d, "w nvector component", retv);
                        out_reg!(env, ret, retv, data, r,
                            VVal::IVec(Box::new(NVec::Vec4(a.i(), b.i(), c.i(), d.i()))));
                    },
                    NVecPos::FVec2(a, b) => {
                        in_reg!(env, ret, data, b);
                        in_reg!(env, ret, data, a);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        out_reg!(env, ret, retv, data, r,
                            VVal::FVec(Box::new(NVec::Vec2(a.f(), b.f()))));
                    },
                    NVecPos::FVec3(a, b, c) => {
                        in_reg!(env, ret, data, c);
                        in_reg!(env, ret, data, b);
                        in_reg!(env, ret, data, a);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        out_reg!(env, ret, retv, data, r,
                            VVal::FVec(Box::new(NVec::Vec3(a.f(), b.f(), c.f()))));
                    },
                    NVecPos::FVec4(a, b, c, d) => {
                        in_reg!(env, ret, data, d);
                        in_reg!(env, ret, data, c);
                        in_reg!(env, ret, data, b);
                        in_reg!(env, ret, data, a);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        let d = handle_err!(d, "w nvector component", retv);
                        out_reg!(env, ret, retv, data, r,
                            VVal::FVec(Box::new(NVec::Vec4(a.f(), b.f(), c.f(), d.f()))));
                    },
                }
            },
            Op::ToRef(a, r, trtype) => {
                match trtype {
                    ToRefType::CaptureRef =>
                        match a {
                            ResPos::Local(i) => out_reg!(env, ret, retv, data, r, {
                                env.get_local_captured_ref(*i as usize)
                            }),
                            ResPos::Global(i) => out_reg!(env, ret, retv, data, r, {
                                prog.data[*i as usize].clone()
                            }),
                            ResPos::Up(i) => out_reg!(env, ret, retv, data, r, {
                                env.get_up_captured_ref(*i as usize)
                            }),
                            _ => op_a_r!(env, ret, retv, data, a, r, { a.to_ref() }),
                        },
                    ToRefType::ToRef =>
                        op_a_r!(env, ret, retv, data, a, r, { a.to_ref() }),
                    ToRefType::Hidden =>
                        op_a_r!(env, ret, retv, data, a, r, {
                            a.to_hidden_boxed_ref()
                        }),
                    ToRefType::Weak =>
                        op_a_r!(env, ret, retv, data, a, r, { a.downgrade() }),
                    ToRefType::Deref =>
                        op_a_r!(env, ret, retv, data, a, r, {
                            a.deref()
                        }),
                }
            },
            Op::Argv(r)             => op_r!(env, ret, retv, data, r, { env.argv() }),
            Op::End                 => { break; },
            Op::Unwind              => { env.unwind_one(); },
            Op::IterInit(iterable, body_ops) => {
                in_reg!(env, ret, data, iterable);

                // 1 unwind offset for push_iter, which needs to be preserved
                // on break/next, as it is explicitly cleaned up.
                env.push_loop_info(pc, pc + *body_ops as usize, 1);

                if let VVal::Iter(i) = iterable {
                    env.push_iter(i);
                } else {
                    env.push_iter(Rc::new(RefCell::new(iterable.iter())));
                }
            },
            Op::IterNext(ivar) => {
                let i = env.iter.as_ref().unwrap();
                let value =
                    if let Some((v, k)) = i.borrow_mut().next() {
                        if let Some(k) = k { VVal::pair(v, k) }
                        else               { v }
                    } else {
                        pc = env.loop_info.break_pc;
                        VVal::None
                    };

                out_reg!(env, ret, retv, data, ivar, value);
            },
            Op::PushLoopInfo(body_ops) => {
                env.push_loop_info(pc, pc + *body_ops as usize, 0);
            },
            Op::ClearLocals(from, to) => {
                env.push_clear_locals(*from as usize, *to as usize);
            },
            Op::Accumulator(typ) => {
                let v =
                    match typ {
                        AccumType::String => VVal::new_str(""),
                        AccumType::Bytes  => VVal::new_byt(vec![]),
                        AccumType::Float  => VVal::Flt(0.0),
                        AccumType::Int    => VVal::Int(0),
                        AccumType::Map    => VVal::map(),
                        AccumType::Vec    => VVal::vec(),
                    };
                env.setup_accumulator(v);
            },
            Op::Add(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Int(a) = a {
                    VVal::Int(a.wrapping_add(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f + b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(Box::new(*ln + re.nvec())),
                        (VVal::FVec(ln), re) => VVal::FVec(Box::new(*ln + re.nvec())),
                        (le, re)             => VVal::Int(le.i().wrapping_add(re.i()))
                    }
                }
            }),
            Op::Sub(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Int(a) = a {
                    VVal::Int(a.wrapping_sub(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f - b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(Box::new(*ln - re.nvec())),
                        (VVal::FVec(ln), re) => VVal::FVec(Box::new(*ln - re.nvec())),
                        (le, re)             => VVal::Int(le.i().wrapping_sub(re.i()))
                    }
                }
            }),
            Op::Div(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Int(a) = a {
                    if b.i() == 0 {
                        env.unwind_to_depth(uw_depth);
                        retv =
                            Err(StackAction::panic_str(
                                format!("Division by 0: {}/{}", a, b.i()),
                                prog.debug[pc].clone()));
                        break;
                    }

                    VVal::Int(a.wrapping_div(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f / b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(Box::new(*ln / re.i())),
                        (VVal::FVec(ln), re) => VVal::FVec(Box::new(*ln / re.f())),
                        (le, re)             => {
                            let re = re.i();
                            if re == 0 {
                                env.unwind_to_depth(uw_depth);
                                retv =
                                    Err(StackAction::panic_str(
                                        format!("Division by 0: {}/{}", le.i(), re),
                                        prog.debug[pc].clone()));
                                break;
                            }
                            VVal::Int(le.i().wrapping_div(re))
                        },
                    }
                }
            }),
            Op::Mul(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Int(a) = a {
                    VVal::Int(a.wrapping_mul(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f * b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(Box::new(*ln * re.i())),
                        (VVal::FVec(ln), re) => VVal::FVec(Box::new(*ln * re.f())),
                        (le, re)             => VVal::Int(le.i().wrapping_mul(re.i()))
                    }
                }
            }),
            Op::Mod(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Flt(f) = a {
                    VVal::Flt(f % b.f())
                } else {
                    if b.i() == 0 {
                        env.unwind_to_depth(uw_depth);
                        retv =
                            Err(StackAction::panic_str(
                                format!("Remainder with divisor of 0: {}%{}", a.i(), b.i()),
                                prog.debug[pc].clone()));
                        break;
                    }

                    VVal::Int(a.i().wrapping_rem(b.i()))
                }
            }),
            Op::Le(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f <= b.f()) }
                else { VVal::Bol(a.i() <= b.i()) }
            }),
            Op::Lt(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f < b.f()) }
                else { VVal::Bol(a.i() < b.i()) }
            }),
            Op::Ge(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f >= b.f()) }
                else { VVal::Bol(a.i() >= b.i()) }
            }),
            Op::Gt(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f > b.f()) }
                else { VVal::Bol(a.i() > b.i()) }
            }),
            Op::Eq(b, a, r) => op_a_b_r!(env, ret, retv, data, b, a, r, {
                VVal::Bol(a.eqv(&b))
            }),
            Op::NewList(r) => op_r!(env, ret, retv, data, r, { VVal::vec() }),
            Op::ListPush(a, b, r) => op_a_b_r!(env, ret, retv, data, a, b, r, {
                b.push(handle_err!(a, "list element", retv));
                b
            }),
            Op::ListSplice(a, b, r) => op_a_b_r!(env, ret, retv, data, a, b, r, {
                if let VVal::Iter(i) = a {
                    let mut i = i.borrow_mut();
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some((v, _)) = i.next() {
                        b.push(v);
                    }
                } else {
                    for (e, _) in a.iter() {
                        b.push(e);
                    }
                }
                b
            }),
            Op::NewMap(r) => op_r!(env, ret, retv, data, r, { VVal::map() }),
            Op::MapSetKey(v, k, m, r) => op_a_b_c_r!(env, ret, retv, data, v, k, m, r, {
                let v = handle_err!(v, "map value", retv);
                let k = handle_err!(k, "map key", retv);
                m.set_key(&k, v)?;
                m
            }),
            Op::GetIdx(o, idx, r) => op_a_r!(env, ret, retv, data, o, r, {
                handle_err!(o, "map/list", retv)
                .at(*idx as usize).unwrap_or_else(|| VVal::None)
            }),
            Op::GetIdx2(o, idx, r) => op_a_r!(env, ret, retv, data, o, r, {
                handle_err!(o, "map/list", retv)
                .at(idx.0 as usize).unwrap_or_else(|| VVal::None)
                .at(idx.1 as usize).unwrap_or_else(|| VVal::None)
            }),
            Op::GetIdx3(o, idx, r) => op_a_r!(env, ret, retv, data, o, r, {
                handle_err!(o, "map/list", retv)
                .at(idx.0 as usize).unwrap_or_else(|| VVal::None)
                .at(idx.1 as usize).unwrap_or_else(|| VVal::None)
                .at(idx.2 as usize).unwrap_or_else(|| VVal::None)
            }),
            Op::GetSym(o, sym, r) => op_a_r!(env, ret, retv, data, o, r, {
                handle_err!(o, "map/list", retv)
                .get_key_sym(&sym).unwrap_or_else(|| VVal::None)
            }),
            Op::GetSym2(o, sym, r) => op_a_r!(env, ret, retv, data, o, r, {
                handle_err!(o, "map/list", retv)
                .get_key_sym(&sym.0).unwrap_or_else(|| VVal::None)
                .get_key_sym(&sym.1).unwrap_or_else(|| VVal::None)
            }),
            Op::GetSym3(o, sym, r) => op_a_r!(env, ret, retv, data, o, r, {
                handle_err!(o, "map/list", retv)
                .get_key_sym(&sym.0).unwrap_or_else(|| VVal::None)
                .get_key_sym(&sym.1).unwrap_or_else(|| VVal::None)
                .get_key_sym(&sym.2).unwrap_or_else(|| VVal::None)
            }),
            Op::GetKey(o, k, r) => {
                in_reg!(env, ret, data, k);
                in_reg!(env, ret, data, o);

                let o = handle_err!(o, "field idx/key", retv);
                let k = handle_err!(k, "map/list", retv);
                let res = get_key!(o, k, get_key, env, retv, uw_depth, prog, pc);
                out_reg!(env, ret, retv, data, r, res);
            },
            Op::Destr(a, info) => {
                in_reg!(env, ret, data, a);
                info.destructure(env, a);
            },
            Op::NewErr(e, r) => op_a_r!(env, ret, retv, data, e, r, {
                VVal::err(e, prog.debug[pc].clone().unwrap())
            }),
            Op::NewClos(f, r) => op_a_r!(env, ret, retv, data, f, r, {
                f.clone_and_rebind_upvalues(|upvs, upvalues| {
                    copy_upvs(upvs, env, upvalues);
                })
            }),
            Op::MapSplice(s, m, r) => op_a_b_r!(env, ret, retv, data, s, m, r, {
                if let VVal::Iter(i) = s {
                    let mut i = i.borrow_mut();
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some((e, k)) = i.next() {
                        if let Some(k) = k {
                            match m.set_key(&k, e) {
                                Ok(_) => (),
                                Err(e) => {
                                    retv =
                                        Err(StackAction::panic_str(
                                            format!("map set key errro: {}", e),
                                            prog.debug[pc].clone()));
                                }
                            }
                        }
                    }
                } else {
                    for (e, k) in s.iter() {
                        match m.set_key(&k.unwrap_or_else(|| VVal::None), e) {
                            Ok(_) => (),
                            Err(e) => {
                                retv =
                                    Err(StackAction::panic_str(
                                        format!("map set key errro: {}", e),
                                        prog.debug[pc].clone()));
                            }
                        }
                    }
                }

                if retv.is_err() {
                    break;
                }

                m
            }),
            Op::CallMethodKey(o, k, argc, r) => {
                in_reg!(env, ret, data, k);
                in_reg!(env, ret, data, o);
                let o = handle_err!(o, "field idx/key", retv);
                let k = handle_err!(k, "map/list", retv);
                let argc = *argc as usize;

                if let VVal::Usr(u) = o {
                    let k = k.s_raw();
                    let ks = &k;
                    call_ud_method!(u, ks, argc, env, retv, uw_depth, prog, pc, v, {
                        out_reg!(env, ret, retv, data, r, v);
                    })

                } else {
                    let f = get_key!(o, k, proto_lookup, env, retv, uw_depth, prog, pc);
                    env.push_unwind_self(o);
                    call_func!(f, argc, argc, env, retv, uw_depth, prog, pc, v, {
                        env.unwind_one();
                        out_reg!(env, ret, retv, data, r, v);
                    });
                }
            },
            Op::CallMethodSym(o, k_argc, r) => {
                in_reg!(env, ret, data, o);
                let o = handle_err!(o, "field idx/key", retv);
                let k    = &k_argc.0;
                let argc = k_argc.1 as usize;

                if let VVal::Usr(u) = o {
                    let k = &*k;
                    call_ud_method!(u, k, argc, env, retv, uw_depth, prog, pc, v, {
                        out_reg!(env, ret, retv, data, r, v);
                    })

                } else {
                    let f = o.proto_lookup(&*k).unwrap_or_else(|| VVal::None);
                    env.push_unwind_self(o);
                    call_func!(f, argc, argc, env, retv, uw_depth, prog, pc, v, {
                        env.unwind_one();
                        out_reg!(env, ret, retv, data, r, v);
                    });
                }

            },
            Op::Call(argc, r) => {
                let argc = *argc as usize;
                let f = env.stk(argc + 1).clone();

                call_func!(f, argc, argc + 1, env, retv, uw_depth, prog, pc, v, {
                    out_reg!(env, ret, retv, data, r, v);
                });
            },
            Op::CallDirect(fun) => {
                let a = &fun.arg;
                let r = &fun.res;
                op_a_r!(env, ret, retv, data, a, r, {
                    (fun.fun)(a, env)
                })
            },
            Op::Apply(argv, func, r) => {
                in_reg!(env, ret, data, argv);
                in_reg!(env, ret, data, func);

                let mut argv = argv;

                let argc =
                    if let VVal::Lst(l) = &argv {
                        l.borrow().len()
                    } else {
                        let a = VVal::vec();
                        a.push(argv);
                        argv = a;
                        1
                    };

                for i in 0..argc {
                    let v = argv.at(i).unwrap_or_else(|| VVal::None);
                    env.push(v);
                }

                let call_ret = func.call_internal(env, argc);
                env.popn(argc);

                match call_ret {
                    Ok(v) => { out_reg!(env, ret, retv, data, r, v); },
                    Err(StackAction::Return(ret)) => {
                        env.unwind_to_depth(uw_depth);
                        retv = Err(StackAction::Return(ret));
                        break;
                    },
                    Err(sa) => {
                        env.unwind_to_depth(uw_depth);
                        retv = Err(sa.wrap_panic(prog.debug[pc].clone()));
                        break;
                    },
                }
            },
            Op::Jmp(jmp_offs) => {
                pc = (pc as i32 + *jmp_offs) as usize;
            },
            Op::JmpIfN(a, jmp_offs) => {
                in_reg!(env, ret, data, a);
                if !a.b() { pc = (pc as i32 + *jmp_offs) as usize; }
            },
            Op::OrJmp(a, jmp_offs, r) => {
                in_reg!(env, ret, data, a);
                if a.b() {
                    pc = (pc as i32 + *jmp_offs) as usize;
                    out_reg!(env, ret, retv, data, r, a);
                }
            },
            Op::AndJmp(a, jmp_offs, r) => {
                in_reg!(env, ret, data, a);
                if !a.b() {
                    pc = (pc as i32 + *jmp_offs) as usize;
                    out_reg!(env, ret, retv, data, r, a);
                }
            },
            Op::JmpTbl(a, tbl) => {
                in_reg!(env, ret, data, a);
                let i = a.i();
                let idx : usize =
                    if i >= tbl.len() as i64 || i < 0 {
                        tbl.len() - 1
                    } else {
                        i as usize
                    };
                pc = (pc as i32 + tbl[idx]) as usize;
            },
            Op::CtrlFlow(flw) => {
                match flw {
                    CtrlFlow::Next => {
                        handle_next!(env, pc, uw_depth, retv);
                    },
                    CtrlFlow::Break(a) => {
                        in_reg!(env, ret, data, a);
                        let a = Box::new(a);
                        handle_break!(env, pc, a, uw_depth, retv);
                    },
                }
            },
            Op::Builtin(b) => {
                match b {
                    Builtin::DumpStack(spos) => {
                        println!("DUMPSTACK@{}", spos);
                        env.dump_stack();
                    },
                    Builtin::DumpVM(spos) => {
                        println!("DUMPPROG@{}", spos);
                        for (i, op) in prog.ops.iter().enumerate() {
                            let syn =
                                if let Some(sp) = &prog.debug[i] { sp.s_short() }
                                else { "".to_string() };
                            println!("{}OP[{:<2} {:>3}]: {:<40}      | sp: {:>3}, bp: {:>3}, uws: {:>3} | {}",
                                     (if i == pc { ">" } else { " " }),
                                     env.vm_nest,
                                     i, format!("{:?}", op),
                                     env.sp, env.bp, env.unwind_depth(), syn);
                            if i == pc {
                                env.dump_stack();
                            }
                        }
                    },
                    Builtin::Export(name, a) => {
                        in_reg!(env, ret, data, a);
                        env.export_name(name, &a);
                    },
                }
            },
        }
        if DEBUG_VM {
            let uws_dump = env.dump_unwind_stack();
            println!("    => uws: {}", uws_dump);
            println!("  loopinfo: uw={},sp={},pc={},bpc={}",
                     env.loop_info.uw_depth,
                     env.loop_info.sp,
                     env.loop_info.pc,
                     env.loop_info.break_pc);
            env.dump_stack();
            println!();
        }
        pc += 1;
    }

    if env.sp > old_sp {
        if DEBUG_VM {
            println!("-- Excess stuff on stack: {}", env.sp - old_sp);
        }
        env.popn(env.sp - old_sp);
    }

    if DEBUG_VM {
        println!("-- END {:>3} -------------------------------",
                 env.vm_nest);
//        prog.dump();
//        println!("# EXEC END PROG:###################################");
    }

    env.unwind_to_depth(uw_depth);
    env.loop_info = old_loop_info;


    env.vm_nest -= 1;

    match retv {
        Ok(()) => Ok(ret),
        Err(e) => Err(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn gen(s: &str) -> String {
        let global = GlobalEnv::new_default();
        match crate::parser::parse(s, "<compiler:s_eval>") {
            Ok(ast) => {
                let mut ce = CompileEnv::new(global.clone());

                match crate::compiler::compile(&ast, &mut ce) {
                    Ok(prog) => {
                        let local_space = ce.borrow().get_local_space();

                        let mut p = Prog::new();
                        prog.eval_to(&mut p, ResPos::Value(ResValue::Ret));
                        p.op_end();

                        let mut e = Env::new(global);
                        e.push(VVal::Flt(14.4));
                        e.push(VVal::Int(10));
                        e.argc = 2;
                        e.set_bp(0);
                        e.push_sp(local_space);

                        match vm(&p, &mut e) {
                            Ok(v) => v.s(),
                            Err(je) => {
                                format!("EXEC ERR: Caught {:?}", je)
                            }
                        }
                    },
                    Err(re) => format!("COMPILE ERROR: {}", re),
                }
            }
            Err(e)  => format!("PARSE ERROR: {}", e),
        }
    }


    #[test]
    fn vm_data() {
        assert_eq!(gen("10"),      "10");
        assert_eq!(gen("\"foo\""), "\"foo\"");
        assert_eq!(gen(":foo"),    ":foo");
    }

    #[test]
    fn vm_pair() {
        assert_eq!(gen("$p(1,2)"),          "$p(1,2)");
        assert_eq!(gen("$p(1,$p(2,3))"),    "$p(1,$p(2,3))");
        assert_eq!(gen("$p($p(1,2),$p($p(3,4),3))"),
                   "$p($p(1,2),$p($p(3,4),3))");
    }

    #[test]
    fn vm_vars() {
        assert_eq!(gen("!:global x = 10; x"),                         "10");
        assert_eq!(gen("!:global x = 10; !:global y = 11; $p(x, y)"), "$p(10,11)");
        assert_eq!(gen("!x = 10; x; 13"),                             "13");
        assert_eq!(gen("!x = 10; !y = 11; $p(x, y)"),                 "$p(10,11)");
        assert_eq!(gen("!x = 10; !y = 11; x; y; $p(x, y)"),           "$p(10,11)");
        assert_eq!(gen("!x = 10; !y = 20; $p($p(1,2),$p(3,4))"),         "$p($p(1,2),$p(3,4))");
        assert_eq!(gen("!x = 10; !y = 20; !z = $p($p(1,2),$p(3,4)); z"), "$p($p(1,2),$p(3,4))");
        assert_eq!(gen("!x = 10; !y = 11; $:x"),                      "$&&10");
        assert_eq!(gen("!x = 11; $:x"),                               "$&&11");
        assert_eq!(gen("!:global x = 12; $:x"),                       "$&&12");
        assert_eq!(gen("!:global x = 10; !y = 11; $:x"),              "$&&10");
        assert_eq!(gen("!:global x = 10; !y = 11; _"),                "14.4");
        assert_eq!(gen("!:global x = 10; !y = 11; $:_"),              "$&&14.4");
        assert_eq!(gen("!:global x = 10; !y = 11; $p(_, $p(_1, $p(_2, y)))"),
                   "$p(14.4,$p(10,$p($n,11)))");
    }

    #[test]
    fn vm_call() {
        assert_eq!(gen("1 + 2"),         "3");
        assert_eq!(gen("1 + 2 + 3 + 4"), "10");
        assert_eq!(gen("1 > 2"),         "$false");
        assert_eq!(gen("2 > 1"),         "$true");
        assert_eq!(gen("1 < 2"),         "$true");
        assert_eq!(gen("2 < 1"),         "$false");
        assert_eq!(gen("1 >= 2"),        "$false");
        assert_eq!(gen("2 >= 1"),        "$true");
        assert_eq!(gen("1 <= 2"),        "$true");
        assert_eq!(gen("2 <= 1"),        "$false");

        assert_eq!(gen("10 < 20"),     "$true");
        assert_eq!(gen("11 < 10"),     "$false");
        assert_eq!(gen("10 < 10"),     "$false");
        assert_eq!(gen("10 > 20"),     "$false");
        assert_eq!(gen("11 > 10"),     "$true");
        assert_eq!(gen("10 > 10"),     "$false");
        assert_eq!(gen("10 <= 20"),    "$true");
        assert_eq!(gen("11 <= 10"),    "$false");
        assert_eq!(gen("10 <= 10"),    "$true");
        assert_eq!(gen("10 >= 20"),    "$false");
        assert_eq!(gen("11 >= 10"),    "$true");
        assert_eq!(gen("10 >= 10"),    "$true");
        assert_eq!(gen("10.1 < 20.4"), "$true");
        assert_eq!(gen("11.2 < 10.2"), "$false");
        assert_eq!(gen("10.3 < 10.4"), "$true");
        assert_eq!(gen("22 == 22"),    "$true");
        assert_eq!(gen("22 == 23"),    "$false");
        assert_eq!(gen("22 != 22"),    "$false");
        assert_eq!(gen("21 != 22"),    "$true");
    }

    #[test]
    fn vm_lists() {
        assert_eq!(gen("$[]"),              "$[]");
        assert_eq!(gen("$[1, 2, 4]"),       "$[1,2,4]");
        assert_eq!(gen("$[1, $[3,5], 4]"),  "$[1,$[3,5],4]");
        assert_eq!(gen("$[1, *$[3,5], 4]"),  "$[1,3,5,4]");
        assert_eq!(gen("$[1, *${a=30}, 4]"),  "$[1,30,4]");
    }

    #[test]
    fn vm_maps() {
        assert_eq!(gen("${}"),              "${}");
        assert_eq!(gen("${a=$n}"),          "${a=$n}");
        assert_eq!(gen("${a=$p(1,2)}"),     "${a=$p(1,2)}");
        assert_eq!(gen("${*${b=10}}"),      "${b=10}");
    }

    #[test]
    fn vm_func() {
        assert_eq!(gen("!x = 10; { x; 20 }"), "&F{@[1,10:<compiler:s_eval>(Func)@x],amin=0,amax=0,locals=0,upvalues=$[$&10]}");
        assert_eq!(gen("!x = 10; { x }[]"), "10");
        assert_eq!(gen("!x = 10; { !y = 4; !k = 5; y + k + x }[]"), "19");
        assert_eq!(gen(r"
            !x = 10;
            !huh = { x };
            !j = { !y = 4; !k = 5; y + k + huh[] }[];
            !u = j[];
            u
        "), "19");
    }

    #[test]
    fn vm_assign() {
        assert_eq!(gen("!x = 10; .x = 11; x"),                "11");
        assert_eq!(gen("!x = 10; { .x = 11; }[]; x"),         "11");
        assert_eq!(gen("!:global x = 10; .x = 11; x"),        "11");
        assert_eq!(gen("!:global x = 10; { .x = 11; }[]; x"), "11");
        assert_eq!(gen(r"
            !x = 0;
            range 1 1000 1 {||
                .x = x + 1;
            };
            x"), "1000");
    }

    #[test]
    fn vm_while() {
        assert_eq!(gen(r"
            !:global x = 0;
            !inc = { .x = x + 1 };
            while { x < 1000 } inc[];
            x
        "), "1000");
        assert_eq!(gen(r"
            !x = 0;
            while { x < 2 } {
                .x = x + 1;
                !k = 1;
                .k = k + 1;
            };
            x
        "), "2");
        assert_eq!(gen(r"
            !x = 0;
            while { x < 2 } {
                .x = x + 1;
                !k = 1;
            };
            x
        "), "2");
        assert_eq!(gen(r"
            !x = 0;
            while { x < 2 } { .x = x + 1 };
            x
        "), "2");
    }

    #[test]
    fn vm_check_bytes_impl() {
        #[cfg(feature="serde_json")]
        assert_eq!(gen("std:ser:json $b\"abc\""),                         "\"[\\n  97,\\n  98,\\n  99\\n]\"", "JSON serializer for bytes ok");

        assert_eq!(gen("str $b\"abc\""),                              "\"abc\"", "Bytes to String by 1:1 Byte to Unicode Char mapping");
        assert_eq!(gen("str $b\"äbcß\""),                             "\"Ã¤bcÃ\\u{9f}\"", "Bytes to String by 1:1 Byte to Unicode Char mapping");
        assert_eq!(gen("std:str:from_utf8 $b\"äbcß\""),                   "\"äbcß\"", "Bytes to String from UTF8");
        assert_eq!(gen("std:str:from_utf8 $b\"\\xC4\\xC3\""),             "$e \"str:from_utf8 decoding error: invalid utf-8 sequence of 1 bytes from index 0\"", "Bytes to String from invalid UTF8");
        assert_eq!(gen("std:str:from_utf8_lossy $b\"\\xC4\\xC3\""),       "\"��\"", "Bytes to String from invalid UTF8 lossy");
        assert_eq!(gen("std:str:to_bytes \"aäß\""),                       "$b\"a\\xC3\\xA4\\xC3\\x9F\"", "Bytes from String as UTF8");
        assert_eq!(gen("std:str:from_utf8 ~ std:str:to_bytes \"aäß\""),       "\"aäß\"", "Bytes from String as UTF8 into String again");
        assert_eq!(gen("$b\"abc\" 1"),                                "$b'b'", "Get single byte from bytes");
        assert_eq!(gen("$b\"abcdef\" 0 2"),                           "$b\"ab\"", "Substring bytes operation");
        assert_eq!(gen("$b\"abcdef\" 3 3"),                           "$b\"def\"", "Substring bytes operation");
        assert_eq!(gen("$b\"abcdef\" $[3, 3]"),                       "$b\"def\"", "Substring bytes operation");
        assert_eq!(gen("$b\"abcdef\" $[3]"),                          "$b\"def\"", "Substring bytes operation");
        assert_eq!(gen("$b\"abcdef\" ${abcdef = 10}"),                "10", "Bytes as map key");
        assert_eq!(gen("std:bytes:to_vec $b\"abcdef\""),                  "$[97,98,99,100,101,102]", "bytes:to_vec");
        assert_eq!(gen("std:bytes:from_vec ~ std:bytes:to_vec $b\"abcdef\""), "$b\"abcdef\"", "bytes:from_vec");
        assert_eq!(gen("std:bytes:from_vec $[]"),                         "$b\"\"", "bytes:from_vec");
        assert_eq!(gen("std:bytes:from_vec $[1,2,3]"),                    "$b\"\\x01\\x02\\x03\"", "bytes:from_vec");

        assert_eq!(gen("std:bytes:to_hex $b\"abc\\xFF\""),                  "\"616263FF\"");
        assert_eq!(gen("std:bytes:to_hex $b\"abc\\xFF\" 6"),                "\"616263 FF\"");
        assert_eq!(gen("std:bytes:to_hex $b\"abc\\xFF\" 6 \":\""),          "\"616263:FF\"");
        assert_eq!(gen("std:bytes:to_hex $b\"abc\\xFF\" 1 \":\""),          "\"6:1:6:2:6:3:F:F\"");

        assert_eq!(gen("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\""),         "$b\"abc\\xFF\"");
        assert_eq!(gen("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 6"),       "$b\"abc\\xFF\"");
        assert_eq!(gen("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 6 \":\""), "$b\"abc\\xFF\"");
        assert_eq!(gen("std:bytes:from_hex ~ std:bytes:to_hex $b\"abc\\xFF\" 1 \":\""), "$b\"abc\\xFF\"");
        assert_eq!(gen("std:bytes:from_hex ~ std:bytes:to_hex $b\"\\x00abc\\xFF\" 1 \":\""), "$b\"\\0abc\\xFF\"");

        assert_eq!(gen("std:str:to_char_vec $q ABC "), "$[65,66,67]");
        assert_eq!(gen("$q ABC | std:str:to_char_vec | std:str:from_char_vec"), "\"ABC\"");
    }

}
