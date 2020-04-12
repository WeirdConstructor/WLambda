use crate::parser::{self};
use crate::compiler::*;
use crate::vval::*;
use crate::ops::*;

use std::rc::Rc;
use std::cell::RefCell;

const DEBUG_VM: bool = false;

#[derive(Debug, Clone)]
pub enum ResultSink {
    WriteTo(ResPos),
    WantResult,
    Null,
}

impl ResultSink {
    fn if_null<T>(&self, f: T) -> bool
        where T: FnOnce(ResPos)
    {
        match self {
            ResultSink::WriteTo(_) => true,
            ResultSink::WantResult => true,
            ResultSink::Null => {
                let rp = ResPos::Value(ResValue::Nul);
                f(rp);
                false
            },
        }
    }

    fn if_must_store<T>(&self, f: T) -> ResPos
        where T: FnOnce(ResPos)
    {
        match self {
            ResultSink::WriteTo(rp) => {
                f(*rp);
                *rp
            },
            ResultSink::WantResult => {
                let rp = ResPos::Stack(0);
                f(rp);
                rp
            },
            ResultSink::Null => ResPos::Value(ResValue::Nul),
        }
    }
}

pub type ProgWriteNode = Box<dyn Fn(&mut Prog, ResultSink) -> ResPos>;

pub struct ProgWriter {
    node:  ProgWriteNode,
}

impl ProgWriter {
    pub fn eval_to(&self, prog: &mut Prog, rp: ResPos) {
        (*self.node)(prog, ResultSink::WriteTo(rp));
    }

    fn eval_proxy(&self, prog: &mut Prog, rs: ResultSink) -> ResPos {
        (*self.node)(prog, rs)
    }

    fn eval_nul(&self, prog: &mut Prog) {
        let rp = (*self.node)(prog, ResultSink::Null);
        if let ResPos::Stack(_) = rp {
            prog.push_op(Op::Mov(rp, ResPos::Value(ResValue::Nul)));
        }
    }

    fn eval(&self, prog: &mut Prog) -> ResPos {
        (*self.node)(prog, ResultSink::WantResult)
    }
}

pub fn pw(f: ProgWriteNode) -> ProgWriter {
    ProgWriter {
        node:   Box::new(f),
    }
}

macro_rules! pw {
    ($prog: ident, $store: ident, $b: block) => {
        Ok(pw(Box::new(move |$prog, $store| {
            $b
        })))
    }
}

macro_rules! pw_null {
    ($prog: ident, $b: block) => {
        pw_provides_result_pos!($prog, {
            $b
            ResPos::Value(ResValue::Nul)
        })
    }
}

macro_rules! pw_provides_result_pos {
    ($prog: ident, $b: block) => {
        pw!($prog, store, {
            let pos = $b;
            match store {
                ResultSink::WriteTo(store_pos) => {
                    $prog.push_op(Op::Mov(pos, store_pos));
                    store_pos
                },
                ResultSink::WantResult => {
                    pos
                },
                ResultSink::Null => {
                    if let ResPos::Stack(_) = pos {
                        $prog.push_op(Op::Mov(pos, ResPos::Value(ResValue::Nul)));
                    }
                    ResPos::Value(ResValue::Nul)
                },
            }
        })
    }
}

macro_rules! pw_store_if_needed {
    ($prog: ident, $pos: ident, $b: block) => {
        pw!($prog, store, {
            let $pos =
                match store {
                    ResultSink::WriteTo(store_pos) => store_pos,
                    ResultSink::WantResult => ResPos::Stack(0),
                    ResultSink::Null => ResPos::Value(ResValue::Nul),
                };

            $b;
            $pos
        })
    }
}


macro_rules! pw_needs_storage {
    ($prog: ident, $pos: ident, $b: block) => {
        pw!($prog, store, {
            match store {
                ResultSink::WriteTo(store_pos) => {
                    let $pos = store_pos;
                    $b;
                    $pos
                },
                ResultSink::WantResult => {
                    let $pos = ResPos::Stack(0);
                    $b;
                    $pos
                },
                ResultSink::Null => {
                    let $pos = ResPos::Stack(0);
                    $b;
                    $prog.push_op(Op::Mov($pos, ResPos::Value(ResValue::Nul)));
                    ResPos::Value(ResValue::Nul)
                },
            }
        })
    }
}

macro_rules! in_reg {
    ($env: ident, $ret: ident, $prog: ident, $respos_var: ident) => {
        let $respos_var =
            match $respos_var {
                ResPos::Local(o)    => $env.get_local(*o as usize),
                ResPos::LocalRef(o) => $env.get_local(*o as usize).deref(),
                ResPos::Up(i)       => $env.get_up(*i as usize),
                ResPos::UpRef(i)    => $env.get_up(*i as usize).deref(),
                ResPos::Global(i)   => $prog.data[*i as usize].deref(),
                ResPos::GlobalRef(i)=> $prog.data[*i as usize].deref().deref(),
                ResPos::Arg(o)      => $env.arg(*o as usize),
                ResPos::Data(i)     => $prog.data[*i as usize].clone(),
                ResPos::Stack(_o)   => $env.pop(),
                ResPos::Value(ResValue::Ret) => std::mem::replace(&mut $ret, VVal::Nul),
                ResPos::Value(ResValue::Nul) => VVal::Nul,
                ResPos::Value(ResValue::SelfObj) => $env.self_object(),
                ResPos::Value(ResValue::SelfData) => $env.self_object().proto_data(),
                ResPos::Value(ResValue::AccumVal) => $env.get_accum_value(),
                ResPos::Value(ResValue::AccumFun) => $env.get_accum_function(),
            };
    }
}

macro_rules! out_reg {
    ($env: ident, $ret: ident, $prog: ident, $respos_var: ident, $val: expr) => {
        match $respos_var {
            ResPos::Local(o)        => $env.set_consume(*o as usize, $val),
            ResPos::LocalRef(o)     => $env.assign_ref_local(*o as usize, $val),
            ResPos::Up(i)           => $env.set_up(*i as usize, $val),
            ResPos::UpRef(i)        => $env.assign_ref_up(*i as usize, $val),
            ResPos::Global(i)       => { $prog.data[*i as usize].set_ref($val); },
            ResPos::GlobalRef(i)    => { $prog.data[*i as usize].deref().set_ref($val); },
            ResPos::Data(i)         => { $prog.data[*i as usize].set_ref($val); },
            ResPos::Stack(_)        => { $env.push($val); },
            ResPos::Value(ResValue::Ret) => { $ret = $val; },
            ResPos::Arg(_)          => (),
            ResPos::Value(_)        => (),
        };
    }
}

macro_rules! op_r {
    ($env: ident, $ret: ident, $prog: ident, $r: ident, $block: block) => {
        {
            let res = $block;
            out_reg!($env, $ret, $prog, $r, res);
        }
    }
}

macro_rules! op_a_r {
    ($env: ident, $ret: ident, $prog: ident, $a: ident, $r: ident, $block: block) => {
        {
            in_reg!($env, $ret, $prog, $a);
            let res = $block;
            out_reg!($env, $ret, $prog, $r, res);
        }
    }
}

macro_rules! op_a_b_r {
    ($env: ident, $ret: ident, $prog: ident, $a: ident, $b: ident, $r: ident, $block: block) => {
        {
            in_reg!($env, $ret, $prog, $a);
            in_reg!($env, $ret, $prog, $b);
            let res = $block;
            out_reg!($env, $ret, $prog, $r, res);
        }
    }
}

macro_rules! op_a_b_c_r {
    ($env: ident, $ret: ident, $prog: ident, $a: ident, $b: ident, $c: ident, $r: ident, $block: block) => {
        {
            in_reg!($env, $ret, $prog, $a);
            in_reg!($env, $ret, $prog, $b);
            in_reg!($env, $ret, $prog, $c);
            let res = $block;
            out_reg!($env, $ret, $prog, $r, res);
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

macro_rules! unwind_loop_info {
    ($env: ident) => {
        $env.unwind_to_depth($env.loop_info.uw_depth);
        while $env.sp > $env.loop_info.sp {
            $env.pop();
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
            unwind_loop_info!($env);
            $pc = $env.loop_info.pc;
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
            unwind_loop_info!($env);
            $pc = $env.loop_info.break_pc;
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
                Err(StackAction::Return((v_lbl, v))) => {
                    $env.unwind_to_depth($uw_depth);
                    $retv = Err(StackAction::Return((v_lbl, v)));
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
            VVal::Int(i)  => $o.at(i as usize).unwrap_or_else(|| VVal::Nul),
            VVal::Bol(b)  => $o.at(b as usize).unwrap_or_else(|| VVal::Nul),
            VVal::Sym(sy) => $o.$method(&sy.borrow()).unwrap_or_else(|| VVal::Nul),
            VVal::Str(sy) => $o.$method(&sy.borrow()).unwrap_or_else(|| VVal::Nul),
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
    let mut ret = VVal::Nul;

    let uw_depth = env.unwind_depth();
    let old_loop_info =
        std::mem::replace(&mut env.loop_info, LoopInfo::new());

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
            Op::Mov(a, r) => op_a_r!(env, ret, prog, a, r, { a }),
            Op::NewPair(a, b, r) => op_a_b_r!(env, ret, prog, a, b, r, {
                VVal::Pair(Box::new((
                    handle_err!(b, "first pair element", retv),
                    handle_err!(a, "second pair element", retv))))
            }),
            Op::NewNVec(vp, r) => {
                use crate::nvec::NVec;

                match vp.as_ref() {
                    NVecPos::IVec2(a, b) => {
                        in_reg!(env, ret, prog, a);
                        in_reg!(env, ret, prog, b);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        out_reg!(env, ret, prog, r,
                            VVal::IVec(NVec::Vec2(a.i(), b.i())));
                    },
                    NVecPos::IVec3(a, b, c) => {
                        in_reg!(env, ret, prog, a);
                        in_reg!(env, ret, prog, b);
                        in_reg!(env, ret, prog, c);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        out_reg!(env, ret, prog, r,
                            VVal::IVec(NVec::Vec3(a.i(), b.i(), c.i())));
                    },
                    NVecPos::IVec4(a, b, c, d) => {
                        in_reg!(env, ret, prog, a);
                        in_reg!(env, ret, prog, b);
                        in_reg!(env, ret, prog, c);
                        in_reg!(env, ret, prog, d);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        let d = handle_err!(d, "w nvector component", retv);
                        out_reg!(env, ret, prog, r,
                            VVal::IVec(NVec::Vec4(a.i(), b.i(), c.i(), d.i())));
                    },
                    NVecPos::FVec2(a, b) => {
                        in_reg!(env, ret, prog, a);
                        in_reg!(env, ret, prog, b);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        out_reg!(env, ret, prog, r,
                            VVal::FVec(NVec::Vec2(a.f(), b.f())));
                    },
                    NVecPos::FVec3(a, b, c) => {
                        in_reg!(env, ret, prog, a);
                        in_reg!(env, ret, prog, b);
                        in_reg!(env, ret, prog, c);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        out_reg!(env, ret, prog, r,
                            VVal::FVec(NVec::Vec3(a.f(), b.f(), c.f())));
                    },
                    NVecPos::FVec4(a, b, c, d) => {
                        in_reg!(env, ret, prog, a);
                        in_reg!(env, ret, prog, b);
                        in_reg!(env, ret, prog, c);
                        in_reg!(env, ret, prog, d);
                        let a = handle_err!(a, "x nvector component", retv);
                        let b = handle_err!(b, "y nvector component", retv);
                        let c = handle_err!(c, "z nvector component", retv);
                        let d = handle_err!(d, "w nvector component", retv);
                        out_reg!(env, ret, prog, r,
                            VVal::FVec(NVec::Vec4(a.f(), b.f(), c.f(), d.f())));
                    },
                }
            },
            Op::ToRef(a, r, trtype) => {
                match trtype {
                    ToRefType::CaptureRef =>
                        match a {
                            ResPos::Local(i) => out_reg!(env, ret, prog, r, {
                                env.get_local_captured_ref(*i as usize)
                            }),
                            ResPos::Global(i) => out_reg!(env, ret, prog, r, {
                                prog.data[*i as usize].clone()
                            }),
                            ResPos::Up(i) => out_reg!(env, ret, prog, r, {
                                env.get_up_captured_ref(*i as usize)
                            }),
                            _ => op_a_r!(env, ret, prog, a, r, { a.to_ref() }),
                        },
                    ToRefType::ToRef =>
                        op_a_r!(env, ret, prog, a, r, { a.to_ref() }),
                    ToRefType::Weakable =>
                        op_a_r!(env, ret, prog, a, r, {
                            a.to_weakened_upvalue_ref()
                        }),
                    ToRefType::Deref =>
                        op_a_r!(env, ret, prog, a, r, {
                            a.deref()
                        }),
                }
            },
            Op::Argv(r)             => op_r!(env, ret, prog, r, { env.argv() }),
            Op::End                 => { break; },
            Op::Unwind              => { env.unwind_one(); },
            Op::IterInit(iterable, body_ops) => {
                in_reg!(env, ret, prog, iterable);
                env.push_loop_info(pc, pc + *body_ops as usize);
                env.push_iter(iterable.iter());
            },
            Op::IterNext(ivar) => {
                if let Some(i) = &mut env.iter {
                    if let Some(v) = i.next() {
                        out_reg!(env, ret, prog, ivar, v.0);
                    } else {
                        unwind_loop_info!(env);
                        pc = env.loop_info.break_pc;
                        env.unwind_one();
                    }
                } else {
                    unwind_loop_info!(env);
                    pc = env.loop_info.break_pc;
                    env.unwind_one();
                }
            },
            Op::PushLoopInfo(body_ops) => {
                env.push_loop_info(pc, pc + *body_ops as usize);
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
            Op::Add(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Int(a) = a {
                    VVal::Int(a.wrapping_add(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f + b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(ln + re.nvec()),
                        (VVal::FVec(ln), re) => VVal::FVec(ln + re.nvec()),
                        (le, re)             => VVal::Int(le.i().wrapping_add(re.i()))
                    }
                }
            }),
            Op::Sub(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Int(a) = a {
                    VVal::Int(a.wrapping_sub(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f - b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(ln - re.nvec()),
                        (VVal::FVec(ln), re) => VVal::FVec(ln - re.nvec()),
                        (le, re)             => VVal::Int(le.i().wrapping_sub(re.i()))
                    }
                }
            }),
            Op::Div(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
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
                        (VVal::IVec(ln), re) => VVal::IVec(ln / re.i()),
                        (VVal::FVec(ln), re) => VVal::FVec(ln / re.f()),
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
            Op::Mul(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Int(a) = a {
                    VVal::Int(a.wrapping_mul(b.i()))
                } else if let VVal::Flt(f) = a {
                    VVal::Flt(f * b.f())
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(ln * re.i()),
                        (VVal::FVec(ln), re) => VVal::FVec(ln * re.f()),
                        (le, re)             => VVal::Int(le.i().wrapping_mul(re.i()))
                    }
                }
            }),
            Op::Mod(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a {
                    VVal::Flt(f % b.f())
                } else {
                    VVal::Int(a.i().wrapping_rem(b.i()))
                }
            }),
            Op::Le(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f <= b.f()) }
                else { VVal::Bol(a.i() <= b.i()) }
            }),
            Op::Lt(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f < b.f()) }
                else { VVal::Bol(a.i() < b.i()) }
            }),
            Op::Ge(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f >= b.f()) }
                else { VVal::Bol(a.i() >= b.i()) }
            }),
            Op::Gt(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Bol(f > b.f()) }
                else { VVal::Bol(a.i() > b.i()) }
            }),
            Op::Eq(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                VVal::Bol(a.eqv(&b))
            }),
            Op::NewList(r) => op_r!(env, ret, prog, r, { VVal::vec() }),
            Op::ListPush(a, b, r) => op_a_b_r!(env, ret, prog, a, b, r, {
                b.push(handle_err!(a, "list element", retv));
                b
            }),
            Op::ListSplice(a, b, r) => op_a_b_r!(env, ret, prog, a, b, r, {
                for (e, _) in a.iter() {
                    b.push(e);
                }
                b
            }),
            Op::NewMap(r) => op_r!(env, ret, prog, r, { VVal::map() }),
            Op::MapSetKey(v, k, m, r) => op_a_b_c_r!(env, ret, prog, v, k, m, r, {
                let v = handle_err!(v, "map value", retv);
                let k = handle_err!(k, "map key", retv);
                m.set_key(&k, v)?;
                m
            }),
            Op::GetIdx(o, idx, r) => op_a_r!(env, ret, prog, o, r, {
                handle_err!(o, "map/list", retv)
                .at(*idx as usize).unwrap_or_else(|| VVal::Nul)
            }),
            Op::GetIdx2(o, idx, r) => op_a_r!(env, ret, prog, o, r, {
                handle_err!(o, "map/list", retv)
                .at(idx.0 as usize).unwrap_or_else(|| VVal::Nul)
                .at(idx.1 as usize).unwrap_or_else(|| VVal::Nul)
            }),
            Op::GetIdx3(o, idx, r) => op_a_r!(env, ret, prog, o, r, {
                handle_err!(o, "map/list", retv)
                .at(idx.0 as usize).unwrap_or_else(|| VVal::Nul)
                .at(idx.1 as usize).unwrap_or_else(|| VVal::Nul)
                .at(idx.2 as usize).unwrap_or_else(|| VVal::Nul)
            }),
            Op::GetSym(o, sym, r) => op_a_r!(env, ret, prog, o, r, {
                handle_err!(o, "map/list", retv)
                .get_key(&sym).unwrap_or_else(|| VVal::Nul)
            }),
            Op::GetSym2(o, sym, r) => op_a_r!(env, ret, prog, o, r, {
                handle_err!(o, "map/list", retv)
                .get_key(&sym.0).unwrap_or_else(|| VVal::Nul)
                .get_key(&sym.1).unwrap_or_else(|| VVal::Nul)
            }),
            Op::GetSym3(o, sym, r) => op_a_r!(env, ret, prog, o, r, {
                handle_err!(o, "map/list", retv)
                .get_key(&sym.0).unwrap_or_else(|| VVal::Nul)
                .get_key(&sym.1).unwrap_or_else(|| VVal::Nul)
                .get_key(&sym.2).unwrap_or_else(|| VVal::Nul)
            }),
            Op::GetKey(o, k, r) => {
                in_reg!(env, ret, prog, k);
                in_reg!(env, ret, prog, o);

                let o = handle_err!(o, "field idx/key", retv);
                let k = handle_err!(k, "map/list", retv);
                let res = get_key!(o, k, get_key, env, retv, uw_depth, prog, pc);
                out_reg!(env, ret, prog, r, res);
            },
            Op::Destr(a, info) => {
                in_reg!(env, ret, prog, a);
                info.destructure(env, a);
            },
            Op::NewErr(e, r) => op_a_r!(env, ret, prog, e, r, {
                VVal::err(e, prog.debug[pc].clone().unwrap())
            }),
            Op::NewClos(f, r) => op_a_r!(env, ret, prog, f, r, {
                let fun = f.clone_and_rebind_upvalues(|upvs, upvalues| {
                    copy_upvs(upvs, env, upvalues);
                });
                fun
            }),
            Op::MapSplice(s, m, r) => op_a_b_r!(env, ret, prog, s, m, r, {
                for (e, k) in s.iter() {
                    match m.set_key(&k.unwrap(), e) {
                        Ok(_) => (),
                        Err(e) => {
                            retv =
                                Err(StackAction::panic_str(
                                    format!("map set key errro: {}", e),
                                    prog.debug[pc].clone()));
                        }
                    }
                }

                if retv.is_err() {
                    break;
                }

                m
            }),
            Op::CallMethodKey(o, k, argc, r) => {
                in_reg!(env, ret, prog, k);
                in_reg!(env, ret, prog, o);
                let o = handle_err!(o, "field idx/key", retv);
                let k = handle_err!(k, "map/list", retv);

                let f = get_key!(o, k, proto_lookup, env, retv, uw_depth, prog, pc);

                let argc = *argc as usize;
                env.push_unwind_self(o);
                let call_ret = call_func!(f, argc, argc, env, retv, uw_depth, prog, pc, v, {
                    env.unwind_one();
                    out_reg!(env, ret, prog, r, v);
                });
            },
            Op::CallMethodSym(o, k, argc, r) => {
                in_reg!(env, ret, prog, o);
                let o = handle_err!(o, "field idx/key", retv);

                let f = o.proto_lookup(&*k).unwrap_or_else(|| VVal::Nul);

                let argc = *argc as usize;
                env.push_unwind_self(o);
                let call_ret = call_func!(f, argc, argc, env, retv, uw_depth, prog, pc, v, {
                    env.unwind_one();
                    out_reg!(env, ret, prog, r, v);
                });
            },
            Op::Call(argc, r) => {
                let argc = *argc as usize;
                let f = env.stk(argc + 1).clone();

                let call_ret = call_func!(f, argc, argc + 1, env, retv, uw_depth, prog, pc, v, {
                    out_reg!(env, ret, prog, r, v);
                });
            },
            Op::Apply(argv, func, r) => {
                in_reg!(env, ret, prog, argv);
                in_reg!(env, ret, prog, func);

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
                    let v = argv.at(i).unwrap_or_else(|| VVal::Nul);
                    env.push(v);
                }

                let call_ret = func.call_internal(env, argc);
                env.popn(argc);

                match call_ret {
                    Ok(v) => { out_reg!(env, ret, prog, r, v); },
                    Err(StackAction::Return((v_lbl, v))) => {
                        env.unwind_to_depth(uw_depth);
                        retv = Err(StackAction::Return((v_lbl, v)));
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
            Op::JmpIf(a, jmp_offs) => {
                in_reg!(env, ret, prog, a);
                if a.b() { pc = (pc as i32 + *jmp_offs) as usize; }
            },
            Op::JmpIfN(a, jmp_offs) => {
                in_reg!(env, ret, prog, a);
                if !a.b() { pc = (pc as i32 + *jmp_offs) as usize; }
            },
            Op::OrJmp(a, jmp_offs, r) => {
                in_reg!(env, ret, prog, a);
                if a.b() {
                    pc = (pc as i32 + *jmp_offs) as usize;
                    out_reg!(env, ret, prog, r, a);
                }
            },
            Op::AndJmp(a, jmp_offs, r) => {
                in_reg!(env, ret, prog, a);
                if !a.b() {
                    pc = (pc as i32 + *jmp_offs) as usize;
                    out_reg!(env, ret, prog, r, a);
                }
            },
            Op::CtrlFlow(flw) => {
                match flw {
                    CtrlFlow::Next => {
                        handle_next!(env, pc, uw_depth, retv);
                    },
                    CtrlFlow::Break(a) => {
                        in_reg!(env, ret, prog, a);
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
                                     pc, format!("{:?}", op),
                                     env.sp, env.bp, env.unwind_depth(), syn);
                            if i == pc {
                                env.dump_stack();
                            }
                        }
                    },
                    Builtin::Export(name, a) => {
                        in_reg!(env, ret, prog, a);
                        env.export_name(name, &a);
                    },
                }
            },
        }
        if DEBUG_VM {
            env.dump_stack();
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

fn vm_compile_def2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_global: bool) -> Result<ProgWriter, CompileError> {
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    let prev_max_arity = ce.borrow().implicit_arity.clone();

    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let destr   = ast.at(3).unwrap_or_else(|| VVal::Nul);

    //d// println!("COMP DEF: {:?} global={}, destr={}", vars, is_global, destr.b());

    if destr.b() {
        let val_pw = vm_compile2(&value, ce)?;

        check_for_at_arity(prev_max_arity, ast, ce, &vars);

        let poses =
            vars.map_ok_skip(
                |v| ce.borrow_mut().def(&v.s_raw(), is_global),
                0);

        pw_null!(prog, {
            let vp = val_pw.eval(prog);
            prog.set_dbg(spos.clone());
            prog.push_op(Op::Destr(vp, Box::new(DestructureInfo {
                vars:   vars.clone(),
                poses:  poses.clone(),
                is_ref: false,
            })));
        })
    } else {
        let varname = vars.at(0).unwrap().s_raw();
        ce.borrow_mut().recent_var = varname.clone();

        if is_global {
            let val_pw = vm_compile2(&value, ce)?;

            if let VarPos::Global(r) = ce.borrow_mut().def(&varname, true) {
                pw_null!(prog, {
                    prog.set_dbg(spos.clone());
                    let gp = prog.global_pos(r.clone());
                    prog.dump();
                    val_pw.eval_to(prog, gp);
                })
            } else {
                panic!("Defining global did not return a global!");
            }
        } else {
            let next_local = ce.borrow_mut().next_local();
            let val_pw = vm_compile2(&value, ce)?;
            ce.borrow_mut().def_local(&varname, next_local);

            pw_null!(prog, {
                prog.set_dbg(spos.clone());
                val_pw.eval_to(prog, ResPos::Local(next_local as u16));
            })
        }
    }
}

pub fn pw_arg(arg_idx: usize, to_ref: bool) -> Result<ProgWriter, CompileError> {
    let arg_pos = ResPos::Arg(arg_idx as u16);

    if to_ref {
        pw!(prog, store, {
            store.if_must_store(|store_pos| {
                prog.push_op(Op::Mov(arg_pos, store_pos));
                prog.push_op(Op::ToRef(store_pos, store_pos, ToRefType::CaptureRef));
            })
        })
    } else {
        pw_provides_result_pos!(prog, {
            arg_pos
        })
    }
}

fn vm_compile_var2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, capt_ref: bool) -> Result<ProgWriter, CompileError> {
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    let var = ast.at(1).unwrap();
    var.with_s_ref(|var_s: &str| -> Result<ProgWriter, CompileError> {
        match var_s {
            "_"  => { set_impl_arity(1,  ce); pw_arg(0, capt_ref) },
            "_1" => { set_impl_arity(2,  ce); pw_arg(1, capt_ref) },
            "_2" => { set_impl_arity(3,  ce); pw_arg(2, capt_ref) },
            "_3" => { set_impl_arity(4,  ce); pw_arg(3, capt_ref) },
            "_4" => { set_impl_arity(5,  ce); pw_arg(4, capt_ref) },
            "_5" => { set_impl_arity(6,  ce); pw_arg(5, capt_ref) },
            "_6" => { set_impl_arity(7,  ce); pw_arg(6, capt_ref) },
            "_7" => { set_impl_arity(8,  ce); pw_arg(7, capt_ref) },
            "_8" => { set_impl_arity(9,  ce); pw_arg(8, capt_ref) },
            "_9" => { set_impl_arity(10, ce); pw_arg(9, capt_ref) },
            "@"  => {
                ce.borrow_mut().implicit_arity.1 = ArityParam::Infinite;
                pw!(prog, store, {
                    store.if_must_store(|store_pos| {
                        prog.set_dbg(spos.clone());
                        prog.push_op(Op::Argv(store_pos));
                        if capt_ref {
                            prog.push_op(Op::ToRef(store_pos, store_pos, ToRefType::CaptureRef));
                        }
                    })
                })
            },
            _ => {
                let pos = ce.borrow_mut().get(var_s);
                let mk_respos : Result<Box<dyn Fn(&mut Prog) -> ResPos>, CompileError> =
                    match pos {
                        VarPos::UpValue(i) =>
                            Ok(Box::new(move |_prog: &mut Prog| ResPos::Up(i as u16))),
                        VarPos::Local(i) =>
                            Ok(Box::new(move |_prog: &mut Prog| ResPos::Local(i as u16))),
                        VarPos::Global(v) =>
                            Ok(Box::new(move |prog: &mut Prog| prog.global_pos(v.clone()))),
                        VarPos::Const(v) =>
                            Ok(Box::new(move |prog: &mut Prog| prog.data_pos(v.clone()))),
                        VarPos::NoPos => {
                            Err(ast.compile_err(
                                format!("Variable '{}' undefined", var_s)))
                        }
                    };
                let mk_respos = mk_respos?;
                pw_provides_result_pos!(prog, {
                    let var_respos = mk_respos(prog);
                    prog.set_dbg(spos.clone());
                    if capt_ref {
                        let rp = ResPos::Stack(0);
                        prog.push_op(Op::ToRef(var_respos, rp, ToRefType::CaptureRef));
                        rp
                    } else {
                        var_respos
                    }
                })
            }
        }
    })
}

fn vm_compile_assign2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>, is_ref: bool)
    -> Result<ProgWriter, CompileError>
{
    let prev_max_arity = ce.borrow().implicit_arity.clone();

    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    let vars          = ast.at(1).unwrap();
    let value         = ast.at(2).unwrap();
    let destr         = ast.at(3).unwrap_or_else(|| VVal::Nul);

    if destr.b() {
        let val_pw = vm_compile2(&value, ce)?;

        check_for_at_arity(prev_max_arity, ast, ce, &vars);

        let poses = vars.map_ok_skip(|v| ce.borrow_mut().get(&v.s_raw()), 0);

        pw_null!(prog, {
            let vp = val_pw.eval(prog);
            prog.set_dbg(spos.clone());
            prog.push_op(Op::Destr(vp, Box::new(DestructureInfo {
                vars:   vars.clone(),
                poses:  poses.clone(),
                is_ref: is_ref,
            })));
        })
    } else {
        let varname = &vars.at(0).unwrap().s_raw();
        let pos     = ce.borrow_mut().get(varname);

        let val_pw = vm_compile2(&value, ce)?;

        match pos {
            VarPos::Const(_) =>
                return Err(ast.compile_err(
                    format!("Can't assign to constant '{}'", varname))),
            VarPos::NoPos =>
                return Err(ast.compile_err(
                    format!("Can't assign to undefined local variable '{}'", varname))),
            _ => (),
        }

        pw_null!(prog, {
            let rp =
                match pos.clone() {
                    VarPos::Local(vip) => {
                        if is_ref {
                            ResPos::LocalRef(vip as u16)
                        } else {
                            ResPos::Local(vip as u16)
                        }
                    },
                    VarPos::Global(r) => {
                        if is_ref {
                            prog.global_ref_pos(r.clone())
                        } else {
                            prog.global_pos(r.clone())
                        }
                    },
                    VarPos::UpValue(vip) => {
                        if is_ref {
                            ResPos::UpRef(vip as u16)
                        } else {
                            ResPos::Up(vip as u16)
                        }
                    },
                    _ => ResPos::Value(ResValue::Nul)
                };

            val_pw.eval_to(prog, rp)
        })
    }
}

fn vm_compile_stmts2(ast: &VVal, skip_cnt: usize, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ProgWriter, CompileError> {
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    let exprs : Vec<ProgWriter> =
        ast.map_skip( |e| { vm_compile2(e, ce) }, skip_cnt)?;

    pw!(prog, store, {
        let expr_count = exprs.len();
        let mut i      = 0;
        let mut res    = ResPos::Value(ResValue::Nul);

        for e in exprs.iter() {
            prog.set_dbg(spos.clone());

            if i == expr_count - 1 {
                res = e.eval_proxy(prog, store.clone());
            } else {
                e.eval_nul(prog);
            }

            i += 1;
        }

        res
    })
}

macro_rules! var_env_clear_locals {
    ($prog: ident, $from: ident, $to: ident, $spos: ident, $block: block) => {
        if $from != $to {
            $prog.set_dbg($spos.clone());
            $prog.push_op(Op::ClearLocals($from as u16, $to as u16));

            let res = $block;

            $prog.set_dbg($spos.clone());
            $prog.push_op(Op::Unwind);

            res
        } else {
            $block
        }
    }
}

fn vm_compile_block2(ast: &VVal, skip_cnt: usize, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ProgWriter, CompileError> {
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    ce.borrow_mut().push_block_env();
    let stmts = vm_compile_stmts2(ast, skip_cnt, ce)?;
    let (from_local_idx, to_local_idx) = ce.borrow_mut().pop_block_env();

    pw!(prog, store, {
        var_env_clear_locals!(prog, from_local_idx, to_local_idx, spos, {
            stmts.eval_proxy(prog, store)
        })
    })
}

fn vm_compile_direct_block2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    match ast {
        VVal::Lst(_) => {
            let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
            let syn  = syn.get_syn();

            match syn {
                Syntax::Func => {
                    let label          = ast.at(1).unwrap();
                    let explicit_arity = ast.at(2).unwrap();

                    if !label.is_none() {
                        return Err(
                            ast.compile_err(
                                format!("direct blocks don't support labels: {}",
                                        ast.s())));
                    }

                    if !explicit_arity.is_none() {
                        return Err(
                            ast.compile_err(
                                format!("direct blocks don't support arity: {}",
                                        ast.s())));
                    }

                    vm_compile_block2(ast, 3, ce)
                },
                _ => vm_compile2(ast, ce),
            }
        },
        _ => vm_compile2(ast, ce),
    }
}


fn vm_compile_binop2(ast: &VVal, op: BinOp, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    let a_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
    let b_pw = vm_compile2(&ast.at(2).unwrap(), ce)?;

    pw_needs_storage!(prog, store, {
        let ap = a_pw.eval(prog);
        let bp = b_pw.eval(prog);
        prog.set_dbg(spos.clone());
        prog.push_op(op.to_op(bp, ap, store));
    })
}

fn vm_compile_const_value(val: &VVal) -> Result<VVal, CompileError> {
    match val {
        VVal::Lst(l) => {
            let l = l.borrow();
            match l[0].get_syn() {
                Syntax::Key => Ok(l[1].clone()),
                Syntax::Str => Ok(l[1].clone()),
                Syntax::Lst => {
                    let v = VVal::vec();
                    for i in l.iter().skip(1) {
                        v.push(vm_compile_const_value(i)?);
                    }
                    Ok(v)
                },
                Syntax::Map => {
                    let m = VVal::map();
                    for i in l.iter().skip(1) {
                        let key = vm_compile_const_value(&i.at(0).unwrap_or(VVal::Nul))?;
                        let val = vm_compile_const_value(&i.at(1).unwrap_or(VVal::Nul))?;
                        m.set_key_mv(key.s_raw(), val);
                    }
                    Ok(m)
                },
                _ => Err(val.to_compile_err(
                    format!(
                        "Invalid literal in constant definition: {}",
                        val.s())).err().unwrap()),
            }
        },
        _ => Ok(val.clone()),
    }
}

fn vm_compile_const(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let vars    = ast.at(1).unwrap();
    let value   = ast.at(2).unwrap();
    let destr   = ast.at(3).unwrap_or(VVal::Nul);

    if destr.b() {
        for (i, (v, _)) in vars.iter().enumerate() {
            let varname = v.s_raw();
            let val = vm_compile_const_value(&value)?;
            let val =
                match val {
                    VVal::Lst(_) => val.at(i).unwrap_or(VVal::Nul),
                    VVal::Map(_) => val.get_key(&varname).unwrap_or(VVal::Nul),
                    _ => val,
                };

            ce.borrow_mut().def_const(&varname, val);
        }
    } else {
        let varname = vars.at(0).unwrap().s_raw();
        let const_val = vm_compile_const_value(&value)?;
        ce.borrow_mut().def_const(&varname, const_val);
    }

    pw_null!(prog, { })
}

pub fn vm_compile_break2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    if ast.len() > 3 {
        return Err(ast.compile_err(format!("break takes 0 or 1 arguments")))
    }

    if let Some(expr) = ast.at(2) {
        let expr = vm_compile2(&expr, ce)?;
        pw_null!(prog, {
            let ep = expr.eval(prog);
            prog.set_dbg(spos.clone());
            prog.push_op(Op::CtrlFlow(CtrlFlow::Break(ep)));
        })
    } else {
        pw_null!(prog, {
            prog.set_dbg(spos.clone());
            prog.push_op(Op::CtrlFlow(
                CtrlFlow::Break(ResPos::Value(ResValue::Nul))));
        })
    }
}

pub fn vm_compile_next2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    if ast.len() > 2 {
        return Err(ast.compile_err(format!("next takes no arguments")))
    }

    pw_null!(prog, {
        prog.set_dbg(spos.clone());
        prog.push_op(Op::CtrlFlow(CtrlFlow::Next));
    })
}

pub fn vm_compile_if2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    if ast.len() != 4 && ast.len() != 5 {
        return Err(ast.compile_err(
            format!("? takes 1 or 2 arguments (condition and expression)")))
    }

    let cond =
        vm_compile_direct_block2(
            &ast.at(2).unwrap_or_else(|| VVal::Nul), ce)?;

    let then_body =
        vm_compile_direct_block2(
            &ast.at(3).unwrap_or_else(|| VVal::Nul), ce)?;

    if let Some(else_body) = ast.at(4) {
        let else_body = vm_compile_direct_block2(&else_body, ce)?;

        pw!(prog, store, {

            let needs_store = store.if_null(|_| {
                let mut then_body_prog = Prog::new();
                let mut else_body_prog = Prog::new();
                let tbp = then_body.eval_nul(&mut then_body_prog);
                let ebp = else_body.eval_nul(&mut else_body_prog);

                then_body_prog.set_dbg(spos.clone());
                then_body_prog.push_op(Op::Jmp(else_body_prog.op_count() as i32));

                let condval = cond.eval(prog);
                prog.set_dbg(spos.clone());
                prog.push_op(Op::JmpIfN(condval, then_body_prog.op_count() as i32));
                prog.append(then_body_prog);
                prog.append(else_body_prog);
            });

            if needs_store {
                store.if_must_store(|store_pos| {
                    let mut then_body_prog = Prog::new();
                    let mut else_body_prog = Prog::new();
                    let tbp = then_body.eval(&mut then_body_prog);
                    let ebp = else_body.eval(&mut else_body_prog);
                    else_body_prog.set_dbg(spos.clone());
                    else_body_prog.push_op(Op::Mov(ebp, store_pos));

                    then_body_prog.set_dbg(spos.clone());
                    then_body_prog.push_op(Op::Mov(tbp, store_pos));
                    then_body_prog.set_dbg(spos.clone());
                    then_body_prog.push_op(Op::Jmp(else_body_prog.op_count() as i32));

                    let condval = cond.eval(prog);
                    prog.set_dbg(spos.clone());
                    prog.push_op(Op::JmpIfN(condval, then_body_prog.op_count() as i32));
                    prog.append(then_body_prog);
                    prog.append(else_body_prog);
                })
            } else {
                ResPos::Value(ResValue::Nul)
            }
        })
    } else {
        pw!(prog, store, {
            let needs_store = store.if_null(|_| {
                let mut then_body_prog = Prog::new();
                then_body.eval_nul(&mut then_body_prog);

                let condval = cond.eval(prog);
                prog.set_dbg(spos.clone());
                prog.push_op(Op::JmpIfN(condval, then_body_prog.op_count() as i32));
                prog.append(then_body_prog);
            });

            if needs_store {
                store.if_must_store(|store_pos| {
                    let mut then_body_prog = Prog::new();
                    let tbp = then_body.eval_to(&mut then_body_prog, store_pos);
                    then_body_prog.set_dbg(spos.clone());
                    then_body_prog.push_op(Op::Jmp(1));

                    let condval = cond.eval(prog);
                    prog.set_dbg(spos.clone());
                    prog.push_op(Op::JmpIfN(condval, then_body_prog.op_count() as i32));
                    prog.append(then_body_prog);
                    prog.push_op(Op::Mov(ResPos::Value(ResValue::Nul), store_pos));
                })
            } else {
                ResPos::Value(ResValue::Nul)
            }
        })
    }
}

pub fn vm_compile_while2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    if ast.len() != 4 {
        return Err(ast.compile_err(
            format!("while takes exactly 2 arguments \
                     (condition and expression)")));
    }

    let cond =
        vm_compile_direct_block2(
            &ast.at(2).unwrap_or_else(|| VVal::Nul), ce)?;

    let body =
        vm_compile_direct_block2(
            &ast.at(3).unwrap_or_else(|| VVal::Nul), ce)?;

    return pw_null!(prog, {
        // Create the OPs for the body:
        let mut body_prog = Prog::new();
        body.eval_nul(&mut body_prog);
        let body_op_count = body_prog.op_count();

        let mut cond_prog = Prog::new();
        let cond_val = cond.eval(&mut cond_prog);

        prog.set_dbg(spos.clone());
        prog.push_op(Op::PushLoopInfo(
            (cond_prog.op_count() + body_op_count + 2) as u16));

        let cond_op_count1 = prog.op_count();
        cond_prog.set_dbg(spos.clone());
        cond_prog.push_op(
            Op::JmpIfN(cond_val, body_op_count as i32 + 1));
        prog.append(cond_prog);

        let cond_offs =
            body_op_count + (prog.op_count() - cond_op_count1);
        body_prog.set_dbg(spos.clone());
        body_prog.push_op(Op::Jmp(-(cond_offs as i32 + 1)));
        prog.append(body_prog);
        prog.set_dbg(spos.clone());
        prog.push_op(Op::Unwind);
    });
}

pub fn vm_compile_iter2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<ProgWriter, CompileError>
{
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    if ast.len() != 5 {
        return Err(ast.compile_err(
            format!("while takes exactly 3 arguments \
                    (variable identifier, iterable expression and \
                    iteration expression)")));
    }

    if ast.at(2).unwrap_or_else(|| VVal::Nul).at(0).unwrap_or_else(|| VVal::Nul).get_syn() != Syntax::Var {
        return Err(ast.compile_err(
            format!("iter takes an identifier as first argument")));
    }

    let varname =
        ast.at(2).unwrap_or_else(|| VVal::Nul)
           .at(1).unwrap_or(VVal::Nul).s_raw();

    ce.borrow_mut().push_block_env();
    let iter_var = ce.borrow_mut().next_local();

    let iterable =
        vm_compile_direct_block2(
            &ast.at(3).unwrap_or_else(|| VVal::Nul), ce)?;

    ce.borrow_mut().def_local(&varname, iter_var);

    let expr =
        vm_compile_direct_block2(
            &ast.at(4).unwrap_or_else(|| VVal::Nul), ce)?;

    let (from_local_idx, to_local_idx) = ce.borrow_mut().pop_block_env();

    pw_null!(prog, {
        let iter_var = ResPos::Local(iter_var as u16);

        let mut body = Prog::new();
        expr.eval_nul(&mut body);
        body.set_dbg(spos.clone());
        body.push_op(Op::Jmp(-(body.op_count() as i32 + 2)));

        let ip = iterable.eval(prog);
        body.set_dbg(spos.clone());
        prog.push_op(Op::IterInit(ip, (body.op_count() + 3) as i32));
        var_env_clear_locals!(prog, from_local_idx, to_local_idx, spos, {
            body.set_dbg(spos.clone());
            prog.push_op(Op::IterNext(iter_var));
            prog.append(body);
        });
    })
}

pub fn vm_compile2(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ProgWriter, CompileError> {
    match ast {
        VVal::Lst(_) => {
            let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
            let spos = syn.get_syn_pos();
            let syn  = syn.get_syn();

            match syn {
                Syntax::Block      => vm_compile_block2(ast, 1, ce),
                Syntax::Assign     => vm_compile_assign2(ast, ce, false),
                Syntax::AssignRef  => vm_compile_assign2(ast, ce, true),
                Syntax::Var        => vm_compile_var2(ast, ce, false),
                Syntax::CaptureRef => vm_compile_var2(ast, ce, true),
                Syntax::Def        => vm_compile_def2(ast, ce, false),
                Syntax::DefGlobRef => vm_compile_def2(ast, ce, true),
                Syntax::DefConst   => vm_compile_const(ast, ce),
                Syntax::BinOpAdd   => vm_compile_binop2(ast, BinOp::Add, ce),
                Syntax::BinOpSub   => vm_compile_binop2(ast, BinOp::Sub, ce),
                Syntax::BinOpDiv   => vm_compile_binop2(ast, BinOp::Div, ce),
                Syntax::BinOpMod   => vm_compile_binop2(ast, BinOp::Mod, ce),
                Syntax::BinOpMul   => vm_compile_binop2(ast, BinOp::Mul, ce),
                Syntax::BinOpGe    => vm_compile_binop2(ast, BinOp::Ge,  ce),
                Syntax::BinOpGt    => vm_compile_binop2(ast, BinOp::Gt,  ce),
                Syntax::BinOpLe    => vm_compile_binop2(ast, BinOp::Le,  ce),
                Syntax::BinOpLt    => vm_compile_binop2(ast, BinOp::Lt,  ce),
                Syntax::BinOpEq    => vm_compile_binop2(ast, BinOp::Eq,  ce),
                Syntax::Ref => {
                    let ref_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    pw_needs_storage!(prog, store, {
                        let ref_rp = ref_pw.eval(prog);
                        prog.push_op(Op::ToRef(ref_rp, store, ToRefType::ToRef));
                    })
                },
                Syntax::WRef => {
                    let ref_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    pw_needs_storage!(prog, store, {
                        let ref_rp = ref_pw.eval(prog);
                        prog.push_op(Op::ToRef(ref_rp, store, ToRefType::Weakable));
                    })
                },
                Syntax::Deref => {
                    let ref_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    pw_needs_storage!(prog, store, {
                        let ref_rp = ref_pw.eval(prog);
                        prog.push_op(Op::ToRef(ref_rp, store, ToRefType::Deref));
                    })
                },
                Syntax::Lst => {
                    let mut pws : std::vec::Vec<(bool, ProgWriter)> = vec![];
                    for (a, _) in ast.iter().skip(1) {
                        if a.is_vec() {
                            if let VVal::Syn(SynPos { syn: Syntax::VecSplice, .. }) =
                                a.at(0).unwrap_or_else(|| VVal::Nul)
                            {
                                let splice_pw = vm_compile2(&a.at(1).unwrap(), ce)?;
                                pws.push((true, splice_pw));
                                continue;
                            }
                        }

                        let val_pw = vm_compile2(&a, ce)?;
                        pws.push((false, val_pw));
                    }

                    pw_provides_result_pos!(prog, {
                        prog.push_op(Op::NewList(ResPos::Stack(0)));

                        for (is_splice, pw) in pws.iter() {
                            pw.eval_to(prog, ResPos::Stack(0));
                            prog.set_dbg(spos.clone());
                            if *is_splice {
                                prog.push_op(Op::ListSplice(
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0)));
                            } else {
                                prog.push_op(Op::ListPush(
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0)));
                            }
                        }

                        ResPos::Stack(0)
                    })
                },
                Syntax::Map => {
                    let mut pws : std::vec::Vec<(ProgWriter, Option<ProgWriter>)> =
                        vec![];

                    for (e, _) in ast.iter().skip(1) {
                        let k = e.at(0).unwrap();
                        let v = e.at(1).unwrap();

                        if let VVal::Syn(SynPos { syn: Syntax::MapSplice, .. }) = k {
                            let sc_pw = vm_compile2(&v, ce)?;
                            pws.push((sc_pw, None));
                            continue;
                        }

                        let kc_pw = vm_compile2(&k, ce)?;
                        if let VVal::Sym(y) = k {
                            ce.borrow_mut().recent_var = y.borrow().clone();
                        } else {
                            let recent_sym = ce.borrow().recent_sym.clone();
                            ce.borrow_mut().recent_var = recent_sym;
                        }

                        let vc_pw = vm_compile2(&v, ce)?;
                        pws.push((kc_pw, Some(vc_pw)));
                    }

                    pw_provides_result_pos!(prog, {
                        prog.push_op(Op::NewMap(ResPos::Stack(0)));

                        for (kc_pw, vc_pw) in pws.iter() {
                            if let Some(vc_pw) = vc_pw {
                                kc_pw.eval_to(prog, ResPos::Stack(0));
                                vc_pw.eval_to(prog, ResPos::Stack(0));

                                prog.set_dbg(spos.clone());
                                prog.push_op(Op::MapSetKey(
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0)));
                            } else {
                                kc_pw.eval_to(prog, ResPos::Stack(0));

                                prog.set_dbg(spos.clone());
                                prog.push_op(Op::MapSplice(
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0)));
                            }
                        }

                        ResPos::Stack(0)
                    })
                },
                Syntax::Func => {
                    let last_def_varname = ce.borrow().recent_var.clone();
                    let mut fun_spos = spos.clone();
                    fun_spos.name = Some(Rc::new(last_def_varname));

                    let mut func_ce = CompileEnv::create_env(Some(ce.clone()));
                    let ce_sub = func_ce.clone();

                    let label          = ast.at(1).unwrap();
                    let explicit_arity = ast.at(2).unwrap();

                    let mut func_prog = Prog::new();

                    let func_pw = vm_compile_stmts2(ast, 3, &mut func_ce)?;
                    func_pw.eval_to(&mut func_prog, ResPos::Value(ResValue::Ret));
                    func_prog.push_op(Op::End);

                    let func_prog = Rc::new(func_prog);

                    ce_sub.borrow_mut().explicit_arity.0 =
                        match explicit_arity.at(0).unwrap_or_else(|| VVal::Nul) {
                            VVal::Int(i) => ArityParam::Limit(i as usize),
                            VVal::Bol(true) => ArityParam::Limit(0),
                            _ => ArityParam::Undefined,
                        };

                    ce_sub.borrow_mut().explicit_arity.1 =
                        match explicit_arity.at(1).unwrap_or_else(|| VVal::Nul) {
                            VVal::Int(i) => ArityParam::Limit(i as usize),
                            VVal::Bol(true) => ArityParam::Infinite,
                            _ => ArityParam::Undefined,
                        };

                    let deciding_min_arity = if ce_sub.borrow().explicit_arity.0 != ArityParam::Undefined {
                        ce_sub.borrow().explicit_arity.0.clone()
                    } else {
                        ce_sub.borrow().implicit_arity.0.clone()
                    };

                    let deciding_max_arity = if ce_sub.borrow().explicit_arity.1 != ArityParam::Undefined {
                        ce_sub.borrow().explicit_arity.1.clone()
                    } else {
                        ce_sub.borrow().implicit_arity.1.clone()
                    };

                    let min_args : Option<usize> = match deciding_min_arity {
                        ArityParam::Infinite  => None,
                        ArityParam::Undefined => Some(0),
                        ArityParam::Limit(i)  => Some(i),
                    };

                    let max_args : Option<usize> = match deciding_max_arity {
                        ArityParam::Infinite  => None,
                        ArityParam::Undefined => Some(0),
                        ArityParam::Limit(i)  => Some(i),
                    };

                    let env_size = ce_sub.borrow().local_env_size();
                    let upvs     = ce_sub.borrow_mut().get_upval_pos();
                    let upvalues = vec![];

                    let fun_template =
                        VValFun::new_prog(
                            func_prog,
                            upvalues, env_size, min_args, max_args, false,
                            Some(fun_spos.clone()),
                            Rc::new(upvs),
                            label);

                    pw_needs_storage!(prog, store, {
                        let fp = prog.data_pos(fun_template.clone());
                        prog.push_op(Op::NewClos(fp, store))
                    })
                },
                Syntax::Call => {
                    if let Some((syntax, object, key)) =
                        fetch_object_key_access(&ast.at(1).unwrap()) {

                        let obj = vm_compile2(&object, ce)?;

                        let mut args = vec![];
                        for (e, _) in ast.iter().skip(2) {
                            args.push(e);
                        }

                        let mut compiled_args = vec![];
                        let mut argc = 0;
                        for e in args.iter() {
                            compiled_args.push(vm_compile2(&e, ce)?);
                            argc += 1;
                        }

                        match syntax {
                            Syntax::GetKey => {
                                let key = vm_compile2(&key, ce)?;
                                pw_store_if_needed!(prog, store, {
                                    for ca in compiled_args.iter() {
                                        ca.eval_to(prog, ResPos::Stack(0));
                                    }
                                    let obj_p = obj.eval(prog);
                                    let key_p = key.eval(prog);
                                    prog.push_op(
                                        Op::CallMethodKey(
                                            obj_p,
                                            key_p,
                                            argc as u16,
                                            store));
                                })
                            },
                            Syntax::GetSym => {
                                let key = key.s_raw();
                                pw_store_if_needed!(prog, store, {
                                    for ca in compiled_args.iter() {
                                        ca.eval_to(prog, ResPos::Stack(0));
                                    }
                                    let obj_p = obj.eval(prog);
                                    prog.push_op(
                                        Op::CallMethodSym(
                                            obj_p,
                                            Box::new(key.clone()),
                                            argc as u16,
                                            store));
                                })
                            },
                            _ => {
                                Err(ast.compile_err(
                                    format!("fetch_object_key_access failed: {}",
                                            ast.s())))
                            },
                        }
                    } else {
                        let symbol =
                            if let Syntax::Var = ast.at(1).unwrap_or_else(|| VVal::Nul).at(0).unwrap_or_else(|| VVal::Nul).get_syn() {
                                let var = ast.at(1).unwrap().at(1).unwrap();
                                Some(var.s_raw())
                            } else {
                                None
                            };

                        if let Some(sym) = symbol {
                            match &sym[..] {
                                "?"     => return vm_compile_if2(ast, ce),
                                "while" => return vm_compile_while2(ast, ce),
                                "iter"  => return vm_compile_iter2(ast, ce),
                                "next"  => return vm_compile_next2(ast, ce),
                                "break" => return vm_compile_break2(ast, ce),
                                _ => (),
                            }
                        }

                        let mut args = vec![];
                        for (e, _) in ast.iter().skip(1) {
                            args.push(e);
                        }

                        let mut compiled_args = vec![];
                        let mut argc = 0;
                        for e in args.iter() {
                            compiled_args.push(vm_compile2(&e, ce)?);
                            argc += 1;
                        }

                        pw_store_if_needed!(prog, store, {
                            for ca in compiled_args.iter() {
                                ca.eval_to(prog, ResPos::Stack(0));
                            }
                            prog.push_op(Op::Call(argc as u16 - 1, store));
                        })
                    }
                },
                Syntax::Err => {
                    let err_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;

                    pw_needs_storage!(prog, store, {
                        prog.set_dbg(spos.clone());
                        let err_val_pw = err_pw.eval(prog);
                        prog.set_dbg(spos.clone());
                        prog.push_op(Op::NewErr(err_val_pw, store));
                    })
                },
                Syntax::Key => {
                    let sym = ast.at(1).unwrap();
                    ce.borrow_mut().recent_sym = sym.s_raw();
                    pw_provides_result_pos!(prog, {
                        prog.set_dbg(spos.clone());
                        prog.data_pos(sym.clone())
                    })
                },
                Syntax::Str => {
                    let string = ast.at(1).unwrap();
                    ce.borrow_mut().recent_sym = string.s_raw();
                    pw_provides_result_pos!(prog, {
                        prog.set_dbg(spos.clone());
                        prog.data_pos(string.clone())
                    })
                },
                Syntax::IVec => {
                    let lc : Vec<ProgWriter> =
                        ast.map_skip(|e| { vm_compile2(e, ce) }, 1)?;

                    match lc.len() {
                        2 => {
                            pw_store_if_needed!(prog, store, {
                                let lc0p = lc[0].eval(prog);
                                let lc1p = lc[1].eval(prog);
                                prog.push_op(
                                    Op::NewNVec(
                                        Box::new(
                                            NVecPos::IVec2(lc0p, lc1p)),
                                        store));
                            })
                        },
                        3 => {
                            pw_store_if_needed!(prog, store, {
                                let lc0p = lc[0].eval(prog);
                                let lc1p = lc[1].eval(prog);
                                let lc2p = lc[2].eval(prog);
                                prog.push_op(
                                    Op::NewNVec(
                                        Box::new(
                                            NVecPos::IVec3(lc0p, lc1p, lc2p)),
                                        store));
                            })
                        },
                        4 => {
                            pw_store_if_needed!(prog, store, {
                                let lc0p = lc[0].eval(prog);
                                let lc1p = lc[1].eval(prog);
                                let lc2p = lc[2].eval(prog);
                                let lc3p = lc[3].eval(prog);
                                prog.push_op(
                                    Op::NewNVec(
                                        Box::new(
                                            NVecPos::IVec4(lc0p, lc1p, lc2p, lc3p)),
                                        store));
                            })
                        },
                        ecount => {
                            Err(ast.compile_err(
                                format!(
                                    "Can only create an IVector with 2, 3 or 4 elements, but got {}.", ecount)))
                        }
                    }
                },
                Syntax::FVec => {
                    let lc : Vec<ProgWriter> =
                        ast.map_skip(|e| { vm_compile2(e, ce) }, 1)?;

                    match lc.len() {
                        2 => {
                            pw_store_if_needed!(prog, store, {
                                let lc0p = lc[0].eval(prog);
                                let lc1p = lc[1].eval(prog);
                                prog.push_op(
                                    Op::NewNVec(
                                        Box::new(
                                            NVecPos::FVec2(lc0p, lc1p)),
                                        store));
                            })
                        },
                        3 => {
                            pw_store_if_needed!(prog, store, {
                                let lc0p = lc[0].eval(prog);
                                let lc1p = lc[1].eval(prog);
                                let lc2p = lc[2].eval(prog);
                                prog.push_op(
                                    Op::NewNVec(
                                        Box::new(
                                            NVecPos::FVec3(lc0p, lc1p, lc2p)),
                                        store));
                            })
                        },
                        4 => {
                            pw_store_if_needed!(prog, store, {
                                let lc0p = lc[0].eval(prog);
                                let lc1p = lc[1].eval(prog);
                                let lc2p = lc[2].eval(prog);
                                let lc3p = lc[3].eval(prog);
                                prog.push_op(
                                    Op::NewNVec(
                                        Box::new(
                                            NVecPos::FVec4(lc0p, lc1p, lc2p, lc3p)),
                                        store));
                            })
                        },
                        ecount => {
                            Err(ast.compile_err(
                                format!(
                                    "Can only create an FVector with 2, 3 or 4 elements, but got {}.", ecount)))
                        }
                    }
                },
                Syntax::SelfObj => {
                    pw_provides_result_pos!(prog,
                        { ResPos::Value(ResValue::SelfObj) })
                },
                Syntax::SelfData => {
                    pw_provides_result_pos!(prog,
                        { ResPos::Value(ResValue::SelfData) })
                },
                Syntax::Accum => {
                    match ast.at(1) {
                        Some(s) => {
                            if s.s_raw() == "@" {
                                pw_provides_result_pos!(prog, {
                                    ResPos::Value(ResValue::AccumVal)
                                })
                            } else {
                                let accum_type =
                                    match &s.s_raw()[..] {
                                        "string" => AccumType::String,
                                        "bytes"  => AccumType::Bytes,
                                        "float"  => AccumType::Float,
                                        "int"    => AccumType::Int,
                                        "map"    => AccumType::Map,
                                        "vec"    => AccumType::Vec,
                                        _ => {
                                            panic!("COMPILER ERROR: BAD ACCUM SYM");
                                        }
                                    };

                                let acc_pw =
                                    vm_compile2(&ast.at(2).unwrap(), ce)?;
                                pw_store_if_needed!(prog, store, {
                                    prog.push_op(Op::Accumulator(accum_type));
                                    acc_pw.eval_nul(prog);
                                    prog.push_op(
                                        Op::Mov(
                                            ResPos::Value(ResValue::AccumVal),
                                            store));
                                    prog.push_op(Op::Unwind);
                                })
                            }
                        },
                        None => {
                            pw_provides_result_pos!(prog, {
                                ResPos::Value(ResValue::AccumFun)
                            })
                        }
                    }
                },
                Syntax::GetIdx => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let idx = ast.at(2).unwrap().i() as u32;

                    pw_store_if_needed!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(Op::GetIdx(opos, idx, store));
                    })
                },
                Syntax::GetIdx2 => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let idx  = ast.at(2).unwrap().i() as u32;
                    let idx2 = ast.at(3).unwrap().i() as u32;
                    pw_store_if_needed!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetIdx2(opos, Box::new((idx, idx2)), store));
                    })
                },
                Syntax::GetIdx3 => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let idx  = ast.at(2).unwrap().i() as u32;
                    let idx2 = ast.at(3).unwrap().i() as u32;
                    let idx3 = ast.at(4).unwrap().i() as u32;
                    pw_store_if_needed!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetIdx3(
                                opos, Box::new((idx, idx2, idx3)), store));
                    })
                },
                Syntax::GetSym => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let sym = ast.at(2).unwrap().s_raw();
                    pw_store_if_needed!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetSym(opos, Box::new(sym.clone()), store));
                    })
                },
                Syntax::GetSym2 => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let sym  = ast.at(2).unwrap().s_raw();
                    let sym2 = ast.at(3).unwrap().s_raw();
                    pw_store_if_needed!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetSym2(
                                opos,
                                Box::new((
                                    sym.clone(),
                                    sym2.clone())),
                                store));
                    })
                },
                Syntax::GetSym3 => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let sym  = ast.at(2).unwrap().s_raw();
                    let sym2 = ast.at(3).unwrap().s_raw();
                    let sym3 = ast.at(4).unwrap().s_raw();
                    pw_store_if_needed!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetSym3(
                                opos,
                                Box::new((
                                    sym.clone(),
                                    sym2.clone(),
                                    sym3.clone())),
                                store));
                    })
                },
                Syntax::GetKey => {
                    let map_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let idx_pw = vm_compile2(&ast.at(2).unwrap(), ce)?;

                    pw_store_if_needed!(prog, store, {
                        let mp = map_pw.eval(prog);
                        let ip = idx_pw.eval(prog);
                        prog.push_op(Op::GetKey(mp, ip, store));
                    })
                },
                Syntax::SetKey => {
                    let map_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let sym_pw = vm_compile2(&ast.at(2).unwrap(), ce)?;

                    let recent_sym = ce.borrow().recent_sym.clone();
                    ce.borrow_mut().recent_var = recent_sym;

                    let val_pw = vm_compile2(&ast.at(3).unwrap(), ce)?;

                    pw_store_if_needed!(prog, store, {
                        let map = map_pw.eval(prog);
                        let sym = sym_pw.eval(prog);
                        let val = val_pw.eval(prog);
                        prog.push_op(Op::MapSetKey(val, sym, map, store));
                    })
                },
                Syntax::Or => {
                    let a = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let b = vm_compile2(&ast.at(2).unwrap(), ce)?;

                    pw_needs_storage!(prog, store, {
                        let mut aprog = Prog::new();
                        let ap = a.eval(&mut aprog);

                        let mut bprog = Prog::new();
                        let bp = b.eval(&mut bprog);

                        bprog.set_dbg(spos.clone());
                        bprog.push_op(Op::Mov(bp, store));
                        aprog.set_dbg(spos.clone());
                        aprog.push_op(Op::OrJmp(ap, bprog.op_count() as i32, store));
                        prog.append(aprog);
                        prog.append(bprog);
                    })
                },
                Syntax::And => {
                    let a = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let b = vm_compile2(&ast.at(2).unwrap(), ce)?;

                    pw_needs_storage!(prog, store, {
                        let mut aprog = Prog::new();
                        let ap = a.eval(&mut aprog);

                        let mut bprog = Prog::new();
                        let bp = b.eval(&mut bprog);

                        bprog.set_dbg(spos.clone());
                        bprog.push_op(Op::Mov(bp, store));
                        aprog.set_dbg(spos.clone());
                        aprog.push_op(Op::AndJmp(ap, bprog.op_count() as i32, store));
                        prog.append(aprog);
                        prog.append(bprog);
                    })
                },
//                Syntax::And => {
//                    let exprs : Vec<EvalNode> =
//                        ast.map_skip(|e| compile(e, ce), 1)?;
//
//                    Ok(Box::new(move |e: &mut Env| {
//                        let mut ret = VVal::Nul;
//                        for x in exprs.iter() {
//                            ret = x(e)?;
//                            if !ret.b() {
//                                return Ok(VVal::Bol(false));
//                            }
//                        }
//                        Ok(ret)
//                    }))
//                },
                Syntax::Apply => {
                    let call_argv_pw = vm_compile2(&ast.at(2).unwrap(), ce)?;
                    let func_pw      = vm_compile2(&ast.at(1).unwrap(), ce)?;

                    pw_store_if_needed!(prog, store, {
                        let f_rp    = func_pw.eval(prog);
                        let argv_rp = call_argv_pw.eval(prog);

                        prog.push_op(Op::Apply(argv_rp, f_rp, store));
                    })
                },
                Syntax::DumpVM => {
                    pw_null!(prog, {
                        prog.set_dbg(spos.clone());
                        prog.push_op(Op::Builtin(Builtin::DumpVM(spos.clone())));
                    })
                },
                Syntax::DumpStack => {
                    pw_null!(prog, {
                        prog.set_dbg(spos.clone());
                        prog.push_op(Op::Builtin(Builtin::DumpStack(spos.clone())));
                    })
                },
                Syntax::Export => {
                    let name = ast.at(1).unwrap();
                    let val_pw = vm_compile2(&ast.at(2).unwrap(), ce)?;

                    pw_null!(prog, {
                        let vp = val_pw.eval(prog);
                        prog.set_dbg(spos.clone());
                        prog.push_op(
                            Op::Builtin(
                                Builtin::Export(
                                    Box::new(name.s_raw()), vp)));
                    })
                },
                Syntax::Import => {
                    let prefix = ast.at(1).unwrap();
                    let name   = ast.at(2).unwrap();
                    let s_prefix = if prefix.is_none() { String::from("") }
                                   else { prefix.s_raw() + ":" };

                    let glob_ref = ce.borrow_mut().global.clone();
                    if glob_ref.borrow_mut().import_module_as(
                         &name.s_raw(), &prefix.s_raw())
                    {
                        return pw_null!(prog, { });
                    }

                    let resolver : Option<Rc<RefCell<dyn ModuleResolver>>> =
                        glob_ref.borrow_mut().resolver.clone();

                    let path : Vec<String> =
                        (&name.s_raw())
                            .split(':')
                            .map(String::from)
                            .collect();

                    let import_file_path = if spos.file.s() == "?" { None } else { Some(spos.file.s()) };

                    if let Some(resolver) = resolver {

                        let r = resolver.borrow();
                        let exports = r.resolve(glob_ref.clone(), &path, import_file_path);
                        match exports {
                            Err(ModuleLoadError::NoSuchModule(p)) => {
                                Err(ast.compile_err(
                                    format!("Couldn't find module '{}' in paths: {}", name.s_raw(), p)))
                            },
                            Err(ModuleLoadError::ModuleEvalError(e)) => {
                                Err(ast.compile_err(
                                    format!("Error on evaluating module '{}': {}", name.s_raw(), e)))
                            },
                            Err(ModuleLoadError::Other(s)) => {
                                Err(ast.compile_err(
                                    format!("Error on resolving module '{}': {}", name.s_raw(), s)))
                            },
                            Ok(symtbl) => {
                                glob_ref.borrow_mut().import_from_symtbl(
                                    &s_prefix, symtbl);

                                pw_null!(prog, { })
                            },
                        }
                    } else {
                        Err(ast.compile_err(
                            format!("Couldn't resolve module '{}'", name.s_raw())))
                    }
                },
                _ => { Err(ast.compile_err(format!("bad input: {}", ast.s()))) },
            }
        },
        VVal::Pair(bx) => {
            let a = vm_compile2(&bx.0, ce)?;
            let b = vm_compile2(&bx.1, ce)?;

            pw_store_if_needed!(prog, store, {
                let ar = a.eval(prog);
                let br = b.eval(prog);
                prog.push_op(Op::NewPair(br, ar, store));
            })
        },
        _ => {
            let ast = ast.clone();
            pw_provides_result_pos!(prog, { prog.data_pos(ast.clone()) })
        },
    }
}

pub fn compile_vm_fun(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<EvalNode, CompileError>
{
    let prog = vm_compile_stmts2(&ast, 1, ce)?;

    let mut p = Prog::new();
    prog.eval_to(&mut p, ResPos::Value(ResValue::Ret));
    p.op_end();

    Ok(Box::new(move |e: &mut Env| { vm(&p, e) }))
}

pub fn gen(s: &str) -> String {
    let global = GlobalEnv::new_default();
    match parser::parse(s, "<compiler:s_eval>") {
        Ok(ast) => {
            let mut ce = CompileEnv::new(global.clone());

            match vm_compile2(&ast, &mut ce) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_data() {
        assert_eq!(gen("10"),      "10");
        assert_eq!(gen("\"foo\""), "\"foo\"");
        assert_eq!(gen(":foo"),    ":\"foo\"");
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
        assert_eq!(gen("!x = 10; { x; 20 }"), "&F{@[1,10:<compiler:s_eval>(Func)@x],amin=0,amax=0,locals=0,upvalues=$[$n]}");
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
        assert_eq!(gen("$b\"abc\" 1"),                                "$b\"b\"", "Get single byte from bytes");
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
