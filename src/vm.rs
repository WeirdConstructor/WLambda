use crate::parser::{self};
use crate::compiler::*;
use crate::vval::*;

use std::rc::Rc;
use std::cell::RefCell;

const DEBUG_VM: bool = true;

#[derive(Clone)]
pub struct Prog {
    debug:   std::vec::Vec<Option<SynPos>>,
    data:    std::vec::Vec<VVal>,
    ops:     std::vec::Vec<Op>,
    nxt_debug: Option<SynPos>,
}

pub type ProgWriteNode = Box<dyn Fn(&mut Prog, Option<ResPos>) -> ResPos>;

pub struct ProgWriter {
    node:  ProgWriteNode,
}

impl ProgWriter {
    pub fn eval_to(&self, prog: &mut Prog, rp: ResPos) {
        (*self.node)(prog, Some(rp));
    }

    fn eval_nul(&self, prog: &mut Prog) {
        let rp = (*self.node)(prog, None);
        if let ResPos::Stack(_) = rp {
            prog.push_op(Op::Mov(rp, ResPos::Value(ResValue::Nul)));
        }
    }

    fn eval(&self, prog: &mut Prog) -> ResPos {
        (*self.node)(prog, None)
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
        pw_respos_or_mov!($prog, {
            $b
            ResPos::Value(ResValue::Nul)
        })
    }
}

macro_rules! pw_respos_or_mov {
    ($prog: ident, $b: block) => {
        pw!($prog, store, {
            let pos = $b;
            if let Some(store) = store {
                $prog.push_op(Op::Mov(pos, store));
                store
            } else {
                pos
            }
        })
    }
}

macro_rules! pw_store_or_stack {
    ($prog: ident, $pos: ident, $b: block) => {
        pw!($prog, store, {
            let $pos = if let Some(store) = store {
                store
            } else {
                ResPos::Stack(0)
            };
            $b;
            $pos
        })
    }
}

fn patch_respos_data(rp: &mut ResPos, idx: u16) {
    match rp {
        ResPos::Data(i)         => { *i = *i + idx; },
        ResPos::Global(i)       => { *i = *i + idx; },
        ResPos::GlobalRef(i)    => { *i = *i + idx; },
        ResPos::Local(_)
        | ResPos::LocalRef(_)
        | ResPos::Up(_)
        | ResPos::UpRef(_)
        | ResPos::Arg(_)
        | ResPos::Stack(_)
        | ResPos::Value(_)
            => (),
    }
}

impl Prog {
    fn append(&mut self, mut prog: Prog) {
        let self_data_next_idx : u16 = self.data.len() as u16;

        for o in prog.ops.iter_mut() {
            match o {
                Op::Add(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Sub(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Mul(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Div(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Mod(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Le(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Lt(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Ge(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Gt(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Eq(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::NewPair(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Mov(p1, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::ToRef(p1, p2, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::NewErr(p1, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::NewClos(p1, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::ListPush(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::ListSplice(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::MapSetKey(p1, p2, p3, p4) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                    patch_respos_data(p4, self_data_next_idx);
                },
                Op::MapSplice(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::JmpIf(p1, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::JmpIfN(p1, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::OrJmp(p1, _, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::AndJmp(p1, _, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::GetIdx(p1, _, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::GetIdx2(p1, _, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::GetIdx3(p1, _, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::GetSym(p1, _, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::GetSym2(p1, _, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::GetSym3(p1, _, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::GetKey(p1, p2, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::Destr(p1, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::Call(_, p1) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::Apply(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Builtin(Builtin::Export(_, p1)) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::NewMap(p1) => { patch_respos_data(p1, self_data_next_idx); },
                Op::NewList(p1) => { patch_respos_data(p1, self_data_next_idx); },
                Op::Argv(p1) => { patch_respos_data(p1, self_data_next_idx); },
                Op::End
                | Op::Builtin(Builtin::DumpStack(_))
                | Op::Unwind
                | Op::Accumulator(_)
                | Op::Jmp(_)
                | Op::ClearLocals(_, _)
                    => (),
            }
        }

        self.debug.append(&mut prog.debug);
        self.data.append(&mut prog.data);
        self.ops.append(&mut prog.ops);
    }

    fn op_count(&self) -> usize { self.ops.len() }

    pub fn new() -> Self {
        Self {
            data:       vec![],
            ops:        vec![],
            debug:      vec![],
            nxt_debug:  None,
        }
    }

    pub fn op_end(&mut self) -> &mut Self { self.push_op(Op::End); self }

    fn global_ref_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::GlobalRef((self.data.len() - 1) as u16)
    }

    fn global_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::Global((self.data.len() - 1) as u16)
    }

    fn data_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::Data((self.data.len() - 1) as u16)
    }

//    fn unshift_op(&mut self, o: Op) -> &mut Self {
//        self.ops.insert(0, o);
//        self.debug.insert(0, std::mem::replace(&mut self.nxt_debug, None));
//        self
//    }

    fn push_op(&mut self, o: Op) -> &mut Self {
        self.ops.push(o);
        self.debug.push(std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    fn set_dbg(&mut self, sp: SynPos) -> &mut Self {
        self.nxt_debug = Some(sp);
        self
    }

    fn dump(&self) {
        println!("PROG:");
        for (i, o) in self.ops.iter().enumerate() {
            println!("    [{:>3}] {:?}", i, o);
        }
        println!("  DATA:");
        for (i, o) in self.data.iter().enumerate() {
            println!("    [{:>3}] {:?}", i, o);
        }
    }
}

fn set_env_at_varpos(e: &mut Env, pos: &VarPos, v: &VVal) {
    match pos {
        VarPos::UpValue(d)           => e.set_up(*d, v.clone()),
        VarPos::Local(d)             => e.set_consume(*d, v.clone()),
        VarPos::Global(VVal::Ref(r)) => { r.replace(v.clone()); },
        VarPos::Global(_)
        | VarPos::Const(_)
        | VarPos::NoPos
            => panic!(format!(
                "Fatal error in WLambda, can't destructure to {:?}", pos)),
    }
}

fn set_ref_at_varpos(e: &mut Env, pos: &VarPos, rv: &VVal) {
    let v = rv.clone();
    match pos {
        VarPos::UpValue(d)           => e.assign_ref_up(*d, v),
        VarPos::Local(d)             => e.assign_ref_local(*d, v),
        VarPos::Global(VVal::Ref(r)) => { r.borrow().set_ref(v); },
        VarPos::Global(_)
        | VarPos::Const(_)
        | VarPos::NoPos
            => panic!(format!(
                "Fatal error in WLambda, can't ref destructure to {:?}", pos)),
    }
}


#[derive(Debug, Clone)]
pub struct DestructureInfo {
    vars:   VVal,
    poses:  std::vec::Vec<VarPos>,
    is_ref: bool,
}

impl DestructureInfo {
    fn destructure(&self, env: &mut Env, val: VVal) {
        let nul = VVal::Nul;
        match val {
            VVal::Lst(l) => {
                for (i, pos) in self.poses.iter().enumerate() {
                    if self.is_ref {
                        set_ref_at_varpos(env, pos, l.borrow().get(i).unwrap_or(&nul));
                    } else {
                        set_env_at_varpos(env, pos, l.borrow().get(i).unwrap_or(&nul));
                    }
                }
            },
            VVal::Map(m) => {
                for (i, pos) in self.poses.iter().enumerate() {
                    let vname = self.vars.at(i).unwrap().s_raw();
                    let val = m.borrow().get(&vname).cloned().unwrap_or_else(|| VVal::Nul);

                    if self.is_ref {
                        set_ref_at_varpos(env, pos, &val);
                    } else {
                        set_env_at_varpos(env, pos, &val);
                    }
                }
            },
            VVal::Pair(p) => {
                let (lv, rv) = *p;

                if let Some(pos) = self.poses.get(0) {
                    if self.is_ref {
                        set_ref_at_varpos(env, pos, &lv);
                    } else {
                        set_env_at_varpos(env, pos, &lv);
                    }
                }

                if let Some(pos) = self.poses.get(1) {
                    if self.is_ref {
                        set_ref_at_varpos(env, pos, &rv);
                    } else {
                        set_env_at_varpos(env, pos, &rv);
                    }
                }
            },
            _ => {
                for pos in self.poses.iter() {
                    if self.is_ref {
                        set_ref_at_varpos(env, pos, &val);
                    } else {
                        set_env_at_varpos(env, pos, &val);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
}

impl BinOp {
    fn to_op(&self, a: ResPos, b: ResPos, out: ResPos) -> Op {
        match self {
            BinOp::Add => Op::Add(a, b, out),
            BinOp::Sub => Op::Sub(a, b, out),
            BinOp::Mul => Op::Mul(a, b, out),
            BinOp::Div => Op::Div(a, b, out),
            BinOp::Mod => Op::Mod(a, b, out),
            BinOp::Le  => Op::Le(a, b, out),
            BinOp::Lt  => Op::Lt(a, b, out),
            BinOp::Ge  => Op::Ge(a, b, out),
            BinOp::Gt  => Op::Gt(a, b, out),
            BinOp::Eq  => Op::Eq(a, b, out),
        }
    }
}

#[derive(Debug,Clone,Copy)]
#[repr(u8)]
pub enum AccumType {
    String,
    Bytes,
    Float,
    Int,
    Map,
    Vec,
}

#[derive(Debug,Clone)]
#[repr(u8)]
pub enum ToRefType {
    CaptureRef,
    ToRef,
    Deref,
    Weakable,
}

#[derive(Debug,Clone)]
#[repr(u8)]
pub enum Builtin {
    Export(Box<String>, ResPos),
    DumpStack(SynPos),
}

#[derive(Debug,Clone)]
#[repr(u8)]
pub enum Op {
    Mov(ResPos, ResPos),
    NewPair(ResPos, ResPos, ResPos),
    Argv(ResPos),
    ToRef(ResPos, ResPos, ToRefType),
    ClearLocals(u16, u16),
    Accumulator(AccumType),
    Add(ResPos, ResPos, ResPos),
    Sub(ResPos, ResPos, ResPos),
    Mul(ResPos, ResPos, ResPos),
    Div(ResPos, ResPos, ResPos),
    Mod(ResPos, ResPos, ResPos),
    Le(ResPos, ResPos, ResPos),
    Lt(ResPos, ResPos, ResPos),
    Ge(ResPos, ResPos, ResPos),
    Gt(ResPos, ResPos, ResPos),
    Eq(ResPos, ResPos, ResPos),
    NewErr(ResPos, ResPos),
    NewList(ResPos),
    ListPush(ResPos, ResPos, ResPos),
    ListSplice(ResPos, ResPos, ResPos),
    NewMap(ResPos),
    MapSetKey(ResPos, ResPos, ResPos, ResPos),
    MapSplice(ResPos, ResPos, ResPos),
    NewClos(ResPos, ResPos),
    GetIdx(ResPos, u32, ResPos),
    GetIdx2(ResPos, Box<(u32, u32)>, ResPos),
    GetIdx3(ResPos, Box<(u32, u32, u32)>, ResPos),
    GetSym(ResPos, Box<String>, ResPos),
    GetSym2(ResPos, Box<(String, String)>, ResPos),
    GetSym3(ResPos, Box<(String, String, String)>, ResPos),
    GetKey(ResPos, ResPos, ResPos),
    Destr(ResPos, Box<DestructureInfo>),
    Call(u16, ResPos),
    Apply(ResPos, ResPos, ResPos),
    Jmp(i32),
    JmpIf(ResPos, i32),
    JmpIfN(ResPos, i32),
    OrJmp(ResPos, i32, ResPos),
    AndJmp(ResPos, i32, ResPos),
    Builtin(Builtin),
    Unwind,
    End,
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

    loop {
        let op = &prog.ops[pc];
        if DEBUG_VM {
            let syn =
                if let Some(sp) = &prog.debug[pc] { format!("{}", sp) }
                else { "".to_string() };
            println!("OP[{:<2} {:>3}]: {:<15}      | sp: {:>3}, bp: {:>3} | {}",
                     env.vm_nest, pc, format!("{:?}", op), env.sp, env.bp, syn);
        }

        match op {
            Op::Mov(a, r) => op_a_r!(env, ret, prog, a, r, { a }),
            Op::NewPair(a, b, r) => op_a_b_r!(env, ret, prog, a, b, r, {
                VVal::Pair(Box::new((b, a)))
            }),
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
            Op::ClearLocals(from, to) => env.null_locals(*from as usize, *to as usize),
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
                } else {
                    match (a, b) {
                        (VVal::IVec(ln), re) => VVal::IVec(ln + re.nvec()),
                        (VVal::FVec(ln), re) => VVal::FVec(ln + re.nvec()),
                        (VVal::Flt(f), re)   => VVal::Flt(f + re.f()),
                        (le, re)             => VVal::Int(le.i().wrapping_add(re.i()))
                    }
                }
            }),
            Op::Sub(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Flt(f - b.f()) }
                else { VVal::Int(a.i().wrapping_sub(b.i())) }
            }),
            Op::Div(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a {
                    VVal::Flt(f / b.f())

                } else if b.i() == 0 {
                    env.unwind_to_depth(uw_depth);
                    retv =
                        Err(StackAction::panic_str(
                            format!("Division by 0: {}/{}", a.i(), b.i()),
                            prog.debug[pc].clone()));
                    break;

                } else {
                    VVal::Int(a.i().wrapping_div(b.i()))
                }
            }),
            Op::Mul(b, a, r) => op_a_b_r!(env, ret, prog, b, a, r, {
                if let VVal::Flt(f) = a { VVal::Flt(f * b.f()) }
                else { VVal::Int(a.i().wrapping_mul(b.i())) }
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
                let res =
                    match k {
                        VVal::Int(i)  => o.at(i as usize).unwrap_or_else(|| VVal::Nul),
                        VVal::Bol(b)  => o.at(b as usize).unwrap_or_else(|| VVal::Nul),
                        VVal::Sym(sy) => o.get_key(&sy.borrow()).unwrap_or_else(|| VVal::Nul),
                        VVal::Str(sy) => o.get_key(&sy.borrow()).unwrap_or_else(|| VVal::Nul),
                        _ => {
                            env.push(o.clone());
                            let call_ret = k.call_internal(env, 1);
                            env.pop();
                            match call_ret {
                                Ok(v) => v,
                                Err(sa) => {
                                    env.unwind_to_depth(uw_depth);
                                    retv =
                                        Err(sa.wrap_panic(prog.debug[pc].clone()));
                                    break;
                                },
                            }
                        }
                    };
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
            Op::Call(argc, r) => {
                let argc = *argc as usize;
                let f = env.stk(argc + 1).clone();
                let call_ret = f.call_internal(env, argc);
                env.popn(argc + 1); // + 1 for the function
                match call_ret {
                    Ok(v) => {
                        out_reg!(env, ret, prog, r, v);
                    },
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
                } else if *jmp_offs == 0 {
                    out_reg!(env, ret, prog, r, a);
                }
            },
            Op::AndJmp(a, jmp_offs, r) => {
                in_reg!(env, ret, prog, a);
                if !a.b() {
                    pc = (pc as i32 + *jmp_offs) as usize;
                    out_reg!(env, ret, prog, r, a);
                } else if *jmp_offs == 0 {
                    out_reg!(env, ret, prog, r, a);
                }
            },
            Op::Builtin(b) => {
                match b {
                    Builtin::DumpStack(spos) => {
                        println!("DUMPSTACK@{}", spos);
                        env.dump_stack();
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

        let val_pw = vm_compile2(&value, ce)?;

        if is_global {
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
            ce.borrow_mut().def_local(&varname, next_local);

            pw_null!(prog, {
                prog.set_dbg(spos.clone());
                val_pw.eval_to(prog, ResPos::Local(next_local as u16));
            })
        }
    }
}

pub fn pw_arg(arg_idx: usize, to_ref: bool) -> Result<ProgWriter, CompileError> {
    pw!(prog, store, {
        let rp = ResPos::Arg(arg_idx as u16);
        if let Some(store) = store {
            prog.push_op(Op::Mov(rp, store));
            if to_ref { prog.push_op(Op::ToRef(store, store, ToRefType::CaptureRef)); }
            store
        } else {
            if to_ref {
                let store = ResPos::Stack(0);
                prog.push_op(Op::ToRef(rp, store, ToRefType::CaptureRef));
                store
            } else {
                rp
            }
        }
    })
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
                pw_store_or_stack!(prog, store, {
                    prog.set_dbg(spos.clone());
                    prog.push_op(Op::Argv(store));
                    if capt_ref {
                        prog.push_op(Op::ToRef(store, store, ToRefType::CaptureRef));
                    }
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
                pw_respos_or_mov!(prog, {
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

            (*val_pw.node)(prog, Some(rp));
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
                if let Some(store) = store {
                    e.eval_to(prog, store);
                    res = store;
                } else {
                    res = e.eval(prog);
                }
            } else {
                e.eval_nul(prog);
            }

            i += 1;
        }

        res
    })
}


fn vm_compile_block2(ast: &VVal, skip_cnt: usize, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<ProgWriter, CompileError> {
    let syn  = ast.at(0).unwrap_or_else(|| VVal::Nul);
    let spos = syn.get_syn_pos();

    ce.borrow_mut().push_block_env();
    let stmts = vm_compile_stmts2(ast, skip_cnt, ce)?;
    let (from_local_idx, to_local_idx) = ce.borrow_mut().pop_block_env();

    pw!(prog, store, {
        prog.set_dbg(spos.clone());
        let res = (*stmts.node)(prog, store);
        if from_local_idx != to_local_idx {
            prog.set_dbg(spos.clone());
            prog.push_op(Op::ClearLocals(
                from_local_idx as u16,
                to_local_idx   as u16));
        }
        res
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

    pw_store_or_stack!(prog, store, {
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
                    pw_store_or_stack!(prog, store, {
                        let ref_rp = ref_pw.eval(prog);
                        prog.push_op(Op::ToRef(ref_rp, store, ToRefType::ToRef));
                    })
                },
                Syntax::WRef => {
                    let ref_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    pw_store_or_stack!(prog, store, {
                        let ref_rp = ref_pw.eval(prog);
                        prog.push_op(Op::ToRef(ref_rp, store, ToRefType::Weakable));
                    })
                },
                Syntax::Deref => {
                    let ref_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    pw_store_or_stack!(prog, store, {
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

                    pw_respos_or_mov!(prog, {
                        prog.push_op(Op::NewList(ResPos::Stack(0)));

                        for (is_splice, pw) in pws.iter() {
                            pw.eval_to(prog, ResPos::Stack(0));
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

                    pw_respos_or_mov!(prog, {
                        prog.push_op(Op::NewMap(ResPos::Stack(0)));

                        for (kc_pw, vc_pw) in pws.iter() {
                            if let Some(vc_pw) = vc_pw {
                                kc_pw.eval_to(prog, ResPos::Stack(0));
                                vc_pw.eval_to(prog, ResPos::Stack(0));

                                prog.push_op(Op::MapSetKey(
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0),
                                    ResPos::Stack(0)));
                            } else {
                                kc_pw.eval_to(prog, ResPos::Stack(0));

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

                    let spos_inner = fun_spos.clone();
                    let fun_ref = Rc::new(RefCell::new(move |env: &mut Env, _argc: usize| {
                        let res = vm(&func_prog, env);
                        match res {
                            Ok(v)  => Ok(v),
                            Err(StackAction::Return((v_lbl, v))) => {
                                return
                                    if v_lbl.eqv(&label) { Ok(v) }
                                    else { Err(StackAction::Return((v_lbl, v))) }
                            },
                            Err(e) => { return Err(e.wrap_panic(Some(spos_inner.clone()))) }
                        }
                    }));

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
                        VValFun::new_val(
                            fun_ref.clone(),
                            upvalues, env_size, min_args, max_args, false,
                            Some(fun_spos.clone()),
                            Rc::new(upvs));

                    pw_store_or_stack!(prog, store, {
                        let fp = prog.data_pos(fun_template.clone());
                        prog.push_op(Op::NewClos(fp, store))
                    })
                },
                Syntax::Call => {
                    let is_while =
                        if let Syntax::Var = ast.at(1).unwrap_or_else(|| VVal::Nul).at(0).unwrap_or_else(|| VVal::Nul).get_syn() {
                            let var = ast.at(1).unwrap().at(1).unwrap();
                            var.with_s_ref(|var_s: &str| var_s == "while")
                        } else {
                            false
                        };

                    if is_while {
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

                            let cond_op_count1 = prog.op_count();
                            let cond_val = cond.eval(prog);

                            prog.push_op(
                                Op::JmpIfN(cond_val, body_op_count as i32 + 1));

                            let cond_offs =
                                body_op_count + (prog.op_count() - cond_op_count1);
                            body_prog.push_op(Op::Jmp(-(cond_offs as i32 + 1)));
                            prog.append(body_prog);
                        });
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

                    pw_store_or_stack!(prog, store, {
                        for ca in compiled_args.iter() {
                            ca.eval_to(prog, ResPos::Stack(0));
                        }
                        prog.push_op(Op::Call(argc as u16 - 1, store));
                    })
                },
                Syntax::Err => {
                    let err_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;

                    pw_store_or_stack!(prog, store, {
                        prog.set_dbg(spos.clone());
                        let err_val_pw = err_pw.eval(prog);
                        prog.set_dbg(spos.clone());
                        prog.push_op(Op::NewErr(err_val_pw, store));
                    })
                },
                Syntax::Key => {
                    let sym = ast.at(1).unwrap();
                    ce.borrow_mut().recent_sym = sym.s_raw();
                    pw_respos_or_mov!(prog, {
                        prog.set_dbg(spos.clone());
                        prog.data_pos(sym.clone())
                    })
                },
                Syntax::Str => {
                    let string = ast.at(1).unwrap();
                    ce.borrow_mut().recent_sym = string.s_raw();
                    pw_respos_or_mov!(prog, {
                        prog.set_dbg(spos.clone());
                        prog.data_pos(string.clone())
                    })
                },
                Syntax::SelfObj => {
                    pw_respos_or_mov!(prog,
                        { ResPos::Value(ResValue::SelfObj) })
                },
                Syntax::SelfData => {
                    pw_respos_or_mov!(prog,
                        { ResPos::Value(ResValue::SelfData) })
                },
                Syntax::Accum => {
                    match ast.at(1) {
                        Some(s) => {
                            if s.s_raw() == "@" {
                                pw_respos_or_mov!(prog, {
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
                                pw_store_or_stack!(prog, store, {
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
                            pw_respos_or_mov!(prog, {
                                ResPos::Value(ResValue::AccumFun)
                            })
                        }
                    }
                },
                Syntax::GetIdx => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let idx = ast.at(2).unwrap().i() as u32;

                    pw_store_or_stack!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(Op::GetIdx(opos, idx, store));
                    })
                },
                Syntax::GetIdx2 => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let idx  = ast.at(2).unwrap().i() as u32;
                    let idx2 = ast.at(3).unwrap().i() as u32;
                    pw_store_or_stack!(prog, store, {
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
                    pw_store_or_stack!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetIdx3(
                                opos, Box::new((idx, idx2, idx3)), store));
                    })
                },
                Syntax::GetSym => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let sym = ast.at(2).unwrap().s_raw();
                    pw_store_or_stack!(prog, store, {
                        let opos = o_pw.eval(prog);
                        prog.push_op(
                            Op::GetSym(opos, Box::new(sym.clone()), store));
                    })
                },
                Syntax::GetSym2 => {
                    let o_pw = vm_compile2(&ast.at(1).unwrap(), ce)?;
                    let sym  = ast.at(2).unwrap().s_raw();
                    let sym2 = ast.at(3).unwrap().s_raw();
                    pw_store_or_stack!(prog, store, {
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
                    pw_store_or_stack!(prog, store, {
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

                    pw_store_or_stack!(prog, store, {
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

                    pw_store_or_stack!(prog, store, {
                        let map = map_pw.eval(prog);
                        let sym = sym_pw.eval(prog);
                        let val = val_pw.eval(prog);
                        prog.push_op(Op::MapSetKey(val, sym, map, store));
                    })
                },
                Syntax::Or => {
                    let mut exprs : Vec<ProgWriter> =
                        ast.map_skip(|e| vm_compile2(e, ce), 1)?;

                    let mut exs : Vec<(Prog, ResPos, usize)> = vec![];
                    for e in exprs.iter() {
                        let mut p = Prog::new();
                        let rp = e.eval(&mut p);
                        println!("PUSH");
                        p.dump();
                        exs.push((p, rp, 0));
                    }

                    let mut jmp_offs = 0;
                    for e in exs.iter_mut().rev() {
                        jmp_offs += e.0.op_count() + 1;
                        e.2 += jmp_offs - 1;
                    }
                    pw_store_or_stack!(prog, store, {
                        for e in exs.iter() {
                            let mut p = e.0.clone();
                            p.push_op(Op::OrJmp(e.1, e.2 as i32, store));
                            prog.append(p);
                        }
                    })

//                    pw_store_or_stack!(prog, store, {
//                        VVal::Nul
//                    })
//                    Ok(Box::new(move |e: &mut Env| {
//                        for x in exprs.iter() {
//                            let ret = x(e)?;
//                            if ret.b() {
//                                return Ok(ret);
//                            }
//                        }
//                        Ok(VVal::Bol(false))
//                    }))
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

                    pw_store_or_stack!(prog, store, {
                        let f_rp    = func_pw.eval(prog);
                        let argv_rp = call_argv_pw.eval(prog);

                        prog.push_op(Op::Apply(argv_rp, f_rp, store));
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

            pw_store_or_stack!(prog, pos, {
                let ar = (*a.node)(prog, None);
                let br = (*b.node)(prog, None);
                prog.push_op(Op::NewPair(br, ar, pos));
            })
        },
        _ => {
            let ast = ast.clone();
            pw_respos_or_mov!(prog, { prog.data_pos(ast.clone()) })
        },
    }
}

pub fn compile_vm_fun(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>)
    -> Result<EvalNode, CompileError>
{
    let mut ce = CompileEnv::create_env(Some(ce.clone()));
    let ce_sub = ce.clone();
    let prog = vm_compile2(&ast, &mut ce)?;
    let local_space = ce_sub.borrow().get_local_space();

    let mut p = Prog::new();
    prog.eval_to(&mut p, ResPos::Value(ResValue::Ret));
    p.op_end();

    Ok(Box::new(move |e: &mut Env| {
        e.push_sp(local_space);
        vm(&p, e)
    }))
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
