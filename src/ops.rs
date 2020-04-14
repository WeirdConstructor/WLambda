use crate::vval::*;
use crate::compiler::*;

#[derive(Clone)]
pub struct Prog {
    pub debug:   std::vec::Vec<Option<SynPos>>,
    pub data:    std::vec::Vec<VVal>,
    pub ops:     std::vec::Vec<Op>,
    pub nxt_debug: Option<SynPos>,
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
    pub fn append(&mut self, mut prog: Prog) {
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
                Op::NewNVec(vec, p1) => {
                    use std::borrow::BorrowMut;
                    match vec.borrow_mut() {
                        NVecPos::IVec2(a, b) => {
                            patch_respos_data(a, self_data_next_idx);
                            patch_respos_data(b, self_data_next_idx);
                        },
                        NVecPos::IVec3(a, b, c) => {
                            patch_respos_data(a, self_data_next_idx);
                            patch_respos_data(b, self_data_next_idx);
                            patch_respos_data(c, self_data_next_idx);
                        },
                        NVecPos::IVec4(a, b, c, d) => {
                            patch_respos_data(a, self_data_next_idx);
                            patch_respos_data(b, self_data_next_idx);
                            patch_respos_data(c, self_data_next_idx);
                            patch_respos_data(d, self_data_next_idx);
                        },
                        NVecPos::FVec2(a, b) => {
                            patch_respos_data(a, self_data_next_idx);
                            patch_respos_data(b, self_data_next_idx);
                        },
                        NVecPos::FVec3(a, b, c) => {
                            patch_respos_data(a, self_data_next_idx);
                            patch_respos_data(b, self_data_next_idx);
                            patch_respos_data(c, self_data_next_idx);
                        },
                        NVecPos::FVec4(a, b, c, d) => {
                            patch_respos_data(a, self_data_next_idx);
                            patch_respos_data(b, self_data_next_idx);
                            patch_respos_data(c, self_data_next_idx);
                            patch_respos_data(d, self_data_next_idx);
                        },
                    }
                    patch_respos_data(p1, self_data_next_idx);
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
                Op::CallMethodKey(p1, p2, _, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::CallMethodSym(p1, _, _, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::Apply(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::Builtin(Builtin::Export(_, p1)) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::CtrlFlow(CtrlFlow::Break(p1)) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::IterInit(p1, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::IterNext(p1) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
//                Op::UnwindMov(p1, p2) => {
//                    patch_respos_data(p1, self_data_next_idx);
//                    patch_respos_data(p2, self_data_next_idx);
//                },
                Op::NewMap(p1)    => { patch_respos_data(p1, self_data_next_idx); },
                Op::NewList(p1)   => { patch_respos_data(p1, self_data_next_idx); },
                Op::Argv(p1)      => { patch_respos_data(p1, self_data_next_idx); },
                Op::End
                | Op::Builtin(Builtin::DumpStack(_))
                | Op::Builtin(Builtin::DumpVM(_))
                | Op::CtrlFlow(CtrlFlow::Next)
                | Op::Unwind
                | Op::Accumulator(_)
                | Op::PushLoopInfo(_)
                | Op::Jmp(_)
                | Op::ClearLocals(_, _)
                    => (),
            }
        }

        self.debug.append(&mut prog.debug);
        self.data.append(&mut prog.data);
        self.ops.append(&mut prog.ops);
    }

    pub fn op_count(&self) -> usize { self.ops.len() }

    pub fn new() -> Self {
        Self {
            data:       vec![],
            ops:        vec![],
            debug:      vec![],
            nxt_debug:  None,
        }
    }

    pub fn op_end(&mut self) -> &mut Self { self.push_op(Op::End); self }

    pub fn global_ref_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::GlobalRef((self.data.len() - 1) as u16)
    }

    pub fn global_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::Global((self.data.len() - 1) as u16)
    }

    pub fn data_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::Data((self.data.len() - 1) as u16)
    }

//    fn unshift_op(&mut self, o: Op) -> &mut Self {
//        self.ops.insert(0, o);
//        self.debug.insert(0, std::mem::replace(&mut self.nxt_debug, None));
//        self
//    }

    pub fn push_op(&mut self, o: Op) -> &mut Self {
        self.ops.push(o);
        self.debug.push(std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    pub fn set_dbg(&mut self, sp: SynPos) -> &mut Self {
        self.nxt_debug = Some(sp);
        self
    }

    pub fn dump(&self) {
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

#[derive(Debug, Clone)]
pub struct DestructureInfo {
    pub vars:   VVal,
    pub poses:  std::vec::Vec<VarPos>,
    pub is_ref: bool,
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

impl DestructureInfo {
    pub fn destructure(&self, env: &mut Env, val: VVal) {
        match val {
            VVal::Lst(l) => {
                let nul = VVal::None;
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
                    let val = m.borrow().get(&vname).cloned().unwrap_or_else(|| VVal::None);

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
pub enum BinOp {
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
    pub fn to_op(&self, a: ResPos, b: ResPos, out: ResPos) -> Op {
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
    DumpVM(SynPos),
}

#[derive(Debug,Clone)]
#[repr(u8)]
pub enum CtrlFlow {
    Next,
    Break(ResPos),
}

#[derive(Debug,Clone)]
#[repr(u8)]
pub enum NVecPos {
    IVec2(ResPos, ResPos),
    IVec3(ResPos, ResPos, ResPos),
    IVec4(ResPos, ResPos, ResPos, ResPos),
    FVec2(ResPos, ResPos),
    FVec3(ResPos, ResPos, ResPos),
    FVec4(ResPos, ResPos, ResPos, ResPos),
}

#[derive(Debug,Clone)]
#[repr(u8)]
pub enum Op {
    Mov(ResPos, ResPos),
    NewPair(ResPos, ResPos, ResPos),
    NewNVec(Box<NVecPos>, ResPos),
    Argv(ResPos),
    ToRef(ResPos, ResPos, ToRefType),
    ClearLocals(u16, u16),
    Accumulator(AccumType),
    PushLoopInfo(u16),
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
    CallMethodKey(ResPos, ResPos, u16, ResPos),
    CallMethodSym(ResPos, Box<String>, u16, ResPos),
    Apply(ResPos, ResPos, ResPos),
    Jmp(i32),
    JmpIf(ResPos, i32),
    JmpIfN(ResPos, i32),
    OrJmp(ResPos, i32, ResPos),
    AndJmp(ResPos, i32, ResPos),
    CtrlFlow(CtrlFlow),
    Builtin(Builtin),
    IterInit(ResPos, i32),
    IterNext(ResPos),
//    UnwindMov(ResPos, ResPos),
    Unwind,
    End,
}
