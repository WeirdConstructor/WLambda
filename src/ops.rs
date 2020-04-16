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
                Op::S(OpS::IterNext(p1)) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::R(OpR::ToRef(p1, _), p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::R(_, p1) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::A(OpA::OrJmp(_, p1), p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::A(OpA::AndJmp(_, p1), p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::A(_, p1) => {
                    patch_respos_data(p1, self_data_next_idx);
                },
                Op::AR(_, p1, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::ABR(_, p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::ABCR(_, bx, p4) => {
                    patch_respos_data(&mut bx.as_mut().0, self_data_next_idx);
                    patch_respos_data(&mut bx.as_mut().1, self_data_next_idx);
                    patch_respos_data(&mut bx.as_mut().2, self_data_next_idx);
                    patch_respos_data(p4, self_data_next_idx);
                },
                Op::ABCDR(_, bx, p5) => {
                    patch_respos_data(&mut bx.as_mut().0, self_data_next_idx);
                    patch_respos_data(&mut bx.as_mut().1, self_data_next_idx);
                    patch_respos_data(&mut bx.as_mut().2, self_data_next_idx);
                    patch_respos_data(&mut bx.as_mut().3, self_data_next_idx);
                    patch_respos_data(p5, self_data_next_idx);
                },
                Op::S(_) => (),
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

    pub fn op_end(&mut self) -> &mut Self {
        self.push_op(Op::S(OpS::End));
        self
    }

    pub fn op_mov(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::Mov, a, r));
    }

    pub fn op_and_jmp(&mut self, sp: &SynPos, a: ResPos, jmp: i32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::AndJmp(jmp, r), a));
    }

    pub fn op_or_jmp(&mut self, sp: &SynPos, a: ResPos, jmp: i32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::OrJmp(jmp, r), a));
    }

    pub fn op_destr(&mut self, sp: &SynPos, a: ResPos, destr_info: DestructureInfo) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::Destr(Box::new(destr_info)), a));
    }

    pub fn op_to_ref(&mut self, sp: &SynPos, a: ResPos, r: ResPos, typ: ToRefType) {
        self.set_dbg(sp.clone());
        self.push_op(Op::R(OpR::ToRef(a, typ), r));
    }

    pub fn op_new_list(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::R(OpR::NewList, r));
    }

    pub fn op_list_splice(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::ListSplice, a, b, r));
    }

    pub fn op_list_push(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::ListPush, a, b, r));
    }

    pub fn op_new_map(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::R(OpR::NewMap, r));
    }

    pub fn op_map_set_key(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABCR(OpABCR::MapSetKey, Box::new((a, b, c)), r));
    }

    pub fn op_map_splice(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::MapSplice, a, b, r));
    }

    pub fn op_get_key(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::GetKey, a, b, r));
    }

    pub fn op_get_idx(&mut self, sp: &SynPos, a: ResPos, i: u32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::GetIdx(i), a, r));
    }

    pub fn op_get_idx2(&mut self, sp: &SynPos, a: ResPos, i: u32, i2: u32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::GetIdx2(Box::new((i, i2))), a, r));
    }

    pub fn op_get_idx3(&mut self, sp: &SynPos, a: ResPos, i: u32, i2: u32, i3: u32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::GetIdx3(Box::new((i, i2, i3))), a, r));
    }

    pub fn op_get_sym(&mut self, sp: &SynPos, a: ResPos, s: String, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::GetSym(Box::new(s)), a, r));
    }

    pub fn op_get_sym2(&mut self, sp: &SynPos, a: ResPos, s: String, s2: String, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::GetSym2(Box::new((s, s2))), a, r));
    }

    pub fn op_get_sym3(&mut self, sp: &SynPos, a: ResPos, s: String, s2: String, s3: String, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::GetSym3(Box::new((s, s2, s3))), a, r));
    }

    pub fn op_new_err(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::NewErr, a, r));
    }

    pub fn op_argv(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::R(OpR::Argv, r));
    }

    pub fn op_unwind(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::Unwind));
    }

    pub fn op_binop(&mut self, sp: &SynPos, op: BinOp, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(op.to_op(), a, b, r));
    }

    pub fn op_new_opt(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::NewOpt, a, r));
    }

    pub fn op_new_none_opt(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::R(OpR::NewNoneOpt, r));
    }

    pub fn op_new_clos(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::NewClos, a, r));
    }

    pub fn op_call_method_key(&mut self, sp: &SynPos, a: ResPos, b: ResPos, argc: u16, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::CallMethodKey(argc), a, b, r));
    }

    pub fn op_call_method_sym(&mut self, sp: &SynPos, a: ResPos, sym: String, argc: u16, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AR(OpAR::CallMethodSym(Box::new(sym), argc), a, r));
    }

    pub fn op_call(&mut self, sp: &SynPos, argc: u16, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::R(OpR::Call(argc), r));
    }

    pub fn op_new_ivec2(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::NewIVec, a, b, r));
    }

    pub fn op_new_ivec3(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABCR(OpABCR::NewIVec, Box::new((a, b, c)), r));
    }

    pub fn op_new_ivec4(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, d: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABCDR(OpABCDR::NewIVec, Box::new((a, b, c, d)), r));
    }

    pub fn op_new_fvec2(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::NewFVec, a, b, r));
    }

    pub fn op_new_fvec3(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABCR(OpABCR::NewFVec, Box::new((a, b, c)), r));
    }

    pub fn op_new_fvec4(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, d: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABCDR(OpABCDR::NewFVec, Box::new((a, b, c, d)), r));
    }

    pub fn op_clear_locals(&mut self, sp: &SynPos, from: u16, to: u16) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::ClearLocals(from, to)));
    }

    pub fn op_ctrl_flow_break(&mut self, sp: &SynPos, a: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::Break, a));
    }

    pub fn op_ctrl_flow_next(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::Next));
    }

    pub fn op_jmp(&mut self, sp: &SynPos, offs: i32) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::Jmp(offs)));
    }

    pub fn op_jmp_ifn(&mut self, sp: &SynPos, a: ResPos, offs: i32) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::JmpIfN(offs), a));
    }

    pub fn op_push_loop_info(&mut self, sp: &SynPos, break_offs: u16) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::PushLoopInfo(break_offs)));
    }

    pub fn op_iter_init(&mut self, sp: &SynPos, a: ResPos, end_offs: i32) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::IterInit(end_offs), a));
    }

    pub fn op_iter_next(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::IterNext(r)));
    }

    pub fn op_apply(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::Apply, a, b, r));
    }

    pub fn op_dump_vm(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::DumpVM(Box::new(sp.clone()))));
    }

    pub fn op_dump_stack(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::DumpStack(Box::new(sp.clone()))));
    }

    pub fn op_export(&mut self, sp: &SynPos, a: ResPos, name: String) {
        self.set_dbg(sp.clone());
        self.push_op(Op::A(OpA::Export(Box::new(name)), a));
    }

    pub fn op_new_pair(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ABR(OpABR::NewPair, a, b, r));
    }

    pub fn op_accumulator(&mut self, sp: &SynPos, at: AccumType) {
        self.set_dbg(sp.clone());
        self.push_op(Op::S(OpS::Accumulator(at)));
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

#[derive(Debug, Clone, Copy)]
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
    pub fn to_op(&self) -> OpABR {
        match self {
            BinOp::Add => OpABR::Add,
            BinOp::Sub => OpABR::Sub,
            BinOp::Mul => OpABR::Mul,
            BinOp::Div => OpABR::Div,
            BinOp::Mod => OpABR::Mod,
            BinOp::Le  => OpABR::Le,
            BinOp::Lt  => OpABR::Lt,
            BinOp::Ge  => OpABR::Ge,
            BinOp::Gt  => OpABR::Gt,
            BinOp::Eq  => OpABR::Eq,
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

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum ToRefType {
    CaptureRef,
    ToRef,
    Deref,
    Weakable,
}

//#[derive(Debug,Clone)]
//#[repr(u8)]
//pub enum Builtin {
//    Export(Box<String>, ResPos),
//    DumpStack(SynPos),
//    DumpVM(SynPos),
//}
//
//#[derive(Debug,Clone)]
//#[repr(u8)]
//pub enum CtrlFlow {
//    Next,
//    Break(ResPos),
//}
//
//#[derive(Debug,Clone)]
//#[repr(u8)]
//pub enum NVecPos {
//    IVec2(ResPos, ResPos),
//    IVec3(ResPos, ResPos, ResPos),
//    IVec4(ResPos, ResPos, ResPos, ResPos),
//    FVec2(ResPos, ResPos),
//    FVec3(ResPos, ResPos, ResPos),
//    FVec4(ResPos, ResPos, ResPos, ResPos),
//}

//#[derive(Debug,Clone)]
//#[repr(u8)]
//pub enum Op {
//    Mov(ResPos, ResPos),
//    NewOpt(ResPos, ResPos),
//    NewPair(ResPos, ResPos, ResPos),
//    NewNVec(Box<NVecPos>, ResPos),
//    Argv(ResPos),
//    ToRef(ResPos, ResPos, ToRefType),
//    ClearLocals(u16, u16),
//    Accumulator(AccumType),
//    PushLoopInfo(u16),
//    Add(ResPos, ResPos, ResPos),
//    Sub(ResPos, ResPos, ResPos),
//    Mul(ResPos, ResPos, ResPos),
//    Div(ResPos, ResPos, ResPos),
//    Mod(ResPos, ResPos, ResPos),
//    Le(ResPos, ResPos, ResPos),
//    Lt(ResPos, ResPos, ResPos),
//    Ge(ResPos, ResPos, ResPos),
//    Gt(ResPos, ResPos, ResPos),
//    Eq(ResPos, ResPos, ResPos),
//    NewErr(ResPos, ResPos),
//    NewList(ResPos),
//    ListPush(ResPos, ResPos, ResPos),
//    ListSplice(ResPos, ResPos, ResPos),
//    NewMap(ResPos),
//    MapSetKey(ResPos, ResPos, ResPos, ResPos),
//    MapSplice(ResPos, ResPos, ResPos),
//    NewClos(ResPos, ResPos),
//    GetIdx(ResPos, u32, ResPos),
//    GetIdx2(ResPos, Box<(u32, u32)>, ResPos),
//    GetIdx3(ResPos, Box<(u32, u32, u32)>, ResPos),
//    GetSym(ResPos, Box<String>, ResPos),
//    GetSym2(ResPos, Box<(String, String)>, ResPos),
//    GetSym3(ResPos, Box<(String, String, String)>, ResPos),
//    GetKey(ResPos, ResPos, ResPos),
//    Destr(ResPos, Box<DestructureInfo>),
//    Call(u16, ResPos),
//    CallMethodKey(ResPos, ResPos, u16, ResPos),
//    CallMethodSym(ResPos, Box<String>, u16, ResPos),
//    Apply(ResPos, ResPos, ResPos),
//    Jmp(i32),
//    JmpIfN(ResPos, i32),
//    OrJmp(ResPos, i32, ResPos),
//    AndJmp(ResPos, i32, ResPos),
//    CtrlFlow(CtrlFlow),
//    Builtin(Builtin),
//    IterInit(ResPos, i32),
//    IterNext(ResPos),
////    UnwindMov(ResPos, ResPos),
//    Unwind,
//    End,
//}

#[derive(Debug,Clone,Copy)]
pub enum OpR {
    Argv,
    Call(u16),
    NewList,
    NewMap,
    NewNoneOpt,
    ToRef(ResPos, ToRefType),
}

#[derive(Debug,Clone)]
pub enum OpA {
    Destr(Box<DestructureInfo>),
    JmpIfN(i32),
    Break,
    Export(Box<String>),
    IterInit(i32),
    OrJmp(i32, ResPos),
    AndJmp(i32, ResPos),
}

#[derive(Debug,Clone)]
pub enum OpAR {
    Mov,
    NewOpt,
    NewErr,
    NewClos,
    GetIdx(u32),
    GetIdx2(Box<(u32, u32)>),
    GetIdx3(Box<(u32, u32, u32)>),
    GetSym(Box<String>),
    GetSym2(Box<(String, String)>),
    GetSym3(Box<(String, String, String)>),
    CallMethodSym(Box<String>, u16),
}

#[derive(Debug,Clone,Copy)]
pub enum OpABR {
    NewPair,
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
    ListPush,
    ListSplice,
    MapSplice,
    GetKey,
    CallMethodKey(u16),
    Apply,
    NewIVec,
    NewFVec,
}

#[derive(Debug,Clone,Copy)]
pub enum OpABCR {
    MapSetKey,
    NewIVec,
    NewFVec,
}

#[derive(Debug,Clone,Copy)]
pub enum OpABCDR {
    NewIVec,
    NewFVec,
}

#[derive(Debug,Clone)]
pub enum OpS {
    ClearLocals(u16, u16),
    Accumulator(AccumType),
    PushLoopInfo(u16),
    Jmp(i32),
    DumpStack(Box<SynPos>),
    DumpVM(Box<SynPos>),
    IterNext(ResPos),
    Next,
    Unwind,
    End,
}

#[derive(Debug,Clone)]
pub enum Op {
    S(OpS),
    R(OpR, ResPos),
    A(OpA, ResPos),
    AR(OpAR, ResPos, ResPos),
    ABR(OpABR, ResPos, ResPos, ResPos),
    ABCR(OpABCR, Box<(ResPos, ResPos, ResPos)>, ResPos),
    ABCDR(OpABCDR, Box<(ResPos, ResPos, ResPos, ResPos)>, ResPos),
}
