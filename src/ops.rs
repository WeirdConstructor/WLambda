// Copyright (c) 2020-2022 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
Implements the VM program and ops data structures for WLambda.
*/

use crate::vval::*;
use crate::compiler::*;
use crate::nvec::NVec;
use crate::str_int::*;
use std::fmt;
use std::fmt::{Debug};
use std::rc::Rc;

#[derive(Clone, Default)]
pub struct Prog {
    pub debug:     std::vec::Vec<Option<SynPos>>,
    pub data:      std::vec::Vec<VVal>,
    pub(crate) ops:       std::vec::Vec<Op>,
    pub nxt_debug: Option<SynPos>,
}

fn patch_respos_data(rp: &mut ResPos, idx: u16) {
    match rp {
        ResPos::Data(i)         => { *i += idx; },
        ResPos::Global(i)       => { *i += idx; },
        ResPos::GlobalRef(i)    => { *i += idx; },
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
    pub(crate) fn append(&mut self, mut prog: Prog) {
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
                Op::SNOr(p1, p2, p3, _) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::NewPair(p1, p2, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::NewOpt(p1, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                },
                Op::NewIter(p1, p2) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
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
                Op::JmpTbl(p1, _) => {
                    patch_respos_data(p1, self_data_next_idx);
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
                Op::CallDirect(fun) => {
                    patch_respos_data(
                        &mut Rc::get_mut(fun).expect("only rc").arg,
                        self_data_next_idx);
                    patch_respos_data(
                        &mut Rc::get_mut(fun).expect("only rc").res,
                        self_data_next_idx);
                },
                Op::CallMethodKey(p1, p2, _, p3) => {
                    patch_respos_data(p1, self_data_next_idx);
                    patch_respos_data(p2, self_data_next_idx);
                    patch_respos_data(p3, self_data_next_idx);
                },
                Op::CallMethodSym(p1, _, p2) => {
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

    pub(crate) fn op_count(&self) -> usize { self.ops.len() }

    pub(crate) fn new() -> Self {
        Self {
            data:       vec![],
            ops:        vec![],
            debug:      vec![],
            nxt_debug:  None,
        }
    }

    pub(crate) fn global_ref_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::GlobalRef((self.data.len() - 1) as u16)
    }

    pub(crate) fn global_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::Global((self.data.len() - 1) as u16)
    }

    pub(crate) fn data_pos(&mut self, data: VVal) -> ResPos {
        self.data.push(data);
        ResPos::Data((self.data.len() - 1) as u16)
    }

//    fn unshift_op(&mut self, o: Op) -> &mut Self {
//        self.ops.insert(0, o);
//        self.debug.insert(0, std::mem::replace(&mut self.nxt_debug, None));
//        self
//    }

    pub(crate) fn push_op(&mut self, o: Op) -> &mut Self {
        self.ops.push(o);
        self.debug.push(std::mem::replace(&mut self.nxt_debug, None));
        self
    }

    pub(crate) fn set_dbg(&mut self, sp: SynPos) -> &mut Self {
        self.nxt_debug = Some(sp);
        self
    }

    pub(crate) fn op_end(&mut self) -> &mut Self {
        self.push_op(Op::End);
        self
    }

    pub(crate) fn op_mov(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Mov(a, r));
    }

    pub(crate) fn op_and_jmp(&mut self, sp: &SynPos, a: ResPos, jmp: i32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::AndJmp(a, jmp, r));
    }

    pub(crate) fn op_or_jmp(&mut self, sp: &SynPos, a: ResPos, jmp: i32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::OrJmp(a, jmp, r));
    }

    pub(crate) fn op_jmp_tbl(&mut self, sp: &SynPos, a: ResPos, tbl: Vec<i32>) {
        self.set_dbg(sp.clone());
        self.push_op(Op::JmpTbl(a, Box::new(tbl)));
    }

    pub(crate) fn op_destr(&mut self, sp: &SynPos, a: ResPos, destr_info: DestructureInfo) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Destr(a, Box::new(destr_info)));
    }

    pub(crate) fn op_to_ref(&mut self, sp: &SynPos, a: ResPos, r: ResPos, typ: ToRefType) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ToRef(a, r, typ));
    }

    pub(crate) fn op_new_list(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewList(r));
    }

    pub(crate) fn op_list_splice(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ListSplice(a, b, r));
    }

    pub(crate) fn op_list_push(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ListPush(a, b, r));
    }

    pub(crate) fn op_new_map(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewMap(r));
    }

    pub(crate) fn op_map_set_key(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::MapSetKey(a, b, c, r));
    }

    pub(crate) fn op_map_splice(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::MapSplice(a, b, r));
    }

    pub(crate) fn op_get_key(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetKey(a, b, r));
    }

    pub(crate) fn op_get_idx(&mut self, sp: &SynPos, a: ResPos, i: u32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetIdx(a, i, r));
    }

    pub(crate) fn op_get_idx2(&mut self, sp: &SynPos, a: ResPos, i: u32, i2: u32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetIdx2(a, Box::new((i, i2)), r));
    }

    pub(crate) fn op_get_idx3(&mut self, sp: &SynPos, a: ResPos, i: u32, i2: u32, i3: u32, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetIdx3(a, Box::new((i, i2, i3)), r));
    }

    pub(crate) fn op_get_sym(&mut self, sp: &SynPos, a: ResPos, s: Symbol, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetSym(a, Box::new(s), r));
    }

    pub(crate) fn op_get_sym2(&mut self, sp: &SynPos, a: ResPos, s: Symbol, s2: Symbol, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetSym2(a, Box::new((s, s2)), r));
    }

    pub(crate) fn op_get_sym3(&mut self, sp: &SynPos, a: ResPos, s: Symbol, s2: Symbol, s3: Symbol, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::GetSym3(a, Box::new((s, s2, s3)), r));
    }

    pub(crate) fn op_new_err(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewErr(a, r));
    }

    pub(crate) fn op_argv(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Argv(r));
    }

    pub(crate) fn op_unwind(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Unwind);
    }

    pub(crate) fn op_binop(&mut self, sp: &SynPos, op: BinOp, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(op.to_op(a, b, r));
    }

    pub(crate) fn op_new_opt(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewOpt(a, r));
    }

    pub(crate) fn op_new_iter(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewIter(a, r));
    }

    pub(crate) fn op_new_clos(&mut self, sp: &SynPos, a: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewClos(a, r));
    }

    pub(crate) fn op_call_method_key(&mut self, sp: &SynPos, a: ResPos, b: ResPos, argc: u16, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::CallMethodKey(a, b, argc, r));
    }

    pub(crate) fn op_call_method_sym(&mut self, sp: &SynPos, a: ResPos, sym: String, argc: u16, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::CallMethodSym(a, Box::new((sym, argc)), r));
    }

    pub(crate) fn op_call_direct(&mut self, sp: &SynPos, a: ResPos, mut fun: DirectFun, r: ResPos) {
        self.set_dbg(sp.clone());
        fun.arg = a;
        fun.res = r;
        self.push_op(Op::CallDirect(Rc::new(fun)));
    }

    pub(crate) fn op_call(&mut self, sp: &SynPos, argc: u16, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Call(argc, r));
    }

    pub(crate) fn op_new_ivec2(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewNVec(Box::new(NVecPos::IVec2(a, b)), r));
    }

    pub(crate) fn op_new_ivec3(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewNVec(Box::new(NVecPos::IVec3(a, b, c)), r));
    }

    #[allow(clippy::many_single_char_names)]
    pub(crate) fn op_new_ivec4(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, d: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewNVec(Box::new(NVecPos::IVec4(a, b, c, d)), r));
    }

    pub(crate) fn op_new_fvec2(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewNVec(Box::new(NVecPos::FVec2(a, b)), r));
    }

    pub(crate) fn op_new_fvec3(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewNVec(Box::new(NVecPos::FVec3(a, b, c)), r));
    }

    #[allow(clippy::many_single_char_names)]
    pub(crate) fn op_new_fvec4(&mut self, sp: &SynPos, a: ResPos, b: ResPos, c: ResPos, d: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewNVec(Box::new(NVecPos::FVec4(a, b, c, d)), r));
    }

    pub(crate) fn op_clear_locals(&mut self, sp: &SynPos, from: u16, to: u16) {
        self.set_dbg(sp.clone());
        self.push_op(Op::ClearLocals(from, to));
    }

    pub(crate) fn op_ctrl_flow_break(&mut self, sp: &SynPos, a: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::CtrlFlow(CtrlFlow::Break(a)));
    }

    pub(crate) fn op_ctrl_flow_next(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::CtrlFlow(CtrlFlow::Next));
    }

    pub(crate) fn op_jmp(&mut self, sp: &SynPos, offs: i32) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Jmp(offs));
    }

    pub(crate) fn op_jmp_ifn(&mut self, sp: &SynPos, a: ResPos, offs: i32) {
        self.set_dbg(sp.clone());
        self.push_op(Op::JmpIfN(a, offs));
    }

    pub(crate) fn op_push_loop_info(&mut self, sp: &SynPos, break_offs: u16) {
        self.set_dbg(sp.clone());
        self.push_op(Op::PushLoopInfo(break_offs));
    }

    pub(crate) fn op_iter_init(&mut self, sp: &SynPos, a: ResPos, end_offs: i32) {
        self.set_dbg(sp.clone());
        self.push_op(Op::IterInit(a, end_offs));
    }

    pub(crate) fn op_iter_next(&mut self, sp: &SynPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::IterNext(r));
    }

    pub(crate) fn op_apply(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Apply(a, b, r));
    }

    pub(crate) fn op_dump_vm(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Builtin(Builtin::DumpVM(Box::new(sp.clone()))));
    }

    pub(crate) fn op_dump_stack(&mut self, sp: &SynPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Builtin(Builtin::DumpStack(Box::new(sp.clone()))));
    }

    pub(crate) fn op_export(&mut self, sp: &SynPos, a: ResPos, name: String) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Builtin(Builtin::Export(Box::new(name), a)));
    }

    pub(crate) fn op_new_pair(&mut self, sp: &SynPos, a: ResPos, b: ResPos, r: ResPos) {
        self.set_dbg(sp.clone());
        self.push_op(Op::NewPair(a, b, r));
    }

    pub(crate) fn op_accumulator(&mut self, sp: &SynPos, at: AccumType) {
        self.set_dbg(sp.clone());
        self.push_op(Op::Accumulator(at));
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

impl Debug for Prog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "PROG:")?;
        for (i, o) in self.ops.iter().enumerate() {
            writeln!(f, "    [{:>3}] {:?}", i, o)?;
        }
        writeln!(f, "  DATA:")?;
        for (i, o) in self.data.iter().enumerate() {
            writeln!(f, "    [{:>3}] {:?}", i, o)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DestructureInfo {
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
            => panic!(
                "Fatal error in WLambda, can't destructure to {:?}", pos),
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
            => panic!(
                "Fatal error in WLambda, can't ref destructure to {:?}", pos),
    }
}

macro_rules! set_at_varpos {
    ($self: ident, $env: ident, $pos: ident, $v: expr) => {
        if $self.is_ref {
            set_ref_at_varpos($env, $pos, $v);
        } else {
            set_env_at_varpos($env, $pos, $v);
        }
    }
}

impl DestructureInfo {
    #[allow(clippy::cognitive_complexity)]
    pub fn destructure(&self, env: &mut Env, val: VVal) {
        match val {
            VVal::Lst(l) => {
                let nul = VVal::None;
                for (i, pos) in self.poses.iter().enumerate() {
                    set_at_varpos!(
                        self, env, pos, l.borrow().get(i).unwrap_or(&nul));
                }
            },
            VVal::Map(m) => {
                for (i, pos) in self.poses.iter().enumerate() {
                    let sym = self.vars.at(i).unwrap().to_sym();
                    let val = m.borrow().get(&sym).cloned().unwrap_or_else(|| VVal::None);

                    set_at_varpos!(self, env, pos, &val);
                }
            },
            VVal::Pair(p) => {
                let (lv, rv) = &*p;

                if let Some(pos) = self.poses.get(0) {
                    set_at_varpos!(self, env, pos, &lv);
                }

                if let Some(pos) = self.poses.get(1) {
                    set_at_varpos!(self, env, pos, &rv);
                }
            },
            VVal::IVec(vb) => {
                match vb.as_ref() {
                    NVec::Vec2(a, b) => {
                        if let Some(pos) = self.poses.get(0) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*a));
                        }

                        if let Some(pos) = self.poses.get(1) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*b));
                        }
                    },
                    NVec::Vec3(a, b, c) => {
                        if let Some(pos) = self.poses.get(0) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*a));
                        }

                        if let Some(pos) = self.poses.get(1) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*b));
                        }

                        if let Some(pos) = self.poses.get(2) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*c));
                        }
                    },
                    NVec::Vec4(a, b, c, d) => {
                        if let Some(pos) = self.poses.get(0) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*a));
                        }

                        if let Some(pos) = self.poses.get(1) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*b));
                        }

                        if let Some(pos) = self.poses.get(2) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*c));
                        }

                        if let Some(pos) = self.poses.get(3) {
                            set_at_varpos!(self, env, pos, &VVal::Int(*d));
                        }
                    },
                }
            },
            VVal::FVec(vb) => {
                match vb.as_ref() {
                    NVec::Vec2(a, b) => {
                        if let Some(pos) = self.poses.get(0) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*a));
                        }

                        if let Some(pos) = self.poses.get(1) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*b));
                        }
                    },
                    NVec::Vec3(a, b, c) => {
                        if let Some(pos) = self.poses.get(0) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*a));
                        }

                        if let Some(pos) = self.poses.get(1) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*b));
                        }

                        if let Some(pos) = self.poses.get(2) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*c));
                        }
                    },
                    NVec::Vec4(a, b, c, d) => {
                        if let Some(pos) = self.poses.get(0) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*a));
                        }

                        if let Some(pos) = self.poses.get(1) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*b));
                        }

                        if let Some(pos) = self.poses.get(2) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*c));
                        }

                        if let Some(pos) = self.poses.get(3) {
                            set_at_varpos!(self, env, pos, &VVal::Flt(*d));
                        }
                    },
                }
            },
            _ => {
                for pos in self.poses.iter() {
                    set_at_varpos!(self, env, pos, &val);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BinOp {
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
    SomeOr,
    ExtSomeOr,
    NoneOr,
    ErrOr,
    OptOr,
}

impl BinOp {
    pub(crate) fn to_op(self, a: ResPos, b: ResPos, out: ResPos) -> Op {
        match self {
            BinOp::Add       => Op::Add(a, b, out),
            BinOp::Sub       => Op::Sub(a, b, out),
            BinOp::Mul       => Op::Mul(a, b, out),
            BinOp::Div       => Op::Div(a, b, out),
            BinOp::Mod       => Op::Mod(a, b, out),
            BinOp::Le        => Op::Le(a, b, out),
            BinOp::Lt        => Op::Lt(a, b, out),
            BinOp::Ge        => Op::Ge(a, b, out),
            BinOp::Gt        => Op::Gt(a, b, out),
            BinOp::Eq        => Op::Eq(a, b, out),
            BinOp::NoneOr    => Op::SNOr(a, b, out, 0),
            BinOp::SomeOr    => Op::SNOr(a, b, out, 1),
            BinOp::ErrOr     => Op::SNOr(a, b, out, 2),
            BinOp::OptOr     => Op::SNOr(a, b, out, 3),
            BinOp::ExtSomeOr => Op::SNOr(a, b, out, 4),
        }
    }
}

#[derive(Debug,Clone,Copy)]
pub(crate) enum AccumType {
    String,
    Bytes,
    Float,
    Int,
    Map,
    Vec,
}

#[derive(Debug,Clone)]
pub(crate) enum ToRefType {
    CaptureRef,
    ToRef,
    Deref,
    Hidden,
    Weak,
}

#[derive(Debug,Clone)]
pub(crate) enum Builtin {
    Export(Box<String>, ResPos),
    DumpStack(Box<SynPos>),
    DumpVM(Box<SynPos>),
}

#[derive(Debug,Clone)]
pub(crate) enum CtrlFlow {
    Next,
    Break(ResPos),
}

#[derive(Debug,Clone)]
pub(crate) enum NVecPos {
    IVec2(ResPos, ResPos),
    IVec3(ResPos, ResPos, ResPos),
    IVec4(ResPos, ResPos, ResPos, ResPos),
    FVec2(ResPos, ResPos),
    FVec3(ResPos, ResPos, ResPos),
    FVec4(ResPos, ResPos, ResPos, ResPos),
}

#[derive(Clone)]
pub(crate) struct DirectFun {
    pub arg: ResPos,
    pub res: ResPos,
    pub fun: Rc<dyn Fn(VVal, &mut Env) -> VVal>,
}

impl DirectFun {
    pub fn new(fun: Rc<dyn Fn(VVal, &mut Env) -> VVal>) -> Self {
        Self {
            arg: ResPos::Stack(0),
            res: ResPos::Stack(0),
            fun,
        }
    }
}

impl Debug for DirectFun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Box<DirectFun:?;arg={:?},res={:?}>", self.arg, self.res)
    }
}

#[allow(clippy::box_collection)]
#[derive(Debug,Clone)]
pub(crate) enum Op {
    Mov(ResPos, ResPos),
    NewOpt(ResPos, ResPos),
    NewIter(ResPos, ResPos),
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
    SNOr(ResPos, ResPos, ResPos, u8),
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
    GetSym(ResPos, Box<Symbol>, ResPos),
    GetSym2(ResPos, Box<(Symbol, Symbol)>, ResPos),
    GetSym3(ResPos, Box<(Symbol, Symbol, Symbol)>, ResPos),
    GetKey(ResPos, ResPos, ResPos),
    Destr(ResPos, Box<DestructureInfo>),
    Call(u16, ResPos),
    CallDirect(Rc<DirectFun>),
    CallMethodKey(ResPos, ResPos, u16, ResPos),
    CallMethodSym(ResPos, Box<(String, u16)>, ResPos),
    Apply(ResPos, ResPos, ResPos),
    Jmp(i32),
    JmpIfN(ResPos, i32),
    OrJmp(ResPos, i32, ResPos),
    AndJmp(ResPos, i32, ResPos),
    JmpTbl(ResPos, Box<Vec<i32>>),
    CtrlFlow(CtrlFlow),
    Builtin(Builtin),
    IterInit(ResPos, i32),
    IterNext(ResPos),
//    UnwindMov(ResPos, ResPos),
    Unwind,
    End,
}
