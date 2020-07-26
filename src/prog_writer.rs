use crate::vval::SynPos;
use crate::ops::*;
use crate::compiler::ResPos;
use crate::compiler::ResValue;

#[derive(Debug, Clone)]
pub(crate) enum ResultSink {
    WriteTo(ResPos),
    WantResult,
    Null,
}

impl ResultSink {
    pub(crate) fn if_null<T>(&self, f: T) -> bool
        where T: FnOnce(ResPos)
    {
        match self {
            ResultSink::WriteTo(_) => true,
            ResultSink::WantResult => true,
            ResultSink::Null => {
                let rp = ResPos::Value(ResValue::None);
                f(rp);
                false
            },
        }
    }

    pub(crate) fn if_must_store<T>(&self, f: T) -> ResPos
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
            ResultSink::Null => ResPos::Value(ResValue::None),
        }
    }
}

pub(crate) type ProgWriteNode = Box<dyn Fn(&mut Prog, ResultSink) -> ResPos>;

pub(crate) struct ProgWriter {
    node:  ProgWriteNode,
}

impl ProgWriter {
    pub(crate) fn eval_to(&self, prog: &mut Prog, rp: ResPos) {
        (*self.node)(prog, ResultSink::WriteTo(rp));
    }

    pub(crate) fn eval_proxy(&self, prog: &mut Prog, rs: ResultSink) -> ResPos {
        (*self.node)(prog, rs)
    }

    pub(crate) fn eval_nul(&self, prog: &mut Prog) {
        let rp = (*self.node)(prog, ResultSink::Null);
        if let ResPos::Stack(_) = rp {
            prog.op_mov(&SynPos::empty(), rp, ResPos::Value(ResValue::None));
        }
    }

    pub(crate) fn eval(&self, prog: &mut Prog) -> ResPos {
        (*self.node)(prog, ResultSink::WantResult)
    }
}

pub(crate) fn pw(f: ProgWriteNode) -> ProgWriter {
    ProgWriter {
        node:   Box::new(f),
    }
}

#[macro_export]
macro_rules! pw {
    ($prog: ident, $store: ident, $b: block) => {
        Ok(pw(Box::new(move |$prog, $store| {
            $b
        })))
    }
}

#[macro_export]
macro_rules! pw_null {
    ($prog: ident, $b: block) => {
        pw_provides_result_pos!($prog, {
            $b
            ResPos::Value(ResValue::None)
        })
    }
}

#[macro_export]
macro_rules! pw_provides_result_pos {
    ($prog: ident, $b: block) => {
        pw!($prog, store, {
            let pos = $b;
            match store {
                ResultSink::WriteTo(store_pos) => {
                    $prog.op_mov(&SynPos::empty(), pos, store_pos);
                    store_pos
                },
                ResultSink::WantResult => {
                    pos
                },
                ResultSink::Null => {
                    if let ResPos::Stack(_) = pos {
                        $prog.op_mov(&SynPos::empty(), pos, ResPos::Value(ResValue::None));
                    }
                    ResPos::Value(ResValue::None)
                },
            }
        })
    }
}

#[macro_export]
macro_rules! pw_store_if_needed {
    ($prog: ident, $pos: ident, $b: block) => {
        pw!($prog, store, {
            let $pos =
                match store {
                    ResultSink::WriteTo(store_pos) => store_pos,
                    ResultSink::WantResult => ResPos::Stack(0),
                    ResultSink::Null => ResPos::Value(ResValue::None),
                };

            $b;
            $pos
        })
    }
}


#[macro_export]
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
                    $prog.op_mov(&SynPos::empty(), $pos, ResPos::Value(ResValue::None));
                    ResPos::Value(ResValue::None)
                },
            }
        })
    }
}
