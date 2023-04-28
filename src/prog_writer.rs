use crate::compiler::ResPos;
use crate::compiler::ResValue;
use crate::ops::*;
use crate::vval::{SynPos, TypeId};

#[derive(Debug, Clone)]
pub(crate) enum ResultSink {
    WriteTo(ResPos),
    WantResult,
    Null,
}

impl ResultSink {
    pub(crate) fn if_null<T>(&self, f: T) -> bool
    where
        T: FnOnce(ResPos),
    {
        match self {
            ResultSink::WriteTo(_) => true,
            ResultSink::WantResult => true,
            ResultSink::Null => {
                let rp = ResPos::Value(ResValue::None);
                f(rp);
                false
            }
        }
    }

    pub(crate) fn if_must_store<T>(&self, f: T) -> ResPos
    where
        T: FnOnce(ResPos),
    {
        match self {
            ResultSink::WriteTo(rp) => {
                f(*rp);
                *rp
            }
            ResultSink::WantResult => {
                let rp = ResPos::Stack(0);
                f(rp);
                rp
            }
            ResultSink::Null => ResPos::Value(ResValue::None),
        }
    }
}

pub(crate) type ProgWriteNode = Box<dyn Fn(&mut Prog, ResultSink) -> ResPos>;

pub(crate) struct ProgWriter {
    pub result_type: TypeId,
    node: ProgWriteNode,
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
    ProgWriter { node: Box::new(f), result_type: 0 }
}

/// Attaches a type to the value the ProgWriter is going to generate.
#[macro_export]
macro_rules! typed {
    ($typeid: expr, $pwres: expr) => {
        {
            match $pwres {
                Ok(mut pw) => {
                    pw.result_type = $typeid;
                    Ok(pw)
                }
                pw => pw,
            }
        }
    }
}

/// Internal helper macro for the code generator. It creates a `ProgWriter`
/// instance from the given code block.
/// `$prog` is the variable of the
/// [Prog] and `$store` is the supplied store type that is requested
/// by the parent of this AST node.
#[macro_export]
macro_rules! pw {
    ($prog: ident, $store: ident, $b: block) => {
        Ok(pw(Box::new(move |$prog, $store| $b)))
    };
}

/// Internal helper macro for the code generator. It creates a `ProgWriter`
/// instance from the given code block. `pw_null` is used in cases where
/// there are only side effects and no return values to be stored anywhere.
/// `$prog` is the variable of the [Prog].
#[macro_export]
macro_rules! pw_null {
    ($prog: ident, $b: block) => {
        pw_provides_result_pos!($prog, {
            $b
            ResPos::Value(ResValue::None)
        })
    }
}

/// Internal helper macro for the code generator. It creates a `ProgWriter`
/// instance from the given code block. It's used in cases where the `ProgWriter`
/// code in the code block `$b` defines the storage position where it wrote
/// the results to. The calling parent then has to take care to store
/// the value in some reasonable place or not store it at all.
/// `$prog` is the variable of the [Prog].
#[macro_export]
macro_rules! pw_provides_result_pos {
    ($prog: ident, $b: block) => {
        pw!($prog, store, {
            let pos = $b;
            match store {
                ResultSink::WriteTo(store_pos) => {
                    $prog.op_mov(&SynPos::empty(), pos, store_pos);
                    store_pos
                }
                ResultSink::WantResult => pos,
                ResultSink::Null => {
                    if let ResPos::Stack(_) = pos {
                        $prog.op_mov(&SynPos::empty(), pos, ResPos::Value(ResValue::None));
                    }
                    ResPos::Value(ResValue::None)
                }
            }
        })
    };
}

/// Internal helper macro for the code generator. It creates a `ProgWriter`
/// instance from the given code block. It's used where the `ProgWriter`
/// needs it's parent to define a place to put it's result to, but it's not
/// required to be stored.
/// `$prog` is the variable of the [Prog].
#[macro_export]
macro_rules! pw_store_if_needed {
    ($prog: ident, $pos: ident, $b: block) => {
        pw!($prog, store, {
            let $pos = match store {
                ResultSink::WriteTo(store_pos) => store_pos,
                ResultSink::WantResult => ResPos::Stack(0),
                ResultSink::Null => ResPos::Value(ResValue::None),
            };

            $b;
            $pos
        })
    };
}

/// Internal helper macro for the code generator. It creates a `ProgWriter`
/// instance from the given code block. It's used where the `ProgWriter`
/// needs it's parent to define a place to put it's result to.
/// `$prog` is the variable of the [Prog].
#[macro_export]
macro_rules! pw_needs_storage {
    ($prog: ident, $pos: ident, $b: block) => {
        pw!($prog, store, {
            match store {
                ResultSink::WriteTo(store_pos) => {
                    let $pos = store_pos;
                    $b;
                    $pos
                }
                ResultSink::WantResult => {
                    let $pos = ResPos::Stack(0);
                    $b;
                    $pos
                }
                ResultSink::Null => {
                    let $pos = ResPos::Stack(0);
                    $b;
                    $prog.op_mov(&SynPos::empty(), $pos, ResPos::Value(ResValue::None));
                    ResPos::Value(ResValue::None)
                }
            }
        })
    };
}
