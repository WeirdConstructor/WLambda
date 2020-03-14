use std::rc::Rc;
use std::cell::RefCell;
use crate::compiler::*;
use crate::vval::*;

struct Prog {
    clos:   std::vec::Vec<EvalNode>,
    data:   std::vec::Vec<VVal>,
    ops:    std::vec::Vec<Op>,
}

impl Prog {
    fn append(&mut self, mut prog: Prog) {
        let mut self_data_next_idx : u32 = (self.data.len() - 1) as u32;

        for o in prog.ops.iter_mut() {
            match o {
                Op::Push(ref mut data_idx) => {
                    self_data_next_idx += 1;
                    *data_idx = *data_idx + self_data_next_idx;
                },
                Op::NewPair => (),
            }
        }

        self.clos.append(&mut prog.clos);
        self.data.append(&mut prog.data);
        self.ops.append(&mut prog.ops);
    }

    fn new() -> Self {
        Self {
            clos: vec![],
            data: vec![],
            ops:  vec![],
        }
    }

    fn push(mut self, v: VVal) -> Self {
        self.data.push(v);
        self.ops.push(Op::Push((self.data.len() - 1) as u32));
        self
    }

    fn op(mut self, o: Op) -> Self {
        self.ops.push(o);
        self
    }
}

#[derive(Debug,Copy,Clone)]
enum Op {
    Push(u32),
    NewPair,
}

fn vm(prog: &Prog, env: &mut Env) -> Result<VVal, StackAction> {
    let mut pc : usize = 0;
    loop {
        let op = prog.ops[pc];
        match op {
            Op::Push(data_idx) => {
                env.push(prog.data[data_idx as usize].clone());
            },
            Op::NewPair => {
                let a = env.pop();
                let b = env.pop();
                env.push(VVal::Pair(Box::new((a, b))));
            }
        }
        pc += 1;
    }
}

fn vm_compile(ast: &VVal, ce: &mut Rc<RefCell<CompileEnv>>) -> Result<Prog, CompileError> {
    match ast {
        VVal::Lst(l) => {
            let syn  = ast.at(0).unwrap_or(VVal::Nul);
            let spos = syn.get_syn_pos();
            let syn  = syn.get_syn();

            match syn {
                _ => { Err(ast.compile_err(format!("bad input: {}", ast.s()))) },
            }
        },
        VVal::Pair(bx) => {
            let a = vm_compile(&bx.0, ce)?;
            let b = vm_compile(&bx.1, ce)?;
            let mut p = Prog::new();
            p.append(a);
            p.append(b);
            Ok(p.op(Op::NewPair))
        },
        _ => {
            Ok(Prog::new().push(ast.clone()))
        }
    }
}
