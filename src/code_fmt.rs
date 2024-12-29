// Copyright (c) 2020-2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This contains the WLambda code formatter, which prints out `SourceAnnotation` trees
as code.
*/

pub use crate::parser::state::State;
use crate::vval::Syntax;

pub struct SourceFormatter {
    syn_stack: Vec<Syntax>,
}

fn chld_syn(ps: &State, childs: &[usize], idx: usize) -> Syntax {
    if let Some(ch_id) = childs.get(idx) {
        if let Some(an) = ps.get_annotation(*ch_id) {
            return an.get_syntax();
        }
    }
    Syntax::TNone
}

impl SourceFormatter {
    pub fn new() -> Self {
        Self { syn_stack: Vec::new() }
    }

    pub fn format_delimited(&mut self, ps: &State, ids: &[usize], delimiter: &str) -> String {
        let mut res = String::from("");
        for (i, id) in ids.iter().enumerate() {
            if i != 0 {
                res += &delimiter;
            }
            res += &self.format_source_from_ann_id(ps, *id);
        }
        res
    }

    pub fn format_source_from_ann_id(&mut self, ps: &State, id: usize) -> String {
        println!("XOO {} [{}] {:?}", id, self.syn_stack.len(), self.syn_stack);
        let mut res = String::from("");
        if let Some(an) = ps.get_annotation(id).clone() {
            self.syn_stack.push(an.get_syntax());

            if let Some(childs) = an.get_childs().as_ref() {
                match an.get_syntax() {
                    Syntax::Call => {
                        if chld_syn(ps, &childs[..], 1) == Syntax::TArgList {
                            if childs.len() > 0 {
                                res += &self.format_source_from_ann_id(ps, childs[0]);
                            }
                            for id in childs[1..].iter() {
                                res += &self.format_source_from_ann_id(ps, *id);
                            }
                        } else {
                            res += &self.format_delimited(ps, &childs[..], " ");
                        }
                    }
                    _ => {
                        for v in childs.iter() {
                            res += &self.format_source_from_ann_id(ps, *v);
                        }
                    }
                }
            } else if let Some(txt) = an.get_text(ps) {
                match an.get_syntax() {
                    Syntax::TQ => {
                        res += &format!("{}", txt);
                    }
                    Syntax::TOp => {
                        res += &format!(" {} ", txt);
                    }
                    Syntax::TDelim => {
                        res += &format!("{} ", txt);
                    }
                    _ => {
                        res += &txt;
                    }
                }
            }

            self.syn_stack.pop();
        }
        println!("END[{}]", res);
        res
    }

    pub fn format_source(&mut self, ps: &State) -> String {
        let mut s = String::from("");
        for ann_id in ps.get_toplevel_annotations().iter() {
            println!("D {}", ps.dump_annotation(Some(*ann_id), Some(1)));
            s += &self.format_source_from_ann_id(ps, *ann_id);
        }
        s
    }
}
