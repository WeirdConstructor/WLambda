// Copyright (c) 2020-2024 Weird Constructor <weirdconstructor@gmail.com>
// This is a part of WLambda. See README.md and COPYING for details.

/*!
This contains the WLambda code formatter, which prints out `SourceAnnotation` trees
as code.
*/

pub use crate::parser::state::State;
use crate::vval::Syntax;

#[derive(Copy, Clone, Debug, PartialEq)]
enum ItemType {
    TOK,
    SP,
    NBSP,
}

#[derive(Copy, Clone, Debug)]
pub struct SourceItem {
    idx: usize,
    char_count: usize,
    typ: ItemType,
}

impl SourceItem {}

pub struct SourceFormatter {
    syn_stack: Vec<Syntax>,
    part_buffer: Vec<String>,
}

fn chld_syn(ps: &State, childs: &[usize], idx: usize) -> Syntax {
    if let Some(ch_id) = childs.get(idx) {
        if let Some(an) = ps.get_annotation(*ch_id) {
            return an.get_syntax();
        }
    }
    Syntax::TNone
}

fn sp() -> SourceItem {
    SourceItem { idx: 0, char_count: 1, typ: ItemType::SP }
}

fn nbsp() -> SourceItem {
    SourceItem { idx: 0, char_count: 1, typ: ItemType::NBSP }
}

fn tok(idx: usize, cc: usize) -> SourceItem {
    SourceItem { idx, char_count: cc, typ: ItemType::TOK }
}

impl SourceFormatter {
    pub fn new() -> Self {
        Self { syn_stack: Vec::new(), part_buffer: vec![String::from(" ")] }
    }

    pub fn item(&mut self, s: String) -> SourceItem {
        let ret = tok(self.part_buffer.len(), s.chars().count());
        self.part_buffer.push(s);
        ret
    }

    pub fn items2string_wrap(&self, items: &[SourceItem], line_width: usize) -> String {
        let mut out = String::from("");
        let mut line = String::from("");
        let mut line_chars = 0;

        // TODO:
        // The wrapping needs some inline information about:
        // - current left alignment column
        // - tail length of the current statement until either ";", a forced newline or a comment.
        //
        // It would be great if we had following information:
        // - min length of item from item chunk
        // - a window (start_column, length) to try to fit in the item chunk.
        //   meaning: the indent to wrap to, in case we need to wrap the added items.
        //
        // Target:
        // - easy case: lists which fit into one line: [1, abfuefew, oeoow]
        // - if wrapped, we need to wrap after start and before the end, adhering to the current indent level.
        //   ` [\n fewfofowfwoefwe,\n  foefoew,\n  feofowe\n ]`
        // - nested lists need to adhere to the same principle:
        //   ` [\n  [1,2,3,4],\n  fewfofowfwoefwe,\n  foefoew,\n  feofowe\n ]`
        // - function calls may wrap like this.
        //   - case: increase indent to expression length (to a limit)
        //      `foo bar x a b c` => `foo bar x\n    a b c`
        //      [needs: tracking indent and length of called expression,
        //              and setting left wrapping column.]
        //      needs also to work for:
        //          !x: int = 1 + 2 + 3 + 4 + 5`
        //       =>
        //          !x: int = 1 + 2 +
        //                    3 + 4 +
        //                    5;
        //      and these cases (imagine the max left column limit was reached):
        //       =>
        //          !x: int =
        //           1 + 2 +
        //           3 + 4 + 5;
        //      [needs: traking of possible breaking points,
        //              because `<operand><ws><operator>` must be wrapped as one unit.
        //              for this we should maybe ask the leaves to insert breaking points into the items:
        //              `!x:<nbsp>int<bs>=<bs>1<nbsp>+<bs>2<nbsp>+...`]
        //   - case: too much stuff to fit into one line after wrapping:
        //      `foo bar x a b c` => `foo bar\n    x\n    a\n    b\n    c`
        //      [needs: checking if we are at the top-level of a call or
        //              in nested expressions. `do[test[1, 2, 3, 4, 5, foo[4039,49349394043]],39239422432]`
        //                                  =>  `do[\n   test[1, 2, ...`
        //                  style question: or? `do[\n   test[\n      1\n,      2\n, ...`
        //                                  or? `do[\n test[\n  1\n,  2\n, ...`
        //              calculating the number of lines the child nodes wrapped to
        //              being able to re-run the wrapping with other windows (indent and possible tail length?)]
        //   - case: indent level is too deep, we need to spill to indent:
        //      `foo babcdefghij x a b c` => `foo\n bar\n x\n a\n b\n c`
        //      [needs: tracking and limiting max left column]
        //   - case: only wrap the tail depending on whether there is a delimiter:
        //      ` foo[1,2,];` => ` foo[1\n,     2,\n ];`
        //      vs:
        //      ` foo[1,2];` => ` foo[1\n,     2];\n`
        for it in items.iter() {
            if (line_chars + it.char_count) > line_width {
                out += &line;
                out += "\n";
                line = String::from("");
                line_chars = 0;
            }
            if !((it.typ == ItemType::SP || it.typ == ItemType::NBSP) && line_chars == 0) {
                line += &self.part_buffer[it.idx];
                line_chars += it.char_count;
            }
        }

        if line_chars > 0 {
            out += &line;
        }

        out
    }

    pub fn items2string(&self, items: &[SourceItem]) -> String {
        let mut out = String::from("");
        for si in items.iter() {
            match si.typ {
                ItemType::SP => out += " ",
                ItemType::NBSP => out += " ",
                ItemType::TOK => out += &self.part_buffer[si.idx],
            }
        }
        out
    }

    pub fn format_delimited(
        &mut self,
        ps: &State,
        ids: &[usize],
        delimiter: &str,
    ) -> Vec<SourceItem> {
        let delim_item = if delimiter == " " { sp() } else { self.item(delimiter.to_string()) };
        let mut out = vec![];
        for (i, id) in ids.iter().enumerate() {
            if i != 0 {
                out.push(delim_item);
            }
            out.append(&mut self.format_source_from_ann_id(ps, *id));
        }
        out
    }

    pub fn format_source_from_ann_id(&mut self, ps: &State, id: usize) -> Vec<SourceItem> {
        println!("XOO {} [{}] {:?}", id, self.syn_stack.len(), self.syn_stack);
        let mut out = vec![];
        if let Some(an) = ps.get_annotation(id).clone() {
            self.syn_stack.push(an.get_syntax());

            if let Some(childs) = an.get_childs().as_ref() {
                match an.get_syntax() {
                    Syntax::Call => {
                        if chld_syn(ps, &childs[..], 1) == Syntax::TArgList {
                            if childs.len() > 0 {
                                out.append(&mut self.format_source_from_ann_id(ps, childs[0]));
                            }
                            for id in childs[1..].iter() {
                                out.append(&mut self.format_source_from_ann_id(ps, *id));
                            }
                        } else {
                            out.append(&mut self.format_delimited(ps, &childs[..], " "));
                        }
                    }
                    _ => {
                        for id in childs.iter() {
                            out.append(&mut self.format_source_from_ann_id(ps, *id));
                        }
                    }
                }
            } else if let Some(txt) = an.get_text(ps) {
                match an.get_syntax() {
                    Syntax::TQ => {
                        out.push(self.item(txt));
                    }
                    Syntax::TOp => {
                        out.push(nbsp());
                        out.push(self.item(txt));
                        out.push(sp());
                    }
                    Syntax::TDelim => {
                        out.push(self.item(txt));
                        out.push(sp());
                    }
                    _ => {
                        out.push(self.item(txt));
                    }
                }
            }

            self.syn_stack.pop();
        }
        println!("END[{}]", self.items2string(&out[..]));
        out
    }

    pub fn format_source(&mut self, ps: &State) -> String {
        let mut out = vec![];
        for ann_id in ps.get_toplevel_annotations().iter() {
            println!("D {}", ps.dump_annotation(Some(*ann_id), Some(1)));
            out.append(&mut self.format_source_from_ann_id(ps, *ann_id));
        }

        self.items2string_wrap(&out[..], 50)
    }
}
