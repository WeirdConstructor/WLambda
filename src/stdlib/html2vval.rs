use super::rcdom::*;
use crate::vval::*;

use html5ever::parse_document;
use html5ever::tendril::TendrilSink;
use tendril::StrTendril;

fn handle2vval(handle: &Handle, prune_childs: bool) -> VVal {
    let v_node = VVal::map();
    match &handle.data {
        NodeData::Document => {
            let _ = v_node.set_key_str("%type", VVal::new_sym("document")).unwrap();
        }
        NodeData::Text { contents } => {
            let _ = v_node.set_key_str("%type", VVal::new_sym("text")).unwrap();
            let _ = v_node
                .set_key_str("%data", VVal::new_str_mv(contents.borrow().to_string()))
                .unwrap();
        }
        NodeData::Element { name, attrs, .. } => {
            let _ = v_node.set_key_str("%type", VVal::new_sym("element")).unwrap();
            let _ = v_node.set_key_str("%name", VVal::new_sym(&name.local)).unwrap();

            let v_attrs = VVal::vec();

            for attr in attrs.borrow().iter() {
                let value = VVal::new_str_mv(attr.value.to_string());
                let _ = v_node.set_key_str(&(String::from("@") + &attr.name.local), value.clone());

                v_attrs.push(VVal::pair(VVal::new_sym(&attr.name.local), value));
            }

            let _ = v_node.set_key_str("%attrs", v_attrs);
        }
        _ => {
            let _ = v_node.set_key_str("%type", VVal::new_sym("unknown"));
        }
    }

    let mut childs = VVal::None;

    for child in handle.children.borrow().iter() {
        if childs.is_none() {
            childs = VVal::vec();
        }
        let v_child = handle2vval(child, prune_childs);

        let c_type = v_child.v_s_rawk("%type");

        if c_type == "text" {
            let mut pre_text = v_node.v_s_rawk("%text");
            pre_text = v_child.v_with_s_refk("%data", |s| pre_text + s);
            let _ = v_node.set_key_str("%text", VVal::new_str_mv(pre_text));

            let mut pre_text = v_node.v_s_rawk("%rtext");
            pre_text = v_child.v_with_s_refk("%data", |s| pre_text + s);
            let _ = v_node.set_key_str("%rtext", VVal::new_str_mv(pre_text));
        } else if c_type == "element" {
            let name = v_child.v_k("%name");
            name.with_s_ref(|e_name| {
                if !v_node.v_k(e_name).is_some() {
                    let _ = v_node.set_key_str(e_name, VVal::vec());
                }

                v_node.v_k(e_name).push(v_child.clone());
            });
        }

        if v_child.v_k("%rtext").is_some() {
            if v_node.v_k("%rtext").is_some() {
                let mut pre_text = v_node.v_s_rawk("%rtext");
                pre_text = v_child.v_with_s_refk("%rtext", |s| pre_text + s);
                let _ = v_node.set_key_str("%rtext", VVal::new_str_mv(pre_text));
            } else {
                let _ = v_node.set_key_str("%rtext", v_child.v_k("%rtext"));
            }
        }

        childs.push(v_child);
    }

    if !prune_childs && childs.is_some() {
        let _ = v_node.set_key_str("%childs", childs);
    }

    v_node
}

fn handle2vval_simplified(handle: &Handle, parent: &VVal, level: u32) {
    let v_node = match &handle.data {
        NodeData::Text { contents } => {
            parent.v_(1).v_k("_").push(VVal::new_str_mv(contents.borrow().to_string()));
            return;
        }
        NodeData::Document => {
            parent.clone()
        }
        NodeData::Element { name, attrs, .. } => {
            let v_attrs = VVal::map1("_", VVal::vec());

            for attr in attrs.borrow().iter() {
                let value = VVal::new_str_mv(attr.value.to_string());
                let _ = v_attrs.set_key_str(&attr.name.local, value);
            }

            let v_node = VVal::pair(VVal::new_sym(&name.local), v_attrs);
            parent.v_(1).v_k("_").push(v_node.clone());
            v_node
        }
        _ => {
            let v_node = VVal::pair(VVal::new_sym("unknown"), VVal::map1("_", VVal::vec()));
            parent.v_(1).v_k("_").push(v_node.clone());
            v_node
        }
    };

    for child in handle.children.borrow().iter() {
        handle2vval_simplified(child, &v_node, level + 1);

//        let name = v_child.v_k("%name");
//        name.with_s_ref(|e_name| {
//            if !v_node.v_k(e_name).is_some() {
//                let _ = v_node.set_key_str(e_name, VVal::vec());
//            }
//
//            v_node.v_k(e_name).push(v_child.clone());
//        });
    }
}

pub fn parse(s: &str, prune_childs: bool) -> VVal {
    let mut result_tok = parse_document(RcDom::default(), Default::default());
    result_tok.process(StrTendril::from(s));
    let dom = result_tok.finish();

    //    println!("DOM: {:?}", dom);
    handle2vval(&dom.document, prune_childs)
}

pub fn parse_simplified(s: &str) -> VVal {
    let mut result_tok = parse_document(RcDom::default(), Default::default());
    result_tok.process(StrTendril::from(s));
    let dom = result_tok.finish();

    //    println!("DOM: {:?}", dom);
    let res = VVal::pair(VVal::new_sym("root"), VVal::map1("_", VVal::vec()));
    handle2vval_simplified(&dom.document, &res, 0);
    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rcdom2vval() {
        let v = parse("<a hannes=\"wurstkopp\" test=3 test=4>mit wurst\n <i>in</i> kaka\n</a><a href=börg>penis</a>", false);
        assert_eq!(v.v_s_rawk("%rtext"), "mit wurst\n in kaka\npenis");
        assert_eq!(
            v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(0).v_s_rawk("%rtext"),
            "mit wurst\n in kaka\n"
        );
        assert_eq!(
            v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(0).v_s_rawk("%text"),
            "mit wurst\n  kaka\n"
        );
        assert_eq!(
            v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(0).v_s_rawk("%attrs"),
            "$[$p(:hannes,\"wurstkopp\"),$p(:test,\"3\")]"
        );
        assert_eq!(
            v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(0).v_s_rawk("@hannes"),
            "wurstkopp"
        );
        assert_eq!(v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(0).v_s_rawk("@test"), "3");
        assert_eq!(v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(1).v_s_rawk("%text"), "penis");
        assert_eq!(
            v.v_k("html").v_(0).v_k("body").v_(0).v_k("a").v_(1).v_s_rawk("%attrs"),
            "$[$p(:href,\"börg\")]"
        );
    }

    fn run_wlambda_p(wlcode: &str, html: &str, prune_childs: bool) -> String {
        use crate::EvalContext;

        let v = parse(html, prune_childs);

        let mut ctx = EvalContext::new_default();
        ctx.set_global_var("@@", &v);

        ctx.eval(wlcode).unwrap().s()
    }

    fn run_wlambda(wlcode: &str, html: &str) -> String {
        run_wlambda_p(wlcode, html, false)
    }

    #[test]
    fn test_dom_wlambda_selector() {
        let html1 = "<a hannes=\"wurstkopp\" test=3 test=4>mit wurst\n <i @x=32>in</i> kaka\n</a><a href=börg>penis</a>";
        assert_eq!(
            run_wlambda("$S{html/0/body/*/%rtext} @@ | 0", html1),
            "\"mit wurst\\n in kaka\\npenis\""
        );
        assert_eq!(run_wlambda("$S{**/@test} @@ | 0", html1), "\"3\"");
        assert_eq!(run_wlambda("$S{**/@@x} @@ | 0", html1), "\"32\"");
        assert_eq!(
            run_wlambda("$S[ html/0/body/0/a/0/%attrs ] @@", html1),
            "$[$[$p(:hannes,\"wurstkopp\"),$p(:test,\"3\")]]"
        );

        let html2 = "<a hannes=\"wurstkopp\" test=3 test=4>mit wurst\n <i @x=32>in</i> kaka\n</a><a href=börg id=\"xxx\">penis</a>";
        assert_eq!(
            run_wlambda(
                "#std:displayln ~ std:ser:json @@;\n $S[ **!key=%childs/*:{@id=xxx}/%text ] @@ | 0",
                html2
            ),
            "\"penis\""
        );
    }
}
