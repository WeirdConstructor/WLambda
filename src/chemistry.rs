use crate::threads::ThreadSafeUsr;
use crate::vval::{Env, StackAction, VVal, VValUserData};

use crate::parser::state::State;
use crate::parser::state::{ParseError, ParseErrorKind};

use std::cell::RefCell;
use std::sync::Arc;

thread_local! {
    static ELEMS_DATA: RefCell<VVal> = RefCell::new(VVal::None);
    static ELEMS_VEC: RefCell<Vec<VVal>> = RefCell::new(vec![]);
}

macro_rules! assert_arg_count {
    ($self: expr, $argv: expr, $count: expr, $function: expr, $env: ident) => {
        if $argv.len() != $count {
            return Err(StackAction::panic_str(
                format!("{}.{} expects {} arguments", $self, $function, $count),
                None,
                $env.argv(),
            ));
        }
    };
}

fn load_elems() {
    use weezl::{decode::Decoder, BitOrder};

    let elems_data_packed = include_bytes!("chemical_elements.json.lzw");

    match Decoder::new(BitOrder::Msb, 8).decode(elems_data_packed) {
        Ok(data) => {
            let s = String::from_utf8(data).unwrap();
            let chemical_data_tree = VVal::from_json(&s).expect("No malformed json compiled in!");
            let mut elems_vec = vec![];

            chemical_data_tree.with_iter(|iter| {
                for (v, _) in iter {
                    elems_vec.push(v);
                }
            });

            ELEMS_VEC.with(|v| {
                (*v.borrow_mut()) = elems_vec;
            });

            ELEMS_DATA.with(|d| {
                (*d.borrow_mut()) = chemical_data_tree;
            });
        }
        Err(e) => panic!("loading chemical elements failed, this should never happen! {}", e),
    }
}

pub fn get_elem_by_atomic_number(num: u8) -> Option<VVal> {
    let not_loaded = ELEMS_VEC.with(|v| v.borrow().is_empty());
    if not_loaded {
        load_elems();
    }
    ELEMS_VEC.with(|v| v.borrow().get(num as usize - 1).cloned())
}

#[derive(Debug, Clone)]
pub enum ChemFormula {
    Element(u8, u32),
    Group(Arc<Vec<ChemFormula>>, u32),
    Ion(Arc<Vec<ChemFormula>>, u32),
}

impl ChemFormula {
    pub fn atomic_numbers(&self, out: &mut Vec<u8>) {
        match self {
            ChemFormula::Element(anum, _) => {
                if !out.contains(anum) {
                    out.push(*anum);
                }
            }
            ChemFormula::Ion(seq, _) | ChemFormula::Group(seq, _) => {
                for cf in seq.iter() {
                    cf.atomic_numbers(out);
                }
            }
        }
    }

    pub fn atoms(&self, out: &mut Vec<(u8, u32)>, factor: u32) {
        match self {
            ChemFormula::Element(anum, count) => {
                for (num, ncount) in out.iter_mut() {
                    if *num == *anum {
                        *ncount += factor * *count;
                        return;
                    }
                }

                out.push((*anum, factor * *count));
            }
            ChemFormula::Ion(seq, count) | ChemFormula::Group(seq, count) => {
                for cf in seq.iter() {
                    cf.atoms(out, factor * *count);
                }
            }
        }
    }

    pub fn first_atomic_number(&self) -> u8 {
        match self {
            ChemFormula::Element(anum, _) => *anum,
            ChemFormula::Ion(seq, _) | ChemFormula::Group(seq, _) => {
                if let Some(cf) = seq.get(0) {
                    cf.first_atomic_number()
                } else {
                    0
                }
            }
        }
    }
}

impl ThreadSafeUsr for ChemFormula {
    fn to_vval(&self) -> VVal {
        VVal::new_usr(self.clone())
    }
}

impl std::fmt::Display for ChemFormula {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ChemFormula::Element(e, num) => {
                let el = get_elem_by_atomic_number(*e).unwrap_or(VVal::None);
                el.v_with_s_refk("symbol", |s: &str| {
                    if *num == 1 {
                        write!(f, "{}", s)
                    } else {
                        write!(f, "{}{}", s, num)
                    }
                })
            }
            ChemFormula::Group(seq, num) => {
                if *num != 1 {
                    write!(f, "(")?;
                }
                for cf in seq.iter() {
                    write!(f, "{}", cf)?;
                }
                if *num != 1 {
                    write!(f, ")")?;
                    write!(f, "{}", num)?
                }
                Ok(())
            }
            ChemFormula::Ion(seq, num) => {
                if *num != 1 {
                    write!(f, "[")?;
                }
                for cf in seq.iter() {
                    write!(f, "{}", cf)?;
                }
                if *num != 1 {
                    write!(f, "]")?;
                    write!(f, "{}", num)?
                }
                Ok(())
            }
        }
    }
}

impl VValUserData for ChemFormula {
    fn s(&self) -> String {
        format!("$<Chem:{}>", *self)
    }
    fn i(&self) -> i64 {
        self.first_atomic_number() as i64
    }

    fn call_method(&self, key: &str, env: &mut Env) -> Result<VVal, StackAction> {
        let argv = env.argv();

        match key {
            "first_elem_info" => {
                assert_arg_count!("$<Chem>", argv, 0, "first_elem_info[]", env);
                let num = self.first_atomic_number();
                Ok(get_elem_by_atomic_number(num).unwrap_or(VVal::None))
            }
            "summary" => {
                assert_arg_count!("$<Chem>", argv, 0, "summary[]", env);
                let mut nums = vec![];
                self.atomic_numbers(&mut nums);
                let v = VVal::vec();
                for anum in nums.iter() {
                    v.push(get_elem_by_atomic_number(*anum).unwrap_or(VVal::None));
                }
                Ok(v)
            }
            "for_each_element" => {
                assert_arg_count!("$<Chem>", argv, 1, "for_each_element[function]", env);
                let mut nums = vec![];
                self.atoms(&mut nums, 1);

                let fun = argv.v_(0);
                fun.disable_function_arity();

                for (anum, count) in nums.iter() {
                    let info = get_elem_by_atomic_number(*anum).unwrap_or(VVal::None);
                    match fun.call(env, &[VVal::Int(*anum as i64), VVal::Int(*count as i64), info])
                    {
                        Ok(_) => (),
                        Err(StackAction::Return(_)) => (),
                        Err(StackAction::Break(_)) => break,
                        Err(StackAction::Next) => continue,
                        Err(a) => return Err(a),
                    }
                }
                Ok(VVal::None)
            }
            _ => Ok(VVal::err_msg(&format!("Unknown method called: {}", key))),
        }
    }

    fn call(&self, env: &mut Env) -> Result<VVal, StackAction> {
        let args = env.argv_ref();
        if args.len() <= 0 {
            return Err(StackAction::panic_msg(format!(
                "{} called with too few arguments",
                self.s()
            )));
        }

        Ok(args[0].clone())
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn clone_ud(&self) -> Box<dyn VValUserData> {
        Box::new(self.clone())
    }
    fn as_thread_safe_usr(&mut self) -> Option<Box<dyn ThreadSafeUsr>> {
        Some(Box::new(self.clone()))
    }
}

fn try_parse_element(ps: &mut State) -> Option<u8> {
    if let Some(s) = ps.peek2() {
        let num = match s.at(0) {
            'A' | 'a' => match s.at(1) {
                'm' => Some((2, 95)), // Am: Americium
                'r' => Some((2, 18)), // Ar: Argon
                'c' => Some((2, 89)), // Ac: Actinium
                'l' => Some((2, 13)), // Al: Aluminum
                's' => Some((2, 33)), // As: Arsenic
                'g' => Some((2, 47)), // Ag: Silver
                'u' => Some((2, 79)), // Au: Gold
                't' => Some((2, 85)), // At: Astatine
                _ => None,
            },
            'B' | 'b' => match s.at(1) {
                'i' => Some((2, 83)),  // Bi: Bismuth
                'e' => Some((2, 4)),   // Be: Beryllium
                'h' => Some((2, 107)), // Bh: Bohrium
                'k' => Some((2, 97)),  // Bk: Berkelium
                'r' => Some((2, 35)),  // Br: Bromine
                'a' => Some((2, 56)),  // Ba: Barium
                _ => Some((1, 5)),     // B: Boron
            },
            'C' | 'c' => match s.at(1) {
                'm' => Some((2, 96)),  // Cm: Curium
                'f' => Some((2, 98)),  // Cf: Californium
                'o' => Some((2, 27)),  // Co: Cobalt
                'r' => Some((2, 24)),  // Cr: Chromium
                'u' => Some((2, 29)),  // Cu: Copper
                'a' => Some((2, 20)),  // Ca: Calcium
                'd' => Some((2, 48)),  // Cd: Cadmium
                'l' => Some((2, 17)),  // Cl: Chlorine
                's' => Some((2, 55)),  // Cs: Cesium
                'e' => Some((2, 58)),  // Ce: Cerium
                'n' => Some((2, 112)), // Cn: Copernicium
                _ => Some((1, 6)),     // C: Carbon
            },
            'D' | 'd' => match s.at(1) {
                'b' => Some((2, 105)), // Db: Dubnium
                's' => Some((2, 110)), // Ds: Darmstadtium
                'y' => Some((2, 66)),  // Dy: Dysprosium
                _ => None,
            },
            'E' | 'e' => match s.at(1) {
                's' => Some((2, 99)), // Es: Einsteinium
                'u' => Some((2, 63)), // Eu: Europium
                'r' => Some((2, 68)), // Er: Erbium
                _ => None,
            },
            'F' | 'f' => match s.at(1) {
                'l' => Some((2, 114)), // Fl: Flerovium
                'm' => Some((2, 100)), // Fm: Fermium
                'e' => Some((2, 26)),  // Fe: Iron
                'r' => Some((2, 87)),  // Fr: Francium
                _ => Some((1, 9)),     // F: Fluorine
            },
            'G' | 'g' => match s.at(1) {
                'd' => Some((2, 64)), // Gd: Gadolinium
                'e' => Some((2, 32)), // Ge: Germanium
                'a' => Some((2, 31)), // Ga: Gallium
                _ => None,
            },
            'H' | 'h' => match s.at(1) {
                'e' => Some((2, 2)),   // He: Helium
                'o' => Some((2, 67)),  // Ho: Holmium
                'g' => Some((2, 80)),  // Hg: Mercury
                'f' => Some((2, 72)),  // Hf: Hafnium
                's' => Some((2, 108)), // Hs: Hassium
                _ => Some((1, 1)),     // H: Hydrogen
            },
            'I' | 'i' => match s.at(1) {
                'r' => Some((2, 77)), // Ir: Iridium
                'n' => Some((2, 49)), // In: Indium
                _ => Some((1, 53)),   // I: Iodine
            },
            'K' | 'k' => match s.at(1) {
                'r' => Some((2, 36)), // Kr: Krypton
                _ => Some((1, 19)),   // K: Potassium
            },
            'L' | 'l' => match s.at(1) {
                'i' => Some((2, 3)),   // Li: Lithium
                'a' => Some((2, 57)),  // La: Lanthanum
                'u' => Some((2, 71)),  // Lu: Lutetium
                'v' => Some((2, 116)), // Lv: Livermorium
                'r' => Some((2, 103)), // Lr: Lawrencium
                _ => None,
            },
            'M' | 'm' => match s.at(1) {
                'n' => Some((2, 25)),  // Mn: Manganese
                'o' => Some((2, 42)),  // Mo: Molybdenum
                'g' => Some((2, 12)),  // Mg: Magnesium
                'd' => Some((2, 101)), // Md: Mendelevium
                't' => Some((2, 109)), // Mt: Meitnerium
                'c' => Some((2, 115)), // Mc: Moscovium
                _ => None,
            },
            'N' | 'n' => match s.at(1) {
                'p' => Some((2, 93)),  // Np: Neptunium
                'i' => Some((2, 28)),  // Ni: Nickel
                'e' => Some((2, 10)),  // Ne: Neon
                'b' => Some((2, 41)),  // Nb: Niobium
                'h' => Some((2, 113)), // Nh: Nihonium
                'a' => Some((2, 11)),  // Na: Sodium
                'o' => Some((2, 102)), // No: Nobelium
                'd' => Some((2, 60)),  // Nd: Neodymium
                _ => Some((1, 7)),     // N: Nitrogen
            },
            'O' | 'o' => match s.at(1) {
                's' => Some((2, 76)),  // Os: Osmium
                'g' => Some((2, 118)), // Og: Oganesson
                _ => Some((1, 8)),     // O: Oxygen
            },
            'P' | 'p' => match s.at(1) {
                'r' => Some((2, 59)), // Pr: Praseodymium
                'a' => Some((2, 91)), // Pa: Protactinium
                'o' => Some((2, 84)), // Po: Polonium
                'm' => Some((2, 61)), // Pm: Promethium
                't' => Some((2, 78)), // Pt: Platinum
                'b' => Some((2, 82)), // Pb: Lead
                'd' => Some((2, 46)), // Pd: Palladium
                'u' => Some((2, 94)), // Pu: Plutonium
                _ => Some((1, 15)),   // P: Phosphorus
            },
            'R' | 'r' => match s.at(1) {
                'e' => Some((2, 75)),  // Re: Rhenium
                'n' => Some((2, 86)),  // Rn: Radon
                'a' => Some((2, 88)),  // Ra: Radium
                'f' => Some((2, 104)), // Rf: Rutherfordium
                'g' => Some((2, 111)), // Rg: Roentgenium
                'u' => Some((2, 44)),  // Ru: Ruthenium
                'h' => Some((2, 45)),  // Rh: Rhodium
                'b' => Some((2, 37)),  // Rb: Rubidium
                _ => None,
            },
            'S' | 's' => match s.at(1) {
                'm' => Some((2, 62)),  // Sm: Samarium
                'g' => Some((2, 106)), // Sg: Seaborgium
                'c' => Some((2, 21)),  // Sc: Scandium
                'e' => Some((2, 34)),  // Se: Selenium
                'i' => Some((2, 14)),  // Si: Silicon
                'r' => Some((2, 38)),  // Sr: Strontium
                'n' => Some((2, 50)),  // Sn: Tin
                'b' => Some((2, 51)),  // Sb: Antimony
                _ => Some((1, 16)),    // S: Sulfur
            },
            'T' | 't' => match s.at(1) {
                's' => Some((2, 117)), // Ts: Tennessine
                'a' => Some((2, 73)),  // Ta: Tantalum
                'c' => Some((2, 43)),  // Tc: Technetium
                'm' => Some((2, 69)),  // Tm: Thulium
                'e' => Some((2, 52)),  // Te: Tellurium
                'b' => Some((2, 65)),  // Tb: Terbium
                'i' => Some((2, 22)),  // Ti: Titanium
                'l' => Some((2, 81)),  // Tl: Thallium
                'h' => Some((2, 90)),  // Th: Thorium
                _ => None,
            },
            'X' | 'x' => match s.at(1) {
                'e' => Some((2, 54)), // Xe: Xenon
                _ => None,
            },
            'Y' | 'y' => match s.at(1) {
                'b' => Some((2, 70)), // Yb: Ytterbium
                _ => Some((1, 39)),   // Y: Yttrium
            },
            'Z' | 'z' => match s.at(1) {
                'n' => Some((2, 30)), // Zn: Zinc
                'r' => Some((2, 40)), // Zr: Zirconium
                _ => None,
            },
            'U' | 'u' => Some((1, 92)), // U: Uranium
            'V' | 'v' => Some((1, 23)), // V: Vanadium
            'W' | 'w' => Some((1, 74)), // W: Tungsten
            _ => None,
        };

        if let Some((count, num)) = num {
            ps.consume_wsc_n(count);
            Some(num)
        } else {
            None
        }
    } else if let Some(c) = ps.peek() {
        let num = match c {
            'B' | 'b' => Some(5),  // B: Boron
            'C' | 'c' => Some(6),  // C: Carbon
            'F' | 'f' => Some(9),  // F: Fluorine
            'H' | 'h' => Some(1),  // H: Hydrogen
            'I' | 'i' => Some(53), // I: Iodine
            'K' | 'k' => Some(19), // K: Potassium
            'N' | 'n' => Some(7),  // N: Nitrogen
            'O' | 'o' => Some(8),  // O: Oxygen
            'P' | 'p' => Some(15), // P: Phosphorus
            'S' | 's' => Some(16), // S: Sulfur
            'U' | 'u' => Some(92), // U: Uranium
            'V' | 'v' => Some(23), // V: Vanadium
            'W' | 'w' => Some(74), // W: Tungsten
            'Y' | 'y' => Some(39), // Y: Yttrium
            _ => None,
        };

        if let Some(num) = num {
            ps.consume_wsc();
            Some(num)
        } else {
            None
        }
    } else {
        None
    }
}

fn try_parse_number(ps: &mut State) -> Result<Option<u32>, ParseError> {
    if let Some(peek_char) = ps.peek() {
        match peek_char {
            '0'..='9' => {
                let num = ps.take_while(|c| c.is_digit(10)).to_string();
                if let Ok(cn) = u32::from_str_radix(&num, 10) {
                    ps.skip_ws_and_comments();
                    Ok(Some(cn))
                } else {
                    Err(ps.err(ParseErrorKind::UnexpectedToken(
                        '?',
                        "Can't parse number in sum formula!",
                    )))
                }
            }
            _ => Ok(None),
        }
    } else {
        Ok(None)
    }
}

fn parse_sequence(ps: &mut State) -> Result<Vec<ChemFormula>, ParseError> {
    let mut ret = vec![];

    while !ps.at_end() {
        if let Some(elem) = try_parse_element(ps) {
            if let Some(num) = try_parse_number(ps)? {
                ret.push(ChemFormula::Element(elem, num));
            } else {
                ret.push(ChemFormula::Element(elem, 1));
            }
        } else {
            match ps.expect_some(ps.peek())? {
                '(' => {
                    ps.consume_wsc();
                    let seq = parse_sequence(ps)?;
                    if !ps.consume_if_eq_wsc(')') {
                        return Err(ps.err(ParseErrorKind::ExpectedToken(')', "sub formula end")));
                    }
                    if let Some(num) = try_parse_number(ps)? {
                        ret.push(ChemFormula::Group(Arc::new(seq), num));
                    } else {
                        ret.push(ChemFormula::Group(Arc::new(seq), 1));
                    }
                }
                '[' => {
                    ps.consume_wsc();
                    let seq = parse_sequence(ps)?;
                    if !ps.consume_if_eq_wsc(']') {
                        return Err(ps.err(ParseErrorKind::ExpectedToken(']', "sub formula end")));
                    }
                    if let Some(num) = try_parse_number(ps)? {
                        ret.push(ChemFormula::Ion(Arc::new(seq), num));
                    } else {
                        ret.push(ChemFormula::Ion(Arc::new(seq), 1));
                    }
                }
                ']' => break,
                ')' => break,
                c => return Err(ps.err(ParseErrorKind::UnexpectedToken(c, "element sequence"))),
            }
        }
    }

    Ok(ret)
}

pub fn get_periodic_table_data() -> VVal {
    let not_loaded = ELEMS_VEC.with(|v| v.borrow().is_empty());
    if not_loaded {
        load_elems();
    }

    let ret = VVal::vec();

    ELEMS_VEC.with(|v| {
        for elem in v.borrow().iter() {
            ret.push(elem.clone());
        }
    });

    ret
}

pub fn parse_chemical_sum_formula(s: &str) -> Result<VVal, ParseError> {
    let mut ps = State::new_verbatim(s, "<chemical-sum-formula>");

    let seq = parse_sequence(&mut ps)?;
    if seq.len() == 0 {
        return Err(ps.err(ParseErrorKind::EOF("premature EOF in formula")));
    }

    if !ps.at_end() {
        return Err(ps.err(ParseErrorKind::UnexpectedToken(ps.peek().unwrap(), "end of formula")));
    }

    if seq.len() == 1 {
        Ok(VVal::new_usr(seq[0].clone()))
    } else {
        Ok(VVal::new_usr(ChemFormula::Group(Arc::new(seq), 1)))
    }
}
