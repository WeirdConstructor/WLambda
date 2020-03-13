use crate::VVal;

/// WLambda supports Integer and Float vectors in two, three, and four dimensions.
#[derive(Debug, Clone)]
pub enum NVector {
    IVec2(i64, i64),
    IVec3(i64, i64, i64),
    IVec4(i64, i64, i64, i64),
    FVec2(f64, f64),
    FVec3(f64, f64, f64),
    FVec4(f64, f64, f64, f64),
}
use NVector::*;

impl NVector {
    #[inline]
    pub fn x(&self) -> VVal {
        match self {
            IVec2(x, _)       => VVal::Int(*x),
            IVec3(x, _, _)    => VVal::Int(*x),
            IVec4(x, _, _, _) => VVal::Int(*x),
            FVec2(x, _)       => VVal::Flt(*x),
            FVec3(x, _, _)    => VVal::Flt(*x),
            FVec4(x, _, _, _) => VVal::Flt(*x),
        }
    }

    #[inline]
    pub fn y(&self) -> VVal {
        match self {
            IVec2(_, y)       => VVal::Int(*y),
            IVec3(_, y, _)    => VVal::Int(*y),
            IVec4(_, y, _, _) => VVal::Int(*y),
            FVec2(_, y)       => VVal::Flt(*y),
            FVec3(_, y, _)    => VVal::Flt(*y),
            FVec4(_, y, _, _) => VVal::Flt(*y),
        }
    }

    #[inline]
    pub fn z(&self) -> Option<VVal> {
        match self {
            IVec2(_, _)       => None,
            IVec3(_, _, z)    => Some(VVal::Int(*z)),
            IVec4(_, _, z, _) => Some(VVal::Int(*z)),
            FVec2(_, _)       => None,
            FVec3(_, _, z)    => Some(VVal::Flt(*z)),
            FVec4(_, _, z, _) => Some(VVal::Flt(*z)),
        }
    }

    #[inline]
    pub fn w(&self) -> Option<VVal> {
        match self {
            IVec2(_, _)       => None,
            IVec3(_, _, _)    => None,
            IVec4(_, _, _, w) => Some(VVal::Int(*w)),
            FVec2(_, _)       => None,
            FVec3(_, _, _)    => None,
            FVec4(_, _, _, w) => Some(VVal::Flt(*w)),
        }
    }

    #[inline]
    pub fn declaration(&self) -> char {
        match self {
            IVec2(_, _)       => 'i',
            IVec3(_, _, _)    => 'i',
            IVec4(_, _, _, _) => 'i',
            FVec2(_, _)       => 'f',
            FVec3(_, _, _)    => 'f',
            FVec4(_, _, _, _) => 'f',
        }
    }

    pub fn s(&self) -> String {
        match (self.x(), self.y(), self.z(), self.w()) {
            (x, y, None,    None)    => format!("$i({},{})", x.s(), y.s()),
            (x, y, Some(z), None)    => format!("$i({},{},{})", x.s(), y.s(), z.s()),
            (x, y, Some(z), Some(w)) => format!("$i({},{},{},{})", x.s(), y.s(), z.s(), w.s()),
            _ => unreachable!()
        }
    }
}
