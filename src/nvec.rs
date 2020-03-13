use crate::VVal;

/// WLambda supports Integer and Float vectors in two, three, and four dimensions.
#[derive(Debug, Clone)]
pub enum NVector<N: NVecNum> {
    Vec2(N, N),
    Vec3(N, N, N),
    Vec4(N, N, N, N),
}
use NVector::*;

pub trait NVecNum: Sized + Copy {
    fn from_vval(v: &VVal) -> Self;

    fn into_vval(self) -> VVal;

    /// Returns a letter representing this type
    fn sign() -> char;

    /// When added to something, has no effect
    fn zero() -> Self;

    fn add(self, o: Self) -> Self;
}

impl NVecNum for f64 {
    fn zero() -> Self {
        0.0
    }
    fn add(self, o: Self) -> Self {
        self + o
    }
    fn from_vval(v: &VVal) -> Self {
        v.f()
    }
    fn into_vval(self) -> VVal {
        VVal::Flt(self)
    }
    fn sign() -> char {
        'f'
    }
}
impl NVecNum for i64 {
    fn zero() -> Self {
        0
    }
    fn add(self, o: Self) -> Self {
        self + o
    }
    fn from_vval(v: &VVal) -> Self {
        v.i()
    }
    fn into_vval(self) -> VVal {
        VVal::Int(self)
    }
    fn sign() -> char {
        'i'
    }
}

impl<N: NVecNum> NVector<N> {
    #[inline]
    pub fn x(&self) -> VVal {
        self.x_raw().into_vval()
    }

    #[inline]
    pub fn x_raw(&self) -> N {
        match self {
            Vec2(x, _)       => *x,
            Vec3(x, _, _)    => *x,
            Vec4(x, _, _, _) => *x,
        }
    }

    #[inline]
    pub fn y(&self) -> VVal {
        self.y_raw().into_vval()
    }

    #[inline]
    pub fn y_raw(&self) -> N {
        match self {
            Vec2(_, y)       => *y,
            Vec3(_, y, _)    => *y,
            Vec4(_, y, _, _) => *y,
        }
    }

    #[inline]
    pub fn z(&self) -> Option<VVal> {
        self.z_raw().map(|v| v.into_vval())
    }

    #[inline]
    pub fn z_raw(&self) -> Option<N> {
        match self {
            Vec2(_, _)       => None,
            Vec3(_, _, z)    => Some(*z),
            Vec4(_, _, z, _) => Some(*z),
        }
    }

    #[inline]
    pub fn w(&self) -> Option<VVal> {
        self.w_raw().map(|v| v.into_vval())
    }

    #[inline]
    pub fn w_raw(&self) -> Option<N> {
        match self {
            Vec2(_, _)       => None,
            Vec3(_, _, _)    => None,
            Vec4(_, _, _, w) => Some(*w),
        }
    }

    #[inline]
    pub fn dimensions(&self) -> u8 {
        match self {
            Vec2(_, _)       => 2,
            Vec3(_, _, _)    => 3,
            Vec4(_, _, _, _) => 4,
        }
    }

    #[inline]
    pub fn tuplify(self) -> (N, N, Option<N>, Option<N>) {
        (
            self.x_raw(),
            self.y_raw(),
            self.z_raw(),
            self.w_raw(),
        )
    }

    #[inline]
    pub fn s(&self) -> String {
        match (self.x(), self.y(), self.z(), self.w()) {
            (x, y, None,    None)    => format!("${}({},{})", N::sign(), x.s(), y.s()),
            (x, y, Some(z), None)    => format!("${}({},{},{})", N::sign(), x.s(), y.s(), z.s()),
            (x, y, Some(z), Some(w)) => format!("${}({},{},{},{})", N::sign(), x.s(), y.s(), z.s(), w.s()),
            _ => unreachable!()
        }
    }
}

impl<N: NVecNum> std::ops::Add for NVector<N> {
    type Output = Self;

    fn add(self, o: NVector<N>) -> NVector<N> {
        let max_dims = self.dimensions().max(o.dimensions());
        let l = self.tuplify();
        let r = o.tuplify();

        match max_dims {
            2 => Vec2(
                l.0.add(r.0),
                l.1.add(r.1)
            ),
            3 => Vec3(
                l.0.add(r.0),
                l.1.add(r.1),
                l.2.unwrap_or(N::zero()).add(r.3.unwrap_or(N::zero()))
            ),
            4 => Vec4(
                l.0.add(r.0),
                l.1.add(r.1),
                l.2.unwrap_or(N::zero()).add(r.2.unwrap_or(N::zero())),
                l.3.unwrap_or(N::zero()).add(r.3.unwrap_or(N::zero()))
            ),
            _ => unreachable!()
        }
    }
}
