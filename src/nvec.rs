use crate::vval::VVal;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::ops::{Add, Sub, Div, Mul};


/// WLambda supports Integer and Float vectors in two, three, and four dimensions.
#[derive(Debug, Copy, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum NVec<N: NVecNum> {
    Vec2(N, N),
    Vec3(N, N, N),
    Vec4(N, N, N, N),
}
#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum NVecDim {
    Two = 2,
    Three = 3,
    Four = 4,
}

/*
impl<N: NVecNum> PartialEq for NVec<N> {
    fn eq(&self, o: &Self) -> bool {
        let (lx, ly, lz, lw) = self.clone().into_tpl();
        let (rx, ry, rz, rw) = o.clone().into_tpl();
        (lx, ly, lz.unwrap_or(N::zero()), lw.unwrap_or(N::zero()))
         == (rx, ry, rz.unwrap_or(N::zero()), rw.unwrap_or(N::zero()))
    }
}*/
use NVec::*;

pub trait NVecNum: Sized + Copy + Clone + PartialEq {
    /// Returns a letter representing this type
    fn sign() -> char;

    fn from_vval(v: &VVal) -> Self;
    fn into_vval(self)     -> VVal;

    fn into_flt(self)      -> f64;
    fn from_flt(f: f64)    -> Self;

    /// When added/subtracted to something, has no effect
    fn zero() -> Self;

    fn add(self, o: Self) -> Self;
    fn sub(self, o: Self) -> Self;
    fn mul(self, o: Self) -> Self;
    fn div(self, o: Self) -> Self;

    fn from_ivec(ivec: NVec<i64>)       -> NVec<Self>;
    fn into_fvec(s: NVec<Self>)         -> NVec<f64>;
    fn from_fvec(fvec: NVec<f64>)       -> NVec<Self>;
    fn from_fvec_round(fvec: NVec<f64>) -> NVec<Self>;
}

impl NVecNum for i64 {
    #[inline]
    fn sign()              -> char { 'i' }

    #[inline]
    fn from_vval(v: &VVal) -> Self { v.i() }
    #[inline]
    fn into_vval(self)     -> VVal { VVal::Int(self) }

    #[inline]
    fn into_flt(self)      -> f64  { self as f64 }
    #[inline]
    fn from_flt(f: f64)    -> Self { f as i64 }

    #[inline]
    fn zero()              -> Self { 0 }

    #[inline]
    fn add(self, o: Self)  -> Self { self + o }
    #[inline]
    fn sub(self, o: Self)  -> Self { self - o }
    #[inline]
    fn mul(self, o: Self)  -> Self { self * o }
    #[inline]
    fn div(self, o: Self)  -> Self { self / o }

    #[inline]
    fn from_ivec(i: NVec<i64>)       -> NVec<Self> { i }
    #[inline]
    fn into_fvec(i: NVec<Self>)      -> NVec<f64>  { NVec::from_vval_tpl(i.into_vval_tpl()).unwrap() }
    #[inline]
    fn from_fvec(f: NVec<f64>)       -> NVec<Self> { NVec::from_vval_tpl(f.into_vval_tpl()).unwrap() }
    #[inline]
    fn from_fvec_round(f: NVec<f64>) -> NVec<Self> {
        let (x, y, z, w) = f.into_tpl();
        NVec::from_tpl((
            x.round() as i64,
            y.round() as i64,
            z.map(|z| z.round() as i64),
            w.map(|w| w.round() as i64)
        )).unwrap()
    }
}

impl NVecNum for f64 {
    #[inline]
    fn sign()              -> char { 'f' }

    #[inline]
    fn from_vval(v: &VVal) -> Self { v.f() }
    #[inline]
    fn into_vval(self)     -> VVal { VVal::Flt(self) }

    #[inline]
    fn into_flt(self)      -> f64  { self }
    #[inline]
    fn from_flt(f: f64)    -> Self { f }

    #[inline]
    fn zero()              -> Self { 0.0 }

    #[inline]
    fn add(self, o: Self)  -> Self { self + o }
    #[inline]
    fn sub(self, o: Self)  -> Self { self - o }
    #[inline]
    fn mul(self, o: Self)  -> Self { self * o }
    #[inline]
    fn div(self, o: Self)  -> Self { self / o }

    #[inline]
    fn from_ivec(i: NVec<i64>)       -> NVec<Self> { NVec::from_vval_tpl(i.into_vval_tpl()).unwrap() }
    #[inline]
    fn into_fvec(f: NVec<Self>)      -> NVec<f64>  { f }
    #[inline]
    fn from_fvec(f: NVec<f64>)       -> NVec<Self> { f }
    #[inline]
    fn from_fvec_round(f: NVec<f64>) -> NVec<Self> { f }
}

impl AsRef<VVal> for VVal {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl<N: NVecNum> NVec<N> {
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
    pub fn dims(&self) -> NVecDim {
        match self {
            Vec2(_, _)       => NVecDim::Two,
            Vec3(_, _, _)    => NVecDim::Three,
            Vec4(_, _, _, _) => NVecDim::Four,
        }
    }

    #[inline]
    pub fn into_tpl(self) -> (N, N, Option<N>, Option<N>) {
        match self {
            Vec2(x, y)       => (x, y, None   , None),
            Vec3(x, y, z)    => (x, y, Some(z), None),
            Vec4(x, y, z, w) => (x, y, Some(z), Some(w)),
        }
    }

    #[inline]
    /// A tuple of four elements representing the components of this vector.
    /// If a component wasn't available, a `0` takes its place.
    pub fn into_zero_tpl(self) -> (N, N, N, N) {
        let o = N::zero();
        match self {
            Vec2(x, y)       => (x, y, o, o),
            Vec3(x, y, z)    => (x, y, z, o),
            Vec4(x, y, z, w) => (x, y, z, w),
        }
    }

    #[inline]
    pub fn into_vval_tpl(self) -> (VVal, VVal, Option<VVal>, Option<VVal>) {
        match self {
            Vec2(x, y)       => (x.into_vval(), y.into_vval(), None   , None),
            Vec3(x, y, z)    => (x.into_vval(), y.into_vval(), Some(z.into_vval()), None),
            Vec4(x, y, z, w) => (x.into_vval(), y.into_vval(), Some(z.into_vval()), Some(w.into_vval())),
        }
    }

    #[inline]
    pub fn from_tpl(tpl: (N, N, Option<N>, Option<N>)) -> Option<Self> {
        Some(match tpl {
            (x, y, None   , None)    => Vec2(x, y),
            (x, y, Some(z), None)    => Vec3(x, y, z),
            (x, y, Some(z), Some(w)) => Vec4(x, y, z, w),
            _ => return None
        })
    }
    
    #[inline]
    pub fn from_vval_tpl<W: AsRef<VVal>>((x, y, z, w): (W, W, Option<W>, Option<W>)) -> Option<Self> {
        Some(match (x.as_ref(), y.as_ref(), z, w) {
            (x, y, None   , None)    =>
                Vec2(N::from_vval(x), N::from_vval(y)),
            (x, y, Some(z), None)    =>
                Vec3(N::from_vval(x), N::from_vval(y), N::from_vval(z.as_ref())),
            (x, y, Some(z), Some(w)) =>
                Vec4(N::from_vval(x), N::from_vval(y), N::from_vval(z.as_ref()), N::from_vval(w.as_ref())),
            _ => return None
        })
    }

    #[inline]
    pub fn s(&self) -> String {
        match self.into_vval_tpl() {
            (x, y, None,    None)    => format!("${}({},{})", N::sign(), x.s(), y.s()),
            (x, y, Some(z), None)    => format!("${}({},{},{})", N::sign(), x.s(), y.s(), z.s()),
            (x, y, Some(z), Some(w)) => format!("${}({},{},{},{})", N::sign(), x.s(), y.s(), z.s(), w.s()),
            _ => unreachable!()
        }
    }

    #[inline]
    /// Converts this vector into one with three dimensions, discarding the unnecessary values.
    pub fn vec2(self) -> Self {
        match self {
            Vec2(_, _)       => self,
            Vec3(x, y, _)    => Vec2(x, y),
            Vec4(x, y, _, _) => Vec2(x, y),
        }
    }

    #[inline]
    /// Converts this vector into one with three dimensions, discarding the unnecessary values
    /// and filling in the missing values with 0s if necessary.
    pub fn vec3(self) -> Self {
        match self {
            Vec2(x, y)       => Vec3(x, y, N::zero()),
            Vec3(_, _, _)    => self,
            Vec4(x, y, z, _) => Vec3(x, y, z),
        }
    }

    /// Converts this vector into one with four dimensions,
    /// filling in the missing values with 0s if necessary.
    #[inline]
    pub fn vec4(self) -> Self {
        let o = N::zero();
        match self {
            Vec2(x, y)       => Vec4(x, y, o, o),
            Vec3(x, y, z)    => Vec4(x, y, z, o),
            Vec4(_, _, _, _) => self,
        }
    }


    #[inline]
    pub fn mag2(&self) -> f64 {
        match self {
            Vec2(x, y)       =>
                x.into_flt().powi(2) + y.into_flt().powi(2),
            Vec3(x, y, z)    =>
                x.into_flt().powi(2) + y.into_flt().powi(2) + z.into_flt().powi(2),
            Vec4(x, y, z, w) =>
                x.into_flt().powi(2) + y.into_flt().powi(2) + z.into_flt().powi(2) + w.into_flt().powi(2),
        }
    }

    #[inline]
    pub fn mag(&self) -> f64 {
        self.mag2().sqrt()
    }

    #[inline]
    pub fn norm(self) -> Self {
        let m = N::from_flt(self.mag());
        if m == N::zero() {
            self
        } else {
            self / m
        }
    }

    #[inline]
    pub fn dot(self, o: NVec<N>) -> N {
        let max_dims = self.dims().max(o.dims());
        let (lx, ly, lz, lw) = self.into_zero_tpl();
        let (rx, ry, rz, rw) = o.into_zero_tpl();

        match max_dims {
            NVecDim::Two   => lx.mul(rx).add(ly.mul(ry)),
            NVecDim::Three => lx.mul(rx).add(ly.mul(ry)).add(lz.mul(rz)),
            NVecDim::Four  => lx.mul(rx).add(ly.mul(ry)).add(lz.mul(rz)).add(lw.mul(rw)),
        }
    }

    #[inline]
    pub fn cross(self, o: NVec<N>) -> Self {
        let a = self.into_zero_tpl();
        let b = o.into_zero_tpl();

        Vec3(
            a.1.mul(b.2).sub(a.2.mul(b.1)),
            a.2.mul(b.0).sub(a.0.mul(b.2)),
            a.0.mul(b.1).sub(a.1.mul(b.0)),
        )
    }

    #[inline]
    pub fn lerp(self, o: NVec<N>, t: f64) -> Self {
        N::from_fvec_round(
            (N::into_fvec(self) * (1.0 - t))
            + (N::into_fvec(o) * t)
        )
    }

    #[inline]
    /// The resulting NVec will almost always have a length of 1,
    /// except for in cases where `o` and `self` are collinear opposites.
    /// To work around this, the resulting vector may be normalized.
    /// 
    /// # Panics
    /// Panics if input vectors aren't unit vectors.
    pub fn slerp(self, o: NVec<N>, t: f64) -> Self {
        let (p0, p1) = {
            let (p0, p1) = (N::into_fvec(self), N::into_fvec(o));
            // work around the edge case wherein p0 and p1 are collinear opposites
            if (p0 + p1).mag2() == 0.0 {
                ((p0 + Vec2(1e-5, 1e-5)).norm(), p1)
            } else {
                (p0, p1)
            }
        };
        let omega = p0.dot(p1).acos();
        let sin_omega = omega.sin();
        N::from_fvec_round(
            (p0 * (((1.0 - t) * omega).sin() / sin_omega))
            + (p1 * ((t * omega).sin() / sin_omega))
        )
    }
}

macro_rules! euler_binop { ( $( $trait:ident | $fn:ident ; )* ) => { $(
    impl<N: NVecNum> $trait for NVec<N> {
        type Output = Self;

        #[inline]
        fn $fn(self, o: NVec<N>) -> NVec<N> {
            let max_dims = self.dims().max(o.dims());
            let (lx, ly, lz, lw) = self.into_zero_tpl();
            let (rx, ry, rz, rw) = o.into_zero_tpl();

            match max_dims {
                NVecDim::Two   => Vec2(lx.$fn(rx), ly.$fn(ry)),
                NVecDim::Three => Vec3(lx.$fn(rx), ly.$fn(ry), lz.$fn(rz)),
                NVecDim::Four  => Vec4(lx.$fn(rx), ly.$fn(ry), lz.$fn(rz), lw.$fn(rw)),
            }
        }
    }
)* } }
euler_binop! {
    Add | add;
    Sub | sub;
}

macro_rules! scalar_binop { ( $( $trait:ident | $fn:ident ; )* ) => { $(
    impl<N: NVecNum> $trait<N> for NVec<N> {
        type Output = Self;

        #[inline]
        fn $fn(self, o: N) -> NVec<N> {
            match self {
                Vec2(x, y)       => Vec2(x.$fn(o), y.$fn(o)),
                Vec3(x, y, z)    => Vec3(x.$fn(o), y.$fn(o), z.$fn(o)),
                Vec4(x, y, z, w) => Vec4(x.$fn(o), y.$fn(o), z.$fn(o), w.$fn(o)),
            }
        }
    }
)* } }
scalar_binop! {
    Mul | mul;
    Div | div;
}
