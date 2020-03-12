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
