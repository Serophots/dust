use miette::SourceSpan;

mod context;
mod source;
mod symbol;
mod token;

pub use context::*;
pub use source::*;
pub use symbol::*;
pub use token::*;

pub fn combine_src(src1: SourceSpan, src2: SourceSpan) -> SourceSpan {
    let lower = std::cmp::min(src1.offset(), src2.offset());
    let upper = std::cmp::max(src1.offset() + src1.len(), src2.offset() + src2.len());
    SourceSpan::from((lower, upper - lower))
}

pub trait TransposeRef<'a, T, E> {
    fn transpose_ref(&self) -> Result<Option<&'a T>, &'a E>;
}

impl<'a, T, E> TransposeRef<'a, T, E> for Option<&'a Result<T, E>> {
    fn transpose_ref(&self) -> Result<Option<&'a T>, &'a E> {
        match self {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }
}
