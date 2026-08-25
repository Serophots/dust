use miette::SourceSpan;

pub mod error;
pub mod lexer;
pub mod parser;

mod ops;
mod token;

pub(crate) fn transpose<T, E>(opt: Option<&Result<T, E>>) -> Result<Option<&T>, &E> {
    match opt {
        Some(Ok(t)) => Ok(Some(t)),
        Some(Err(err)) => Err(err),
        None => Ok(None),
    }
}

pub(crate) fn combine_src(src1: SourceSpan, src2: SourceSpan) -> SourceSpan {
    let lower = std::cmp::min(src1.offset(), src2.offset());
    let upper = std::cmp::max(src1.offset() + src1.len(), src2.offset() + src2.len());
    SourceSpan::from((lower, upper - lower))
}
