#![feature(peekable_next_if_map)]

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
