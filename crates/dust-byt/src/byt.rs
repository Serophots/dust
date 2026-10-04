//! Bytecode encoding is based on (Lua 5.0)[https://www.lua.org/doc/jucs05.pdf]
//!
//! R(X)        Xth register
//! K(X)        Xth constant
//! RK(X)       if _ { R(X) } else { K(X_) }
#![feature(const_trait_impl, const_convert)]

use utils::{Box, Ident, Lit};

mod instr;

pub use instr::*;

/// A compiled, interprettable function
#[derive(Clone, PartialEq, serde::Serialize, Debug)]
pub struct Func<'byt> {
    pub ident: Ident,

    #[serde(with = "utils::boxed_slice_serialize_with")]
    pub instrs: Box<'byt, [Instr]>, // TODO: Make these boxed slices when allocator api is a bit more ergonomic
    // TODO: Can constants exist globally to the krate, and not duplicated into each chunk which uses them
    #[serde(with = "utils::boxed_slice_serialize_with")]
    pub consts: Box<'byt, [Lit]>,
}

#[derive(Clone, PartialEq, serde::Serialize, Debug)]
/// A compiled, interporettable krate
pub struct Krate<'byt> {
    pub main: &'byt Func<'byt>,
    #[serde(with = "utils::boxed_slice_serialize_with")]
    pub funcs: Box<'byt, [&'byt Func<'byt>]>,
}
