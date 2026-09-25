//! Bytecode encoding is based on (Lua 5.0)[https://www.lua.org/doc/jucs05.pdf]
//!
//! R(X)        Xth register
//! K(X)        Xth constant
//! RK(X)       if _ { R(X) } else { K(X_) }
#![feature(const_trait_impl, const_convert)]

mod instr;

pub use instr::*;
use utils::Lit;

/// A chunk of bytecode which can be interpretted
pub struct Chunk {
    pub instrs: Box<[Instr]>,
    pub consts: Box<[Lit]>,
}
