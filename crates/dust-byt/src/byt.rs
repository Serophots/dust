#![feature(const_trait_impl, const_convert)]
//! Bytecode encoding is based on (Lua 5.0)[https://www.lua.org/doc/jucs05.pdf]
//!
//! R(X)        Xth register
//! K(X)        Xth constant
//! RK(X)       if _ { R(X) } else { K(X_) }
//!

use std::range::RangeInclusive;

/// An index
pub struct RegIdx(usize);

pub trait Op {
    fn op(&self) -> u8;
}

const OP_ABC: RangeInclusive<u8> = RangeInclusive::from(0..=20);
const OP_ABX: RangeInclusive<u8> = RangeInclusive::from(21..=40);
const OP_ASBX: RangeInclusive<u8> = RangeInclusive::from(41..=60);

#[derive(Copy, Clone, num_enum::IntoPrimitive, num_enum::TryFromPrimitive)]
#[repr(u8)]
pub enum OpAbc {
    /// R(A) := R(B)
    Move = OP_ABC.start,
    /// R(A) := RK(B) + RK(C)
    Add,
    /// R(A) := RK(B) - RK(C)
    Sub,
    /// R(A) := RK(B) * RK(C)
    Mul,
    /// R(A) := RK(B) / RK(C)
    Div,
}

impl Op for OpAbc {
    fn op(&self) -> u8 {
        (*self).into()
    }
}

#[derive(Copy, Clone, num_enum::IntoPrimitive, num_enum::TryFromPrimitive)]
#[repr(u8)]
pub enum OpABx {
    TestABx = OP_ABX.start,
}

impl Op for OpABx {
    fn op(&self) -> u8 {
        (*self).into()
    }
}

#[derive(Copy, Clone, num_enum::IntoPrimitive, num_enum::TryFromPrimitive)]
#[repr(u8)]
pub enum OpAsBx {
    TestAsBx = OP_ASBX.start,
}

impl Op for OpAsBx {
    fn op(&self) -> u8 {
        (*self).into()
    }
}

/// An expanded bytecode instruction
pub enum Instruction {
    /// Arguments: A, plus two 8 bit fields B and C
    Abc {
        /// encoded into 6 bits
        operation: OpAbc,
        /// unsigned, encoded into 8 bits
        a: u8,
        /// unsigned, encoded into 9 bits
        b: u16,
        /// unsigned, encoded into 9 bits
        c: u16,
    },
    /// Arguments: A, plus one unsigned 18 bit field B
    ABx {
        /// encoded into 6 bits
        operation: OpABx,
        /// unsigned, encoded into 8 bits
        a: u8,
        /// unsigned, encoded into 18 bits
        bx: u32,
    },
    /// Arguments: A, plus one signed 18 bit field B
    AsBx {
        /// encoded into 6 bits
        operation: OpAsBx,
        /// unsigned, encoded into 8 bits
        a: u8,
        /// signed, encoded into 18 bits
        sbx: i32,
    },
}

impl Instruction {
    pub fn op(&self) -> u8 {
        match self {
            Instruction::Abc { operation, .. } => (*operation).op(),
            Instruction::ABx { operation, .. } => (*operation).op(),
            Instruction::AsBx { operation, .. } => (*operation).op(),
        }
    }

    pub fn a(&self) -> u8 {
        match self {
            Instruction::Abc { a, .. } => *a,
            Instruction::ABx { a, .. } => *a,
            Instruction::AsBx { a, .. } => *a,
        }
    }

    pub fn encode(&self) -> Instr {
        todo!()
    }
}

impl From<Instruction> for Instr {
    fn from(value: Instruction) -> Self {
        value.encode()
    }
}

/// A compact bytecode instruction, encoded in 32 bits
pub struct Instr(u32);

impl Instr {
    const MASK_OP___: u32 = 0b0000_0000_0000_0000_0000_0000_0011_1111;
    const MASK_A____: u32 = 0b0000_0000_0000_0000_0011_1111_1100_0000;
    const MASK_ABC_B: u32 = 0b0000_0000_0111_1111_1100_0000_0000_0000;
    const MASK_ABC_C: u32 = 0b1111_1111_1000_0000_0000_0000_0000_0000;
    const MASK_ABX_B: u32 = Self::MASK_ABC_B | Self::MASK_ABC_C;

    pub fn decode(&self) -> Instruction {
        match self.op() {
            op if OP_ABC.contains(&op) => Instruction::Abc {
                operation: OpAbc::try_from(op).unwrap(),
                a: self.a(),
                b: self.b(),
                c: self.c(),
            },
            op if OP_ABX.contains(&op) => Instruction::ABx {
                operation: OpABx::try_from(op).unwrap(),
                a: self.a(),
                bx: self.bx(),
            },
            op if OP_ASBX.contains(&op) => Instruction::AsBx {
                operation: OpAsBx::try_from(op).unwrap(),
                a: self.a(),
                sbx: self.sbx(),
            },
            _ => unreachable!("unrecognised opcode"),
        }
    }

    // First 6 bits
    #[inline(always)]
    fn op(&self) -> u8 {
        (self.0 & Self::MASK_OP___) as u8
    }

    fn a(&self) -> u8 {
        ((self.0 & Self::MASK_A____) >> Self::MASK_A____.lowest_one().unwrap()) as u8
    }

    fn b(&self) -> u16 {
        ((self.0 & Self::MASK_ABC_B) >> Self::MASK_ABC_B.lowest_one().unwrap()) as u16
    }

    fn c(&self) -> u16 {
        ((self.0 & Self::MASK_ABC_C) >> Self::MASK_ABC_C.lowest_one().unwrap()) as u16
    }

    fn bx(&self) -> u32 {
        ((self.0 & Self::MASK_ABX_B) >> Self::MASK_ABX_B.lowest_one().unwrap()) as u32
    }

    fn sbx(&self) -> i32 {
        ((self.0 & Self::MASK_ABX_B) >> Self::MASK_ABX_B.lowest_one().unwrap()) as i32
    }
}

// pub enum Instr {
//     /// R[a] := R[b]
//     Move {},
//     /// R[a] := R[b]
//     Add {
//         // store:
//     },
// }
