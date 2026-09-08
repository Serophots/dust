use std::range::RangeInclusive;

pub trait Op {
    fn op(&self) -> u8;
}

const OP_ABC: RangeInclusive<u8> = RangeInclusive::from(0..=20);
const OP_ABX: RangeInclusive<u8> = RangeInclusive::from(21..=40);
const OP_ASBX: RangeInclusive<u8> = RangeInclusive::from(41..=60);

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
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

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
#[repr(u8)]
pub enum OpABx {
    TestABx = OP_ABX.start,
}

impl Op for OpABx {
    fn op(&self) -> u8 {
        (*self).into()
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, num_enum::IntoPrimitive, num_enum::TryFromPrimitive,
)]
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
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
        match self {
            Instruction::Abc { operation, a, b, c } => {
                let c = ((*c as u32) << Instr::LOWEST_ONE_C) & Instr::MASK_C;
                let b = ((*b as u32) << Instr::LOWEST_ONE_B) & Instr::MASK_B;
                let a = ((*a as u32) << Instr::LOWEST_ONE_A) & Instr::MASK_A;
                let op = (operation.op() as u32) & Instr::MASK_OP;

                Instr(op | a | b | c)
            }
            Instruction::ABx { operation, a, bx } => {
                let bx = (*bx << Instr::LOWEST_ONE_BX) & Instr::MASK_BX;
                let a = ((*a as u32) << Instr::LOWEST_ONE_A) & Instr::MASK_A;
                let op = (operation.op() as u32) & Instr::MASK_OP;

                Instr(op | a | bx)
            }
            Instruction::AsBx { operation, a, sbx } => {
                let sbx = ((*sbx as u32) << Instr::LOWEST_ONE_BX) & Instr::MASK_BX;
                let a = ((*a as u32) << Instr::LOWEST_ONE_A) & Instr::MASK_A;
                let op = (operation.op() as u32) & Instr::MASK_OP;

                Instr(op | a | sbx)
            }
        }
    }
}

impl From<Instruction> for Instr {
    fn from(value: Instruction) -> Self {
        value.encode()
    }
}

/// A compact bytecode instruction, encoded in 32 bits
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Instr(u32);

impl Instr {
    const MASK_OP: u32 = 0b0000_0000_0000_0000_0000_0000_0011_1111;
    const MASK_A: u32 = 0b0000_0000_0000_0000_0011_1111_1100_0000;
    const MASK_B: u32 = 0b0000_0000_0111_1111_1100_0000_0000_0000;
    const MASK_C: u32 = 0b1111_1111_1000_0000_0000_0000_0000_0000;
    const MASK_BX: u32 = Self::MASK_B | Self::MASK_C;

    const LOWEST_ONE_A: u32 = Self::MASK_A.lowest_one().unwrap();
    const LOWEST_ONE_B: u32 = Self::MASK_B.lowest_one().unwrap();
    const LOWEST_ONE_C: u32 = Self::MASK_C.lowest_one().unwrap();
    const LOWEST_ONE_BX: u32 = Self::MASK_BX.lowest_one().unwrap();

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
        (self.0 & Self::MASK_OP) as u8
    }

    #[inline(always)]
    fn a(&self) -> u8 {
        ((self.0 & Self::MASK_A) >> Self::LOWEST_ONE_A) as u8
    }

    #[inline(always)]
    fn b(&self) -> u16 {
        ((self.0 & Self::MASK_B) >> Self::LOWEST_ONE_B) as u16
    }

    #[inline(always)]
    fn c(&self) -> u16 {
        ((self.0 & Self::MASK_C) >> Self::LOWEST_ONE_C) as u16
    }

    #[inline(always)]
    fn bx(&self) -> u32 {
        ((self.0 & Self::MASK_BX) >> Self::LOWEST_ONE_BX) as u32
    }

    #[inline(always)]
    fn sbx(&self) -> i32 {
        let raw = (((self.0 & Self::MASK_BX) >> Self::LOWEST_ONE_BX) as u32) as i32;
        ((raw << Self::LOWEST_ONE_BX) as i32) >> Self::LOWEST_ONE_BX
    }
}

impl core::fmt::Debug for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.decode().fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Instruction, OpABx, OpAbc, OpAsBx};

    #[test]
    fn test_instr() {
        let abc = Instruction::Abc {
            operation: OpAbc::Div,
            a: 44,
            b: 258,
            c: 259,
        };

        assert_eq!(abc, abc.encode().decode());

        let abx = Instruction::ABx {
            operation: OpABx::TestABx,
            a: 44,
            bx: 555,
        };

        assert_eq!(abx, abx.encode().decode());

        let asbx = Instruction::AsBx {
            operation: OpAsBx::TestAsBx,
            a: 44,
            sbx: -555,
        };

        assert_eq!(asbx, asbx.encode().decode());
    }
}
