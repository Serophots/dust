use dust_byt::{Func, Instruction, Op, OpABx, OpAbc};
use utils::Lit;

mod stack;

pub use stack::*;

/// State which can be used to execute chunks
pub struct VirtualMachine<'a> {
    pub stack: Stack,
    chunk: &'a Func,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(chunk: &'a Func) -> Self {
        VirtualMachine {
            stack: Stack::new(),
            chunk,
        }
    }

    pub fn exec_chunk(&mut self, chunk: &'a Func) {
        self.chunk = chunk;

        for instr in chunk.instrs.iter().copied() {
            self.exec_instr(Instruction::from(instr));
        }
    }

    fn exec_instr(&mut self, instr: Instruction) {
        match instr {
            Instruction::Abc { operation, a, b, c } => match operation {
                OpAbc::Move => {
                    let _ = c;
                    self.stack[a as usize] = self.stack[b as usize];
                }
                OpAbc::Add => {
                    self.stack[a as usize] =
                        Lit::add(self.stack[b as usize], self.stack[c as usize]).unwrap();
                }
                OpAbc::Sub => {
                    self.stack[a as usize] =
                        Lit::sub(self.stack[b as usize], self.stack[c as usize]).unwrap();
                }
                OpAbc::Mul => {
                    self.stack[a as usize] =
                        Lit::mul(self.stack[b as usize], self.stack[c as usize]).unwrap();
                }
                OpAbc::Div => {
                    self.stack[a as usize] =
                        Lit::div(self.stack[b as usize], self.stack[c as usize]).unwrap();
                }
                OpAbc::Eq => {
                    self.stack[a as usize] = Lit::Bool(std::cmp::PartialEq::eq(
                        &self.stack[b as usize],
                        &self.stack[c as usize],
                    ));
                }
                OpAbc::NEq => {
                    self.stack[a as usize] = Lit::Bool(std::cmp::PartialEq::ne(
                        &self.stack[b as usize],
                        &self.stack[c as usize],
                    ));
                }
                OpAbc::Greater => {
                    self.stack[a as usize] = Lit::Bool(std::cmp::PartialOrd::gt(
                        &self.stack[b as usize],
                        &self.stack[c as usize],
                    ));
                }
                OpAbc::GreaterEqual => {
                    self.stack[a as usize] = Lit::Bool(std::cmp::PartialOrd::ge(
                        &self.stack[b as usize],
                        &self.stack[c as usize],
                    ));
                }
                OpAbc::Lesser => {
                    self.stack[a as usize] = Lit::Bool(std::cmp::PartialOrd::lt(
                        &self.stack[b as usize],
                        &self.stack[c as usize],
                    ));
                }
                OpAbc::LesserEqual => {
                    self.stack[a as usize] = Lit::Bool(std::cmp::PartialOrd::le(
                        &self.stack[b as usize],
                        &self.stack[c as usize],
                    ));
                }
                OpAbc::And => {
                    self.stack[a as usize] =
                        Lit::logical_and(self.stack[b as usize], self.stack[c as usize]).unwrap();
                }
                OpAbc::Or => {
                    self.stack[a as usize] =
                        Lit::logical_or(self.stack[b as usize], self.stack[c as usize]).unwrap();
                }
                OpAbc::Call => todo!(),
            },
            Instruction::ABx { operation, a, bx } => match operation {
                OpABx::LoadK => {
                    self.stack[a as usize] = self.chunk.consts[bx as usize];
                }
                OpABx::LoadNil => {
                    self.stack[a as usize..=a as usize + bx as usize].fill(Lit::Nil);
                }
            },
            Instruction::AsBx { operation, a, sbx } => todo!(),
        }
    }
}
