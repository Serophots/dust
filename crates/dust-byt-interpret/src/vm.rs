use dust_byt::{Func, Instruction, Krate, OpABx, OpAbc};
use utils::Lit;

mod stack;

pub use stack::*;

/// State which can be used to execute chunks
pub struct VirtualMachine<'byt> {
    pub stack: Stack,
    krate: &'byt Krate<'byt>,
}

impl<'byt> VirtualMachine<'byt> {
    pub fn new(krate: &'byt Krate<'byt>) -> Self {
        VirtualMachine {
            stack: Stack::new(),
            krate,
        }
    }

    pub fn exec_func(&mut self, func: &'byt Func<'byt>) {
        for instr in func.instrs.iter().copied() {
            self.exec_instr(Instruction::from(instr), func);
        }
    }

    fn exec_instr(&mut self, instr: Instruction, func: &'byt Func<'byt>) {
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
                OpAbc::Call => {
                    let func = self.krate.funcs[b as usize];

                    assert_eq!(c, 0);

                    self.exec_func(func);
                }
            },
            Instruction::ABx { operation, a, bx } => match operation {
                OpABx::LoadK => {
                    self.stack[a as usize] = func.consts[bx as usize];
                }
                OpABx::LoadNil => {
                    self.stack[a as usize..=a as usize + bx as usize].fill(Lit::Nil);
                }
            },
            Instruction::AsBx { operation, a, sbx } => todo!(),
        }
    }
}
