use dust_byt::{Func, Instruction, Krate, OpABx, OpAbc};
use dust_ctxt::BytCtx;
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

    pub fn exec_func(&mut self, func: &'byt Func<'byt>, ctx: BytCtx<'byt, '_>) {
        // println!("==== new function ====");
        // self.stack.print(&self);

        for (i, instr) in func.instrs.iter().copied().enumerate() {
            let instr = Instruction::from(instr);

            // instr.print(i, func, ctx.gcx);

            self.exec_instr(instr, func, ctx);

            // self.stack.print(&self);
        }
    }

    fn exec_instr(&mut self, instr: Instruction, func: &'byt Func<'byt>, ctx: BytCtx<'byt, '_>) {
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

                    let old_top = self.stack.top();
                    self.stack.set_top(a as usize);
                    self.exec_func(func, ctx);
                    self.stack.set_top(old_top);
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
