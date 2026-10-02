use std::ops::{Index, IndexMut};

use dust_byt::{Chunk, Instruction, OpABx, OpAbc};
use utils::Lit;

pub struct Stack {
    s: Vec<Lit>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            s: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl Index<usize> for Stack {
    type Output = Lit;

    fn index(&self, i: usize) -> &Self::Output {
        match self.s.get(i as usize) {
            Some(l) => l,
            None => &Lit::Nil,
        }
    }
}

impl IndexMut<usize> for Stack {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.s.resize(i + 1, Lit::Nil);
        &mut self.s[i as usize]
    }
}

impl core::fmt::Debug for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.s.fmt(f)
    }
}

/// State which can be used to execute chunks
pub struct VirtualMachine<'a> {
    pub stack: Stack,
    chunk: &'a Chunk,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        VirtualMachine {
            stack: Stack::new(),
            chunk,
        }
    }

    pub fn exec_chunk(&mut self, chunk: &'a Chunk) {
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
                OpAbc::Sub => {}
                OpAbc::Mul => {}
                OpAbc::Div => {}
            },
            Instruction::ABx { operation, a, bx } => match operation {
                OpABx::LoadK => {
                    self.stack[a as usize] = self.chunk.consts[bx as usize];
                }
            },
            Instruction::AsBx { operation, a, sbx } => todo!(),
        }
    }
}
