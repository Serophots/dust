use dust_byt::{Chunk, Instruction, OpABx, OpAbc};
use utils::Lit;

struct Stack {
    s: [Lit; ]
}

/// State which can be used to execute chunks
pub struct VirtualMachine<'a> {
    pub stack: Vec<Lit>,
    chunk: &'a Chunk,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        VirtualMachine {
            stack: Vec::with_capacity(u8::MAX as usize),
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
