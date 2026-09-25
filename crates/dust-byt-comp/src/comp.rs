use ahash::HashMap;
use dust_byt::{Chunk, Instr, Instruction, OpABx, OpAbc};
use dust_hir::{Binary, Block, Expr, Func, Let, Literal, Stmt};
use miette::Result;
use utils::{Ident, Lit};

mod stack;

pub fn comp_main<'hir, 'byt, 'gcx>(krate: &'hir dust_hir::Krate<'hir>) -> Result<Chunk> {
    Ok(comp_func(krate.main))
}

fn comp_func<'hir>(func: &'hir Func<'hir>) -> Chunk {
    let mut comp_func = CompChunk::default();

    comp_func.comp_block(func.block);

    dbg!(&comp_func);
    Chunk::from(&comp_func)
}

/// Output & in-flight state for when compiling a chunk
/// TODO: Define what a Chunk is
#[derive(Default, Debug)]
pub struct CompChunk {
    // Output into the final chunk
    instrs: Vec<Instruction>,
    consts: Vec<Lit>,

    // Intermediaries which are not output into the final chunk
    /// Where on the stack is the value of this local?
    locals: HashMap<Ident, u8>,
    next_stack: u8,
}

impl CompChunk {
    fn next_stack(&mut self) -> u8 {
        let ret = self.next_stack;
        self.next_stack += 1;
        ret
    }

    fn next_const(&mut self, v: Lit) -> u32 {
        // TODO: Some sort of de-duplication

        let idx = self.consts.len();

        self.consts.push(v);

        idx as u32
    }

    fn comp_block<'hir>(&mut self, block: &'hir Block<'hir>) {
        for &stmt in block.stmts.iter() {
            self.comp_stmt(stmt);
        }
    }

    fn comp_stmt<'hir>(&mut self, stmt: &'hir Stmt<'hir>) {
        match *stmt {
            Stmt::Let(r#let) => self.comp_let(r#let),
            Stmt::Expr(expr) => {
                let _ = self.comp_expr(expr);
            }
        }
    }

    fn comp_let<'hir>(&mut self, r#let: &'hir Let) {
        let Some(expr) = r#let.expr.copied() else {
            // No bytecode necessary
            return;
        };

        let expr = self.comp_expr(&expr);

        self.locals.insert(r#let.ident, expr);
    }

    /// An expression compiles to some instructions
    /// pertaining to a stack index, which is returned
    fn comp_expr<'hir>(&mut self, expr: &'hir Expr) -> u8 {
        match *expr {
            Expr::Call(call) => todo!(),
            Expr::Binary(binary) => self.comp_bin(binary),
            Expr::Unary(unary) => todo!(),
            Expr::Literal(literal) => self.comp_lit(literal),
            Expr::Assign => todo!(),
            Expr::Res(res) => match *res {
                dust_resolve::Res::Local(local) => self.comp_local(&local),
            },
            Expr::Block(block) => todo!(),
            Expr::If => todo!(),
            Expr::Loop => todo!(),
        }
    }

    fn comp_local(&mut self, local: &Ident) -> u8 {
        *self.locals.get(local).unwrap()
    }

    fn comp_bin<'hir>(&mut self, bin: &'hir Binary) -> u8 {
        let lhs = self.comp_expr(bin.lhs);
        let rhs = self.comp_expr(bin.rhs);

        let a = self.next_stack();

        let instr = Instruction::Abc {
            operation: OpAbc::Add,
            a,
            b: lhs as u16,
            c: rhs as u16,
        };

        self.instrs.push(instr);

        a
    }

    fn comp_lit<'hir>(&mut self, lit: &'hir Literal) -> u8 {
        let a = self.next_stack();

        let instr = Instruction::ABx {
            operation: OpABx::LoadK,
            a,
            bx: self.next_const(*lit.lit),
        };

        self.instrs.push(instr);

        a
    }
}

impl From<&CompChunk> for Chunk {
    fn from(comp: &CompChunk) -> Self {
        Chunk {
            instrs: comp.instrs.iter().copied().map(Instr::from).collect(),
            consts: comp.consts.iter().copied().collect(),
        }
    }
}
