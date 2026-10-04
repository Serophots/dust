use ahash::HashMap;
use dust_byt::{Instr, Instruction, OpABx, OpAbc};
use dust_hir::{Binary, Block, Call, Expr, Func, Let, Literal, Stmt};
use miette::{LabeledSpan, Result};
use utils::{Ident, Lit};

pub fn comp_krate<'hir, 'byt, 'gcx>(krate: &'hir dust_hir::Krate<'hir>) -> Result<CompileFunc> {
    Ok(comp_func(krate.main)?)
}

fn comp_func<'hir>(func: &'hir Func<'hir>) -> Result<CompileFunc> {
    let mut comp_func = CompileFunc::default();

    comp_func.comp_block(func.block)?;

    Ok(comp_func)
}

/// State responsible for compiling a function into a bytecode Func.
#[derive(Default, Debug)]
pub struct CompileFunc {
    // Output into the final chunk
    pub instrs: Vec<Instruction>,
    // TODO: Can constants exist globally to the krate, and not duplicated into each chunk which uses them
    pub consts: Vec<Lit>,

    // Intermediaries which are not output into the final chunk
    /// Where on the stack is the value of this local?
    pub locals: HashMap<Ident, u8>,
    pub next_stack: u8,
}

impl CompileFunc {
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

    fn comp_block<'hir>(&mut self, block: &'hir Block<'hir>) -> Result<u8> {
        for &stmt in block.stmts.iter() {
            self.comp_stmt(stmt)?;
        }

        match block.expr {
            Some(expr) => self.comp_expr(expr),
            None => {
                let a = self.next_stack();

                let instr = Instruction::ABx {
                    operation: OpABx::LoadNil,
                    a,
                    bx: 0,
                };

                self.instrs.push(instr);

                Ok(a)
            }
        }
    }

    fn comp_stmt<'hir>(&mut self, stmt: &'hir Stmt<'hir>) -> Result<()> {
        match *stmt {
            Stmt::Let(r#let) => self.comp_let(r#let)?,
            Stmt::Expr(expr) => {
                let _ = self.comp_expr(expr)?;
            }
        }

        Ok(())
    }

    fn comp_let<'hir>(&mut self, r#let: &'hir Let) -> Result<()> {
        let Some(expr) = r#let.expr.copied() else {
            // No bytecode necessary
            return Ok(());
        };

        let expr = self.comp_expr(&expr)?;

        self.locals.insert(r#let.ident, expr);

        Ok(())
    }

    /// An expression compiles to some instructions
    /// pertaining to a stack index, which is returned
    fn comp_expr<'hir>(&mut self, expr: &'hir Expr) -> Result<u8> {
        Ok(match *expr {
            Expr::Call(call) => self.comp_call(call)?,
            Expr::Binary(binary) => self.comp_bin(binary)?,
            Expr::Unary(unary) => todo!(),
            Expr::Literal(literal) => self.comp_lit(literal),
            Expr::Assign => todo!(),
            Expr::Local(local) => self.comp_local(&local),
            Expr::Func(func) => {
                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(func.span, "function expression")],
                    "cannot evaluate a function expr; function expressions should be called."
                ));
            }
            Expr::Block(block) => self.comp_block(block)?,
            Expr::If => todo!(),
            Expr::Loop => todo!(),
        })
    }

    fn comp_call(&mut self, call: &Call) -> Result<u8> {
        // TODO: Allow calling arbritrary expressions
        // which may evaluate to a Function type.

        let a = self.next_stack();

        let callee = match *call.expr {
            Expr::Func(func) => {
                // we need to point to another compiled function dust_byt::Func
                todo!()
            }
            _ => {
                return Err(
                    miette::miette!(
                        labels = vec![LabeledSpan::at(call.span, "cannot call this")],
                        "you can only call a function expression"
                    ), // .with_source_code(self.source.to_owned())
                );
            }
        };

        let instr = Instruction::Abc {
            operation: OpAbc::Call,
            a,
            b: todo!(),
            c: 0, // we don't support calling with arguments yet
        };

        self.instrs.push(instr);

        Ok(callee)
    }

    fn comp_local(&mut self, local: &Ident) -> u8 {
        *self.locals.get(local).unwrap()
    }

    fn comp_bin<'hir>(&mut self, bin: &'hir Binary) -> Result<u8> {
        let lhs = self.comp_expr(bin.lhs)?;
        let rhs = self.comp_expr(bin.rhs)?;

        let a = self.next_stack();

        let instr = Instruction::Abc {
            operation: match bin.op {
                utils::BinaryOp::Add => OpAbc::Add,
                utils::BinaryOp::Sub => OpAbc::Sub,
                utils::BinaryOp::Mul => OpAbc::Mul,
                utils::BinaryOp::Div => OpAbc::Div,
                utils::BinaryOp::Equal => OpAbc::Eq,
                utils::BinaryOp::NotEqual => OpAbc::NEq,
                utils::BinaryOp::Greater => OpAbc::Greater,
                utils::BinaryOp::GreaterEqual => OpAbc::GreaterEqual,
                utils::BinaryOp::Lesser => OpAbc::Lesser,
                utils::BinaryOp::LesserEqual => OpAbc::LesserEqual,
                utils::BinaryOp::And => OpAbc::And,
                utils::BinaryOp::Or => OpAbc::Or,
            },
            a,
            b: lhs as u16,
            c: rhs as u16,
        };

        self.instrs.push(instr);

        Ok(a)
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

impl From<&CompileFunc> for dust_byt::Func {
    fn from(comp: &CompileFunc) -> Self {
        dust_byt::Func {
            instrs: comp.instrs.iter().copied().map(Instr::from).collect(),
            consts: comp.consts.iter().copied().collect(),
        }
    }
}
