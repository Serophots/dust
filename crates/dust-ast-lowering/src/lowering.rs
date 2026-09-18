#![feature(allocator_api)]

use dust_ctxt::{AstLowCtx, SymbolDebug};
use dust_hir::{Binary, Block, Expr, Func, Krate, Let, Literal, Stmt, Unary};
use dust_resolve::ResolverCtx;
use miette::Result;

pub struct Lowering<'ast, 'hir, 'gcx> {
    resolver: ResolverCtx,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
}

impl<'ast, 'hir, 'gcx> Lowering<'ast, 'hir, 'gcx> {}

pub fn lower_krate<'ast, 'hir, 'gcx>(
    krate: &'ast dust_ast::Krate<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Krate<'hir>> {
    let mut low = Lowering {
        resolver: ResolverCtx::default(),
        ctx,
    };

    let main = krate
        .root
        .func_by_name("main", low.ctx.gcx)
        .ok_or_else(|| miette::miette!("Root module did not have a main function"))?;

    Ok(low.ctx.hir_arena.alloc(Krate {
        main: lower_func(main, &mut low)?,
    }))
}

fn lower_func<'ast, 'hir, 'gcx>(
    func: &'ast dust_ast::Func<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Func<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Func {
        ident: func.ident,
        block: lower_block(func.block, low)?,
        span: func.span,
    }))
}

fn lower_block<'ast, 'hir, 'gcx>(
    block: &'ast dust_ast::Block<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Block<'hir>> {
    let stmts = {
        let mut vec = Vec::new_in(low.ctx.hir_arena);
        vec.reserve_exact(block.stmts.len());

        for &stmt in block.stmts.iter() {
            vec.push(lower_stmt(stmt, low)?);
        }

        vec.into_boxed_slice()
    };

    Ok(low.ctx.hir_arena.alloc(Block {
        stmts,
        expr: block
            .expr
            .map(|block_expr| lower_expr(block_expr, low))
            .transpose()?,
        span: block.span,
    }))
}

fn lower_stmt<'ast, 'hir, 'gcx>(
    stmt: &'ast dust_ast::Stmt<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Stmt<'hir>> {
    Ok(low.ctx.hir_arena.alloc(match *stmt {
        dust_ast::Stmt::Item(item) => {
            // Resolve references to these?
            todo!()
        }
        dust_ast::Stmt::Let(r#let) => Stmt::Let(lower_let(r#let, low)?),
        dust_ast::Stmt::Expr(expr) => Stmt::Expr(lower_expr(expr, low)?),
    }))
}

fn lower_let<'ast, 'hir, 'gcx>(
    r#let: &'ast dust_ast::Let<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Let<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Let {
        // ident: todo!(),
        expr: r#let
            .expr
            .map(|let_expr| lower_expr(let_expr, low))
            .transpose()?,
        span: r#let.span,
    }))
}

fn lower_expr<'ast, 'hir, 'gcx>(
    expr: &'ast dust_ast::Expr<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Expr<'hir>> {
    Ok(low.ctx.hir_arena.alloc(match *expr {
        dust_ast::Expr::Path(path) => {
            dbg!(path.dbg(low.ctx.gcx));
            todo!()
        }
        dust_ast::Expr::Binary(binary) => Expr::Binary(lower_binary(binary, low)?),
        dust_ast::Expr::Unary(unary) => Expr::Unary(lower_unary(unary, low)?),
        dust_ast::Expr::Literal(literal) => Expr::Literal(lower_literal(literal, low)?),
        dust_ast::Expr::Block(block) => Expr::Block(lower_block(block, low)?),
        dust_ast::Expr::Call(call) => todo!(),
        dust_ast::Expr::Assign => todo!(),
        dust_ast::Expr::If => todo!(),
        dust_ast::Expr::Loop => todo!(),
    }))
}

fn lower_literal<'ast, 'hir, 'gcx>(
    literal: &'ast dust_ast::Literal<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Literal<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Literal {
        lit: low.ctx.hir_arena.alloc(*literal.lit),
        span: literal.span,
    }))
}

fn lower_binary<'ast, 'hir, 'gcx>(
    binary: &'ast dust_ast::Binary<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Binary<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Binary {
        lhs: lower_expr(binary.lhs, low)?,
        rhs: lower_expr(binary.rhs, low)?,
        op: binary.op,
        span: binary.span,
    }))
}

fn lower_unary<'ast, 'hir, 'gcx>(
    unary: &'ast dust_ast::Unary<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Unary<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Unary {
        expr: lower_expr(unary.expr, low)?,
        op: unary.op,
        span: unary.span,
    }))
}
