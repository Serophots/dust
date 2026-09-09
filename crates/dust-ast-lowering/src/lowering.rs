#![feature(allocator_api)]

use bumpalo::Bump;
use dust_ctxt::AstLowCtx;
use dust_hir::{Block, Expr, Func, Let, Main, Stmt};
use miette::Result;
use utils::Ident;

mod namespace;

pub struct Lowering<'ast> {
    namespace: Vec<Ident, &'ast Bump>,
}

pub fn lower_krate<'ast, 'hir, 'gcx>(
    krate: &'ast dust_ast::Krate<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Main<'hir>> {
    // First, sweep the namespace:
    // Map out every

    let main = krate
        .root
        .func_by_name("main", ctx.gcx)
        .ok_or_else(|| miette::miette!("Root module did not have a main function"))?;

    Ok(ctx.hir_arena.alloc(Main {
        main: lower_func(main, ctx)?,
    }))
}

fn lower_func<'ast, 'hir, 'gcx>(
    func: &'ast dust_ast::Func<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Func<'hir>> {
    Ok(ctx.hir_arena.alloc(Func {
        ident: func.ident,
        block: lower_block(func.block, ctx)?,
        span: func.span,
    }))
}

fn lower_block<'ast, 'hir, 'gcx>(
    block: &'ast dust_ast::Block<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Block<'hir>> {
    let stmts = {
        let mut vec = Vec::new_in(ctx.hir_arena);
        vec.reserve_exact(block.stmts.len());

        for &stmt in block.stmts.iter() {
            vec.push(lower_stmt(stmt, ctx)?);
        }

        vec.into_boxed_slice()
    };

    Ok(ctx.hir_arena.alloc(Block {
        stmts,
        expr: block
            .expr
            .map(|block_expr| lower_expr(block_expr, ctx))
            .transpose()?,
        span: block.span,
    }))
}

fn lower_stmt<'ast, 'hir, 'gcx>(
    stmt: &'ast dust_ast::Stmt<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Stmt<'hir>> {
    Ok(ctx.hir_arena.alloc(match *stmt {
        dust_ast::Stmt::Item(item) => {
            // Resolve references to these?
            todo!()
        }
        dust_ast::Stmt::Let(r#let) => Stmt::Let(lower_let(r#let, ctx)?),
        dust_ast::Stmt::Expr(expr) => Stmt::Expr(lower_expr(expr, ctx)?),
    }))
}

fn lower_let<'ast, 'hir, 'gcx>(
    r#let: &'ast dust_ast::Let<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Let<'hir>> {
    Ok(ctx.hir_arena.alloc(Let {
        ident: r#let.ident,
        expr: r#let
            .expr
            .map(|let_expr| lower_expr(let_expr, ctx))
            .transpose()?,
        span: r#let.span,
    }))
}

fn lower_expr<'ast, 'hir, 'gcx>(
    expr: &'ast dust_ast::Expr<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Expr<'hir>> {
    Ok(ctx.hir_arena.alloc(match *expr {
        dust_ast::Expr::Call(call) => todo!(),
        dust_ast::Expr::Binary(binary) => todo!(),
        dust_ast::Expr::Unary(unary) => todo!(),
        dust_ast::Expr::Path(path) => todo!(),
        dust_ast::Expr::Literal(literal) => todo!(),
        dust_ast::Expr::Assign => todo!(),
        dust_ast::Expr::Block(block) => todo!(),
        dust_ast::Expr::If => todo!(),
        dust_ast::Expr::Loop => todo!(),
    }))
}

// fn lower_arith<'ast, 'hir, 'gcx>(
//     arith: &'ast dust_ast::Arith<'ast>,
//     ctx: AstLowCtx<'ast, 'hir, 'gcx>,
// ) -> Result<&'hir Arith<'hir>> {
//     Ok(ctx.hir_arena.alloc(match *arith {
//         dust_ast::Expr::Arith(arith) => todo!(),
//         dust_ast::Expr::Assign => todo!(),
//         dust_ast::Expr::Call(call) => todo!(),
//         dust_ast::Expr::Path(path) => todo!(),
//         dust_ast::Expr::Block(block) => todo!(),
//         dust_ast::Expr::If => todo!(),
//         dust_ast::Expr::Loop => todo!(),
//     }))
// }
