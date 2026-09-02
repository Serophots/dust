#![feature(allocator_api)]

use bumpalo::collections::CollectIn;
use dust_ctxt::AstLowCtx;
use dust_hir::{Block, Expr, Func, Item, ItemType, Let, Main, Module, Stmt};
use miette::Result;

pub fn lower_krate<'ast, 'hir, 'gcx>(
    krate: &'ast dust_ast::Krate<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Main<'hir>> {
    let main = krate
        .root
        .func_by_name("main", ctx.gcx)
        .ok_or_else(|| miette::miette!("Root module did not have a main function"))?;

    Ok(ctx.hir_arena.alloc(Main(lower_func(main, ctx)?)))
}

fn lower_module<'ast, 'hir, 'gcx>(
    module: &'ast dust_ast::Module<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Module<'hir>> {
    let items = {
        let mut vec = Vec::new_in(ctx.hir_arena);
        vec.reserve_exact(module.items.len());

        for &item in module.items.iter() {
            vec.push(lower_item(item, ctx)?);
        }

        vec.into_boxed_slice()
    };

    Ok(ctx.hir_arena.alloc(Module {
        ident: module.ident,
        items,
        span: module.span,
    }))
}

fn lower_item<'ast, 'hir, 'gcx>(
    item: &'ast dust_ast::Item<'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Item<'hir>> {
    Ok(ctx.hir_arena.alloc(Item {
        r#type: match item.r#type {
            dust_ast::ItemType::Module(module) => ItemType::Module(lower_module(module, ctx)?),
            dust_ast::ItemType::Func(func) => ItemType::Func(lower_func(func, ctx)?),
            dust_ast::ItemType::Use(_) => todo!(),
        },
        span: item.span,
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
        dust_ast::Stmt::Semicolon => todo!(),
        dust_ast::Stmt::Item(item) => todo!(),
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
        dust_ast::Expr::Arith(arith) => todo!(),
        dust_ast::Expr::Assign => todo!(),
        dust_ast::Expr::Call(call) => todo!(),
        dust_ast::Expr::Path(path) => todo!(),
        dust_ast::Expr::Block(block) => todo!(),
        dust_ast::Expr::IfExpr => todo!(),
        dust_ast::Expr::LoopExpr => todo!(),
    }))
}
