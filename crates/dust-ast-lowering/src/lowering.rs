use ahash::HashMap;
use dust_ctxt::AstLowCtx;
use dust_hir::{Binary, Block, Call, Expr, Func, FuncExpr, Krate, Let, Literal, Stmt, Unary};
use miette::{LabeledSpan, Result};

mod resolve;

pub use resolve::*;
use utils::{NodeId, Vec};

use crate::resolve::Namespace::ValueNS;

pub struct LowerKrate<'ast, 'hir, 'gcx> {
    pub resolver: ResolverCtx,
    pub module: &'ast dust_ast::Module<'gcx, 'ast>,
    pub ctx: AstLowCtx<'ast, 'hir, 'gcx>,

    /// Maps the node of the function in the AST tree
    /// to the lowered function in the HIR.
    pub funcs: HashMap<NodeId, &'hir Func<'hir>>,
}

impl<'ast, 'hir, 'gcx> LowerKrate<'ast, 'hir, 'gcx> {
    pub fn with_rib<F, T>(&mut self, namespace: Namespace, kind: RibKind, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let len = self.resolver.ribs[namespace].len();
        self.resolver.ribs[namespace].push(Rib {
            bindings: Default::default(),
            kind,
        });

        let ret = f(self);

        self.resolver.ribs[namespace].truncate(len);
        ret
    }

    pub fn last_rib_mut<'a>(&'a mut self, namespace: Namespace) -> Option<&'a mut Rib> {
        self.resolver.ribs[namespace].last_mut()
    }
}

pub fn lower_krate<'ast, 'hir, 'gcx>(
    krate: &'ast dust_ast::Krate<'gcx, 'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Krate<'hir>> {
    let main_symbol = ctx.gcx.symbols.get_or_intern("main");

    let mut low = LowerKrate {
        resolver: ResolverCtx::default(),
        module: krate.root,
        funcs: std::collections::HashMap::default(),
        ctx,
    };

    lower_module(krate.root, &mut low)?;

    let main = *low
        .funcs
        .values()
        .find(|f| f.ident.symbol == main_symbol)
        .ok_or_else(|| miette::miette!("Root module did not have a main function"))?;

    let LowerKrate { funcs, ctx, .. } = low;

    // TODO: Construct this in one allocation?
    let mut funcs_vec = Vec::new_in(ctx.hir_arena);
    funcs_vec.extend(funcs.values().copied());

    Ok(ctx.hir_arena.alloc(Krate {
        main,
        funcs: funcs_vec.into(), // TODO: This re-alloc into Box<[]> from Vec sucks
    }))
}

/// Add a rib to namespace resolution which
/// identifies all of the items in this module
fn in_module_namespace<'ast, 'hir, 'gcx, F, T>(
    module: &'ast dust_ast::Module<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
    f: F,
) -> Result<T>
where
    F: FnOnce(&mut LowerKrate<'ast, 'hir, 'gcx>) -> Result<T>,
{
    low.with_rib(ValueNS, RibKind::Module, |low| {
        for &item in module.items.iter() {
            let binding = match item.r#type {
                dust_ast::ItemType::Module(_) => None,
                dust_ast::ItemType::Use(_) => todo!(),
                dust_ast::ItemType::Func(func) => Some((func.ident.symbol, Res::Func(func.id))),
            };

            match binding {
                Some((symbol, res)) => {
                    low.last_rib_mut(ValueNS)
                        .unwrap()
                        .bindings
                        .insert(symbol, res);
                }
                None => {}
            }
        }

        f(low)
    })
}

fn lower_module<'ast, 'hir, 'gcx>(
    module: &'ast dust_ast::Module<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<()> {
    in_module_namespace(module, low, |low| {
        for &item in module.items.iter() {
            match item.r#type {
                dust_ast::ItemType::Module(module) => {
                    lower_module(module, low)?;
                }
                dust_ast::ItemType::Func(ast_func) => {
                    let hir_func = lower_func(ast_func, low)?;
                    low.funcs.insert(ast_func.id, hir_func);
                }
                dust_ast::ItemType::Use(_) => todo!(),
            }
        }

        Ok(())
    })
}

fn lower_func<'ast, 'hir, 'gcx>(
    func: &'ast dust_ast::Func<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Func<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Func {
        ident: func.ident,
        block: lower_block(func.block, low)?,
        span: func.span,
        id: func.id,
    }))
}

fn lower_block<'ast, 'hir, 'gcx>(
    block: &'ast dust_ast::Block<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Block<'hir>> {
    let block = low.with_rib(ValueNS, RibKind::Block, |low| {
        let stmts = {
            let mut vec = Vec::new_in(low.ctx.hir_arena);
            vec.reserve_exact(block.stmts.len());

            for &stmt in block.stmts.iter() {
                vec.push(lower_stmt(stmt, low)?);
            }

            vec.into_boxed_slice()
        };

        let expr = block
            .expr
            .map(|block_expr| lower_expr(block_expr, low))
            .transpose()?;

        Result::<_>::Ok(low.ctx.hir_arena.alloc(Block {
            stmts,
            expr,
            span: block.span,
        }))
    })?;

    Ok(block)
}

fn lower_stmt<'ast, 'hir, 'gcx>(
    stmt: &'ast dust_ast::Stmt<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
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
    r#let: &'ast dust_ast::Let<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Let<'hir>> {
    let ident = r#let.ident;

    let expr = r#let
        .expr
        .map(|let_expr| lower_expr(let_expr, low))
        .transpose()?;

    low.resolver.push_rib(ValueNS, RibKind::Normal, |rib| {
        rib.bindings.insert(ident.symbol, Res::Local(ident));
    });

    Ok(low.ctx.hir_arena.alloc(Let {
        expr,
        ident,
        span: r#let.span,
    }))
}

fn lower_call<'ast, 'hir, 'gcx>(
    call: &'ast dust_ast::Call<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Call<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Call {
        expr: lower_expr(call.expr, low)?,
        span: call.span,
    }))
}

fn lower_expr<'ast, 'hir, 'gcx>(
    expr: &'ast dust_ast::Expr<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Expr<'hir>> {
    Ok(low.ctx.hir_arena.alloc(match *expr {
        dust_ast::Expr::Binary(binary) => Expr::Binary(lower_binary(binary, low)?),
        dust_ast::Expr::Path(path) => lower_path(path, low)?,
        dust_ast::Expr::Unary(unary) => Expr::Unary(lower_unary(unary, low)?),
        dust_ast::Expr::Literal(literal) => Expr::Literal(lower_literal(literal, low)?),
        dust_ast::Expr::Block(block) => Expr::Block(lower_block(block, low)?),
        dust_ast::Expr::Call(call) => Expr::Call(lower_call(call, low)?),
        dust_ast::Expr::Assign => todo!(),
        dust_ast::Expr::If => todo!(),
        dust_ast::Expr::Loop => todo!(),
    }))
}

fn lower_literal<'ast, 'hir, 'gcx>(
    literal: &'ast dust_ast::Literal<'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Literal<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Literal {
        lit: low.ctx.hir_arena.alloc(*literal.lit),
        span: literal.span,
    }))
}

fn lower_binary<'ast, 'hir, 'gcx>(
    binary: &'ast dust_ast::Binary<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Binary<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Binary {
        lhs: lower_expr(binary.lhs, low)?,
        rhs: lower_expr(binary.rhs, low)?,
        op: binary.op,
        span: binary.span,
    }))
}

fn lower_unary<'ast, 'hir, 'gcx>(
    unary: &'ast dust_ast::Unary<'gcx, 'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<&'hir Unary<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Unary {
        expr: lower_expr(unary.expr, low)?,
        op: unary.op,
        span: unary.span,
    }))
}

fn lower_path<'ast, 'hir, 'gcx>(
    path: &'ast dust_ast::Path<'ast>,
    low: &mut LowerKrate<'ast, 'hir, 'gcx>,
) -> Result<Expr<'hir>> {
    Ok(match path.try_into_ident() {
        // The path is length 1
        Some(ident) => {
            let res = low.resolver.resolve_ident(ident, ValueNS).ok_or_else(|| {
                miette::miette!(
                    labels = vec![LabeledSpan::at(ident.span, "identifier")],
                    "{}\n\n{}",
                    "failed to resolve identifier in the value namespace",
                    format!(
                        "value namespace: {:?}",
                        low.resolver.inspect_namespace(ValueNS, low.ctx.gcx)
                    )
                )
                .with_source_code(low.module.source.to_owned())
            })?;

            match res {
                Res::Local(ident) => Expr::Local(ident),
                Res::Func(node_id) => Expr::Func(low.ctx.hir_arena.alloc(FuncExpr {
                    span: path.span,
                    node_id,
                })),
            }
        }
        None => {
            todo!()
        }
    })
}
