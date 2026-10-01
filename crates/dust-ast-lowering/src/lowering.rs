use dust_ctxt::AstLowCtx;
use dust_hir::{Binary, Block, Expr, Func, Krate, Let, Literal, Stmt, Unary};
use dust_resolve::{
    Namespace::{self, ValueNS},
    Res, ResolverCtx, Rib, RibKind,
};
use miette::{LabeledSpan, Result};

pub struct Lowering<'ast, 'hir, 'gcx> {
    pub resolver: ResolverCtx<'hir>,
    pub module: &'ast dust_ast::Module<'gcx, 'ast>,
    pub ctx: AstLowCtx<'ast, 'hir, 'gcx>,
}

impl<'ast, 'hir, 'gcx> Lowering<'ast, 'hir, 'gcx> {
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
}

pub fn lower_krate<'ast, 'hir, 'gcx>(
    krate: &'ast dust_ast::Krate<'gcx, 'ast>,
    ctx: AstLowCtx<'ast, 'hir, 'gcx>,
) -> Result<&'hir Krate<'hir>> {
    let mut low = Lowering {
        resolver: ResolverCtx::default(),
        module: krate.root,
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
    func: &'ast dust_ast::Func<'gcx, 'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Func<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Func {
        ident: func.ident,
        block: lower_block(func.block, low)?,
        span: func.span,
    }))
}

fn lower_block<'ast, 'hir, 'gcx>(
    block: &'ast dust_ast::Block<'gcx, 'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
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
    r#let: &'ast dust_ast::Let<'gcx, 'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Let<'hir>> {
    let ident = r#let.ident;

    let expr = r#let
        .expr
        .map(|let_expr| lower_expr(let_expr, low))
        .transpose()?;

    low.resolver.push_rib(ValueNS, RibKind::Normal, |rib| {
        rib.bindings
            .insert(ident.symbol, low.ctx.hir_arena.alloc(Res::Local(ident)));
    });

    Ok(low.ctx.hir_arena.alloc(Let {
        expr,
        ident,
        span: r#let.span,
    }))
}

fn lower_expr<'ast, 'hir, 'gcx>(
    expr: &'ast dust_ast::Expr<'gcx, 'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Expr<'hir>> {
    Ok(low.ctx.hir_arena.alloc(match *expr {
        dust_ast::Expr::Path(path) => Expr::Res(lower_path(path, low)?),
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
    binary: &'ast dust_ast::Binary<'gcx, 'ast>,
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
    unary: &'ast dust_ast::Unary<'gcx, 'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir Unary<'hir>> {
    Ok(low.ctx.hir_arena.alloc(Unary {
        expr: lower_expr(unary.expr, low)?,
        op: unary.op,
        span: unary.span,
    }))
}

fn lower_path<'ast, 'hir, 'gcx>(
    path: &'ast dust_ast::Path<'ast>,
    low: &mut Lowering<'ast, 'hir, 'gcx>,
) -> Result<&'hir dust_resolve::Res> {
    match path.try_into_ident() {
        // The path is length 1
        Some(ident) => low.resolver.resolve_ident(ident, ValueNS).ok_or_else(|| {
            miette::miette!(
                labels = vec![LabeledSpan::at(ident.span, "identifier")],
                "failed to resolve identifier in the value namespace"
            )
            .with_source_code(low.module.source.to_owned())
        }),
        None => {
            todo!()
        }
    }
}
