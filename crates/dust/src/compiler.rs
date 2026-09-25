use std::{marker::PhantomData, ops::ControlFlow};

use camino::Utf8Path;
use dust_byt::Chunk;
use dust_ctxt::{AstCtx, AstLowCtx, GblCtxt, HirCtx, WithContexts};
use miette::Result;

/// Any trait which implements this `Compiler`
/// trait can drive the compilation process.
pub trait Compiler<'gcx>: Sized {
    fn run(self, root: &Utf8Path, gcx: GblCtxt<'gcx>) -> Result<Option<Chunk>> {
        CompilerWrapper(self, PhantomData).run(root, gcx)
    }

    fn hook_ast<'ast, 'a>(&'a self, _ast: &'ast dust_ast::Krate<'gcx, 'ast>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    fn hook_ast_lw<'hir, 'a>(&'a self, _ast: &'hir dust_hir::Krate<'hir>) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

/// Any implementor of
struct CompilerWrapper<'gcx, T>(T, PhantomData<&'gcx ()>)
where
    T: Compiler<'gcx>;

impl<'gcx, T> WithContexts<'gcx> for CompilerWrapper<'gcx, T>
where
    T: Compiler<'gcx>,
{
    type RetAst<'ast>
        = &'ast dust_ast::Krate<'gcx, 'ast>
    where
        'gcx: 'ast;

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast dust_ast::Krate<'gcx, 'ast>> {
        Ok(dust_ast::parse_root(ctx)?)
    }

    fn hook_ast<'ast, 'a>(&'a self, ast: &'a Self::RetAst<'ast>) -> ControlFlow<()>
    where
        'gcx: 'ast,
    {
        self.0.hook_ast(ast)
    }

    type RetAstLw<'hir>
        = &'hir dust_hir::Krate<'hir>
    where
        'gcx: 'hir;

    fn run_ast_lowering<'ast, 'hir>(
        &self,
        krate: &'ast dust_ast::Krate<'gcx, 'ast>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> Result<&'hir dust_hir::Krate<'hir>> {
        Ok(dust_ast_lowering::lower_krate(krate, ctx)?)
    }

    fn hook_ast_lw<'hir, 'a>(&'a self, ast: &'a Self::RetAstLw<'hir>) -> ControlFlow<()>
    where
        'gcx: 'hir,
    {
        self.0.hook_ast_lw(ast)
    }

    type RetHir = dust_byt::Chunk;

    fn run_hir<'hir>(
        &self,
        main: &'hir dust_hir::Krate<'hir>,
        _ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<dust_byt::Chunk> {
        let chunk = dust_byt_comp::comp_main(main)?;

        Ok(chunk)
    }
}
