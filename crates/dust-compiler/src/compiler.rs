use camino::Utf8PathBuf;
use dust_ctxt::{AstCtx, AstLowCtx, CtxtRunner, HirCtx};
use miette::Result;

pub struct Compiler {
    pub root_module: Utf8PathBuf,
}

impl<'gcx> CtxtRunner<'gcx> for Compiler {
    type RetAst<'ast>
        = &'ast dust_ast::Krate<'ast>
    where
        'gcx: 'ast;
    type RetAstLw<'hir>
        = &'hir dust_hir::Main<'hir>
    where
        'gcx: 'hir;
    type RetHir = ();

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast dust_ast::Krate<'ast>> {
        Ok(dust_ast::parse_module(&self.root_module, ctx)?)
    }

    fn run_ast_lowering<'ast, 'hir>(
        &self,
        krate: &'ast dust_ast::Krate<'ast>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> Result<&'hir dust_hir::Main<'hir>> {
        Ok(dust_ast_lowering::lower_krate(krate, ctx)?)
    }

    fn run_hir<'hir>(
        &self,
        hir: &'hir dust_hir::Main<'hir>,
        ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<Self::RetHir> {
        todo!()
    }
}
