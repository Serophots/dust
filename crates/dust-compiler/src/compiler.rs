use camino::Utf8PathBuf;
use dust_ctxt::{AstCtx, AstLoweringCtx, CtxtRunner, HirCtx};
use miette::Result;

pub struct Compiler {
    pub root_module: Utf8PathBuf,
}

impl<'gcx> CtxtRunner<'gcx> for Compiler {
    type RetAst = Result<(), miette::Report>;
    type RetAstLw = ();
    type RetHir = ();

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast Self::RetAst> {
        // Parse the root module into AST
        let piss = dust_ast::parse_module(&self.root_module, ctx)?;

        // Parse referenced modules into AST
        //TODO

        todo!()
    }

    fn run_ast_lowering<'ast, 'hir>(
        &self,
        ref_ast: &'ast Self::RetAst,
        ctx: AstLoweringCtx<'ast, 'hir, 'gcx>,
    ) -> Result<&'hir Self::RetAstLw> {
        todo!()
    }

    fn run_hir<'hir>(
        &self,
        ref_hir: &'hir Self::RetAstLw,
        ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<&'hir Self::RetHir> {
        todo!()
    }
}
