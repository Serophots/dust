use std::sync::OnceLock;

use bumpalo::Bump;
use miette::Result;

use crate::{AstCtx, AstLowCtx, HirCtx, SymbolInterner};

#[derive(Default)]
pub struct GblCtxtInner {
    pub symbols: SymbolInterner,
}

#[derive(Copy, Clone)]
pub struct GblCtx<'gcx> {
    gcx: &'gcx GblCtxtInner,
}

impl<'gcx> core::ops::Deref for GblCtx<'gcx> {
    type Target = &'gcx GblCtxtInner;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.gcx
    }
}

#[must_use]
pub fn create_and_enter_global_ctxt<T, F>(f: F) -> T
where
    F: for<'gcx> FnOnce(GblCtx<'gcx>) -> T,
{
    let gcx_cell = OnceLock::new();
    let gcx = gcx_cell.get_or_init(|| GblCtxtInner::default());

    f(GblCtx { gcx })
}

pub trait CtxtRunner<'gcx> {
    type RetAst<'ast>
    where
        'gcx: 'ast;
    type RetAstLw<'hir>
    where
        'gcx: 'hir;
    type RetHir;

    fn run(&self, gcx: GblCtx<'gcx>) -> Result<()> {
        let ast_arena = Bump::new();
        let ast_ctx = AstCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &ast_arena,
        };

        // Run ast
        let ref_ast = self.run_ast(ast_ctx)?;

        let hir_arena = Bump::new();
        let ast_lw_ctx = AstLowCtx::<'_, '_, 'gcx> {
            gcx: gcx,
            ast_arena: &ast_arena,
            hir_arena: &hir_arena,
        };

        // Run ast lowering
        let ref_hir = self.run_ast_lowering(ref_ast, ast_lw_ctx)?;
        drop(ast_arena);

        let hir_ctx = HirCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &hir_arena,
        };

        // Run hir
        let ref_hir = self.run_hir(ref_hir, hir_ctx);

        Ok(())
    }

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<Self::RetAst<'ast>>;

    fn run_ast_lowering<'ast, 'hir>(
        &self,
        ref_ast: Self::RetAst<'ast>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> Result<Self::RetAstLw<'hir>>;

    fn run_hir<'hir>(
        &self,
        ref_hir: Self::RetAstLw<'hir>,
        ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<Self::RetHir>;
}
