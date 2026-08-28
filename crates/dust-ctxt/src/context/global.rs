use std::sync::OnceLock;

use bumpalo::Bump;

use crate::{AstCtx, AstLoweringCtx, HirCtx, SymbolInterner};

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

pub fn create_and_enter_global_ctxt<T, F>(f: F) -> T
where
    F: for<'gcx> FnOnce(GblCtx<'gcx>) -> T,
{
    let gcx_cell = OnceLock::new();
    let gcx = gcx_cell.get_or_init(|| GblCtxtInner::default());

    f(GblCtx { gcx })
}

pub trait CtxtRunner<'gcx> {
    type RetAst;
    type RetAstLw;
    type RetHir;

    fn run(&self, gcx: GblCtx<'gcx>) {
        let ast_arena = Bump::new();
        let ast_ctx = AstCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &ast_arena,
        };

        // Run ast
        let ref_ast = self.run_ast(ast_ctx);

        let hir_arena = Bump::new();
        let ast_lw_ctx = AstLoweringCtx::<'_, '_, 'gcx> {
            gcx: gcx,
            ast_arena: &ast_arena,
            hir_arena: &hir_arena,
        };

        // Run ast lowering
        let ref_hir = self.run_ast_lowering(ref_ast, ast_lw_ctx);
        drop(ast_arena);

        let hir_ctx = HirCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &hir_arena,
        };

        // Run hir
        let ref_hir = self.run_hir(ref_hir, hir_ctx);
    }

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> &'ast Self::RetAst;

    fn run_ast_lowering<'ast, 'hir>(
        &self,
        ref_ast: &'ast Self::RetAst,
        ctx: AstLoweringCtx<'ast, 'hir, 'gcx>,
    ) -> &'hir Self::RetAstLw;

    fn run_hir<'hir>(
        &self,
        ref_hir: &'hir Self::RetAstLw,
        ctx: HirCtx<'hir, 'gcx>,
    ) -> &'hir Self::RetHir;
}
