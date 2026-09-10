use std::{ops::ControlFlow, sync::OnceLock};

use bumpalo::Bump;
use camino::Utf8Path;
use dust_resolve::ResolverCtx;
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

/// Instantiates the various contexts, calling into
/// methods for each of the stages of compilation,
/// with the correct instantiated contexts.
pub trait WithContexts<'gcx> {
    type RetAst<'ast>
    where
        'gcx: 'ast;
    type RetAstLw<'hir>
    where
        'gcx: 'hir;
    type RetHir;

    fn run(&self, root: &Utf8Path, gcx: GblCtx<'gcx>) -> Result<()> {
        let ast_arena = Bump::new();
        let root = ast_arena.alloc(root.canonicalize_utf8().unwrap());
        let root_ident = gcx.symbols.get_or_intern(root.file_stem().unwrap());
        let ast_ctx = AstCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &ast_arena,
            root: Some((root_ident, root)),
        };

        // Run ast
        let ast = self.run_ast(ast_ctx)?;
        if self.hook_ast(&ast).is_break() {
            return Ok(());
        };

        let hir_arena = Bump::new();
        let ast_lw_ctx = AstLowCtx::<'_, '_, 'gcx> {
            gcx: gcx,
            ast_arena: &ast_arena,
            hir_arena: &hir_arena,
            resolver: ast_arena.alloc(ResolverCtx::default()),
        };

        // Run ast lowering
        let ast_lw = self.run_ast_lowering(ast, ast_lw_ctx)?;
        drop(ast_arena);
        if self.hook_ast_lw(&ast_lw).is_break() {
            return Ok(());
        }

        let hir_ctx = HirCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &hir_arena,
        };

        // Run hir
        let hir = self.run_hir(ast_lw, hir_ctx)?;

        Ok(())
    }

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<Self::RetAst<'ast>>;

    fn hook_ast<'ast, 'a>(&'a self, _ast: &'a Self::RetAst<'ast>) -> ControlFlow<()>
    where
        'gcx: 'ast;

    fn run_ast_lowering<'ast, 'hir>(
        &self,
        ref_ast: Self::RetAst<'ast>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> Result<Self::RetAstLw<'hir>>;

    fn hook_ast_lw<'hir, 'a>(&'a self, _ast: &'a Self::RetAstLw<'hir>) -> ControlFlow<()>
    where
        'gcx: 'hir;

    fn run_hir<'hir>(
        &self,
        ref_hir: Self::RetAstLw<'hir>,
        ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<Self::RetHir>;
}
