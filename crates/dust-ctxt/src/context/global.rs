use std::{ops::ControlFlow, sync::OnceLock};

use bumpalo::Bump;
use camino::Utf8Path;
use miette::Result;

use crate::{AstCtx, AstLowCtx, BytCtx, HirCtx, HirLowCtx, NodeIdAllocator, SymbolInterner};

#[derive(Default)]
pub struct GblCtxtInner {
    pub arena: Bump,
    pub symbols: SymbolInterner,
    pub node_id: NodeIdAllocator,
}

#[derive(Copy, Clone)]
pub struct GblCtxt<'gcx> {
    gcx: &'gcx GblCtxtInner,
}

impl<'gcx> core::ops::Deref for GblCtxt<'gcx> {
    type Target = &'gcx GblCtxtInner;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.gcx
    }
}

#[must_use]
pub fn create_and_enter_global_ctxt<T, F>(f: F) -> T
where
    F: for<'gcx> FnOnce(GblCtxt<'gcx>) -> T,
{
    let gcx_cell = OnceLock::new();
    let gcx = gcx_cell.get_or_init(|| GblCtxtInner::default());

    f(GblCtxt { gcx })
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

    type RetHir<'hir>;

    type RetHirLw<'byt>;

    fn run<'hir, 'byt>(&self, root: &Utf8Path, gcx: GblCtxt<'gcx>) -> Result<Option<()>> {
        let ast_arena = Bump::new();
        let root = ast_arena.alloc(root.canonicalize_utf8().unwrap());
        let root_ident = gcx.symbols.get_or_intern(root.file_stem().unwrap());

        // Run ast
        let ast_ctx = AstCtx::<'_, 'gcx> {
            gcx: gcx,
            arena: &ast_arena,
            root: Some((root_ident, root)),
        };

        let ast = self.run_ast(ast_ctx)?;
        if self.hook_ast(&ast, ast_ctx).is_break() {
            return Ok(None);
        };

        // Run ast lowering
        let hir_arena = Bump::new();
        let ast_lw_ctx = AstLowCtx::<'_, '_, 'gcx> {
            gcx: gcx,
            ast_arena: &ast_arena,
            hir_arena: &hir_arena,
        };

        let ast_lw = self.run_ast_lw(ast, ast_lw_ctx)?;
        if self.hook_ast_lw(&ast_lw, ast_lw_ctx).is_break() {
            return Ok(None);
        }
        drop(ast_arena);

        // Run hir
        let hir_ctx = HirCtx::<'_, 'gcx> {
            gcx,
            arena: &hir_arena,
        };

        let hir = self.run_hir(ast_lw, hir_ctx)?;

        // Run hir lowering
        let byt_arena = Bump::new();
        let hir_lw_ctx = HirLowCtx::<'_, '_, 'gcx> {
            gcx,
            hir_arena: &hir_arena,
            byt_arena: &byt_arena,
        };

        let hir_lw = self.run_hir_lw(hir, hir_lw_ctx)?;
        drop(hir_arena);

        // Run byt
        let byt_ctx = BytCtx::<'_, 'gcx> {
            gcx,
            arena: &byt_arena,
        };

        self.run_byt(hir_lw, byt_ctx);

        Ok(Some(()))
    }

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<Self::RetAst<'ast>>;

    fn hook_ast<'ast, 'a>(
        &'a self,
        _ast: &'a Self::RetAst<'ast>,
        ctx: AstCtx<'ast, 'gcx>,
    ) -> ControlFlow<()>
    where
        'gcx: 'ast;

    fn run_ast_lw<'ast, 'hir>(
        &self,
        ast: Self::RetAst<'ast>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> Result<Self::RetAstLw<'hir>>;

    fn hook_ast_lw<'ast, 'hir, 'a>(
        &'a self,
        _ast: &'a Self::RetAstLw<'hir>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> ControlFlow<()>
    where
        'gcx: 'hir;

    fn run_hir<'hir>(
        &self,
        hir: Self::RetAstLw<'hir>,
        ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<Self::RetHir<'hir>>;

    fn run_hir_lw<'hir, 'byt>(
        &self,
        hir: Self::RetHir<'hir>,
        ctx: HirLowCtx<'hir, 'byt, 'gcx>,
    ) -> Result<Self::RetHirLw<'byt>>;

    fn run_byt<'byt>(&self, byt: Self::RetHirLw<'byt>, ctx: BytCtx<'byt, 'gcx>);
}
