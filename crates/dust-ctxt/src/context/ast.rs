use bumpalo::Bump;
use camino::Utf8Path;
use dust_resolve::ResolverCtx;
use utils::Symbol;

use crate::GblCtx;

#[derive(Copy, Clone)]
pub struct AstCtx<'ast, 'gcx>
where
    'gcx: 'ast,
{
    pub gcx: GblCtx<'gcx>,
    pub arena: &'ast Bump,
    /// None opts out of module resolution on the file system
    pub root: Option<(Symbol, &'ast Utf8Path)>,
}

impl<'ast, 'gcx> AstCtx<'ast, 'gcx>
where
    'gcx: 'ast,
{
    pub fn expect_root(&self) -> &(Symbol, &'ast Utf8Path) {
        self.root
            .as_ref()
            .expect("expected AstCtx to be constructed with the filepath of the crate root.")
    }
}

/// Prefer the CtxtRunner trait
#[must_use]
pub fn create_and_enter_ast_ctxt<'gcx, T, F>(ctx: GblCtx<'gcx>, f: F) -> T
where
    F: for<'ast> FnOnce(AstCtx<'ast, 'gcx>) -> T,
{
    let arena = Bump::new();
    let ctx = AstCtx {
        gcx: ctx,
        arena: &arena,
        root: None,
    };

    f(ctx)
}

#[derive(Copy, Clone)]
pub struct AstLowCtx<'ast, 'hir, 'gcx>
where
    'gcx: 'ast,
    'gcx: 'hir,
{
    pub gcx: GblCtx<'gcx>,
    pub ast_arena: &'ast Bump,
    pub hir_arena: &'hir Bump,
    pub resolver: &'ast ResolverCtx,
}
