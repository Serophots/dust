use bumpalo::Bump;

use crate::GblCtx;

#[derive(Copy, Clone)]
pub struct AstCtx<'ast, 'gcx> {
    pub gcx: GblCtx<'gcx>,
    pub arena: &'ast Bump,
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
    };

    f(ctx)
}

pub struct AstLoweringCtx<'ast, 'hir, 'gcx> {
    pub gcx: GblCtx<'gcx>,
    pub ast_arena: &'ast Bump,
    pub hir_arena: &'hir Bump,
}
