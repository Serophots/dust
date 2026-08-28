use bumpalo::Bump;

use crate::GblCtx;

pub struct AstCtx<'ast, 'gcx> {
    pub gcx: GblCtx<'gcx>,
    pub arena: &'ast Bump,
}

pub struct AstLoweringCtx<'ast, 'hir, 'gcx> {
    pub gcx: GblCtx<'gcx>,
    pub ast_arena: &'ast Bump,
    pub hir_arena: &'hir Bump,
}
