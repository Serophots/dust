use bumpalo::Bump;

use crate::GblCtxt;

#[derive(Copy, Clone)]
pub struct HirCtx<'hir, 'gcx>
where
    'gcx: 'hir,
{
    pub gcx: GblCtxt<'gcx>,
    pub arena: &'hir Bump,
}

#[derive(Copy, Clone)]
pub struct HirLowCtx<'hir, 'byt, 'gcx> {
    pub gcx: GblCtxt<'gcx>,
    pub hir_arena: &'hir Bump,
    pub byt_arena: &'byt Bump,
}
