use bumpalo::Bump;

use crate::GblCtx;

pub struct HirCtx<'hir, 'gcx>
where
    'gcx: 'hir,
{
    pub gcx: GblCtx<'gcx>,
    pub arena: &'hir Bump,
}
