use bumpalo::Bump;

use crate::GblCtxt;

pub struct HirCtx<'hir, 'gcx>
where
    'gcx: 'hir,
{
    pub gcx: GblCtxt<'gcx>,
    pub arena: &'hir Bump,
}
