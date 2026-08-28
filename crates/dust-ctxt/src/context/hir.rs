use bumpalo::Bump;

use crate::GblCtx;

pub struct HirCtx<'hir, 'gcx> {
    pub gcx: GblCtx<'gcx>,
    pub arena: &'hir Bump,
}
