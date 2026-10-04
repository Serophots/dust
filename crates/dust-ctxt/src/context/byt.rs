use bumpalo::Bump;

use crate::GblCtxt;

#[derive(Copy, Clone)]
pub struct BytCtx<'byt, 'gcx>
where
    'gcx: 'byt,
{
    pub gcx: GblCtxt<'gcx>,
    pub arena: &'byt Bump,
}
