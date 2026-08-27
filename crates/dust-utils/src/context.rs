use std::sync::OnceLock;

use crate::SymbolInterner;

#[derive(Default)]
pub struct GlobalCtxt {
    pub symbols: SymbolInterner,
}

#[derive(Copy, Clone)]
pub struct GblCx<'gcx> {
    gcx: &'gcx GlobalCtxt,
}

impl<'gcx> core::ops::Deref for GblCx<'gcx> {
    type Target = &'gcx GlobalCtxt;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.gcx
    }
}

pub fn create_and_enter_global_ctxt<T, F>(f: F) -> T
where
    F: for<'gcx> FnOnce(GblCx<'gcx>) -> T,
{
    let gcx_cell = OnceLock::new();
    let gcx = gcx_cell.get_or_init(|| GlobalCtxt::default());

    f(GblCx { gcx })
}
