use camino::Utf8PathBuf;
use miette::Result;
use utils::GblCx;

use crate::Module;

pub struct AstCx<'gcx> {
    gcx: GblCx<'gcx>,
}

pub fn create_and_enter_ast_ctxt<T, F>(gcx: GblCx, f: F) -> T
where
    F: for<'gcx> FnOnce(AstCx<'gcx>) -> T,
{
    let ast_ctx = AstCx { gcx };
    f(ast_ctx)
}

impl<'gcx> AstCx<'gcx> {
    pub fn parse_root_module(&self, source: Utf8PathBuf) -> Result<Module> {
        todo!()
    }
}
