use miette::Result;
use utils::{GblCx, TextSource};

pub struct HirCx<'cx> {
    cx: GblCx<'cx>,
}

pub fn create_and_enter_hir_ctxt<T, F>(glbl_ctx: GblCx, f: F) -> T
where
    F: for<'gcx> FnOnce(HirCx<'gcx>) -> T,
{
    let hir_ctx = HirCx { cx: glbl_ctx };

    f(hir_ctx)
}

impl<'hir> HirCx<'hir> {
    pub fn lower_root_module(&mut self, source: TextSource) -> Result<()> {
        todo!()
        // let src = self.alloc_source(
        //     source
        //         .content()
        //         .context(format!("opening root module {}", source))?,
        // );

        // let root = dust_ast::Parser::new(src)
        //     .mod_file()
        //     .context(format!("parsing AST for root module {}", source))?;

        // self.lower_module(root);

        // todo!()
    }
}
