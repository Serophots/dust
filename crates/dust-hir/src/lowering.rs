use dust_ast::Module;

use crate::HirCx;

mod item;

pub struct LoweringCtx<'hir> {
    hir_ctx: HirCx<'hir>,

    sources: Vec<String>,
}

impl<'hir> LoweringCtx<'hir> {
    pub(crate) fn new(hir_ctx: &mut ()) {}

    fn lower_module(&mut self, module: &Module) {}

    pub fn alloc_source(&'hir mut self, src: String) -> &'hir str {
        self.sources.push(src);
        self.sources.last().unwrap()
    }
}

// fn parse_source(source: TextSource, cx: &mut HirCx) -> Result<dust_ast::Module> {
//     todo!()
// }

// impl HirCx {}
