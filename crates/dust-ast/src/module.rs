use dust_ctxt::GblCtxt;

use crate::{Func, ItemType, Module};

impl<'gcx, 'ast> Module<'gcx, 'ast> {
    pub fn func_by_name(&self, name: &str, ctx: GblCtxt<'ast>) -> Option<&'ast Func<'gcx, 'ast>> {
        for &item in self.items.iter() {
            match item.r#type {
                ItemType::Func(function)
                    if function.ident.symbol == ctx.symbols.get_or_intern(name) =>
                {
                    return Some(function);
                }
                _ => {}
            }
        }

        None
    }
}
