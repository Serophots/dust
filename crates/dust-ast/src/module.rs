use dust_ctxt::{AstCtx, GblCtx};

use crate::{Func, ItemType, Module};

impl<'ast> Module<'ast> {
    pub fn func_by_name(&self, name: &str, ctx: GblCtx<'ast>) -> Option<&'ast Func<'ast>> {
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
