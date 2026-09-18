use std::cell::RefCell;

use string_interner::{StringInterner, backend::StringBackend};
use utils::{Ident, Symbol};

use crate::GblCtxt;

pub struct SymbolInterner {
    interner: RefCell<StringInterner<StringBackend<Symbol>>>,
}

impl Default for SymbolInterner {
    fn default() -> Self {
        Self {
            interner: RefCell::new(StringInterner::new()),
        }
    }
}

impl SymbolInterner {
    pub fn get_or_intern<T: AsRef<str>>(&self, string: T) -> Symbol {
        self.interner.borrow_mut().get_or_intern(string)
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<String> {
        self.interner
            .borrow()
            .resolve(symbol)
            .map(ToOwned::to_owned) // TODO: This cloning sucks a lot
    }
}

pub trait SymbolDebug {
    fn dbg<'gcx>(&self, ctx: GblCtxt<'gcx>) -> String;
}

impl SymbolDebug for Symbol {
    fn dbg<'gcx>(&self, ctx: GblCtxt<'gcx>) -> String {
        let Some(resolved) = ctx.symbols.resolve(self.clone()) else {
            return format!("{:?}", self);
        };

        resolved
    }
}

impl SymbolDebug for Ident {
    fn dbg<'gcx>(&self, ctx: GblCtxt<'gcx>) -> String {
        let Some(resolved) = ctx.symbols.resolve(self.symbol) else {
            return format!("{:?}", self);
        };

        resolved
    }
}
