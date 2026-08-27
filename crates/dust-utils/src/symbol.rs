use std::cell::RefCell;

use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolUsize};

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

/// An interned symbol from the source
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(SymbolUsize);

impl string_interner::Symbol for Symbol {
    fn try_from_usize(index: usize) -> Option<Self> {
        SymbolUsize::try_from_usize(index).map(Symbol)
    }

    fn to_usize(self) -> usize {
        self.0.to_usize()
    }
}

impl SymbolInterner {
    pub fn get_or_intern<T: AsRef<str>>(&self, string: T) -> Symbol {
        self.interner.borrow_mut().get_or_intern(string)
    }
}
