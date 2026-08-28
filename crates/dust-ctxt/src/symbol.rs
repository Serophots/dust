use std::cell::RefCell;

use string_interner::{StringInterner, backend::StringBackend};
use utils::Symbol;

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
}
