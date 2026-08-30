use string_interner::Symbol as _;

/// An interned symbol from the source
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize)]
pub struct Symbol(string_interner::symbol::SymbolUsize);

impl core::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Symbol").field(&self.0.to_usize()).finish()
    }
}

impl string_interner::Symbol for Symbol {
    fn try_from_usize(index: usize) -> Option<Self> {
        string_interner::symbol::SymbolUsize::try_from_usize(index).map(Symbol)
    }

    fn to_usize(self) -> usize {
        self.0.to_usize()
    }
}
