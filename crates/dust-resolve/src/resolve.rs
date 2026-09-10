use ahash::HashMap;

#[derive(Copy, Clone)]
pub enum Namespace {
    /// Functions, consts, statics, local variables
    ValueNS,
    /// `struct`s, `enum`s, `mod`s
    TypeNs,
}

#[derive(Clone, Default)]
pub struct ForNamespaces<T> {
    pub value_ns: T,
    pub type_ns: T,
}

impl<T> core::ops::Index<Namespace> for ForNamespaces<T> {
    type Output = T;

    fn index(&self, index: Namespace) -> &Self::Output {
        match index {
            Namespace::ValueNS => &self.value_ns,
            Namespace::TypeNs => &self.type_ns,
        }
    }
}

impl<T> core::ops::IndexMut<Namespace> for ForNamespaces<T> {
    fn index_mut(&mut self, index: Namespace) -> &mut Self::Output {
        match index {
            Namespace::ValueNS => &mut self.value_ns,
            Namespace::TypeNs => &mut self.type_ns,
        }
    }
}

pub struct Res {}

/// Each namespace has a stack of ribs. Each rib
/// represents a region of the code for which these
/// bindings apply. To resolve a binding, the stack
/// is searched top to bottom, with each rib defining
/// its own transparency with respect to the sort of
/// binding being searched for
///
/// A new rib is introduced every time the accessible
/// bindings change. I.e. a let statement, any sort
/// of block.
pub struct Rib {
    bindings: HashMap<(), Res>,
    kind: RibKind,
}

pub enum RibKind {}

#[derive(Default)]
pub struct ResolverCtx {
    ribs: ForNamespaces<Vec<Rib>>,
}
