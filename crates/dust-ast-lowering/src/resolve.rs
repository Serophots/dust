use ahash::{HashMap, HashMapExt};
use dust_ctxt::GblCtxt;
use utils::{Ident, Symbol};

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
pub struct Rib<'ast, 'gcx> {
    pub bindings: HashMap<Symbol, Res<'ast, 'gcx>>,
    pub kind: RibKind,
}

pub enum RibKind {
    Normal,

    Block,

    Fn,

    Module,
}

#[derive(Default)]
pub struct ResolverCtx<'ast, 'gcx> {
    pub ribs: ForNamespaces<Vec<Rib<'ast, 'gcx>>>,
}

impl<'ast, 'hir, 'gcx> ResolverCtx<'ast, 'gcx> {
    pub fn push_rib<F>(&mut self, namespace: Namespace, kind: RibKind, f: F)
    where
        F: FnOnce(&mut Rib<'ast, 'gcx>),
    {
        let mut rib = Rib {
            bindings: Default::default(),
            kind,
        };

        f(&mut rib);

        self.ribs[namespace].push(rib);
    }

    pub fn resolve_ident(&mut self, ident: Ident, namespace: Namespace) -> Option<Res<'ast, 'gcx>> {
        let ribs = self.ribs[namespace].iter();

        for rib in ribs.rev() {
            if let Some(res) = rib.bindings.get(&ident.symbol) {
                return Some(*res);
            }
        }

        None
    }

    pub fn inspect_namespace(
        &self,
        namespace: Namespace,
        ctx: GblCtxt,
    ) -> HashMap<String, Res<'ast, 'gcx>> {
        let ribs = self.ribs[namespace].iter();

        let mut bindings: HashMap<String, Res<'ast, 'gcx>> = HashMap::new();

        for rib in ribs {
            bindings.extend(
                rib.bindings
                    .iter()
                    .map(|(&symbol, res)| (ctx.symbols.resolve(symbol).unwrap(), *res)),
            );
        }

        bindings
    }
}

/// A namespace resolution
#[derive(Copy, Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Res<'ast, 'gcx> {
    /// Local variable or function parameter.
    /// The ident span must point to the defining site
    /// for this local variable for proper equality.
    ///
    /// **Value namespace**
    Local(Ident),

    /// A function
    ///
    /// **Value namespace**
    Func(&'ast dust_ast::Func<'gcx, 'ast>),
}

impl<'ast, 'gcx> core::fmt::Debug for Res<'ast, 'gcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Res::Local(arg0) => f.debug_tuple("Local").field(arg0).finish(),
            Res::Func(arg0) => f.debug_tuple("Func").field(arg0).finish(),
        }
    }
}
