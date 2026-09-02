use miette::SourceSpan;
use utils::{Box, Ident};

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Module<'ast> {
    pub ident: Option<Ident>,
    #[serde(with = "utils::box_serialize_with")]
    pub items: Box<'ast, [&'ast Item<'ast>]>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Module<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("ident", &self.ident)
            .field("items", &self.items)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Item<'ast> {
    pub r#type: ItemType<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Item<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.r#type.fmt(f)
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum ItemType<'ast> {
    Module(&'ast Module<'ast>),
    Func(&'ast Func<'ast>),
}

impl<'ast> core::fmt::Debug for ItemType<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Module(arg0) => arg0.fmt(f),
            Self::Func(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive, Debug)]
pub struct Main<'ast>(pub &'ast Func<'ast>);

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Func<'ast> {
    pub ident: Ident,
    pub block: &'ast Block<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Func<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("ident", &self.ident)
            .field("block", &self.block)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Block<'ast> {
    #[serde(with = "utils::box_serialize_with")]
    pub stmts: Box<'ast, [&'ast Stmt<'ast>]>,
    pub expr: Option<&'ast Expr<'ast>>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Block<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("expr", &self.expr)
            .field("stmts", &self.stmts)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Stmt<'ast> {
    Item(&'ast Item<'ast>),
    Let(&'ast Let<'ast>),
    Expr(&'ast Expr<'ast>),
}

impl<'ast> core::fmt::Debug for Stmt<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Item(arg0) => arg0.fmt(f),
            Self::Let(arg0) => arg0.fmt(f),
            Self::Expr(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Let<'ast> {
    pub ident: Ident,
    pub expr: Option<&'ast Expr<'ast>>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Let<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LetStatement")
            .field("ident", &self.ident)
            .field("expr", &self.expr)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Expr<'ast> {
    Arith,
    Assign,
    Call(&'ast Call<'ast>),
    Block,
    If,
    Loop,
}

impl<'ast> core::fmt::Debug for Expr<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
        // match self {
        //     Self::Arith(arg0) => arg0.fmt(f),
        //     Self::Assign => todo!(),
        //     Self::Call(arg0) => arg0.fmt(f),
        //     Self::Path(arg0) => arg0.fmt(f),
        //     Self::Block(arg0) => arg0.fmt(f),
        //     Self::IfExpr => todo!(),
        //     Self::LoopExpr => todo!(),
        // }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Call<'ast> {
    pub func: &'ast Func<'ast>,
    pub span: SourceSpan,
}
