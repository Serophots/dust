use miette::SourceSpan;
use utils::{BinaryOp, Box, Ident, Literal};

// A module exists in the AST only for scoping

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive, Debug)]
pub struct Main<'hir> {
    pub main: &'hir Func<'hir>,
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Func<'hir> {
    pub ident: Ident,
    pub block: &'hir Block<'hir>,
    pub span: SourceSpan,
}

impl<'hir> core::fmt::Debug for Func<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("ident", &self.ident)
            .field("block", &self.block)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Block<'hir> {
    #[serde(with = "utils::box_serialize_with")]
    pub stmts: Box<'hir, [&'hir Stmt<'hir>]>,
    pub expr: Option<&'hir Expr<'hir>>,
    pub span: SourceSpan,
}

impl<'hir> core::fmt::Debug for Block<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("expr", &self.expr)
            .field("stmts", &self.stmts)
            .finish()
    }
}

#[derive(Copy, Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Stmt<'hir> {
    Func(&'hir Func<'hir>),
    Let(&'hir Let<'hir>),
    Expr(&'hir Expr<'hir>),
}

impl<'hir> core::fmt::Debug for Stmt<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Func(arg0) => arg0.fmt(f),
            Self::Let(arg0) => arg0.fmt(f),
            Self::Expr(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Let<'hir> {
    pub ident: Ident,
    pub expr: Option<&'hir Expr<'hir>>,
    pub span: SourceSpan,
}

impl<'hir> core::fmt::Debug for Let<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LetStatement")
            .field("ident", &self.ident)
            .field("expr", &self.expr)
            .finish()
    }
}

#[derive(Copy, Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Expr<'hir> {
    Arith,
    Assign,
    Call(&'hir Call<'hir>),
    Block,
    If,
    Loop,
}

impl<'hir> core::fmt::Debug for Expr<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
        // match self {
        //     Self::Arith(arg0) => arg0.fmt(f),
        //     Self::Assign => todo!(),
        // Self::Call(arg0) => arg0.fmt(f),
        // Self::Path(arg0) => arg0.fmt(f),
        //     Self::Block(arg0) => arg0.fmt(f),
        //     Self::IfExpr => todo!(),
        //     Self::LoopExpr => todo!(),
        // }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Call<'hir> {
    pub expr: &'hir Expr<'hir>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Call<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CallExpr").field(&self.expr).finish()
    }
}
