use std::fmt::Pointer;

use miette::SourceSpan;
use utils::{BinaryOp, Box, Ident, Lit, UnaryOp};

// A module exists in the AST only for namespace scoping
// so we don't need to include it in the HIR.

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive, Debug)]
pub struct Krate<'hir> {
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
    #[serde(with = "utils::boxed_slice_serialize_with")]
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
    Call(&'hir Call<'hir>),
    Binary(&'hir Binary<'hir>),
    Unary(&'hir Unary<'hir>),
    Literal(&'hir Literal<'hir>),
    Assign,
    /// A namespace resolution
    Res(&'hir dust_resolve::Res),
    Block(&'hir Block<'hir>),
    If,
    Loop,
}

impl<'hir> core::fmt::Debug for Expr<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Expr::Call(arg0) => arg0.fmt(f),
            Expr::Binary(arg0) => arg0.fmt(f),
            Expr::Unary(arg0) => arg0.fmt(f),
            Expr::Literal(arg0) => arg0.fmt(f),
            Expr::Assign => todo!(),
            Expr::Res(arg0) => arg0.fmt(f),
            Expr::Block(arg0) => arg0.fmt(f),
            Expr::If => todo!(),
            Expr::Loop => todo!(),
        }
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

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Literal<'hir> {
    pub lit: &'hir Lit,
    pub span: SourceSpan,
}

impl<'hir> core::fmt::Debug for Literal<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.lit.fmt(f)
    }
}

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Binary<'hir> {
    pub lhs: &'hir Expr<'hir>,
    pub rhs: &'hir Expr<'hir>,
    pub op: BinaryOp,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Binary<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Binary")
            .field("op", &self.op)
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .finish()
    }
}

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Unary<'hir> {
    pub expr: &'hir Expr<'hir>,
    pub op: UnaryOp,
    pub span: SourceSpan,
}

impl<'hir> core::fmt::Debug for Unary<'hir> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Unary")
            .field("op", &self.op)
            .field("expr", self.expr)
            .finish()
    }
}
