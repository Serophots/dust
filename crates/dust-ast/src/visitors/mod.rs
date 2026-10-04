use derive_generic_visitor::Visit;
use miette::SourceSpan;
use utils::{BinaryOp, Box, Ident, Lit, NodeId, Symbol, UnaryOp};

use crate::{
    Binary, Block, Call, Expr, Func, Item, ItemType, Let, Literal, Module, Path, Stmt, Unary, Use,
    Visibility, VisibilityType,
};

mod path;

pub use path::*;

#[derive(derive_generic_visitor::Visitor, derive_generic_visitor::Visit)]
#[visit(drive(for<'gcx, 'ast> &'ast Module<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Module<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> Box<'ast, [&'ast Item<'gcx, 'ast>]>))]
#[visit(drive(for<'gcx, 'ast> [&'ast Item<'gcx, 'ast>]))]
#[visit(drive(for<'gcx, 'ast> &'ast Item<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Item<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> ItemType<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> &'ast Func<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Func<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> &'ast Block<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Block<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> Option<&'ast Expr<'gcx, 'ast>>))]
#[visit(drive(for<'gcx, 'ast> &'ast Expr<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Expr<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> &'ast Call<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Call<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> &'ast Binary<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Binary<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> &'ast Unary<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Unary<'gcx, 'ast>))]
#[visit(drive(for<'ast> &'ast Literal<'ast>))]
#[visit(enter(for<'ast> Literal<'ast>))]
#[visit(skip(for<'ast> &'ast Lit))]
#[visit(drive(BinaryOp))]
#[visit(drive(UnaryOp))]
#[visit(drive(for<'gcx, 'ast> Box<'ast, [&'ast Stmt<'gcx, 'ast>]>))]
#[visit(drive(for<'gcx, 'ast> [&'ast Stmt<'gcx, 'ast>]))]
#[visit(drive(for<'gcx, 'ast> &'ast Stmt<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Stmt<'gcx, 'ast>))]
#[visit(drive(for<'gcx, 'ast> &'ast Let<'gcx, 'ast>))]
#[visit(enter(for<'gcx, 'ast> Let<'gcx, 'ast>))]
#[visit(drive(for<'ast> &'ast Use<'ast>))]
#[visit(enter(for<'ast> Use<'ast>))]
#[visit(drive(for<'ast> &'ast Path<'ast>))]
#[visit(enter(for<'ast> Path<'ast>))]
#[visit(drive(for<'ast> Box<'ast, [Ident]>))]
#[visit(drive([Ident]))]
#[visit(enter(Ident))]
#[visit(drive(Option<Visibility>))]
#[visit(drive(Visibility))]
#[visit(drive(VisibilityType))]
#[visit(drive(Option<Ident>))]
#[visit(drive(Symbol))]
#[visit(skip(string_interner::symbol::SymbolUsize))]
#[visit(skip(Option<SourceSpan>))]
#[visit(skip(SourceSpan))]
#[visit(skip(f64))]
#[visit(skip(bool))]
#[visit(skip(for<'a> &'a str))]
#[visit(skip(NodeId))]
#[allow(dead_code)] // TODO
struct AstVisitor<V: Visitor>(pub V);

#[allow(dead_code)] // TODO
impl<V: Visitor> AstVisitor<V> {
    pub fn visit<'gcx, 'ast>(self, module: &'ast Module<'gcx, 'ast>) {
        self.visit_by_val_infallible(module);
    }

    fn enter_module<'gcx, 'ast>(&mut self, p: &'ast Module<'gcx, 'ast>) {
        self.0.enter_module(p)
    }
    fn enter_block<'gcx, 'ast>(&mut self, p: &'ast Block<'gcx, 'ast>) {
        self.0.enter_block(p)
    }
    fn enter_call<'gcx, 'ast>(&mut self, p: &'ast Call<'gcx, 'ast>) {
        self.0.enter_call(p)
    }
    fn enter_func<'gcx, 'ast>(&mut self, p: &'ast Func<'gcx, 'ast>) {
        self.0.enter_func(p)
    }
    fn enter_use<'ast>(&mut self, p: &'ast Use<'ast>) {
        self.0.enter_use(p)
    }
    fn enter_stmt<'gcx, 'ast>(&mut self, p: &'ast Stmt<'gcx, 'ast>) {
        self.0.enter_stmt(p)
    }
    fn enter_expr<'gcx, 'ast>(&mut self, p: &'ast Expr<'gcx, 'ast>) {
        self.0.enter_expr(p)
    }
    fn enter_ident<'ast>(&mut self, p: &'ast Ident) {
        self.0.enter_ident(p)
    }
    fn enter_item<'gcx, 'ast>(&mut self, p: &'ast Item<'gcx, 'ast>) {
        self.0.enter_item(p)
    }
    fn enter_let<'gcx, 'ast>(&mut self, p: &'ast Let<'gcx, 'ast>) {
        self.0.enter_let(p)
    }
    fn enter_literal<'ast>(&mut self, p: &'ast Literal<'ast>) {
        self.0.enter_literal(p)
    }
    fn enter_path<'ast>(&mut self, p: &Path<'ast>) {
        self.0.enter_path(p)
    }
    fn enter_binary<'gcx, 'ast>(&mut self, p: &Binary<'gcx, 'ast>) {
        self.0.enter_binary(p);
    }
    fn enter_unary<'gcx, 'ast>(&mut self, p: &Unary<'gcx, 'ast>) {
        self.0.enter_unary(p);
    }
}

pub trait Visitor {
    fn enter_module<'gcx, 'ast>(&mut self, _: &'ast Module<'gcx, 'ast>) {}
    fn enter_block<'gcx, 'ast>(&mut self, _: &'ast Block<'gcx, 'ast>) {}
    fn enter_call<'gcx, 'ast>(&mut self, _: &'ast Call<'gcx, 'ast>) {}
    fn enter_func<'gcx, 'ast>(&mut self, _: &'ast Func<'gcx, 'ast>) {}
    fn enter_use<'ast>(&mut self, _: &'ast Use<'ast>) {}
    fn enter_stmt<'gcx, 'ast>(&mut self, _: &'ast Stmt<'gcx, 'ast>) {}
    fn enter_expr<'gcx, 'ast>(&mut self, _: &'ast Expr<'gcx, 'ast>) {}
    fn enter_ident<'ast>(&mut self, _: &'ast Ident) {}
    fn enter_item<'gcx, 'ast>(&mut self, _: &'ast Item<'gcx, 'ast>) {}
    fn enter_let<'gcx, 'ast>(&mut self, _: &'ast Let<'gcx, 'ast>) {}
    fn enter_literal<'ast>(&mut self, _: &'ast Literal) {}
    fn enter_path<'ast>(&mut self, _: &Path<'ast>) {}
    fn enter_binary<'gcx, 'ast>(&mut self, _: &Binary<'gcx, 'ast>) {}
    fn enter_unary<'gcx, 'ast>(&mut self, _: &Unary<'gcx, 'ast>) {}
}
