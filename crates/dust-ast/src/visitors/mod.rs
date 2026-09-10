use derive_generic_visitor::Visit;
use miette::SourceSpan;
use utils::{BinaryOp, Box, Ident, Lit, Symbol, UnaryOp};

use crate::{
    Binary, Block, Call, Expr, Func, Item, ItemType, Let, Literal, Module, Path, Stmt, Unary, Use,
    Visibility, VisibilityType,
};

mod path;

pub use path::*;

#[derive(derive_generic_visitor::Visitor, derive_generic_visitor::Visit)]
#[visit(drive(for<'ast> &'ast Module<'ast>))]
#[visit(enter(for<'ast> Module<'ast>))]
#[visit(drive(for<'ast> Box<'ast, [&'ast Item<'ast>]>))]
#[visit(drive(for<'ast> [&'ast Item<'ast>]))]
#[visit(drive(for<'ast> &'ast Item<'ast>))]
#[visit(enter(for<'ast> Item<'ast>))]
#[visit(drive(for<'ast> ItemType<'ast>))]
#[visit(drive(for<'ast> &'ast Func<'ast>))]
#[visit(enter(for<'ast> Func<'ast>))]
#[visit(drive(for<'ast> &'ast Block<'ast>))]
#[visit(enter(for<'ast> Block<'ast>))]
#[visit(drive(for<'ast> Option<&'ast Expr<'ast>>))]
#[visit(drive(for<'ast> &'ast Expr<'ast>))]
#[visit(enter(for<'ast> Expr<'ast>))]
#[visit(drive(for<'ast> &'ast Call<'ast>))]
#[visit(enter(for<'ast> Call<'ast>))]
#[visit(drive(for<'ast> &'ast Binary<'ast>))]
#[visit(enter(for<'ast> Binary<'ast>))]
#[visit(drive(for<'ast> &'ast Unary<'ast>))]
#[visit(enter(for<'ast> Unary<'ast>))]
#[visit(drive(for<'ast> &'ast Literal<'ast>))]
#[visit(enter(for<'ast> Literal<'ast>))]
#[visit(skip(for<'ast> &'ast Lit))]
#[visit(drive(BinaryOp))]
#[visit(drive(UnaryOp))]
#[visit(drive(for<'ast> Box<'ast, [&'ast Stmt<'ast>]>))]
#[visit(drive(for<'ast> [&'ast Stmt<'ast>]))]
#[visit(drive(for<'ast> &'ast Stmt<'ast>))]
#[visit(enter(for<'ast> Stmt<'ast>))]
#[visit(drive(for<'ast> &'ast Let<'ast>))]
#[visit(enter(for<'ast> Let<'ast>))]
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
#[allow(dead_code)] // TODO
struct AstVisitor<V: Visitor>(pub V);

#[allow(dead_code)] // TODO
impl<V: Visitor> AstVisitor<V> {
    pub fn visit<'ast>(self, module: &'ast Module<'ast>) {
        self.visit_by_val_infallible(module);
    }

    fn enter_module<'ast>(&mut self, p: &'ast Module<'ast>) {
        self.0.enter_module(p)
    }
    fn enter_block<'ast>(&mut self, p: &'ast Block<'ast>) {
        self.0.enter_block(p)
    }
    fn enter_call<'ast>(&mut self, p: &'ast Call<'ast>) {
        self.0.enter_call(p)
    }
    fn enter_func<'ast>(&mut self, p: &'ast Func<'ast>) {
        self.0.enter_func(p)
    }
    fn enter_use<'ast>(&mut self, p: &'ast Use<'ast>) {
        self.0.enter_use(p)
    }
    fn enter_stmt<'ast>(&mut self, p: &'ast Stmt<'ast>) {
        self.0.enter_stmt(p)
    }
    fn enter_expr<'ast>(&mut self, p: &'ast Expr<'ast>) {
        self.0.enter_expr(p)
    }
    fn enter_ident<'ast>(&mut self, p: &'ast Ident) {
        self.0.enter_ident(p)
    }
    fn enter_item<'ast>(&mut self, p: &'ast Item<'ast>) {
        self.0.enter_item(p)
    }
    fn enter_let<'ast>(&mut self, p: &'ast Let<'ast>) {
        self.0.enter_let(p)
    }
    fn enter_literal<'ast>(&mut self, p: &'ast Literal<'ast>) {
        self.0.enter_literal(p)
    }
    fn enter_path<'ast>(&mut self, p: &Path<'ast>) {
        self.0.enter_path(p)
    }
    fn enter_binary<'ast>(&mut self, p: &Binary<'ast>) {
        self.0.enter_binary(p);
    }
    fn enter_unary<'ast>(&mut self, p: &Unary<'ast>) {
        self.0.enter_unary(p);
    }
}

pub trait Visitor {
    fn enter_module<'ast>(&mut self, _: &'ast Module<'ast>) {}
    fn enter_block<'ast>(&mut self, _: &'ast Block<'ast>) {}
    fn enter_call<'ast>(&mut self, _: &'ast Call<'ast>) {}
    fn enter_func<'ast>(&mut self, _: &'ast Func<'ast>) {}
    fn enter_use<'ast>(&mut self, _: &'ast Use<'ast>) {}
    fn enter_stmt<'ast>(&mut self, _: &'ast Stmt<'ast>) {}
    fn enter_expr<'ast>(&mut self, _: &'ast Expr<'ast>) {}
    fn enter_ident<'ast>(&mut self, _: &'ast Ident) {}
    fn enter_item<'ast>(&mut self, _: &'ast Item<'ast>) {}
    fn enter_let<'ast>(&mut self, _: &'ast Let<'ast>) {}
    fn enter_literal<'ast>(&mut self, _: &'ast Literal) {}
    fn enter_path<'ast>(&mut self, _: &Path<'ast>) {}
    fn enter_binary<'ast>(&mut self, _: &Binary<'ast>) {}
    fn enter_unary<'ast>(&mut self, _: &Unary<'ast>) {}
}
