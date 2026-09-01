use derive_generic_visitor::Visit;
use miette::SourceSpan;
use utils::{Box, Ident, Symbol};

use crate::{
    Arith, BinaryOperation, Block, Call, Expr, Func, Item, ItemType, Let, Module, Path, Primitive,
    Stmt, Use, Visibility, VisibilityType,
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
#[visit(drive(for<'ast> &'ast Arith<'ast>))]
#[visit(enter(for<'ast> Arith<'ast>))]
#[visit(enter(for<'ast> Primitive))]
#[visit(drive(for<'ast> BinaryOperation))]
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
#[visit(skip(SourceSpan))]
struct AstVisitor<V: Visitor>(pub V);

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
    fn enter_arith<'ast>(&mut self, p: &'ast Arith<'ast>) {
        self.0.enter_arith(p)
    }
    fn enter_primitive<'ast>(&mut self, p: &'ast Primitive) {
        self.0.enter_primitive(p)
    }
    fn enter_path<'ast>(&mut self, p: &Path<'ast>) {
        self.0.enter_path(p)
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
    fn enter_arith<'ast>(&mut self, _: &'ast Arith<'ast>) {}
    fn enter_primitive<'ast>(&mut self, _: &'ast Primitive) {}
    fn enter_path<'ast>(&mut self, _: &Path<'ast>) {}
}
