use dust_ast::{
    Block, Call, Expr, Func, Item, ItemType, Let, Module, Path, Stmt, Use, Visibility,
    VisibilityType,
};
use miette::LabeledSpan;
use utils::Ident;

use crate::SourceLabeller;

impl<'a, 'b> SourceLabeller for &Module<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        if let Some(ident_span) = self.ident_span {
            Ident {
                symbol: self.ident,
                span: ident_span,
            }
            .label(labels);
        }

        for item in &self.items {
            item.label(labels);
        }
    }
}

impl<'a, 'b> SourceLabeller for &Item<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        if let Some(vis) = &self.vis {
            vis.label(labels);
        }

        match &self.r#type {
            ItemType::Module(module) => module.label(labels),
            ItemType::Func(function) => function.label(labels),
            ItemType::Use(path) => path.label(labels),
        }
    }
}

impl<'a> SourceLabeller for &Use<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.path.label(labels);
    }
}

impl<'a, 'b> SourceLabeller for &Func<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);
        self.block.label(labels);
    }
}

impl<'a, 'b> SourceLabeller for &Block<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        for stmt in &self.stmts {
            stmt.label(labels);
        }

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl<'a, 'b> SourceLabeller for &Stmt<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        match self {
            Stmt::Item(item) => item.label(labels),
            Stmt::Let(let_statement) => let_statement.label(labels),
            Stmt::Expr(expression) => {
                expression.label(labels);
            }
        }
    }
}

impl<'a, 'b> SourceLabeller for &Let<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl<'a, 'b> SourceLabeller for &Expr<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        match *self {
            Expr::Assign => todo!(),
            Expr::Call(call_expression) => call_expression.label(labels),
            Expr::Path(path) => path.label(labels),
            Expr::Block(block) => block.label(labels),
            Expr::If => todo!(),
            Expr::Loop => todo!(),
            Expr::Binary(_) => {}
            Expr::Unary(_) => {}
            Expr::Literal(_) => {}
        }
    }
}

impl<'a, 'b> SourceLabeller for &Call<'a, 'b> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.expr.span(), "call"));
    }
}

impl<'a> SourceLabeller for &Path<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.span, "path"));
    }
}

impl SourceLabeller for &Visibility {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(
            self.span,
            match self.r#type {
                VisibilityType::Pub => "pub",
            },
        ));
    }
}

impl SourceLabeller for &Ident {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.span, "ident"));
    }
}
