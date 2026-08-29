use dust_ast::{
    Block, CallExpr, Expr, Function, Item, ItemType, LetStatement, Module, Path, Statement, Use,
    Visibility, VisibilityType,
};
use miette::LabeledSpan;
use utils::Ident;

/// Recurse a data structure, labelling each part as you go
pub trait LabelPrinter {
    fn label(self, labels: &mut Vec<LabeledSpan>);
}

impl<'a> LabelPrinter for &Module<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        if let Some(ident) = &self.ident {
            ident.label(labels);
        }

        for item in &self.items {
            item.label(labels);
        }
    }
}

impl<'a> LabelPrinter for &Item<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        if let Some(vis) = &self.vis {
            vis.label(labels);
        }

        match &self.r#type {
            ItemType::Module(module) => module.label(labels),
            ItemType::Function(function) => function.label(labels),
            ItemType::Use(path) => path.label(labels),
        }
    }
}

impl<'a> LabelPrinter for &Use<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.path.label(labels);
    }
}

impl<'a> LabelPrinter for &Function<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);
        self.block.label(labels);
    }
}

impl<'a> LabelPrinter for &Block<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        for stmt in &self.stmts {
            stmt.label(labels);
        }

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl<'a> LabelPrinter for &Statement<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        match self {
            Statement::Semicolon => {}
            Statement::Item(item) => item.label(labels),
            Statement::LetStatement(let_statement) => let_statement.label(labels),
            Statement::Expression(expression) => {
                expression.label(labels);
            }
        }
    }
}

impl<'a> LabelPrinter for &LetStatement<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl<'a> LabelPrinter for &Expr<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        match self {
            Expr::Arith(arith) => {
                labels.push(LabeledSpan::at(arith.span(), "arithmetic"));
            }
            Expr::Assign => todo!(),
            Expr::CallExpr(call_expression) => call_expression.label(labels),
            Expr::Path(path) => path.label(labels),
            Expr::Block(block) => block.label(labels),
            Expr::IfExpr => todo!(),
            Expr::LoopExpr => todo!(),
        }
    }
}

impl<'a> LabelPrinter for &CallExpr<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.expr.span(), "call"));
    }
}

impl<'a> LabelPrinter for &Path<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.span, "path"));
    }
}

impl LabelPrinter for &Visibility {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(
            self.span,
            match self.r#type {
                VisibilityType::Pub => "pub",
            },
        ));
    }
}

impl LabelPrinter for &Ident {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.span, "ident"));
    }
}
