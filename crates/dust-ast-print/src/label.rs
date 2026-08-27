use dust_ast::{
    Block, CallExpression, Expression, Function, Item, ItemType, LetStatement, Module, Path,
    Statement, Use, Visibility,
};
use miette::LabeledSpan;
use utils::{Ident, Token};

/// Recurse a data structure, labelling each part as you go
pub trait LabelPrinter {
    fn label(self, labels: &mut Vec<LabeledSpan>);
}

impl<'a, T> LabelPrinter for &'a Token<T>
where
    Token<&'a T>: LabelPrinter,
{
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        let token = Token {
            kind: &self.kind,
            src: self.src,
        };
        token.label(labels);
    }
}

impl<'a> LabelPrinter for &Module {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        if let Some(ident) = &self.ident {
            ident.label(labels);
        }

        for item in &self.items {
            item.label(labels);
        }
    }
}

impl<'a> LabelPrinter for &Item {
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

impl LabelPrinter for &Token<Item> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.kind.label(labels);
    }
}

impl LabelPrinter for &Use {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.path.label(labels);
    }
}

impl LabelPrinter for &Function {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);
        self.block.label(labels);
    }
}

impl LabelPrinter for &Block {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        for stmt in &self.stmts {
            stmt.label(labels);
        }

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl LabelPrinter for &Token<Block> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.kind.label(labels);
    }
}

impl LabelPrinter for Token<&Statement> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        match &self.kind {
            Statement::Semicolon => {}
            Statement::Item(item) => item.label(labels),
            Statement::LetStatement(let_statement) => let_statement.label(labels),
            Statement::Expression(expression) => {
                let token = Token {
                    kind: expression,
                    src: self.src,
                };

                token.label(labels);
            }
        }
    }
}

impl LabelPrinter for &LetStatement {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl LabelPrinter for Token<&Expression> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        match self.kind {
            Expression::Arithmetic(_) => {
                labels.push(LabeledSpan::at(self.src, "arithmetic"));
            }
            Expression::Assign => todo!(),
            Expression::CallExpr(call_expression) => call_expression.label(labels),
            Expression::Path(path) => {
                let token = Token {
                    kind: path,
                    src: self.src,
                };

                token.label(labels);
            }
            Expression::Block(block) => block.label(labels),
            Expression::IfExpr => todo!(),
            Expression::LoopExpr => todo!(),
        }
    }
}

impl LabelPrinter for &CallExpression {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.expr.src, "call"));
    }
}

impl LabelPrinter for Token<&Path> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.src, "path"));
    }
}

impl LabelPrinter for Token<&Visibility> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(
            self.src,
            match self.kind {
                Visibility::Pub => "pub",
            },
        ));
    }
}

impl LabelPrinter for Token<&Ident> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.src, "ident"));
    }
}
