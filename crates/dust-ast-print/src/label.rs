use dust_ast::{
    Block, Expression, Function, Item, ItemType, LetStatement, Module, Statement, Visibility,
};
use miette::LabeledSpan;
use utils::{Ident, Token};

/// Recurse a data structure, labelling each part as you go
pub trait LabelPrinter {
    fn label(self, labels: &mut Vec<LabeledSpan>);
}

impl<'a, T> LabelPrinter for &'a Token<T>
where
    &'a T: LabelPrinter,
{
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.kind.label(labels);
    }
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
        }
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

impl<'a> LabelPrinter for &Token<Statement<'a>> {
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

impl<'a> LabelPrinter for &LetStatement<'a> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        self.ident.label(labels);

        if let Some(expr) = &self.expr {
            expr.label(labels);
        }
    }
}

impl<'a> LabelPrinter for Token<&Expression<'a>> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.src, "expr"));
    }
}

impl<'a> LabelPrinter for &Token<Visibility> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(
            self.src,
            match self.kind {
                Visibility::Pub => "pub",
            },
        ));
    }
}

impl<'a> LabelPrinter for &Token<Ident<'a>> {
    fn label(self, labels: &mut Vec<LabeledSpan>) {
        labels.push(LabeledSpan::at(self.src, "ident"));
    }
}
