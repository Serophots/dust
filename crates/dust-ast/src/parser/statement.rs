//! statement      → ";"
//!                | item
//!                | let_stmt
//!                | (expression ";")   ;
//!
//! let_stmt       → "let" ident ("=" expression )? ";"

use miette::Result;
use utils::{Token, TokenKind};

use crate::{Arithmetic, Parser, parser::Expression};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Semicolon,
    Item,
    LetStatement,
    Expression(Expression<'a>),
}

impl<'a> Parser<'a> {
    pub fn statement(&mut self) -> Result<Token<Statement<'a>>> {
        match self.first_token_kind() {
            Some(TokenKind::Semicolon) => {
                // TODO: Consume ";"
                todo!()
            }
            Some(TokenKind::Let) => self.let_stmt(),
            _ => {
                let expr = self.expression()?.map(Statement::Expression);
                // TODO: Consume ";"
                Ok(expr)
            }
        }
    }

    pub fn let_stmt(&mut self) -> Result<Token<Statement<'a>>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Arithmetic, Parser, Primitive, parser::expression::Expression};

    #[test]
    fn test_lexer() {
        let test_script = include_str!("../../../../assets/tests/ast-parser/statement.dst");
        let mut parser = Parser::new(test_script);
        let mut expressions = Vec::new();

        while let Ok(token) = parser.expression() {
            expressions.push(token.kind);
        }

        assert_eq!(
            expressions,
            vec![
                Expression::Arithmetic(Arithmetic::Primitive(Primitive::Number(4.0))),
                Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(true))),
                Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(false))),
            ]
        );
    }
}
