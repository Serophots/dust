//! statement      → ";"
//!                | item
//!                | let_stmt
//!                | (expression ";")   ;
//!
//! let_stmt       → "let" ident ("=" expression )? ";"

use miette::Result;
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Parser, parser::Expression};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Semicolon,
    Item,
    LetStatement(LetStatement<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement<'a> {
    ident: Token<Ident<'a>>,
    expr: Option<Token<Expression<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn statement(&mut self) -> Result<Token<Statement<'a>>> {
        loop {
            match self.first_token_kind() {
                Some(TokenKind::Semicolon) => {
                    self.expect_token(TokenKind::Semicolon)?;
                }
                Some(TokenKind::Let) => {
                    return self.let_stmt();
                }
                Some(_) => {
                    // Expression
                    let expr = self.expression()?.map(Statement::Expression);
                    self.expect_token(TokenKind::Semicolon)?;
                    return Ok(expr);
                }
                None => todo!(),
            }
        }
    }

    pub fn let_stmt(&mut self) -> Result<Token<Statement<'a>>> {
        let r#let = self.expect_token(TokenKind::Let)?;
        let ident = self.expect_token_ident()?;

        match self.first_token_kind() {
            Some(TokenKind::Equal) => {
                self.expect_token(TokenKind::Equal)?;
                let expr = self.expression()?;
                let semi = self.expect_token(TokenKind::Semicolon)?;

                Ok(Token {
                    kind: Statement::LetStatement(LetStatement {
                        ident,
                        expr: Some(expr),
                    }),
                    src: combine_src(r#let.src, semi.src),
                })
            }
            Some(TokenKind::Semicolon) => {
                let semi = self.expect_token(TokenKind::Semicolon)?;
                Ok(Token {
                    kind: Statement::LetStatement(LetStatement { ident, expr: None }),
                    src: combine_src(r#let.src, semi.src),
                })
            }
            _ => Err(todo!()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Arithmetic, Parser, Primitive, Statement, parser::expression::Expression};

    #[test]
    fn test_lexer() {
        let test_script = include_str!("../../../../assets/tests/ast-parser/statement.dst");
        let mut parser = Parser::new(test_script);
        let mut statements = Vec::new();

        while let token = parser.statement().unwrap() {
            statements.push(token.kind);
        }

        assert_eq!(
            statements,
            vec![
                Statement::Expression(Expression::Arithmetic(Arithmetic::Primitive(
                    Primitive::Number(4.0)
                ))),
                Statement::Expression(Expression::Arithmetic(Arithmetic::Primitive(
                    Primitive::Bool(true)
                ))),
                Statement::Expression(Expression::Arithmetic(Arithmetic::Primitive(
                    Primitive::Bool(false)
                ))),
            ]
        );
    }
}
