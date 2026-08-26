//!block_expr     → "{"
//!                    (
//!                        statement*
//!                      | statement* expression_w/o_block
//!                    )
//!                  "}" ;
//!
//! statement      → ";"
//!                | item
//!                | let_stmt
//!                | (expression ";")   ;
//!
//! let_stmt       → "let" ident ("=" expression )? ";"

use miette::{LabeledSpan, Result};
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Parser, parser::Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    stmts: Box<[Token<Statement<'a>>]>,
    expr: Option<Token<Expression<'a>>>,
}

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
    pub(crate) fn block(&mut self) -> Result<Token<Block<'a>>> {
        let left_brace = self.expect_token(TokenKind::LeftBrace)?;

        let mut statements = Vec::new();
        while let Some(statement) = self.statement()? {
            statements.push(statement);
        }

        let expression = if !matches!(self.first_token_kind(), Some(TokenKind::RightBrace)) {
            Some(self.expression()?)
        } else {
            None
        };

        let right_brace = self.expect_token(TokenKind::RightBrace)?;

        Ok(Token {
            kind: Block {
                stmts: statements.into_boxed_slice(),
                expr: expression,
            },
            src: combine_src(left_brace.src, right_brace.src),
        })
    }

    fn statement(&mut self) -> Result<Option<Token<Statement<'a>>>> {
        loop {
            match self.first_token_kind() {
                Some(TokenKind::Semicolon) => {
                    self.expect_token(TokenKind::Semicolon)?;
                }
                Some(TokenKind::Let) => {
                    return Ok(Some(self.let_stmt()?));
                }
                Some(_) => {
                    // Expression

                    let old_parser = self.clone();

                    let expr = self.expression()?;
                    let semi = self.expect_token(TokenKind::Semicolon);

                    if let Ok(semi) = semi {
                        return Ok(Some(Token {
                            kind: Statement::Expression(expr.kind),
                            src: combine_src(expr.src, semi.src),
                        }));
                    } else {
                        // No semicolon;
                        *self = old_parser;
                        return Ok(None);
                    }
                }
                None => return Ok(None),
            }
        }
    }

    fn let_stmt(&mut self) -> Result<Token<Statement<'a>>> {
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
            _ => Err(miette::miette!(
                labels = vec![LabeledSpan::at(ident.src, "expected '=' or ';' following")],
                "expected '=' or ';'",
            )
            .with_source_code(self.source.to_owned())),
        }
    }
}

#[cfg(test)]
mod tests {
    use miette::{SourceOffset, SourceSpan};
    use utils::{Ident, Token};

    use crate::{
        Arithmetic, Block, LetStatement, Parser, Primitive, Statement,
        parser::expression::Expression,
    };

    #[test]
    fn test_statement() {
        let test_script = include_str!("../../../../assets/tests/ast-parser/statement.dst");
        let mut parser = Parser::new(test_script);
        let mut statements = Vec::new();

        while let Some(token) = parser.statement().unwrap() {
            statements.push(token.kind);
        }

        assert_eq!(
            statements,
            vec![
                Statement::LetStatement(LetStatement {
                    ident: Token {
                        kind: Ident("foo"),
                        src: SourceSpan::new(SourceOffset::from(49), 3)
                    },
                    expr: None
                }),
                Statement::LetStatement(LetStatement {
                    ident: Token {
                        kind: Ident("bar"),
                        src: SourceSpan::new(SourceOffset::from(58), 3)
                    },
                    expr: Some(Token {
                        kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Number(2.0))),
                        src: SourceSpan::new(SourceOffset::from(64), 1)
                    })
                }),
                Statement::LetStatement(LetStatement {
                    ident: Token {
                        kind: Ident("far"),
                        src: SourceSpan::new(SourceOffset::from(71), 3)
                    },
                    expr: Some(Token {
                        kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(true))),
                        src: SourceSpan::new(SourceOffset::from(77), 18)
                    })
                }),
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

    #[test]
    fn test_block() {
        let test_script = include_str!("../../../../assets/tests/ast-parser/block.dst");
        let mut parser = Parser::new(test_script);
        let mut blocks = Vec::new();

        while let Ok(token) = parser.block() {
            blocks.push(token.kind);
        }

        assert_eq!(
            blocks,
            vec![
                Block {
                    stmts: vec![
                        Token {
                            kind: Statement::LetStatement(LetStatement {
                                ident: Token {
                                    kind: Ident("foo"),
                                    src: SourceSpan::new(SourceOffset::from(10), 3)
                                },
                                expr: None
                            }),
                            src: SourceSpan::new(SourceOffset::from(6), 8)
                        },
                        Token {
                            kind: Statement::Expression(Expression::Arithmetic(
                                Arithmetic::Primitive(Primitive::Number(15.0))
                            )),
                            src: SourceSpan::new(SourceOffset::from(19), 5)
                        }
                    ]
                    .into_boxed_slice(),
                    expr: Some(Token {
                        kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(true))),
                        src: SourceSpan::new(SourceOffset::from(30), 4)
                    })
                },
                Block {
                    stmts: vec![Token {
                        kind: Statement::LetStatement(LetStatement {
                            ident: Token {
                                kind: Ident("foo"),
                                src: SourceSpan::new(SourceOffset::from(48), 3)
                            },
                            expr: None
                        }),
                        src: SourceSpan::new(SourceOffset::from(44), 8)
                    }]
                    .into_boxed_slice(),
                    expr: Some(Token {
                        kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Number(2.0))),
                        src: SourceSpan::new(SourceOffset::from(82), 3)
                    })
                }
            ]
        );
    }
}
