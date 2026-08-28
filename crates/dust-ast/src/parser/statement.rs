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

use crate::{Item, Parser, parser::Expression};

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Box<[Token<Statement>]>,
    pub expr: Option<Token<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Semicolon,
    Item(Item),
    LetStatement(LetStatement),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub ident: Token<Ident>,
    pub expr: Option<Token<Expression>>,
}

impl<'a> Parser<'a> {
    pub(crate) fn block(&mut self) -> Result<Token<Block>> {
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

    fn statement(&mut self) -> Result<Option<Token<Statement>>> {
        loop {
            // First, try to pass an item
            if let Some(item) = self.try_to_parse(|parser| {
                //
                parser.item().ok()
            }) {
                return Ok(Some(item.map(Statement::Item)));
            }

            match self.first_token_kind() {
                Some(TokenKind::Semicolon) => {
                    self.expect_token(TokenKind::Semicolon)?;
                }
                Some(TokenKind::Let) => {
                    return Ok(Some(self.let_stmt()?));
                }
                Some(_) => {
                    // Expression

                    if let Some((expr, semi)) = self.try_to_parse(|parser| {
                        let expr = parser.expression().ok()?;
                        let semi = parser.expect_token(TokenKind::Semicolon).ok()?;
                        Some((expr, semi))
                    }) {
                        return Ok(Some(Token {
                            kind: Statement::Expression(expr.kind),
                            src: combine_src(expr.src, semi.src),
                        }));
                    } else {
                        return Ok(None);
                    };
                }
                None => return Ok(None),
            }
        }
    }

    fn let_stmt(&mut self) -> Result<Token<Statement>> {
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
    use dust_ctxt::create_and_enter_global_ctxt;
    use miette::{SourceOffset, SourceSpan};
    use utils::{Ident, Token};

    use crate::{
        Arithmetic, Block, LetStatement, Parser, Primitive, Statement,
        parser::expression::Expression,
    };

    #[test]
    fn test_statement() {
        create_and_enter_global_ctxt(|ctx| {
            let test_script = include_str!("../../../../assets/tests/ast-parser/statement.dst");
            let mut parser = Parser::new(test_script, ctx);
            let mut statements = Vec::new();

            while let Some(token) = parser.statement().unwrap() {
                statements.push(token.kind);
            }

            assert_eq!(
                statements,
                vec![
                    Statement::LetStatement(LetStatement {
                        ident: Token {
                            kind: Ident(ctx.symbols.get_or_intern("foo")),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        },
                        expr: None
                    }),
                    Statement::LetStatement(LetStatement {
                        ident: Token {
                            kind: Ident(ctx.symbols.get_or_intern("bar")),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        },
                        expr: Some(Token {
                            kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Number(
                                2.0
                            ))),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        })
                    }),
                    Statement::LetStatement(LetStatement {
                        ident: Token {
                            kind: Ident(ctx.symbols.get_or_intern("far")),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        },
                        expr: Some(Token {
                            kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(
                                true
                            ))),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
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
        });
    }

    #[test]
    fn test_block() {
        create_and_enter_global_ctxt(|ctx| {
            let test_script = include_str!("../../../../assets/tests/ast-parser/block.dst");
            let mut parser = Parser::new(test_script, ctx);
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
                                        kind: Ident(ctx.symbols.get_or_intern("foo")),
                                        src: SourceSpan::new(SourceOffset::from(0), 0),
                                    },
                                    expr: None
                                }),
                                src: SourceSpan::new(SourceOffset::from(0), 0),
                            },
                            Token {
                                kind: Statement::Expression(Expression::Arithmetic(
                                    Arithmetic::Primitive(Primitive::Number(15.0))
                                )),
                                src: SourceSpan::new(SourceOffset::from(0), 0),
                            }
                        ]
                        .into_boxed_slice(),
                        expr: Some(Token {
                            kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(
                                true
                            ))),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        })
                    },
                    Block {
                        stmts: vec![Token {
                            kind: Statement::LetStatement(LetStatement {
                                ident: Token {
                                    kind: Ident(ctx.symbols.get_or_intern("foo")),
                                    src: SourceSpan::new(SourceOffset::from(0), 0),
                                },
                                expr: None
                            }),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        }]
                        .into_boxed_slice(),
                        expr: Some(Token {
                            kind: Expression::Arithmetic(Arithmetic::Primitive(Primitive::Number(
                                2.0
                            ))),
                            src: SourceSpan::new(SourceOffset::from(0), 0),
                        })
                    },
                    Block {
                        stmts: vec![
                            Token::kind(Statement::LetStatement(LetStatement {
                                ident: Token::kind(Ident(ctx.symbols.get_or_intern("foo"))),
                                expr: None
                            })),
                            Token::kind(Statement::LetStatement(LetStatement {
                                ident: Token::kind(Ident(ctx.symbols.get_or_intern("bar"))),
                                expr: Some(Token::kind(Expression::Arithmetic(
                                    Arithmetic::Primitive(Primitive::Number(2.0))
                                )))
                            })),
                            Token::kind(Statement::LetStatement(LetStatement {
                                ident: Token::kind(Ident(ctx.symbols.get_or_intern("far"))),
                                expr: Some(Token::kind(Expression::Arithmetic(
                                    Arithmetic::Primitive(Primitive::Bool(true))
                                )))
                            }))
                        ]
                        .into_boxed_slice(),
                        expr: None
                    }
                ]
            );
        });
    }
}
