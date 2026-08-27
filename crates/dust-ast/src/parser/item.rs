//! module         → item* EOF | mod ident "{" item* "}";
//! item           → visibility? (
//!                   module | function
//!                 ) ;
//!
//!
//! function       → "fn" ident "()" block_expr ;

use miette::{LabeledSpan, Result, SourceOffset, SourceSpan};
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Block, Parser, Path};

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    pub ident: Option<Token<Ident<'a>>>,
    pub items: Box<[Token<Item<'a>>]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item<'a> {
    pub vis: Option<Token<Visibility>>,
    pub r#type: ItemType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemType<'a> {
    Module(Module<'a>),
    Function(Function<'a>),
    Use(Use<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use<'a> {
    pub path: Token<Path<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    pub ident: Token<Ident<'a>>,
    pub block: Token<Block<'a>>,
}

impl<'a> Parser<'a> {
    pub fn mod_file(&mut self) -> Result<Token<Module<'a>>> {
        let mut items = Vec::new();

        loop {
            if self.first_token().is_none() {
                break;
            }

            let item = self.item()?;
            items.push(item);
        }

        Ok(Token {
            src: match (items.first(), items.last()) {
                (Some(first), Some(last)) => combine_src(first.src, last.src),
                _ => SourceSpan::new(SourceOffset::from(0), 0),
            },
            kind: Module {
                ident: None,
                items: items.into_boxed_slice(),
            },
        })
    }

    pub fn mod_block(&mut self) -> Result<Token<Module<'a>>> {
        todo!()
    }

    pub(crate) fn item(&mut self) -> Result<Token<Item<'a>>> {
        let visibility = match self.first_token_kind() {
            Some(TokenKind::Pub) => {
                let token = self.expect_token(TokenKind::Pub)?;
                Some(token.map(|_| Visibility::Pub))
            }
            _ => None,
        };

        let item_type = match self.first_token_kind() {
            Some(TokenKind::Function) => self.function()?.map(ItemType::Function),
            Some(TokenKind::Mod) => self.mod_block()?.map(ItemType::Module),
            Some(TokenKind::Use) => self.use_decl()?.map(ItemType::Use),
            _ => match self.first_token() {
                Some(got) => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(got.src, "here")],
                        "expected an item ('fn', 'mod', 'use', ..), got {:?}",
                        got.kind
                    )
                    .with_source_code(self.source.to_owned()));
                }
                None => {
                    return Err(miette::miette!(
                        "expected an item ('fn', 'mod', 'use', ..), got EOF"
                    )
                    .with_source_code(self.source.to_owned()));
                }
            },
        };

        Ok(Token {
            src: match &visibility {
                Some(v) => combine_src(v.src, item_type.src),
                None => item_type.src,
            },
            kind: Item {
                vis: visibility,
                r#type: item_type.kind,
            },
        })
    }

    fn use_decl(&mut self) -> Result<Token<Use<'a>>> {
        let r#use = self.expect_token(TokenKind::Use)?;
        let path = self.path_expr()?;
        let semi = self.expect_token(TokenKind::Semicolon)?;

        Ok(Token {
            src: combine_src(r#use.src, semi.src),
            kind: Use { path },
        })
    }

    fn function(&mut self) -> Result<Token<Function<'a>>> {
        let r#fn = self.expect_token(TokenKind::Function)?;
        let ident = self.expect_token_ident()?;
        self.expect_token(TokenKind::LeftParen)?;
        self.expect_token(TokenKind::RightParen)?;
        let block = self.block()?;

        Ok(Token {
            src: combine_src(r#fn.src, block.src),
            kind: Function { ident, block },
        })
    }
}

#[cfg(test)]
mod tests {
    use miette::{SourceOffset, SourceSpan};
    use utils::{Ident, Token};

    use crate::{
        Arithmetic, Block, Expression, Function, Item, ItemType, LetStatement, Module, Parser,
        Primitive, Statement, Visibility,
    };

    #[test]
    fn test_item() {
        let test_script = include_str!("../../../../assets/tests/ast-parser/item.dst");
        let mut parser = Parser::new(test_script);

        assert_eq!(
            parser.mod_file().unwrap().kind,
            Module {
                ident: None,
                items: vec![
                    Token {
                        kind: Item {
                            vis: None,
                            r#type: ItemType::Function(Function {
                                ident: Token {
                                    kind: Ident("first"),
                                    src: SourceSpan::new(SourceOffset::from(0), 0),
                                },
                                block: Token {
                                    kind: Block {
                                        stmts: vec![].into_boxed_slice(),
                                        expr: Some(Token {
                                            kind: Expression::Arithmetic(Arithmetic::Primitive(
                                                Primitive::Number(15.0)
                                            )),
                                            src: SourceSpan::new(SourceOffset::from(0), 0),
                                        })
                                    },
                                    src: SourceSpan::new(SourceOffset::from(0), 0),
                                }
                            })
                        },
                        src: SourceSpan::new(SourceOffset::from(0), 0),
                    },
                    Token {
                        kind: Item {
                            vis: Some(Token::kind(Visibility::Pub)),
                            r#type: ItemType::Function(Function {
                                ident: Token {
                                    kind: Ident("second"),
                                    src: SourceSpan::new(SourceOffset::from(0), 0),
                                },
                                block: Token {
                                    kind: Block {
                                        stmts: vec![
                                            Token {
                                                kind: Statement::LetStatement(LetStatement {
                                                    ident: Token {
                                                        kind: Ident("foo"),
                                                        src: SourceSpan::new(
                                                            SourceOffset::from(0),
                                                            0
                                                        )
                                                    },
                                                    expr: Some(Token {
                                                        kind: Expression::Arithmetic(
                                                            Arithmetic::Primitive(
                                                                Primitive::Number(15.0)
                                                            )
                                                        ),
                                                        src: SourceSpan::new(
                                                            SourceOffset::from(0),
                                                            0
                                                        )
                                                    })
                                                }),
                                                src: SourceSpan::new(SourceOffset::from(0), 0),
                                            },
                                            Token {
                                                kind: Statement::LetStatement(LetStatement {
                                                    ident: Token {
                                                        kind: Ident("bar"),
                                                        src: SourceSpan::new(
                                                            SourceOffset::from(0),
                                                            0
                                                        )
                                                    },
                                                    expr: None
                                                }),
                                                src: SourceSpan::new(SourceOffset::from(0), 0),
                                            }
                                        ]
                                        .into_boxed_slice(),
                                        expr: Some(Token {
                                            kind: Expression::Arithmetic(Arithmetic::Ident(Ident(
                                                "foo"
                                            ))),
                                            src: SourceSpan::new(SourceOffset::from(0), 0),
                                        })
                                    },
                                    src: SourceSpan::new(SourceOffset::from(0), 0),
                                }
                            })
                        },
                        src: SourceSpan::new(SourceOffset::from(0), 0),
                    },
                    Token::kind(Item {
                        vis: None,
                        r#type: ItemType::Function(Function {
                            ident: Token::kind(Ident("third")),
                            block: Token::kind(Block {
                                stmts: vec![Token::kind(Statement::Item(Item {
                                    vis: None,
                                    r#type: ItemType::Function(Function {
                                        ident: Token::kind(Ident("fourth")),
                                        block: Token::kind(Block {
                                            stmts: vec![].into_boxed_slice(),
                                            expr: Some(Token::kind(Expression::Arithmetic(
                                                Arithmetic::Primitive(Primitive::Number(3.0))
                                            )))
                                        })
                                    })
                                }))]
                                .into_boxed_slice(),
                                expr: Some(Token::kind(Expression::Arithmetic(
                                    Arithmetic::Primitive(Primitive::Number(5.0))
                                )))
                            })
                        })
                    })
                ]
                .into_boxed_slice()
            }
        );
    }
}
