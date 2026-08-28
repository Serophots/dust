//! module         → item* EOF | mod ident "{" item* "}";
//! item           → visibility? (
//!                   module | function
//!                 ) ;
//!
//!
//! function       → "fn" ident "()" block_expr ;

use miette::{LabeledSpan, Result};
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Block, Parser, Path};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub ident: Option<Token<Ident>>,
    pub items: Box<[Token<Item>]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub vis: Option<Token<Visibility>>,
    pub r#type: ItemType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemType {
    Module(Module),
    Function(Function),
    Use(Use),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    pub path: Token<Path>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub ident: Token<Ident>,
    pub block: Token<Block>,
}

impl<'a> Parser<'a> {
    pub fn mod_file(&mut self) -> Result<Module> {
        let mut items = Vec::new();

        loop {
            if self.first_token().is_none() {
                break;
            }

            let item = self.item()?;
            items.push(item);
        }

        Ok(Module {
            ident: None,
            items: items.into_boxed_slice(),
        })
    }

    pub fn mod_block(&mut self) -> Result<Token<Module>> {
        todo!()
    }

    pub(crate) fn item(&mut self) -> Result<Token<Item>> {
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

    fn use_decl(&mut self) -> Result<Token<Use>> {
        let r#use = self.expect_token(TokenKind::Use)?;
        let path = self.path_expr()?;
        let semi = self.expect_token(TokenKind::Semicolon)?;

        Ok(Token {
            src: combine_src(r#use.src, semi.src),
            kind: Use { path },
        })
    }

    fn function(&mut self) -> Result<Token<Function>> {
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
    use dust_ctxt::create_and_enter_global_ctxt;
    use miette::{SourceOffset, SourceSpan};
    use utils::{Ident, Token};

    use crate::{
        Arithmetic, Block, Expression, Function, Item, ItemType, LetStatement, Module, Parser,
        Primitive, Statement, Visibility,
    };

    #[test]
    fn test_item() {
        create_and_enter_global_ctxt(|ctx| {
            let test_script = include_str!("../../../../assets/tests/ast-parser/item.dst");
            let mut parser = Parser::new(test_script, ctx);

            assert_eq!(
                parser.mod_file().unwrap(),
                Module {
                    ident: None,
                    items: vec![
                        Token {
                            kind: Item {
                                vis: None,
                                r#type: ItemType::Function(Function {
                                    ident: Token {
                                        kind: Ident(ctx.symbols.get_or_intern("first")),
                                        src: SourceSpan::new(SourceOffset::from(0), 0),
                                    },
                                    block: Token {
                                        kind: Block {
                                            stmts: vec![].into_boxed_slice(),
                                            expr: Some(Token {
                                                kind: Expression::Arithmetic(
                                                    Arithmetic::Primitive(Primitive::Number(15.0))
                                                ),
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
                                        kind: Ident(ctx.symbols.get_or_intern("second")),
                                        src: SourceSpan::new(SourceOffset::from(0), 0),
                                    },
                                    block: Token {
                                        kind: Block {
                                            stmts: vec![
                                                Token {
                                                    kind: Statement::LetStatement(LetStatement {
                                                        ident: Token {
                                                            kind: Ident(
                                                                ctx.symbols.get_or_intern("foo")
                                                            ),
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
                                                            kind: Ident(
                                                                ctx.symbols.get_or_intern("bar")
                                                            ),
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
                                                kind: Expression::Arithmetic(Arithmetic::Ident(
                                                    Ident(ctx.symbols.get_or_intern("foo"))
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
                        Token::kind(Item {
                            vis: None,
                            r#type: ItemType::Function(Function {
                                ident: Token::kind(Ident(ctx.symbols.get_or_intern("third"))),
                                block: Token::kind(Block {
                                    stmts: vec![Token::kind(Statement::Item(Item {
                                        vis: None,
                                        r#type: ItemType::Function(Function {
                                            ident: Token::kind(Ident(
                                                ctx.symbols.get_or_intern("fourth")
                                            )),
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
        });
    }
}
