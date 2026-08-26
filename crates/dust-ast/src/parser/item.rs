//! module         → item* EOF | mod ident "{" item* "}";
//! item           → visibility? (
//!                   module | function
//!                 ) ;
//!
//!
//! function       → "fn" ident "()" block_expr ;

use miette::{Result, SourceOffset, SourceSpan};
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Block, Parser};

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
    // TODO: Token<..> these?
    Module(Module<'a>),
    Function(Function<'a>),
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
            _ => {
                return Err(miette::miette!("expected an item ('fn', 'mod', ..)",)
                    .with_source_code(self.source.to_owned()));
            }
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
        Primitive, Statement,
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
                                    src: SourceSpan::new(SourceOffset::from(3), 5)
                                },
                                block: Token {
                                    kind: Block {
                                        stmts: vec![].into_boxed_slice(),
                                        expr: Some(Token {
                                            kind: Expression::Arithmetic(Arithmetic::Primitive(
                                                Primitive::Number(15.0)
                                            )),
                                            src: SourceSpan::new(SourceOffset::from(17), 9)
                                        })
                                    },
                                    src: SourceSpan::new(SourceOffset::from(11), 17)
                                }
                            })
                        },
                        src: SourceSpan::new(SourceOffset::from(0), 28)
                    },
                    Token {
                        kind: Item {
                            vis: None,
                            r#type: ItemType::Function(Function {
                                ident: Token {
                                    kind: Ident("second"),
                                    src: SourceSpan::new(SourceOffset::from(33), 6)
                                },
                                block: Token {
                                    kind: Block {
                                        stmts: vec![
                                            Token {
                                                kind: Statement::LetStatement(LetStatement {
                                                    ident: Token {
                                                        kind: Ident("foo"),
                                                        src: SourceSpan::new(
                                                            SourceOffset::from(52),
                                                            3
                                                        )
                                                    },
                                                    expr: Some(Token {
                                                        kind: Expression::Arithmetic(
                                                            Arithmetic::Primitive(
                                                                Primitive::Number(15.0)
                                                            )
                                                        ),
                                                        src: SourceSpan::new(
                                                            SourceOffset::from(58),
                                                            9
                                                        )
                                                    })
                                                }),
                                                src: SourceSpan::new(SourceOffset::from(48), 20)
                                            },
                                            Token {
                                                kind: Statement::LetStatement(LetStatement {
                                                    ident: Token {
                                                        kind: Ident("bar"),
                                                        src: SourceSpan::new(
                                                            SourceOffset::from(77),
                                                            3
                                                        )
                                                    },
                                                    expr: None
                                                }),
                                                src: SourceSpan::new(SourceOffset::from(73), 8)
                                            }
                                        ]
                                        .into_boxed_slice(),
                                        expr: Some(Token {
                                            kind: Expression::Arithmetic(Arithmetic::Ident(Ident(
                                                "foo"
                                            ))),
                                            src: SourceSpan::new(SourceOffset::from(86), 3)
                                        })
                                    },
                                    src: SourceSpan::new(SourceOffset::from(42), 49)
                                }
                            })
                        },
                        src: SourceSpan::new(SourceOffset::from(30), 61)
                    }
                ]
                .into_boxed_slice()
            }
        );
    }
}
