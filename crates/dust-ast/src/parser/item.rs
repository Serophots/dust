//! module         → item* EOF | mod ident "{" item* "}";
//! item           → visibility? (
//!                   module | function
//!                 ) ;
//!
//!
//! function       → "fn" ident "()" block_expr ;

use miette::Result;
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Block, Parser};

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    ident: Option<Token<Ident<'a>>>,
    items: Box<[Item<'a>]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item<'a> {
    vis: Option<Token<Visibility>>,
    r#type: ItemType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemType<'a> {
    Module(Module<'a>),
    Function(Function<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    ident: Token<Ident<'a>>,
    block: Token<Block<'a>>,
}

impl<'a> Parser<'a> {
    pub fn mod_file(&mut self) -> Result<Token<Module<'a>>> {
        todo!()
    }

    pub fn mod_block(&mut self) -> Result<Token<Module<'a>>> {
        todo!()
    }

    fn item(&mut self) -> Result<Token<Item<'a>>> {
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
