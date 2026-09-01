//! module         → item* EOF | mod ident "{" item* "}";
//! item           → visibility? (
//!                   module | function
//!                 ) ;
//!
//!
//! function       → "fn" ident "()" block_expr ;

use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result, SourceOffset, SourceSpan};
use utils::{Box, Ident, TokenKind, combine_src};

use crate::{Block, Parser, Path};

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Module<'ast> {
    pub ident: Option<Ident>,
    #[serde(with = "utils::box_serialize_with")]
    pub items: Box<'ast, [&'ast Item<'ast>]>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Module<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("ident", &self.ident)
            .field("items", &self.items)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Item<'ast> {
    pub vis: Option<Visibility>,
    pub r#type: ItemType<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Item<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Item")
            .field("vis", &self.vis)
            .field("r#type", &self.r#type)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum ItemType<'ast> {
    Module(&'ast Module<'ast>),
    Func(&'ast Func<'ast>),
    Use(&'ast Use<'ast>),
}

impl<'ast> core::fmt::Debug for ItemType<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Module(arg0) => arg0.fmt(f),
            Self::Func(arg0) => arg0.fmt(f),
            Self::Use(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Visibility {
    pub r#type: VisibilityType,
    pub span: SourceSpan,
}

impl core::fmt::Debug for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Visibility").field(&self.r#type).finish()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum VisibilityType {
    Pub,
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Use<'ast> {
    pub path: &'ast Path<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Use<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Use").field(&self.path).finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Func<'ast> {
    pub ident: Ident,
    pub block: &'ast Block<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Func<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("ident", &self.ident)
            .field("block", &self.block)
            .finish()
    }
}

impl<'ast> Parser<'ast> {
    pub fn mod_file(mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast mut Module<'ast>> {
        let mut items = Vec::new_in(ctx.arena);

        loop {
            if self.first_token().is_none() {
                break;
            }

            let item = self.item(ctx)?;
            items.push(item);
        }

        Ok(ctx.arena.alloc(Module {
            span: match (items.first(), items.last()) {
                (Some(first), Some(last)) => combine_src(first.span, last.span),
                _ => SourceSpan::new(SourceOffset::from(0), 0),
            },
            ident: None,
            items: items.into_boxed_slice(),
        }))
    }

    pub(crate) fn mod_block(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Module<'ast>> {
        todo!()
    }

    pub(crate) fn item(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Item<'ast>> {
        let vis = match self.first_token_kind() {
            Some(TokenKind::Pub) => {
                let token = self.expect_token(TokenKind::Pub)?;

                Some(Visibility {
                    r#type: VisibilityType::Pub,
                    span: token.span,
                })
            }
            _ => None,
        };

        let (item_type, item_span) = match self.first_token_kind() {
            Some(TokenKind::Function) => {
                let function = self.function(ctx)?;
                (ItemType::Func(function), function.span)
            }
            Some(TokenKind::Mod) => {
                let r#mod = self.mod_block(ctx)?;
                (ItemType::Module(r#mod), r#mod.span)
            }
            Some(TokenKind::Use) => {
                let r#use = self.use_decl(ctx)?;
                (ItemType::Use(r#use), r#use.span)
            }
            _ => match self.first_token() {
                Some(got) => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(got.span, "here")],
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

        Ok(ctx.arena.alloc(Item {
            span: match &vis {
                Some(v) => combine_src(v.span, item_span),
                None => item_span,
            },
            vis,
            r#type: item_type,
        }))
    }

    fn use_decl(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Use<'ast>> {
        let r#use = self.expect_token(TokenKind::Use)?;
        let path = self.path_expr(ctx)?;
        let semi = self.expect_token(TokenKind::Semicolon)?;

        Ok(ctx.arena.alloc(Use {
            span: combine_src(r#use.span, semi.span),
            path: path,
        }))
    }

    fn function(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Func<'ast>> {
        let r#fn = self.expect_token(TokenKind::Function)?;
        let ident = self.expect_token_ident()?;
        self.expect_token(TokenKind::LeftParen)?;
        self.expect_token(TokenKind::RightParen)?;
        let block = self.block(ctx)?;

        Ok(ctx.arena.alloc(Func {
            span: combine_src(r#fn.span, block.span),
            ident,
            block,
        }))
    }
}
