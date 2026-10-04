//! module         → item* EOF | mod ident "{" item* "}";
//! item           → visibility? (
//!                   module | function
//!                 ) ;
//!
//!
//! function       → "fn" ident "()" block_expr ;

use std::hash::{Hash as _, Hasher};

use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result, SourceOffset, SourceSpan};
use utils::{Box, Ident, NodeId, Symbol, TokenKind, combine_src};

use crate::{Block, Parser, Path};

/// mod ident { items }
/// or
/// the root "module" created for each file
#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Module<'gcx, 'ast> {
    pub ident: Symbol,
    pub ident_span: Option<SourceSpan>,

    #[serde(with = "utils::boxed_slice_serialize_with")]
    pub items: Box<'ast, [&'ast Item<'gcx, 'ast>]>,

    #[serde(skip)]
    pub source: &'gcx str,
    pub span: SourceSpan,
}

impl<'gcx, 'ast> core::fmt::Debug for Module<'gcx, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_hash = {
            let mut hasher = std::hash::DefaultHasher::new();
            self.source.hash(&mut hasher);
            hasher.finish()
        };

        f.debug_struct("Module")
            .field("ident", &self.ident)
            .field("items", &self.items)
            .field("source", &source_hash)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Item<'gcx, 'ast> {
    pub vis: Option<Visibility>,
    pub r#type: ItemType<'gcx, 'ast>,
    pub span: SourceSpan,
}

impl<'gcx, 'ast> core::fmt::Debug for Item<'gcx, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Item")
            .field("vis", &self.vis)
            .field("r#type", &self.r#type)
            .finish()
    }
}

#[derive(Copy, Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum ItemType<'gcx, 'ast> {
    Module(&'ast Module<'gcx, 'ast>),
    Func(&'ast Func<'gcx, 'ast>),
    Use(&'ast Use<'ast>),
}

impl<'gcx, 'ast> core::fmt::Debug for ItemType<'gcx, 'ast> {
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
pub struct Func<'gcx, 'ast> {
    pub ident: Ident,
    pub block: &'ast Block<'gcx, 'ast>,
    pub span: SourceSpan,
    pub id: NodeId,
}

impl<'gcx, 'ast> core::fmt::Debug for Func<'gcx, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("ident", &self.ident)
            .field("block", &self.block)
            .finish()
    }
}

impl<'gcx, 'ast> Parser<'gcx, 'ast>
where
    'gcx: 'ast,
{
    pub fn parse(mut self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast mut Module<'gcx, 'ast>> {
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
            ident: *self.path.last().unwrap(),
            ident_span: None,
            source: self.source,
            items: items.into_boxed_slice(),
        }))
    }

    /// mod ident { ..items }
    /// or
    /// mod ident;
    pub(crate) fn r#mod(&mut self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast Module<'gcx, 'ast>> {
        let r#mod = self.expect_token(TokenKind::Mod)?;
        let ident = self.expect_token_ident()?;

        match self.first_token_kind() {
            Some(TokenKind::LeftBrace) => {
                let _left = self.expect_token(TokenKind::LeftBrace)?;

                let mut items = Vec::new_in(ctx.arena);

                loop {
                    if self.first_token_kind() == Some(TokenKind::RightBrace) {
                        break;
                    }

                    let item = self.item(ctx)?;
                    items.push(item);
                }

                let right = self.expect_token(TokenKind::RightBrace)?;

                Ok(ctx.arena.alloc(Module {
                    source: self.source,
                    span: combine_src(r#mod.span, right.span),
                    ident: ident.symbol,
                    ident_span: Some(ident.span),
                    items: items.into_boxed_slice(),
                }))
            }
            Some(TokenKind::Semicolon) => {
                let _semi = self.expect_token(TokenKind::Semicolon)?;

                // We need to load this module from a file
                let mut path = self.path.clone();
                path.push(ident.symbol);

                let module: &'ast Module<'gcx, 'ast> = crate::parse_sub_module(&path, ctx)?;
                Ok(module)
            }
            _ => {
                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(combine_src(r#mod.span, ident.span), "mod")],
                    "expected a block \"{{}}\" or \";\""
                )
                .with_source_code(self.source.to_owned()));
            }
        }
    }

    pub(crate) fn item(&mut self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast Item<'gcx, 'ast>> {
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
                let r#mod = self.r#mod(ctx)?;
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

    fn use_decl(&mut self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast Use<'ast>> {
        let r#use = self.expect_token(TokenKind::Use)?;
        let path = self.path_expr(ctx)?;
        let semi = self.expect_token(TokenKind::Semicolon)?;

        Ok(ctx.arena.alloc(Use {
            span: combine_src(r#use.span, semi.span),
            path: path,
        }))
    }

    fn function(&mut self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast Func<'gcx, 'ast>> {
        let r#fn = self.expect_token(TokenKind::Function)?;
        let ident = self.expect_token_ident()?;
        self.expect_token(TokenKind::LeftParen)?;
        self.expect_token(TokenKind::RightParen)?;
        let block = self.block(ctx)?;

        Ok(ctx.arena.alloc(Func {
            span: combine_src(r#fn.span, block.span),
            id: ctx.gcx.node_id.next(),
            ident,
            block,
        }))
    }
}
