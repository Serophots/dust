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

use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result, SourceSpan};
use utils::{Box, Ident, TokenKind, combine_src};

use crate::{Item, Parser, parser::Expr};

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Block<'ast> {
    #[serde(with = "utils::box_serialize_with")]
    pub stmts: Box<'ast, [&'ast Stmt<'ast>]>,
    pub expr: Option<&'ast Expr<'ast>>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Block<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("expr", &self.expr)
            .field("stmts", &self.stmts)
            .finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Stmt<'ast> {
    Semicolon,
    Item(&'ast Item<'ast>),
    Let(&'ast Let<'ast>),
    Expr(&'ast Expr<'ast>),
}

impl<'ast> core::fmt::Debug for Stmt<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Semicolon => write!(f, "Semicolon"),
            Self::Item(arg0) => arg0.fmt(f),
            Self::Let(arg0) => arg0.fmt(f),
            Self::Expr(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Let<'ast> {
    pub ident: Ident,
    pub expr: Option<&'ast Expr<'ast>>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Let<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LetStatement")
            .field("ident", &self.ident)
            .field("expr", &self.expr)
            .finish()
    }
}

impl<'ast> Parser<'ast> {
    pub(crate) fn block(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Block<'ast>> {
        let left_brace = self.expect_token(TokenKind::LeftBrace)?;

        let mut stmts = Vec::new_in(ctx.arena);
        while let Some(statement) = self.statement(ctx)? {
            stmts.push(statement);
        }

        let expr = if !matches!(self.first_token_kind(), Some(TokenKind::RightBrace)) {
            Some(self.expression(ctx)?)
        } else {
            None
        };

        let right_brace = self.expect_token(TokenKind::RightBrace)?;

        Ok(ctx.arena.alloc(Block {
            span: combine_src(left_brace.span, right_brace.span),
            stmts: stmts.into_boxed_slice(),
            expr,
        }))
    }

    fn statement(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<Option<&'ast Stmt<'ast>>> {
        loop {
            // First, try to pass an item
            if let Some(item) = self.try_to_parse(|parser| {
                // formating
                parser.item(ctx).ok()
            }) {
                return Ok(Some(ctx.arena.alloc(Stmt::Item(item))));
            }

            match self.first_token_kind() {
                Some(TokenKind::Semicolon) => {
                    self.expect_token(TokenKind::Semicolon)?;
                }
                Some(TokenKind::Let) => {
                    return Ok(Some(self.let_stmt(ctx)?));
                }
                Some(_) => {
                    // Expression

                    if let Some((expr, _semi)) = self.try_to_parse(|parser| {
                        let expr = parser.expression(ctx).ok()?;
                        let semi = parser.expect_token(TokenKind::Semicolon).ok()?;
                        Some((expr, semi))
                    }) {
                        return Ok(Some(ctx.arena.alloc(Stmt::Expr(expr))));
                    } else {
                        return Ok(None);
                    };
                }
                None => return Ok(None),
            }
        }
    }

    fn let_stmt(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Stmt<'ast>> {
        let r#let = self.expect_token(TokenKind::Let)?;
        let ident = self.expect_token_ident()?;

        match self.first_token_kind() {
            Some(TokenKind::Equal) => {
                self.expect_token(TokenKind::Equal)?;
                let expr = self.expression(ctx)?;
                let semi = self.expect_token(TokenKind::Semicolon)?;

                Ok(ctx.arena.alloc(Stmt::Let(ctx.arena.alloc(Let {
                    ident,
                    expr: Some(expr),
                    span: combine_src(r#let.span, semi.span),
                }))))
            }
            Some(TokenKind::Semicolon) => {
                let semi = self.expect_token(TokenKind::Semicolon)?;
                Ok(ctx.arena.alloc(Stmt::Let(ctx.arena.alloc(Let {
                    ident,
                    expr: None,
                    span: combine_src(r#let.span, semi.span),
                }))))
            }
            _ => Err(miette::miette!(
                labels = vec![LabeledSpan::at(ident.span, "expected '=' or ';' following")],
                "expected '=' or ';'",
            )
            .with_source_code(self.source.to_owned())),
        }
    }
}

#[cfg(test)]
mod tests {
    use dust_ctxt::{create_and_enter_ast_ctxt, create_and_enter_global_ctxt};

    use crate::Parser;

    #[test]
    fn test_statement() {
        let () = create_and_enter_global_ctxt(|ctx| {
            let () = create_and_enter_ast_ctxt(ctx, |ctx| {
                let test_script = include_str!("../../../../assets/tests/ast-parser/statement.dst");
                let mut parser = Parser::new(test_script, ctx);
                let mut stmts = Vec::new();

                while let Some(token) = parser.statement(ctx).unwrap() {
                    stmts.push(token);
                }

                insta::assert_json_snapshot!(stmts);
            });
        });
    }

    #[test]
    fn test_block() {
        let () = create_and_enter_global_ctxt(|ctx| {
            let () = create_and_enter_ast_ctxt(ctx, |ctx| {
                let test_script = include_str!("../../../../assets/tests/ast-parser/block.dst");
                let mut parser = Parser::new(test_script, ctx);
                let mut blocks = Vec::new();

                while let Ok(token) = parser.block(ctx) {
                    blocks.push(token);
                }

                insta::assert_json_snapshot!(blocks);
            });
        });
    }
}
