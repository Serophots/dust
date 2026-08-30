//! expression     →  arithmetic
//!                 | ident "=" expression
//!                 | expression "()"
//!                 | path
//!                 | todo..
//!             (the block ones)
//!                 | block_expr
//!                 | if_expr
//!                 | loop_expr ;
//!
//! path           → ident ( "::" ident )*  ;
//!
//! if_expr        → "if" expression block_expr
//!                 ("else" (block_expr | if_expr) )? ;
//!
//! loop_expr      → "loop" block_expr ;

use dust_ctxt::AstCtx;
use miette::{Result, SourceSpan};
use utils::{Box, Ident, TokenKind, combine_src};

use crate::{Arith, Block, Parser};

/// Expression
#[derive(Copy, Clone, PartialEq, serde::Serialize)]
pub enum Expr<'ast> {
    Arith(&'ast Arith<'ast>),
    Assign,
    CallExpr(&'ast CallExpr<'ast>),
    Path(&'ast Path<'ast>),
    Block(&'ast Block<'ast>),
    IfExpr,
    LoopExpr,
}

impl<'ast> core::fmt::Debug for Expr<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Arith(arg0) => arg0.fmt(f),
            Self::Assign => todo!(),
            Self::CallExpr(arg0) => arg0.fmt(f),
            Self::Path(arg0) => arg0.fmt(f),
            Self::Block(arg0) => arg0.fmt(f),
            Self::IfExpr => todo!(),
            Self::LoopExpr => todo!(),
        }
    }
}

impl<'ast> Expr<'ast> {
    pub fn span(self) -> SourceSpan {
        match self {
            Expr::Arith(arith) => arith.span(),
            Expr::Assign => todo!(),
            Expr::CallExpr(call_expr) => call_expr.span,
            Expr::Path(path) => path.span,
            Expr::Block(block) => block.span,
            Expr::IfExpr => todo!(),
            Expr::LoopExpr => todo!(),
        }
    }
}

#[derive(PartialEq, serde::Serialize)]
pub struct Path<'ast> {
    #[serde(with = "utils::box_serialize_with")]
    pub cmpts: Box<'ast, [Ident]>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Path<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Path").field(&self.cmpts).finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize)]
pub struct CallExpr<'ast> {
    pub expr: &'ast Expr<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for CallExpr<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CallExpr").field(&self.expr).finish()
    }
}

impl<'ast> Parser<'ast> {
    pub fn expression(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let expr = self.expression_no_call(ctx)?;

        match self.first_token_kind() {
            Some(TokenKind::LeftParen) => {
                // Call this expression
                self.expect_token(TokenKind::LeftParen)?;
                let right_paren = self.expect_token(TokenKind::RightParen)?;

                Ok(ctx.arena.alloc(Expr::CallExpr(ctx.arena.alloc(CallExpr {
                    span: combine_src(expr.span(), right_paren.span),
                    expr,
                }))))
            }
            _ => Ok(expr),
        }
    }

    /// See `expression` which calls this, then checks whether
    /// the parsed expression is immediately called.
    fn expression_no_call(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        match self.first_token_kind() {
            Some(TokenKind::If) => return Ok(self.if_expr(ctx)?),
            Some(TokenKind::Loop) => return Ok(self.loop_expr(ctx)?),
            Some(TokenKind::LeftBrace) => {
                let block = self.block(ctx)?;

                return Ok(ctx.arena.alloc(Expr::Block(block)));
            }
            Some(TokenKind::Ident(_))
                if matches!(self.second_token_kind(), Some(TokenKind::Equal)) =>
            {
                // Assignment
                return Ok(self.assign_expr(ctx)?);
            }
            Some(TokenKind::Ident(_))
                if matches!(self.second_token_kind(), Some(TokenKind::PathSep)) =>
            {
                // Path
                let path = self.path_expr(ctx)?;

                return Ok(ctx.arena.alloc(Expr::Path(path)));
            }

            _ => {}
        };

        // Fallback to arithmetic
        {
            let arith = self.arithmetic(ctx)?;

            return Ok(ctx.arena.alloc(Expr::Arith(arith)));
        }
    }

    pub fn path_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Path<'ast>> {
        let first = self.expect_token_ident()?;

        let mut cmpts = Vec::new_in(ctx.arena);
        cmpts.push(first);

        while let Ok(_sep) = self.expect_token(TokenKind::PathSep) {
            let next = self.expect_token_ident()?;
            cmpts.push(next);
        }

        Ok(ctx.arena.alloc(Path {
            span: match (cmpts.first().unwrap(), cmpts.last()) {
                (first, Some(last)) => combine_src(first.span, last.span),
                (first, None) => first.span,
            },
            cmpts: cmpts.into_boxed_slice(),
        }))
    }

    fn if_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        todo!()
    }

    fn loop_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        todo!()
    }

    fn assign_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        todo!()
    }
}
