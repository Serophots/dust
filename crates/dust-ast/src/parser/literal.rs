use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result};
use utils::{Lit, TokenKind};

use crate::{Literal, parser::Parser};

impl<'ast> Parser<'ast> {
    ///  literal        → NUMBER | STRING | "true" | "false" | "nil" ;
    pub fn expr_literal(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Literal<'ast>> {
        let Some(token) = self.next_token()? else {
            let eof = self.source.chars().count();

            return Err(miette::miette!(
                labels = vec![LabeledSpan::at(eof..=eof, "unexpected eof")],
                "expected another Primary token"
            )
            .with_source_code(self.source.to_owned()));
        };

        match token.kind {
            TokenKind::True => Ok(ctx.arena.alloc(Literal {
                lit: ctx.arena.alloc(Lit::Bool(true)),
                span: token.span,
            })),
            TokenKind::False => Ok(ctx.arena.alloc(Literal {
                lit: ctx.arena.alloc(Lit::Bool(false)),
                span: token.span,
            })),
            TokenKind::Nil => Ok(ctx.arena.alloc(Literal {
                lit: ctx.arena.alloc(Lit::Nil),
                span: token.span,
            })),
            TokenKind::StringLiteral(str) => Ok(ctx.arena.alloc(Literal {
                lit: ctx.arena.alloc(Lit::String(str)),
                span: token.span,
            })),
            TokenKind::NumberLiteral(n) => Ok(ctx.arena.alloc(Literal {
                lit: ctx.arena.alloc(Lit::Number(n)),
                span: token.span,
            })),

            t => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.span,
                    format!("unexpected token '{t:?}'")
                )],
                "expected a literal (string, number, boolean, etc)"
            )
            .with_source_code(self.source.to_owned())),
        }
    }
}
