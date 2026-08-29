use std::iter::Filter;

use dust_ctxt::AstCtx;
use dust_lexer::Lexer;
use miette::{LabeledSpan, Result, SourceSpan};
use utils::{Ident, Token, TokenKind};

mod arithmetic;
mod expression;
mod item;
mod statement;

pub use expression::*;
pub use item::*;
pub use statement::*;

#[derive(Clone)]
pub struct Parser<'ast> {
    pub source: &'ast str,
    lexer: Filter<Lexer<'ast>, fn(&Result<Token<TokenKind>>) -> bool>,
}

impl<'ast> Parser<'ast> {
    pub fn new(source: &'ast str, ctx: AstCtx<'ast, 'ast>) -> Parser<'ast> {
        fn predicate<'a, 'b>(token: &'a Result<Token<TokenKind>>) -> bool {
            !matches!(token.as_ref().map(|t| t.kind), Ok(TokenKind::Comment))
        }

        let predicate: fn(&Result<Token<TokenKind>>) -> bool = predicate;
        let lexer = Lexer::new(source, ctx).filter(predicate);

        Parser { source, lexer }
    }

    /// Consume the next token in the lexer
    pub fn next_token<F, R>(&mut self, f: F) -> Result<Option<R>>
    // TODO: Is this closure ever used to do anything interesting? I suspect not
    where
        F: Fn(Token<TokenKind>) -> R,
    {
        Ok(self.lexer.next().transpose()?.map(f))
    }

    /// Consume the next token, erroring otherwise
    pub fn expect_token(&mut self, exp_kind: TokenKind) -> Result<Token<TokenKind>> {
        let token = self.first_token();

        match token {
            Some(Token { kind, .. }) if kind == exp_kind => {
                self.next_token(|t| t).unwrap();

                Ok(token.unwrap())
            }
            Some(Token { span: src, .. }) => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    src,
                    format!("expected token '{:?}' following", exp_kind)
                )],
                "expected token '{:?}'",
                exp_kind
            )
            .with_source_code(self.source.to_owned())),
            None => Err(miette::miette!("expected token '{:?}'", exp_kind)
                .with_source_code(self.source.to_owned())),
        }
    }

    /// Consume the next token, erroring otherwise
    pub fn expect_token_matches<F>(
        &mut self,
        f: F,
    ) -> Result<std::result::Result<Token<TokenKind>, Option<SourceSpan>>>
    where
        F: Fn(TokenKind) -> bool,
    {
        let token = self.next_token(|t| t)?;

        Ok(match token {
            Some(Token { kind, .. }) if f(kind) => Ok(token.unwrap()),
            Some(Token { span: src, .. }) => Err(Some(src)),
            None => Err(None),
        })
    }

    pub fn expect_token_ident(&mut self) -> Result<Ident> {
        let token = self.next_token(|t| t)?;

        match token {
            Some(Token {
                kind: TokenKind::Ident(symbol),
                span,
            }) => Ok(Ident { symbol, span }),
            Some(Token { span: src, .. }) => Err(miette::miette!(
                labels = vec![LabeledSpan::at(src, "expected identifier following")],
                "expected identifier",
            )
            .with_source_code(self.source.to_owned())),
            None => {
                Err(miette::miette!("expected identifier",)
                    .with_source_code(self.source.to_owned()))
            }
        }
    }

    /// Peek the first token in the lexer
    pub fn first_token<'s>(&'s self) -> Option<Token<TokenKind>> {
        let mut lexer = self.lexer.clone();
        lexer.next().map(Result::ok).flatten()
    }

    /// Peek the first token in the lexer
    pub fn first_token_kind<'s>(&'s self) -> Option<TokenKind> {
        self.first_token().map(|t| t.kind)
    }

    /// Peek the second token in the lexer
    pub fn second_token<'s>(&'s self) -> Option<Token<TokenKind>> {
        let mut lexer = self.lexer.clone();
        let _ = lexer.next();
        lexer.next().map(Result::ok).flatten()
    }

    /// Peek the second token in the lexer
    pub fn second_token_kind<'s>(&'s self) -> Option<TokenKind> {
        self.second_token().map(|t| t.kind)
    }

    /// Run the closure in a "sandboxed" closure, which
    /// upon returning None, rolls back its' state.
    /// upon returning Some, retains its' state.
    pub fn try_to_parse<F, T>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&mut Parser<'ast>) -> Option<T>,
    {
        let mut new = self.clone();
        let ret = f(&mut new);

        if ret.is_some() {
            *self = new;
        } else {
            // Discard state
        }

        ret
    }
}
