use std::iter::Filter;

use dust_lexer::Lexer;
use miette::{LabeledSpan, Result, SourceSpan};
use utils::{Ident, Token, TokenKind};

mod arithmetic;
mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

#[derive(Clone)]
pub struct Parser<'a> {
    pub source: &'a str,
    lexer: Filter<Lexer<'a>, fn(&Result<Token<TokenKind<'_>>>) -> bool>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser<'a> {
        fn predicate<'a, 'b>(token: &'a Result<Token<TokenKind<'b>>>) -> bool {
            !matches!(token.as_ref().map(|t| t.kind), Ok(TokenKind::Comment(_)))
        }

        let predicate: fn(&Result<Token<TokenKind<'_>>>) -> bool = predicate;
        let lexer = Lexer::new(source).filter(predicate);

        Parser { source, lexer }
    }

    /// Consume the next token in the lexer
    pub fn next_token<F, R>(&mut self, f: F) -> Result<Option<R>>
    where
        F: Fn(Token<TokenKind<'a>>) -> R,
    {
        Ok(self.lexer.next().transpose()?.map(f))
    }

    /// Consume the next token, erroring otherwise
    pub fn expect_token(&mut self, exp_kind: TokenKind) -> Result<Token<TokenKind<'a>>> {
        let token = self.next_token(|t| t)?;

        match token {
            Some(Token { kind, .. }) if kind == exp_kind => Ok(token.unwrap()),
            Some(Token { src, .. }) => Err(miette::miette!(
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
    ) -> Result<std::result::Result<Token<TokenKind<'a>>, Option<SourceSpan>>>
    where
        F: Fn(TokenKind) -> bool,
    {
        let token = self.next_token(|t| t)?;

        Ok(match token {
            Some(Token { kind, .. }) if f(kind) => Ok(token.unwrap()),
            Some(Token { src, .. }) => Err(Some(src)),
            None => Err(None),
        })
    }

    pub fn expect_token_ident(&mut self) -> Result<Token<Ident<'a>>> {
        let token = self.next_token(|t| t)?;

        match token {
            Some(Token {
                kind: TokenKind::Ident(ident),
                src,
            }) => Ok(Token { kind: ident, src }),
            Some(Token { src, .. }) => Err(miette::miette!(
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
    pub fn first_token<'s>(&'s self) -> Option<Token<TokenKind<'a>>> {
        let mut lexer = self.lexer.clone();
        lexer.next().map(Result::ok).flatten()
    }

    pub fn first_token_kind<'s>(&'s self) -> Option<TokenKind<'a>> {
        self.first_token().map(|t| t.kind)
    }

    /// Peek the second token in the lexer
    // We don't want this to take &mut self
    // it'd mutually exclude the first_token
    // &mut borrow.
    pub fn second_token<'s>(&'s self) -> Option<Token<TokenKind<'a>>> {
        let mut lexer = self.lexer.clone();
        let _ = lexer.next();
        lexer.next().map(Result::ok).flatten()
    }

    pub fn second_token_kind<'s>(&'s self) -> Option<TokenKind<'a>> {
        self.second_token().map(|t| t.kind)
    }
}
