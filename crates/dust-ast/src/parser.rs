use std::iter::Filter;

use dust_lexer::Lexer;
use miette::Result;
use utils::{Token, TokenKind};

mod arithmetic;
mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

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
