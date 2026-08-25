use std::iter::{Filter, Peekable};

use dust_lexer::Lexer;
use miette::Result;
use utils::{Token, TokenKind, TransposeRef};

mod arithmetic;
mod expression;

pub struct Parser<'a> {
    pub source: &'a str,
    // Warning: Type gymnastics incoming
    lexer: Peekable<Filter<Lexer<'a>, fn(&Result<Token<TokenKind<'_>>>) -> bool>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Parser<'a> {
        fn predicate<'a, 'b>(token: &'a Result<Token<TokenKind<'b>>>) -> bool {
            !matches!(token.as_ref().map(|t| t.kind), Ok(TokenKind::Comment(_)))
        }

        let predicate: fn(&Result<Token<TokenKind<'_>>>) -> bool = predicate;
        let lexer = Lexer::new(source).filter(predicate).peekable();

        Parser { source, lexer }
    }

    /// Consume the next token in the lexer
    pub fn next_token<F, R>(&mut self, f: F) -> Result<Option<R>>
    where
        F: Fn(Token<TokenKind<'a>>) -> R,
    {
        Ok(self.lexer.next().transpose()?.map(f))
    }

    /// Peek the next token in the lexer.
    /// If the lexer yeilds an error for the next token,
    /// this error token is consumed (not peeked) and
    /// returned as an Err()
    pub fn peek_token_or_err<'s>(&'s mut self) -> Result<Option<&'s Token<TokenKind<'s>>>> {
        let is_err = self.lexer.peek().transpose_ref().is_err();

        if is_err {
            if let Err(err) = self.lexer.next().transpose() {
                Err(err)
            } else {
                unreachable!("expected Err")
            }
        } else {
            if let Ok(token) = self.lexer.peek().transpose_ref() {
                Ok(token)
            } else {
                unreachable!("expected Ok")
            }
        }
    }

    /// Peek the next token in the lexer, ignoring any
    /// errors parsed up by the lexer as None
    pub fn peek_token<'s>(&'s mut self) -> Option<&'s Token<TokenKind>> {
        if let Ok(Some(token)) = self.lexer.peek().transpose_ref() {
            Some(token)
        } else {
            None
        }
    }

    /// Peek the next token in the lexer, ignoring any
    /// errors parsed up by the lexer as None
    pub fn peek_token_kind<'s>(&'s mut self) -> Option<&'s TokenKind> {
        self.peek_token().map(|token| &token.kind)
    }
}
