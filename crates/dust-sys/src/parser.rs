use std::iter::{Filter, Peekable};

use miette::Result;

use crate::{
    lexer::Lexer,
    token::{Token, TokenKind},
    transpose,
};

mod expression;
mod statement;

pub use expression::*;
pub use statement::*;

/// Evaluates a string of equality/comparison/addition/multiplication
///
///
///  ## Statement
/// ```ignore
/// statement      → exprStmt
///                | printStmt ;
///
/// exprStmt       → expression ";" ;
/// printStmt      → "print" expression ";" ;
/// ```
///
/// ## Expression
/// ```ignore
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;
/// ```
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
        let is_err = transpose(self.lexer.peek()).is_err();

        if is_err {
            if let Err(err) = self.lexer.next().transpose() {
                Err(err)
            } else {
                unreachable!("expected Err")
            }
        } else {
            if let Ok(token) = transpose(self.lexer.peek()) {
                Ok(token)
            } else {
                unreachable!("expected Ok")
            }
        }
    }

    /// Peek the next token in the lexer, ignoring any
    /// errors parsed up by the lexer as None
    pub fn peek_token<F, R>(&mut self, f: F) -> Option<R>
    where
        F: Fn(&Token<TokenKind<'a>>) -> R,
    {
        if let Ok(Some(token)) = transpose(self.lexer.peek()) {
            Some(f(token))
        } else {
            None
        }
    }
}
