use std::iter::{Filter, Peekable};

use miette::Result;

use crate::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

mod expression;

pub use expression::*;

/// Evaluates a string of equality/comparison/addition/multiplication
///
///
///  ## Statement
/// ```
/// statement      → exprStmt
///                | printStmt ;
///
/// exprStmt       → expression ";" ;
/// printStmt      → "print" expression ";" ;
/// ```
///
/// ## Expression
/// ```
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

    pub fn parse(&mut self) -> Result<Token<Primary<'a>>> {
        self.equality()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Parser, Primary};

    #[test]
    fn test_parser() {
        assert_eq!(
            Parser::new("5").primary().unwrap().kind,
            Primary::Number(5.0)
        );

        assert_eq!(
            Parser::new("-5").unary().unwrap().kind,
            Primary::Number(-5.0)
        );

        assert_eq!(
            Parser::new("3 * 5 / 7").factor().unwrap().kind,
            Primary::Number(15.0 / 7.0)
        );

        assert_eq!(
            Parser::new("-7 * 5 / 7").factor().unwrap().kind,
            Primary::Number(-5.0)
        );

        assert_eq!(
            Parser::new("-7 + 5 * 7").term().unwrap().kind,
            Primary::Number(28.0)
        );

        assert_eq!(
            Parser::new("(-3 + 5) * 5 / 7").term().unwrap().kind,
            Primary::Number(10.0 / 7.0)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 < 4").comparison().unwrap().kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 > 4").comparison().unwrap().kind,
            Primary::Bool(false)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 <= -5").comparison().unwrap().kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5").comparison().unwrap().kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 == true")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 == false")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(false)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 != true")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(false)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 != false")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Parser::new("(1 / 2) == (1 / 2)").equality().unwrap().kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Parser::new("(0 / 0) == (0 / 0)").equality().unwrap().kind,
            Primary::Bool(false)
        );
    }
}
