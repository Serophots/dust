///! expression     → equality ;
///! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
///! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
///! term           → factor ( ( "-" | "+" ) factor )* ;
///! factor         → unary ( ( "/" | "*" ) unary )* ;
///! unary          → ( "!" | "-" ) unary
///!                | primary ;
///! primary        → NUMBER | STRING | "true" | "false" | "nil"
///!                | "(" expression ")"
///!                | IDENTIFIER ;
use std::ops::Not as _;

use miette::{LabeledSpan, Result};

use crate::{
    parser::Parser,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Expression<'a> {
    Number(f64),
    String(&'a str),
    Bool(bool),
    Nil,
    Identifier(&'a str),
}

impl<'a> Parser<'a> {
    /// Read an expression
    ///  expression     → equality ;
    pub(super) fn expression(&mut self) -> Result<Token<Expression<'a>>> {
        self.equality()
    }

    /// Read a string of equalities == / !=
    ///  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.comparison()?;

        loop {
            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self.peek_token(|token| token.kind) else {
                break;
            };

            if operator == TokenKind::EqualEqual {
                self.lexer.next();
                let rhs = self.comparison()?;

                lhs = lhs.equals(rhs);
            } else if operator == TokenKind::BangEqual {
                self.lexer.next();
                let rhs = self.comparison()?;
                lhs = lhs.not_equals(rhs);
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a string of comparisons GT/GE/LT/LE
    ///  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.term()?;

        loop {
            // If the operator token should be an error then don't greedily gobble it up into the comparison
            let Some(operator) = self.peek_token(|token| token.kind) else {
                break;
            };

            if operator == TokenKind::Greater {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.greater(rhs);
            } else if operator == TokenKind::GreaterEqual {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.greater_equal(rhs);
            } else if operator == TokenKind::Lesser {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.lesser(rhs);
            } else if operator == TokenKind::LesserEqual {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.lesser_equal(rhs);
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a string of additions/subtractions
    ///  term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.factor()?;

        loop {
            // If the operator token should be an error then don't greedily gobble it up into the term
            let Some(operator) = self.peek_token(|token| token.kind) else {
                break;
            };

            if operator == TokenKind::Plus {
                self.lexer.next();
                let rhs = self.factor()?;
                lhs = lhs + rhs;
            } else if operator == TokenKind::Minus {
                self.lexer.next();
                let rhs = self.factor()?;
                lhs = lhs - rhs;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a string of multiplications/divisions
    ///  factor         → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.unary()?;

        loop {
            // If the operator token should be an error then don't greedily gobble it up into the factor
            let Some(operator) = self.peek_token(|token| token.kind) else {
                break;
            };

            if operator == TokenKind::Star {
                self.lexer.next();
                let rhs = self.unary()?;
                lhs = lhs * rhs;
            } else if operator == TokenKind::Slash {
                self.lexer.next();
                let rhs = self.unary()?;
                lhs = lhs / rhs;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a negated unary or a primary
    ///  unary          → ( "!" | "-" ) unary | primary
    fn unary(&mut self) -> Result<Token<Expression<'a>>> {
        let unary =
            self.peek_token(|token| matches!(token.kind, TokenKind::Bang | TokenKind::Minus));

        if unary.unwrap_or(false) {
            self.lexer.next();

            Ok(self.unary()?.not())
        } else {
            Ok(self.primary()?)
        }
    }

    /// Read a terminal token or a grouped expression
    ///  primary        → NUMBER | STRING | "true" | "false" | "nil"
    ///                | "(" expression ")"
    ///                | IDENTIFIER ;
    fn primary(&mut self) -> Result<Token<Expression<'a>>> {
        let Some(token) = self.next_token(|token| token)? else {
            let eof = self.source.chars().count();

            return Err(miette::miette!(
                labels = vec![LabeledSpan::at(eof..=eof, "unexpected eof")],
                "expected another Primary token"
            )
            .with_source_code(self.source.to_owned()));
        };

        match token.kind {
            TokenKind::True => Ok(Token::new(Expression::Bool(true), token.src)),
            TokenKind::False => Ok(Token::new(Expression::Bool(false), token.src)),
            TokenKind::Nil => Ok(Token::new(Expression::Nil, token.src)),
            TokenKind::StringLiteral(str) => Ok(Token::new(Expression::String(str), token.src)),
            TokenKind::NumberLiteral(n) => Ok(Token::new(Expression::Number(n), token.src)),
            TokenKind::LeftParen => {
                let equality = self.equality();
                let right_paren = self
                    .next_token(|token| matches!(token.kind, TokenKind::RightParen))?
                    .unwrap_or(false);

                if right_paren {
                    Ok(equality?)
                } else {
                    Err(miette::miette!(
                        labels = vec![LabeledSpan::at(token.src, "unclosed '('")],
                        "expected ')'"
                    )
                    .with_source_code(self.source.to_owned()))
                }
            }
            TokenKind::Identifier(str) => Ok(Token::new(Expression::Identifier(str), token.src)),

            t => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.src,
                    format!("unexpected token '{t:?}'")
                )],
                "expected Primary token"
            )
            .with_source_code(self.source.to_owned())),
        }
    }
}

// TODO: Nice to see in std lib?

#[cfg(test)]
mod tests {
    use crate::parser::{Expression, Parser};

    #[test]
    fn test_expression() {
        assert_eq!(
            Parser::new("5").primary().unwrap().kind,
            Expression::Number(5.0)
        );

        assert_eq!(
            Parser::new("-5").unary().unwrap().kind,
            Expression::Number(-5.0)
        );

        assert_eq!(
            Parser::new("3 * 5 / 7").factor().unwrap().kind,
            Expression::Number(15.0 / 7.0)
        );

        assert_eq!(
            Parser::new("-7 * 5 / 7").factor().unwrap().kind,
            Expression::Number(-5.0)
        );

        assert_eq!(
            Parser::new("-7 + 5 * 7").term().unwrap().kind,
            Expression::Number(28.0)
        );

        assert_eq!(
            Parser::new("(-3 + 5) * 5 / 7").term().unwrap().kind,
            Expression::Number(10.0 / 7.0)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 < 4").comparison().unwrap().kind,
            Expression::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 > 4").comparison().unwrap().kind,
            Expression::Bool(false)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 <= -5").comparison().unwrap().kind,
            Expression::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5").comparison().unwrap().kind,
            Expression::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 == true")
                .equality()
                .unwrap()
                .kind,
            Expression::Bool(true)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 == false")
                .equality()
                .unwrap()
                .kind,
            Expression::Bool(false)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 != true")
                .equality()
                .unwrap()
                .kind,
            Expression::Bool(false)
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 != false")
                .equality()
                .unwrap()
                .kind,
            Expression::Bool(true)
        );

        assert_eq!(
            Parser::new("(1 / 2) == (1 / 2)").equality().unwrap().kind,
            Expression::Bool(true)
        );

        assert_eq!(
            Parser::new("(0 / 0) == (0 / 0)").equality().unwrap().kind,
            Expression::Bool(false)
        );
    }
}
