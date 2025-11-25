///! expression     → equality ;
///! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
///! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
///! term           → factor ( ( "-" | "+" ) factor )* ;
///! factor         → unary ( ( "/" | "*" ) unary )* ;
///! unary          → ( "!" | "-" ) unary
///!                | primary ;
///! primary        → NUMBER | STRING | "true" | "false" | "nil"
///!                | "(" expression ")" ;
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
            let Some(operator) = self.peek_token()? else {
                break;
            };

            if operator.kind == TokenKind::EqualEqual {
                self.lexer.next();
                let rhs = self.comparison()?;

                lhs = lhs.equals(rhs);
            } else if operator.kind == TokenKind::BangEqual {
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
            let Some(operator) = self.peek_token()? else {
                break;
            };

            if operator.kind == TokenKind::Greater {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.greater(rhs);
            } else if operator.kind == TokenKind::GreaterEqual {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.greater_equal(rhs);
            } else if operator.kind == TokenKind::Lesser {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.lesser(rhs);
            } else if operator.kind == TokenKind::LesserEqual {
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
            let Some(operator) = self.peek_token()? else {
                break;
            };

            if operator.kind == TokenKind::Plus {
                self.lexer.next();
                let rhs = self.factor()?;
                lhs = lhs + rhs;
            } else if operator.kind == TokenKind::Minus {
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
            let Some(operator) = self.peek_token()? else {
                break;
            };

            if operator.kind == TokenKind::Star {
                self.lexer.next();
                let rhs = self.unary()?;
                lhs = lhs * rhs;
            } else if operator.kind == TokenKind::Slash {
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
        let Some(operator) = self.peek_token()? else {
            //TODO: Error in the input; something is wrong
            todo!()
        };

        if matches!(operator.kind, TokenKind::Bang | TokenKind::Minus) {
            self.lexer.next();

            Ok(self.unary()?.not())
        } else {
            Ok(self.primary()?)
        }
    }

    /// Read a terminal token or a grouped expression
    ///  primary        → NUMBER | STRING | "true" | "false" | "nil"
    ///                | "(" expression ")" ;
    fn primary(&mut self) -> Result<Token<Expression<'a>>> {
        let Some(token) = self.next_token()? else {
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
                let group_token = self.equality()?;

                let right_brace = self.next_token()?;

                if let Some(right_brace) = right_brace
                    && matches!(right_brace.kind, TokenKind::RightParen)
                {
                    Ok(group_token)
                } else {
                    Err(miette::miette!(
                        labels = vec![LabeledSpan::at(token.src, "unclosed '('")],
                        "expected ')'"
                    )
                    .with_source_code(self.source.to_owned()))
                }
            }

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
