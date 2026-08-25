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
use std::ops::{self, Not as _};

use miette::{LabeledSpan, Result};

use crate::{
    combine_src,
    parser::Parser,
    token::{Token, TokenKind},
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Primitive<'a> {
    Number(f64),
    String(&'a str),
    Bool(bool),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Primitive(Primitive<'a>),
    Identifier(&'a str),
    Unary(Box<Token<Expression<'a>>>),
    Binary {
        lhs: Box<Token<Expression<'a>>>,
        rhs: Box<Token<Expression<'a>>>,
        op: BinaryOperation,
    },
}

impl<'a> Expression<'a> {
    /// Try to eagerly simplify the tree where possible;
    /// i.e. a Primitive::Number(a) + Primitive::Number(b)
    /// can be reduced to Primitive::Number(a+b) at
    /// parsing-time.
    ///
    /// The tree is simplified leaves-up so that this function
    /// needn't recurse; it can assume that any leaves dangling
    /// from this expression have been simplified fully already.
    pub fn simplify(self) -> Expression<'a> {
        match self {
            Expression::Primitive(_) => {}
            Expression::Identifier(_) => {}
            Expression::Unary(ref expression) => match &expression.kind {
                Expression::Primitive(primitive) => {
                    // Primitives are cheap to clone
                    return Expression::Primitive(primitive.clone().not());
                }
                _ => {}
            },
            Expression::Binary {
                ref lhs,
                ref rhs,
                op,
            } => match (&lhs.kind, &rhs.kind) {
                (Expression::Primitive(lhs), Expression::Primitive(rhs)) => match op {
                    // Primitives are cheap to clone
                    BinaryOperation::Add => {
                        return Expression::Primitive(ops::Add::add(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Sub => {
                        return Expression::Primitive(ops::Sub::sub(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Mul => {
                        return Expression::Primitive(ops::Mul::mul(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Div => {
                        return Expression::Primitive(ops::Div::div(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Equal => {
                        return Expression::Primitive(Primitive::Bool(std::cmp::PartialEq::eq(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::NotEqual => {
                        return Expression::Primitive(Primitive::Bool(std::cmp::PartialEq::ne(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::Greater => {
                        return Expression::Primitive(Primitive::Bool(std::cmp::PartialOrd::gt(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::GreaterEqual => {
                        return Expression::Primitive(Primitive::Bool(std::cmp::PartialOrd::ge(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::Lesser => {
                        return Expression::Primitive(Primitive::Bool(std::cmp::PartialOrd::lt(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::LesserEqual => {
                        return Expression::Primitive(Primitive::Bool(std::cmp::PartialOrd::le(
                            lhs, rhs,
                        )));
                    }
                },
                _ => {}
            },
        }

        self
    }
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
            enum EqualityOperator {
                Equal,
                NotEqual,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self
                .peek_token(|token| match token.kind {
                    TokenKind::EqualEqual => Some(EqualityOperator::Equal),
                    TokenKind::BangEqual => Some(EqualityOperator::NotEqual),
                    _ => None,
                })
                .flatten()
            else {
                break;
            };

            self.lexer.next();
            let rhs = self.comparison()?;
            let src = combine_src(lhs.src, rhs.src);

            lhs = Token {
                kind: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: match operator {
                        EqualityOperator::Equal => BinaryOperation::Equal,
                        EqualityOperator::NotEqual => BinaryOperation::NotEqual,
                    },
                }
                .simplify(),
                src,
            };
        }

        Ok(lhs)
    }

    /// Read a string of comparisons GT/GE/LT/LE
    ///  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.term()?;

        loop {
            enum ComparisonOperator {
                Greater,
                GreaterEqual,
                Lesser,
                LesserEqual,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self
                .peek_token(|token| match token.kind {
                    TokenKind::Greater => Some(ComparisonOperator::Greater),
                    TokenKind::GreaterEqual => Some(ComparisonOperator::GreaterEqual),
                    TokenKind::Lesser => Some(ComparisonOperator::Lesser),
                    TokenKind::LesserEqual => Some(ComparisonOperator::LesserEqual),
                    _ => None,
                })
                .flatten()
            else {
                break;
            };

            self.lexer.next();
            let rhs = self.term()?;
            let src = combine_src(lhs.src, rhs.src);

            lhs = Token {
                kind: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: match operator {
                        ComparisonOperator::Greater => BinaryOperation::Greater,
                        ComparisonOperator::GreaterEqual => BinaryOperation::GreaterEqual,
                        ComparisonOperator::Lesser => BinaryOperation::Lesser,
                        ComparisonOperator::LesserEqual => BinaryOperation::LesserEqual,
                    },
                }
                .simplify(),
                src,
            };
        }

        Ok(lhs)
    }

    /// Read a string of additions/subtractions
    ///  term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.factor()?;

        loop {
            enum TermOperator {
                Add,
                Sub,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self
                .peek_token(|token| match token.kind {
                    TokenKind::Plus => Some(TermOperator::Add),
                    TokenKind::Minus => Some(TermOperator::Sub),
                    _ => None,
                })
                .flatten()
            else {
                break;
            };

            self.lexer.next();
            let rhs = self.factor()?;
            let src = combine_src(lhs.src, rhs.src);

            lhs = Token {
                kind: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: match operator {
                        TermOperator::Add => BinaryOperation::Add,
                        TermOperator::Sub => BinaryOperation::Sub,
                    },
                }
                .simplify(),
                src,
            };
        }

        Ok(lhs)
    }

    /// Read a string of multiplications/divisions
    ///  factor         → unary ( ( "/" | "\*" ) unary )\* ;
    fn factor(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.unary()?;

        loop {
            enum FactorOperator {
                Mul,
                Div,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self
                .peek_token(|token| match token.kind {
                    TokenKind::Star => Some(FactorOperator::Mul),
                    TokenKind::Slash => Some(FactorOperator::Div),
                    _ => None,
                })
                .flatten()
            else {
                break;
            };

            self.lexer.next();
            let rhs = self.unary()?;
            let src = combine_src(lhs.src, rhs.src);

            lhs = Token {
                kind: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: match operator {
                        FactorOperator::Mul => BinaryOperation::Mul,
                        FactorOperator::Div => BinaryOperation::Div,
                    },
                }
                .simplify(),
                src,
            };
        }

        Ok(lhs)
    }

    /// Read a negated unary or a primary
    ///  unary          → ( "!" | "-" ) unary | primary
    fn unary(&mut self) -> Result<Token<Expression<'a>>> {
        let unary = self
            .peek_token(|token| matches!(token.kind, TokenKind::Bang | TokenKind::Minus))
            .unwrap_or(false);

        if unary {
            let op = self.next_token(|f| f.src).unwrap().unwrap();
            let unary = self.unary()?;
            let src = combine_src(op, unary.src);

            Ok(Token {
                kind: Expression::Unary(Box::new(unary)).simplify(),
                src,
            })
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
            TokenKind::True => Ok(Token::new(
                Expression::Primitive(Primitive::Bool(true)),
                token.src,
            )),
            TokenKind::False => Ok(Token::new(
                Expression::Primitive(Primitive::Bool(false)),
                token.src,
            )),
            TokenKind::Nil => Ok(Token::new(Expression::Primitive(Primitive::Nil), token.src)),
            TokenKind::StringLiteral(str) => Ok(Token::new(
                Expression::Primitive(Primitive::String(str)),
                token.src,
            )),
            TokenKind::NumberLiteral(n) => Ok(Token::new(
                Expression::Primitive(Primitive::Number(n)),
                token.src,
            )),
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

#[cfg(test)]
mod tests {
    use std::assert_matches;

    use crate::parser::{Expression, Parser, Primitive};

    #[test]
    fn test_expression() {
        assert_eq!(
            Parser::new("5").primary().unwrap().kind,
            Expression::Primitive(Primitive::Number(5.0))
        );

        assert_eq!(
            Parser::new("-5").unary().unwrap().kind,
            Expression::Primitive(Primitive::Number(-5.0))
        );

        assert_eq!(
            Parser::new("3 * 5 / 7").factor().unwrap().kind,
            Expression::Primitive(Primitive::Number(15.0 / 7.0))
        );

        assert_eq!(
            Parser::new("-7 * 5 / 7").factor().unwrap().kind,
            Expression::Primitive(Primitive::Number(-5.0))
        );

        assert_eq!(
            Parser::new("-7 + 5 * 7").term().unwrap().kind,
            Expression::Primitive(Primitive::Number(28.0))
        );

        assert_eq!(
            Parser::new("(-3 + 5) * 5 / 7").term().unwrap().kind,
            Expression::Primitive(Primitive::Number(10.0 / 7.0))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3").term().unwrap().kind,
            Expression::Primitive(Primitive::Number(-5.0))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 < 4").comparison().unwrap().kind,
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 > 4").comparison().unwrap().kind,
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 <= -5").comparison().unwrap().kind,
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5").comparison().unwrap().kind,
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 == true")
                .equality()
                .unwrap()
                .kind,
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 == false")
                .equality()
                .unwrap()
                .kind,
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 != true")
                .equality()
                .unwrap()
                .kind,
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            Parser::new("1 - 2 * 3 >= -5 != false")
                .equality()
                .unwrap()
                .kind,
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            Parser::new("(1 / 2) == (1 / 2)").equality().unwrap().kind,
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_matches!(Parser::new("(0 / 0)").equality().unwrap().kind, Expression::Primitive(Primitive::Number(f)) if f.is_nan());

        assert_eq!(
            Parser::new("(0 / 0) == (0 / 0)").equality().unwrap().kind,
            Expression::Primitive(Primitive::Bool(false))
        );
    }
}
