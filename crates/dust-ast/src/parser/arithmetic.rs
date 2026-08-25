//! arithmetic     → logic_or
//!
//! logic_or       → logic_and ( "||" logic_and )* ;
//! logic_and      → equality ( "&&" equality )* ;
//!
//! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//!
//! term           → factor ( ( "-" | "+" ) factor )* ;
//! factor         → unary ( ( "/" | "*" ) unary )* ;
//!
//! unary          → ( "!" | "-" ) unary
//!                | primary ;
//! primary        → NUMBER | STRING | "true" | "false" | "nil"
//!                | "(" logic_or ")" ;
//!
use miette::{LabeledSpan, Result};
use utils::{Token, TokenKind, combine_src};

use crate::{BinaryOperation, Expression, Primitive, parser::Parser};

impl<'a> Parser<'a> {
    pub fn arithmetic(&mut self) -> Result<Token<Expression<'a>>> {
        self.logic_or()
    }

    /// Read a string of or's ||
    ///  logic_or       → logic_and ( "||" logic_and )* ;
    fn logic_or(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.logic_and()?;

        loop {
            enum EqualityOperator {
                Or,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self
                .peek_token(|token| match token.kind {
                    TokenKind::Or => Some(EqualityOperator::Or),
                    _ => None,
                })
                .flatten()
            else {
                break;
            };

            self.lexer.next();
            let rhs = self.logic_and()?;
            let src = combine_src(lhs.src, rhs.src);

            lhs = Token {
                kind: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: match operator {
                        EqualityOperator::Or => BinaryOperation::Or,
                    },
                }
                .simplify(),
                src,
            };
        }

        Ok(lhs)
    }

    /// Read a string of and's &&
    ///  logic_and      → equality ( "&&" equality )* ;
    fn logic_and(&mut self) -> Result<Token<Expression<'a>>> {
        let mut lhs = self.equality()?;

        loop {
            enum EqualityOperator {
                And,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = self
                .peek_token(|token| match token.kind {
                    TokenKind::And => Some(EqualityOperator::And),
                    _ => None,
                })
                .flatten()
            else {
                break;
            };

            self.lexer.next();
            let rhs = self.equality()?;
            let src = combine_src(lhs.src, rhs.src);

            lhs = Token {
                kind: Expression::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: match operator {
                        EqualityOperator::And => BinaryOperation::And,
                    },
                }
                .simplify(),
                src,
            };
        }

        Ok(lhs)
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
                let equality = self.logic_or();
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

    use crate::{Expression, Primitive, parser::Parser};

    #[test]
    fn test_expression() {
        let parse = |s| Parser::new(s).arithmetic().unwrap().kind;

        assert_eq!(parse("5"), Expression::Primitive(Primitive::Number(5.0)));

        assert_eq!(parse("-5"), Expression::Primitive(Primitive::Number(-5.0)));

        assert_eq!(
            parse("3 * 5 / 7"),
            Expression::Primitive(Primitive::Number(15.0 / 7.0))
        );

        assert_eq!(
            parse("-7 * 5 / 7"),
            Expression::Primitive(Primitive::Number(-5.0))
        );

        assert_eq!(
            parse("-7 + 5 * 7"),
            Expression::Primitive(Primitive::Number(28.0))
        );

        assert_eq!(
            parse("(-3 + 5) * 5 / 7"),
            Expression::Primitive(Primitive::Number(10.0 / 7.0))
        );

        assert_eq!(
            parse("1 - 2 * 3"),
            Expression::Primitive(Primitive::Number(-5.0))
        );

        assert_eq!(
            parse("1 - 2 * 3 < 4"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("1 - 2 * 3 > 4"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("1 - 2 * 3 <= -5"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("1 - 2 * 3 >= -5"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("1 - 2 * 3 >= -5 == true"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("1 - 2 * 3 >= -5 == false"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("1 - 2 * 3 >= -5 != true"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("1 - 2 * 3 >= -5 != false"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("(1 / 2) == (1 / 2)"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_matches!(parse("(0 / 0)"), Expression::Primitive(Primitive::Number(f)) if f.is_nan());

        assert_eq!(
            parse("(0 / 0) == (0 / 0)"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("true && true"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("true && false"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("false && false"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("false || false"),
            Expression::Primitive(Primitive::Bool(false))
        );

        assert_eq!(
            parse("true || false"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("true || true"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("(4/2==2) && true"),
            Expression::Primitive(Primitive::Bool(true))
        );

        assert_eq!(
            parse("(4/2==2) && (5<3)"),
            Expression::Primitive(Primitive::Bool(false))
        );
    }
}
