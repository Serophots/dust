use std::ops::{self, Not as _};

use utils::Token;

use crate::{BinaryOperation, Primitive};

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
                    BinaryOperation::And => {
                        return Expression::Primitive(Primitive::Bool(Primitive::logical_and(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::Or => {
                        return Expression::Primitive(Primitive::Bool(Primitive::logical_or(
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
