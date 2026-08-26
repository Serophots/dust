use std::ops::{self, Not as _};

use utils::{Ident, Token};

use crate::{BinaryOperation, Primitive};

#[derive(Debug, Clone, PartialEq)]
pub enum Arithmetic<'a> {
    Primitive(Primitive<'a>),
    Ident(Ident<'a>),
    Unary(Box<Token<Arithmetic<'a>>>),
    Binary {
        lhs: Box<Token<Arithmetic<'a>>>,
        rhs: Box<Token<Arithmetic<'a>>>,
        op: BinaryOperation,
    },
}

impl<'a> Arithmetic<'a> {
    /// Try to eagerly simplify the tree where possible;
    /// i.e. a Primitive::Number(a) + Primitive::Number(b)
    /// can be reduced to Primitive::Number(a+b) at
    /// parsing-time.
    ///
    /// The tree is simplified leaves-up so that this function
    /// needn't recurse; it can assume that any leaves dangling
    /// from this expression have been simplified fully already.
    pub fn simplify(self) -> Arithmetic<'a> {
        match self {
            Arithmetic::Primitive(_) => {}
            Arithmetic::Ident(_) => {}
            Arithmetic::Unary(ref expression) => match &expression.kind {
                Arithmetic::Primitive(primitive) => {
                    // Primitives are cheap to clone
                    return Arithmetic::Primitive(primitive.clone().not());
                }
                _ => {}
            },
            Arithmetic::Binary {
                ref lhs,
                ref rhs,
                op,
            } => match (&lhs.kind, &rhs.kind) {
                (Arithmetic::Primitive(lhs), Arithmetic::Primitive(rhs)) => match op {
                    // Primitives are cheap to clone
                    BinaryOperation::Add => {
                        return Arithmetic::Primitive(ops::Add::add(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Sub => {
                        return Arithmetic::Primitive(ops::Sub::sub(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Mul => {
                        return Arithmetic::Primitive(ops::Mul::mul(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Div => {
                        return Arithmetic::Primitive(ops::Div::div(lhs.clone(), rhs.clone()));
                    }
                    BinaryOperation::Equal => {
                        return Arithmetic::Primitive(Primitive::Bool(std::cmp::PartialEq::eq(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::NotEqual => {
                        return Arithmetic::Primitive(Primitive::Bool(std::cmp::PartialEq::ne(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::Greater => {
                        return Arithmetic::Primitive(Primitive::Bool(std::cmp::PartialOrd::gt(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::GreaterEqual => {
                        return Arithmetic::Primitive(Primitive::Bool(std::cmp::PartialOrd::ge(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::Lesser => {
                        return Arithmetic::Primitive(Primitive::Bool(std::cmp::PartialOrd::lt(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::LesserEqual => {
                        return Arithmetic::Primitive(Primitive::Bool(std::cmp::PartialOrd::le(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::And => {
                        return Arithmetic::Primitive(Primitive::Bool(Primitive::logical_and(
                            lhs, rhs,
                        )));
                    }
                    BinaryOperation::Or => {
                        return Arithmetic::Primitive(Primitive::Bool(Primitive::logical_or(
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
