use miette::{LabeledSpan, Result};
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
    pub fn simplify(self, source: &'_ str) -> Result<Arithmetic<'a>> {
        match self {
            Arithmetic::Primitive(_) => {}
            Arithmetic::Ident(_) => {}
            Arithmetic::Unary(ref expression) => match &expression.kind {
                Arithmetic::Primitive(prim) => {
                    return Ok(Arithmetic::Primitive(Primitive::not(*prim).map_err(
                        |()| {
                            miette::miette!(
                                labels = vec![LabeledSpan::at(
                                    expression.src,
                                    format!("not {:?}", prim)
                                ),],
                                "cannot negate incompatible primitive"
                            )
                        },
                    )?));
                }
                _ => {}
            },
            Arithmetic::Binary {
                ref lhs,
                ref rhs,
                op,
            } => match (&lhs.kind, &rhs.kind) {
                (Arithmetic::Primitive(lhs_prim), Arithmetic::Primitive(rhs_prim)) => match op {
                    // Primitives are cheap to clone
                    BinaryOperation::Add => {
                        return Ok(Arithmetic::Primitive(
                            Primitive::add(*lhs_prim, *rhs_prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![
                                        LabeledSpan::at(lhs.src, format!("lhs {:?}", lhs_prim)),
                                        LabeledSpan::at(rhs.src, format!("rhs {:?}", rhs_prim))
                                    ],
                                    "cannot add incompatible primitives"
                                )
                                .with_source_code(source.to_owned())
                            })?,
                        ));
                    }
                    BinaryOperation::Sub => {
                        return Ok(Arithmetic::Primitive(
                            Primitive::sub(*lhs_prim, *rhs_prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![
                                        LabeledSpan::at(lhs.src, format!("lhs {:?}", lhs_prim)),
                                        LabeledSpan::at(rhs.src, format!("rhs {:?}", rhs_prim))
                                    ],
                                    "cannot sub incompatible primitives"
                                )
                                .with_source_code(source.to_owned())
                            })?,
                        ));
                    }
                    BinaryOperation::Mul => {
                        return Ok(Arithmetic::Primitive(
                            Primitive::mul(*lhs_prim, *rhs_prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![
                                        LabeledSpan::at(lhs.src, format!("lhs {:?}", lhs_prim)),
                                        LabeledSpan::at(rhs.src, format!("rhs {:?}", rhs_prim))
                                    ],
                                    "cannot mul incompatible primitives"
                                )
                                .with_source_code(source.to_owned())
                            })?,
                        ));
                    }
                    BinaryOperation::Div => {
                        return Ok(Arithmetic::Primitive(
                            Primitive::div(*lhs_prim, *rhs_prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![
                                        LabeledSpan::at(lhs.src, format!("lhs {:?}", lhs_prim)),
                                        LabeledSpan::at(rhs.src, format!("rhs {:?}", rhs_prim))
                                    ],
                                    "cannot div incompatible primitives"
                                )
                                .with_source_code(source.to_owned())
                            })?,
                        ));
                    }
                    BinaryOperation::Equal => {
                        return Ok(Arithmetic::Primitive(Primitive::Bool(
                            std::cmp::PartialEq::eq(lhs_prim, rhs_prim),
                        )));
                    }
                    BinaryOperation::NotEqual => {
                        return Ok(Arithmetic::Primitive(Primitive::Bool(
                            std::cmp::PartialEq::ne(lhs_prim, rhs_prim),
                        )));
                    }
                    BinaryOperation::Greater => {
                        return Ok(Arithmetic::Primitive(Primitive::Bool(
                            std::cmp::PartialOrd::gt(lhs_prim, rhs_prim),
                        )));
                    }
                    BinaryOperation::GreaterEqual => {
                        return Ok(Arithmetic::Primitive(Primitive::Bool(
                            std::cmp::PartialOrd::ge(lhs_prim, rhs_prim),
                        )));
                    }
                    BinaryOperation::Lesser => {
                        return Ok(Arithmetic::Primitive(Primitive::Bool(
                            std::cmp::PartialOrd::lt(lhs_prim, rhs_prim),
                        )));
                    }
                    BinaryOperation::LesserEqual => {
                        return Ok(Arithmetic::Primitive(Primitive::Bool(
                            std::cmp::PartialOrd::le(lhs_prim, rhs_prim),
                        )));
                    }
                    BinaryOperation::And => {
                        return Ok(Arithmetic::Primitive(
                            Primitive::logical_and(lhs_prim, rhs_prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![
                                        LabeledSpan::at(lhs.src, format!("lhs {:?}", lhs_prim)),
                                        LabeledSpan::at(rhs.src, format!("rhs {:?}", rhs_prim))
                                    ],
                                    "cannot and incompatible primitives"
                                )
                                .with_source_code(source.to_owned())
                            })?,
                        ));
                    }
                    BinaryOperation::Or => {
                        return Ok(Arithmetic::Primitive(
                            Primitive::logical_or(lhs_prim, rhs_prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![
                                        LabeledSpan::at(lhs.src, format!("lhs {:?}", lhs_prim)),
                                        LabeledSpan::at(rhs.src, format!("rhs {:?}", rhs_prim))
                                    ],
                                    "cannot or incompatible primitives"
                                )
                                .with_source_code(source.to_owned())
                            })?,
                        ));
                    }
                },
                _ => {}
            },
        }

        Ok(self)
    }
}
