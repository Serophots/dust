use miette::{LabeledSpan, Result, SourceSpan};
use utils::Ident;

use crate::{BinaryOperation, Primitive};

/// Arithmetic
#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Arith<'ast> {
    Primitive {
        prim: Primitive,
        // #[drive(skip)]
        span: SourceSpan,
    },
    Ident(Ident),
    Unary {
        // TODO: Specify the unary operation..? lol
        // #[drive(skip)]
        unary: &'ast Arith<'ast>,
        // #[drive(skip)]
        span: SourceSpan,
    },
    Binary {
        // #[drive(skip)]
        lhs: &'ast Arith<'ast>,
        // #[drive(skip)]
        rhs: &'ast Arith<'ast>,
        // #[drive(skip)]
        op: BinaryOperation,
        // #[drive(skip)]
        span: SourceSpan,
    },
}

impl<'ast> core::fmt::Debug for Arith<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive { prim, .. } => match prim {
                Primitive::Number(num) => f.debug_tuple("Number").field(num).finish(),
                Primitive::String(symbol) => f.debug_tuple("Str").field(symbol).finish(),
                Primitive::Bool(bool) => f.debug_tuple("Bool").field(bool).finish(),
                Primitive::Nil => f.debug_tuple("Nil").finish(),
            },
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::Unary { unary, .. } => f.debug_struct("Unary").field("Field", unary).finish(),
            Self::Binary { lhs, rhs, op, .. } => f
                .debug_struct("Binary")
                .field("op", op)
                .field("lhs", lhs)
                .field("rhs", rhs)
                .finish(),
        }
    }
}

impl<'ast> Arith<'ast> {
    pub fn span(&self) -> SourceSpan {
        match self {
            Arith::Primitive { span, .. } => *span,
            Arith::Ident(ident) => ident.span,
            Arith::Unary { span, .. } => *span,
            Arith::Binary { span, .. } => *span,
        }
    }
}

impl<'ast> Arith<'ast> {
    /// Try to eagerly simplify the tree where possible;
    /// i.e. a Primitive::Number(a) + Primitive::Number(b)
    /// can be reduced to Primitive::Number(a+b) at
    /// parsing-time.
    ///
    /// The tree is simplified leaves-up so that this function
    /// needn't recurse; it can assume that any leaves dangling
    /// from this expression have been simplified fully already.
    pub fn simplify(self, source: &'ast str) -> Result<Arith<'ast>> {
        if !cfg!(feature = "no-simplify") {
            match self {
                Arith::Primitive { .. } => {}
                Arith::Ident(_) => {}
                Arith::Unary { unary, span } => match unary {
                    Arith::Primitive { prim, .. } => {
                        return Ok(Arith::Primitive {
                            prim: Primitive::not(*prim).map_err(|()| {
                                miette::miette!(
                                    labels = vec![LabeledSpan::at(
                                        unary.span(),
                                        format!("not {:?}", prim)
                                    ),],
                                    "cannot negate incompatible primitive"
                                )
                            })?,
                            span,
                        });
                    }
                    _ => {}
                },
                Arith::Binary { lhs, rhs, op, span } => match (lhs, rhs) {
                    (
                        Arith::Primitive {
                            prim: lhs_prim,
                            span: lhs_span,
                        },
                        Arith::Primitive {
                            prim: rhs_prim,
                            span: rhs_span,
                        },
                    ) => {
                        match op {
                            // Primitives are cheap to clone
                            BinaryOperation::Add => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::add(*lhs_prim, *rhs_prim).map_err(|()| {
                                        miette::miette!(
                                            labels = vec![
                                                LabeledSpan::at(
                                                    *lhs_span,
                                                    format!("lhs {:?}", lhs_prim)
                                                ),
                                                LabeledSpan::at(
                                                    *rhs_span,
                                                    format!("rhs {:?}", rhs_prim)
                                                )
                                            ],
                                            "cannot add incompatible primitives"
                                        )
                                        .with_source_code(source.to_owned())
                                    })?,
                                    span,
                                });
                            }
                            BinaryOperation::Sub => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::sub(*lhs_prim, *rhs_prim).map_err(|()| {
                                        miette::miette!(
                                            labels = vec![
                                                LabeledSpan::at(
                                                    *lhs_span,
                                                    format!("lhs {:?}", lhs_prim)
                                                ),
                                                LabeledSpan::at(
                                                    *rhs_span,
                                                    format!("rhs {:?}", rhs_prim)
                                                )
                                            ],
                                            "cannot sub incompatible primitives"
                                        )
                                        .with_source_code(source.to_owned())
                                    })?,
                                    span,
                                });
                            }
                            BinaryOperation::Mul => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::mul(*lhs_prim, *rhs_prim).map_err(|()| {
                                        miette::miette!(
                                            labels = vec![
                                                LabeledSpan::at(
                                                    *lhs_span,
                                                    format!("lhs {:?}", lhs_prim)
                                                ),
                                                LabeledSpan::at(
                                                    *rhs_span,
                                                    format!("rhs {:?}", rhs_prim)
                                                )
                                            ],
                                            "cannot mul incompatible primitives"
                                        )
                                        .with_source_code(source.to_owned())
                                    })?,
                                    span,
                                });
                            }
                            BinaryOperation::Div => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::div(*lhs_prim, *rhs_prim).map_err(|()| {
                                        miette::miette!(
                                            labels = vec![
                                                LabeledSpan::at(
                                                    *lhs_span,
                                                    format!("lhs {:?}", lhs_prim)
                                                ),
                                                LabeledSpan::at(
                                                    *rhs_span,
                                                    format!("rhs {:?}", rhs_prim)
                                                )
                                            ],
                                            "cannot div incompatible primitives"
                                        )
                                        .with_source_code(source.to_owned())
                                    })?,
                                    span,
                                });
                            }
                            BinaryOperation::Equal => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::Bool(std::cmp::PartialEq::eq(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOperation::NotEqual => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::Bool(std::cmp::PartialEq::ne(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOperation::Greater => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::Bool(std::cmp::PartialOrd::gt(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOperation::GreaterEqual => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::Bool(std::cmp::PartialOrd::ge(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOperation::Lesser => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::Bool(std::cmp::PartialOrd::lt(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOperation::LesserEqual => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::Bool(std::cmp::PartialOrd::le(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOperation::And => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::logical_and(lhs_prim, rhs_prim).map_err(
                                        |()| {
                                            miette::miette!(
                                                labels = vec![
                                                    LabeledSpan::at(
                                                        *lhs_span,
                                                        format!("lhs {:?}", lhs_prim)
                                                    ),
                                                    LabeledSpan::at(
                                                        *rhs_span,
                                                        format!("rhs {:?}", rhs_prim)
                                                    )
                                                ],
                                                "cannot and incompatible primitives"
                                            )
                                            .with_source_code(source.to_owned())
                                        },
                                    )?,
                                    span,
                                });
                            }
                            BinaryOperation::Or => {
                                return Ok(Arith::Primitive {
                                    prim: Primitive::logical_or(lhs_prim, rhs_prim).map_err(
                                        |()| {
                                            miette::miette!(
                                                labels = vec![
                                                    LabeledSpan::at(
                                                        *lhs_span,
                                                        format!("lhs {:?}", lhs_prim)
                                                    ),
                                                    LabeledSpan::at(
                                                        *rhs_span,
                                                        format!("rhs {:?}", rhs_prim)
                                                    )
                                                ],
                                                "cannot or incompatible primitives"
                                            )
                                            .with_source_code(source.to_owned())
                                        },
                                    )?,
                                    span,
                                });
                            }
                        }
                    }
                    _ => {}
                },
            }
        }

        Ok(self)
    }
}
