use miette::{LabeledSpan, Result, SourceSpan};
use utils::{BinaryOp, Ident, Literal, UnaryOp};

/// Arithmetic
#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Arith<'ast> {
    Literal {
        lit: Literal,
        span: SourceSpan,
    },
    Ident(Ident),
    Unary {
        // TODO: Specify the unary operation..? lol
        unary: &'ast Arith<'ast>,
        op: UnaryOp,
        span: SourceSpan,
    },
    Binary {
        lhs: &'ast Arith<'ast>,
        rhs: &'ast Arith<'ast>,
        op: BinaryOp,
        span: SourceSpan,
    },
}

impl<'ast> core::fmt::Debug for Arith<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal { lit, .. } => match lit {
                Literal::Number(num) => f.debug_tuple("Number").field(num).finish(),
                Literal::String(symbol) => f.debug_tuple("Str").field(symbol).finish(),
                Literal::Bool(bool) => f.debug_tuple("Bool").field(bool).finish(),
                Literal::Nil => f.debug_tuple("Nil").finish(),
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
            Arith::Literal { span, .. } => *span,
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
                Arith::Literal { .. } => {}
                Arith::Ident(_) => {}
                Arith::Unary { unary, op, span } => match unary {
                    Arith::Literal { lit, .. } => match op {
                        UnaryOp::Negate => {
                            return Ok(Arith::Literal {
                                lit: Literal::negate(*lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![LabeledSpan::at(
                                            unary.span(),
                                            format!("negate {:?}", lit)
                                        ),],
                                        "cannot negate incompatible primitive"
                                    )
                                })?,
                                span,
                            });
                        }
                        UnaryOp::Not => {
                            return Ok(Arith::Literal {
                                lit: Literal::not(*lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![LabeledSpan::at(
                                            unary.span(),
                                            format!("NOT {:?}", lit)
                                        ),],
                                        "cannot NOT incompatible primitive"
                                    )
                                })?,
                                span,
                            });
                        }
                    },
                    _ => {}
                },
                Arith::Binary { lhs, rhs, op, span } => match (lhs, rhs) {
                    (
                        Arith::Literal {
                            lit: lhs_prim,
                            span: lhs_span,
                        },
                        Arith::Literal {
                            lit: rhs_prim,
                            span: rhs_span,
                        },
                    ) => {
                        match op {
                            // Primitives are cheap to clone
                            BinaryOp::Add => {
                                return Ok(Arith::Literal {
                                    lit: Literal::add(*lhs_prim, *rhs_prim).map_err(|()| {
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
                            BinaryOp::Sub => {
                                return Ok(Arith::Literal {
                                    lit: Literal::sub(*lhs_prim, *rhs_prim).map_err(|()| {
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
                            BinaryOp::Mul => {
                                return Ok(Arith::Literal {
                                    lit: Literal::mul(*lhs_prim, *rhs_prim).map_err(|()| {
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
                            BinaryOp::Div => {
                                return Ok(Arith::Literal {
                                    lit: Literal::div(*lhs_prim, *rhs_prim).map_err(|()| {
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
                            BinaryOp::Equal => {
                                return Ok(Arith::Literal {
                                    lit: Literal::Bool(std::cmp::PartialEq::eq(lhs_prim, rhs_prim)),
                                    span,
                                });
                            }
                            BinaryOp::NotEqual => {
                                return Ok(Arith::Literal {
                                    lit: Literal::Bool(std::cmp::PartialEq::ne(lhs_prim, rhs_prim)),
                                    span,
                                });
                            }
                            BinaryOp::Greater => {
                                return Ok(Arith::Literal {
                                    lit: Literal::Bool(std::cmp::PartialOrd::gt(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOp::GreaterEqual => {
                                return Ok(Arith::Literal {
                                    lit: Literal::Bool(std::cmp::PartialOrd::ge(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOp::Lesser => {
                                return Ok(Arith::Literal {
                                    lit: Literal::Bool(std::cmp::PartialOrd::lt(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOp::LesserEqual => {
                                return Ok(Arith::Literal {
                                    lit: Literal::Bool(std::cmp::PartialOrd::le(
                                        lhs_prim, rhs_prim,
                                    )),
                                    span,
                                });
                            }
                            BinaryOp::And => {
                                return Ok(Arith::Literal {
                                    lit: Literal::logical_and(lhs_prim, rhs_prim).map_err(
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
                            BinaryOp::Or => {
                                return Ok(Arith::Literal {
                                    lit: Literal::logical_or(lhs_prim, rhs_prim).map_err(|()| {
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
                                    })?,
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
