use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result};
use utils::{BinaryOp, Lit, UnaryOp};

use crate::{Binary, Expr, Literal, Unary};

impl<'ast> Expr<'ast> {
    /// Try to eagerly simplify the tree where possible;
    /// i.e. a Primitive::Number(a) + Primitive::Number(b)
    /// can be reduced to Primitive::Number(a+b) at
    /// parsing-time.
    ///
    /// The tree is simplified leafs-up so that this function
    /// needn't recurse; it can assume that any leafs dangling
    /// from this expression have been simplified fully already.
    pub fn simplify(self, source: &'ast str, ctx: AstCtx<'ast, 'ast>) -> Result<Expr<'ast>> {
        if !cfg!(feature = "no-simplify") {
            match self {
                Expr::Unary(&Unary { expr, op, span }) => match expr {
                    Expr::Literal(literal) => {
                        return Ok(Expr::Literal(ctx.arena.alloc(Literal {
                            span,
                            lit: ctx.arena.alloc(match op {
                                UnaryOp::Negate => Lit::negate(*literal.lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![LabeledSpan::at(
                                            literal.span,
                                            format!("negate (-) {:?}", literal)
                                        ),],
                                        "cannot negate incompatible primitive"
                                    )
                                })?,
                                UnaryOp::Not => Lit::not(*literal.lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![LabeledSpan::at(
                                            literal.span,
                                            format!("not (!) {:?}", literal)
                                        ),],
                                        "cannot NOT incompatible primitive"
                                    )
                                })?,
                            }),
                        })));
                    }
                    _ => {}
                },
                Expr::Binary(&Binary { lhs, rhs, op, span }) => match (lhs, rhs) {
                    (
                        Expr::Literal(Literal {
                            lit: lhs_lit,
                            span: lhs_span,
                        }),
                        Expr::Literal(Literal {
                            lit: rhs_lit,
                            span: rhs_span,
                        }),
                    ) => {
                        let lhs_lit = **lhs_lit;
                        let rhs_lit = **rhs_lit;
                        let lhs_span = *lhs_span;
                        let rhs_span = *rhs_span;

                        return Ok(Expr::Literal(ctx.arena.alloc(Literal {
                            span,
                            lit: ctx.arena.alloc(match op {
                                BinaryOp::Add => Lit::add(lhs_lit, rhs_lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![
                                            LabeledSpan::at(lhs_span, format!("lhs {:?}", lhs_lit)),
                                            LabeledSpan::at(rhs_span, format!("rhs {:?}", rhs_lit))
                                        ],
                                        "cannot add incompatible primitives"
                                    )
                                    .with_source_code(source.to_owned())
                                })?,
                                BinaryOp::Sub => Lit::sub(lhs_lit, rhs_lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![
                                            LabeledSpan::at(lhs_span, format!("lhs {:?}", lhs_lit)),
                                            LabeledSpan::at(rhs_span, format!("rhs {:?}", rhs_lit))
                                        ],
                                        "cannot sub incompatible primitives"
                                    )
                                    .with_source_code(source.to_owned())
                                })?,
                                BinaryOp::Mul => Lit::mul(lhs_lit, rhs_lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![
                                            LabeledSpan::at(lhs_span, format!("lhs {:?}", lhs_lit)),
                                            LabeledSpan::at(rhs_span, format!("rhs {:?}", rhs_lit))
                                        ],
                                        "cannot mul incompatible primitives"
                                    )
                                    .with_source_code(source.to_owned())
                                })?,
                                BinaryOp::Div => Lit::div(lhs_lit, rhs_lit).map_err(|()| {
                                    miette::miette!(
                                        labels = vec![
                                            LabeledSpan::at(lhs_span, format!("lhs {:?}", lhs_lit)),
                                            LabeledSpan::at(rhs_span, format!("rhs {:?}", rhs_lit))
                                        ],
                                        "cannot div incompatible primitives"
                                    )
                                    .with_source_code(source.to_owned())
                                })?,
                                BinaryOp::Equal => {
                                    Lit::Bool(std::cmp::PartialEq::eq(&lhs_lit, &rhs_lit))
                                }
                                BinaryOp::NotEqual => {
                                    Lit::Bool(std::cmp::PartialEq::ne(&lhs_lit, &rhs_lit))
                                }
                                BinaryOp::Greater => {
                                    Lit::Bool(std::cmp::PartialOrd::gt(&lhs_lit, &rhs_lit))
                                }
                                BinaryOp::GreaterEqual => {
                                    Lit::Bool(std::cmp::PartialOrd::ge(&lhs_lit, &rhs_lit))
                                }
                                BinaryOp::Lesser => {
                                    Lit::Bool(std::cmp::PartialOrd::lt(&lhs_lit, &rhs_lit))
                                }
                                BinaryOp::LesserEqual => {
                                    Lit::Bool(std::cmp::PartialOrd::le(&lhs_lit, &rhs_lit))
                                }
                                BinaryOp::And => {
                                    Lit::logical_and(lhs_lit, rhs_lit).map_err(|()| {
                                        miette::miette!(
                                            labels = vec![
                                                LabeledSpan::at(
                                                    lhs_span,
                                                    format!("lhs {:?}", lhs_lit)
                                                ),
                                                LabeledSpan::at(
                                                    rhs_span,
                                                    format!("rhs {:?}", rhs_lit)
                                                )
                                            ],
                                            "cannot and incompatible primitives"
                                        )
                                        .with_source_code(source.to_owned())
                                    })?
                                }
                                BinaryOp::Or => {
                                    Lit::logical_or(lhs_lit, rhs_lit).map_err(|()| {
                                        miette::miette!(
                                            labels = vec![
                                                LabeledSpan::at(
                                                    lhs_span,
                                                    format!("lhs {:?}", lhs_lit)
                                                ),
                                                LabeledSpan::at(
                                                    rhs_span,
                                                    format!("rhs {:?}", rhs_lit)
                                                )
                                            ],
                                            "cannot or incompatible primitives"
                                        )
                                        .with_source_code(source.to_owned())
                                    })?
                                }
                            }),
                        })));
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        Ok(self)
    }
}
