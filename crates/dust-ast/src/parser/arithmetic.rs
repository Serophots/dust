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
use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result};
use utils::{Ident, TokenKind, combine_src};

use crate::{Arith, BinaryOperation, Primitive, parser::Parser};

impl<'ast> Parser<'ast> {
    pub fn arithmetic(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        self.logic_or(ctx)
    }

    /// Read a string of or's ||
    ///  logic_or       → logic_and ( "||" logic_and )* ;
    fn logic_or(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let mut lhs = self.logic_and(ctx)?;

        loop {
            enum EqualityOperator {
                Or,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = (match self.first_token_kind() {
                Some(TokenKind::Or) => Some(EqualityOperator::Or),
                _ => None,
            }) else {
                break;
            };

            self.lexer.next();
            let rhs = self.logic_and(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Arith::Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        EqualityOperator::Or => BinaryOperation::Or,
                    },
                    span,
                }
                .simplify(&self.source)?,
            );
        }

        Ok(lhs)
    }

    /// Read a string of and's &&
    ///  logic_and      → equality ( "&&" equality )* ;
    fn logic_and(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let mut lhs = self.equality(ctx)?;

        loop {
            enum EqualityOperator {
                And,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = (match self.first_token_kind() {
                Some(TokenKind::And) => Some(EqualityOperator::And),
                _ => None,
            }) else {
                break;
            };

            self.lexer.next();
            let rhs = self.equality(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Arith::Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        EqualityOperator::And => BinaryOperation::And,
                    },
                    span,
                }
                .simplify(&self.source)?,
            );
        }

        Ok(lhs)
    }

    /// Read a string of equalities == / !=
    ///  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let mut lhs = self.comparison(ctx)?;

        loop {
            enum EqualityOperator {
                Equal,
                NotEqual,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = (match self.first_token_kind() {
                Some(TokenKind::EqualEqual) => Some(EqualityOperator::Equal),
                Some(TokenKind::BangEqual) => Some(EqualityOperator::NotEqual),
                _ => None,
            }) else {
                break;
            };

            self.lexer.next();
            let rhs = self.comparison(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Arith::Binary {
                    lhs: lhs,
                    rhs: rhs,
                    op: match operator {
                        EqualityOperator::Equal => BinaryOperation::Equal,
                        EqualityOperator::NotEqual => BinaryOperation::NotEqual,
                    },
                    span,
                }
                .simplify(&self.source)?,
            );
        }

        Ok(lhs)
    }

    /// Read a string of comparisons GT/GE/LT/LE
    ///  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let mut lhs = self.term(ctx)?;

        loop {
            enum ComparisonOperator {
                Greater,
                GreaterEqual,
                Lesser,
                LesserEqual,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = (match self.first_token_kind() {
                Some(TokenKind::Greater) => Some(ComparisonOperator::Greater),
                Some(TokenKind::GreaterEqual) => Some(ComparisonOperator::GreaterEqual),
                Some(TokenKind::Lesser) => Some(ComparisonOperator::Lesser),
                Some(TokenKind::LesserEqual) => Some(ComparisonOperator::LesserEqual),
                _ => None,
            }) else {
                break;
            };

            self.lexer.next();
            let rhs = self.term(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Arith::Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        ComparisonOperator::Greater => BinaryOperation::Greater,
                        ComparisonOperator::GreaterEqual => BinaryOperation::GreaterEqual,
                        ComparisonOperator::Lesser => BinaryOperation::Lesser,
                        ComparisonOperator::LesserEqual => BinaryOperation::LesserEqual,
                    },
                    span,
                }
                .simplify(&self.source)?,
            );
        }

        Ok(lhs)
    }

    /// Read a string of additions/subtractions
    ///  term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let mut lhs = self.factor(ctx)?;

        loop {
            enum TermOperator {
                Add,
                Sub,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = (match self.first_token_kind() {
                Some(TokenKind::Plus) => Some(TermOperator::Add),
                Some(TokenKind::Minus) => Some(TermOperator::Sub),
                _ => None,
            }) else {
                break;
            };

            self.lexer.next();
            let rhs = self.factor(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Arith::Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        TermOperator::Add => BinaryOperation::Add,
                        TermOperator::Sub => BinaryOperation::Sub,
                    },
                    span,
                }
                .simplify(&self.source)?,
            );
        }

        Ok(lhs)
    }

    /// Read a string of multiplications/divisions
    ///  factor         → unary ( ( "/" | "\*" ) unary )\* ;
    fn factor(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let mut lhs = self.unary(ctx)?;

        loop {
            enum FactorOperator {
                Mul,
                Div,
            }

            // If the operator token should be an error then don't greedily gobble it up into the equality
            let Some(operator) = (match self.first_token_kind() {
                Some(TokenKind::Star) => Some(FactorOperator::Mul),
                Some(TokenKind::Slash) => Some(FactorOperator::Div),
                _ => None,
            }) else {
                break;
            };

            self.lexer.next();
            let rhs = self.unary(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Arith::Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        FactorOperator::Mul => BinaryOperation::Mul,
                        FactorOperator::Div => BinaryOperation::Div,
                    },
                    span,
                }
                .simplify(&self.source)?,
            );
        }

        Ok(lhs)
    }

    /// Read a negated unary or a primary
    ///  unary          → ( "!" | "-" ) unary | primary
    fn unary(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let unary = matches!(
            self.first_token_kind(),
            Some(TokenKind::Bang) | Some(TokenKind::Minus)
        );

        if unary {
            let op = self.next_token(|f| f.span)?.unwrap();
            let unary = self.unary(ctx)?;
            let span = combine_src(op, unary.span());

            Ok(ctx
                .arena
                .alloc(Arith::Unary { unary, span }.simplify(&self.source)?))
        } else {
            Ok(self.primary(ctx)?)
        }
    }

    /// Read a terminal token or a grouped expression
    ///  primary        → NUMBER | STRING | "true" | "false" | "nil"
    ///                | "(" arithmetic ")"
    ///                | IDENTIFIER ;
    fn primary(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Arith<'ast>> {
        let Some(token) = self.next_token(|token| token)? else {
            let eof = self.source.chars().count();

            return Err(miette::miette!(
                labels = vec![LabeledSpan::at(eof..=eof, "unexpected eof")],
                "expected another Primary token"
            )
            .with_source_code(self.source.to_owned()));
        };

        match token.kind {
            TokenKind::True => Ok(ctx.arena.alloc(Arith::Primitive {
                prim: Primitive::Bool(true),
                span: token.span,
            })),
            TokenKind::False => Ok(ctx.arena.alloc(Arith::Primitive {
                prim: Primitive::Bool(false),
                span: token.span,
            })),
            TokenKind::Nil => Ok(ctx.arena.alloc(Arith::Primitive {
                prim: Primitive::Nil,
                span: token.span,
            })),
            TokenKind::StringLiteral(str) => Ok(ctx.arena.alloc(Arith::Primitive {
                prim: Primitive::String(str),
                span: token.span,
            })),
            TokenKind::NumberLiteral(n) => Ok(ctx.arena.alloc(Arith::Primitive {
                prim: Primitive::Number(n),
                span: token.span,
            })),
            TokenKind::LeftParen => {
                let equality = self.logic_or(ctx);
                let right_paren = self
                    .next_token(|token| matches!(token.kind, TokenKind::RightParen))?
                    .unwrap_or(false);

                if right_paren {
                    Ok(equality?)
                } else {
                    Err(miette::miette!(
                        labels = vec![LabeledSpan::at(token.span, "unclosed '('")],
                        "expected ')'"
                    )
                    .with_source_code(self.source.to_owned()))
                }
            }
            TokenKind::Ident(str) => Ok(ctx.arena.alloc(Arith::Ident(Ident {
                symbol: str,
                span: token.span,
            }))),

            t => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.span,
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

    use dust_ctxt::{create_and_enter_ast_ctxt, create_and_enter_global_ctxt};

    use crate::parser::Parser;

    #[test]
    fn test_expression() {
        let () = create_and_enter_global_ctxt(|ctx| {
            let () = create_and_enter_ast_ctxt(ctx, |ctx| {
                let parse = |s| Parser::new(s, ctx).arithmetic(ctx).unwrap();

                panic!("TODO");

                // assert_eq!(parse("5"), &Arith::Primitive(Primitive::Number(5.0)));

                // assert_eq!(parse("-5"), &Arith::Primitive(Primitive::Number(-5.0)));

                // assert_eq!(
                //     parse("3 * 5 / 7"),
                //     &Arith::Primitive(Primitive::Number(15.0 / 7.0))
                // );

                // assert_eq!(
                //     parse("-7 * 5 / 7"),
                //     &Arith::Primitive(Primitive::Number(-5.0))
                // );

                // assert_eq!(
                //     parse("-7 + 5 * 7"),
                //     &Arith::Primitive(Primitive::Number(28.0))
                // );

                // assert_eq!(
                //     parse("(-3 + 5) * 5 / 7"),
                //     &Arith::Primitive(Primitive::Number(10.0 / 7.0))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3"),
                //     &Arith::Primitive(Primitive::Number(-5.0))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 < 4"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 > 4"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 <= -5"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 >= -5"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 >= -5 == true"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 >= -5 == false"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 >= -5 != true"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("1 - 2 * 3 >= -5 != false"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("(1 / 2) == (1 / 2)"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_matches!(parse("(0 / 0)"), Arith::Primitive(Primitive::Number(f)) if f.is_nan());

                // assert_eq!(
                //     parse("(0 / 0) == (0 / 0)"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("true && true"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("true && false"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("false && false"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("false || false"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );

                // assert_eq!(
                //     parse("true || false"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("true || true"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("(4/2==2) && true"),
                //     &Arith::Primitive(Primitive::Bool(true))
                // );

                // assert_eq!(
                //     parse("(4/2==2) && (5<3)"),
                //     &Arith::Primitive(Primitive::Bool(false))
                // );
            });
        });
    }
}
