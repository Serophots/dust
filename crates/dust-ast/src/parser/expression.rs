use dust_ctxt::AstCtx;
use miette::{LabeledSpan, Result, SourceSpan};
use utils::{BinaryOp, Box, Ident, Lit, TokenKind, UnaryOp, combine_src};

use crate::{Block, Parser};

/// Expression
#[derive(Copy, Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub enum Expr<'ast> {
    Call(&'ast Call<'ast>),
    Binary(&'ast Binary<'ast>),
    Unary(&'ast Unary<'ast>),
    Path(&'ast Path<'ast>),
    Literal(&'ast Literal<'ast>),
    Assign,
    Block(&'ast Block<'ast>),
    If,
    Loop,
}

impl<'ast> core::fmt::Debug for Expr<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Call(arg0) => arg0.fmt(f),
            Self::Binary(arg0) => arg0.fmt(f),
            Self::Unary(arg0) => arg0.fmt(f),
            Self::Literal(arg0) => arg0.fmt(f),
            Self::Path(arg0) => arg0.fmt(f),
            Self::Block(arg0) => arg0.fmt(f),
            Self::Assign => todo!(),
            Self::If => todo!(),
            Self::Loop => todo!(),
        }
    }
}

impl<'ast> Expr<'ast> {
    pub fn span(self) -> SourceSpan {
        match self {
            Expr::Assign => todo!(),
            Expr::Call(call_expr) => call_expr.span,
            Expr::Path(path) => path.span,
            Expr::Block(block) => block.span,
            Expr::If => todo!(),
            Expr::Loop => todo!(),
            Expr::Binary(binary) => binary.span,
            Expr::Unary(unary) => unary.span,
            Expr::Literal(literal) => literal.span,
        }
    }
}

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Literal<'ast> {
    pub lit: &'ast Lit,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Literal<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.lit.fmt(f)
    }
}

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Binary<'ast> {
    pub lhs: &'ast Expr<'ast>,
    pub rhs: &'ast Expr<'ast>,
    pub op: BinaryOp,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Binary<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Binary")
            .field("op", &self.op)
            .field("lhs", self.lhs)
            .field("rhs", self.rhs)
            .finish()
    }
}

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Unary<'ast> {
    pub expr: &'ast Expr<'ast>,
    pub op: UnaryOp,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Unary<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Unary")
            .field("op", &self.op)
            .field("expr", self.expr)
            .finish()
    }
}

#[derive(PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Path<'ast> {
    #[serde(with = "utils::boxed_slice_serialize_with")]
    pub cmpts: Box<'ast, [Ident]>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Path<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Path").field(&self.cmpts).finish()
    }
}

#[derive(Clone, PartialEq, serde::Serialize, derive_generic_visitor::Drive)]
pub struct Call<'ast> {
    pub expr: &'ast Expr<'ast>,
    pub span: SourceSpan,
}

impl<'ast> core::fmt::Debug for Call<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CallExpr").field(&self.expr).finish()
    }
}

impl<'ast> Parser<'ast> {
    pub fn expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        self.expr_call(ctx)
    }

    /// Parse atleast one ident, followed by zero or more further (`::` ident)
    pub fn path_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Path<'ast>> {
        let first = self.expect_token_ident()?;

        let mut cmpts = Vec::new_in(ctx.arena);
        cmpts.push(first);

        while let Some(TokenKind::PathSep) = self.first_token_kind() {
            let _sep = self.expect_token(TokenKind::PathSep)?;
            let next = self.expect_token_ident()?;
            cmpts.push(next);
        }

        Ok(ctx.arena.alloc(Path {
            span: match (cmpts.first().unwrap(), cmpts.last()) {
                (first, Some(last)) => combine_src(first.span, last.span),
                (first, None) => first.span,
            },
            cmpts: cmpts.into_boxed_slice(),
        }))
    }

    fn if_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        todo!()
    }

    fn loop_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        todo!()
    }

    fn assign_expr(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        todo!()
    }

    /// logic_or "()"
    fn expr_call(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let expr = self.expr_or(ctx)?;

        match self.first_token_kind() {
            Some(TokenKind::LeftParen) => {
                // Call this expression
                self.expect_token(TokenKind::LeftParen)?;
                let right_paren = self.expect_token(TokenKind::RightParen)?;

                Ok(ctx.arena.alloc(Expr::Call(ctx.arena.alloc(Call {
                    span: combine_src(expr.span(), right_paren.span),
                    expr,
                }))))
            }
            _ => Ok(expr),
        }
    }

    ///  logic_or       → logic_and ( "||" logic_and )* ;
    fn expr_or(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let mut lhs = self.expr_and(ctx)?;

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
            let rhs = self.expr_and(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Expr::Binary(ctx.arena.alloc(Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        EqualityOperator::Or => BinaryOp::Or,
                    },
                    span,
                }))
                .simplify(&self.source, ctx)?,
            );
        }

        Ok(lhs)
    }

    ///  logic_and      → equality ( "&&" equality )* ;
    fn expr_and(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let mut lhs = self.expr_eq(ctx)?;

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
            let rhs = self.expr_eq(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Expr::Binary(ctx.arena.alloc(Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        EqualityOperator::And => BinaryOp::And,
                    },
                    span,
                }))
                .simplify(&self.source, ctx)?,
            );
        }

        Ok(lhs)
    }

    ///  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn expr_eq(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let mut lhs = self.expr_compar(ctx)?;

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
            let rhs = self.expr_compar(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Expr::Binary(ctx.arena.alloc(Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        EqualityOperator::Equal => BinaryOp::Equal,
                        EqualityOperator::NotEqual => BinaryOp::NotEqual,
                    },
                    span,
                }))
                .simplify(&self.source, ctx)?,
            );
        }

        Ok(lhs)
    }

    ///  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn expr_compar(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let mut lhs = self.expr_term(ctx)?;

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
            let rhs = self.expr_term(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Expr::Binary(ctx.arena.alloc(Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        ComparisonOperator::Greater => BinaryOp::Greater,
                        ComparisonOperator::GreaterEqual => BinaryOp::GreaterEqual,
                        ComparisonOperator::Lesser => BinaryOp::Lesser,
                        ComparisonOperator::LesserEqual => BinaryOp::LesserEqual,
                    },
                    span,
                }))
                .simplify(&self.source, ctx)?,
            );
        }

        Ok(lhs)
    }

    ///  term           → factor ( ( "-" | "+" ) factor )* ;
    fn expr_term(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let mut lhs = self.expr_factor(ctx)?;

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
            let rhs = self.expr_factor(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Expr::Binary(ctx.arena.alloc(Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        TermOperator::Add => BinaryOp::Add,
                        TermOperator::Sub => BinaryOp::Sub,
                    },
                    span,
                }))
                .simplify(&self.source, ctx)?,
            );
        }

        Ok(lhs)
    }

    ///  factor         → unary ( ( "/" | "*" ) unary )* ;
    fn expr_factor(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let mut lhs = self.expr_unary(ctx)?;

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
            let rhs = self.expr_unary(ctx)?;
            let span = combine_src(lhs.span(), rhs.span());

            lhs = ctx.arena.alloc(
                Expr::Binary(ctx.arena.alloc(Binary {
                    lhs,
                    rhs,
                    op: match operator {
                        FactorOperator::Mul => BinaryOp::Mul,
                        FactorOperator::Div => BinaryOp::Div,
                    },
                    span,
                }))
                .simplify(&self.source, ctx)?,
            );
        }

        Ok(lhs)
    }

    ///  unary          → ( "!" | "-" ) unary | parenth
    fn expr_unary(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        let op = match self.first_token_kind() {
            Some(TokenKind::Bang) => UnaryOp::Not,
            Some(TokenKind::Minus) => UnaryOp::Negate,
            _ => return Ok(self.expr_parenth(ctx)?),
        };
        let op_span = self.next_token()?.unwrap().span;

        let expr = self.expr_unary(ctx)?;

        Ok(ctx.arena.alloc(
            Expr::Unary(ctx.arena.alloc(Unary {
                span: combine_src(op_span, expr.span()),
                expr,
                op,
            }))
            .simplify(&self.source, ctx)?,
        ))
    }

    /// "(" expression ")" | primary
    fn expr_parenth(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        if let Ok(left_parenth) = self.expect_token(TokenKind::LeftParen) {
            let expr = self.expr(ctx);
            let right_parenth = self.expect_token(TokenKind::RightParen);

            match right_parenth {
                Ok(_right_parenth) => {
                    // We can't very easily grow the span to include these parenthesis
                    return Ok(expr?);
                }
                Err(_) => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(left_parenth.span, "unclosed '('")],
                        "expected ')'"
                    )
                    .with_source_code(self.source.to_owned()));
                }
            }
        }

        self.expr_primary(ctx)
    }

    /// Path, literal / ident, assign, block, if block, loop block
    fn expr_primary(&mut self, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast Expr<'ast>> {
        match self.first_token_kind() {
            Some(TokenKind::If) => Ok(self.if_expr(ctx)?),
            Some(TokenKind::Loop) => Ok(self.loop_expr(ctx)?),
            Some(TokenKind::LeftBrace) => {
                let block = self.block(ctx)?;
                Ok(ctx.arena.alloc(Expr::Block(block)))
            }
            Some(TokenKind::Ident(_))
                if matches!(self.second_token_kind(), Some(TokenKind::Equal)) =>
            {
                // Assignment
                Ok(self.assign_expr(ctx)?)
            }
            Some(TokenKind::Ident(_)) => {
                // Path
                let path = self.path_expr(ctx)?;

                Ok(ctx.arena.alloc(Expr::Path(path)))
            }

            _ => {
                // Literal
                let lit = self.expr_literal(ctx)?;
                Ok(ctx.arena.alloc(Expr::Literal(lit)))
            }
        }
    }
}
