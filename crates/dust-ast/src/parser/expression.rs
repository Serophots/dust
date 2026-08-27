//! expression     →  arithmetic
//!                 | ident "=" expression
//!                 | expression "()"
//!                 | path
//!                 | todo..
//!             (the block ones)
//!                 | block_expr
//!                 | if_expr
//!                 | loop_expr ;
//!
//! path           → ident ( "::" ident )*  ;
//!
//! if_expr        → "if" expression block_expr
//!                 ("else" (block_expr | if_expr) )? ;
//!
//! loop_expr      → "loop" block_expr ;

use miette::{Result, SourceOffset, SourceSpan};
use utils::{Ident, Token, TokenKind, combine_src};

use crate::{Arithmetic, Block, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Arithmetic(Arithmetic<'a>),
    Assign,
    CallExpr(CallExpression<'a>),
    Path(Path<'a>),
    Block(Box<Block<'a>>),
    IfExpr,
    LoopExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path<'a> {
    cmpts: Vec<Token<Ident<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression<'a> {
    pub expr: Box<Token<Expression<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn expression(&mut self) -> Result<Token<Expression<'a>>> {
        let expr = self.expression_no_call()?;
        println!("passed inner expr {:?}", expr);
        println!("next {:?}", self.first_token_kind());

        match self.first_token_kind() {
            Some(TokenKind::LeftParen) => {
                // Call this expression
                println!("got a call");
                self.expect_token(TokenKind::LeftParen)?;
                let right_paren = self.expect_token(TokenKind::RightParen)?;

                Ok(Token {
                    src: combine_src(expr.src, right_paren.src),
                    kind: Expression::CallExpr(CallExpression {
                        expr: Box::new(expr),
                    }),
                })
            }
            _ => Ok(expr),
        }
    }

    fn expression_no_call(&mut self) -> Result<Token<Expression<'a>>> {
        match self.first_token_kind() {
            Some(TokenKind::If) => return Ok(self.if_expr()?),
            Some(TokenKind::Loop) => return Ok(self.loop_expr()?),
            Some(TokenKind::LeftBrace) => {
                return Ok(self.block()?.map(Box::new).map(Expression::Block));
            }
            Some(TokenKind::Ident(_))
                if matches!(self.second_token_kind(), Some(TokenKind::Equal)) =>
            {
                // Assignment
                return Ok(self.assign_expr()?);
            }
            Some(TokenKind::Ident(_))
                if matches!(self.second_token_kind(), Some(TokenKind::PathSep)) =>
            {
                // Path
                return Ok(self.path_expr()?.map(Expression::Path));
            }

            _ => {}
        };

        // Fallback to arithmetic
        Ok(self.arithmetic()?.map(Expression::Arithmetic))
    }

    pub fn path_expr(&mut self) -> Result<Token<Path<'a>>> {
        let first = self.expect_token_ident()?;
        let mut cmpts = vec![first];

        while let Ok(_sep) = self.expect_token(TokenKind::PathSep) {
            let next = self.expect_token_ident()?;
            cmpts.push(next);
        }

        println!("passed cmpts as path {:?}", cmpts);

        Ok(Token {
            src: match (cmpts.first(), cmpts.last()) {
                (Some(first), Some(last)) => combine_src(first.src, last.src),
                _ => SourceSpan::new(SourceOffset::from(0), 0),
            },
            kind: Path { cmpts },
        })
    }

    fn if_expr(&mut self) -> Result<Token<Expression<'a>>> {
        todo!()
    }

    fn loop_expr(&mut self) -> Result<Token<Expression<'a>>> {
        todo!()
    }

    fn assign_expr(&mut self) -> Result<Token<Expression<'a>>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Arithmetic, Parser, Primitive, parser::expression::Expression};

    #[test]
    fn test_lexer() {
        let test_script = include_str!("../../../../assets/tests/ast-parser/expression.dst");
        let mut parser = Parser::new(test_script);
        let mut expressions = Vec::new();

        while let Ok(token) = parser.expression() {
            expressions.push(token.kind);
        }

        assert_eq!(
            expressions,
            vec![
                Expression::Arithmetic(Arithmetic::Primitive(Primitive::Number(4.0))),
                Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(true))),
                Expression::Arithmetic(Arithmetic::Primitive(Primitive::Bool(false))),
            ]
        );
    }
}
