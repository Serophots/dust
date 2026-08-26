//! expression     →  arithmetic
//!                 | ident "=" expression
//!                 | todo..
//!             (the block ones)
//!                 | block_expr
//!                 | if_expr
//!                 | loop_expr ;
//!
//!
//! if_expr        → "if" expression block_expr
//!                 ("else" (block_expr | if_expr) )? ;
//!
//! loop_expr      → "loop" block_expr ;

use miette::Result;
use utils::{Token, TokenKind};

use crate::{Arithmetic, Block, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Arithmetic(Arithmetic<'a>),
    Assign,
    Block(Box<Block<'a>>),
    IfExpr,
    LoopExpr,
}

impl<'a> Parser<'a> {
    pub fn expression(&mut self) -> Result<Token<Expression<'a>>> {
        match self.first_token_kind() {
            Some(TokenKind::If) => self.if_expr(),
            Some(TokenKind::Loop) => self.loop_expr(),
            Some(TokenKind::LeftBrace) => Ok(self.block()?.map(Box::new).map(Expression::Block)),

            Some(TokenKind::Ident(_))
                if matches!(self.second_token_kind(), Some(TokenKind::Equal)) =>
            {
                // Assignment
                todo!()
            }
            _ => Ok(self.arithmetic()?.map(Expression::Arithmetic)),
        }
    }

    fn if_expr(&mut self) -> Result<Token<Expression<'a>>> {
        todo!()
    }

    fn loop_expr(&mut self) -> Result<Token<Expression<'a>>> {
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
