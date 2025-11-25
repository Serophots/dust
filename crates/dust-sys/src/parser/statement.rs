use miette::{LabeledSpan, Result};

///! program        → statement* EOF ;
///! statement      → exprStmt
///!                | printStmt ;
///!
///! exprStmt       → expression ";" ;
///! printStmt      → "print" expression ";" ;
use crate::{
    parser::{Expression, Parser},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Statement<'a> {
    Expresion(Token<Expression<'a>>),
    Print(Token<Expression<'a>>),
}

impl<'a> Parser<'a> {
    /// Read a statement
    ///  statement      → exprStmt
    ///                | printStmt ;
    fn statement(&mut self) -> Result<Option<Statement<'a>>> {
        let Some(token) = self.peek_token()? else {
            return Ok(None);
        };

        Ok(Some(match token.kind {
            TokenKind::Print => self.print_statement()?,
            _ => self.expression_statement()?,
        }))
    }

    /// Read an expression statement (a statement which evaluates to an expression)
    ///  exprStmt       → expression ";" ;
    fn expression_statement(&mut self) -> Result<Statement<'a>> {
        let expression = self.expression()?;
        let colon = self.next_token()?;

        if let Some(colon) = colon
            && matches!(colon.kind, TokenKind::Semicolon)
        {
            Ok(Statement::Expresion(expression))
        } else {
            Err(miette::miette!(
                labels = vec![LabeledSpan::at(expression.src, "this expression")],
                "unterminated expression statement; expected ';'"
            )
            .with_source_code(self.source.to_owned()))
        }
    }

    /// Read a print statement
    ///  printStmt      → "print" expression ";" ;
    fn print_statement(&mut self) -> Result<Statement<'a>> {
        let print = self.next_token()?;
        let expression = self.expression()?;
        let colon = self.next_token()?;

        if let Some(print) = print
            && matches!(print.kind, TokenKind::Print)
        {
            if let Some(colon) = colon
                && matches!(colon.kind, TokenKind::Semicolon)
            {
                Ok(Statement::Print(expression))
            } else {
                Err(miette::miette!(
                    labels = vec![LabeledSpan::at(expression.src, "this expression")],
                    "unterminated print statement; expected ';'"
                )
                .with_source_code(self.source.to_owned()))
            }
        } else {
            Err(miette::miette!(
                labels = vec![LabeledSpan::at(expression.src, "this expression")],
                "expected 'print'"
            )
            .with_source_code(self.source.to_owned()))
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Statement<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statement().transpose()
    }
}
