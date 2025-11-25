use miette::Result;

///! program        → statement* EOF ;
///! statement      → exprStmt
///!                | printStmt ;
///!
///! exprStmt       → expression ";" ;
///! printStmt      → "print" expression ";" ;
use crate::parser::Parser;

#[derive(Debug)]
pub enum Statement {}

impl<'a> Parser<'a> {
    /// Read a statement
    ///  statement      → exprStmt
    ///                | printStmt ;
    fn statement(&mut self) -> Result<Option<Statement>> {
        let token = match self.lexer.next() {
            Some(token) => token?,
            None => {
                return Ok(None);
            }
        };

        match token.kind {
            _ => {}
        }

        todo!()
    }

    /// Read an expression statement (a statement which evaluates to an expression)
    ///  exprStmt       → expression ";" ;
    fn expression_statement(&mut self) {}

    /// Read a print statement
    ///  printStmt      → "print" expression ";" ;
    fn print_statement(&mut self) {}
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.statement().transpose()
    }
}
