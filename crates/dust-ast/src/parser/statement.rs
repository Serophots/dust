use miette::{LabeledSpan, Result};
use utils::{Token, TokenKind};

///! program        → statement* EOF ;
///!
///! statement      → exprStmt
///!                | printStmt
///!                | varDecl;
///!
///! varDecl        → "let" IDENTIFIER ( "=" expression )? ";" ;
///!
///! exprStmt       → expression ";" ;
///! printStmt      → "print" expression ";" ;
use crate::{Expression, Primitive, parser::Parser};

#[derive(Debug)]
pub enum Statement<'a> {
    Expresion(Token<Expression<'a>>),
    Print(Token<Expression<'a>>),
    Declaration {
        identifier: Token<&'a str>,
        value: Token<Expression<'a>>,
    },
}

impl<'a> Parser<'a> {
    // /// Read a statement
    // ///  statement      → exprStmt
    // ///                | printStmt ;
    // ///                | varDecl;
    // fn statement(&mut self) -> Result<Option<Statement<'a>>> {
    //     let Some(token) = self.peek_token_or_err()? else {
    //         return Ok(None);
    //     };

    //     Ok(Some(match token.kind {
    //         TokenKind::Print => self.print_statement()?,
    //         TokenKind::Let => self.variable_declaration()?,
    //         _ => self.expression_statement()?,
    //     }))
    // }

    // /// Read a variable declaration
    // ///  varDecl        → "let" IDENTIFIER ( "=" expression )? ";" ;
    // fn variable_declaration(&mut self) -> Result<Statement<'a>> {
    //     let var = self
    //         .next_token(|token| match token.kind {
    //             TokenKind::Let => Some(Token {
    //                 kind: (),
    //                 src: token.src,
    //             }),
    //             _ => None,
    //         })?
    //         .flatten();

    //     let Some(var) = var else {
    //         return Err(miette::miette!("expected 'var'").with_source_code(self.source.to_owned()));
    //     };

    //     let identifier = self
    //         .next_token(|token| match token.kind {
    //             TokenKind::Identifier(s) => Some(Token {
    //                 kind: s,
    //                 src: token.src,
    //             }),
    //             _ => None,
    //         })?
    //         .flatten();

    //     let Some(identifier) = identifier else {
    //         return Err(miette::miette!(
    //             labels = vec![LabeledSpan::at(var.src, "this variable declaration")],
    //             "expected identifier for variable declaration"
    //         )
    //         .with_source_code(self.source.to_owned()));
    //     };

    //     enum NextToken {
    //         Equals,
    //         Semicolon,
    //         Other,
    //     }

    //     let token = self.next_token(|token| Token {
    //         kind: match token.kind {
    //             TokenKind::Semicolon => NextToken::Semicolon,
    //             TokenKind::Equal => NextToken::Equals,
    //             _ => NextToken::Other,
    //         },
    //         src: token.src,
    //     })?;

    //     match token {
    //         Some(token) => match token.kind {
    //             NextToken::Semicolon => {
    //                 return Ok(Statement::Declaration {
    //                     identifier,
    //                     value: Token {
    //                         kind: Expression::Primitive(Primitive::Nil),
    //                         src: token.src,
    //                     },
    //                 });
    //             }
    //             NextToken::Equals => {
    //                 let expression = self.expression()?;
    //                 let semicolon = self
    //                     .next_token(|token| matches!(token.kind, TokenKind::Semicolon))?
    //                     .unwrap_or(false);

    //                 if semicolon {
    //                     return Ok(Statement::Declaration {
    //                         identifier,
    //                         value: expression,
    //                     });
    //                 }
    //             }
    //             _ => {}
    //         },
    //         None => {}
    //     }

    //     Err(miette::miette!(
    //         labels = vec![LabeledSpan::at(identifier.src, "this variable declaration")],
    //         "expected '=' or ';' after a variable declaration"
    //     )
    //     .with_source_code(self.source.to_owned()))
    // }

    // /// Read an expression statement (a statement which evaluates to an expression)
    // ///  exprStmt       → expression ";" ;
    // fn expression_statement(&mut self) -> Result<Statement<'a>> {
    //     let expression = self.expression()?;
    //     let semicolon = self
    //         .next_token(|token| matches!(token.kind, TokenKind::Semicolon))?
    //         .unwrap_or(false);

    //     if semicolon {
    //         Ok(Statement::Expresion(expression))
    //     } else {
    //         Err(miette::miette!(
    //             labels = vec![LabeledSpan::at(expression.src, "this expression")],
    //             "expected ';' after a statement"
    //         )
    //         .with_source_code(self.source.to_owned()))
    //     }
    // }

    // /// Read a print statement
    // ///  printStmt      → "print" expression ";" ;
    // fn print_statement(&mut self) -> Result<Statement<'a>> {
    //     let print = self
    //         .next_token(|token| matches!(token.kind, TokenKind::Print))?
    //         .unwrap_or(false);
    //     let expression = self.expression()?;

    //     if !print {
    //         return Err(miette::miette!(
    //             labels = vec![LabeledSpan::at(expression.src, "this expression")],
    //             "expected 'print'"
    //         )
    //         .with_source_code(self.source.to_owned()));
    //     }

    //     let semicolon = self
    //         .next_token(|token| matches!(token.kind, TokenKind::Semicolon))?
    //         .unwrap_or(false);

    //     if !semicolon {
    //         return Err(miette::miette!(
    //             labels = vec![LabeledSpan::at(expression.src, "this expression")],
    //             "expected ';' after a print statement"
    //         )
    //         .with_source_code(self.source.to_owned()));
    //     }

    //     Ok(Statement::Print(expression))
    // }
}

// impl<'a> Iterator for Parser<'a> {
//     type Item = Result<Statement<'a>>;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.statement().transpose()
//     }
// }
