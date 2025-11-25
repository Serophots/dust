use std::{
    iter::{Filter, Peekable},
    ops::Not,
};

use miette::{LabeledSpan, Result};

use crate::{
    lexer::Lexer,
    token::{Token, TokenKind},
};

/// Evaluates a string of equality/comparison/addition/multiplication
///
/// ```
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;
/// ```
pub struct Calculator<'a> {
    pub source: &'a str,
    // Warning: Type gymnastics incoming
    lexer: Peekable<Filter<Lexer<'a>, fn(&Result<Token<TokenKind<'_>>>) -> bool>>,
}

impl<'a> Calculator<'a> {
    pub fn new(source: &'a str) -> Calculator<'a> {
        fn predicate<'a, 'b>(token: &'a Result<Token<TokenKind<'b>>>) -> bool {
            !matches!(token.as_ref().map(|t| t.kind), Ok(TokenKind::Comment(_)))
        }

        let predicate: fn(&Result<Token<TokenKind<'_>>>) -> bool = predicate;
        let lexer = Lexer::new(source).filter(predicate).peekable();

        Calculator { source, lexer }
    }

    pub fn parse(&mut self) -> Result<Token<Primary<'a>>> {
        self.equality()
    }

    /// Read a string of equalities == / !=
    ///  equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Token<Primary<'a>>> {
        let mut lhs = self.comparison()?;

        loop {
            let operator = match self.lexer.peek() {
                Some(Ok(token)) => token,
                Some(Err(_)) => {
                    let token = self.lexer.next().unwrap();
                    token?;
                    unreachable!("token should be Err")
                }
                None => break,
            };

            if operator.kind == TokenKind::EqualEqual {
                self.lexer.next();
                let rhs = self.comparison()?;

                lhs = lhs.equals(rhs);
            } else if operator.kind == TokenKind::BangEqual {
                self.lexer.next();
                let rhs = self.comparison()?;
                lhs = lhs.not_equals(rhs);
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a string of comparisons GT/GE/LT/LE
    ///  comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Token<Primary<'a>>> {
        let mut lhs = self.term()?;

        loop {
            let operator = match self.lexer.peek() {
                Some(Ok(token)) => token,
                Some(Err(_)) => {
                    let token = self.lexer.next().unwrap();
                    token?;
                    unreachable!("token should be Err")
                }
                None => break,
            };

            if operator.kind == TokenKind::Greater {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.greater(rhs);
            } else if operator.kind == TokenKind::GreaterEqual {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.greater_equal(rhs);
            } else if operator.kind == TokenKind::Lesser {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.lesser(rhs);
            } else if operator.kind == TokenKind::LesserEqual {
                self.lexer.next();
                let rhs = self.term()?;
                lhs = lhs.lesser_equal(rhs);
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a string of additions/subtractions
    ///  term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Token<Primary<'a>>> {
        let mut lhs = self.factor()?;

        loop {
            let operator = match self.lexer.peek() {
                Some(Ok(token)) => token,
                Some(Err(_)) => {
                    let token = self.lexer.next().unwrap();
                    token?;
                    unreachable!("token should be Err")
                }
                None => break,
            };

            if operator.kind == TokenKind::Plus {
                self.lexer.next();
                let rhs = self.factor()?;
                lhs = lhs + rhs;
            } else if operator.kind == TokenKind::Minus {
                self.lexer.next();
                let rhs = self.factor()?;
                lhs = lhs - rhs;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a string of multiplications/divisions
    ///  factor         → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Token<Primary<'a>>> {
        let mut lhs = self.unary()?;

        loop {
            let operator = match self.lexer.peek() {
                Some(Ok(token)) => token,
                Some(Err(_)) => {
                    let token = self.lexer.next().unwrap();
                    token?;
                    unreachable!("token should be Err")
                }
                None => break,
            };

            if operator.kind == TokenKind::Star {
                self.lexer.next();
                let rhs = self.unary()?;
                lhs = lhs * rhs;
            } else if operator.kind == TokenKind::Slash {
                self.lexer.next();
                let rhs = self.unary()?;
                lhs = lhs / rhs;
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    /// Read a negated unary or a primary
    ///  unary          → ( "!" | "-" ) unary | primary
    fn unary(&mut self) -> Result<Token<Primary<'a>>> {
        let operator = match self.lexer.peek() {
            Some(Ok(token)) => token,
            Some(Err(_)) => {
                let token = self.lexer.next().unwrap();
                token?;
                unreachable!("token should be Err")
            }
            None => {
                //TODO: Error in the input; something is wrong
                todo!()
            }
        };

        if matches!(operator.kind, TokenKind::Bang | TokenKind::Minus) {
            self.lexer.next();

            Ok(self.unary()?.not())
        } else {
            Ok(self.primary()?)
        }
    }

    /// Read a terminal token or a grouped expression
    ///  primary        → NUMBER | STRING | "true" | "false" | "nil"
    ///                | "(" expression ")" ;
    fn primary(&mut self) -> Result<Token<Primary<'a>>> {
        let token = match self.lexer.next() {
            Some(token) => token?,
            None => {
                let eof = self.source.chars().count();

                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(eof..=eof, "unexpected eof")],
                    "expected another Primary token"
                )
                .with_source_code(self.source.to_owned()));
            }
        };

        match token.kind {
            TokenKind::True => Ok(Token::new(Primary::Bool(true), token.src)),
            TokenKind::False => Ok(Token::new(Primary::Bool(false), token.src)),
            TokenKind::Nil => Ok(Token::new(Primary::Nil, token.src)),
            TokenKind::StringLiteral(str) => Ok(Token::new(Primary::String(str), token.src)),
            TokenKind::NumberLiteral(n) => Ok(Token::new(Primary::Number(n), token.src)),
            TokenKind::LeftParen => {
                let group_token = self.equality()?;

                let right_brace = match self.lexer.next() {
                    Some(Ok(right_brace)) => Some(right_brace),
                    Some(Err(err)) => Err(err)?,
                    None => None,
                };

                if let Some(right_brace) = right_brace
                    && matches!(right_brace.kind, TokenKind::RightParen)
                {
                    Ok(group_token)
                } else {
                    Err(miette::miette!(
                        labels = vec![LabeledSpan::at(token.src, "unclosed '('")],
                        "expected ')'"
                    )
                    .with_source_code(self.source.to_owned()))
                }
            }

            t => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.src,
                    format!("unexpected token '{t:?}'")
                )],
                "expected Primary token"
            )
            .with_source_code(self.source.to_owned())),
        }
    }
}

#[derive(Debug)]
pub enum Primary<'a> {
    Number(f64),
    String(&'a str),
    Bool(bool),
    Nil,
}

#[cfg(test)]
mod tests {
    use crate::calculator::{Calculator, Primary};

    #[test]
    fn test_parser() {
        assert_eq!(
            Calculator::new("5").primary().unwrap().kind,
            Primary::Number(5.0)
        );

        assert_eq!(
            Calculator::new("-5").unary().unwrap().kind,
            Primary::Number(-5.0)
        );

        assert_eq!(
            Calculator::new("3 * 5 / 7").factor().unwrap().kind,
            Primary::Number(15.0 / 7.0)
        );

        assert_eq!(
            Calculator::new("-7 * 5 / 7").factor().unwrap().kind,
            Primary::Number(-5.0)
        );

        assert_eq!(
            Calculator::new("-7 + 5 * 7").term().unwrap().kind,
            Primary::Number(28.0)
        );

        assert_eq!(
            Calculator::new("(-3 + 5) * 5 / 7").term().unwrap().kind,
            Primary::Number(10.0 / 7.0)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 < 4").comparison().unwrap().kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 > 4").comparison().unwrap().kind,
            Primary::Bool(false)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 <= -5")
                .comparison()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 >= -5")
                .comparison()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 >= -5 == true")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 >= -5 == false")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(false)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 >= -5 != true")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(false)
        );

        assert_eq!(
            Calculator::new("1 - 2 * 3 >= -5 != false")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Calculator::new("(1 / 2) == (1 / 2)")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(true)
        );

        assert_eq!(
            Calculator::new("(0 / 0) == (0 / 0)")
                .equality()
                .unwrap()
                .kind,
            Primary::Bool(false)
        );
    }
}
