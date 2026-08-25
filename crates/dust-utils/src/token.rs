use miette::SourceSpan;

use crate::combine_src;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<T> {
    pub kind: T,
    pub src: SourceSpan,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenKind<'a> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Minus,
    Plus,
    Semicolon,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    LesserEqual,
    GreaterEqual,
    Lesser,
    Greater,
    Slash,
    And,
    Or,
    If,
    Else,
    True,
    False,
    While,
    For,
    Function,
    Nil,
    Return,
    Let,
    Print,
    Comment(&'a str),
    DocComment(&'a str),
    StringLiteral(&'a str),
    NumberLiteral(f64),
    Identifier(&'a str),
}

impl<T> Token<T> {
    pub fn new(kind: T, src: impl Into<SourceSpan>) -> Token<T> {
        Token {
            kind,
            src: src.into(),
        }
    }
}

impl<T> std::ops::Not for Token<T>
where
    T: std::ops::Not,
{
    type Output = Token<T::Output>;

    fn not(self) -> Self::Output {
        Token::new(!self.kind, self.src)
    }
}

impl<T> std::ops::Mul for Token<T>
where
    T: std::ops::Mul,
{
    type Output = Token<T::Output>;

    fn mul(self, rhs: Self) -> Self::Output {
        Token::new(self.kind * rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> std::ops::Div for Token<T>
where
    T: std::ops::Div,
{
    type Output = Token<T::Output>;

    fn div(self, rhs: Self) -> Self::Output {
        Token::new(self.kind / rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> std::ops::Add for Token<T>
where
    T: std::ops::Add,
{
    type Output = Token<T::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        Token::new(self.kind + rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> std::ops::Sub for Token<T>
where
    T: std::ops::Sub,
{
    type Output = Token<T::Output>;

    fn sub(self, rhs: Self) -> Self::Output {
        Token::new(self.kind - rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> PartialOrd for Token<T>
where
    T: PartialOrd,
    Token<T>: PartialEq,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        PartialOrd::partial_cmp(&self.kind, &other.kind)
    }
}
