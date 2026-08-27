use miette::{SourceOffset, SourceSpan};

use crate::combine_src;

#[derive(Clone)]
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
    /// ::
    PathSep,
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
    /// &&
    And,
    /// ||
    Or,
    If,
    Else,
    True,
    False,
    Loop,
    For,
    While,
    Pub,
    Function,
    Mod,
    Nil,
    Return,
    Let,
    Comment(&'a str),
    DocComment(&'a str),
    StringLiteral(&'a str),
    NumberLiteral(f64),
    Ident(Ident<'a>),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Ident<'a>(pub &'a str);

impl<T> Token<T> {
    pub fn new(kind: T, src: impl Into<SourceSpan>) -> Token<T> {
        Token {
            kind,
            src: src.into(),
        }
    }

    /// Intended for use in tests
    pub fn kind(kind: T) -> Token<T> {
        Token {
            kind,
            src: SourceSpan::new(SourceOffset::from(0), 0),
        }
    }

    pub fn map<U, F>(self, f: F) -> Token<U>
    where
        F: FnOnce(T) -> U,
    {
        Token {
            kind: f(self.kind),
            src: self.src,
        }
    }
}

impl<T> core::fmt::Debug for Token<T>
where
    T: core::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl<T> PartialEq for Token<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
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
