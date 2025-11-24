use std::ops::{Add, Div, Mul, Not, Sub};

use miette::SourceSpan;

use crate::calculator::Primary;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Token<T> {
    pub kind: T,
    pub src: SourceSpan,
}

impl<T> Token<T> {
    pub fn new(kind: T, src: impl Into<SourceSpan>) -> Token<T> {
        Token {
            kind,
            src: src.into(),
        }
    }
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
    Comment(&'a str),
    DocComment(&'a str),
    StringLiteral(&'a str),
    NumberLiteral(f64),
    Identifier(&'a str),
}

impl<T> Clone for Token<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            src: self.src.clone(),
        }
    }
}

impl<T> Not for Token<T>
where
    T: Not,
{
    type Output = Token<T::Output>;

    fn not(self) -> Self::Output {
        Token::new(!self.kind, self.src)
    }
}

impl<T> Mul for Token<T>
where
    T: Mul,
{
    type Output = Token<T::Output>;

    fn mul(self, rhs: Self) -> Self::Output {
        Token::new(self.kind * rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> Div for Token<T>
where
    T: Div,
{
    type Output = Token<T::Output>;

    fn div(self, rhs: Self) -> Self::Output {
        Token::new(self.kind / rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> Add for Token<T>
where
    T: Add,
{
    type Output = Token<T::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        Token::new(self.kind + rhs.kind, combine_src(self.src, rhs.src))
    }
}

impl<T> Sub for Token<T>
where
    T: Sub,
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

impl<T> Token<T>
where
    T: PartialOrd,
{
    pub fn greater<'a>(self, rhs: Self) -> Token<Primary<'a>> {
        Token::new(
            Primary::Bool(self.kind > rhs.kind),
            combine_src(self.src, rhs.src),
        )
    }

    pub fn greater_equal<'a>(self, rhs: Self) -> Token<Primary<'a>> {
        Token::new(
            Primary::Bool(self.kind >= rhs.kind),
            combine_src(self.src, rhs.src),
        )
    }

    pub fn lesser<'a>(self, rhs: Self) -> Token<Primary<'a>> {
        Token::new(
            Primary::Bool(self.kind < rhs.kind),
            combine_src(self.src, rhs.src),
        )
    }

    pub fn lesser_equal<'a>(self, rhs: Self) -> Token<Primary<'a>> {
        Token::new(
            Primary::Bool(self.kind <= rhs.kind),
            combine_src(self.src, rhs.src),
        )
    }
}

impl<T> Token<T>
where
    T: PartialEq,
{
    pub fn equals<'a>(self, rhs: Self) -> Token<Primary<'a>> {
        Token::new(
            Primary::Bool(self.kind == rhs.kind),
            combine_src(self.src, rhs.src),
        )
    }

    pub fn not_equals<'a>(self, rhs: Self) -> Token<Primary<'a>> {
        Token::new(
            Primary::Bool(self.kind != rhs.kind),
            combine_src(self.src, rhs.src),
        )
    }
}

fn combine_src(src1: SourceSpan, src2: SourceSpan) -> SourceSpan {
    let lower = std::cmp::min(src1.offset(), src2.offset());
    let upper = std::cmp::max(src1.offset() + src1.len(), src2.offset() + src2.len());
    SourceSpan::from((lower, upper - lower))
}
