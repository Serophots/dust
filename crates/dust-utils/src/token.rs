use std::fmt::Debug;

use miette::SourceSpan;

use crate::Symbol;

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: SourceSpan,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
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
    Use,
    Nil,
    Return,
    Let,
    Comment,
    DocComment,
    StringLiteral(Symbol),
    NumberLiteral(f64),
    Ident(Symbol),
}

#[derive(PartialEq, Copy, Clone, serde::Serialize)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: SourceSpan,
}

impl core::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Ident").field(&self.symbol).finish()
    }
}

impl Token {
    pub fn new(kind: TokenKind, src: impl Into<SourceSpan>) -> Token {
        Token {
            kind,
            span: src.into(),
        }
    }
}

impl core::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
