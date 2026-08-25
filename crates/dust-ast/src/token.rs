use utils::{Token, combine_src};

use crate::{Expression, Primitive};

pub trait TokenExtOrd {
    fn greater<'a>(self, rhs: Self) -> Token<Expression<'a>>;

    fn greater_equal<'a>(self, rhs: Self) -> Token<Expression<'a>>;

    fn lesser<'a>(self, rhs: Self) -> Token<Expression<'a>>;

    fn lesser_equal<'a>(self, rhs: Self) -> Token<Expression<'a>>;
}

pub trait TokenExtEq {
    fn equals<'a>(self, rhs: Self) -> Token<Expression<'a>>;

    fn not_equals<'a>(self, rhs: Self) -> Token<Expression<'a>>;
}

impl<T> TokenExtOrd for Token<T>
where
    T: PartialOrd,
{
    fn greater<'a>(self, rhs: Self) -> Token<Expression<'a>> {
        Token::new(
            Expression::Primitive(Primitive::Bool(self.kind > rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn greater_equal<'a>(self, rhs: Self) -> Token<Expression<'a>> {
        Token::new(
            Expression::Primitive(Primitive::Bool(self.kind >= rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn lesser<'a>(self, rhs: Self) -> Token<Expression<'a>> {
        Token::new(
            Expression::Primitive(Primitive::Bool(self.kind < rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn lesser_equal<'a>(self, rhs: Self) -> Token<Expression<'a>> {
        Token::new(
            Expression::Primitive(Primitive::Bool(self.kind <= rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }
}

impl<T> TokenExtEq for Token<T>
where
    T: PartialEq,
{
    fn equals<'a>(self, rhs: Self) -> Token<Expression<'a>> {
        Token::new(
            Expression::Primitive(Primitive::Bool(self.kind == rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn not_equals<'a>(self, rhs: Self) -> Token<Expression<'a>> {
        Token::new(
            Expression::Primitive(Primitive::Bool(self.kind != rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }
}
