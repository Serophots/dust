use utils::{Token, combine_src};

use crate::{Arithmetic, Primitive};

pub trait TokenExtOrd {
    fn greater<'a>(self, rhs: Self) -> Token<Arithmetic<'a>>;

    fn greater_equal<'a>(self, rhs: Self) -> Token<Arithmetic<'a>>;

    fn lesser<'a>(self, rhs: Self) -> Token<Arithmetic<'a>>;

    fn lesser_equal<'a>(self, rhs: Self) -> Token<Arithmetic<'a>>;
}

pub trait TokenExtEq {
    fn equals<'a>(self, rhs: Self) -> Token<Arithmetic<'a>>;

    fn not_equals<'a>(self, rhs: Self) -> Token<Arithmetic<'a>>;
}

impl<T> TokenExtOrd for Token<T>
where
    T: PartialOrd,
{
    fn greater<'a>(self, rhs: Self) -> Token<Arithmetic<'a>> {
        Token::new(
            Arithmetic::Primitive(Primitive::Bool(self.kind > rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn greater_equal<'a>(self, rhs: Self) -> Token<Arithmetic<'a>> {
        Token::new(
            Arithmetic::Primitive(Primitive::Bool(self.kind >= rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn lesser<'a>(self, rhs: Self) -> Token<Arithmetic<'a>> {
        Token::new(
            Arithmetic::Primitive(Primitive::Bool(self.kind < rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn lesser_equal<'a>(self, rhs: Self) -> Token<Arithmetic<'a>> {
        Token::new(
            Arithmetic::Primitive(Primitive::Bool(self.kind <= rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }
}

impl<T> TokenExtEq for Token<T>
where
    T: PartialEq,
{
    fn equals<'a>(self, rhs: Self) -> Token<Arithmetic<'a>> {
        Token::new(
            Arithmetic::Primitive(Primitive::Bool(self.kind == rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }

    fn not_equals<'a>(self, rhs: Self) -> Token<Arithmetic<'a>> {
        Token::new(
            Arithmetic::Primitive(Primitive::Bool(self.kind != rhs.kind)),
            combine_src(self.src, rhs.src),
        )
    }
}
