use utils::{Token, combine_src};

use crate::{Arith, Primitive};

pub trait TokenExtOrd {
    fn greater<'a>(self, rhs: Self) -> Arith<'a>;

    fn greater_equal<'a>(self, rhs: Self) -> Arith<'a>;

    fn lesser<'a>(self, rhs: Self) -> Arith<'a>;

    fn lesser_equal<'a>(self, rhs: Self) -> Arith<'a>;
}

pub trait TokenExtEq {
    fn equals<'a>(self, rhs: Self) -> Arith<'a>;

    fn not_equals<'a>(self, rhs: Self) -> Arith<'a>;
}

impl<T> TokenExtOrd for Token<T>
where
    T: PartialOrd,
{
    fn greater<'a>(self, rhs: Self) -> Arith<'a> {
        Arith::Primitive {
            prim: Primitive::Bool(self.kind > rhs.kind),
            span: combine_src(self.span, rhs.span),
        }
    }

    fn greater_equal<'a>(self, rhs: Self) -> Arith<'a> {
        Arith::Primitive {
            prim: Primitive::Bool(self.kind >= rhs.kind),
            span: combine_src(self.span, rhs.span),
        }
    }

    fn lesser<'a>(self, rhs: Self) -> Arith<'a> {
        Arith::Primitive {
            prim: Primitive::Bool(self.kind < rhs.kind),
            span: combine_src(self.span, rhs.span),
        }
    }

    fn lesser_equal<'a>(self, rhs: Self) -> Arith<'a> {
        Arith::Primitive {
            prim: Primitive::Bool(self.kind <= rhs.kind),
            span: combine_src(self.span, rhs.span),
        }
    }
}

impl<T> TokenExtEq for Token<T>
where
    T: PartialEq,
{
    fn equals<'a>(self, rhs: Self) -> Arith<'a> {
        Arith::Primitive {
            prim: Primitive::Bool(self.kind == rhs.kind),
            span: combine_src(self.span, rhs.span),
        }
    }

    fn not_equals<'a>(self, rhs: Self) -> Arith<'a> {
        Arith::Primitive {
            prim: Primitive::Bool(self.kind != rhs.kind),
            span: combine_src(self.span, rhs.span),
        }
    }
}
