use std::ops::{Add, Div, Mul, Not, Sub};

use crate::parser::Primitive;

impl<'a> Not for Primitive<'a> {
    type Output = Primitive<'a>;

    fn not(self) -> Self::Output {
        match self {
            Primitive::Number(n) => Primitive::Number(-n),
            Primitive::Bool(b) => Primitive::Bool(!b),
            Primitive::String(_) => todo!(),
            Primitive::Nil => todo!(),
        }
    }
}

impl<'a> Mul for Primitive<'a> {
    type Output = Primitive<'a>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 * n2),
            (a, b) => {
                panic!("tried to multiply {:?} by {:?}", a, b);
            }
        }
    }
}

impl<'a> Div for Primitive<'a> {
    type Output = Primitive<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 / n2),
            _ => todo!(),
        }
    }
}

impl<'a> Add for Primitive<'a> {
    type Output = Primitive<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 + n2),
            _ => todo!(),
        }
    }
}

impl<'a> Sub for Primitive<'a> {
    type Output = Primitive<'a>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 - n2),
            _ => todo!(),
        }
    }
}
