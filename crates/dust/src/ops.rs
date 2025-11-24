use std::ops::{Add, Div, Mul, Not, Sub};

use crate::parser::Primary;

impl<'a> Not for Primary<'a> {
    type Output = Primary<'a>;

    fn not(self) -> Self::Output {
        match self {
            Primary::Number(n) => Primary::Number(-n),
            Primary::Bool(b) => Primary::Bool(!b),
            Primary::String(_) => todo!(),
            Primary::Nil => todo!(),
        }
    }
}

impl<'a> Mul for Primary<'a> {
    type Output = Primary<'a>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Primary::Number(n1) => match rhs {
                Primary::Number(n2) => Primary::Number(n1 * n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> Div for Primary<'a> {
    type Output = Primary<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Primary::Number(n1) => match rhs {
                Primary::Number(n2) => Primary::Number(n1 / n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> Add for Primary<'a> {
    type Output = Primary<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Primary::Number(n1) => match rhs {
                Primary::Number(n2) => Primary::Number(n1 + n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> Sub for Primary<'a> {
    type Output = Primary<'a>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Primary::Number(n1) => match rhs {
                Primary::Number(n2) => Primary::Number(n1 - n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> PartialOrd for Primary<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Primary::Number(n1) => match other {
                Primary::Number(n2) => PartialOrd::partial_cmp(n1, &n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> PartialEq for Primary<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            _ => false,
        }
    }
}
