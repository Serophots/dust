use std::ops::{Add, Div, Mul, Not, Sub};

use crate::parser::Expression;

impl<'a> Not for Expression<'a> {
    type Output = Expression<'a>;

    fn not(self) -> Self::Output {
        match self {
            Expression::Number(n) => Expression::Number(-n),
            Expression::Bool(b) => Expression::Bool(!b),
            Expression::String(_) => todo!(),
            Expression::Nil => todo!(),
            Expression::Identifier(_) => todo!(),
        }
    }
}

impl<'a> Mul for Expression<'a> {
    type Output = Expression<'a>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Number(n1) => match rhs {
                Expression::Number(n2) => Expression::Number(n1 * n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> Div for Expression<'a> {
    type Output = Expression<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Number(n1) => match rhs {
                Expression::Number(n2) => Expression::Number(n1 / n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> Add for Expression<'a> {
    type Output = Expression<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Number(n1) => match rhs {
                Expression::Number(n2) => Expression::Number(n1 + n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> Sub for Expression<'a> {
    type Output = Expression<'a>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Expression::Number(n1) => match rhs {
                Expression::Number(n2) => Expression::Number(n1 - n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> PartialOrd for Expression<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Expression::Number(n1) => match other {
                Expression::Number(n2) => PartialOrd::partial_cmp(n1, &n2),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

impl<'a> PartialEq for Expression<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            _ => false,
        }
    }
}
