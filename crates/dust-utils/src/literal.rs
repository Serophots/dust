use crate::Symbol;

#[derive(
    Debug, Copy, Clone, PartialEq, PartialOrd, serde::Serialize, derive_generic_visitor::Drive,
)]
pub enum Literal {
    Number(#[drive(skip)] f64),
    String(Symbol),
    Bool(#[drive(skip)] bool),
    Nil,
}

impl Literal {
    pub fn negate(self) -> Result<Literal, ()> {
        match self {
            Literal::Number(b) => Ok(Literal::Number(-b)),
            _ => Err(()),
        }
    }

    pub fn not(self) -> Result<Literal, ()> {
        match self {
            Literal::Bool(b) => Ok(Literal::Bool(!b)),
            _ => Err(()),
        }
    }

    pub fn logical_and(&self, rhs: &Self) -> Result<Literal, ()> {
        match (self, rhs) {
            (Literal::Bool(b1), Literal::Bool(b2)) => Ok(Literal::Bool(*b1 && *b2)),
            _ => Err(()),
        }
    }

    pub fn logical_or(&self, rhs: &Self) -> Result<Literal, ()> {
        match (self, rhs) {
            (Literal::Bool(b1), Literal::Bool(b2)) => Ok(Literal::Bool(*b1 || *b2)),
            _ => Err(()),
        }
    }

    pub fn mul(self, rhs: Self) -> Result<Literal, ()> {
        match (self, rhs) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 * n2)),
            _ => Err(()),
        }
    }

    pub fn div(self, rhs: Self) -> Result<Literal, ()> {
        match (self, rhs) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 / n2)),
            _ => Err(()),
        }
    }

    pub fn add(self, rhs: Self) -> Result<Literal, ()> {
        match (self, rhs) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 + n2)),
            _ => Err(()),
        }
    }

    pub fn sub(self, rhs: Self) -> Result<Literal, ()> {
        match (self, rhs) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 - n2)),
            _ => Err(()),
        }
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, derive_generic_visitor::Drive,
)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,
    And,
    Or,
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Hash, serde::Serialize, derive_generic_visitor::Drive,
)]
pub enum UnaryOp {
    Negate,
    Not,
}
