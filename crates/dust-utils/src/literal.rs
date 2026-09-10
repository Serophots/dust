use crate::Symbol;

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    // NOT EQ  but &Lit can be :>
    PartialOrd,
    serde::Serialize,
    derive_generic_visitor::Drive,
)]
pub enum Lit {
    Number(f64),
    String(Symbol),
    Bool(bool),
    Nil,
}

impl Lit {
    pub fn negate(self) -> Result<Lit, ()> {
        match self {
            Lit::Number(b) => Ok(Lit::Number(-b)),
            _ => Err(()),
        }
    }

    pub fn not(self) -> Result<Lit, ()> {
        match self {
            Lit::Bool(b) => Ok(Lit::Bool(!b)),
            _ => Err(()),
        }
    }

    pub fn logical_and(self, rhs: Self) -> Result<Lit, ()> {
        match (self, rhs) {
            (Lit::Bool(b1), Lit::Bool(b2)) => Ok(Lit::Bool(b1 && b2)),
            _ => Err(()),
        }
    }

    pub fn logical_or(self, rhs: Self) -> Result<Lit, ()> {
        match (self, rhs) {
            (Lit::Bool(b1), Lit::Bool(b2)) => Ok(Lit::Bool(b1 || b2)),
            _ => Err(()),
        }
    }

    pub fn mul(self, rhs: Self) -> Result<Lit, ()> {
        match (self, rhs) {
            (Lit::Number(n1), Lit::Number(n2)) => Ok(Lit::Number(n1 * n2)),
            _ => Err(()),
        }
    }

    pub fn div(self, rhs: Self) -> Result<Lit, ()> {
        match (self, rhs) {
            (Lit::Number(n1), Lit::Number(n2)) => Ok(Lit::Number(n1 / n2)),
            _ => Err(()),
        }
    }

    pub fn add(self, rhs: Self) -> Result<Lit, ()> {
        match (self, rhs) {
            (Lit::Number(n1), Lit::Number(n2)) => Ok(Lit::Number(n1 + n2)),
            _ => Err(()),
        }
    }

    pub fn sub(self, rhs: Self) -> Result<Lit, ()> {
        match (self, rhs) {
            (Lit::Number(n1), Lit::Number(n2)) => Ok(Lit::Number(n1 - n2)),
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
