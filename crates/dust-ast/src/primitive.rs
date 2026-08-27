#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Primitive<'a> {
    Number(f64),
    String(&'a str),
    Bool(bool),
    Nil,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperation {
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

impl<'a> Primitive<'a> {
    pub fn not(self) -> Result<Primitive<'a>, ()> {
        match self {
            Primitive::Number(n) => Ok(Primitive::Number(-n)),
            Primitive::Bool(b) => Ok(Primitive::Bool(!b)),
            Primitive::String(_) => Err(()),
            Primitive::Nil => Err(()),
        }
    }

    pub fn logical_and(&self, rhs: &Self) -> Result<Primitive<'a>, ()> {
        match (self, rhs) {
            (Primitive::Bool(b1), Primitive::Bool(b2)) => Ok(Primitive::Bool(*b1 && *b2)),
            _ => Err(()),
        }
    }

    pub fn logical_or(&self, rhs: &Self) -> Result<Primitive<'a>, ()> {
        match (self, rhs) {
            (Primitive::Bool(b1), Primitive::Bool(b2)) => Ok(Primitive::Bool(*b1 || *b2)),
            _ => Err(()),
        }
    }

    pub fn mul(self, rhs: Self) -> Result<Primitive<'a>, ()> {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Ok(Primitive::Number(n1 * n2)),
            _ => Err(()),
        }
    }

    pub fn div(self, rhs: Self) -> Result<Primitive<'a>, ()> {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Ok(Primitive::Number(n1 / n2)),
            _ => Err(()),
        }
    }

    pub fn add(self, rhs: Self) -> Result<Primitive<'a>, ()> {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Ok(Primitive::Number(n1 + n2)),
            _ => Err(()),
        }
    }

    pub fn sub(self, rhs: Self) -> Result<Primitive<'a>, ()> {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Ok(Primitive::Number(n1 - n2)),
            _ => Err(()),
        }
    }
}
