#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

impl<'a> std::ops::Not for Primitive<'a> {
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

impl<'a> Primitive<'a> {
    pub fn logical_and(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Primitive::Bool(b1), Primitive::Bool(b2)) => *b1 && *b2,
            _ => todo!(),
        }
    }

    pub fn logical_or(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Primitive::Bool(b1), Primitive::Bool(b2)) => *b1 || *b2,
            _ => todo!(),
        }
    }
}

impl<'a> std::ops::Mul for Primitive<'a> {
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

impl<'a> std::ops::Div for Primitive<'a> {
    type Output = Primitive<'a>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 / n2),
            _ => todo!(),
        }
    }
}

impl<'a> std::ops::Add for Primitive<'a> {
    type Output = Primitive<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 + n2),
            _ => todo!(),
        }
    }
}

impl<'a> std::ops::Sub for Primitive<'a> {
    type Output = Primitive<'a>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Primitive::Number(n1), Primitive::Number(n2)) => Primitive::Number(n1 - n2),
            _ => todo!(),
        }
    }
}
