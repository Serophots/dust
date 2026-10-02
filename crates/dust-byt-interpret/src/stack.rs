use utils::Lit;

pub struct Stack {
    s: Vec<Lit>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            s: Vec::with_capacity(u8::MAX as usize),
        }
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }

    pub fn is_empty(&self) -> bool {
        self.s.is_empty()
    }

    pub fn iter<'a>(&'a self) -> core::slice::Iter<'a, Lit> {
        self.s.iter()
    }
}

impl core::ops::Index<usize> for Stack {
    type Output = Lit;

    fn index(&self, i: usize) -> &Self::Output {
        match self.s.get(i as usize) {
            Some(l) => l,
            None => &Lit::Nil,
        }
    }
}

impl core::ops::IndexMut<usize> for Stack {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.s.resize(i + 1, Lit::Nil);
        &mut self.s[i as usize]
    }
}

impl core::fmt::Debug for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.s.iter().enumerate().map(|(i, lit)| format!("")))
            .finish()
    }
}
