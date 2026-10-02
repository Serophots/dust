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

impl<I: std::slice::SliceIndex<[Lit]>> std::ops::Index<I> for Stack {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        std::ops::Index::index(&self.s, index)
    }
}

impl core::ops::IndexMut<usize> for Stack {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.s.resize(i + 1, Lit::Nil);
        &mut self.s[i as usize]
    }
}

impl core::ops::IndexMut<std::ops::Range<usize>> for Stack {
    fn index_mut(&mut self, i: std::ops::Range<usize>) -> &mut Self::Output {
        self.s.resize(i.end, Lit::Nil);
        &mut self.s[i]
    }
}

impl core::ops::IndexMut<std::ops::RangeInclusive<usize>> for Stack {
    fn index_mut(&mut self, i: std::ops::RangeInclusive<usize>) -> &mut Self::Output {
        self.s.resize(i.end() + 1, Lit::Nil);
        &mut self.s[i]
    }
}
