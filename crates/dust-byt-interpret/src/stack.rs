use colored::Colorize;
use utils::Lit;

use crate::VirtualMachine;

pub struct Stack {
    top: usize,
    s: Vec<Lit>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            s: Vec::with_capacity(u8::MAX as usize),
            top: 0,
        }
    }

    pub fn top(&self) -> usize {
        self.top
    }

    pub fn set_top(&mut self, top: usize) {
        self.top = top;
    }

    pub fn print(&self, vm: &VirtualMachine) {
        println!(
            "{}{}",
            "stack".magenta(),
            format!("(len = {})", vm.stack.len()).white()
        );

        for (i, lit) in vm.stack.iter().enumerate() {
            if i == self.top {
                println!("{}", "--TOP--".green());
            }

            let i = format!("{:<4}", i as isize - self.top as isize);

            let literal = match lit {
                Lit::Number(f) => format!("; {}", f),
                Lit::String(symbol) => {
                    todo!()
                    // format!("; \"{}\"", ctx.symbols.resolve(symbol).unwrap())
                }
                Lit::Bool(b) => match b {
                    true => format!("; TRUE"),
                    false => format!("; FALSE"),
                },
                Lit::Nil => format!("; NIL"),
            };

            println!("{:<4} {}", i.blue(), literal.yellow(),);
        }

        println!("{}", "end".magenta());
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
        std::ops::Index::index(&self.s[self.top()..], index)
    }
}

impl core::ops::IndexMut<usize> for Stack {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        let top = self.top();

        self.s.resize((i + top) + 1, Lit::Nil);
        &mut self.s[top..][i]
    }
}

impl core::ops::IndexMut<std::ops::Range<usize>> for Stack {
    fn index_mut(&mut self, i: std::ops::Range<usize>) -> &mut Self::Output {
        let top = self.top();

        self.s.resize(i.end + top, Lit::Nil);
        &mut self.s[top..][i]
    }
}

impl core::ops::IndexMut<std::ops::RangeInclusive<usize>> for Stack {
    fn index_mut(&mut self, i: std::ops::RangeInclusive<usize>) -> &mut Self::Output {
        let top = self.top();

        self.s.resize(i.end() + top + 1, Lit::Nil);
        &mut self.s[top..][i]
    }
}
