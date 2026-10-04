use colored::Colorize as _;
use dust_ctxt::GblCtxt;
use dust_hir::{Func, Krate};

use crate::SourceLabeller;

impl<'hir> SourceLabeller for &Func<'hir> {
    fn label(self, labels: &mut Vec<miette::LabeledSpan>) {
        todo!()
    }
}

pub fn print_hir(krate: &Krate, ctx: GblCtxt) {
    for func in krate.funcs.iter() {
        let ident = ctx.symbols.resolve(func.ident.symbol).unwrap();
        println!("{} {}{}", "fn".magenta(), ident.blue(), "(...)".white());

        println!("{:#?}", func.block);

        println!("{}", "end".magenta());
        print!("\n")
    }
}
