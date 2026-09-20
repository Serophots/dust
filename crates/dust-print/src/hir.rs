use dust_hir::Func;

use crate::LabelPrinter;

impl<'hir> LabelPrinter for &Func<'hir> {
    fn label(self, labels: &mut Vec<miette::LabeledSpan>) {
        todo!()
    }
}
