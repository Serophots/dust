use dust_hir::Func;

use crate::SourceLabeller;

impl<'hir> SourceLabeller for &Func<'hir> {
    fn label(self, labels: &mut Vec<miette::LabeledSpan>) {
        todo!()
    }
}
