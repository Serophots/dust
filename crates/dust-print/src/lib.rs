use miette::LabeledSpan;

mod ast;
mod hir;

/// Recurse a data structure, labelling each part as you go
pub trait LabelPrinter {
    fn label(self, labels: &mut Vec<LabeledSpan>);
}
