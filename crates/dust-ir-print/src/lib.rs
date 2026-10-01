use miette::LabeledSpan;

mod ast;
mod hir;

/// Recurse a data structure,
/// adding labels to the source code as you go
pub trait SourceLabeller {
    fn label(self, labels: &mut Vec<LabeledSpan>);
}
