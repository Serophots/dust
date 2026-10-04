use miette::LabeledSpan;

mod ast;
mod hir;

pub use hir::print_hir;

/// Recurse a data structure,
/// adding labels to the source code as you go
pub trait SourceLabeller {
    fn label(self, labels: &mut Vec<LabeledSpan>);
}
