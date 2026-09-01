use crate::{Path, Visitor};

pub struct PathVisitor {}

impl Visitor for PathVisitor {
    fn enter_path<'ast>(&mut self, p: &Path<'ast>) {

        // for i in 1..

        // let cmpts = &p.cmpts[..];
    }
}
