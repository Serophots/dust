use std::cell::RefCell;

use utils::NodeId;

#[derive(Default)]
pub struct NodeIdAllocator(RefCell<NodeId>);

impl NodeIdAllocator {
    pub fn next(&self) -> NodeId {
        let mut borrow = self.0.borrow_mut();
        let ret = *borrow;
        *borrow = ret + NodeId::ONE;
        ret
    }
}
