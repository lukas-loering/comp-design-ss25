use super::node::IrNode;

pub trait Optimizer {
    fn transform(&mut self, node: IrNode) -> IrNode;
}

pub struct NoOptimizer;

impl Optimizer for NoOptimizer {
    fn transform(&mut self, node: IrNode) -> IrNode {
        node
    }
}
