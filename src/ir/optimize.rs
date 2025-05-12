use std::collections::HashMap;

use super::{NodeId, IrGraph};

pub trait Optimizer {
    fn transform(&mut self,graph: &mut IrGraph, node: NodeId) -> NodeId;
}

pub struct NoOptimizer;

impl Optimizer for NoOptimizer {
    fn transform(&mut self, graph: &mut IrGraph, node: NodeId) -> NodeId {
        node
    }
}

/// Depends on [`NodeId::eq`]
#[derive(Default, Debug, Clone)]
pub struct LocalValueNumbering {
    known_nodes: Vec<NodeId>,
}

impl Optimizer for LocalValueNumbering {
    fn transform(&mut self, graph: &mut IrGraph, node: NodeId) -> NodeId {
        // this linear lookup is O(n) which is not good.
        // however, I also dont have to implement a custom hash function (for know), which is good
        let found = self.known_nodes.iter().find(|other| node.eq(**other, &graph)).copied();
        match found {
            Some(found) => found,
            None => {
                self.known_nodes.push(node);
                node
            },
        }

    }
}