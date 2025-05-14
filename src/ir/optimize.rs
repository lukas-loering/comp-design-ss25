use std::collections::HashMap;

use super::{IrGraph, Node, NodeId};

#[derive(Debug)]
pub struct OptimizableNode<'g> {
    node: Node,
    graph: &'g mut IrGraph,
}

impl<'g> OptimizableNode<'g> {
    pub fn new(node: Node, graph: &'g mut IrGraph) -> OptimizableNode<'g> {        
        Self { node, graph }
    }
    
    pub fn reject(self) {}
    pub fn accept(self) -> NodeId {
        let id = self.node.id;
        let node = self.node;
        for p in &node.predecessors {
            self.graph.register_successor(*p, id);
        }
        self.graph.nodes.insert(id, node);
        id
    }
}

pub trait Optimizer {
    fn transform(&mut self, node: OptimizableNode<'_>) -> NodeId;
}

pub struct NoOptimizer;

impl Optimizer for NoOptimizer {
    fn transform(&mut self, node: OptimizableNode<'_>) -> NodeId {
        node.accept()
    }
}

/// Depends on [`NodeId::eq`]
#[derive(Default, Debug, Clone)]
pub struct LocalValueNumbering {
    known_nodes: Vec<NodeId>,
}

impl Optimizer for LocalValueNumbering {
    fn transform(&mut self, node: OptimizableNode<'_>) -> NodeId {
        // this linear lookup is O(n) which is not good.
        // however, I also dont have to implement a custom hash function (for know), which is good
        let found = self.known_nodes.iter().find(|other| node.node.eq(**other, &node.graph)).copied();
        match found {
            Some(found) => {
                node.reject();
                found
            },
            None => {
                let node = node.accept();
                self.known_nodes.push(node);
                node
            },
        }

    }
}