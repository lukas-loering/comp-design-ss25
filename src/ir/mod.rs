use std::{cell::RefCell, collections::HashMap, rc::Rc};

use linked_hash_set::LinkedHashSet;
use node::{Block, Node, NodeId, NodeRepository};
use tracing_subscriber::field::debug;

mod constructor;
mod node;
mod optimize;
mod ssa;

type Graph = Rc<RefCell<IrGraph>>;

#[derive(Debug, Clone)]
struct IrGraph {
    nodes: NodeRepository,
    name: Box<str>,
    successors: HashMap<NodeId, LinkedHashSet<NodeId>>,
    start_block: NodeId,
    end_block: NodeId,
}

impl IrGraph {
    fn new(name: Box<str>) -> impl FnOnce() -> Graph {
        let graph = Self {
            nodes: NodeRepository::new(),
            name,
            successors: Default::default(),
            start_block: NodeId::NULL,
            end_block: NodeId::NULL,
        };
        let graph = Rc::new(RefCell::new(graph));
        let start_block = Block::new(graph).id;
        || {

        }
    }

    fn register_successor(&mut self, r#for: NodeId, successor: NodeId) {
        self.successors.entry(r#for).or_default().insert(successor);
    }

    fn remove_sucessor(&mut self, r#for: NodeId, successor: NodeId) {
        self.successors.entry(r#for).or_default().remove(&successor);
    }

    fn nodes_mut(&mut self) -> &mut NodeRepository {
        &mut self.nodes
    }
}
