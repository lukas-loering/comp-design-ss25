use std::collections::HashMap;
use std::rc::Rc;

use super::{Graph, IrGraph};
pub use base::Node;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub const NULL: Self = NodeId(0);
    pub const FIRST: Self = NodeId(1);

    pub fn next(&self) -> Self {
        NodeId(
            self.0
                .checked_add(1)
                .expect("can not create more than 4,294,967,295 IR nodes"),
        )
    }
}

mod base {
    use std::hash::{Hash, Hasher};
    use std::rc::Rc;

    use crate::ir::{Graph, IrGraph};

    use super::NodeId;

    #[derive(Debug, Clone)]
    pub struct Node {
        graph: Graph,
        id: NodeId,
        predecessors: Vec<NodeId>,
        block: NodeId,
    }

    impl Node {
        pub fn new(graph: Graph, id: NodeId, predecessors: Vec<NodeId>, block: NodeId) -> Self {
            for predecssor in &predecessors {
                graph.borrow_mut().register_successor(*predecssor, id);
            }

            Self {
                graph,
                id,
                predecessors,
                block,
            }
        }

        pub fn id(&self) -> NodeId {
            self.id
        }

        pub fn block(&self) -> NodeId {
            self.block
        }

        pub fn graph(&self) -> Graph {
            self.graph.clone()
        }
    }

    impl PartialEq for Node {
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }

    impl Eq for Node {}
}

#[derive(Debug, Clone)]
pub struct NodeRepository {
    next_node_id: NodeId,
    nodes: HashMap<NodeId, Node>,
}

impl NodeRepository {
    pub fn new() -> Self {
        Self {
            next_node_id: NodeId::FIRST,
            nodes: Default::default(),
        }
    }
    fn new_node(&mut self, block: NodeId, predecessors: &[NodeId]) -> NodeId {
        let block = self.get(block);
        let graph = block.graph();
        let block_id = block.id();
        self.register(|id| Node::new(graph.clone(), id, predecessors.to_vec(), block_id))
    }

    fn new_block(&mut self, graph: Graph) -> NodeId {
        self.register(|id| Node::new(graph.clone(), id, Default::default(), id))
    }

    fn new_binary_op(&mut self, block: NodeId, left: NodeId, right: NodeId) -> NodeId {
        self.new_node(block, &[left, right])
    }
    fn new_binary_op_sideeffect(
        &mut self,
        block: NodeId,
        left: NodeId,
        right: NodeId,
        sideeffect: NodeId,
    ) -> NodeId {
        self.new_node(block, &[left, right, sideeffect])
    }

    #[must_use]
    fn register(&mut self, create_node: impl Fn(NodeId) -> Node) -> NodeId {
        let current_id = self.next_node_id;
        self.nodes
            .insert(current_id, create_node(current_id))
            .expect("node ids must be unique");
        self.next_node_id = self.next_node_id.next();
        current_id
    }

    fn get(&self, node: NodeId) -> &Node {
        // This repository is the only one who can create node ids.
        // If we get a node id we know it is valid
        self.nodes.get(&node).expect("node must exists")
    }

    fn get_mut(&mut self, node: NodeId) -> &mut Node {
        // This repository is the only one who can create node ids.
        // If we get a node id we know it is valid
        self.nodes.get_mut(&node).expect("node must exists")
    }
}

pub enum IrNode {
    Block(Block),
}

pub struct Block {
    id: NodeId,
}

impl Block {
    pub fn new(graph: Graph) -> Self {
        Self {
            id: graph.borrow_mut().nodes_mut().new_block(graph.clone()),
        }
    }

    pub fn node(&self) -> NodeId {
        self.id
    }
}

pub struct BinaryOpNode {
    node: NodeId,
}

impl BinaryOpNode {
    pub fn new(graph: Graph, block: NodeId, left: NodeId, right: NodeId) -> Self {
        Self {
            node: graph
                .borrow_mut()
                .nodes_mut()
                .new_binary_op(block, left, right),
        }
    }
}
