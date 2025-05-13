use std::{
    any::Any,
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, format},
    rc::Rc,
};

use linked_hash_set::LinkedHashSet;

mod constructor;
pub mod debug;
mod node;
pub mod optimize;
mod ssa;

pub use ssa::SsaTranslation;

type Graph = Rc<RefCell<IrGraph>>;

#[derive(Debug)]
pub struct IrGraph {
    next_node_id: NodeId,
    nodes: HashMap<NodeId, Node>,
    name: Box<str>,
    successors: HashMap<NodeId, LinkedHashSet<NodeId>>,
    start_block: NodeId,
    end_block: NodeId,
    // store in nodes in the java impl:
    node_data: HashMap<NodeId, Box<dyn Any>>,
}

impl IrGraph {
    fn new(name: Box<str>) -> IrGraph {
        let start_block = NodeId::FIRST;
        let end_block = start_block.next();
        let next_node_id = end_block.next();
        let mut nodes = HashMap::new();
        nodes.insert(
            start_block,
            Node::new(start_block, start_block, vec![], NodeKind::Block),
        );
        nodes.insert(
            end_block,
            Node::new(end_block, end_block, vec![], NodeKind::Block),
        );

        let graph = Self {
            name,
            start_block,
            end_block,
            next_node_id,
            nodes: nodes,
            successors: Default::default(),
            node_data: Default::default(),
        };

        graph
    }

    fn register_successor(&mut self, r#for: NodeId, successor: NodeId) {
        self.successors.entry(r#for).or_default().insert(successor);
    }

    fn remove_sucessor(&mut self, r#for: NodeId, successor: NodeId) {
        self.successors.entry(r#for).or_default().remove(&successor);
    }

    pub fn successors(&self, node: NodeId) -> LinkedHashSet<NodeId> {
        self.successors.get(&node).cloned().unwrap_or_default()
    }

    fn next_node_id(&mut self) -> NodeId {
        let current = self.next_node_id;
        self.next_node_id = current.next();
        current
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn end_block(&self) -> NodeId {
        self.end_block
    }
}

impl AsMut<IrGraph> for &mut IrGraph {
    fn as_mut(&mut self) -> &mut IrGraph {
        self
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub const NULL: Self = NodeId(0);
    pub const FIRST: Self = NodeId(1);

    pub fn next(mut self) -> Self {
        self.0 = self
            .0
            .checked_add(1)
            .expect("can not create more than 4,294,967,295 IR nodes");
        self
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct Node {
    id: NodeId,
    block: NodeId,
    predecessors: Vec<NodeId>,
    kind: NodeKind,
    data: Option<Box<dyn Any>>,
}

impl Node {
    pub fn new(id: NodeId, block: NodeId, predecessors: Vec<NodeId>, kind: NodeKind) -> Self {
        Self {
            id,
            block,
            predecessors,
            kind,
            data: None,
        }
    }

    fn new_node(
        mut g: impl AsMut<IrGraph>,
        kind: NodeKind,
        block: NodeId,
        predecessors: &[NodeId],
    ) -> NodeId {
        let g = g.as_mut();
        let id = g.next_node_id();
        g.nodes
            .insert(id, Node::new(id, block, predecessors.to_vec(), kind));
        for p in predecessors {
            g.register_successor(*p, id);
        }
        id
    }

    fn set_data<T: 'static>(&mut self, data: T) {
        self.data = Some(Box::new(data))
    }

    pub fn get_data<T: 'static>(&self) -> Option<&T> {
        if let Some(data) = &self.data {
            data.downcast_ref::<T>()
        } else {
            None
        }
    }

    fn info(&self) -> String {
        let value = match self.kind {
            NodeKind::ConstInt => Some(self.get_data::<i64>().unwrap().to_string()),
            _ => None,
        };
        match value {
            Some(v) => format!("({}){:?}[{}]", self.id, self.kind, v),
            None => format!("({}){:?}", self.id, self.kind),
        }
    }

    pub fn kind(&self) -> NodeKind {
        self.kind
    }
}

impl NodeId {
    fn set_predecessor(self, g: &mut IrGraph, idx: usize, node: NodeId) {
        let predecessor = g
            .get(self)
            .predecessors
            .get(idx)
            .copied()
            .expect("predecessor to exist");
        g.remove_sucessor(predecessor, self);
        g.get_mut(self).predecessors[idx] = node;
        g.register_successor(node, self);
    }

    pub fn predecessor(self, g: &IrGraph, idx: usize) -> NodeId {
        g.get(self)
            .predecessors
            .get(idx)
            .expect("predecssor to exist")
            .clone()
    }
    pub fn predecessor_skip_proj(self, g: &IrGraph, idx: usize) -> NodeId {
        let pred = self.predecessor(g, idx);
        if matches!(g.get(pred).kind, NodeKind::Projection(_)) {
            return pred.predecessor(g, NodeKind::PROJ_IN);
        }
        return pred;
    }

    pub fn predecessors(self, g: &IrGraph) -> &[NodeId] {
        &g.get(self).predecessors
    }

    pub fn info(self, g: &IrGraph) -> String {
        g.get(self).info()
    }

    fn add_predecessor(self, g: &mut IrGraph, node: NodeId) {
        g.get_mut(self).predecessors.push(node);
        g.register_successor(node, self);
    }

    /// For most nodes, it checks for the same node kind and node id
    ///
    /// For const int nodes:
    ///   - the value is compared
    ///
    /// For commutative binary operations:
    ///   - the regular and swapped order of the operations are comapred
    fn eq(self, other: Self, g: &IrGraph) -> bool {
        let node = g.get(self);
        let other = g.get(other);

        if node.kind != other.kind {
            return false;
        }

        match (node.kind, other.kind) {
            (NodeKind::BinaryOp(o1), NodeKind::BinaryOp(o2)) => {
                // conservative approach for binary ops which can cause side effects
                if matches!(o1, BinaryOp::Div | BinaryOp::Mod) {
                    return self == other.id;
                }
                let lhs1 = node.predecessors[BinaryOp::LEFT];
                let lhs2 = other.predecessors[BinaryOp::LEFT];
                let rhs1 = node.predecessors[BinaryOp::RIGHT];
                let rhs2 = other.predecessors[BinaryOp::RIGHT];
                let mut equal = lhs1.eq(lhs2, g) && rhs1.eq(rhs2, g);
                // for commutative operations we check if inverted operations are equal
                if !equal && matches!(o1, BinaryOp::Add | BinaryOp::Mul) {
                    equal = lhs1.eq(rhs1, g) && lhs2.eq(rhs2, g);
                };
                equal
            }
            (NodeKind::ConstInt, NodeKind::ConstInt) => {
                node.block == other.block
                    && (*node.get_data::<i64>().unwrap()) == (*other.get_data::<i64>().unwrap())
            }
            (_, _) => self == other.id,
        }
    }
}

impl NodeId {
    fn phi_append_operand(self, g: &mut IrGraph, operand: NodeId) {
        self.add_predecessor(g, operand);
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum NodeKind {
    Block,
    BinaryOp(BinaryOp),
    ConstInt,
    Phi,
    Return,
    Start,
    Projection(ProjectionInfo),
}

impl NodeKind {
    pub const PROJ_IN: usize = 0;
    pub const RETURN_SIDE_EFFECT: usize = 0;
    pub const RETURN_RESULT: usize = 1;
}
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ProjectionInfo {
    Result,
    SideEffect,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Div,
    Mod,
    Mul,
    Sub,
}

impl BinaryOp {
    pub const LEFT: usize = 0;
    pub const RIGHT: usize = 1;
}

pub trait NodeProvider {
    fn get(&self, node: NodeId) -> &Node;
    fn get_mut(&mut self, node: NodeId) -> &mut Node;
}

impl NodeProvider for IrGraph {
    fn get(&self, node: NodeId) -> &Node {
        self.nodes.get(&node).expect("node to exist")
    }
    fn get_mut(&mut self, node: NodeId) -> &mut Node {
        self.nodes.get_mut(&node).expect("node to exist")
    }
}
