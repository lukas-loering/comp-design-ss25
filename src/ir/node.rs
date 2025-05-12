// use std::collections::HashMap;

// use super::IrGraph;


// #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
// pub struct NodeId(u32);

// impl NodeId {
//     pub const NULL: Self = NodeId(0);
//     pub const FIRST: Self = NodeId(1);

//     pub fn next(mut self) -> Self {
//         self.0 = self
//             .0
//             .checked_add(1)
//             .expect("can not create more than 4,294,967,295 IR nodes");
//         self
//     }
// }

// #[derive(Debug)]
// pub struct Node {
//     id: NodeId,    
//     kind: NodeKind,
// }

// impl Node {
//     pub fn new(id: NodeId, kind: NodeKind) -> Self {
//         Self { id, kind }
//     }

//     fn new_node(g: &mut IrGraph, block: NodeId, predecessors: Vec<NodeId>) -> NodeId {
//         g.no
//     }

//     pub fn new_phi(g: AsMut<IrGraph>, block: NodeId) -> NodeId {
//         g.as_mut().
//     }
// }

// #[derive(Debug, Copy, Clone)]
// pub enum NodeKind {
//     Block,
//     BinaryOp,
//     ConstInt,
//     Phi,
//     Return,
//     Start,
//     Projection,
// }

// #[derive(Debug, Copy, Clone)]
// pub enum BinaryOp {
//     Add,
//     Div,
//     Mod,
//     Mul,
//     Sub,
// }

// impl BinaryOp {
//     const LEFT: usize = 0;
//     const RIGHT: usize = 1;
// }
