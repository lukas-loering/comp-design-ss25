use std::{
    collections::{HashMap, HashSet},
    fmt::DebugList,
    result,
};

use crate::parser::symbol::Name;

use super::{
    optimize::{OptimizableNode, Optimizer}, BinaryOp, IrGraph, Node, NodeId, NodeKind, NodeProvider, ProjectionInfo
};

pub struct GraphConstructor {
    optimizer: Box<dyn Optimizer>,
    pub graph: IrGraph,
    current_def: HashMap<Name, HashMap<NodeId, NodeId>>,
    incomplete_phis: HashMap<NodeId, HashMap<Name, NodeId>>,
    current_side_effect: HashMap<NodeId, NodeId>,
    incomplete_side_effect_phis: HashMap<NodeId, NodeId>,
    sealed_blocks: HashSet<NodeId>,
    current_block: NodeId,    
}

impl GraphConstructor {
    pub fn new(optimizer: impl Optimizer + 'static, name: String) -> Self {
        let graph = IrGraph::new(name.into());
        let mut c = Self {
            current_block: graph.start_block,
            optimizer: Box::new(optimizer),
            graph,
            current_def: Default::default(),
            incomplete_phis: Default::default(),
            current_side_effect: Default::default(),
            incomplete_side_effect_phis: Default::default(),
            sealed_blocks: Default::default(),
        };
        c.seal_block(c.current_block);
        c
    }

    pub fn write_variable(&mut self, variable: Name, block: NodeId, value: NodeId) {
        self.current_def
            .entry(variable)
            .or_default()
            .insert(block, value);
    }

    pub fn read_variable(&mut self, variable: Name, block: NodeId) -> NodeId {
        let node = self
            .current_def
            .get(&variable)
            .and_then(|def| def.get(&block))
            .copied();
        match node {
            Some(node) => node,
            None => self.read_variable_recursive(variable, block),
        }
    }

    fn read_variable_recursive(&mut self, variable: Name, block: NodeId) -> NodeId {
        let value = if !self.sealed_blocks.contains(&block) {
            let value = self.new_phi();
            self.incomplete_phis
                .entry(block)
                .or_default()
                .insert(variable.clone(), value);
            value
        } else if let Some(previous_block) = block.predecessors(&self.graph).first() {
            self.read_variable(variable.clone(), self.get(*previous_block).block)
        } else {
            let value = self.new_phi();
            self.write_variable(variable.clone(), block, value);
            self.add_phi_operands_vars(variable.clone(), value)
        };
        self.write_variable(variable, block, value);
        value
    }

    fn read_current_side_effect(&mut self) -> NodeId {
        self.read_side_effect(self.current_block)
    }

    fn read_side_effect(&mut self, block: NodeId) -> NodeId {
        let node = self.current_side_effect.get(&block).copied();
        match node {
            Some(node) => node,
            None => self.read_side_effect_recursive(block),
        }
    }

    fn read_side_effect_recursive(&mut self, block: NodeId) -> NodeId {
        let mut val;
        if !self.sealed_blocks.contains(&block) {
            val = self.new_phi();
            let old = self.incomplete_side_effect_phis.insert(block, val);
            assert!(
                old.is_none(),
                "double read_side_effect_recursive for {block:?}"
            );
        } else if let Some(previous_block) = block.predecessors(&self.graph).first() {
            val = self.read_side_effect(self.get(*previous_block).block);
        } else {
            val = self.new_phi();
            self.write_side_effect(block, val);
            val = self.add_phi_operands_side_effects(val);
        }
        val
    }

    pub fn write_current_side_effect(&mut self, node: NodeId) {
        self.write_side_effect(self.current_block, node);
    }

    fn write_side_effect(&mut self, block: NodeId, node: NodeId) {
        self.current_side_effect.insert(block, node);
    }

    fn add_phi_operands_vars(&mut self, variable: Name, phi: NodeId) -> NodeId {
        for pred in self.get(self.get(phi).block).predecessors.clone() {
            let block = self.get(pred).block;
            let operand = self.read_variable(variable.clone(), block);
            phi.phi_append_operand(&mut self.graph, operand);
        }
        self.remove_trivial_phis(phi)
    }

    fn add_phi_operands_side_effects(&mut self, phi: NodeId) -> NodeId {
        for pred in self.get(self.get(phi).block).predecessors.clone() {
            let block = self.get(pred).block;
            let operand = self.read_side_effect(block);
            phi.phi_append_operand(&mut self.graph, operand);
        }
        self.remove_trivial_phis(phi)
    }

    fn remove_trivial_phis(&mut self, phi: NodeId) -> NodeId {
        // NOTE: the paper shows how to remove trivial phis.
        // as this is not a problem in Lab 1 and it is just
        // a simplification, we recommend to implement this
        // part yourself.
        phi
    }

    pub fn current_block(&self) -> NodeId {
        self.current_block
    }

    fn seal_block(&mut self, block: NodeId) {
        let map = self.incomplete_phis.get(&block);
        if let Some(map) = map {
            for (name, node_id) in map.clone() {
                self.add_phi_operands_vars(name.clone(), node_id);
            }
        }
        self.sealed_blocks.insert(block);
    }
}

impl GraphConstructor {  

    pub fn new_phi(&mut self) -> NodeId {
        let node = Node::new_node(&mut self.graph, NodeKind::Phi, self.current_block, &[]);
        self.optimizer.transform(node)
    }
    pub fn new_sub(&mut self, left: NodeId, right: NodeId) -> NodeId {
        let node = Node::new_node(
            &mut self.graph,
            NodeKind::BinaryOp(BinaryOp::Sub),
            self.current_block,
            &[left, right],
        );
        self.optimizer.transform(node)
    }
    pub fn new_add(&mut self, left: NodeId, right: NodeId) -> NodeId {
        let node = Node::new_node(
            &mut self.graph,
            NodeKind::BinaryOp(BinaryOp::Add),
            self.current_block,
            &[left, right],
        );
        self.optimizer.transform(node)
    }
    pub fn new_mul(&mut self, left: NodeId, right: NodeId) -> NodeId {
        let node = Node::new_node(
            &mut self.graph,
            NodeKind::BinaryOp(BinaryOp::Mul),
            self.current_block,
            &[left, right],
        );
        self.optimizer.transform(node)
    }
    pub fn new_div(&mut self, left: NodeId, right: NodeId) -> NodeId {
        let side_effect = self.read_current_side_effect();
        let node = Node::new_node(
            &mut self.graph,
            NodeKind::BinaryOp(BinaryOp::Div),
            self.current_block,
            &[left, right, side_effect],
        );
        self.optimizer.transform(node)
    }
    pub fn new_mod(&mut self, left: NodeId, right: NodeId) -> NodeId {
        let side_effect = self.read_current_side_effect();
        let node = Node::new_node(
            &mut self.graph,
            NodeKind::BinaryOp(BinaryOp::Mod),
            self.current_block,
            &[left, right, side_effect],
        );
        self.optimizer.transform(node)
    }

    pub fn new_start(&mut self) -> NodeId {
        assert!(
            self.current_block == self.graph.start_block,
            "start node must be start block"
        );
        Node::new_node(&mut self.graph, NodeKind::Start, self.current_block, &[]).accept()
    }

    pub fn new_const_int(&mut self, value: i64) -> NodeId {
        let start_block = self.graph.start_block;
        let node = Node::new_node_with_data(&mut self.graph, NodeKind::ConstInt, start_block, value,&[]);
        self.optimizer.transform(node)        
        
    }

    pub fn new_side_effect_proj(&mut self, node: NodeId) -> NodeId {
        Node::new_node(
            &mut self.graph,
            NodeKind::Projection(ProjectionInfo::SideEffect),
            self.current_block,
            &[node],
        ).accept()
    }

    pub fn new_result_proj(&mut self, node: NodeId) -> NodeId {
        Node::new_node(
            &mut self.graph,
            NodeKind::Projection(ProjectionInfo::Result),
            self.current_block,
            &[node],
        ).accept()
    }

    pub fn new_return(&mut self, result: NodeId) -> NodeId {
        let side_effect = self.read_current_side_effect();
        Node::new_node(
            &mut self.graph,
            NodeKind::Return,
            self.current_block,
            &[side_effect, result],
        ).accept()
    }
}

impl AsMut<IrGraph> for &mut GraphConstructor {
    fn as_mut(&mut self) -> &mut IrGraph {
        &mut self.graph
    }
}

impl NodeProvider for GraphConstructor {
    fn get(&self, node: NodeId) -> &Node {
        self.graph.get(node)
    }

    fn get_mut(&mut self, node: NodeId) -> &mut Node {
        self.graph.get_mut(node)
    }
}
