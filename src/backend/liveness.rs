use std::{
    collections::{HashMap, HashSet},
    fmt::DebugList,
};

use std::fmt::Write;

use interference::InterferenceGraph;
use linked_hash_map::LinkedHashMap;
use tracing::trace;

use crate::ir::{BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider};

use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct Liveness {
    live_at: LinkedHashMap<NodeId, HashSet<NodeId>>,
    edges: InterferenceGraph,
}

mod interference {

    use crate::ir::{IrGraph, NodeId};
    use std::fmt::Write;

    #[derive(Default, Debug, Clone)]
    pub(super) struct InterferenceGraph {
        edges: Vec<(NodeId, NodeId)>,
    }

    impl InterferenceGraph {
        pub(super) fn add_edge(&mut self, mut n1: NodeId, mut n2: NodeId) {
            if n1 == n2 {
                panic!("interference graph must be irreflexive");
            } else if n2 > n1 {
                let tmp = n1;
                n1 = n2;
                n2 = tmp;
            }
            if !self.edges.contains(&(n1, n2)) {
                self.edges.push((n1, n2));
            }
        }

        pub(super) fn edges(self) -> Vec<(NodeId, NodeId)> {
            self.edges
        }

        pub(super) fn graph_viz(&self, g: &IrGraph) -> String {
            let mut result = r#"digraph "main" {
compound=true;
layout=dot;
node [shape=box];
splines=ortho;
overlap=false;
edge [arrowhead="none"];"#
                .to_string();
            writeln!(result);
            for (id, node) in g.nodes() {
                write!(result, "{id} [label=\"{}\"]\n", node.info()).unwrap();
            }
            for (from, to) in &self.edges {
                write!(result, "{from} -> {to}\n").unwrap();
            }
            write!(result, "}}\n").unwrap();
            result
        }
    }
}

impl Liveness {
    pub fn show(&self, g: &IrGraph) -> String {
        self.edges.graph_viz(g)
        // let mut result = String::new();
        // for (node, live) in &self.live_at {
        //     let live: String = live
        //         .into_iter()
        //         .map(|n| format!("{n:2}"))
        //         .intersperse_with(|| ", ".to_string())
        //         .collect();
        //     write!(result, "{node:2} [{live}]\n").unwrap();
        // }
        // result
    }
}

impl Liveness {
    #[tracing::instrument(skip(g))]
    pub fn generate(g: &IrGraph) -> Self {
        let end_block = g.end_block();
        let mut live_at = LinkedHashMap::default();
        let mut visited = HashSet::default();
        let mut interference = InterferenceGraph::default();
        visited.insert(end_block);
        Self::scan(g, end_block, &mut live_at, &mut visited, &mut interference);
        Self {
            live_at,
            edges: interference,
        }
    }
}

impl Liveness {
    fn scan(
        g: &IrGraph,
        node: NodeId,
        live_at: &mut LinkedHashMap<NodeId, HashSet<NodeId>>,
        visited: &mut HashSet<NodeId>,
        interference: &mut InterferenceGraph,
    ) {
        let mut live = HashSet::new();
        // Inference Rule II:
        // A variable is live, if it is that is used on the right-hand side of an instruction
        match g.get(node).kind() {
            NodeKind::BinaryOp(binary_op) => {
                let lhs = node.predecessor_skip_proj(g, BinaryOp::LEFT);
                let rhs = node.predecessor_skip_proj(g, BinaryOp::RIGHT);
                trace!("(R2) {} live at {}", lhs.info(g), node.info(g));
                trace!("(R2) {} live at {}", rhs.info(g), node.info(g));
                live.insert(lhs);
                live.insert(rhs);
            }
            NodeKind::Return => {
                let pred = node.predecessor_skip_proj(g, NodeKind::RETURN_RESULT);
                trace!("(R2) {} live at {}", pred.info(g), node.info(g));
                live.insert(pred);
            }
            NodeKind::ConstInt
            | NodeKind::Phi
            | NodeKind::Block
            | NodeKind::Start
            | NodeKind::Projection(_) => {
                // no assignments of variables
            }
        }

        // Inference Rule I:
        // A variable is live on line `l`, if a variable is live on `l + 1`` and is not assigned on line `l`
        for succ in g.successors(node) {
            if let Some(next_line) = live_at.get(&succ) {
                for live_node in next_line {
                    if *live_node != node {
                        trace!("(R1) {} live at {}", live_node.info(g), node.info(g));
                        live.insert(*live_node);
                    }
                }
            }
        }

        // Construct Interference graph
        match g.get(node).kind() {
            NodeKind::BinaryOp(binary_op) => {
                // For an t ← s1 ⊕ s2 instruction we create an edge between t and any different
                // variable ti ̸= t live after this line, i.e., live-in at the successor. t and ti should
                // be assigned to different registers, because otherwise the assignment to t could
                // destroy the proper contents of ti that are live-in after, so still needed.

                for succ in g.successors(node) {
                    if let Some(next_line) = live_at.get(&succ) {
                        for live_node in next_line {
                            if *live_node != node {
                                interference.add_edge(*live_node, node);
                            }
                        }
                    }
                }
            }
            NodeKind::ConstInt => {
                // For a t ← s instruction (move) we create an edge between t and any variable ti live after this line that is different from t and s.
                for succ in g.successors(node) {
                    if let Some(next_line) = live_at.get(&succ) {
                        for live_node in next_line {
                            if *live_node != node {
                                interference.add_edge(*live_node, node);
                            }
                        }
                    }
                }
            }
            NodeKind::Return => {
                // For a t ← s instruction (move) we create an edge between t and any variable ti live after this line that is different from t and s.
                let pred = node.predecessor_skip_proj(g, NodeKind::RETURN_RESULT);
                for succ in g.successors(node) {
                    if let Some(next_line) = live_at.get(&succ) {
                        for live_node in next_line {
                            if *live_node != node && *live_node != pred {
                                interference.add_edge(*live_node, node);
                            }
                        }
                    }
                }
            }
            NodeKind::Block | NodeKind::Phi | NodeKind::Start | NodeKind::Projection(_) => {
                // no rule here
            }
        }

        live_at.insert(node, live);

        // Walk Up
        for pred in node.predecessors(g) {
            if (visited.insert(*pred)) {
                Self::scan(g, *pred, live_at, visited, interference);
            }
        }
    }
}
