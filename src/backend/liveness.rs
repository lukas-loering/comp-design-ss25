use std::{
    collections::{HashMap, HashSet},
    fmt::DebugList,
};

use std::fmt::Write;

use linked_hash_map::LinkedHashMap;
use tracing::trace;

use crate::ir::{BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider};

use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct Liveness {
    live_at: LinkedHashMap<NodeId, HashSet<NodeId>>,
}

impl Liveness {
    pub fn show(&self) -> String {
        let mut result = String::new();
        for (node, live) in &self.live_at {
            let live: String = live
                .into_iter()
                .map(|n| format!("{n:2}"))
                .intersperse_with(|| ", ".to_string())
                .collect();
            write!(result, "{node:2} [{live}]\n").unwrap();
        }
        result
    }
}

impl Liveness {
    #[tracing::instrument(skip(g))]
    pub fn generate(g: &IrGraph) -> Self {
        let end_block = g.end_block();
        let mut live_at = LinkedHashMap::default();
        let mut visited = HashSet::default();
        visited.insert(end_block);
        Self::scan(g, end_block, &mut live_at, &mut visited);
        Self { live_at }
    }
}

impl Liveness {
    fn scan(
        g: &IrGraph,
        node: NodeId,
        live_at: &mut LinkedHashMap<NodeId, HashSet<NodeId>>,
        visited: &mut HashSet<NodeId>,
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
        live_at.insert(node, live);

        // Walk Up
        for pred in node.predecessors(g) {
            if (visited.insert(*pred)) {
                Self::scan(g, *pred, live_at, visited);
            }
        }
    }
}
