use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::DebugList,
};

use std::fmt::Write;

use interference::InterferenceGraph;
use linked_hash_map::LinkedHashMap;
use tracing::trace;

use crate::ir::{BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider};

use itertools::Itertools;

mod interference {

    use itertools::Itertools;
    use linked_hash_map::LinkedHashMap;
    use linked_hash_set::LinkedHashSet;

    use crate::{
        ir::{IrGraph, NodeId},
        semantic,
    };
    use std::{
        collections::{HashMap, HashSet},
        fmt::Write,
        i64::MAX,
    };

    #[derive(Default, Debug, Clone)]
    pub(super) struct InterferenceGraph {
        edges: HashMap<NodeId, HashSet<NodeId>>,
    }

    impl InterferenceGraph {
        pub fn coloring(&self, graph: &IrGraph) -> LinkedHashMap<NodeId, u32> {
            let max_color = self.max_out_deg() as u32;
            let mut coloring = LinkedHashMap::new();
            let mut used_colors = HashSet::new();
            let ordering = self.maximum_cardinality_search(graph);
            'next_node: for node in ordering {
                for neighbour in self.neighbors(node) {
                    if let Some(color) = coloring.get(&neighbour) {
                        used_colors.insert(*color);
                    }
                }
                for color in 0..=max_color {
                    if !used_colors.contains(&color) {
                        coloring.insert(node, color);
                        used_colors.clear();
                        continue 'next_node;
                    }
                }
                panic!("no more colors available");
            }
            coloring
        }

        fn maximum_cardinality_search(&self, graph: &IrGraph) -> Vec<NodeId> {
            let mut weights = HashMap::<NodeId, i32>::new();
            let mut visited = HashSet::new();
            let mut ordering: Vec<NodeId> = Vec::with_capacity(graph.nodes().len());

            let mut unvisited: LinkedHashSet<NodeId> =
                graph.nodes().keys().into_iter().sorted().copied().collect();

            for _ in 0..unvisited.len() {
                // Select node with maximum weight from unvisited nodes
                let &max_node = unvisited
                    .iter()
                    .max_by_key(|u| weights.get(u).unwrap_or(&0))
                    .expect("Graph should not be empty");

                ordering.push(max_node);
                visited.insert(max_node);
                unvisited.remove(&max_node);

                // Increase weight of unvisited neighbors
                for &neighbor in self.neighbors(max_node).iter() {
                    if !visited.contains(&neighbor) {
                        *weights.entry(neighbor).or_default() += 1;
                    }
                }
            }
            ordering
                .into_iter()
                .filter(|id| id.needs_register(graph))
                .collect()
            // ordering
        }

        fn max_out_deg(&self) -> usize {
            let max = self.edges.values().max_by_key(|set| set.len());
            max.map(|s| s.len()).unwrap_or(0)
        }

        /// Get neighbors of a node
        fn neighbors(&self, v: NodeId) -> HashSet<NodeId> {
            self.edges.get(&v).cloned().unwrap_or_default()
        }
        pub(super) fn add_edge(&mut self, mut n1: NodeId, mut n2: NodeId) {
            if n1 == n2 {
                panic!("interference graph must be irreflexive");
            };
            self.edges.entry(n1).or_default().insert(n2);
            self.edges.entry(n2).or_default().insert(n1);
        }

        pub(super) fn edges(self) -> HashMap<NodeId, HashSet<NodeId>> {
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
            for (from, set) in &self.edges {
                for to in set.iter().filter(|to| from < to) {
                    write!(result, "{from} -> {to}\n").unwrap();
                }
            }
            write!(result, "}}\n").unwrap();
            result
        }
    }
}

#[derive(Debug, Clone)]
pub struct Liveness {
    edges: InterferenceGraph,
}

impl Liveness {
    pub fn coloring(&self, g: &IrGraph) -> LinkedHashMap<NodeId, u32> {
        self.edges.coloring(g)
    }
}

#[derive(Default, Debug)]
struct Facts {
    defs: HashMap<NodeId, HashSet<NodeId>>,
    uses: HashMap<NodeId, HashSet<NodeId>>,
    succs: HashMap<NodeId, HashSet<NodeId>>,
    live_in: HashMap<NodeId, HashSet<NodeId>>,
    live_out: HashMap<NodeId, HashSet<NodeId>>,
}

impl Facts {
    fn def(&self, l: NodeId, x: NodeId) -> bool {
        self.defs.get(&l).is_some_and(|set| set.contains(&x))
    }
    fn r#use(&self, l: NodeId, x: NodeId) -> bool {
        self.defs.get(&l).is_some_and(|set| set.contains(&x))
    }
    fn succ(&self, l: NodeId, x: NodeId) -> bool {
        self.succs.get(&l).is_some_and(|set| set.contains(&x))
    }
    fn live_in(&self, l: NodeId, x: NodeId) -> bool {
        self.live_in.get(&l).is_some_and(|set| set.contains(&x))
    }

    fn add_live_in(&mut self, l: NodeId, x: NodeId) {
        self.live_in.entry(l).or_default().insert(x);
    }
}

impl Facts {
    pub fn generate(g: &IrGraph) -> Self {
        let mut res = Self::default();
        res.gen_facts(g);
        tracing::debug!("completed def, use, succ analysis");
        res.gen_live(g);
        // let end_block = g.end_block();
        // res.gen_live_simple(g, end_block);
        res.generate_live_out(g);
        tracing::trace!("facts\n{res:?}");
        res
    }

    fn gen_live_simple(&mut self, g: &IrGraph, id: NodeId) {
        let node = g.get(id);
        // If used on right side -> live
        match node.kind() {
            NodeKind::BinaryOp(binary_op) => {
                let left = id.predecessor_skip_proj(g, BinaryOp::LEFT);
                let right = id.predecessor_skip_proj(g, BinaryOp::RIGHT);
                self.add_live_in(id, left);
                self.add_live_in(id, right);
            }
            NodeKind::Return => {
                let return_value = id.predecessor_skip_proj(g, NodeKind::RETURN_RESULT);
                self.add_live_in(id, return_value);
            }
            _ => {}
        }

        let is_assigned = matches!(node.kind(), NodeKind::BinaryOp(_) | NodeKind::ConstInt);

        for succ in g.successors(id) {
            let Some(live_in_next_set) = self.live_in.get(&succ).cloned() else {
                continue;
            };
            for live_in_next in live_in_next_set {
                if is_assigned && live_in_next == id {
                    continue;
                }
                self.add_live_in(id, live_in_next);
            }
        }

        if let Some(set) = self.live_in.get(&id) {
            let alive: String = set
                .into_iter()
                .map(|id| id.info(g))
                .intersperse(", ".to_string())
                .collect();
            tracing::trace!("{} live-in {{{alive}}}", id.info(g));
        }

        for &pred in id.predecessors(g) {
            self.gen_live_simple(g, pred);
        }
    }

    fn gen_live(&mut self, g: &IrGraph) {
        // K1: use(l, x) => live(l, x)
        for (l, x) in &self.uses {
            self.live_in.insert(*l, x.clone());
        }

        // K2: live(l', u) and succ(l, l') and (not (def(l, u))

        let mut worklist = self
            .live_in
            .iter()
            .map(|(x, set)| set.iter().map(|y| (*x, *y)))
            .flatten()
            .collect::<VecDeque<_>>();
        while let Some((l_prime, u)) = worklist.pop_front() {
            let succs = self
                .succs
                .iter()
                .filter_map(|(l, _)| {
                    if self.succ(*l, l_prime) {
                        Some(*l)
                    } else {
                        None
                    }
                })
                .collect_vec();
            for l in succs {
                if !self.def(l, u) {
                    if !self.live_in(l, u) {
                        worklist.push_back((l, u));
                        self.add_live_in(l, u);
                    }
                }
            }
        }
    }

    fn generate_live_out(&mut self, g: &IrGraph) {
        let mut live_out = HashMap::<_, HashSet<_>>::default();
        // Transform live_in to live_out
        for (n, set) in &self.live_in {
            for v in set {
                for m in n.predecessors(g) {
                    live_out.entry(*m).or_default().insert(*v);
                }
            }
        }
        self.live_out = live_out;
    }

    fn gen_facts(&mut self, g: &IrGraph) {
        for (id, node) in g.nodes() {
            match node.kind() {
                NodeKind::BinaryOp(binary_op) => {
                    // J1: z <- x ★ y
                    let x = id.predecessor_skip_proj(g, BinaryOp::LEFT);
                    let y = id.predecessor_skip_proj(g, BinaryOp::RIGHT);
                    self.defs.entry(*id).or_default().insert(*id);
                    self.uses.entry(*id).or_default().insert(x);
                    self.uses.entry(*id).or_default().insert(y);

                    // Note: This is described in Sebastian Hack's PhD thesis.
                    //
                    // Lowering from 3-address to 2-address form transforms
                    // an instruction like:
                    //     l: z <- x ★ y
                    // into:
                    //     z <- x
                    //     z ★= y
                    //
                    // According to our liveness rules, x and y are only required to be live at this point
                    // if they are not used later.
                    //
                    // This explanation assumes x ≠ y. If x == y, the problem described below does not occur.
                    //
                    // The register allocator is allowed to reuse the register of either x or y for z, e.g.:
                    //     r0: {z, x}, r1: {y}    or    r0: {z, y}, r1: {x}
                    //
                    // In the first case, the lowering becomes:
                    //     r0 <- r0       ; no-op, can be omitted
                    //     r0 ★= r1       ; correct
                    //
                    // In the second case, it becomes:
                    //     r0 <- r1       ; overwrites y
                    //     r0 ★= r0       ; incorrect: computes x ★ x instead of x ★ y
                    //
                    // For commutative operations (like addition), this can be resolved by swapping operands.
                    // For non-commutative operations (like subtraction), this is not possible (unless rewritten as negation + addition).
                    //
                    // To avoid this, we insert an artificial use of `y` after the current instruction,
                    // ensuring the register allocator assigns z and y to different registers for non-commutative operations.
                    let artifical_use = binary_op == BinaryOp::Sub;
                    let succs = g.successors(*id);
                    for succ in succs {
                        self.succs.entry(*id).or_default().insert(succ);
                        if artifical_use {
                            self.uses.entry(succ).or_default().insert(y);

                        }
                    }
                }
                NodeKind::Return => {
                    // J2: return x
                    let x = id.predecessor_skip_proj(g, NodeKind::RETURN_RESULT);
                    self.uses.entry(*id).or_default().insert(x);
                }
                NodeKind::ConstInt => {
                    // J3: x <- c
                    self.defs.entry(*id).or_default().insert(*id);
                    let succs = g.successors(*id);
                    for succ in succs {
                        self.succs.entry(*id).or_default().insert(succ);
                    }
                }
                NodeKind::Block | NodeKind::Phi | NodeKind::Start | NodeKind::Projection(_) => {
                    // no rules for these blocks
                }
            }
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
        let facts = Facts::generate(g);
        let mut visited = HashSet::default();
        let mut interference = InterferenceGraph::default();
        visited.insert(end_block);
        Self::scan(g, end_block, &facts, &mut visited, &mut interference);
        Self {
            edges: interference,
        }
    }
}

impl Liveness {
    fn scan(
        g: &IrGraph,
        node: NodeId,
        facts: &Facts,
        visited: &mut HashSet<NodeId>,
        interference: &mut InterferenceGraph,
    ) {
        //         Loop over the instructions in the program, and for each one:

        // Let:

        // def(i) = variables defined (assigned) at instruction i

        // live_out(i) = variables live after instruction i

        // For each variable d in def(i):

        // For each variable l in live_out(i):

        // If l ≠ d, then add an undirected edge between d and l

        // This encodes that d and l interfere because d is written while l is still live.

        // Construct Interference graph
        for &id in g.nodes().keys() {
            //     let node = g.get(id);
            //     match node.kind() {
            //         NodeKind::BinaryOp(binary_op) => {

            //             for succ in g.successors(id) {
            //                 let Some(live_in) = facts.live_in.get(&succ).cloned() else {
            //                     continue;
            //                 };
            //                 for alive in live_in {
            //                     if alive != id {
            //                         interference.add_edge(id, alive);
            //                     }
            //                 }
            //             }
            //         }
            //         NodeKind::ConstInt => {
            //             for succ in g.successors(id) {
            //                 let Some(live_in) = facts.live_in.get(&succ).cloned() else {
            //                     continue;
            //                 };
            //                 for alive in live_in {
            //                     if alive != id {
            //                         interference.add_edge(id, alive);
            //                     }
            //                 }
            //             }
            //         }

            //         _ => {}
            //     }
            let Some(defs) = facts.defs.get(&id) else {
                continue;
            };
            let Some(live_out) = facts.live_out.get(&id) else {
                continue;
            };
            for d in defs {
                for l in live_out {
                    if l != d {
                        interference.add_edge(*d, *l);
                    }
                }
            }
        }
    }
}
