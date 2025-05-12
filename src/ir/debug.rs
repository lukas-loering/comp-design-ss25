use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use tracing_subscriber::fmt::format;

use super::{IrGraph, NodeId, NodeKind, NodeProvider};

pub struct GraphVizPrinter<'g> {
    clusters: HashMap<NodeId, HashSet<NodeId>>,
    edges: Vec<Edge>,
    builder: String,
    graph: &'g IrGraph,
}

impl<'g> GraphVizPrinter<'g> {
    pub fn print(graph: &'g IrGraph) -> String {
        let mut viz = GraphVizPrinter::new(graph);
        let mut seen = HashSet::new();
        viz.prepare(graph.end_block, &mut seen);
        viz.print_impl();
        viz.builder
    }

    fn new(graph: &'g IrGraph) -> GraphVizPrinter<'g> {
        Self {
            graph,
            edges: Default::default(),
            builder: Default::default(),
            clusters: Default::default(),
        }
    }

    fn prepare(&mut self, id: NodeId, seen: &mut HashSet<NodeId>) {
        if !seen.insert(id) {
            return;
        }
        let node = self.graph.get(id);
        if node.kind != NodeKind::Block {
            self.clusters.entry(node.block).or_default().insert(id);
        }
        let mut idx = 0;
        for pred in &node.predecessors {
            self.edges.push(Edge {
                from: *pred,
                to: id,
                idx: idx,
            });
            idx += 1;
            self.prepare(*pred, seen);
        }
        if id == self.graph.end_block {
            self.clusters
                .insert(self.graph.end_block, Default::default());
        }
    }

    fn print_impl(&mut self) {
        write!(
            self.builder,
            "digraph \"{}\" {{\n\
                compound=true;\n\
                layout=dot;\n\
                node [shape=box];\n\
                splines=ortho;\n\
                overlap=false;\n\n",
            self.graph.name()
        )
        .unwrap();

        for (block, nodes) in &self.clusters {
            write!(
                self.builder,
                "    subgraph cluster_{} {{\n\
                        c_{} [width=0, height=0, fixedsize=true, style=invis];\n",
                block, block,
            )
            .unwrap();

            if *block == self.graph.end_block {
                write!(self.builder, "        label=End;\n").unwrap();
            }

            for node in nodes {
                write!(
                    self.builder,
                    "        {} [label=\"{}\"",
                    node,
                    self.label_for(*node)
                )
                .unwrap();

                // if let Some(span) = node.debug_info().and_then(|info| info.source_span()) {
                //     write!(self.builder, ", tooltip=\"source span: {}\"", span).unwrap();
                // }

                writeln!(self.builder, "];").unwrap();
            }

            writeln!(self.builder, "    }}\n").unwrap();
        }

        for edge in &self.edges {
            write!(
                self.builder,
                "    {} -> {} [label={}",
                self.name_for(edge.from),
                self.name_for(edge.to),
                edge.idx
            )
            .unwrap();

            let is_from_block = self.graph.get(edge.from).kind == NodeKind::Block;
            let is_to_block = self.graph.get(edge.to).kind == NodeKind::Block;

            if is_from_block {
                write!(self.builder, ", ltail=cluster_{}", edge.from).unwrap();
            }

            if is_to_block {
                write!(self.builder, ", lhead=cluster_{}", edge.to).unwrap();
            }

            writeln!(self.builder, "];").unwrap();
        }

        writeln!(self.builder, "}}").unwrap();
    }

    fn label_for(&self, node: NodeId) -> String {
        let node = self.graph.get(node);
        node.info()
    }

    fn name_for(&self, node: NodeId) -> String {
        let kind = self.graph.get(node).kind;
        if kind == NodeKind::Block {
            format!("c{node}")
        } else {
            format!("{node}")
        }
    }
}

struct Edge {
    from: NodeId,
    to: NodeId,
    idx: usize,
}
