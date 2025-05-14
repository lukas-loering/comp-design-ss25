use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write, write},
};

use liveness::Liveness;
use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    ir::{BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider},
    parser::ast::{AssignmentTree, BlockTree, ExpressionTree, NameTree, StatementTree},
};

mod liveness;

pub trait CodeGenerator {
    fn generate(self) -> Result<String, Box<dyn std::error::Error>>;
}

pub trait RegisterProvider {
    fn allocate(&mut self, graph: &IrGraph) -> Result<(), Box<dyn std::error::Error>>;
    fn register(&self, node: NodeId) -> Register;
}

#[derive(Default)]
pub struct TrivialRegisterProvider {
    map: HashMap<NodeId, Register>,
}

impl RegisterProvider for TrivialRegisterProvider {
    fn register(&self, node: NodeId) -> Register {
        *self
            .map
            .get(&node)
            .expect(&format!("node {node} should have a register"))
    }

    fn allocate(&mut self, graph: &IrGraph) -> Result<(), Box<dyn std::error::Error>> {
        let mut visited = HashSet::new();
        visited.insert(graph.end_block());
        self.scan(graph, graph.end_block(), &mut visited)?;
        Ok(())
    }
}

impl TrivialRegisterProvider {
    fn scan(
        &mut self,
        graph: &IrGraph,
        node: NodeId,
        visited: &mut HashSet<NodeId>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        for pred in node.predecessors(graph) {
            if (visited.insert(*pred)) {
                self.scan(graph, *pred, visited)?;
            }
        }
        if self.needs_register(graph, node) {
            let next = u8::try_from(u8::from(Register::MIN) + self.map.len() as u8)?;
            let register = Register::try_from(next)?;
            // we just cheked that this register is unused
            if self.map.insert(node, register).is_some() {
                panic!("node already had a register")
            };
        }
        Ok(())
    }

    fn needs_register(&self, graph: &IrGraph, node: NodeId) -> bool {
        let kind = graph.get(node).kind();
        let no_register_needed = matches!(
            kind,
            NodeKind::Projection(_) | NodeKind::Start | NodeKind::Block | NodeKind::Return
        );
        !no_register_needed
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register {
    const MIN: Self = Register::R8;
    const MAX: Self = Register::R15;
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Register::Rax => "%rax",
            Register::Rbx => "%rbx",
            Register::Rcx => "%rcx",
            Register::Rdx => "%rdx",
            Register::Rsi => "%rsi",
            Register::Rdi => "%rdi",
            Register::Rbp => "%rbp",
            Register::Rsp => "%rsp",
            Register::R8 => "%r8",
            Register::R9 => "%r9",
            Register::R10 => "%r10",
            Register::R11 => "%r11",
            Register::R12 => "%r12",
            Register::R13 => "%r13",
            Register::R14 => "%r14",
            Register::R15 => "%r15",
        };
        write!(f, "{name}")
    }
}

pub struct X86_64Asm<'g, P>
where
    P: RegisterProvider,
{
    graph: &'g IrGraph,
    provider: P,
    builder: Vec<Asm>,
}

impl<'g, P> X86_64Asm<'g, P>
where
    P: RegisterProvider,
{
    pub fn new(graph: &'g IrGraph, provider: P) -> Self {
        Self {
            graph,
            provider,
            builder: Default::default(),
        }
    }

    fn emit(&mut self, instr: Asm) {
        self.builder.push(instr);
    }

    fn scan(&mut self, node: NodeId, visited: &mut HashSet<NodeId>) {
        for pred in node.predecessors(self.graph) {
            if visited.insert(*pred) {
                self.scan(*pred, visited);
            }
        }
        match self.graph.get(node).kind() {
            NodeKind::BinaryOp(binary_op) => self.binary(binary_op, node),
            NodeKind::ConstInt => {
                let value = *self.graph.get(node).get_data::<i64>().unwrap();
                self.emit(Asm::Movq(Source::Immediate(value), self.reg(node).into()));
            }
            NodeKind::Return => {
                let pred = node.predecessor_skip_proj(self.graph, NodeKind::RETURN_RESULT);
                self.emit(Asm::Movq(self.reg(pred).into(), Register::Rax.into()));
                self.emit(Asm::Ret);
            }
            NodeKind::Phi => panic!("phi not valid in code_gen"),
            _ => {}
        };
    }

    fn reg(&self, node: NodeId) -> Register {
        self.provider.register(node)
    }

    fn binary(&mut self, binary_op: BinaryOp, node: NodeId) {
        let s1 = self.reg(node.predecessor_skip_proj(self.graph, BinaryOp::LEFT));
        let s2 = self.reg(node.predecessor_skip_proj(self.graph, BinaryOp::RIGHT));
        let d = self.reg(node);

        match binary_op {
            BinaryOp::Add => {
                // d <- s1 + s2
                self.emit(Asm::Movq(s1.into(), d));
                self.emit(Asm::Addq(s2.into(), d));
            }
            BinaryOp::Sub => {
                self.emit(Asm::Movq(s1.into(), d));
                self.emit(Asm::Subq(s2.into(), d));
            }
            BinaryOp::Mul => {
                // d <- s1 * s2
                // movq %s1 %rax
                // imulq %s2
                // movq %rax d
                self.emit(Asm::Movq(s1.into(), Register::Rax.into()));
                self.emit(Asm::Imulq(s2.into()));
                self.emit(Asm::Movq(Register::Rax.into(), d.into()));
            }
            (BinaryOp::Div | BinaryOp::Mod) => {
                // d <- s1 / s2
                // movq %s1 %rax
                // idvq %s2
                // cqto ; sign extend into rdx
                // movq %rax d  ; quotiend store here
                // movq %rdx d  ; remainder store here

                self.emit(Asm::Movq(s1.into(), Register::Rax.into()));
                self.emit(Asm::Cqto);
                self.emit(Asm::Idivq(s2.into()));

                let result = if binary_op == BinaryOp::Div {
                    Register::Rax.into()
                } else if binary_op == BinaryOp::Mod {
                    Register::Rdx.into()
                } else {
                    unreachable!("not a mod-div operation")
                };
                self.emit(Asm::Movq(result, d.into()));
            }
        };
    }
}

impl<'g, P> CodeGenerator for X86_64Asm<'g, P>
where
    P: RegisterProvider,
{
    fn generate(mut self) -> Result<String, Box<dyn std::error::Error>> {
        let liveness = Liveness::generate(self.graph);
        tracing::debug!("{}", liveness.show(self.graph));
        self.provider.allocate(self.graph)?;
        let mut code = String::new();
        let mut visited = HashSet::new();
        self.scan(self.graph.end_block(), &mut visited);
        write!(code, "_{}:\n", self.graph.name())?;
        write!(
            code,
            "{}",
            self.builder
                .into_iter()
                .map(|instr| format!("  {instr}\n"))
                .collect::<String>()
        );
        Ok(code)
    }
}

#[derive(Debug)]
enum Asm {
    Movq(Source, Register),
    Addq(Source, Register),
    Subq(Source, Register),
    Imulq(Source),
    Idivq(Source),
    Cqto,
    Ret,
}

#[derive(Debug)]
enum Source {
    Register(Register),
    Immediate(i64),
}

impl From<Register> for Source {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Source::Register(register) => write!(f, "{register}"),
            Source::Immediate(v) => write!(f, "${v}"),
        }
    }
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Asm::Movq(s, d) => write!(f, "movq {s}, {d}"),
            Asm::Addq(s, d) => write!(f, "addq {s}, {d}"),
            Asm::Subq(s, d) => write!(f, "subq {s}, {d}"),
            Asm::Imulq(s) => write!(f, "imulq {s}"),
            Asm::Idivq(s) => write!(f, "idivq {s}"),
            Asm::Cqto => write!(f, "cqto"),
            Asm::Ret => write!(f, "ret"),
        }
    }
}
