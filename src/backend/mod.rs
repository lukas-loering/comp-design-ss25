use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write, write},
};

use liveness::Liveness;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use regalloc::Alloc;
use strum::EnumIter;

use crate::{
    ir::{BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider},
    parser::ast::{AssignmentTree, BlockTree, ExpressionTree, NameTree, StatementTree},
};

pub use regalloc::RegAlloc;
mod liveness;
mod regalloc;

pub trait CodeGenerator {
    fn generate(self) -> Result<String, Box<dyn std::error::Error>>;
}

pub trait RegisterProvider {
    fn allocate(&mut self, graph: &IrGraph) -> Result<(), Box<dyn std::error::Error>>;
    fn register(&self, node: NodeId) -> Alloc;
    fn spill_register(&self) -> Alloc;
    fn required_stack_size(&self) -> usize {
        0
    }
}

#[derive(Default)]
pub struct TrivialRegisterProvider {
    map: HashMap<NodeId, Register>,
}

impl RegisterProvider for TrivialRegisterProvider {
    fn register(&self, node: NodeId) -> Alloc {
        let r = *self
            .map
            .get(&node)
            .expect(&format!("node {node} should have a register"));
        Alloc::Register(r)
    }

    fn allocate(&mut self, graph: &IrGraph) -> Result<(), Box<dyn std::error::Error>> {
        let mut visited = HashSet::new();
        visited.insert(graph.end_block());
        self.scan(graph, graph.end_block(), &mut visited)?;
        Ok(())
    }

    fn spill_register(&self) -> Alloc {
        panic!("this dont spill")
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
        if node.needs_register(graph) {
            let next = u8::try_from(u8::from(Register::MIN) + self.map.len() as u8)?;
            let register = Register::try_from(next)?;
            // we just cheked that this register is unused
            if self.map.insert(node, register).is_some() {
                panic!("node already had a register")
            };
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, TryFromPrimitive, IntoPrimitive, EnumIter)]
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

    fn emit(&mut self, mut instr: Asm) {
        match &mut instr {
            Asm::Movq(l1, l2) | Asm::Addq(l1, l2) | Asm::Subq(l1, l2) => {
                if l1.is_indirect() && l2.is_indirect() {
                    let spill_reg = self.provider.spill_register().into();
                    self.emit(Asm::Movq(*l1, spill_reg));
                    *l1 = spill_reg;
                    self.emit(instr);
                } else if matches!(l1, Location::Immediate(v) if *v > i64::from(i32::MAX))
                    && l2.is_indirect()
                {
                    self.emit(Asm::Movq(*l1, self.provider.spill_register().into()));
                    *l1 = self.provider.spill_register().into();
                    self.emit(instr);
                } else {
                    self.builder.push(instr);
                }
            }
            Asm::Imulq(_) | Asm::Idivq(_) | Asm::Cqto | Asm::Ret => {
                self.builder.push(instr);
            }
        };
    }

    fn emit_return(&mut self) {
        self.emit(Asm::Addq(
            Location::Immediate(self.provider.required_stack_size() as i64),
            Register::Rsp.into(),
        ));
        self.emit(Asm::Ret);
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
                self.emit(Asm::Movq(Location::Immediate(value), self.reg(node).into()));
            }
            NodeKind::Return => {
                let pred = node.predecessor_skip_proj(self.graph, NodeKind::RETURN_RESULT);
                self.emit(Asm::Movq(self.reg(pred).into(), Register::Rax.into()));
                self.emit_return();
            }
            NodeKind::Phi => panic!("phi not valid in code_gen"),
            _ => {}
        };
    }

    fn reg(&self, node: NodeId) -> Alloc {
        self.provider.register(node)
    }

    fn binary(&mut self, binary_op: BinaryOp, node: NodeId) {
        let s1 = self.reg(node.predecessor_skip_proj(self.graph, BinaryOp::LEFT));
        let s2 = self.reg(node.predecessor_skip_proj(self.graph, BinaryOp::RIGHT));
        let d = self.reg(node);

        match binary_op {
            BinaryOp::Add => {
                // d <- s1 + s2
                // if s2 and d have same register this breaks
                self.emit(Asm::Movq(s2.into(), self.provider.spill_register().into()));
                self.emit(Asm::Movq(s1.into(), d.into()));
                self.emit(Asm::Addq(self.provider.spill_register().into(), d.into()));
            }
            BinaryOp::Sub => {
                self.emit(Asm::Movq(s2.into(), self.provider.spill_register().into()));
                self.emit(Asm::Movq(s1.into(), d.into()));
                self.emit(Asm::Subq(self.provider.spill_register().into(), d.into()));
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
        self.provider.allocate(self.graph)?;
        tracing::info!("completed register allocation");
        let mut code = String::new();
        let mut visited = HashSet::new();
        // subq $80, %rsp
        self.emit(Asm::Subq(
            Location::Immediate(self.provider.required_stack_size() as i64),
            Register::Rsp.into(),
        ));
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
        tracing::info!("completed code generation");
        Ok(code)
    }
}

#[derive(Debug, Copy, Clone)]
enum Asm {
    Movq(Location, Location),
    Addq(Location, Location),
    Subq(Location, Location),
    Imulq(Location),
    Idivq(Location),
    Cqto,
    Ret,
}

#[derive(Debug, Clone, Copy)]
enum Location {
    Register(Register),
    Indirect(Register),
    IndirectDisplacement(usize, Register),
    Immediate(i64),
}

impl Location {
    pub fn is_indirect(&self) -> bool {
        matches!(
            self,
            Location::Indirect(_) | Location::IndirectDisplacement(_, _)
        )
    }
}

impl From<Register> for Location {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Register(register) => write!(f, "{register}"),
            Location::Immediate(v) => write!(f, "${v}"),
            Location::Indirect(register) => write!(f, "({register})"),
            Location::IndirectDisplacement(offset, register) => write!(f, "{offset}({register})"),
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
