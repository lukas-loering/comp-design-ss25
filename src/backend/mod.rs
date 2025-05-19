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
            Register::Rax => "%eax",
            Register::Rbx => "%ebx",
            Register::Rcx => "%ecx",
            Register::Rdx => "%edx",
            Register::Rsi => "%esi",
            Register::Rdi => "%edi",
            Register::Rbp => "%rbp",
            Register::Rsp => "%rsp",
            Register::R8 => "%r8d",
            Register::R9 => "%r9d",
            Register::R10 => "%r10d",
            Register::R11 => "%r11d",
            Register::R12 => "%r12d",
            Register::R13 => "%r13d",
            Register::R14 => "%r14d",
            Register::R15 => "%r15d",
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
            Asm::Movl(l1, l2)
            | Asm::Movq(l1, l2)
            | Asm::Addl(l1, l2)
            | Asm::Subl(l1, l2)
            | Asm::Addq(l1, l2)
            | Asm::Subq(l1, l2) => {
                if l1.is_indirect() && l2.is_indirect() {
                    let spill_reg = self.provider.spill_register().into();
                    self.emit(Asm::Movl(*l1, spill_reg));
                    *l1 = spill_reg;
                    self.emit(instr);
                } else if matches!(l1, Location::Immediate(v) if *v > i64::from(i32::MAX))
                    && l2.is_indirect()
                {
                    self.emit(Asm::Movl(*l1, self.provider.spill_register().into()));
                    *l1 = self.provider.spill_register().into();
                    self.emit(instr);
                } else {
                    self.builder.push(instr);
                }
            }
            Asm::Imull(_) | Asm::Idivl(_) | Asm::Cltd | Asm::Ret | Asm::Pushq(_) | Asm::Popq(_) => {
                self.builder.push(instr);
            }
        };
    }

    fn emit_return(&mut self) {
        self.emit(Asm::Addq(
            Location::Immediate(self.provider.required_stack_size() as i64),
            Register::Rsp.into(),
        ));
        self.emit(Asm::Popq(Register::Rbp.into()));
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
                self.emit(Asm::Movl(Location::Immediate(value), self.reg(node).into()));
            }
            NodeKind::Return => {
                let pred = node.predecessor_skip_proj(self.graph, NodeKind::RETURN_RESULT);
                self.emit(Asm::Movl(self.reg(pred).into(), Register::Rax.into()));
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
                if d == s1 {
                    self.emit(Asm::Addl(s2.into(), d.into()))
                } else if d == s2 {
                    self.emit(Asm::Addl(s1.into(), d.into()))
                } else {
                    self.emit(Asm::Movl(s1.into(), d.into()));
                    self.emit(Asm::Addl(s2.into(), d.into()));
                }
            }
            BinaryOp::Sub => {
                // d <- s1 - s2
                //
                // d <- s1
                // d -= s2
                if d == s1 {
                    self.emit(Asm::Subl(s2.into(), d.into()))
                } else if d == s2 {
                    panic!(
                        "for non-commutative operations, allocationg s2 and d to same register should be forbidden by the register allocator"
                    )
                } else {
                    self.emit(Asm::Movl(s1.into(), d.into()));
                    self.emit(Asm::Subl(s2.into(), d.into()));
                }
            }
            BinaryOp::Mul => {
                // d <- s1 * s2
                // movq %s1 %rax
                // imulq %s2
                // movq %rax d
                self.emit(Asm::Movl(s1.into(), Register::Rax.into()));
                self.emit(Asm::Imull(s2.into()));
                self.emit(Asm::Movl(Register::Rax.into(), d.into()));
            }
            (BinaryOp::Div | BinaryOp::Mod) => {
                // d <- s1 / s2                
                // movq %s1 %rax
                // idvq %s2
                // cqto ; sign extend into rdx
                // movq %rax d  ; quotiend store here
                // movq %rdx d  ; remainder store here

                self.emit(Asm::Movl(s1.into(), Register::Rax.into()));
                self.emit(Asm::Cltd);
                self.emit(Asm::Idivl(s2.into()));

                let result = if binary_op == BinaryOp::Div {
                    Register::Rax.into()
                } else if binary_op == BinaryOp::Mod {
                    Register::Rdx.into()
                } else {
                    unreachable!("not a mod-div operation")
                };
                self.emit(Asm::Movl(result, d.into()));
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
        
        // Save frame base pointer
        self.emit(Asm::Pushq(Register::Rbp.into()));
        // set new frame base pointer
        self.emit(Asm::Movq(Register::Rsp.into(), Register::Rbp.into()));
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
    Movl(Location, Location),
    Addl(Location, Location),
    Addq(Location, Location),
    Subq(Location, Location),
    Subl(Location, Location),
    Imull(Location),
    Idivl(Location),
    Pushq(Location),
    Popq(Location),
    Cltd,
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
            Asm::Movl(s, d) => write!(f, "movl {s}, {d}"),
            Asm::Addq(s, d) => write!(f, "addq {s}, {d}"),
            Asm::Addl(s, d) => write!(f, "addl {s}, {d}"),
            Asm::Subl(s, d) => write!(f, "subl {s}, {d}"),
            Asm::Subq(s, d) => write!(f, "subq {s}, {d}"),
            Asm::Imull(s) => write!(f, "imull {s}"),
            Asm::Idivl(s) => write!(f, "idivl {s}"),
            Asm::Cltd => write!(f, "cltd"),
            Asm::Ret => write!(f, "ret"),
            Asm::Pushq(s) => write!(f, "pushq {s}"),
            Asm::Popq(s) => write!(f, "popq {s}"),
        }
    }
}
