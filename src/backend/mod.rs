use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write},
};

use num_enum::TryFromPrimitive;

use crate::{
    ir::{BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider},
    parser::ast::{AssignmentTree, BlockTree, ExpressionTree, NameTree, StatementTree},
};

// #[derive(Debug, Clone, Default)]
// pub struct CodeGen {
//     reg_map: HashMap<VarName, Register>,
//     next_reg: Register,
//     code: Vec<String>,
// }

// impl CodeGen {
//     pub fn generate(ast: &BlockTree) -> Vec<String> {
//         let mut code_gen = Self::default();
//         code_gen.gen_block(ast.statements());
//         code_gen.code
//     }

//     fn gen_block(&mut self, statements: &[StatementTree]) {
//         for statement in statements {
//             self.gen_statement(statement);
//         }
//     }

//     fn gen_statement(&mut self, statement: &StatementTree) {
//         match statement {
//             StatementTree::AssignmentTree(assignment_tree) => {
//                 let op = assignment_tree.operator();
//                 let rhs = self.gen_expr(assignment_tree.expression());
//                 let lhs = self.lookup_var(assignment_tree.lvalue().name());
//                 self.emit(format!("{lhs} {op} {rhs}"));
//             }
//             StatementTree::BlockTree(block_tree) => self.gen_block(block_tree.statements()),
//             StatementTree::DeclarationTree(declaration_tree) => {
//                 let r = self.fresh_reg();
//                 self.assign_var(declaration_tree.name().into(), r);
//                 if let Some(initializer) = declaration_tree.initializer() {
//                     let r2 = self.gen_expr(initializer);
//                     self.emit(format!("{r} = {r2}"));
//                 }
//             }
//             StatementTree::ReturnTree(return_tree) => {
//                 let r = self.gen_expr(return_tree.expr());
//                 self.emit(format!("ret {r}"));
//             }
//         }
//     }

//     fn gen_expr(&mut self, expr: &ExpressionTree) -> Register {
//         match expr {
//             ExpressionTree::BinaryOpTree(binary_op_tree) => {
//                 let op = binary_op_tree.operator_kind();
//                 let r1 = self.gen_expr(binary_op_tree.lhs());
//                 let r2 = self.gen_expr(binary_op_tree.rhs());
//                 let r = self.fresh_reg();
//                 self.emit(format!("{r} = {op} {r1} {r2}"));
//                 r
//             }
//             ExpressionTree::IdentExprTree(ident_expr_tree) => {
//                 self.lookup_var(ident_expr_tree.name())
//             }
//             ExpressionTree::LiteralTree(literal_tree) => {
//                 let r = self.fresh_reg();
//                 self.emit(format!("{r} = {}", literal_tree.value()));
//                 r
//             }
//             ExpressionTree::NegateTree(negate_tree) => {
//                 let op = negate_tree.operator_kind();
//                 let r1 = self.gen_expr(negate_tree.expr_tree());
//                 let r = self.fresh_reg();
//                 self.emit(format!("{r} = {op} {r1}"));
//                 r
//             }
//         }
//     }

//     fn emit(&mut self, instruction: String) {
//         self.code.push(instruction);
//     }

//     fn fresh_reg(&mut self) -> Register {
//         let fresh = self.next_reg;
//         self.next_reg = Register(self.next_reg.0 + 1);
//         fresh
//     }

//     fn assign_var(&mut self, var_name: VarName, reg: Register) {
//         self.reg_map.insert(var_name, reg);
//     }

//     fn lookup_var(&mut self, name: impl Into<VarName>) -> Register {
//         *self
//             .reg_map
//             .get(&name.into())
//             .expect("unreachable, fix semantic analysis")
//     }
// }

// #[derive(Debug, Clone, Eq, PartialEq, Hash)]
// struct VarName(Box<str>);

// impl From<&NameTree> for VarName {
//     fn from(value: &NameTree) -> Self {
//         Self(value.name().name().into())
//     }
// }

// #[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
// struct Register(u32);

// impl Display for Register {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "%{}", self.0)
//     }
// }

pub trait CodeGenerator {
    fn generate(self) -> Result<String, Box<dyn std::error::Error>>;
}

pub trait RegisterProvider {
    type Register: Register;
    fn allocate(&mut self, graph: &IrGraph) -> Result<(), Box<dyn std::error::Error>>;
    fn get_register(&self, node: NodeId) -> Self::Register;
}

pub trait Register: Clone + Copy {
    fn name(&self) -> &str;
}
pub trait X64Register: Register {}

#[derive(Default)]
pub struct TrivialRegisterProvider {
    map: HashMap<NodeId, X64Registers>,
}

impl RegisterProvider for TrivialRegisterProvider {
    type Register = X64Registers;

    fn get_register(&self, node: NodeId) -> Self::Register {
        *self.map.get(&node).unwrap()
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
            let next = u8::try_from(self.map.len())?;
            let register = X64Registers::try_from(next)?;
            // we just cheked that this register is unused
            self.map.insert(node, register).is_none();
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, TryFromPrimitive)]
#[repr(u8)]
pub enum X64Registers {
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register for X64Registers {
    fn name(&self) -> &str {
        match self {
            X64Registers::R8 => "%r8",
            X64Registers::R9 => "%r9",
            X64Registers::R10 => "%r10",
            X64Registers::R11 => "%r11",
            X64Registers::R12 => "%r12",
            X64Registers::R13 => "%r13",
            X64Registers::R14 => "%r14",
            X64Registers::R15 => "%r15",
        }
    }
}

impl X64Register for X64Registers {}

pub struct X86_64Asm<'g, P>
where
    P: RegisterProvider,
    P::Register: X64Register,
{
    graph: &'g IrGraph,
    provider: P,
    builder: String,
}

impl<'g, P> X86_64Asm<'g, P>
where
    P: RegisterProvider,
    P::Register: X64Register,
{
    pub fn new(graph: &'g IrGraph, provider: P) -> Self {
        Self {
            graph,
            provider,
            builder: Default::default(),
        }
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
                write!(
                    self.builder,
                    "movq ${}, {}\n",
                    value,
                    self.provider.get_register(node).name()
                )
                .unwrap();
            }
            NodeKind::Return => {
                let pred = node.predecessor_skip_proj(self.graph, NodeKind::RETURN_RESULT);
                write!(
                    self.builder,
                    "movq {}, %rax\n",
                    self.provider.get_register(pred).name()
                )
                .unwrap();
            }
            NodeKind::Phi => panic!("phi not valid in code_gen"),
            _ => {}
        };
    }

    fn binary(&mut self, binary_op: BinaryOp, node: NodeId) {
        let lhs = node.predecessor(self.graph, BinaryOp::LEFT);
        let rhs = node.predecessor(self.graph, BinaryOp::RIGHT);

        write!(
            self.builder,
            "movq {}, {}\n",
            self.provider.get_register(lhs).name(),
            self.provider.get_register(node).name()
        )
        .unwrap();
        let op = match binary_op {
            BinaryOp::Add => "addq",
            BinaryOp::Div => "divq",
            BinaryOp::Mod => "modq",
            BinaryOp::Mul => "mulq",
            BinaryOp::Sub => "subq",
        };
        write!(
            self.builder,
            "{op} {}, {}\n",
            self.provider.get_register(rhs).name(),
            self.provider.get_register(node).name()
        )
        .unwrap();
    }
}

impl<'g, P> CodeGenerator for X86_64Asm<'g, P>
where
    P: RegisterProvider,
    P::Register: X64Register,
{
    fn generate(mut self) -> Result<String, Box<dyn std::error::Error>> {
        self.provider.allocate(self.graph)?;
        write!(self.builder, "_{}:\n", self.graph.name())?;
        let mut visited = HashSet::new();
        self.scan(self.graph.end_block(), &mut visited);
        Ok(self.builder)
    }
}
