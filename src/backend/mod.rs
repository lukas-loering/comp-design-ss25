use std::{collections::HashMap, fmt::Display};

use crate::parser::ast::{AssignmentTree, BlockTree, ExpressionTree, NameTree, StatementTree};

#[derive(Debug, Clone, Default)]
pub struct CodeGen {
    reg_map: HashMap<VarName, Register>,
    next_reg: Register,
    code: Vec<String>,
}

impl CodeGen {
    pub fn generate(ast: &BlockTree) -> Vec<String> {
        let mut code_gen = Self::default();
        code_gen.gen_block(ast.statements());
        code_gen.code
    }

    fn gen_block(&mut self, statements: &[StatementTree]) {
        for statement in statements {
            self.gen_statement(statement);
        }
    }

    fn gen_statement(&mut self, statement: &StatementTree) {
        match statement {
            StatementTree::AssignmentTree(assignment_tree) => {
                let op = assignment_tree.operator();
                let rhs = self.gen_expr(assignment_tree.expression());
                let lhs = self.lookup_var(assignment_tree.lvalue().name());
                self.emit(format!("{lhs} {op} {rhs}"));
            }
            StatementTree::BlockTree(block_tree) => self.gen_block(block_tree.statements()),
            StatementTree::DeclarationTree(declaration_tree) => {
                let r = self.fresh_reg();
                self.assign_var(declaration_tree.name().into(), r);
                if let Some(initializer) = declaration_tree.initializer() {
                    let r2 = self.gen_expr(initializer);
                    self.emit(format!("{r} = {r2}"));
                }
            }
            StatementTree::ReturnTree(return_tree) => {
                let r = self.gen_expr(return_tree.expr());
                self.emit(format!("ret {r}"));
            }
        }
    }

    fn gen_expr(&mut self, expr: &ExpressionTree) -> Register {
        match expr {
            ExpressionTree::BinaryOpTree(binary_op_tree) => {
                let op = binary_op_tree.operator_kind();
                let r1 = self.gen_expr(binary_op_tree.lhs());
                let r2 = self.gen_expr(binary_op_tree.rhs());
                let r = self.fresh_reg();
                self.emit(format!("{r} = {op} {r1} {r2}"));
                r
            }
            ExpressionTree::IdentExprTree(ident_expr_tree) => {
                self.lookup_var(ident_expr_tree.name())
            }
            ExpressionTree::LiteralTree(literal_tree) => {
                let r = self.fresh_reg();
                self.emit(format!("{r} = {}", literal_tree.value()));
                r
            }
            ExpressionTree::NegateTree(negate_tree) => {
                let op = negate_tree.operator_kind();
                let r1 = self.gen_expr(negate_tree.expr_tree());
                let r = self.fresh_reg();
                self.emit(format!("{r} = {op} {r1}"));
                r
            }
        }
    }

    fn emit(&mut self, instruction: String) {
        self.code.push(instruction);
    }

    fn fresh_reg(&mut self) -> Register {
        let fresh = self.next_reg;
        self.next_reg = Register(self.next_reg.0 + 1);
        fresh
    }

    fn assign_var(&mut self, var_name: VarName, reg: Register) {
        self.reg_map.insert(var_name, reg);
    }

    fn lookup_var(&mut self, name: impl Into<VarName>) -> Register {
        *self
            .reg_map
            .get(&name.into())
            .expect("unreachable, fix semantic analysis")
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct VarName(Box<str>);

impl From<&NameTree> for VarName {
    fn from(value: &NameTree) -> Self {
        Self(value.name().name().into())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
struct Register(u32);

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}
