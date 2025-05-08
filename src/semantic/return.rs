use tracing::{debug, error};

use crate::parser::{
    ProgrammTree,
    ast::NameTree,
    visitor::{NoOpVisitor, Visitor},
};

use super::{SemanticError, namespace::Namespace};

pub(super) struct ReturnAnalysis {
    no_op: NoOpVisitor<ReturnState, (), SemanticError>,
}

impl Visitor<ReturnState, (), SemanticError> for ReturnAnalysis {
    fn visit_assignment(
        &self,
        tree: &crate::parser::ast::AssignmentTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_assignment(&self.no_op, tree, data)
    }

    fn visit_binary_op(
        &self,
        tree: &crate::parser::ast::BinaryOpTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_binary_op(&self.no_op, tree, data)
    }

    fn visit_block(
        &self,
        tree: &crate::parser::ast::BlockTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_block(&self.no_op, tree, data)
    }

    fn visit_declaration(
        &self,
        tree: &crate::parser::ast::DeclarationTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_declaration(&self.no_op, tree, data)
    }

    fn visit_function(
        &self,
        tree: &crate::parser::ast::FunctionTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        if !data.returns {
            return Err(SemanticError::NoReturn(tree.name().name().name().into()));
        }
        data.returns = false;
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_function(&self.no_op, tree, data)
    }

    fn visit_ident_expr(
        &self,
        tree: &crate::parser::ast::IdentExprTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_ident_expr(&self.no_op, tree, data)
    }

    fn visit_literal(
        &self,
        tree: &crate::parser::ast::LiteralTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_literal(&self.no_op, tree, data)
    }

    fn visit_lvalue_ident(
        &self,
        tree: &crate::parser::ast::LValueIdentTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_lvalue_ident(&self.no_op, tree, data)
    }

    fn visit_name(&self, tree: &NameTree, data: &mut ReturnState) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_name(&self.no_op, tree, data)
    }

    fn visit_negate(
        &self,
        tree: &crate::parser::ast::NegateTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_negate(&self.no_op, tree, data)
    }

    fn visit_programm(
        &self,
        tree: &ProgrammTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_programm(&self.no_op, tree, data)
    }

    fn visit_return(
        &self,
        tree: &crate::parser::ast::ReturnTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        data.returns = true;
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_return(&self.no_op, tree, data)
    }

    fn visit_kind(
        &self,
        tree: &crate::parser::ast::KindTree,
        data: &mut ReturnState,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<ReturnState, (), SemanticError> as Visitor<ReturnState, (), SemanticError>>::visit_kind(&self.no_op, tree, data)
    }
}
impl ReturnAnalysis {
    pub(super) fn new() -> Self {
        Self {
            no_op: NoOpVisitor::new(),
        }
    }
}
#[derive(Debug, Clone)]
pub(super) struct ReturnState {
    returns: bool,
}

impl ReturnState {
    pub(super) fn new() -> Self {
        Self { returns: false }
    }
}
