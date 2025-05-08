use tracing::{debug, error};

use crate::parser::{
    ProgrammTree,
    ast::NameTree,
    visitor::{NoOpVisitor, Visitor},
};

use super::{SemanticError, namespace::Namespace};

pub(super) struct IntegerLiteralRangeAnalysis {
    no_op: NoOpVisitor<Namespace<()>, (), SemanticError>,
}
impl IntegerLiteralRangeAnalysis {
    pub(super) fn new() -> Self {
        Self {
            no_op: NoOpVisitor::new(),
        }
    }
}

impl Visitor<Namespace<()>, (), SemanticError> for IntegerLiteralRangeAnalysis {
    fn visit_assignment(
        &self,
        tree: &crate::parser::ast::AssignmentTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_assignment(&self.no_op, tree, data)
    }

    fn visit_binary_op(
        &self,
        tree: &crate::parser::ast::BinaryOpTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_binary_op(&self.no_op, tree, data)
    }

    fn visit_block(
        &self,
        tree: &crate::parser::ast::BlockTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_block(&self.no_op, tree, data)
    }

    fn visit_declaration(
        &self,
        tree: &crate::parser::ast::DeclarationTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_declaration(&self.no_op, tree, data)
    }

    fn visit_function(
        &self,
        tree: &crate::parser::ast::FunctionTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_function(&self.no_op, tree, data)
    }

    fn visit_ident_expr(
        &self,
        tree: &crate::parser::ast::IdentExprTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_ident_expr(&self.no_op, tree, data)
    }

    fn visit_literal(
        &self,
        tree: &crate::parser::ast::LiteralTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        debug!("checking int literal {}", tree.value());
        let _ = tree.parse_value().ok_or_else(|| {
            error!("invalid int literal");
            SemanticError::InvalidIntegerLiteral(tree.value().into())
        })?;
        self.no_op.visit_literal(tree, data)
    }

    fn visit_lvalue_ident(
        &self,
        tree: &crate::parser::ast::LValueIdentTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_lvalue_ident(&self.no_op, tree, data)
    }

    fn visit_name(&self, tree: &NameTree, data: &mut Namespace<()>) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_name(&self.no_op, tree, data)
    }

    fn visit_negate(
        &self,
        tree: &crate::parser::ast::NegateTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_negate(&self.no_op, tree, data)
    }

    fn visit_programm(
        &self,
        tree: &ProgrammTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_programm(&self.no_op, tree, data)
    }

    fn visit_return(
        &self,
        tree: &crate::parser::ast::ReturnTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_return(&self.no_op, tree, data)
    }

    fn visit_kind(
        &self,
        tree: &crate::parser::ast::KindTree,
        data: &mut Namespace<()>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<()>, (), SemanticError> as Visitor<
            Namespace<()>,
            (),
            SemanticError,
        >>::visit_kind(&self.no_op, tree, data)
    }
}
