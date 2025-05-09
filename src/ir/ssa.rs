use thiserror::Error;

use crate::parser::{ast::FunctionTree, visitor::Visitor};

use super::{constructor::GraphConstructor, node::IrNode, optimize::Optimizer};

#[derive(Debug, Error)]
#[error("{reason}")]
struct SsaError{reason: Box<str>}

impl From<String> for SsaError {
    fn from(value: String) -> Self {
        Self{reason: value.into_boxed_str()}
    }
}

pub struct SsaTranslation {
    function: FunctionTree,
    constructor: GraphConstructor,
}

impl SsaTranslation {
    pub fn new(function: FunctionTree, optimizer: impl Optimizer + 'static) -> Self {
        Self { function, constructor: GraphConstructor::new(optimizer, graph) }
    }
}

struct SsaTranslationVisitor;

impl Visitor<SsaTranslation, Option<IrNode>, SsaError> for SsaTranslationVisitor {
    fn visit_assignment(&self, tree: &crate::parser::ast::AssignmentTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()        
    }

    fn visit_binary_op(&self, tree: &crate::parser::ast::BinaryOpTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_block(&self, tree: &crate::parser::ast::BlockTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_declaration(&self, tree: &crate::parser::ast::DeclarationTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_function(&self, tree: &FunctionTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_ident_expr(&self, tree: &crate::parser::ast::IdentExprTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_literal(&self, tree: &crate::parser::ast::LiteralTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_lvalue_ident(&self, tree: &crate::parser::ast::LValueIdentTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_name(&self, tree: &crate::parser::ast::NameTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_negate(&self, tree: &crate::parser::ast::NegateTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_programm(&self, tree: &crate::parser::ProgrammTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_return(&self, tree: &crate::parser::ast::ReturnTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }

    fn visit_kind(&self, tree: &crate::parser::ast::KindTree, data: &mut SsaTranslation) -> Result<Option<IrNode>, SsaError> {
        todo!()
    }
}