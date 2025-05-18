use std::{collections::VecDeque, time::Instant};

use thiserror::Error;

use crate::{
    parser::{
        Tree,
        ast::{FunctionTree, StatementTree},
        symbol::Name,
        visitor::Visitor,
    },
    span::HasSpan,
};

use super::{
    BinaryOp, IrGraph, NodeId, NodeKind, NodeProvider, constructor::GraphConstructor,
    debug::DebugInfo, optimize::Optimizer,
};

// #[derive(Debug, Error)]
// #[error("{reason}")]
// struct SsaError {
//     reason: Box<str>,
// }

// impl From<String> for SsaError {
//     fn from(value: String) -> Self {
//         Self {
//             reason: value.into_boxed_str(),
//         }
//     }
// }

type SsaError = ();

pub struct SsaTranslation {
    constructor: GraphConstructor,
    function: FunctionTree,
    debug_stack: VecDeque<DebugInfo>,
}

impl SsaTranslation {
    pub fn new(function: FunctionTree, optimizer: impl Optimizer + 'static) -> Self {
        let name = function.name().name().name().into();
        Self {
            constructor: GraphConstructor::new(optimizer, name),
            debug_stack: Default::default(),
            function,
        }
    }

    pub fn translate(mut self) -> IrGraph {
        let visitor = SsaTranslationVisitor::default();
        // NOTE: This clone is super stupid but I cant be bothered to work with rust here atm
        self.function.clone().accept(&visitor, &mut self);
        self.constructor.graph
    }
}

impl SsaTranslation {
    fn write_variable(&mut self, variable: Name, block: NodeId, value: NodeId) {
        self.constructor.write_variable(variable, block, value);
    }

    fn read_variable(&mut self, variable: Name, block: NodeId) -> NodeId {
        self.constructor.read_variable(variable, block)
    }

    fn current_block(&mut self) -> NodeId {
        self.constructor.current_block()
    }

    fn push_span(&mut self, span: &impl HasSpan) {
        self.debug_stack.push_back(DebugInfo::get_debug_info());
        DebugInfo::set_debug_info(DebugInfo::SourceInfo(span.span()));
    }

    fn pop_span(&mut self) {
        DebugInfo::set_debug_info(self.debug_stack.pop_front().unwrap_or(DebugInfo::None));
    }
}

#[derive(Default)]
struct SsaTranslationVisitor {}

impl SsaTranslationVisitor {
    fn proj_result_div_mod(data: &mut SsaTranslation, div_mod: NodeId) -> NodeId {
        let kind = data.constructor.get(div_mod).kind;
        // check if optimizations already changed the div/mod node to something else
        if !matches!(
            kind,
            NodeKind::BinaryOp(BinaryOp::Div) | NodeKind::BinaryOp(BinaryOp::Mod)
        ) {
            return div_mod;
        };
        let proj_side_effect = data.constructor.new_side_effect_proj(div_mod);
        data.constructor.write_current_side_effect(proj_side_effect);
        data.constructor.new_result_proj(div_mod)
    }
}

impl Visitor<SsaTranslation, Option<NodeId>, SsaError> for SsaTranslationVisitor {
    fn visit_assignment(
        &self,
        tree: &crate::parser::ast::AssignmentTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_assignment {tree:?}");
        data.push_span(tree);
        let constructor: Option<Box<dyn FnMut(NodeId, NodeId, &mut SsaTranslation) -> NodeId>> =
            match tree.operator().kind() {
                crate::lexer::tokens::OperatorKind::AssignMinus => {
                    Some(Box::new(|left, right, d| {
                        d.constructor.new_sub(left, right)
                    }))
                }
                crate::lexer::tokens::OperatorKind::AssignPlus => {
                    Some(Box::new(|left, right, d| {
                        d.constructor.new_add(left, right)
                    }))
                }
                crate::lexer::tokens::OperatorKind::AssignMul => {
                    Some(Box::new(|left, right, d| {
                        d.constructor.new_mul(left, right)
                    }))
                }
                crate::lexer::tokens::OperatorKind::AssignDiv => {
                    Some(Box::new(|left, right, d| {
                        let div = d.constructor.new_div(left, right);
                        Self::proj_result_div_mod(d, div)
                    }))
                }
                crate::lexer::tokens::OperatorKind::AssignMod => {
                    Some(Box::new(|left, right, d| {
                        let r#mod = d.constructor.new_mod(left, right);
                        Self::proj_result_div_mod(d, r#mod)
                    }))
                }
                crate::lexer::tokens::OperatorKind::Assign => None,
                default => panic!("not an assignment operator {default}"),
            };
        match tree.lvalue() {
            crate::parser::ast::LValueTree::LValueIdentTree(lvalue_ident_tree) => {
                let name = lvalue_ident_tree.name().name();
                let mut right = tree.expression().accept(self, data).unwrap().unwrap();
                if let Some(mut constructor) = constructor {
                    let current_block = data.current_block();
                    let left = data.read_variable(name.clone(), current_block);
                    right = constructor(left, right, data);
                }
                let current_block = data.current_block();
                data.write_variable(name.clone(), current_block, right);
            }
        };
        data.pop_span();
        Ok(None)
    }

    fn visit_binary_op(
        &self,
        tree: &crate::parser::ast::BinaryOpTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_binary_op {tree:?}");
        data.push_span(tree);
        let left = tree.lhs().accept(self, data).unwrap().unwrap();
        let right = tree.rhs().accept(self, data).unwrap().unwrap();
        let res = match tree.operator_kind() {
            crate::lexer::tokens::OperatorKind::Minus => data.constructor.new_sub(left, right),
            crate::lexer::tokens::OperatorKind::Plus => data.constructor.new_add(left, right),
            crate::lexer::tokens::OperatorKind::Mul => data.constructor.new_mul(left, right),
            crate::lexer::tokens::OperatorKind::Div => {
                let div = data.constructor.new_div(left, right);
                Self::proj_result_div_mod(data, div)
            }
            crate::lexer::tokens::OperatorKind::Mod => {
                let r#mod = data.constructor.new_mod(left, right);
                Self::proj_result_div_mod(data, r#mod)
            }
            default => panic!("not a binary expression operator {default}"),
        };
        data.pop_span();
        Ok(Some(res))
    }

    fn visit_block(
        &self,
        tree: &crate::parser::ast::BlockTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_block {tree:?}");
        data.push_span(tree);
        for statement in tree.statements() {
            statement.accept(self, data);
            // simple dead code elimination: if we hit a return skip everything after that in this block
            if matches!(statement, StatementTree::ReturnTree(_)) {
                break;
            }
        }
        data.pop_span();
        Ok(None)
    }

    fn visit_declaration(
        &self,
        tree: &crate::parser::ast::DeclarationTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_declaration {tree:?}");
        data.push_span(tree);
        if let Some(initializer) = tree.initializer() {
            let rhs = initializer.accept(self, data).unwrap().unwrap();
            let current_blcok = data.current_block();
            data.write_variable(tree.name().name().clone(), current_blcok, rhs);
        }
        data.pop_span();
        Ok(None)
    }

    fn visit_function(
        &self,
        tree: &FunctionTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_function {tree:?}");
        data.push_span(tree);
        let start = data.constructor.new_start();
        let side_effect = data.constructor.new_side_effect_proj(start);
        data.constructor.write_current_side_effect(side_effect);
        tree.body().accept(self, data);
        data.pop_span();
        Ok(None)
    }

    fn visit_ident_expr(
        &self,
        tree: &crate::parser::ast::IdentExprTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_ident_expr {tree:?}");
        data.push_span(tree);
        let current_block = data.current_block();
        let value = Ok(Some(
            data.read_variable(tree.name().name().clone(), current_block),
        ));
        data.pop_span();
        value
    }

    fn visit_literal(
        &self,
        tree: &crate::parser::ast::LiteralTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_literal {tree:?}");
        data.push_span(tree);
        let value = Ok(Some(
            data.constructor.new_const_int(tree.parse_value().unwrap()),
        ));
        data.pop_span();
        value
    }

    fn visit_lvalue_ident(
        &self,
        tree: &crate::parser::ast::LValueIdentTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        Ok(None)
    }

    fn visit_name(
        &self,
        tree: &crate::parser::ast::NameTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        Ok(None)
    }

    fn visit_negate(
        &self,
        tree: &crate::parser::ast::NegateTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_negate {tree:?}");
        data.push_span(tree);
        let node = tree.expr_tree().accept(self, data).unwrap().unwrap();
        let zero = data.constructor.new_const_int(0);
        let res = data.constructor.new_sub(zero, node);
        data.pop_span();
        Ok(Some(res))
    }

    fn visit_programm(
        &self,
        tree: &crate::parser::ProgrammTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        panic!("unsupported operation");
    }

    fn visit_return(
        &self,
        tree: &crate::parser::ast::ReturnTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        tracing::trace!("visit_return {tree:?}");
        data.push_span(tree);
        let node = tree.expr().accept(self, data).unwrap().unwrap();
        let ret = data.constructor.new_return(node);
        data.constructor
            .graph
            .end_block
            .add_predecessor(&mut data.constructor.graph, ret);
        data.pop_span();
        Ok(None)
    }

    fn visit_kind(
        &self,
        tree: &crate::parser::ast::KindTree,
        data: &mut SsaTranslation,
    ) -> Result<Option<NodeId>, SsaError> {
        panic!("unsupported operation");
    }
}
