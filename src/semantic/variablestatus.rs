use std::fmt::Display;

use crate::parser::{
    ProgrammTree,
    ast::NameTree,
    visitor::{NoOpVisitor, Visitor},
};

use super::{SemanticError, SemanticResult, namespace::Namespace};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum VariableState {
    Declared,
    Initialized,
}

impl Display for VariableState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableState::Declared => write!(f, "declared"),
            VariableState::Initialized => write!(f, "initialized"),
        }
    }
}

pub(super) struct VariableStatusAnalysis {
    no_op: NoOpVisitor<Namespace<VariableState>, (), SemanticError>,
}

impl VariableStatusAnalysis {
    pub(super) fn new() -> Self {
        Self {
            no_op: NoOpVisitor::new(),
        }
    }

    fn expect_undeclared(name: &NameTree, var_state: Option<VariableState>) -> SemanticResult<()> {
        if var_state.is_some() {
            Err(SemanticError::MultipleVariableDeclarations(
                name.name().name().into(),
            ))
        } else {
            Ok(())
        }
    }

    fn expect_declared(
        name: &NameTree,
        var_state: Option<VariableState>,
    ) -> SemanticResult<VariableState> {
        match var_state {
            Some(s) => Ok(s),
            None => Err(SemanticError::AssignedUndeclaredVariable(
                name.name().name().into(),
            )),
        }
    }

    fn expect_intialized(name: &NameTree, state: Option<VariableState>) -> SemanticResult<()> {
        if matches!(state, None | Some(VariableState::Declared)) {
            Err(SemanticError::UnitializedVariableUse(
                name.name().name().into(),
            ))
        } else {
            Ok(())
        }
    }

    fn update_state(
        data: &mut Namespace<VariableState>,
        state: VariableState,
        name: &NameTree,
    ) -> SemanticResult<()> {
        data.put(name, state, |existing, replacement| {
            if *existing > replacement {
                Err(SemanticError::InvalidVariableState {
                    variable: name.name().name().into(),
                    existing: existing.to_string().into_boxed_str(),
                    replacement: replacement.to_string().into_boxed_str(),
                })
            } else {
                Ok(replacement)
            }
        })
    }
}

impl Visitor<Namespace<VariableState>, (), SemanticError> for VariableStatusAnalysis {
    fn visit_assignment(
        &self,
        tree: &crate::parser::ast::AssignmentTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        match tree.lvalue() {
            crate::parser::ast::LValueTree::LValueIdentTree(lvalue_ident_tree) => {
                let name = lvalue_ident_tree.name();
                let state = data.get(name);
                let state = VariableStatusAnalysis::expect_declared(name, state)?;
                if state != VariableState::Initialized {
                    Self::update_state(data, VariableState::Initialized, name);
                }
            }
        }
        self.no_op.visit_assignment(tree, data)
    }

    fn visit_binary_op(
        &self,
        tree: &crate::parser::ast::BinaryOpTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_binary_op(&self.no_op, tree, data)
    }

    fn visit_block(
        &self,
        tree: &crate::parser::ast::BlockTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_block(&self.no_op, tree, data)
    }

    fn visit_declaration(
        &self,
        tree: &crate::parser::ast::DeclarationTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        Self::expect_undeclared(tree.name(), data.get(tree.name()))?;
        let state = if tree.initializer().is_none() {
            VariableState::Declared
        } else {
            VariableState::Initialized
        };
        Self::update_state(data, state, tree.name())?;
        self.no_op.visit_declaration(tree, data)
    }

    fn visit_function(
        &self,
        tree: &crate::parser::ast::FunctionTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_function(&self.no_op, tree, data)
    }

    fn visit_ident_expr(
        &self,
        tree: &crate::parser::ast::IdentExprTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        let state = data.get(tree.name());
        Self::expect_intialized(tree.name(), state)?;
        self.no_op.visit_ident_expr(tree, data)
    }

    fn visit_literal(
        &self,
        tree: &crate::parser::ast::LiteralTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_literal(&self.no_op, tree, data)
    }

    fn visit_lvalue_ident(
        &self,
        tree: &crate::parser::ast::LValueIdentTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_lvalue_ident(&self.no_op, tree, data)
    }

    fn visit_name(
        &self,
        tree: &NameTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_name(&self.no_op, tree, data)
    }

    fn visit_negate(
        &self,
        tree: &crate::parser::ast::NegateTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_negate(&self.no_op, tree, data)
    }

    fn visit_programm(
        &self,
        tree: &ProgrammTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_programm(&self.no_op, tree, data)
    }

    fn visit_return(
        &self,
        tree: &crate::parser::ast::ReturnTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_return(&self.no_op, tree, data)
    }

    fn visit_kind(
        &self,
        tree: &crate::parser::ast::KindTree,
        data: &mut Namespace<VariableState>,
    ) -> Result<(), SemanticError> {
        <NoOpVisitor<Namespace<VariableState>, (), SemanticError> as Visitor<
            Namespace<VariableState>,
            (),
            SemanticError,
        >>::visit_kind(&self.no_op, tree, data)
    }
}
