use std::collections::HashMap;

use integerrange::IntegerLiteralRangeAnalysis;
use namespace::Namespace;
use thiserror::Error;
use tracing::info;
use variablestatus::VariableState;
use variablestatus::VariableStatusAnalysis;

use crate::parser::ParseResult;
use crate::parser::ProgrammTree;
use crate::parser::Tree;
use crate::parser::ast::NameTree;
use crate::parser::symbol::Name;
use crate::parser::visitor::NoOpVisitor;
use crate::parser::visitor::RecursivePostOrderVisitor;
use crate::parser::visitor::Visitor;

mod integerrange;
mod namespace;
mod variablestatus;

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("invalid integer literal `{0}`")]
    InvalidIntegerLiteral(Box<str>),
    #[error("variable `{0}` must be declared before assignment")]
    AssignedUndeclaredVariable(Box<str>),
    #[error("variable `{variable}` is already {existing}. Can not be {replacement} here.")]
    InvalidVariableState {
        variable: Box<str>,
        existing: Box<str>,
        replacement: Box<str>,
    },
}

type SemanticResult<T> = Result<T, SemanticError>;

pub struct SemanticAnalysis {
    program: ProgrammTree,
}

impl SemanticAnalysis {
    pub fn new(program: ProgrammTree) -> Self {
        Self { program }
    }

    #[tracing::instrument(skip(self))]
    pub fn analyze(&mut self) -> SemanticResult<()> {
        info!("integer literal range analysis");
        self.integer_literal_range_analysis()?;
        info!("variable statusanalysis");
        self.variable_status_analysis()
    }

    fn integer_literal_range_analysis(&mut self) -> Result<(), SemanticError> {
        let inner: Box<dyn Visitor<Namespace<()>, SemanticResult<()>>> =
            Box::new(IntegerLiteralRangeAnalysis::new());
        let outer: Box<dyn Visitor<Namespace<()>, SemanticResult<()>>> =
            Box::new(RecursivePostOrderVisitor::new(inner));
        let mut namespace = Namespace::new();
        self.program.accept(outer.as_ref(), &mut namespace)
    }

    fn variable_status_analysis(&mut self) -> Result<(), SemanticError> {
        let inner: Box<dyn Visitor<Namespace<VariableState>, SemanticResult<()>>> =
            Box::new(VariableStatusAnalysis::new());
        let outer: Box<dyn Visitor<Namespace<VariableState>, SemanticResult<()>>> =
            Box::new(RecursivePostOrderVisitor::new(inner));
        let mut namespace = Namespace::new();
        self.program.accept(outer.as_ref(), &mut namespace)
    }
}
