use std::marker::PhantomData;

use super::ast::{
    AssignmentTree, BinaryOpTree, BlockTree, DeclarationTree, FunctionTree, IdentExprTree,
    KindTree, LValueIdentTree, LiteralTree, NameTree, NegateTree, ProgrammTree, ReturnTree,
};

pub struct Unit;
pub struct NoOpVisitor<T> {
    _marker: PhantomData<T>,
}

impl<T> Visitor<T, Unit> for NoOpVisitor<T> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_binary_op(&self, tree: &BinaryOpTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_block(&self, tree: &BlockTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_declaration(&self, tree: &DeclarationTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_function(&self, tree: &FunctionTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_ident_expr(&self, tree: &IdentExprTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_literal(&self, tree: &LiteralTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_name(&self, tree: &NameTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_negate(&self, tree: &NegateTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_programm(&self, tree: &ProgrammTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_return(&self, tree: &ReturnTree, data: T) -> Unit {
        Unit {}
    }

    fn visit_kind(&self, tree: &KindTree, data: T) -> Unit {
        Unit {}
    }
}

pub trait Visitor<T, R> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: T) -> R;
    fn visit_binary_op(&self, tree: &BinaryOpTree, data: T) -> R;
    fn visit_block(&self, tree: &BlockTree, data: T) -> R;
    fn visit_declaration(&self, tree: &DeclarationTree, data: T) -> R;
    fn visit_function(&self, tree: &FunctionTree, data: T) -> R;
    fn visit_ident_expr(&self, tree: &IdentExprTree, data: T) -> R;
    fn visit_literal(&self, tree: &LiteralTree, data: T) -> R;
    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: T) -> R;
    fn visit_name(&self, tree: &NameTree, data: T) -> R;
    fn visit_negate(&self, tree: &NegateTree, data: T) -> R;
    fn visit_programm(&self, tree: &ProgrammTree, data: T) -> R;
    fn visit_return(&self, tree: &ReturnTree, data: T) -> R;
    fn visit_kind(&self, tree: &KindTree, data: T) -> R;
}
