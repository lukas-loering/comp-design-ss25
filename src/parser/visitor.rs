use std::marker::PhantomData;

use super::ast::{
    AssignmentTree, BinaryOpTree, BlockTree, DeclarationTree, FunctionTree, IdentExprTree,
    KindTree, LValueIdentTree, LiteralTree, NameTree, NegateTree, ProgrammTree, ReturnTree, Tree,
};


pub trait Visitor<T, R> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: &mut T) -> R;
    fn visit_binary_op(&self, tree: &BinaryOpTree, data: &mut T) -> R;
    fn visit_block(&self, tree: &BlockTree, data: &mut T) -> R;
    fn visit_declaration(&self, tree: &DeclarationTree, data: &mut T) -> R;
    fn visit_function(&self, tree: &FunctionTree, data: &mut T) -> R;
    fn visit_ident_expr(&self, tree: &IdentExprTree, data: &mut T) -> R;
    fn visit_literal(&self, tree: &LiteralTree, data: &mut T) -> R;
    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: &mut T) -> R;
    fn visit_name(&self, tree: &NameTree, data: &mut T) -> R;
    fn visit_negate(&self, tree: &NegateTree, data: &mut T) -> R;
    fn visit_programm(&self, tree: &ProgrammTree, data: &mut T) -> R;
    fn visit_return(&self, tree: &ReturnTree, data: &mut T) -> R;
    fn visit_kind(&self, tree: &KindTree, data: &mut T) -> R;
}

struct RecursivePostOrderVisitor<T, R> {
    visitor: Box<dyn Visitor<T, R>>
}

impl<T, R> RecursivePostOrderVisitor<T, R> {
    fn accumulate<'d>(&self, data: &'d mut T, value: R) -> &'d mut T {
        // no op (for now)
        return data
    }
}

impl<T, R> Visitor<T, R> for RecursivePostOrderVisitor<T, R> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: &mut T) -> R {
        let r = tree.lvalue().accept(self, data);
        let r = tree.expression().accept(self, self.accumulate(data, r));
        let r = self.visitor.visit_assignment(tree, self.accumulate(data, r));
        r
    }

    fn visit_binary_op(&self, tree: &BinaryOpTree, data: &mut T) -> R {
        let r = tree.lhs().accept(self, data);
        let r = tree.rhs().accept(self, self.accumulate(data, r));
        let r = self.visitor.visit_binary_op(tree, self.accumulate(data, r));
        r
    }

    fn visit_block(&self, tree: &BlockTree, data: &mut T) -> R {
        let mut r;
        let mut d = data;
        for statement in tree.statements() {
            r = statement.accept(self, d);
            d = self.accumulate(d, r);
        }
        r = self.visitor.visit_block(tree, d);
        r
    }

    fn visit_declaration(&self, tree: &DeclarationTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_function(&self, tree: &FunctionTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_ident_expr(&self, tree: &IdentExprTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_literal(&self, tree: &LiteralTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_name(&self, tree: &NameTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_negate(&self, tree: &NegateTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_programm(&self, tree: &ProgrammTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_return(&self, tree: &ReturnTree, data: &mut T) -> R {
        todo!()
    }

    fn visit_kind(&self, tree: &KindTree, data: &mut T) -> R {
        todo!()
    }
}

pub struct Unit;
pub struct NoOpVisitor<T> {
    _marker: PhantomData<T>,
}

impl<T> Visitor<T, Unit> for NoOpVisitor<T> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_binary_op(&self, tree: &BinaryOpTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_block(&self, tree: &BlockTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_declaration(&self, tree: &DeclarationTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_function(&self, tree: &FunctionTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_ident_expr(&self, tree: &IdentExprTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_literal(&self, tree: &LiteralTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_name(&self, tree: &NameTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_negate(&self, tree: &NegateTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_programm(&self, tree: &ProgrammTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_return(&self, tree: &ReturnTree, data: &mut T) -> Unit {
        Unit {}
    }

    fn visit_kind(&self, tree: &KindTree, data: &mut T) -> Unit {
        Unit {}
    }
}

