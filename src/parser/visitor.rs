use std::marker::PhantomData;

use super::ast::{
    AssignmentTree, BinaryOpTree, BlockTree, DeclarationTree, FunctionTree, IdentExprTree,
    KindTree, LValueIdentTree, LiteralTree, NameTree, NegateTree, ProgrammTree, ReturnTree, Tree,
};

pub trait Visitor<T, R, E> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: &mut T) -> Result<R, E>;
    fn visit_binary_op(&self, tree: &BinaryOpTree, data: &mut T) -> Result<R, E>;
    fn visit_block(&self, tree: &BlockTree, data: &mut T) -> Result<R, E>;
    fn visit_declaration(&self, tree: &DeclarationTree, data: &mut T) -> Result<R, E>;
    fn visit_function(&self, tree: &FunctionTree, data: &mut T) -> Result<R, E>;
    fn visit_ident_expr(&self, tree: &IdentExprTree, data: &mut T) -> Result<R, E>;
    fn visit_literal(&self, tree: &LiteralTree, data: &mut T) -> Result<R, E>;
    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: &mut T) -> Result<R, E>;
    fn visit_name(&self, tree: &NameTree, data: &mut T) -> Result<R, E>;
    fn visit_negate(&self, tree: &NegateTree, data: &mut T) -> Result<R, E>;
    fn visit_programm(&self, tree: &ProgrammTree, data: &mut T) -> Result<R, E>;
    fn visit_return(&self, tree: &ReturnTree, data: &mut T) -> Result<R, E>;
    fn visit_kind(&self, tree: &KindTree, data: &mut T) -> Result<R, E>;
}

pub struct RecursivePostOrderVisitor<T, R, E> {
    visitor: Box<dyn Visitor<T, R, E>>,
}

impl<T, R, E> RecursivePostOrderVisitor<T, R, E> {
    pub fn new(visitor: Box<dyn Visitor<T, R, E>>) -> Self {
        Self { visitor }
    }

    fn accumulate<'d>(&self, data: &'d mut T, value: R) -> &'d mut T {
        // no op (for now)
        return data;
    }
}

impl<T, R, E> Visitor<T, R, E> for RecursivePostOrderVisitor<T, R, E> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: &mut T) -> Result<R, E> {
        let r = tree.lvalue().accept(self, data)?;
        let r = tree.expression().accept(self, self.accumulate(data, r))?;
        let r = self
            .visitor
            .visit_assignment(tree, self.accumulate(data, r));
        r
    }

    fn visit_binary_op(&self, tree: &BinaryOpTree, data: &mut T) -> Result<R, E> {
        let r = tree.lhs().accept(self, data)?;
        let r = tree.rhs().accept(self, self.accumulate(data, r))?;
        let r = self.visitor.visit_binary_op(tree, self.accumulate(data, r));
        r
    }

    fn visit_block(&self, tree: &BlockTree, data: &mut T) -> Result<R, E> {
        let mut r;
        let mut d = data;
        for statement in tree.statements() {
            r = statement.accept(self, d)?;
            d = self.accumulate(d, r);
        }
        r = self.visitor.visit_block(tree, d)?;
        Ok(r)
    }

    fn visit_declaration(&self, tree: &DeclarationTree, data: &mut T) -> Result<R, E> {
        let mut r = tree.kind().accept(self, data)?;
        r = tree.name().accept(self, self.accumulate(data, r))?;
        if let Some(initializer) = tree.initializer() {
            r = initializer.accept(self, self.accumulate(data, r))?;
        }
        r = self
            .visitor
            .visit_declaration(tree, self.accumulate(data, r))?;
        Ok(r)
    }

    fn visit_function(&self, tree: &FunctionTree, data: &mut T) -> Result<R, E> {
        let mut r = tree.return_type().accept(self, data)?;
        r = tree.name().accept(self, self.accumulate(data, r))?;
        r = tree.body().accept(self, self.accumulate(data, r))?;
        r = self
            .visitor
            .visit_function(tree, self.accumulate(data, r))?;
        Ok(r)
    }

    fn visit_ident_expr(&self, tree: &IdentExprTree, data: &mut T) -> Result<R, E> {
        let mut r = tree.name().accept(self, data)?;
        r = self
            .visitor
            .visit_ident_expr(tree, self.accumulate(data, r))?;
        Ok(r)
    }

    fn visit_literal(&self, tree: &LiteralTree, data: &mut T) -> Result<R, E> {
        self.visitor.visit_literal(tree, data)
    }

    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: &mut T) -> Result<R, E> {
        let mut r = tree.name().accept(self, data)?;
        r = self
            .visitor
            .visit_lvalue_ident(tree, self.accumulate(data, r))?;
        Ok(r)
    }

    fn visit_name(&self, tree: &NameTree, data: &mut T) -> Result<R, E> {
        self.visitor.visit_name(tree, data)
    }

    fn visit_negate(&self, tree: &NegateTree, data: &mut T) -> Result<R, E> {
        let mut r = tree.expr_tree().accept(self, data)?;
        r = self.visitor.visit_negate(tree, self.accumulate(data, r))?;
        Ok(r)
    }

    fn visit_programm(&self, tree: &ProgrammTree, data: &mut T) -> Result<R, E> {
        let mut r;
        let mut d = data;
        for top_level_tree in tree.top_level() {
            r = top_level_tree.accept(self, d)?;
            d = self.accumulate(d, r);
        }
        r = self.visitor.visit_programm(tree, d)?;
        Ok(r)
    }

    fn visit_return(&self, tree: &ReturnTree, data: &mut T) -> Result<R, E> {
        let mut r = tree.expr().accept(self, data)?;
        r = self.visitor.visit_return(tree, self.accumulate(data, r))?;
        Ok(r)
    }

    fn visit_kind(&self, tree: &KindTree, data: &mut T) -> Result<R, E> {
        self.visitor.visit_kind(tree, data)
    }
}

pub struct NoOpVisitor<T, R: Default, E> {
    _m0: PhantomData<T>,
    _m1: PhantomData<R>,
    _m2: PhantomData<E>,
}

impl<T, R: Default, E> NoOpVisitor<T, R, E> {
    pub fn new() -> Self {
        Self {
            _m0: PhantomData,
            _m1: PhantomData,
            _m2: PhantomData,
        }
    }
}
impl<T, R: Default, E> Visitor<T, R, E> for NoOpVisitor<T, R, E> {
    fn visit_assignment(&self, tree: &AssignmentTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_binary_op(&self, tree: &BinaryOpTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_block(&self, tree: &BlockTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_declaration(&self, tree: &DeclarationTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_function(&self, tree: &FunctionTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_ident_expr(&self, tree: &IdentExprTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_literal(&self, tree: &LiteralTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_lvalue_ident(&self, tree: &LValueIdentTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_name(&self, tree: &NameTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_negate(&self, tree: &NegateTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_programm(&self, tree: &ProgrammTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_return(&self, tree: &ReturnTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }

    fn visit_kind(&self, tree: &KindTree, data: &mut T) -> Result<R, E> {
        Ok(Default::default())
    }
}
