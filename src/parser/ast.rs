use crate::{
    lexer::tokens::{Operator, OperatorKind},
    span::{HasSpan, Position, Span},
};

use super::{kind::Kind, symbol::Name, visitor::Visitor};

pub trait Tree<T, R>: HasSpan {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R;
}

pub enum LValueTree {
    LValueIdentTree(LValueIdentTree),
}

impl From<LValueIdentTree> for LValueTree {
    fn from(value: LValueIdentTree) -> Self {
        Self::LValueIdentTree(value)
    }
}

impl HasSpan for LValueTree {
    fn span(&self) -> Span {
        match self {
            LValueTree::LValueIdentTree(lvalue_ident_tree) => lvalue_ident_tree.span(),
        }
    }
}

impl<T, R> Tree<T, R> for LValueTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        match self {
            LValueTree::LValueIdentTree(lvalue_ident_tree) => {
                lvalue_ident_tree.accept(visitor, data)
            }
        }
    }
}

pub struct NameTree {
    name: Name,
    span: Span,
}

impl NameTree {
    pub fn new(name: Name, span: Span) -> Self {
        Self { name, span }
    }
}

impl HasSpan for NameTree {
    fn span(&self) -> Span {
        self.span
    }
}

pub struct LValueIdentTree {
    name: NameTree,
}

impl LValueIdentTree {
    pub fn new(name: NameTree) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &NameTree {
        &self.name
    }
}

impl<T, R> Tree<T, R> for NameTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_name(self, data)
    }
}

impl HasSpan for LValueIdentTree {
    fn span(&self) -> Span {
        self.name.span()
    }
}

impl<T, R> Tree<T, R> for LValueIdentTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_lvalue_ident(self, data)
    }
}

pub enum ExpressionTree {
    BinaryOpTree(BinaryOpTree),
    IdentExprTree(IdentExprTree),
    LiteralTree(LiteralTree),
    NegateTree(NegateTree),
}

impl From<BinaryOpTree> for ExpressionTree {
    fn from(value: BinaryOpTree) -> Self {
        Self::BinaryOpTree(value)
    }
}

impl From<IdentExprTree> for ExpressionTree {
    fn from(value: IdentExprTree) -> Self {
        Self::IdentExprTree(value)
    }
}

impl From<LiteralTree> for ExpressionTree {
    fn from(value: LiteralTree) -> Self {
        Self::LiteralTree(value)
    }
}

impl From<NegateTree> for ExpressionTree {
    fn from(value: NegateTree) -> Self {
        Self::NegateTree(value)
    }
}

impl HasSpan for ExpressionTree {
    fn span(&self) -> Span {
        match self {
            ExpressionTree::BinaryOpTree(binary_op_tree) => binary_op_tree.span(),
            ExpressionTree::IdentExprTree(ident_expr_tree) => ident_expr_tree.span(),
            ExpressionTree::LiteralTree(literal_tree) => literal_tree.span(),
            ExpressionTree::NegateTree(negate_tree) => negate_tree.span(),
        }
    }
}

impl<T, R> Tree<T, R> for ExpressionTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        match self {
            ExpressionTree::BinaryOpTree(binary_op_tree) => binary_op_tree.accept(visitor, data),
            ExpressionTree::IdentExprTree(ident_expr_tree) => ident_expr_tree.accept(visitor, data),
            ExpressionTree::LiteralTree(literal_tree) => literal_tree.accept(visitor, data),
            ExpressionTree::NegateTree(negate_tree) => negate_tree.accept(visitor, data),
        }
    }
}

pub struct BinaryOpTree {
    lhs: Box<ExpressionTree>,
    rhs: Box<ExpressionTree>,
    operator_kind: OperatorKind,
}

impl BinaryOpTree {
    pub fn new(
        lhs: Box<ExpressionTree>,
        rhs: Box<ExpressionTree>,
        operator_kind: OperatorKind,
    ) -> Self {
        Self {
            lhs,
            rhs,
            operator_kind,
        }
    }

    pub fn lhs(&self) -> &ExpressionTree {
        &self.lhs
    }

    pub fn rhs(&self) -> &ExpressionTree {
        &self.rhs
    }
}

impl HasSpan for BinaryOpTree {
    fn span(&self) -> Span {
        self.lhs.span().merge(self.rhs.span())
    }
}

impl<T, R> Tree<T, R> for BinaryOpTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_binary_op(self, data)
    }
}
pub struct IdentExprTree {
    name: NameTree,
}

impl IdentExprTree {
    pub fn new(name: NameTree) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &NameTree {
        &self.name
    }
}

impl HasSpan for IdentExprTree {
    fn span(&self) -> Span {
        self.name.span()
    }
}

impl<T, R> Tree<T, R> for IdentExprTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_ident_expr(self, data)
    }
}

pub struct LiteralTree {
    value: Box<str>,
    base: u32,
    span: Span,
}

impl HasSpan for LiteralTree {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T, R> Tree<T, R> for LiteralTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_literal(self, data)
    }
}

impl LiteralTree {
    pub fn new(value: Box<str>, base: u32, span: Span) -> Self {
        Self { value, base, span }
    }

    pub fn parse_value(&self) -> Option<i64> {
        let value = if self.base == 16 {
            // skip the `0x` prefix of hex literals
            &self.value[2..]
        } else {
            &self.value
        };
        let value = value.parse::<i64>().ok()?;
        // bounds check, we only want non-negative 32-bit integers (for now)
        if value < 0 || value > i32::MAX.into() {
            return None;
        }
        Some(value)
    }
}

pub struct NegateTree {
    expr_tree: Box<ExpressionTree>,
    minus_pos: Span,
}

impl NegateTree {
    pub fn new(expr_tree: Box<ExpressionTree>, minus_pos: Span) -> Self {
        Self {
            expr_tree,
            minus_pos,
        }
    }

    pub fn expr_tree(&self) -> &ExpressionTree {
        &self.expr_tree
    }
}

impl HasSpan for NegateTree {
    fn span(&self) -> Span {
        self.minus_pos.merge(self.expr_tree.span())
    }
}

impl<T, R> Tree<T, R> for NegateTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_negate(self, data)
    }
}

pub enum StatementTree {
    AssignmentTree(AssignmentTree),
    BlockTree(BlockTree),
    DeclarationTree(DeclarationTree),
    ReturnTree(ReturnTree),
}

impl From<AssignmentTree> for StatementTree {
    fn from(value: AssignmentTree) -> Self {
        Self::AssignmentTree(value)
    }
}

impl From<BlockTree> for StatementTree {
    fn from(value: BlockTree) -> Self {
        Self::BlockTree(value)
    }
}

impl From<DeclarationTree> for StatementTree {
    fn from(value: DeclarationTree) -> Self {
        Self::DeclarationTree(value)
    }
}

impl From<ReturnTree> for StatementTree {
    fn from(value: ReturnTree) -> Self {
        Self::ReturnTree(value)
    }
}

impl HasSpan for StatementTree {
    fn span(&self) -> Span {
        match self {
            StatementTree::AssignmentTree(assignment_tree) => assignment_tree.span(),
            StatementTree::BlockTree(block_tree) => block_tree.span(),
            StatementTree::DeclarationTree(declaration_tree) => declaration_tree.span(),
            StatementTree::ReturnTree(return_tree) => return_tree.span(),
        }
    }
}

impl<T, R> Tree<T, R> for StatementTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        match self {
            StatementTree::AssignmentTree(assignment_tree) => assignment_tree.accept(visitor, data),
            StatementTree::BlockTree(block_tree) => block_tree.accept(visitor, data),
            StatementTree::DeclarationTree(declaration_tree) => {
                declaration_tree.accept(visitor, data)
            }
            StatementTree::ReturnTree(return_tree) => return_tree.accept(visitor, data),
        }
    }
}

pub struct AssignmentTree {
    lvalue: LValueTree,
    operator: Operator,
    expression: ExpressionTree,
}

impl HasSpan for AssignmentTree {
    fn span(&self) -> Span {
        self.lvalue.span().merge(self.expression.span())
    }
}

impl<T, R> Tree<T, R> for AssignmentTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_assignment(self, data)
    }
}

impl AssignmentTree {
    pub fn new(lvalue: LValueTree, operator: Operator, expression: ExpressionTree) -> Self {
        Self {
            lvalue,
            operator,
            expression,
        }
    }

    pub fn lvalue(&self) -> &LValueTree {
        &self.lvalue
    }

    pub fn expression(&self) -> &ExpressionTree {
        &self.expression
    }
}

pub struct BlockTree {
    statements: Box<[StatementTree]>,
    span: Span,
}

impl BlockTree {
    pub fn new(statements: Box<[StatementTree]>, span: Span) -> Self {
        Self { statements, span }
    }

    pub fn statements(&self) -> &[StatementTree] {
        &self.statements
    }
}

impl HasSpan for BlockTree {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T, R> Tree<T, R> for BlockTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_block(self, data)
    }
}

pub struct DeclarationTree {
    kind: KindTree,
    name: NameTree,
    initializer: Option<ExpressionTree>,
}

impl DeclarationTree {
    pub fn new(kind: KindTree, name: NameTree, initializer: Option<ExpressionTree>) -> Self {
        Self {
            kind,
            name,
            initializer,
        }
    }

    pub fn kind(&self) -> &KindTree {
        &self.kind
    }

    pub fn name(&self) -> &NameTree {
        &self.name
    }

    pub fn initializer(&self) -> Option<&ExpressionTree> {
        self.initializer.as_ref()
    }
}

impl HasSpan for DeclarationTree {
    fn span(&self) -> Span {
        match &self.initializer {
            Some(initializer) => self.kind.span().merge(initializer.span()),
            None => self.kind.span().merge(self.name.span()),
        }
    }
}

impl<T, R> Tree<T, R> for DeclarationTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_declaration(self, data)
    }
}

pub struct KindTree {
    kind: Kind,
    span: Span,
}

impl KindTree {
    pub fn new(kind: impl Into<Kind>, span: Span) -> Self {
        Self {
            kind: kind.into(),
            span,
        }
    }
}

impl HasSpan for KindTree {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T, R> Tree<T, R> for KindTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_kind(self, data)
    }
}

pub struct ReturnTree {
    expr: ExpressionTree,
    start_pos: Position,
}

impl ReturnTree {
    pub fn new(expr: ExpressionTree, start_pos: Position) -> Self {
        Self { expr, start_pos }
    }

    pub fn expr(&self) -> &ExpressionTree {
        &self.expr
    }
}

impl HasSpan for ReturnTree {
    fn span(&self) -> Span {
        Span::new(self.start_pos, self.expr.span().end())
    }
}

impl<T, R> Tree<T, R> for ReturnTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_return(self, data)
    }
}

pub struct ProgrammTree {
    top_level: Box<[FunctionTree]>,
}

impl HasSpan for ProgrammTree {
    fn span(&self) -> Span {
        let first = self.top_level.first().expect("should never be empty");
        let last = self.top_level.last().expect("should never be empty");
        Span::new(first.span().start(), last.span().end())
    }
}

impl<T, R> Tree<T, R> for ProgrammTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_programm(self, data)
    }
}

impl ProgrammTree {
    pub fn new(top_level: Box<[FunctionTree]>) -> Self {
        assert!(!top_level.is_empty(), "must not be empty");
        Self { top_level }
    }

    pub fn top_level(&self) -> &[FunctionTree] {
        &self.top_level
    }
}

pub struct FunctionTree {
    return_type: KindTree,
    name: NameTree,
    body: BlockTree,
}

impl FunctionTree {
    pub fn new(return_type: KindTree, name: NameTree, body: BlockTree) -> Self {
        Self {
            return_type,
            name,
            body,
        }
    }

    pub fn return_type(&self) -> &KindTree {
        &self.return_type
    }

    pub fn name(&self) -> &NameTree {
        &self.name
    }

    pub fn body(&self) -> &BlockTree {
        &self.body
    }
}

impl HasSpan for FunctionTree {
    fn span(&self) -> Span {
        Span::new(self.return_type.span().start(), self.body.span().end())
    }
}

impl<T, R> Tree<T, R> for FunctionTree {
    fn accept(&self, visitor: &dyn Visitor<T, R>, data: &mut T) -> R {
        visitor.visit_function(self, data)
    }
}
