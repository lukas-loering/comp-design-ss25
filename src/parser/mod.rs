use ast::{
    AssignmentTree, BinaryOpTree, BlockTree, DeclarationTree, ExpressionTree, FunctionTree,
    IdentExprTree, KindTree, LValueIdentTree, LValueTree, LiteralTree, NameTree, NegateTree,
    ReturnTree, StatementTree,
};

pub use ast::{ProgrammTree, Tree};
use kind::BasicKind;
pub use kind::NumericBase;
use symbol::Name;
pub use tokensource::TokenSource;
use tracing::debug;

use crate::{
    lexer::tokens::{Identifier, KeywordKind, Operator, OperatorKind, SeparatorKind, Token},
    span::HasSpan,
};

pub mod ast;
mod kind;
pub mod symbol;
mod tokensource;
pub mod visitor;

#[derive(Debug, Clone, thiserror::Error)]
#[non_exhaustive]
pub enum ParseError {
    #[error("expected operator `{expected}` got `{got}`")]
    ExpectedOperator { expected: OperatorKind, got: Token },
    #[error("expected assignment got `{got}`")]
    ExpectedAssignment { got: Token },
    #[error("expected keyword `{expected}` got `{got}`")]
    ExpectedKeyword { expected: KeywordKind, got: Token },
    #[error("expected identifier got `{got}`")]
    ExpectedIdentifier { got: Token },
    #[error("expected identifier `{ident}` got `{got}`")]
    ExpectedExactIdentifier { ident: Box<str>, got: Token },
    #[error("expected separator `{expected}` got `{got}`")]
    ExpectedSeparator { expected: SeparatorKind, got: Token },
    #[error("invalid factor `{got}`")]
    InvalidFactor { got: Token },
    #[error("reached end of file")]
    EndOfFile,
}

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: TokenSource,
}

pub type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(tokens: TokenSource) -> Self {
        Self { tokens }
    }
    #[tracing::instrument(skip(self))]
    pub fn parse_programm(&mut self) -> ParseResult<ProgrammTree> {
        debug!("parsing programm");
        Ok(ProgrammTree::new([self.parse_function()?].into()))
    }
}

impl Parser {
    fn parse_function(&mut self) -> ParseResult<FunctionTree> {
        debug!("parsing function");
        let return_type = self.tokens.expect_keyword(KeywordKind::Int)?;
        let ident = self.tokens.expect_identifier()?;
        if ident.value() != "main" {
            return Err(ParseError::ExpectedExactIdentifier { ident: "main".into(), got: Token::Identifier(ident) });
        }
        self.tokens.expect_separator(SeparatorKind::ParenOpen)?;
        self.tokens.expect_separator(SeparatorKind::ParenClose)?;
        let body = self.parse_block()?;
        Ok(FunctionTree::new(
            KindTree::new(BasicKind::Int, return_type.span()),
            Parser::name_tree(ident),
            body,
        ))
    }

    fn parse_block(&mut self) -> ParseResult<BlockTree> {
        debug!("parsing block");
        let body_open = self.tokens.expect_separator(SeparatorKind::BraceOpen)?;
        let mut statements = vec![];
        loop {
            let token = self.tokens.peek()?;
            if token.is_separator(SeparatorKind::BraceClose) {
                break;
            }
            statements.push(self.parse_statement()?);
        }
        let body_close = self.tokens.expect_separator(SeparatorKind::BraceClose)?;
        let block_tree = BlockTree::new(
            statements.into_boxed_slice(),
            body_open.span().merge(body_close.span()),
        );
        Ok(block_tree)
    }

    fn parse_statement(&mut self) -> ParseResult<StatementTree> {
        debug!("parsing statement");
        let statement = if self.tokens.peek()?.is_keyword(KeywordKind::Int) {
            self.parse_declaration()?.into()
        } else if self.tokens.peek()?.is_keyword(KeywordKind::Return) {
            self.parse_return()?.into()
        } else {
            self.parse_simple()?.into()
        };
        self.tokens.expect_separator(SeparatorKind::Semicolon)?;
        Ok(statement)
    }

    fn parse_declaration(&mut self) -> ParseResult<DeclarationTree> {
        debug!("parsing declaration");
        let kind = self.tokens.expect_keyword(KeywordKind::Int)?;
        let ident = self.tokens.expect_identifier()?;
        let expr = if self.tokens.peek()?.is_operator(OperatorKind::Assign) {
            self.tokens.expect_operator(OperatorKind::Assign)?;
            Some(self.parse_expr()?)
        } else {
            None
        };
        let declaration_tree = DeclarationTree::new(
            KindTree::new(BasicKind::Int, kind.span()),
            Parser::name_tree(ident),
            expr,
        );
        Ok(declaration_tree)
    }

    fn parse_simple(&mut self) -> ParseResult<AssignmentTree> {
        debug!("parsing simple assignment");
        let lvalue = self.parse_lvalue()?;
        let assignment_op = self.parse_assignment_op()?;
        let expr = self.parse_expr()?;
        let tree = AssignmentTree::new(lvalue, assignment_op, expr);
        Ok(tree)
    }

    fn parse_return(&mut self) -> ParseResult<ReturnTree> {
        debug!("parsing return");
        let ret = self.tokens.expect_keyword(KeywordKind::Return)?;
        let expr = self.parse_expr()?;
        Ok(ReturnTree::new(expr, ret.span().start()))
    }

    fn parse_expr(&mut self) -> ParseResult<ExpressionTree> {
        debug!("parsing expression");
        let mut lhs = self.parse_term()?;
        loop {
            let token = self.tokens.peek()?;
            let Token::Operator(operator) = token else {
                break;
            };
            match operator.kind() {
                OperatorKind::Plus | OperatorKind::Minus => {
                    self.tokens.consume()?;
                    lhs = BinaryOpTree::new(lhs.into(), self.parse_term()?.into(), operator.kind())
                        .into()
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_lvalue(&mut self) -> ParseResult<LValueTree> {
        debug!("parsing lvalue");
        if self.tokens.peek()?.is_separator(SeparatorKind::ParenOpen) {
            self.tokens.expect_separator(SeparatorKind::ParenOpen)?;
            let inner = self.parse_lvalue()?;
            self.tokens.expect_separator(SeparatorKind::ParenClose)?;
            Ok(inner)
        } else {
            let ident = self.tokens.expect_identifier()?;
            let tree = LValueIdentTree::new(Parser::name_tree(ident)).into();
            Ok(tree)
        }
    }

    fn parse_term(&mut self) -> ParseResult<ExpressionTree> {
        debug!("parsing termm");
        let mut lhs = self.parse_factor()?;
        loop {
            let token = self.tokens.peek()?;
            let Token::Operator(operator) = token else {
                break;
            };
            match operator.kind() {
                OperatorKind::Mul | OperatorKind::Div | OperatorKind::Mod => {
                    self.tokens.consume()?;
                    lhs =
                        BinaryOpTree::new(lhs.into(), self.parse_factor()?.into(), operator.kind())
                            .into()
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse_factor(&mut self) -> ParseResult<ExpressionTree> {
        debug!("parsing factor");
        match self.tokens.peek()? {
            Token::Separator(separator) if separator.kind() == SeparatorKind::ParenOpen => {
                self.tokens.consume()?;
                let expr = self.parse_expr()?;
                self.tokens.expect_separator(SeparatorKind::ParenClose)?;
                Ok(expr)
            }
            Token::Operator(operator) if operator.kind() == OperatorKind::Minus => {
                let span = self.tokens.consume()?.span();
                let tree = NegateTree::new(self.parse_factor()?.into(), span);
                Ok(tree.into())
            }
            Token::Identifier(ident) => {
                self.tokens.consume()?;
                let tree = IdentExprTree::new(Parser::name_tree(ident));
                Ok(tree.into())
            }
            Token::NumberLiteral(number_literal) => {
                self.tokens.consume()?;
                let tree = LiteralTree::new(
                    number_literal.value().into(),
                    number_literal.base(),
                    number_literal.span(),
                );
                Ok(tree.into())
            }
            token => Err(ParseError::InvalidFactor { got: token }),
        }
    }

    fn parse_assignment_op(&mut self) -> ParseResult<Operator> {
        debug!("parsing assignment operator");
        let token = self.tokens.peek()?;
        let Token::Operator(operator) = token else {
            return Err(ParseError::ExpectedAssignment { got: token });
        };
        match operator.kind() {
            OperatorKind::AssignMinus
            | OperatorKind::AssignPlus
            | OperatorKind::AssignMul
            | OperatorKind::AssignDiv
            | OperatorKind::AssignMod
            | OperatorKind::Assign => {
                self.tokens.consume()?;
                return Ok(operator);
            }
            _ => return Err(ParseError::ExpectedAssignment { got: token }),
        }
    }

    fn name_tree(ident: Identifier) -> NameTree {
        let span = ident.span();
        NameTree::new(ident.into(), span)
    }
}
