use tokensource::TokenSource;

use crate::lexer::tokens::{Identifier, KeywordKind, OperatorKind, SeparatorKind, Token};

mod ast;
mod kind;
mod symbol;
mod tokensource;
mod visitor;

#[derive(Debug, Clone, thiserror::Error)]
#[non_exhaustive]
enum ParseError {
    #[error("expected operator {expected} got {got}")]
    ExpectedOperator { expected: OperatorKind, got: Token },
    #[error("expected keyword {expected} got {got}")]
    ExpectedKeyword { expected: KeywordKind, got: Token },
    #[error("expected identifier got {got}")]
    ExpectedIdentifier { got: Token },
    #[error("expected separator {expected} got {got}")]
    ExpectedSeparator { expected: SeparatorKind, got: Token },
    #[error("reached end of file")]
    EndOfFile,
}

#[derive(Debug, Clone)]
struct Parser {
    tokens: TokenSource,
}

impl Parser {
    pub fn new(tokens: TokenSource) -> Self {
        Self { tokens }
    }
}
