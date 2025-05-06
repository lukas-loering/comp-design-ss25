use crate::lexer::tokens::{Identifier, KeywordKind, OperatorKind, SeparatorKind, Token};

mod tokensource;

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
