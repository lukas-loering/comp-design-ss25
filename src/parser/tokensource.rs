use crate::lexer::{
    Lexer,
    tokens::{
        Identifier, Keyword, KeywordKind, Operator, OperatorKind, Separator, SeparatorKind, Token,
    },
};

use super::ParseError;

#[derive(Debug, Clone)]
pub struct TokenSource {
    tokens: Box<[Token]>,
    idx: usize,
}

impl TokenSource {
    pub fn new(lexer: Lexer<'_>) -> Self {
        Self {
            tokens: lexer.collect(),
            idx: 0,
        }
    }

    pub fn peek(&self) -> Result<Token, ParseError> {
        self.has_more()?;
        let token = self.tokens[self.idx].clone();
        Ok(token)
    }

    pub fn consume(&mut self) -> Result<Token, ParseError> {
        let token = self.peek()?;
        self.idx += 1;
        Ok(token)
    }

    #[must_use]
    pub fn expect_keyword(&mut self, kind: KeywordKind) -> Result<Keyword, ParseError> {
        let token = self.peek()?;
        match token {
            Token::Keyword(k) if k.kind() == kind => {
                self.idx += 1;
                Ok(k)
            }
            _ => Err(ParseError::ExpectedKeyword {
                expected: kind,
                got: token,
            }),
        }
    }

    #[must_use]
    pub fn expect_separator(&mut self, kind: SeparatorKind) -> Result<Separator, ParseError> {
        let token = self.peek()?;
        match token {
            Token::Separator(s) if s.kind() == kind => {
                self.idx += 1;
                Ok(s)
            }
            _ => Err(ParseError::ExpectedSeparator {
                expected: kind,
                got: token,
            }),
        }
    }

    #[must_use]
    pub fn expect_operator(&mut self, kind: OperatorKind) -> Result<Operator, ParseError> {
        let token = self.peek()?;
        match token {
            Token::Operator(o) if o.kind() == kind => {
                self.idx += 1;
                Ok(o)
            },
            _ => Err(ParseError::ExpectedOperator {
                expected: kind,
                got: token,
            }),
        }
    }

    #[must_use]
    pub fn expect_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = self.peek()?;
        match token {
            Token::Identifier(i) => {
                self.idx += 1;
                Ok(i)
            },
            _ => Err(ParseError::ExpectedIdentifier { got: token }),
        }
    }

    fn has_more(&self) -> Result<(), ParseError> {
        if self.idx >= self.tokens.len() {
            return Err(ParseError::EndOfFile);
        }
        Ok(())
    }
}
