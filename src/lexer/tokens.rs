use std::fmt::Display;

use crate::{
    parser::NumericBase,
    span::{HasSpan, Span},
};

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(Identifier),
    Operator(Operator),
    ErrorToken(ErrorToken),
    NumberLiteral(NumberLiteral),
    Separator(Separator),
}

impl Token {
    pub fn is_separator(&self, separator: SeparatorKind) -> bool {
        matches!(self, Self::Separator(Separator { span: _, kind }) if *kind == separator)
    }

    pub fn is_keyword(&self, keyword: KeywordKind) -> bool {
        matches!(self, Self::Keyword(Keyword { span: _, kind }) if *kind == keyword)
    }

    pub fn is_operator(&self, operator: OperatorKind) -> bool {
        matches!(self, Self::Operator(Operator { span: _, kind }) if *kind == operator)
    }
}

impl HasSpan for Token {
    fn span(&self) -> Span {
        match self {
            Token::Keyword(keyword) => keyword.span(),
            Token::Identifier(identifier) => identifier.span(),
            Token::Operator(operator) => operator.span(),
            Token::ErrorToken(error_token) => error_token.span(),
            Token::NumberLiteral(number_literal) => number_literal.span(),
            Token::Separator(separator) => separator.span(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::Identifier(identifier) => write!(f, "{identifier}"),
            Token::Operator(operator) => write!(f, "{operator}"),
            Token::ErrorToken(error_token) => write!(f, "{error_token}"),
            Token::NumberLiteral(number_literal) => write!(f, "{number_literal}"),
            Token::Separator(separator) => write!(f, "{separator}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    value: Box<str>,
    span: Span,
}

impl Identifier {
    pub fn new(value: Box<str>, span: Span) -> Self {
        Self { value, span }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl HasSpan for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<Identifier> for Token {
    fn from(value: Identifier) -> Self {
        Self::Identifier(value)
    }
}

#[derive(Debug, Clone)]
pub struct ErrorToken {
    value: Box<str>,
    span: Span,
    err_msg: Box<str>,
}

impl ErrorToken {
    pub fn new(value: Box<str>, err_msg: Box<str>, span: Span) -> Self {
        Self {
            value,
            span,
            err_msg,
        }
    }
}

impl HasSpan for ErrorToken {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for ErrorToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<ErrorToken> for Token {
    fn from(value: ErrorToken) -> Self {
        Self::ErrorToken(value)
    }
}

#[derive(Debug, Clone)]
pub struct NumberLiteral {
    value: Box<str>,
    base: NumericBase,
    span: Span,
}

impl NumberLiteral {
    pub fn new(value: Box<str>, base: NumericBase, span: Span) -> Self {
        Self { value, base, span }
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn base(&self) -> NumericBase {
        self.base
    }
}

impl HasSpan for NumberLiteral {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for NumberLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<NumberLiteral> for Token {
    fn from(value: NumberLiteral) -> Self {
        Self::NumberLiteral(value)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Separator {
    kind: SeparatorKind,
    span: Span,
}

impl Separator {
    pub fn new(kind: SeparatorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> SeparatorKind {
        self.kind
    }
}

impl HasSpan for Separator {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl From<Separator> for Token {
    fn from(value: Separator) -> Self {
        Self::Separator(value)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SeparatorKind {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
}

impl Display for SeparatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            SeparatorKind::ParenOpen => "(",
            SeparatorKind::ParenClose => ")",
            SeparatorKind::BraceOpen => "{",
            SeparatorKind::BraceClose => "}",
            SeparatorKind::Semicolon => ";",
        };
        write!(f, "{s}")
    }
}
#[derive(Debug, Clone, Copy)]
pub struct Operator {
    kind: OperatorKind,
    span: Span,
}

impl Operator {
    pub fn new(kind: OperatorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> OperatorKind {
        self.kind
    }
}

impl HasSpan for Operator {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl From<Operator> for Token {
    fn from(value: Operator) -> Self {
        Self::Operator(value)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OperatorKind {
    AssignMinus,
    Minus,
    AssignPlus,
    Plus,
    AssignMul,
    Mul,
    AssignDiv,
    Div,
    AssignMod,
    Mod,
    Assign,
}

impl Display for OperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            OperatorKind::AssignMinus => "-=",
            OperatorKind::Minus => "-",
            OperatorKind::AssignPlus => "+=",
            OperatorKind::Plus => "+",
            OperatorKind::AssignMul => "*=",
            OperatorKind::Mul => "*",
            OperatorKind::AssignDiv => "/=",
            OperatorKind::Div => "/",
            OperatorKind::AssignMod => "%=",
            OperatorKind::Mod => "%",
            OperatorKind::Assign => "=",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Keyword {
    kind: KeywordKind,
    span: Span,
}

impl Keyword {
    pub fn new(kind: KeywordKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> KeywordKind {
        self.kind
    }
}

impl HasSpan for Keyword {
    fn span(&self) -> Span {
        self.span
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl From<Keyword> for Token {
    fn from(value: Keyword) -> Self {
        Self::Keyword(value)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum KeywordKind {
    Struct,
    If,
    Else,
    While,
    For,
    Continue,
    Break,
    Return,
    Assert,
    True,
    False,
    Null,
    Print,
    Read,
    Alloc,
    AllocArray,
    Int,
    Bool,
    Void,
    Char,
    String,
}

impl KeywordKind {
    pub fn is_keyword(input: &str) -> Option<KeywordKind> {
        match input {
            "struct" => Some(KeywordKind::Struct),
            "if" => Some(KeywordKind::If),
            "else" => Some(KeywordKind::Else),
            "while" => Some(KeywordKind::While),
            "for" => Some(KeywordKind::For),
            "continue" => Some(KeywordKind::Continue),
            "break" => Some(KeywordKind::Break),
            "return" => Some(KeywordKind::Return),
            "assert" => Some(KeywordKind::Assert),
            "true" => Some(KeywordKind::True),
            "false" => Some(KeywordKind::False),
            "NULL" => Some(KeywordKind::Null),
            "print" => Some(KeywordKind::Print),
            "read" => Some(KeywordKind::Read),
            "alloc" => Some(KeywordKind::Alloc),
            "alloc_array" => Some(KeywordKind::AllocArray),
            "int" => Some(KeywordKind::Int),
            "bool" => Some(KeywordKind::Bool),
            "void" => Some(KeywordKind::Void),
            "char" => Some(KeywordKind::Char),
            "string" => Some(KeywordKind::String),
            _ => None,
        }
    }
}

impl Display for KeywordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            KeywordKind::Struct => "struct",
            KeywordKind::If => "if",
            KeywordKind::Else => "else",
            KeywordKind::While => "while",
            KeywordKind::For => "for",
            KeywordKind::Continue => "continue",
            KeywordKind::Break => "break",
            KeywordKind::Return => "return",
            KeywordKind::Assert => "assert",
            KeywordKind::True => "true",
            KeywordKind::False => "false",
            KeywordKind::Null => "NULL",
            KeywordKind::Print => "print",
            KeywordKind::Read => "read",
            KeywordKind::Alloc => "alloc",
            KeywordKind::AllocArray => "alloc_array",
            KeywordKind::Int => "int",
            KeywordKind::Bool => "bool",
            KeywordKind::Void => "void",
            KeywordKind::Char => "char",
            KeywordKind::String => "string",
        };
        write!(f, "{s}")
    }
}
