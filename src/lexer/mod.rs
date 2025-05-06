pub mod tokens;

use unicode_segmentation::UnicodeSegmentation;

use crate::span::{Position, Span};

use self::tokens::*;

#[derive(Debug, Clone)]
pub struct Lexer<'s> {
    /// the grapheme clusteres of a source
    source: Vec<&'s str>,
    /// the currently indexed cluster
    pos: usize,
    /// the index of the first cluster of the current line
    line_start: usize,
    /// the current line in regards to the source file (1-based)
    line: usize,
}

fn is_identifier_char(s: &str) -> bool {
    s.chars().all(|c| c == '_' || c.is_ascii_alphanumeric())
}

fn is_ascii_digit(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_digit())
}

fn is_hex(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_hexdigit())
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Lexer<'s> {
        let source = UnicodeSegmentation::graphemes(source, true).collect::<Vec<&str>>();
        Lexer {
            source: source,
            pos: 0,
            line_start: 0,
            line: 1,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(error_token) = self.skip_whitespace() {
            return Some(Token::ErrorToken(error_token));
        };
        if self.pos >= self.source.len() {
            return None;
        };
        let token = match self.peek(0) {
            ")" => self.lex_separator(SeparatorKind::ParenClose),
            "(" => self.lex_separator(SeparatorKind::ParenOpen),
            "}" => self.lex_separator(SeparatorKind::BraceClose),
            "{" => self.lex_separator(SeparatorKind::BraceOpen),
            ";" => self.lex_separator(SeparatorKind::Semicolon),
            "-" => self.lex_single_or_assign(OperatorKind::Minus, OperatorKind::AssignMinus),
            "+" => self.lex_single_or_assign(OperatorKind::Plus, OperatorKind::AssignPlus),
            "*" => self.lex_single_or_assign(OperatorKind::Mul, OperatorKind::AssignMul),
            "/" => self.lex_single_or_assign(OperatorKind::Div, OperatorKind::AssignDiv),
            "%" => self.lex_single_or_assign(OperatorKind::Mod, OperatorKind::AssignMod),
            "=" => Token::Operator(Operator::new(OperatorKind::Assign, self.build_span(1))),
            _ => {
                if is_identifier_char(self.peek(0)) {
                    if is_ascii_digit(self.peek(0)) {
                        self.lex_number()
                    } else {
                        self.lex_identifier_or_keyword()
                    }
                } else {
                    Token::ErrorToken(ErrorToken::new(self.peek(0).into(), self.build_span(1)))
                }
            }
        };
        Some(token)
    }

    fn lex_separator(&mut self, separator_kind: SeparatorKind) -> Token {
        Token::Separator(Separator::new(separator_kind, self.build_span(1)))
    }

    fn lex_single_or_assign(&mut self, single: OperatorKind, assign: OperatorKind) -> Token {
        let operator = if self.has_more(1) && self.peek(1) == "=" {
            Operator::new(assign, self.build_span(2))
        } else {
            Operator::new(single, self.build_span(1))
        };
        Token::Operator(operator)
    }

    fn skip_whitespace(&mut self) -> Option<ErrorToken> {
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum CommentKind {
            None,
            SingleLine,
            MultiLine,
        }

        let mut comment = CommentKind::None;
        let mut comment_start = 0usize;
        let mut multi_line_depth = 0usize;

        while self.has_more(0) {
            match self.peek(0) {
                " " | "\t" => self.pos += 1,
                "\n" | "\r" => {
                    self.pos += 1;
                    self.line_start = self.pos;
                    self.line += 1;
                    // we have moved to the next line, so unset comment state if we were on a single line comment
                    if matches!(comment, CommentKind::SingleLine) {
                        comment = CommentKind::None;
                    }
                }
                "/" => {
                    if matches!(comment, CommentKind::SingleLine) {
                        self.pos += 1;
                        continue;
                    };
                    // Detect Comments
                    if self.has_more(1) {
                        if self.peek(1) == "/" && matches!(comment, CommentKind::None) {
                            comment = CommentKind::SingleLine;
                        } else if self.peek(1) == "*" {
                            comment = CommentKind::MultiLine;
                            multi_line_depth += 1;
                        } else {
                            return None;
                        }
                        comment_start = self.pos;
                        // we looked at two chars
                        self.pos += 2;
                        continue;
                    }

                    if multi_line_depth > 0 {
                        self.pos += 1;
                        continue;
                    }
                    return None;
                }
                _ => {
                    match comment {
                        CommentKind::None => return None,
                        CommentKind::SingleLine => {
                            self.pos += 1;
                            continue;
                        }
                        CommentKind::MultiLine => {
                            let end_of_comment =
                                self.peek(0) == "*" && self.has_more(1) && self.peek(1) == "/";
                            if end_of_comment {
                                self.pos += 2;
                                multi_line_depth -= 1;
                                if multi_line_depth == 0 {
                                    comment = CommentKind::None;
                                }
                            } else {
                                self.pos += 1;
                            }
                            continue;
                        }
                    };
                }
            }
        }

        if !self.has_more(0) && comment == CommentKind::MultiLine {
            return Some(ErrorToken::new(
                self.source[comment_start..].concat().into_boxed_str(),
                self.build_span(0),
            ));
        }
        None
    }

    fn has_more(&self, offset: usize) -> bool {
        return self.pos + offset < self.source.len();
    }

    fn build_span(&mut self, proceed: usize) -> Span {
        let start = self.pos;
        self.pos += proceed;
        let s = Position::new(self.line, start - self.line_start);
        let e = Position::new(self.line, start - self.line_start + proceed);
        Span::new(s, e)
    }

    fn peek(&self, offset: usize) -> &str {
        return self.source[self.pos + offset];
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let mut offset = 1;
        while self.has_more(offset) && is_identifier_char(self.peek(offset)) {
            offset += 1;
        }
        let ident = self.get_value(offset);
        match KeywordKind::is_keyword(&ident) {
            Some(keyword_kind) => {
                Token::Keyword(Keyword::new(keyword_kind, self.build_span(offset)))
            }
            None => Token::Identifier(Identifier::new(
                ident.into_boxed_str(),
                self.build_span(offset),
            )),
        }
    }

    fn lex_number(&mut self) -> Token {
        if self.is_hex_prefix() {
            let mut offset = 2;
            while self.has_more(offset) && is_hex(self.peek(offset)) {
                offset += 1;
            }
            let value = self.get_value(offset).into_boxed_str();
            let span = self.build_span(offset);
            if offset == 2 {
                // `0x` without any more digits -> an error
                return Token::ErrorToken(ErrorToken::new(value, span));
            }
            return Token::NumberLiteral(NumberLiteral::new(value, 16, span));
        }
        // not a hex digit, parse base 10
        let mut offset = 1;
        while self.has_more(offset) && is_ascii_digit(self.peek(offset)) {
            offset += 1;
        }
        if self.peek(offset) == "0" && offset > 1 {
            // leading zero not allowed
            return Token::ErrorToken(ErrorToken::new(
                self.get_value(offset).into_boxed_str(),
                self.build_span(offset),
            ));
        }
        return Token::NumberLiteral(NumberLiteral::new(
            self.get_value(offset).into_boxed_str(),
            10,
            self.build_span(offset),
        ));
    }

    fn is_hex_prefix(&mut self) -> bool {
        self.peek(0) == "0" && self.has_more(1) && (self.peek(1) == "x" || self.peek(1) == "X")
    }

    fn get_value(&self, offset: usize) -> String {
        self.source[self.pos..self.pos + offset].concat()
    }
}
