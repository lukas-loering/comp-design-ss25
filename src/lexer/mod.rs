pub mod tokens;

use tracing::trace;
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    parser::NumericBase,
    span::{Position, Span},
};
use derive_more::Debug;

use self::tokens::*;

#[derive(Debug, Clone)]
pub struct Lexer<'s> {
    /// the grapheme clusteres of a source
    #[debug(skip)]
    source: Vec<&'s str>,
    /// the currently indexed cluster
    pos: usize,
    /// the index of the first cluster of the current line
    line_start: usize,
    /// the current line in regards to the source file (1-based)
    line: usize,
}

impl<'s> Iterator for Lexer<'s> {
    type Item = tokens::Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
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

    #[tracing::instrument]
    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(error_token) = self.skip_whitespace().map(Token::from) {
            trace!("found error token");
            return Some(error_token);
        };
        trace!("skipped whitespace");
        if self.pos >= self.source.len() {
            trace!("read to/past end of source");
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
                    Token::ErrorToken(ErrorToken::new(
                        self.peek(0).into(),
                        "invalid token".into(),
                        self.build_span(1),
                    ))
                }
            }
        };
        Some(token)
    }

    #[tracing::instrument]
    fn lex_separator(&mut self, separator_kind: SeparatorKind) -> Token {
        trace!("lex separator");
        Separator::new(separator_kind, self.build_span(1)).into()
    }

    #[tracing::instrument]
    fn lex_single_or_assign(&mut self, single: OperatorKind, assign: OperatorKind) -> Token {
        trace!("lex single or assign");
        let operator = if self.has_more(1) && self.peek(1) == "=" {
            Operator::new(assign, self.build_span(2))
        } else {
            Operator::new(single, self.build_span(1))
        };
        operator.into()
    }

    #[tracing::instrument]
    fn skip_whitespace(&mut self) -> Option<ErrorToken> {
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum CommentKind {
            None,
            SingleLine,
            MultiLine,
        }

        trace!("skip whitespace");
        let mut comment = CommentKind::None;
        let mut comment_start = 0usize;
        let mut multi_line_depth = 0usize;

        while self.has_more(0) {
            match self.peek(0) {
                " " | "\t" => {
                    trace!("space or tab");
                    self.pos += 1
                }
                "\n" | "\r" => {
                    trace!("new line");
                    self.pos += 1;
                    self.line_start = self.pos;
                    self.line += 1;
                    // we have moved to the next line, so unset comment state if we were on a single line comment
                    if comment == CommentKind::SingleLine {
                        comment = CommentKind::None;
                    }
                }
                "/" => {
                    trace!("fwd slash");
                    if comment == CommentKind::SingleLine {
                        self.pos += 1;
                        continue;
                    };
                    // Detect Comments
                    if self.has_more(1) {
                        if self.peek(1) == "/" && comment == CommentKind::None {
                            comment = CommentKind::SingleLine;
                        } else if self.peek(1) == "*" {
                            comment = CommentKind::MultiLine;
                            multi_line_depth += 1;
                        } else if comment == CommentKind::MultiLine {
                            self.pos += 1;
                            continue;
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
                    trace!("other");
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
                                } else {
                                    comment = CommentKind::MultiLine;
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
                "unclosed multi-line comment".into(),
                self.build_span(0),
            ));
        }
        None
    }

    fn has_more(&self, offset: usize) -> bool {
        trace!(
            "has more? pos: {}, len: {}",
            self.pos + offset,
            self.source.len()
        );
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
            Some(keyword_kind) => Keyword::new(keyword_kind, self.build_span(offset)).into(),
            None => Identifier::new(ident.into_boxed_str(), self.build_span(offset)).into(),
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
                return ErrorToken::new(value, "orphaned `0x`".into(), span).into();
            }
            return NumberLiteral::new(value, NumericBase::Hex, span).into();
        }
        // not a hex digit, parse base 10
        let mut offset = 1;
        while self.has_more(offset) && is_ascii_digit(self.peek(offset)) {
            offset += 1;
        }
        if self.peek(offset) == "0" && offset > 1 {
            // leading zero not allowed
            return ErrorToken::new(
                self.get_value(offset).into_boxed_str(),
                "leading zeros are not allowed for numeric literals".into(),
                self.build_span(offset),
            )
            .into();
        }
        return NumberLiteral::new(
            self.get_value(offset).into_boxed_str(),
            NumericBase::Decimal,
            self.build_span(offset),
        )
        .into();
    }

    fn is_hex_prefix(&mut self) -> bool {
        self.peek(0) == "0" && self.has_more(1) && (self.peek(1) == "x" || self.peek(1) == "X")
    }

    fn get_value(&self, offset: usize) -> String {
        self.source[self.pos..self.pos + offset].concat()
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;

    // #[test]
    // fn lex_assignment() {
    //     let s = "
    //     foo = bar + 5;
    //     ";
    //     let mut l = Lexer::new(s);
    //     assert_eq!(Some(l.next())
    // }
}
