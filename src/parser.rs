use crate::lexer::tokens::Token;

#[derive(Debug)]
struct TokenSource {
    tokens: Vec<Token>,
    idx: usize
}