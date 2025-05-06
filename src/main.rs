#![allow(unused)]
use std::path::PathBuf;

use crow::CrowExitCodes;
use lexer::{Lexer, tokens::Token};

mod crow;
mod lexer;
mod parser;
mod span;

fn main() {
    let mut args = std::env::args();
    // the first arg is the programm name
    if args.len() != 3 {
        eprintln!("expected exactly two arguments: <INPUT_FILE> <OUTPUT_FILE>");
        std::process::exit(3);
    }
    let mut args = args.skip(1);
    let input = PathBuf::from(args.next().expect("exactly 2 args"));
    let output = PathBuf::from(args.next().expect("exactly 2 args"));
    let Ok(source) = std::fs::read_to_string(&input) else {
        eprintln!("file {} not found", input.display());
        std::process::exit(3)
    };
    lex_and_parse(source);
}

pub fn lex_and_parse(source: String) -> () {
    let mut lexer = Lexer::new(source.as_str());
    let mut tokens = vec![];
    while let Some(token) = lexer.next_token() {
        if matches!(token, Token::ErrorToken { .. }) {
            eprintln!("ERR: {token:#?}");
            std::process::exit(CrowExitCodes::ParsingErr.into())
        }
        println!("{token}");
        tokens.push(token);
    }
    println!("{tokens:#?}")
}
