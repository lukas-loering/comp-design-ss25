#![allow(unused)]
use std::{error::Error, path::PathBuf};

use crow::CrowExitCodes;
use lexer::{Lexer, tokens::Token};
use parser::{ParseResult, Parser, ProgrammTree, TokenSource};
use tracing::Level;

mod crow;
mod lexer;
mod parser;
mod span;

fn main() -> Result<(), Box<dyn Error>> {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(Level::DEBUG)
        .finish();
    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");
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
    lex_and_parse(source)?;
    Ok(())
}

pub fn lex_and_parse(source: String) -> ParseResult<ProgrammTree> {
    let mut lexer = Lexer::new(source.as_str());
    let mut parser = Parser::new(TokenSource::new(lexer));
    parser.parse_programm()
}
