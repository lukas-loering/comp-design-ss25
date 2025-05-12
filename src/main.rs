#![allow(unused)]
use std::{error::Error, path::PathBuf};

use backend::CodeGen;
use crow::CrowExitCodes;
use ir::{SsaTranslation, debug::GraphVizPrinter, optimize::LocalValueNumbering};
use lexer::{Lexer, tokens::Token};
use parser::{ParseResult, Parser, ProgrammTree, TokenSource};
use semantic::SemanticAnalysis;
use tracing::{Level, debug};

mod backend;
mod crow;
mod ir;
mod lexer;
mod parser;
mod semantic;
mod span;

fn main() -> Result<(), Box<dyn Error>> {
    let subscriber = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(Level::INFO)
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
    let programm = lex_and_parse(source)?;
    let mut semantic = SemanticAnalysis::new(&programm);
    semantic.analyze()?;
    debug!("{programm:#?}");
    let main = programm
        .top_level()
        .first()
        .expect("expect at least one function")
        .clone();
    let ir = SsaTranslation::new(main, LocalValueNumbering::default()).translate();
    let code = GraphVizPrinter::print(&ir);
    // let code = CodeGen::generate(main.body());
    // let code = code.join("\n");
    std::fs::write(output, code)?;
    Ok(())
}

pub fn lex_and_parse(source: String) -> ParseResult<ProgrammTree> {
    let mut lexer = Lexer::new(source.as_str());
    let mut parser = Parser::new(TokenSource::new(lexer));
    parser.parse_programm()
}
