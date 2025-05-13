#![allow(unused)]
use std::{error::Error, path::PathBuf};

use backend::{CodeGenerator, TrivialRegisterProvider, X86_64Asm};
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

fn main() {
    match main_impl() {
        Ok(_) => std::process::exit(CrowExitCodes::Success.into()),
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn main_impl() -> Result<(), Box<dyn Error>> {
    if cfg!(debug_assertions) {
        let subscriber = tracing_subscriber::FmtSubscriber::builder()
            .with_max_level(Level::TRACE)
            .finish();
        tracing::subscriber::set_global_default(subscriber)
            .expect("setting default subscriber failed");
    }
    let mut args = std::env::args();
    // the first arg is the programm name
    if args.len() != 3 {
        eprintln!("expected exactly two arguments: <INPUT_FILE> <OUTPUT_FILE>");
        std::process::exit(3);
    }
    let mut args = args.skip(1);
    let input = PathBuf::from(args.next().expect("exactly 2 args"));
    let output = PathBuf::from(args.next().expect("exactly 2 args"));
    let source = std::fs::read_to_string(&input)?;
    let Ok(programm) = lex_and_parse(source).inspect_err(|e| std::eprintln!("ParseError: {e}"))
    else {
        std::process::exit(CrowExitCodes::ParsingErr.into())
    };
    let mut semantic = SemanticAnalysis::new(&programm);
    if let Err(e) = semantic.analyze() {
        std::eprintln!("SemanticError: {e}");
        std::process::exit(CrowExitCodes::SemanticErr.into());
    };
    debug!("{programm:#?}");
    let main = programm
        .top_level()
        .first()
        .expect("expect at least one function")
        .clone();
    let ir = SsaTranslation::new(main, LocalValueNumbering::default()).translate();
    if cfg!(debug_assertions) {
        let debug_graph = GraphVizPrinter::print(&ir);
        std::fs::write(format!("{}.viz", &output.display()), debug_graph)?;
    }
    let mut code_gen = X86_64Asm::new(&ir, TrivialRegisterProvider::default()).generate()?;

    std::fs::write(
        format!("{}.s", &output.display()),
        format!("{ASM_TEMPLATE}{code_gen}"),
    )?;
    std::process::Command::new("gcc")
        .args(&[
            format!("{}.s", &output.display()).into(),
            PathBuf::from("-o"),
            output,
        ])
        .status()?;
    Ok(())
}

pub fn lex_and_parse(source: String) -> ParseResult<ProgrammTree> {
    let mut lexer = Lexer::new(source.as_str());
    let mut parser = Parser::new(TokenSource::new(lexer));
    parser.parse_programm()
}

const ASM_TEMPLATE: &'static str = r#"
.global main
.global _main
.text

main:
call _main
movq %rax, %rdi
movq $0x3C, %rax
syscall

"#;
