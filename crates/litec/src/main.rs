#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]

use clap::{AppSettings, Parser as ArgParser};
use error::Reporter;
use lexer::{tokens::TokenKind, Lexer};
use parser::Parser;
use std::{fs::File, io::Read, path::Path};

#[derive(ArgParser)]
#[clap(version, about)]
#[clap(setting = AppSettings::ArgRequiredElseHelp)]
struct Args {
    /// The file to compile
    #[clap(value_parser)]
    filename: Option<String>,

    // Print the tokenized output of the source code
    #[clap(short = 'T', long, action)]
    print_tokens: bool,

    /// Print the parsed abstract syntax tree
    #[clap(short = 'A', long, action)]
    print_ast: bool,

    /// Compile code from a given string
    #[clap(short, long, value_parser)]
    eval: Option<String>,
}

fn tokenize(source: &str) {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();

    loop {
        let tok = lexer.next_token();

        if tok.0 == TokenKind::EoF {
            break;
        }

        tokens.push(tok.0);
    }

    for token in tokens {
        println!("{:?}", token);
    }
}

fn run_string(source: &str, filename: &str, print_tokens: bool, print_ast: bool) {
    if print_tokens {
        tokenize(source);
    }

    let mut parser = Parser::new(source, filename);
    let mut reporter = Reporter::new(vec![], source);

    let (ast, errors) = parser.parse();
    reporter.add_reports(errors);

    if print_ast {
        for node in ast {
            println!("{}", node.0);
        }
    }

    reporter.report().expect("Error while reporting errors");
}

fn compile_file(filename: &str, print_tokens: bool, print_ast: bool) {
    let path = Path::new(filename);
    let mut file = File::open(path).expect("Could not open file");
    let mut content = String::new();

    file.read_to_string(&mut content)
        .expect("Error while reading from file");

    run_string(&content, filename, print_tokens, print_ast);
}

fn main() {
    let args = Args::parse();

    if let Some(filename) = args.filename.as_deref() {
        compile_file(filename, args.print_tokens, args.print_ast);
    }

    if let Some(eval) = args.eval.as_deref() {
        run_string(eval, "main.lt", args.print_tokens, args.print_ast);
    }
}
