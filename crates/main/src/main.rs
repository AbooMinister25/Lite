#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]

use clap::Parser as ArgParser;
use error::Reporter;
use lexer::{tokens::TokenKind, Lexer};
use parser::Parser;
use std::{fs::File, io::Read, path::Path};

#[derive(ArgParser)]
#[clap(author, version)]
struct Args {
    filename: Option<String>,
    #[clap(long)]
    tokenize: bool,
    #[clap(short, long)]
    output_ast: bool,
    #[clap(long)]
    compile_string: Option<String>,
}

fn run(source: String, filename: &str, tokenize: bool, output_ast: bool) {
    if tokenize {
        let mut lexer = Lexer::new(&source);
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
    } else {
        let mut parser = Parser::new(&source, filename);
        let mut reporter = Reporter::new(vec![], &source);

        let (ast, errors) = parser.parse();
        reporter.add_reports(errors);

        // let mut nodes = vec![];

        // let mut reporter = Reporter::new(vec![], &source);

        // while !parser.at_end() {
        //     let node = parser.parse_statement();

        //     match node {
        //         Ok(n) => nodes.push(n.0),
        //         Err(e) => reporter.add_report(Box::new(e)),
        //     }
        // }

        if output_ast {
            for node in ast {
                println!("{}", node.0);
            }
        }

        reporter.report().expect("Error while reporting errors");
    }
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();

    if let Some(filename) = args.filename {
        let path = Path::new(&filename);
        let mut file = File::open(path).expect("Could not open file");
        let mut content = String::new();

        file.read_to_string(&mut content)
            .expect("Error while reading from file");

        run(content, &filename, args.tokenize, args.output_ast)
    } else if let Some(content) = args.compile_string {
        run(content, "source.lt", args.tokenize, args.output_ast)
    } else {
        println!("todo :p");
    }

    Ok(())
}
