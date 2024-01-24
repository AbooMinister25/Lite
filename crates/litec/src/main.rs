#![warn(clippy::pedantic, clippy::nursery)]

use clap::Parser as ArgParser;
use color_eyre::Result;
use parser::Parser;

#[derive(ArgParser)]
#[clap(version, about)]
struct Args {
    /// The file to compile
    filename: Option<String>,

    /// Compile code from a given string
    #[clap(short, long)]
    eval: Option<String>,
}

fn main() {
    let args = Args::parse();

    if let Some(filename) = args.filename.as_deref() {
        println!("{filename}");
    }

    if let Some(s) = args.eval.as_deref() {
        let mut parser = Parser::new(s, "eval");

        let (nodes, errors) = parser.parse();

        for node in nodes {
            println!("{}", node.0);
        }

        for error in errors {
            println!("{error}");
        }
    }
}
