use clap::Parser as ArgParser;
use lexer::{tokens::TokenKind, Lexer};
use parser::Parser;
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
    time::Instant,
};

#[derive(ArgParser)]
#[clap(author, version)]
struct Args {
    filename: Option<String>,
    #[clap(long)]
    tokenize: bool,
    #[clap(short, long)]
    output_ast: bool,
}

fn main() -> std::io::Result<()> {
    let args = Args::parse();

    if let Some(filename) = args.filename {
        let path = Path::new(&filename);
        let mut file = File::open(path).expect("Could not open file");
        let mut content = String::new();

        file.read_to_string(&mut content)
            .expect("Error while reading from file");

        if args.tokenize {
            let mut lexer = Lexer::new(&content);
            let mut tokens = Vec::new();

            loop {
                let tok = lexer.next_token();

                if tok.0 == TokenKind::EoF {
                    break;
                }

                tokens.push(tok.0);
            }

            println!("Tokens: {:#?}", tokens);
            println!("------");
        }

        let mut parser = Parser::new(&content, &filename);
        let (node, _) = parser.parse_expression(1).expect("Error while parsing");

        if args.output_ast {
            println!("Generated AST:");
            println!("{node}");
        }
    } else {
        println!("todo :p");
    }

    Ok(())
}
