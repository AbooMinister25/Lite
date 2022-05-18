use clap::Parser as ArgParser;
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

            for token in tokens {
                println!("{:?}", token)
            }
        } else {
            let mut parser = Parser::new(&content, &filename);
            let mut nodes = vec![];

            while !parser.at_end() {
                let (expr, _) = parser
                    .parse_expression(1)
                    .expect("Parser encountered an error");
                nodes.push(expr);
            }

            if args.output_ast {
                for node in nodes {
                    println!("{node}");
                }
            }
        }
    } else {
        println!("todo :p");
    }

    Ok(())
}
