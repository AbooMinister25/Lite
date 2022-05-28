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
            println!("{:?}", token)
        }
    } else {
        let mut parser = Parser::new(&source, filename);
        let mut nodes = vec![];

        while !parser.at_end() {
            let (node, _) = parser.parse_statement().unwrap_or_else(|_| {
                panic!("Parser encountered an error. Recovered AST: {:?}", nodes)
            });
            nodes.push(node);
            println!("{:?} {:?}", parser.at_end(), nodes);
        }

        if output_ast {
            for node in nodes {
                println!("{node}");
            }
        }
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
