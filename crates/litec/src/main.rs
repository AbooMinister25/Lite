#![warn(clippy::pedantic, clippy::nursery)]

use clap::Parser as ArgParser;

#[derive(ArgParser)]
#[clap(version, about)]
struct Args {
    /// The file to compile
    filename: Option<String>,
}

fn main() {
    let args = Args::parse();

    if let Some(filename) = args.filename.as_deref() {
        println!("{filename}");
    }
}
