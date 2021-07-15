use std::io::Write;
use std::{error, io, result};

use octo::lexer::Lexer;
use octo::parser::Parser;

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        let mut input = String::new();
        print!(">> ");
        stdout.flush().expect("Error faild to flush");
        stdin
            .read_line(&mut input)
            .expect("Error reading from STDIN");

        let lexer = Lexer::new(&input);
        match Parser::new(lexer).parse() {
            Ok(program) => {
                println!("--- {:?}", program);
            }
            Err(err) => println!("parse error: {}", err),
        }
    }
}
