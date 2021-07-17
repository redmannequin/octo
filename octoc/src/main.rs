use std::io::Write;
use std::{error, io, result};

use octo::evaluator::eval;
use octo::lexer::Lexer;
use octo::obejct::Environment;
use octo::parser::Parser;

type Result<T> = result::Result<T, Box<dyn error::Error>>;

fn main() -> Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::new();

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
                let obj = eval(&program, &mut env);
                println!("--- {:?}", obj);
            }
            Err(err) => println!("parse error: {}", err),
        }
    }
}
