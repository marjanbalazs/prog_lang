use std::vec::Vec;
mod interpreter;
mod parser;
mod scanner;

use self::{
    interpreter::Interpreter,
    parser::Parser,
    scanner::{Scanner, Token},
};

pub fn run_script(code: &str) {
    let mut tokens: Vec<Token> = Vec::new();
    let mut scanner = Scanner::new(code, &mut tokens);
    scanner.scan_tokens();
    //scanner.print();

    let mut parser = Parser::new(&tokens);
    println!("{:?}", parser.parse_program());
    //parser.print();

    let mut interpreter = Interpreter::new(parser.get_ast());
    interpreter.interpret();
}
