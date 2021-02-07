use std::vec::Vec;
mod parser;
mod scanner;

use self::{parser::Parser, scanner::{Scanner, Token}};

pub fn run_script(code: &str) {
    println!("Running script...");
    let mut tokens: Vec<Token> = Vec::new();
    let mut scanner = Scanner::new(code, &mut tokens);
    scanner.scan_tokens();

    let mut parser = Parser::new(&tokens);
    parser.build_ast();
    println!("{:?}", parser.tree);
}
