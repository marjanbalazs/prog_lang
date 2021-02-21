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
    let mut parser = Parser::new(&tokens);
    parser.parse_program();
    let mut interpreter = Interpreter::new(parser.get_ast());
    interpreter.interpret();
}
