use std::{collections::VecDeque, convert::TryInto};
#[derive(Debug)]
enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

struct Token {
    lexeme: String,
    tokenType: TokenType,
    line: u32,
}

pub fn run_script(code: &String) {
    let mut tokens: VecDeque<TokenType> = VecDeque::new();
    let mut scanner = Scanner::new(code, &mut tokens);
    scanner.scan_tokens();
    print!("{:?}", tokens);
}

struct Scanner<'v> {
    source: String,
    tokens: &'v mut VecDeque<TokenType>,
    state: String,
    start: usize,
    current: usize,
    line: usize,
}

fn print_error(line: usize, what: &str, message: &str) {
    println!("[line: {}] Error: {}: {}", line, what, message);
}

impl<'v> Scanner<'v> {
    pub fn new(source: &String, vec: &'v mut VecDeque<TokenType>) -> Scanner<'v> {
        Scanner {
            source: source.to_owned(),
            start: 0,
            tokens: vec,
            state: "".to_owned(),
            current: 0,
            line: 0,
        }
    }
    fn peek_char(&self, idx: usize) -> Option<char> {
        self.source.chars().nth(idx)
    }

    pub fn scan_tokens(&mut self) {
        let mut iter = self.source.chars().enumerate();
        while let Some((u, x)) = iter.next() {
            match x {
                '(' => self.tokens.push_back(TokenType::LeftParen),
                ')' => self.tokens.push_back(TokenType::RightParen),
                '{' => self.tokens.push_back(TokenType::LeftBrace),
                '}' => self.tokens.push_back(TokenType::RightBrace),
                ',' => self.tokens.push_back(TokenType::Comma),
                '.' => self.tokens.push_back(TokenType::Dot),
                '-' => self.tokens.push_back(TokenType::Minus),
                '+' => self.tokens.push_back(TokenType::Plus),
                ';' => self.tokens.push_back(TokenType::Semicolon),
                '*' => self.tokens.push_back(TokenType::Star),
                '!' => {
                    let next_char = self.peek_char(u + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push_back(TokenType::BangEqual);
                            iter.next();
                        }
                        Some(_) => self.tokens.push_back(TokenType::Bang),
                        None => print_error(self.line, "!", "not parasable"),
                    }
                }
                '=' => {
                    let next_char = self.peek_char(u + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push_back(TokenType::EqualEqual);
                            iter.next();
                        }
                        Some(_) => self.tokens.push_back(TokenType::Equal),
                        None => print_error(self.line, "=", "not parasable"),
                    }
                }
                '<' => {
                    let next_char = self.peek_char(u + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push_back(TokenType::LessEqual);
                            iter.next();
                        }
                        Some(_) => self.tokens.push_back(TokenType::Less),
                        None => print_error(self.line, "<", "not parasable"),
                    }
                }
                '>' => {
                    let next_char = self.peek_char(u + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push_back(TokenType::GreaterEqual);
                            iter.next();
                        }
                        Some(_) => self.tokens.push_back(TokenType::Greater),
                        None => print_error(self.line, ">", "not parasable"),
                    }
                }
                '/' => {
                    let next_char = self.peek_char(u + 1);
                    match next_char {
                        Some('/') => {
                            // This is a comment
                            loop {
                                let x = iter.next();
                                match x {
                                    // Comments end on linebreaks
                                    Some((_, '\n')) => break,
                                    Some((_, _)) => {}
                                    None => {
                                        print_error(self.line, "/", "not parsable");
                                        break;
                                    }
                                }
                            }
                        }
                        Some(_) => self.tokens.push_back(TokenType::Slash),
                        None => print_error(self.line, ">", "not parasable"),
                    }
                }
                '"' => {
                    // String literal
                    let mut literal = String::new();
                    loop {
                        let x = iter.next();
                        match x {
                            // Literals end on backslashes
                            Some((_, '\"')) => break,
                            Some(c) => literal.push(c.1),
                            None => {
                                print_error(self.line, "\"", "unterminated string");
                                break;
                            }
                        }
                    }
                    self.tokens.push_back(TokenType::String(literal))
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    // Number literal
                    let mut literal = String::new();
                    literal.push(x);
                    loop {
                        let next_char = self.peek_char(u + 1);
                        match next_char {
                            Some(c) => {
                                match c.to_digit(10) {
                                    Some(x) => {
                                        literal.push(iter.next().unwrap().1);
                                    },
                                    // Next char is not a digit
                                    None => break,
                                }
                            },
                            // No more chars in the source
                            None => break
                        } 
                    }
                    // Awful
                    self.tokens.push_back(TokenType::Number(literal.chars().rev().collect::<String>().parse::<f64>().unwrap()))
                }
                ' ' => {}
                '\r' => {}
                '\t' => {}
                '\n' => self.line += 1,
                _ => {}
            }
        }
    }
}
