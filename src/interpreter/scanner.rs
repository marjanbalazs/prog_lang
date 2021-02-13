use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum TokenType {
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
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // EOF
    Eof,
}

pub struct Token {
    pub line: usize,
    pub char_pos: usize,
    pub token_type: TokenType,
}

pub struct Scanner<'v> {
    source: String,
    tokens: &'v mut Vec<Token>,
    line: usize,
    char_pos: usize,
    keyword_map: HashMap<&'static str, TokenType>,
}

fn print_error(line: usize, what: &str, message: &str) {
    println!("[line: {}] Error: {}: {}", line, what, message);
}

impl<'v> Scanner<'v> {
    pub fn new(source: &str, vec: &'v mut Vec<Token>) -> Scanner<'v> {
        Scanner {
            source: source.to_owned(),
            tokens: vec,
            line: 0,
            char_pos: 0,
            keyword_map: {
                let mut keywords = HashMap::new();
                keywords.insert("and", TokenType::And);
                keywords.insert("class", TokenType::Class);
                keywords.insert("else", TokenType::Else);
                keywords.insert("false", TokenType::False);
                keywords.insert("fun", TokenType::Fun);
                keywords.insert("for", TokenType::For);
                keywords.insert("if", TokenType::If);
                keywords.insert("or", TokenType::Or);
                keywords.insert("print", TokenType::Print);
                keywords.insert("return", TokenType::Return);
                keywords.insert("super", TokenType::Super);
                keywords.insert("this", TokenType::This);
                keywords.insert("true", TokenType::True);
                keywords.insert("var", TokenType::Var);
                keywords.insert("while", TokenType::While);
                keywords
            },
        }
    }
    pub fn print(&self) {
        for x in self.tokens.iter() {
            print!("{:?}", x.token_type);
        }
    }
    fn peek_char(&self, idx: usize) -> Option<char> {
        self.source.chars().nth(idx)
    }
    fn create_token(&self, token: TokenType) -> Token {
        Token {
            line: self.line,
            char_pos: self.char_pos,
            token_type: token,
        }
    }
    pub fn scan_tokens(&mut self) {
        let mut iter = self.source.chars().enumerate();
        while let Some((pos, x)) = iter.next() {
            self.char_pos = pos;
            match x {
                '(' => self.tokens.push(self.create_token(TokenType::LeftParen)),
                ')' => self.tokens.push(self.create_token(TokenType::RightParen)),
                '{' => self.tokens.push(self.create_token(TokenType::LeftBrace)),
                '}' => self.tokens.push(self.create_token(TokenType::RightBrace)),
                ',' => self.tokens.push(self.create_token(TokenType::Comma)),
                '.' => self.tokens.push(self.create_token(TokenType::Dot)),
                '-' => self.tokens.push(self.create_token(TokenType::Minus)),
                '+' => self.tokens.push(self.create_token(TokenType::Plus)),
                ';' => self.tokens.push(self.create_token(TokenType::Semicolon)),
                '*' => self.tokens.push(self.create_token(TokenType::Star)),
                '!' => {
                    let next_char = self.peek_char(pos + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push(self.create_token(TokenType::BangEqual));
                            iter.next();
                        }
                        Some(_) => self.tokens.push(self.create_token(TokenType::Bang)),
                        None => print_error(self.line, "!", "not parasable"),
                    }
                }
                '=' => {
                    let next_char = self.peek_char(pos + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push(self.create_token(TokenType::EqualEqual));
                            iter.next();
                        }
                        Some(_) => self.tokens.push(self.create_token(TokenType::Equal)),
                        None => print_error(self.line, "=", "not parasable"),
                    }
                }
                '<' => {
                    let next_char = self.peek_char(pos + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push(self.create_token(TokenType::LessEqual));
                            iter.next();
                        }
                        Some(_) => self.tokens.push(self.create_token(TokenType::Less)),
                        None => print_error(self.line, "<", "not parasable"),
                    }
                }
                '>' => {
                    let next_char = self.peek_char(pos + 1);
                    match next_char {
                        Some('=') => {
                            self.tokens.push(self.create_token(TokenType::GreaterEqual));
                            iter.next();
                        }
                        Some(_) => self.tokens.push(self.create_token(TokenType::Greater)),
                        None => print_error(self.line, ">", "not parasable"),
                    }
                }
                '/' => {
                    let next_char = self.peek_char(pos + 1);
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
                        Some(_) => self.tokens.push(self.create_token(TokenType::Slash)),
                        None => print_error(self.line, ">", "not parasable"),
                    }
                }
                '"' => {
                    // String literal
                    let mut literal = String::new();
                    let res = loop {
                        let x = iter.next();
                        match x {
                            // Literals end on backslashes
                            Some((_, '\"')) => break Some(literal),
                            Some(c) => literal.push(c.1),
                            None => {
                                break None;
                            }
                        }
                    };
                    match res {
                        Some(r) => self.tokens.push(self.create_token(TokenType::String(r))),
                        None => print_error(self.line, "\"", "unterminated string"),
                    }
                }
                '0'..='9' => {
                    // Number literal
                    let mut literal = String::new();
                    literal.push(x);
                    let mut next_pos = pos + 1;
                    loop {
                        let next_char = self.peek_char(next_pos);
                        match next_char {
                            Some('.') => {
                                let (pos, chr) = iter.next().unwrap();
                                literal.push(chr);
                                next_pos = pos + 1;
                            }
                            Some(c) => match c {
                                '0'..='9' => {
                                    let (pos, chr) = iter.next().unwrap();
                                    literal.push(chr);
                                    next_pos = pos + 1;
                                }
                                _ => break,
                            },
                            None => break,
                        }
                    }
                    self.tokens.push(
                        self.create_token(TokenType::Number(literal.parse::<f64>().unwrap())),
                    );
                }
                ' ' => {}
                '\r' => {}
                '\t' => {}
                '\n' => self.line += 1,
                'a'..='z' | 'A'..='Z' | '_' => {
                    // This is so nice
                    let mut parsed = String::new();
                    parsed.push(x);
                    let mut next_pos = pos + 1;
                    loop {
                        let next_char = self.peek_char(next_pos);
                        match next_char {
                            Some(c) => match c {
                                'a'..='z' | 'A'..='Z' | '_' => {
                                    let (pos, chr) = iter.next().unwrap();
                                    parsed.push(chr);
                                    next_pos = pos + 1;
                                }
                                _ => break,
                            },
                            None => break,
                        }
                    }
                    match self.keyword_map.get(&parsed[..]) {
                        Some(key) => self.tokens.push(self.create_token(key.to_owned())),
                        None => self
                            .tokens
                            .push(self.create_token(TokenType::Identifier(parsed))),
                    }
                }
                _ => {}
            }
        }
        self.tokens.push(self.create_token(TokenType::Eof));
    }
}
