use super::scanner::{Token, TokenType};
use std::format;

/*
program        → declaration* EOF ;

declaration    → classDecl
               | funDecl
               | varDecl
               | statement ;
classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
                 "{" function* "}" ;
funDecl        → "fun" function ;
varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;
exprStmt       → expression ";" ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                           expression? ";"
                           expression? ")" statement ;
ifStmt         → "if" "(" expression ")" statement
                 ( "else" statement )? ;
printStmt      → "print" expression ";" ;
returnStmt     → "return" expression? ";" ;
whileStmt      → "while" "(" expression ")" statement ;
block          → "{" declaration* "}" ;

expression     → assignment ;

assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;

logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;

unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
primary        → "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;

function       → IDENTIFIER "(" parameters? ")" block ;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
arguments      → expression ( "," expression )* ;

NUMBER         → DIGIT+ ( "." DIGIT+ )? ;
STRING         → "\"" <any char except "\"">* "\"" ;
IDENTIFIER     → ALPHA ( ALPHA | DIGIT )* ;
ALPHA          → "a" ... "z" | "A" ... "Z" | "_" ;
DIGIT          → "0" ... "9" ;
*/

pub struct Parser<'a> {
    tree: Vec<Decl>,
    current_node: usize,
    tokens: &'a Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser {
            tree: Vec::new(),
            current_node: 0,
            tokens,
        }
    }
    pub fn get_ast(&self) -> &Vec<Decl> {
        &self.tree
    }
    fn match_token(&mut self, token_to_match: &TokenType) -> Result<(), String> {
        match self.tokens.get(self.current_node) {
            Some(token) => {
                let token_type = &token.token_type;
                let line = &token.line;
                if token_type == token_to_match {
                    Ok(())
                } else {
                    let err = format!(
                        "Expected token: {:?}, found: {:?}, line {}, {:?}",
                        token_to_match, token_type, line, self.tree,
                    );
                    Err(err)
                }
            }
            None => Err("No token found".to_owned()),
        }
    }
    pub fn parse_program(&mut self) -> Result<(), String> {
        while let Some(token) = self.tokens.get(self.current_node) {
            match token.token_type {
                TokenType::Eof => break,
                _ => {
                    // Start the chain
                    let decl = self.parse_decl()?;
                    self.tree.push(decl);
                }
            }
        }
        Ok(())
    }
    fn parse_decl(&mut self) -> Result<Decl, String> {
        match self.tokens.get(self.current_node) {
            Some(tok) => match tok.token_type {
                TokenType::Var => {
                    let (ident, expr) = self.parse_var()?;
                    Ok(Decl::Var(ident, Box::new(expr)))
                }
                TokenType::Fun => {
                    self.current_node += 1;
                    let name = match self.tokens.get(self.current_node) {
                        Some(t) => match &t.token_type {
                            TokenType::Identifier(ident) => ident,
                            _ => return Err("No identifier found after fun decl keyword".to_owned())
                        }
                        None => return Err("Unexpected end of tokens at function decl".to_owned()),
                    };
                    self.match_token(&TokenType::LeftParen)?;
                    let mut params = Vec::new();
                    loop {
                        self.current_node += 1;
                        let current_token = self.tokens.get(self.current_node);
                        let name = match current_token {
                            Some(token) => match &token.token_type {
                                TokenType::RightParen => {
                                    break;
                                },
                                TokenType::Comma => {
                                    continue;
                                }
                                TokenType::Identifier(ident) => ident,
                                _ => return Err("Could not parse function arg".to_owned())
                            },
                            None => {
                                return Err("No more tokens at call argument parsing".to_owned())
                            }
                        };
                        params.push(name.to_owned());
                    }
                    let body = self.parse_block()?;
                    Ok(Decl::Func(name.to_owned(), params, body))
                },
                _ => {
                    // Assume a statement
                    let stmt = self.parse_stmt()?;
                    Ok(Decl::Statement(Box::new(stmt)))
                }
            },
            _ => Err("Impossible".to_owned()),
        }
    }
    fn parse_var(&mut self) -> Result<(String, Option<Expression>), String> {
        // Identifier
        match self.tokens.get(self.current_node) {
            Some(tok) => match tok.token_type {
                TokenType::Var => {
                    self.current_node += 1;
                }
                _ => return Err("This is not a variable declaration".to_owned()),
            },
            _ => return Err("Impossible".to_owned()),
        }
        let ident = match self.tokens.get(self.current_node) {
            Some(token) => match &token.token_type {
                TokenType::Identifier(s) => s.to_owned(),
                _ => return Err("Expected identifier after keyword Var".to_owned()),
            },
            None => return Err("Unexpected end of tokens".to_owned()),
        };
        // Initializer
        self.current_node += 1;
        match self.tokens.get(self.current_node) {
            Some(token) => match token.token_type {
                TokenType::Equal => {
                    self.current_node += 1;
                    let expr = self.parse_expr()?;
                    self.match_token(&TokenType::Semicolon)?;
                    self.current_node += 1;
                    Ok((ident, Some(expr)))
                }
                TokenType::Semicolon => {
                    self.current_node += 1;
                    Ok((ident, None))
                }
                _ => Err("Unexpected token after variable declaration".to_owned()),
            },
            None => Err("Unexpected end of token stream".to_owned()),
        }
    }
    fn parse_stmt(&mut self) -> Result<Statement, String> {
        match self.tokens.get(self.current_node) {
            Some(tok) => {
                match tok.token_type {
                    TokenType::Print => {
                        self.current_node += 1;
                        let expr = self.parse_expr()?;
                        self.match_token(&TokenType::Semicolon)?;
                        self.current_node += 1;
                        Ok(Statement::Print(Box::new(expr)))
                    }
                    TokenType::LeftBrace => {
                        let block = self.parse_block()?;
                        Ok(Statement::Block(block))
                    }
                    TokenType::If => {
                        self.current_node += 1;
                        self.match_token(&TokenType::LeftParen)?;
                        self.current_node += 1;
                        let condition = self.parse_expr()?;
                        self.match_token(&TokenType::RightParen)?;
                        self.current_node += 1;
                        self.match_token(&TokenType::LeftBrace)?;
                        self.current_node += 1;
                        let then_block = self.parse_block()?;
                        self.current_node += 1;
                        let else_block = match self.tokens.get(self.current_node) {
                            Some(token) => match token.token_type {
                                TokenType::Else => {
                                    self.current_node += 1;
                                    self.match_token(&TokenType::LeftBrace)?;
                                    self.current_node += 1;
                                    let block = self.parse_block()?;
                                    self.current_node += 1;
                                    Some(Statement::Block(block))
                                }
                                _ => None,
                            },
                            None => return Err("Unexpected end of token stream".to_owned()),
                        };
                        Ok(Statement::If(
                            Box::new(condition),
                            Box::new(Statement::Block(then_block)),
                            Box::new(else_block),
                        ))
                    }
                    TokenType::While => {
                        self.current_node += 1;
                        self.match_token(&TokenType::LeftParen)?;
                        self.current_node += 1;
                        let condition = self.parse_expr()?;
                        self.current_node += 1;
                        self.match_token(&TokenType::LeftBrace)?;
                        self.current_node += 1;
                        let then_block = self.parse_block()?;
                        self.current_node += 1;
                        Ok(Statement::While(
                            Box::new(condition),
                            Box::new(Statement::Block(then_block)),
                        ))
                    }
                    _ => {
                        // Assume expression
                        let expr = self.parse_expr()?;
                        self.match_token(&TokenType::Semicolon)?;
                        self.current_node += 1;
                        Ok(Statement::Expr(Box::new(expr)))
                    }
                }
            }
            _ => Err("Impossible".to_owned()),
        }
    }
    fn parse_block(&mut self) -> Result<Vec<Decl>, String> {
        let mut stmts: Vec<Decl> = Vec::new();
        while let Some(x) = self.tokens.get(self.current_node) {
            match x.token_type {
                TokenType::RightBrace => {
                    return Ok(stmts);
                }
                _ => {
                    let decl = self.parse_decl()?;
                    stmts.push(decl);
                }
            }
        }
        Ok(stmts)
    }
    fn parse_expr(&mut self) -> Result<Expression, String> {
        self.parse_assignment()
    }
    fn parse_assignment(&mut self) -> Result<Expression, String> {
        let expr = self.parse_equality()?;
        match self.tokens.get(self.current_node) {
            Some(token) => match token.token_type {
                TokenType::Equal => {
                    let name = match expr {
                        Expression::Variable(name) => name,
                        _ => {
                            return Err(
                                "Expected variable at the left hand side of assignment".to_owned()
                            )
                        }
                    };
                    self.current_node += 1;
                    let value = self.parse_assignment()?;
                    self.match_token(&TokenType::Semicolon)?;
                    Ok(Expression::Assignment(name, Box::new(value)))
                }
                _ => Ok(expr),
            },
            None => Err("Unexpected end of tokens".to_owned()),
        }
    }
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;

        while let Some(x) = self.tokens.get(self.current_node) {
            match x.token_type {
                TokenType::EqualEqual | TokenType::BangEqual => {
                    self.current_node += 1;
                    let right = self.parse_comparison()?;
                    expr = Expression::Binary(
                        BinaryOperator::from(&x.token_type)?,
                        Box::new(expr),
                        Box::new(right),
                    );
                }
                _ => return Ok(expr),
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;

        while let Some(x) = self.tokens.get(self.current_node) {
            match x.token_type {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    self.current_node += 1;
                    let right = self.parse_term()?;
                    expr = Expression::Binary(
                        BinaryOperator::from(&x.token_type)?,
                        Box::new(expr),
                        Box::new(right),
                    );
                }
                _ => return Ok(expr),
            }
        }
        Ok(expr)
    }
    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;

        while let Some(x) = self.tokens.get(self.current_node) {
            match x.token_type {
                TokenType::Minus | TokenType::Plus => {
                    self.current_node += 1;
                    let right = self.parse_factor()?;
                    expr = Expression::Binary(
                        BinaryOperator::from(&x.token_type)?,
                        Box::new(expr),
                        Box::new(right),
                    );
                }
                _ => return Ok(expr),
            }
        }
        Ok(expr)
    }
    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;

        while let Some(x) = self.tokens.get(self.current_node) {
            match x.token_type {
                TokenType::Star | TokenType::Slash => {
                    self.current_node += 1;
                    let right = self.parse_unary()?;
                    expr = Expression::Binary(
                        BinaryOperator::from(&x.token_type)?,
                        Box::new(expr),
                        Box::new(right),
                    );
                }
                _ => return Ok(expr),
            }
        }
        Ok(expr)
    }
    fn parse_unary(&mut self) -> Result<Expression, String> {
        match self.tokens.get(self.current_node) {
            Some(x) => match x.token_type {
                TokenType::Bang | TokenType::Minus => {
                    let right = self.parse_unary()?;
                    Ok(Expression::Unary(
                        UnaryOperator::from(&x.token_type)?,
                        Box::new(right),
                    ))
                }
                _ => self.parse_call(),
            },
            None => Err("No more tokens at unary".to_owned()),
        }
    }
    fn parse_call(&mut self) -> Result<Expression, String> {
        match self.tokens.get(self.current_node) {
            Some(_) => {
                let primary = match self.parse_primary() {
                    Ok(p) => p,
                    Err(e) => return Err(e),
                };
                match self.match_token(&TokenType::LeftParen) {
                    Ok(_) => {
                        let mut args = Vec::new();
                        loop {
                            self.current_node += 1;
                            let current_token = self.tokens.get(self.current_node);
                            let expr = match current_token {
                                Some(token) => match token.token_type {
                                    TokenType::RightParen => {
                                        return Ok(Expression::Call(Box::new(primary), args))
                                    },
                                    TokenType::Comma => {
                                        continue;
                                    }
                                    _ => self.parse_expr(),
                                },
                                None => {
                                    return Err("No more tokens at call argument parsing".to_owned())
                                }
                            };
                            match expr {
                                Ok(e) => args.push(e),
                                Err(e) => return Err(e),
                            }
                        }
                    }
                    Err(_) => return Ok(primary),
                }
            }
            None => Err("No more token found for parsing".to_owned())
        }
    }
    fn parse_primary(&mut self) -> Result<Expression, String> {
        let literal = match self.tokens.get(self.current_node) {
            Some(x) => match &x.token_type {
                TokenType::String(f) => Expression::Literal(LiteralType::String(f.to_owned())),
                TokenType::Number(n) => Expression::Literal(LiteralType::Number(*n)),
                TokenType::True => Expression::Literal(LiteralType::Boolean(true)),
                TokenType::False => Expression::Literal(LiteralType::Boolean(false)),
                TokenType::Identifier(s) => Expression::Variable(s.to_owned()),
                TokenType::LeftParen => {
                    self.current_node += 1;
                    let expr = self.parse_expr()?;
                    match self.tokens.get(self.current_node) {
                        Some(c) => match c.token_type {
                            TokenType::RightParen => Expression::Grouping(Box::new(expr)),
                            _ => {
                                return Err(String::from(
                                    "Unexpected token in palce of parentheses",
                                ))
                            }
                        },
                        None => return Err(String::from("Unmatched parentheses")),
                    }
                }
                _ => {
                    return Err(String::from(format!(
                        "Unexpected token at parsing literal {:?} ",
                        x
                    )));
                }
            },
            None => return Err(String::from("Error parsing literal")),
        };
        self.current_node += 1;
        Ok(literal)
    }
}

#[derive(Debug)]
pub enum Decl {
    Var(String, Box<Option<Expression>>),
    Statement(Box<Statement>),
    Func(String, Vec<String>, Vec<Decl>),
}
#[derive(Debug)]
pub enum Statement {
    Expr(Box<Expression>),
    If(Box<Expression>, Box<Statement>, Box<Option<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Block(Vec<Decl>),
    Print(Box<Expression>),
}
#[derive(Debug)]
pub enum Expression {
    Literal(LiteralType),
    Variable(String),
    Assignment(String, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Grouping(Box<Expression>),
}
#[derive(Debug)]
pub enum LiteralType {
    Number(f64),
    String(String),
    Boolean(bool),
}
#[derive(Debug)]
pub enum UnaryOperator {
    Bang,
    Negation,
}
#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl UnaryOperator {
    fn from(token: &TokenType) -> Result<UnaryOperator, String> {
        let ret = match token {
            TokenType::Minus => UnaryOperator::Negation,
            TokenType::Bang => UnaryOperator::Bang,
            _ => return Err(String::from("Not an unary operator")),
        };
        Ok(ret)
    }
}

impl BinaryOperator {
    fn from(token: &TokenType) -> Result<BinaryOperator, String> {
        let ret = match token {
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Star => BinaryOperator::Star,
            TokenType::Slash => BinaryOperator::Slash,
            TokenType::BangEqual => BinaryOperator::BangEqual,
            TokenType::EqualEqual => BinaryOperator::EqualEqual,
            TokenType::Greater => BinaryOperator::Greater,
            TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenType::Less => BinaryOperator::Less,
            TokenType::LessEqual => BinaryOperator::LessEqual,
            _ => return Err(String::from("Not a binary operator")),
        };
        Ok(ret)
    }
}
