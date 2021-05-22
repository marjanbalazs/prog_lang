use std::format;

use super::scanner::{Token, TokenType};
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
    fn print_error(&self, e: &str) {
        let curr_line = self.tokens[self.current_node].line;
        let curr_pos = self.tokens[self.current_node].char_pos;
        let token = &self.tokens[self.current_node].token_type;
        println!(
            "Error at line: {} at pos {}. Message: {} Token: {:?}",
            curr_line, curr_pos, e, token
        )
    }
    pub fn get_ast(&self) -> &Vec<Decl> {
        &self.tree
    }
    fn match_next_token(&mut self, token_to_match: &TokenType) -> Result<(), String> {
        match self.tokens.get(self.current_node) {
            Some(token) => {
                let token_type = &token.token_type;
                if token_type == token_to_match {
                    Ok(())
                } else {
                    let err = format!(
                        "Expected token: {:?}, found: {:?}",
                        token_to_match, token_type
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
        println!("{:?}", self.tree);
        Ok(())
    }
    fn parse_decl(&mut self) -> Result<Decl, String> {
        match self.tokens.get(self.current_node) {
            Some(tok) => match tok.token_type {
                TokenType::Var => {
                    self.current_node += 1;
                    let (ident, expr) = self.parse_var()?;
                    Ok(Decl::Var(ident, Box::new(expr)))
                }
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
        let ident = match self.tokens.get(self.current_node) {
            Some(token) => match &token.token_type {
                TokenType::Identifier(s) => s.to_owned(),
                _ => return Err("Expected identifier after keyword Var".to_owned()),
            },
            None => return Err("Unexpected end of tokens".to_owned()),
        };
        // Initializer
        self.current_node += 1;
        let expr = match self.tokens.get(self.current_node) {
            Some(token) => match token.token_type {
                TokenType::Equal => {
                    self.current_node += 1;
                    self.parse_expr()?
                }
                TokenType::Semicolon => return Ok((ident, None)),
                _ => return Err("Unexpected token after declaration".to_owned()),
            },
            None => return Err("Unexpected end of tokens".to_owned()),
        };
        // Semicolon
        match self.match_next_token(&TokenType::Semicolon) {
            Ok(_) => {
                self.current_node += 1;
                Ok((ident, Some(expr)))

            },
            Err(err) => Err(err),
        }
    }
    fn parse_stmt(&mut self) -> Result<Statement, String> {
        match self.tokens.get(self.current_node) {
            Some(tok) => {
                match tok.token_type {
                    TokenType::Print => {
                        self.current_node += 1;
                        let expr = self.parse_expr()?;
                        self.match_next_token(&TokenType::Semicolon)?;
                        self.current_node += 1;
                        Ok(Statement::Print(Box::new(expr)))
                    },
                    TokenType::LeftBrace => {
                        self.current_node += 1;
                        let block = self.parse_block()?;
                        Ok(Statement::Block(block))
                    },
                    _ => {
                        // Assume expression
                        let expr = self.parse_expr()?;
                        self.match_next_token(&TokenType::Semicolon)?;
                        Ok(Statement::Expr(Box::new(expr)))
                    }
                }
            }
            _ => Err("Impossible".to_owned()),
        }
    }
    fn parse_block(&mut self) -> Result<Vec<Decl>, String> {
        let mut stmts: Vec<Decl> = Vec::new();
        println!("Parsing block");
        while let Some(x) = self.tokens.get(self.current_node) {
            match x.token_type {
                TokenType::RightBrace => {
                    self.current_node += 1;
                    println!("Right brace");
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
                    self.current_node += 1;
                    let value = self.parse_assignment()?;
                    match expr {
                        Expression::Variable(name) => {
                            Ok(Expression::Assignment(name, Box::new(value)))
                        }
                        _ => {
                            Err("Expected variable at the left hand side of assignment".to_owned())
                        }
                    }
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
                    self.current_node += 1;
                    let right = self.parse_unary()?;
                    Ok(Expression::Unary(
                        UnaryOperator::from(&x.token_type)?,
                        Box::new(right),
                    ))
                }
                _ => self.parse_primary(),
            },
            None => Err("No more tokens at unary".to_owned()),
        }
    }
    fn parse_primary(&mut self) -> Result<Expression, String> {
        let literal = match self.tokens.get(self.current_node) {
            Some(x) => match &x.token_type {
                TokenType::String(f) => Expression::Literal(LiteralType::String(f.to_owned())),
                TokenType::Number(n) => Expression::Literal(LiteralType::Number(*n)),
                TokenType::True => Expression::Literal(LiteralType::Boolean(true)),
                TokenType::False => Expression::Literal(LiteralType::Boolean(true)),
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
                    println!("Unexpected token: {:?} {:?} {:?}", x, x.token_type, self.tree);
                    return Err(String::from("Unexpected token at parsing literal"));
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
}
#[derive(Debug)]
pub enum Statement {
    Print(Box<Expression>),
    Expr(Box<Expression>),
    Block(Vec<Decl>)
}

#[derive(Debug)]
pub enum Expression {
    Literal(LiteralType),
    Variable(String),
    Assignment(String, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
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
