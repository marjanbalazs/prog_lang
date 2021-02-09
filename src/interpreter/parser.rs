use super::scanner::{Token, TokenType};
pub struct Parser<'a> {
    tree: Option<Box<Expression>>,
    current_node: usize,
    tokens: &'a Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser {
            tree: None,
            current_node: 0,
            tokens,
        }
    }
    fn print_error(&self, e: &str) {
        let curr_line = self.tokens[self.current_node].line;
        let curr_pos = self.tokens[self.current_node].char_pos;
        println!(
            "Error at line: {} at pos {}. Message: {}",
            curr_line, curr_pos, e
        )
    }
    pub fn print(&self) {
        println!("{:?}", self.tree);
    }
    pub fn build_ast(&mut self) {
        self.tree = match self.parse_expr() {
            Ok(x) => Some(Box::new(x)),
            Err(e) => {
                self.print_error(&e);
                None
            }
        }
    }
    pub fn get_ast(&self) -> &Box<Expression> {
        &self.tree.as_ref().unwrap()
    }
    fn parse_expr(&mut self) -> Result<Expression, String> {
        self.parse_equality()
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
                    return Ok(Expression::Unary(
                        UnaryOperator::from(&x.token_type)?,
                        Box::new(right),
                    ));
                }
                _ => self.parse_primary(),
            },
            None => return Err(String::from("No more tokens at unary")),
        }
    }
    fn parse_primary(&mut self) -> Result<Expression, String> {
        let literal = match self.tokens.get(self.current_node) {
            Some(x) => match &x.token_type {
                TokenType::String(f) => Expression::Literal(LiteralType::String(f.to_owned())),
                TokenType::Number(n) => Expression::Literal(LiteralType::Number(*n)),
                TokenType::True => Expression::Literal(LiteralType::Boolean(true)),
                TokenType::False => Expression::Literal(LiteralType::Boolean(true)),
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
pub enum Expression {
    Literal(LiteralType),
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
    Equal,
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
            TokenType::Equal => BinaryOperator::Equal,
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
