use super::scanner::{Token, TokenType};
pub struct Parser<'a> {
    pub tree: Option<Box<Expression>>,
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
    pub fn build_ast(&mut self) {
        self.tree = match self.parse_expr() {
            Ok(x) => Some(Box::new(x)),
            Err(e) => {
                self.print_error(&e);
                None
            }
        }
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
        let expr = self.parse_unary()?;

        match self.tokens.get(self.current_node) {
            Some(x) => match x.token_type {
                TokenType::Star | TokenType::Slash => {
                    self.current_node += 1;
                    let right = self.parse_unary()?;
                    Ok(Expression::Binary(
                        BinaryOperator::from(&x.token_type)?,
                        Box::new(expr),
                        Box::new(right),
                    ))
                }
                _ => Ok(expr),
            },
            None => return Err(String::from("No more tokens at factor")),
        }
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
                TokenType::True => Expression::Literal(LiteralType::True),
                TokenType::False => Expression::Literal(LiteralType::False),
                TokenType::Nil => Expression::Literal(LiteralType::Nil),
                TokenType::LeftParen => {
                    self.current_node += 1;
                    let e = self.parse_expr()?;
                    match self.tokens.get(self.current_node) {
                        Some(c) => match c.token_type {
                            TokenType::RightParen => {}
                            _ => {
                                return Err(String::from(
                                    "Unexpected token in palce of parentheses",
                                ))
                            }
                        },
                        None => return Err(String::from("Unmatched parentheses")),
                    }
                    e
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
enum Expression {
    Literal(LiteralType),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Grouping(GroupingOperator, Box<Expression>),
}
#[derive(Debug)]
enum LiteralType {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}
#[derive(Debug)]
enum UnaryOperator {
    Bang,
    Negation,
}
#[derive(Debug)]
enum BinaryOperator {
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

#[derive(Debug)]
enum GroupingOperator {
    LeftBrace,
    RightBrace,
}
