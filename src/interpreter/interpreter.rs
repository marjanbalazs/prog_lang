use super::parser::{BinaryOperator, Expression, LiteralType, UnaryOperator};
trait Visitor {
    fn visit_expr(&self, expr: &Expression) -> Result<Value, String>;
}

#[derive(Debug)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
}

pub struct Interpreter<'a> {
    ast: &'a Box<Expression>,
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: &Box<Expression>) -> Interpreter {
        Interpreter { ast }
    }
    pub fn interpret(&self) {
        let f = self.visit_expr(self.ast);
        match f {
            Ok(val) => {
                println!("{:?}", val)
            }
            Err(e) => {
                println!("Error: {}", e)
            }
        }
    }
}

impl<'a> Visitor for Interpreter<'a> {
    fn visit_expr(&self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Literal(x) => match x {
                LiteralType::Number(f) => Ok(Value::Number(*f)),
                LiteralType::String(s) => Ok(Value::String(s.to_owned())),
                LiteralType::Boolean(b) => Ok(Value::Boolean(*b)),
            },
            Expression::Unary(op, rhs) => match op {
                UnaryOperator::Bang => {
                    let x = self.visit_expr(rhs);
                    match x {
                        Ok(o) => match o {
                            Value::Boolean(b) => Ok(Value::Boolean(!b)),
                            _ => return Err(String::from("Boolean expected")),
                        },
                        Err(e) => return Err(e),
                    }
                }
                UnaryOperator::Negation => {
                    let x = self.visit_expr(rhs);
                    match x {
                        Ok(o) => match o {
                            Value::Number(f) => Ok(Value::Number(-f)),
                            _ => return Err(String::from("Boolean expected")),
                        },
                        Err(e) => return Err(e),
                    }
                }
            },
            Expression::Binary(op, lhs, rhs) => {
                let x = self.visit_expr(lhs);
                let y = self.visit_expr(rhs);
                match (x, y) {
                    (Ok(left), Ok(right)) => match (left, right) {
                        (Value::Number(left), Value::Number(right)) => match op {
                            BinaryOperator::Plus => Ok(Value::Number(left + right)),
                            BinaryOperator::Minus => Ok(Value::Number(left - right)),
                            BinaryOperator::Star => Ok(Value::Number(left * right)),
                            BinaryOperator::Slash => Ok(Value::Number(left / right)),
                            BinaryOperator::Equal => Ok(Value::Number(right)), // I'm not at all sure about this.
                            BinaryOperator::BangEqual => Ok(Value::Boolean(left != right)),
                            BinaryOperator::EqualEqual => Ok(Value::Boolean(left == right)),
                            BinaryOperator::Greater => Ok(Value::Boolean(left > right)),
                            BinaryOperator::GreaterEqual => Ok(Value::Boolean(left >= right)),
                            BinaryOperator::Less => Ok(Value::Boolean(left < right)),
                            BinaryOperator::LessEqual => Ok(Value::Boolean(left <= right)),
                        },
                        (Value::Number(_), Value::String(_)) => Err(
                            "Incompatible types on two sides of the expression Number/String"
                                .to_owned(),
                        ),
                        (Value::Number(_), Value::Boolean(_)) => Err(
                            "Incompatible types on two sides of the expression Number/String"
                                .to_owned(),
                        ),
                        (Value::String(_), Value::Number(_)) => Err(
                            "Incompatible types on two sides of the expression Number/String"
                                .to_owned(),
                        ),
                        (Value::String(left), Value::String(right)) => match op {
                            BinaryOperator::Plus => Ok(Value::String(String::from(left + &right))),
                            _ => Err("Unknown operator on String types".to_owned()),
                        },
                        (Value::String(_), Value::Boolean(_)) => Err(
                            "Incompatible types on two sides of the expression Number/String"
                                .to_owned(),
                        ),
                        (Value::Boolean(_), Value::Number(_)) => Err(
                            "Incompatible types on two sides of the expression Number/String"
                                .to_owned(),
                        ),
                        (Value::Boolean(_), Value::String(_)) => Err(
                            "Incompatible types on two sides of the expression Number/String"
                                .to_owned(),
                        ),
                        (Value::Boolean(left), Value::Boolean(right)) => match op {
                            BinaryOperator::BangEqual => Ok(Value::Boolean(left == right)),
                            BinaryOperator::EqualEqual => Ok(Value::Boolean(left != right)),
                            _ => Err("Unknown operator on Boolean Types".to_owned()),
                        },
                    },
                    _ => return Err("Error in interpretation".to_owned()),
                }
            }
            Expression::Grouping(x) => self.visit_expr(x),
        }
    }
}
