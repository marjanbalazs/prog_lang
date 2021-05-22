use std::ops::{Deref, IndexMut};

use super::parser::{BinaryOperator, Decl, Expression, LiteralType, Statement, UnaryOperator};

#[derive(Debug)]
pub struct EnvBlock {
    parent_block: Option<usize>,
    vars: Vec<(String, Option<Value>)>,
}
pub struct Environment {
    blocks: Vec<EnvBlock>,
    current_scope: usize,
}

impl Environment {
    pub fn new() -> Environment {
        let mut global = Environment {
            blocks: Vec::new(),
            current_scope: 0,
        };
        global.blocks.push(EnvBlock {
            parent_block: None,
            vars: Vec::new(),
        });
        global
    }

    fn get(&self, ident: &str) -> Option<&Value> {
        self.get_arg(self.current_scope, ident)
    }

    fn get_arg(&self, scope: usize, ident: &str) -> Option<&Value> {
        let k = self.blocks.get(scope);
        match k {
            Some(block) => {
                let found = block.vars.iter().find(|val| val.0 == ident);
                match found {
                    Some(val) => match &val.1 {
                        Some(f) => Some(f),
                        None => None,
                    },
                    None => match block.parent_block {
                        Some(parent) => self.get_arg(parent, ident),
                        None => None,
                    },
                }
            }
            None => None,
        }
    }

    fn get_arg_pos(&self, scope: usize, ident: &str) -> Option<(usize, usize)> {
        let k = self.blocks.get(scope);
        match k {
            Some(block) => {
                let res = block
                    .vars
                    .iter()
                    .enumerate()
                    .find(|(_, val)| val.0 == ident);
                match res {
                    Some((index, _)) => Some((scope, index)),
                    None => match block.parent_block {
                        Some(parent) => self.get_arg_pos(parent, ident),
                        None => None,
                    },
                }
            }
            None => None,
        }
    }

    fn set(&mut self, ident: &str, value: Value) -> Result<(), ()> {
        self.set_arg(self.current_scope, ident, value)
    }

    fn set_arg(&mut self, scope: usize, ident: &str, value: Value) -> Result<(), ()> {
        let found = self.get_arg_pos(scope, ident);
        match found {
            Some((scope, index)) => {
                let block = self.blocks.get_mut(scope).unwrap();
                block.vars.index_mut(index).1 = Some(value);
                Ok(())
            }
            None => Err(()),
        }
    }

    fn decl_arg(&mut self, ident: &str, value: Option<Value>) -> Result<(), ()> {
        let block = self.blocks.get_mut(self.current_scope).unwrap();
        block.vars.push((ident.to_owned(), value));
        Ok(())
    }

    fn new_block(&mut self) {
        self.blocks.push({
            EnvBlock {
                parent_block: Some(self.current_scope),
                vars: Vec::new()
            }
        });
        println!("Opened block");
        self.current_scope = self.blocks.len() - 1;
    }

    fn drop_block(&mut self) {
        self.current_scope = self.blocks.get(self.current_scope).unwrap().parent_block.unwrap();
        println!("Dropped block");
        self.blocks.pop();
    }
}

trait Visitor {
    fn visit_decl(&mut self, expr: &Decl) -> Result<(), String>;
    fn visit_stmt(&mut self, expr: &Statement) -> Result<(), String>;
    fn visit_expr(&mut self, expr: &Expression) -> Result<Value, String>;
}

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
}

pub struct Interpreter<'a> {
    ast: &'a Vec<Decl>,
    env: Environment,
}

impl<'a, 'b> Interpreter<'a> {
    pub fn new(ast: &'a Vec<Decl>) -> Interpreter<'a> {
        Interpreter {
            ast,
            env: Environment::new(),
        }
    }
    pub fn interpret(&mut self) {
        for decl in self.ast.iter() {
            let res = self.visit_decl(decl);
            match res {
                Ok(_) => {}
                Err(e) => {
                    println!("Error: {}", e)
                }
            }
        }
    }
}

impl<'a, 'b> Visitor for Interpreter<'a> {
    fn visit_decl(&mut self, decl: &Decl) -> Result<(), String> {
        match decl {
            Decl::Var(ident, expr) => match expr.deref() {
                Some(e) => {
                    let val = self.visit_expr(e)?;
                    self.env.decl_arg(ident, Some(val));
                    Ok(())
                }
                None => {
                    self.env.decl_arg(ident, None);
                    Ok(())
                }
            },
            Decl::Statement(stmt) => self.visit_stmt(stmt),
        }
    }
    fn visit_stmt(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::Print(expr) => {
                let val = self.visit_expr(expr)?;
                println!("{:?}", val);
            }
            Statement::Expr(expr) => {
                let val = self.visit_expr(expr)?;
                println!("{:?}", val);
            }
            Statement::Block(block) => {
                self.env.new_block();
                for decl in block.iter() {
                    self.visit_decl(decl);
                }
                self.env.drop_block();
            }
        }
        Ok(())
    }
    fn visit_expr(&mut self, expr: &Expression) -> Result<Value, String> {
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
                            _ => Err(String::from("Boolean expected")),
                        },
                        Err(e) => Err(e),
                    }
                }
                UnaryOperator::Negation => {
                    let x = self.visit_expr(rhs);
                    match x {
                        Ok(o) => match o {
                            Value::Number(f) => Ok(Value::Number(-f)),
                            _ => Err(String::from("Boolean expected")),
                        },
                        Err(e) => Err(e),
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
                            BinaryOperator::Plus => Ok(Value::String(left + &right)),
                            BinaryOperator::EqualEqual => Ok(Value::Boolean(left == right)),
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
                    _ => Err("Error in interpretation".to_owned()),
                }
            }
            Expression::Grouping(x) => self.visit_expr(x),
            Expression::Variable(ident) => Ok(self.env.get(ident).unwrap().clone()),
            Expression::Assignment(ident, e) => {
                let val = self.visit_expr(e)?;
                match self.env.set(ident, val) {
                    Ok(_) => Ok(Value::Boolean(true)),
                    Err(_) => Err("Assignment failed".to_owned()),
                }
            }
        }
    }
}
