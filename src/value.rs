#![allow(dead_code)]

use std::sync::Arc;

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use rpds::{RedBlackTreeMapSync, VectorSync};

use crate::{eval::EvalError, num::Num, parse::*};

pub type List = VectorSync<Value>;
pub type Table = RedBlackTreeMapSync<Key, Value>;

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Value {
    #[display(fmt = "{}", "\"nil\".blue()")]
    Nil,
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Num(Num),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Bool(bool),
    #[display(fmt = "{}", "_0.green()")]
    String(String),
    #[display(fmt = "{}", "_0.red()")]
    Error(String),
    List(List),
    Table(Table),
    Function(Arc<Function>),
    #[display(fmt = "{}", "format_expression_value(expr, val)")]
    Expression {
        expr: Arc<Expression>,
        val: Option<Box<Value>>,
    },
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Num(_) => Type::Number,
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            Value::List(_) => Type::List,
            Value::Table(_) => Type::Table,
            Value::Function(_) => Type::Function,
            Value::Expression { val, .. } => {
                val.as_deref().map(Value::ty).unwrap_or(Type::Expression)
            }
            Value::Error(_) => Type::Error,
        }
    }
    pub fn is_truth(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false) | Value::Error(_))
    }
    pub fn is_err(&self) -> bool {
        self.ty() == Type::Error
    }
    pub fn is_evald(&self) -> bool {
        !matches!(self, Value::Expression { val: None, .. })
    }
    pub fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Value::List(list) => list.iter().any(|val| val.contains_ident(ident)),
            Value::Table(table) => table.values().any(|val| val.contains_ident(ident)),
            Value::Function(function) => function.contains_ident(ident),
            Value::Expression { expr, val } => {
                expr.contains_ident(ident)
                    || val.as_ref().map_or(false, |val| val.contains_ident(ident))
            }
            _ => false,
        }
    }
    pub fn reset(&mut self) {
        if let Value::Expression { val, .. } = self {
            *val = None;
        }
    }
    pub fn as_evald(&self) -> &Value {
        if let Value::Expression { val: Some(val), .. } = self {
            &**val
        } else {
            self
        }
    }
    pub fn as_evald_mut(&mut self) -> &mut Value {
        if let Value::Expression { val: Some(val), .. } = self {
            &mut **val
        } else {
            self
        }
    }
}

impl From<Expression> for Value {
    fn from(expr: Expression) -> Self {
        Value::Expression {
            expr: expr.into(),
            val: None,
        }
    }
}

impl From<Result<Value, EvalError>> for Value {
    fn from(res: Result<Value, EvalError>) -> Self {
        res.map_err(|e| e.to_string()).unwrap_or_else(Value::Error)
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Key {
    Num(Num),
    String(String),
}

#[derive(Debug, Display, Clone)]
#[display(
    fmt = "{}({}) {}",
    "\"fn\".magenta()",
    "args.iter().map(|s| s.as_str()).intersperse(\", \").collect::<String>()",
    body
)]
pub struct Function {
    pub args: Vec<String>,
    pub body: Expression,
    pub env: RedBlackTreeMapSync<String, Value>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.args == other.args && self.body == other.body && self.env == other.env
    }
}

impl Eq for Function {}

impl Function {
    pub fn contains_ident(&self, ident: &str) -> bool {
        self.body.contains_ident(ident) && !self.args.iter().any(|i| i == ident)
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    #[display(fmt = "?")]
    Unknown,
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "number")]
    Number,
    #[display(fmt = "string")]
    String,
    #[display(fmt = "list")]
    List,
    #[display(fmt = "table")]
    Table,
    #[display(fmt = "function")]
    Function,
    #[display(fmt = "expression")]
    Expression,
    #[display(fmt = "error")]
    Error,
    #[display(fmt = "nil")]
    Nil,
}

pub fn format_expression_value(expr: &Expression, val: &Option<Box<Value>>) -> String {
    let expr = expr.to_string();
    let val = val.as_ref().map(ToString::to_string);
    if let Some(val) = val {
        if expr == val {
            val
        } else {
            format!("{} = {}", expr, val)
        }
    } else {
        expr
    }
}
