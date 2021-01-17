#![allow(dead_code)]

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use rpds::{RedBlackTreeMap, Vector};

use crate::{num::Num, parse::Expression};

pub type List = Vector<Value>;
pub type Table = RedBlackTreeMap<Value, Value>;

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    #[display(fmt = "{}", "\"nil\".blue()")]
    Nil,
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Num(Num),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Bool(bool),
    #[display(fmt = "{}", "_0.to_string().green()")]
    String(String),
    List(List),
    Table(Table),
    Function(Box<Function>),
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
        }
    }
    pub fn is_truth(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(
    fmt = "{}({}) {}",
    "\"fn\".magenta()",
    "args.iter().map(|s| s.as_str()).intersperse(\", \").collect::<String>()",
    body
)]
pub struct Function {
    pub args: Vec<String>,
    pub body: Expression,
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
    #[display(fmt = "error")]
    Error,
    #[display(fmt = "nil")]
    Nil,
}
