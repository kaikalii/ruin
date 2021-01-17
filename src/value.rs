#![allow(dead_code)]

use colored::Colorize;
use derive_more::Display;
use rpds::{RedBlackTreeMap, Vector};

use crate::num::Num;

pub type List = Vector<Value>;
pub type Table = RedBlackTreeMap<Value, Value>;

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
    #[display(fmt = "error")]
    Error,
    #[display(fmt = "nil")]
    Nil,
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    #[display(fmt = "{}" "\"nil\".blue()")]
    Nil,
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Num(Num),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Bool(bool),
    #[display(fmt = "{}", "_0.to_string().green()")]
    String(String),
    List(List),
    Table(Table),
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
        }
    }
    pub fn is_truth(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }
}
