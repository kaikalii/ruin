#![allow(dead_code)]

use std::{
    fmt::{self, Debug, Formatter},
    sync::{Arc, Mutex},
};

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use rpds::{RedBlackTreeMapSync, VectorSync};

use crate::{compile::*, num::Num, parse::*};

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
    #[display(fmt = "seq")]
    Seq,
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
            Value::Seq => Type::Seq,
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
    pub fn seq(&self) -> EvalResult<()> {
        match self.ty() {
            Type::Seq => Ok(()),
            found => Err(EvalError::TypeMismatch {
                expected: Type::Seq,
                found,
            }),
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

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Value::Function(Arc::new(f))
    }
}

impl From<EvalError> for Value {
    fn from(e: EvalError) -> Self {
        match e {
            EvalError::Value(e) => Value::Error(e),
            e => Value::Error(e.to_string()),
        }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Key {
    Num(Num),
    String(String),
}

pub type FunctionEnv = RedBlackTreeMapSync<String, Value>;

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{}", "self.format()")]
pub struct Function {
    pub args: Vec<String>,
    pub body: FunctionBody,
    pub env: RedBlackTreeMapSync<String, Value>,
    pub bar: bool,
}

impl Function {
    pub fn contains_ident(&self, ident: &str) -> bool {
        if let FunctionBody::Expr { expr, .. } = &self.body {
            expr.contains_ident(ident) && !self.args.iter().any(|i| i == ident)
        } else {
            false
        }
    }
    pub fn new_builtin<I, F>(args: I, f: F) -> Self
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
        F: Fn(&mut Stack) -> EvalResult + Send + Sync + 'static,
    {
        Function {
            args: args.into_iter().map(|s| s.as_ref().into()).collect(),
            body: FunctionBody::Builtin(Arc::new(f)),
            env: RedBlackTreeMapSync::default(),
            bar: false,
        }
    }
    fn format(&self) -> String {
        format!(
            "{}{}{} {}",
            if self.bar {
                "|".normal().to_string()
            } else {
                format!("{}{}", "fn".magenta(), '(')
            },
            self.args
                .iter()
                .map(|s| s.as_str())
                .intersperse(", ")
                .collect::<String>(),
            if self.bar { "|" } else { ")" },
            self.body
        )
    }
}

pub type BuiltinFunction = Arc<dyn Fn(&mut Stack) -> EvalResult + Send + Sync + 'static>;

#[derive(Display, Clone)]
pub enum FunctionBody {
    #[display(fmt = "{}", expr)]
    Expr {
        expr: Expression,
        instrs: Arc<Mutex<Option<Instrs>>>,
    },
    #[display(fmt = "built-in")]
    Builtin(BuiltinFunction),
}

impl Debug for FunctionBody {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            FunctionBody::Expr { expr, .. } => expr.fmt(f),
            FunctionBody::Builtin(_) => write!(f, "built-in"),
        }
    }
}

impl From<Expression> for FunctionBody {
    fn from(expr: Expression) -> Self {
        FunctionBody::Expr {
            expr,
            instrs: Arc::new(Mutex::new(None)),
        }
    }
}

impl PartialEq for FunctionBody {
    fn eq(&self, other: &Self) -> bool {
        if let (FunctionBody::Expr { expr: a, .. }, FunctionBody::Expr { expr: b, .. }) =
            (self, other)
        {
            a == b
        } else {
            false
        }
    }
}

impl Eq for FunctionBody {}

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
    #[display(fmt = "seq")]
    Seq,
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
