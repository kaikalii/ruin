use std::fmt;

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;

use crate::{num::Num, value::*};

#[derive(Debug)]
pub enum Command {
    Assignment(Assignment),
    Eval(Expression),
    FunctionDecl(FunctionDecl),
    Command,
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{} = {}", "ident.bright_white()", expr)]
pub struct Assignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(
    fmt = "{} {}({}) {}",
    r#""fn".magenta()"#,
    "ident.bright_white()",
    "function.args",
    "function.body"
)]
pub struct FunctionDecl {
    pub ident: String,
    pub function: Function,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Args {
    pub idents: Vec<String>,
}

impl fmt::Display for Args {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for s in self.idents.iter().map(AsRef::as_ref).intersperse(", ") {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(bound = "O: fmt::Display, T: fmt::Display")]
#[display(
    fmt = "{}{}",
    "left",
    r#"rights.iter().map(|r| { format!(" {} {}", r.op, r.expr) }).collect::<String>()"#
)]
pub struct BinExpr<O, T> {
    pub left: Box<T>,
    pub rights: Vec<Right<O, T>>,
}

impl<O, T> BinExpr<O, T> {
    pub fn new(left: T, rights: Vec<Right<O, T>>) -> Self {
        BinExpr {
            left: Box::new(left),
            rights,
        }
    }
}

pub trait Node {
    type Child;
    fn contains_ident(&self, ident: &str) -> bool;
    fn terms(&self) -> usize;
    fn wrapping(child: Self::Child) -> Self;
}

impl<O, T> Node for BinExpr<O, T>
where
    T: Node,
{
    type Child = T;
    fn contains_ident(&self, ident: &str) -> bool {
        self.left.contains_ident(ident)
            || self
                .rights
                .iter()
                .any(|right| right.expr.contains_ident(ident))
    }
    fn terms(&self) -> usize {
        self.left.terms()
            + self
                .rights
                .iter()
                .map(|right| right.expr.terms())
                .sum::<usize>()
    }
    fn wrapping(child: Self::Child) -> Self {
        BinExpr::new(child, Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Right<O, T> {
    pub op: O,
    pub expr: T,
}

impl<O, T> Right<O, T> {
    pub fn new(op: O, expr: T) -> Self {
        Right { op, expr }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(
    fmt = "{}{}",
    r#"(0..*count).map(|_| "not ").collect::<String>()"#,
    expr
)]
pub struct UnExpr<O, T> {
    pub op: O,
    pub count: usize,
    pub expr: T,
}

impl<O, T> Node for UnExpr<O, T>
where
    T: Node,
    O: Default,
{
    type Child = T;
    fn contains_ident(&self, ident: &str) -> bool {
        self.expr.contains_ident(ident)
    }
    fn terms(&self) -> usize {
        self.expr.terms()
    }
    fn wrapping(child: Self::Child) -> Self {
        UnExpr {
            op: O::default(),
            count: 0,
            expr: child,
        }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, Default)]
#[display(fmt = "or")]
pub struct OpOr;
#[derive(Debug, Display, Clone, PartialEq, Eq, Default)]
#[display(fmt = "and")]
pub struct OpAnd;
#[derive(Debug, Display, Clone, PartialEq, Eq, Default)]
#[display(fmt = "not")]
pub struct OpNot;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OpCmp {
    #[display(fmt = "is")]
    Is,
    #[display(fmt = "isnt")]
    Isnt,
    #[display(fmt = "<")]
    Less,
    #[display(fmt = ">")]
    Greater,
    #[display(fmt = "<=")]
    LessOrEqual,
    #[display(fmt = ">=")]
    GreaterOrEqual,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OpAS {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OpMDR {
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
}

pub type Expression = ExprOr;
pub type ExprOr = BinExpr<OpOr, ExprAnd>;
pub type ExprAnd = BinExpr<OpAnd, ExprCmp>;
pub type ExprCmp = BinExpr<OpCmp, ExprAS>;
pub type ExprAS = BinExpr<OpAS, ExprMDR>;
pub type ExprMDR = BinExpr<OpMDR, ExprNot>;
pub type ExprNot = UnExpr<OpNot, ExprCall>;

fn _expression_size() {
    #[allow(invalid_value)]
    let _: [u8; 32] = unsafe { std::mem::transmute::<Expression, _>(std::mem::zeroed()) };
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum ExprCall {
    #[display(
        fmt = "{}{}",
        "term",
        r#"args.as_ref().map(|args| args
            .iter()
            .map(ToString::to_string)
            .intersperse(", ".into())
            .collect::<String>()
        ).map(|s| format!("({})", s)).unwrap_or_default()"#
    )]
    Regular {
        term: Term,
        args: Option<Vec<Expression>>,
    },
    #[display(
        fmt = "{}{}",
        first,
        r#"calls.iter().map(|call| format!(
            ":{}({})", 
            call.term, 
            call.args.iter().map(ToString::to_string).intersperse(", ".into()).collect::<String>()
        )).collect::<String>()"#
    )]
    Method { first: Term, calls: Vec<Call> },
}

impl Node for ExprCall {
    type Child = Term;
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            ExprCall::Regular { term, args } => {
                term.contains_ident(ident)
                    || args.as_ref().map_or(false, |args| {
                        args.iter().any(|expr| expr.contains_ident(ident))
                    })
            }
            ExprCall::Method { first, calls } => {
                first.contains_ident(ident)
                    || calls.iter().any(|call| {
                        call.term.contains_ident(ident)
                            || call.args.iter().any(|expr| expr.contains_ident(ident))
                    })
            }
        }
    }
    fn terms(&self) -> usize {
        match self {
            ExprCall::Regular { term, args } => {
                term.terms()
                    + args.as_ref().map_or(0, |args| {
                        args.iter().map(|expr| expr.terms()).sum::<usize>()
                    })
            }
            ExprCall::Method { first, calls } => {
                first.terms()
                    + calls
                        .iter()
                        .map(|call| {
                            call.term.terms()
                                + call.args.iter().map(|expr| expr.terms()).sum::<usize>()
                        })
                        .sum::<usize>()
            }
        }
    }
    fn wrapping(child: Self::Child) -> Self {
        ExprCall::Regular {
            term: child,
            args: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    pub term: Term,
    pub args: Vec<Expression>,
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
pub enum Term {
    #[display(fmt = "({})", _0)]
    Expr(Box<Expression>),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Num(Num),
    #[display(fmt = "{}", "_0.bright_white()")]
    Ident(String),
    #[display(fmt = "{}", "_0.to_string().blue()")]
    Bool(bool),
    #[display(fmt = "{}", "format!(\"{:?}\", _0).green()")]
    String(String),
    Function(Box<Function>),
    #[display(fmt = "{}", "\"nil\".blue()")]
    Nil,
}

impl Node for Term {
    type Child = Expression;
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Term::Expr(expr) => expr.contains_ident(ident),
            Term::Ident(p) => p == ident,
            Term::Function(f) => f.contains_ident(ident),
            _ => false,
        }
    }
    fn terms(&self) -> usize {
        if let Term::Expr(expr) = self {
            expr.terms()
        } else {
            1
        }
    }
    fn wrapping(child: Self::Child) -> Self {
        Term::Expr(Box::new(child))
    }
}
