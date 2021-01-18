use std::fmt::Display as StdDisplay;

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use tokenate::{LexError, Sp};

use crate::{lexer::*, num::Num, value::*};

#[derive(Debug, Display)]
pub enum ParseError {
    Lex(LexError),
    #[display(fmt = "Expected {}", _0)]
    Expected(String),
    #[display(fmt = "Invalid command")]
    InvalidCommand,
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError::Lex(e)
    }
}

pub type Parse<T> = Result<Sp<T>, ParseError>;
pub type MaybeParse<T> = Result<Option<Sp<T>>, ParseError>;

trait TokenPattern: StdDisplay {
    fn matches(&self, token: &Token) -> bool;
}

impl<'a, T> TokenPattern for &'a T
where
    T: TokenPattern,
{
    fn matches(&self, token: &Token) -> bool {
        (*self).matches(token)
    }
}

impl TokenPattern for Token {
    fn matches(&self, token: &Token) -> bool {
        self == token
    }
}

impl<'a> TokenPattern for &'a str {
    fn matches(&self, token: &Token) -> bool {
        match token {
            Token::Ident(ident) => ident == self,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Command {
    Assignment(Assignment),
    Load(String),
}

struct Tokens {
    iter: std::vec::IntoIter<Sp<Token>>,
    put_back: Vec<Sp<Token>>,
}

impl Tokens {
    pub fn take(&mut self) -> Option<Sp<Token>> {
        self.put_back.pop().or_else(|| self.iter.next())
    }
    pub fn take_as<F, T>(&mut self, f: F) -> Option<Sp<T>>
    where
        F: Fn(Token) -> Result<T, Token>,
    {
        if let Some(sp_token) = self.take() {
            match f(sp_token.data) {
                Ok(val) => Some(sp_token.span.sp(val)),
                Err(token) => {
                    self.put_back.push(sp_token.span.sp(token));
                    None
                }
            }
        } else {
            None
        }
    }
    pub fn put_back(&mut self, token: Sp<Token>) {
        self.put_back.push(token);
    }
    pub fn take_if<P>(&mut self, pattern: P) -> Option<Sp<Token>>
    where
        P: TokenPattern,
    {
        self.take_as(|token| {
            if pattern.matches(&token) {
                Ok(token)
            } else {
                Err(token)
            }
        })
    }
    pub fn matches_as<P, T>(&mut self, pattern: P, val: T) -> Option<Sp<T>>
    where
        P: TokenPattern,
    {
        self.take_if(pattern).map(|token| token.span.sp(val))
    }
    pub fn matches<P>(&mut self, pattern: P) -> bool
    where
        P: TokenPattern,
    {
        self.matches_as(pattern, ()).is_some()
    }
    pub fn require_token<P>(&mut self, pattern: P) -> Parse<Token>
    where
        P: TokenPattern,
    {
        self.take_if(&pattern)
            .ok_or_else(|| ParseError::Expected(pattern.to_string()))
    }
    pub fn _require<F, T>(&mut self, f: F, name: &str) -> Parse<T>
    where
        F: Fn(&mut Self) -> MaybeParse<T>,
    {
        f(self).and_then(|op| op.ok_or_else(|| ParseError::Expected(name.into())))
    }
    pub fn command(&mut self) -> Parse<Command> {
        if let Some(ass) = self.assigment()? {
            Ok(ass.map(Command::Assignment))
        } else if let Some(ident) = self.ident()? {
            todo!()
        } else {
            Err(ParseError::InvalidCommand)
        }
    }
    pub fn ident(&mut self) -> MaybeParse<String> {
        Ok(self.take_as(|token| {
            if let Token::Ident(s) = token {
                Ok(s)
            } else {
                Err(token)
            }
        }))
    }
    pub fn string_literal(&mut self) -> MaybeParse<String> {
        Ok(self.take_as(|token| {
            if let Token::String(s) = token {
                Ok(s)
            } else {
                Err(token)
            }
        }))
    }
    pub fn num(&mut self) -> MaybeParse<Num> {
        Ok(self.take_as(|token| {
            if let Token::Num(num) = token {
                Ok(num)
            } else {
                Err(token)
            }
        }))
    }
    pub fn boolean(&mut self) -> MaybeParse<bool> {
        Ok(self.take_as(|token| {
            if let Token::Bool(b) = token {
                Ok(b)
            } else {
                Err(token)
            }
        }))
    }
    pub fn cmp(&mut self) -> MaybeParse<OpCmp> {
        Ok(self.take_as(|token| {
            if let Token::Cmp(cmp) = token {
                Ok(cmp)
            } else {
                Err(token)
            }
        }))
    }
    pub fn _path(&mut self) -> MaybeParse<Path> {
        Ok(if let Some(name) = self.ident()? {
            let mut end = name.span.end;
            let mut disam = Vec::new();
            while let Some(ident) = self.ident()? {
                disam.push(ident.data);
                end = ident.span.end;
            }
            Some(name.span.start.to(end).sp(Path::new(disam, name.data)))
        } else {
            None
        })
    }
    pub fn assigment(&mut self) -> MaybeParse<Assignment> {
        let ident = if let Some(ident) = self.ident()? {
            ident
        } else {
            return Ok(None);
        };
        if !self.matches(Token::Equals) {
            self.put_back(ident.map(Token::Ident));
            return Ok(None);
        }
        let expr = self.expression()?;
        let ass = ident.join(expr, |ident, expr| Assignment { ident, expr });
        Ok(Some(ass))
    }
    pub fn expression(&mut self) -> Parse<Expression> {
        self.expr_or()
    }
    pub fn expr_or(&mut self) -> Parse<ExprOr> {
        let left = self.expr_and()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .matches_as(Token::Or, OpOr)
            .map(|op| self.expr_and().map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        let span = if let Some(right) = rights.last() {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprOr { left, rights }))
    }
    pub fn expr_and(&mut self) -> Parse<ExprAnd> {
        let left = self.expr_cmp()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .matches_as(Token::And, OpAnd)
            .map(|op| self.expr_cmp().map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        let span = if let Some(right) = rights.last() {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprAnd { left, rights }))
    }
    pub fn expr_cmp(&mut self) -> Parse<ExprCmp> {
        let left = self.expr_as()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .cmp()?
            .map(|op| self.expr_as().map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        let span = if let Some(right) = rights.last() {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprCmp { left, rights }))
    }
    pub fn expr_as(&mut self) -> Parse<ExprAS> {
        let left = self.expr_mdr()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .matches_as(Token::Plus, OpAS::Add)
            .or_else(|| self.matches_as(Token::Hyphen, OpAS::Sub))
            .map(|op| self.expr_mdr().map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        let span = if let Some(right) = rights.last() {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprAS { left, rights }))
    }
    pub fn expr_mdr(&mut self) -> Parse<ExprMDR> {
        let left = self.expr_call()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .matches_as(Token::Asterisk, OpMDR::Mul)
            .or_else(|| self.matches_as(Token::Slash, OpMDR::Div))
            .or_else(|| self.matches_as(Token::Percent, OpMDR::Rem))
            .map(|op| self.expr_call().map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        let span = if let Some(right) = rights.last() {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprMDR { left, rights }))
    }
    pub fn expr_call(&mut self) -> Parse<ExprCall> {
        let term = self.term()?;
        let mut end = term.span.end;
        let args = if self.matches(Token::OpenParen) {
            let mut args = Vec::new();
            while !self.matches(Token::CloseParen) {
                let expr = self.expression()?;
                end = expr.span.end;
                args.push(expr.data);
                if !self.matches(Token::Comma) {
                    self.require_token(Token::CloseParen)?;
                    break;
                }
            }
            Some(args)
        } else {
            None
        };
        Ok(term.span.start.to(end).sp(ExprCall {
            term: term.data,
            args,
        }))
    }
    pub fn term(&mut self) -> Parse<Term> {
        Ok(if self.matches(Token::OpenParen) {
            let expr = self.expression()?;
            self.require_token(Token::CloseParen)?;
            expr.map(Box::new).map(Term::Expr)
        } else if let Some(num) = self.num()? {
            num.map(Term::Num)
        } else if let Some(ident) = self.ident()? {
            ident.map(Term::Ident)
        } else if let Some(s) = self.string_literal()? {
            s.map(Term::String)
        } else if let Some(b) = self.boolean()? {
            b.map(Term::Bool)
        } else if let Some(function) = self.inline_function()? {
            function.map(Box::new).map(Term::Function)
        } else if let Some(nil) = self.take_if(Token::Nil) {
            nil.span.sp(Term::Nil)
        } else {
            return Err(ParseError::Expected("term".into()));
        })
    }
    pub fn inline_function(&mut self) -> MaybeParse<Function> {
        Ok(if let Some(fn_token) = self.take_if(Token::Fn) {
            self.require_token(Token::OpenParen)?;
            let mut args = Vec::new();
            while let Some(ident) = self.ident()? {
                args.push(ident.data);
                if !self.matches(Token::Comma) {
                    break;
                }
            }
            self.require_token(Token::CloseParen)?;
            let expr = self.expression()?;
            Some((fn_token.span | expr.span).sp(Function {
                args,
                body: expr.data,
            }))
        } else {
            None
        })
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(fmt = "{} = {}", "ident.bright_white()", expr)]
pub struct Assignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(bound = "O: StdDisplay, T: StdDisplay")]
#[display(
    fmt = "{}{}",
    "left.data",
    r#"rights.iter().map(|r| { format!(" {} {}", r.op.data, r.expr.data) }).collect::<String>()"#
)]
pub struct BinExpr<T, O> {
    pub left: Sp<T>,
    pub rights: Vec<Right<T, O>>,
}

pub trait Node {
    fn contains_ident(&self, ident: &str) -> bool;
    fn terms(&self) -> usize;
}

impl<T, O> Node for BinExpr<T, O>
where
    T: Node,
{
    fn contains_ident(&self, ident: &str) -> bool {
        self.left.data.contains_ident(ident)
            || self
                .rights
                .iter()
                .any(|right| right.expr.data.contains_ident(ident))
    }
    fn terms(&self) -> usize {
        self.left.data.terms()
            + self
                .rights
                .iter()
                .map(|right| right.expr.data.terms())
                .sum::<usize>()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Right<T, O> {
    pub op: Sp<O>,
    pub expr: Sp<T>,
}

impl<T, O> Right<T, O> {
    fn new(op: Sp<O>, expr: Sp<T>) -> Self {
        Right { op, expr }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(fmt = "or")]
pub struct OpOr;
#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(fmt = "and")]
pub struct OpAnd;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpAS {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpMDR {
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
}

pub type Expression = ExprOr;
pub type ExprOr = BinExpr<ExprAnd, OpOr>;
pub type ExprAnd = BinExpr<ExprCmp, OpAnd>;
pub type ExprCmp = BinExpr<ExprAS, OpCmp>;
pub type ExprAS = BinExpr<ExprMDR, OpAS>;
pub type ExprMDR = BinExpr<ExprCall, OpMDR>;

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(
    fmt = "{}{}",
    term,
    r#"args.as_ref().map(
        |args| format!("({})", args.iter().map(ToString::to_string).intersperse(", ".into()).collect::<String>())
    ).unwrap_or_default()"#
)]
pub struct ExprCall {
    pub term: Term,
    pub args: Option<Vec<Expression>>,
}

impl Node for ExprCall {
    fn contains_ident(&self, ident: &str) -> bool {
        self.term.contains_ident(ident)
            || self.args.as_ref().map_or(false, |args| {
                args.iter().any(|arg| arg.contains_ident(ident))
            })
    }
    fn terms(&self) -> usize {
        self.term.terms()
            + self
                .args
                .as_ref()
                .map_or(0, |args| args.iter().map(Node::terms).sum())
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(
    fmt = "{}{}",
    r#"disam.iter().map(|s| format!("{}.", s)).collect::<String>()"#,
    "name.as_deref().unwrap_or_default()"
)]
pub struct Path {
    pub disam: Vec<String>,
    pub name: Option<String>,
}

impl<S> From<S> for Path
where
    S: Into<String>,
{
    fn from(s: S) -> Self {
        Path::new(Option::<String>::None, s)
    }
}

impl Path {
    pub const GLOBAL: Self = Path {
        disam: Vec::new(),
        name: None,
    };
    pub fn new<D, N>(disam: D, name: N) -> Self
    where
        D: IntoIterator,
        D::Item: Into<String>,
        N: Into<String>,
    {
        Path {
            disam: disam.into_iter().map(Into::into).collect(),
            name: Some(name.into()),
        }
    }
    pub fn join<P>(&self, other: P) -> Self
    where
        P: Into<Path>,
    {
        let mut base = self.clone();
        let other = other.into();
        base.disam.extend(base.name);
        base.disam.extend(other.disam);
        base.name = other.name;
        base
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Term::Expr(expr) => expr.contains_ident(ident),
            Term::Ident(p) => p == ident,
            Term::Function(f) => f.body.contains_ident(ident) && !f.args.iter().any(|i| i == ident),
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
}

pub fn parse(input: &str) -> Result<Command, ParseError> {
    Tokens {
        iter: lex(input)?.into_iter(),
        put_back: Vec::new(),
    }
    .command()
    .map(|com| com.data)
}
