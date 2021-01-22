use std::{fmt::Display as StdDisplay, io::Read};

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use tokenate::{LexError, LexResult};

use crate::{lexer::*, num::Num, value::*};

#[derive(Debug, Display)]
pub enum ParseError {
    Lex(LexError),
    #[display(fmt = "Expected {}", _0)]
    Expected(String),
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> Self {
        ParseError::Lex(e)
    }
}

pub type Parse<T> = Result<T, ParseError>;
pub type MaybeParse<T> = Result<Option<T>, ParseError>;

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
    Eval(Expression),
    Command,
}

struct Tokens {
    iter: std::vec::IntoIter<Token>,
    history: Vec<Token>,
    cursor: usize,
    // revert_trackers: usize,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct RevertHandle(usize);

impl Tokens {
    pub fn new<R>(input: R) -> LexResult<Self>
    where
        R: Read,
    {
        Ok(Tokens {
            iter: lex(input)?.into_iter(),
            history: Vec::new(),
            cursor: 0,
            // revert_trackers: 0,
        })
    }
    pub fn take(&mut self) -> Option<Token> {
        let token = if self.cursor < self.history.len() {
            Some(&self.history[self.cursor])
        } else if let Some(token) = self.iter.next() {
            self.history.push(token);
            self.history.last()
        } else {
            None
        };
        if let Some(token) = token {
            self.cursor += 1;
            Some(token.clone())
        } else {
            None
        }
    }
    #[allow(dead_code)]
    pub fn peek(&mut self) -> Option<&Token> {
        if self.cursor == self.history.len() {
            self.take()?;
            self.put_back();
        }
        self.history.get(self.cursor)
    }
    pub fn take_as<F, T>(&mut self, f: F) -> Option<T>
    where
        F: Fn(&Token) -> Option<T>,
    {
        if let Some(sp_token) = self.take() {
            match f(&sp_token) {
                Some(val) => Some(val),
                None => {
                    self.put_back();
                    None
                }
            }
        } else {
            None
        }
    }
    fn put_back(&mut self) {
        self.cursor -= 1;
    }
    pub fn track(&mut self) -> RevertHandle {
        // self.revert_trackers += 1;
        RevertHandle(self.cursor)
    }
    pub fn revert(&mut self, handle: RevertHandle) {
        self.cursor = handle.0;
        // self.revert_trackers -= 1;
        // if self.revert_trackers == 0 {
        //     self.cursor = 0;
        //     self.history.clear();
        // }
    }
    pub fn take_if<P>(&mut self, pattern: P) -> Option<Token>
    where
        P: TokenPattern,
    {
        if let Some(token) = self.take() {
            if pattern.matches(&token) {
                Some(token)
            } else {
                self.put_back();
                None
            }
        } else {
            None
        }
    }
    pub fn matches_as<P, T>(&mut self, pattern: P, val: T) -> Option<T>
    where
        P: TokenPattern,
    {
        self.take_if(pattern).map(|_| val)
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
    pub fn require<F, T>(&mut self, f: F, name: &str) -> Parse<T>
    where
        F: Fn(&mut Self) -> MaybeParse<T>,
    {
        f(self).and_then(|op| op.ok_or_else(|| ParseError::Expected(name.into())))
    }
    pub fn ident(&mut self) -> MaybeParse<String> {
        Ok(self.take_as(|token| {
            if let Token::Ident(s) = token {
                Some(s.clone())
            } else {
                None
            }
        }))
    }
    pub fn string_literal(&mut self) -> MaybeParse<String> {
        Ok(self.take_as(|token| {
            if let Token::String(s) = token {
                Some(s.clone())
            } else {
                None
            }
        }))
    }
    pub fn num(&mut self) -> MaybeParse<Num> {
        Ok(self.take_as(|token| {
            if let Token::Num(num) = token {
                Some(*num)
            } else {
                None
            }
        }))
    }
    pub fn boolean(&mut self) -> MaybeParse<bool> {
        Ok(self.take_as(|token| {
            if let Token::Bool(b) = token {
                Some(*b)
            } else {
                None
            }
        }))
    }
    pub fn cmp(&mut self) -> MaybeParse<OpCmp> {
        Ok(self.take_as(|token| {
            if let Token::Cmp(cmp) = token {
                Some(*cmp)
            } else {
                None
            }
        }))
    }
    pub fn command(&mut self) -> Result<Command, ParseError> {
        if let Some(ass) = self.assigment()? {
            Ok(Command::Assignment(ass))
        } else if self.matches(Token::Slash) {
            Ok(Command::Command)
        } else {
            self.expression().map(Command::Eval)
        }
    }
    pub fn assigment(&mut self) -> MaybeParse<Assignment> {
        let tracker = self.track();
        let ident = if let Some(ident) = self.ident()? {
            ident
        } else {
            return Ok(None);
        };
        if !self.matches(Token::Equals) {
            self.revert(tracker);
            return Ok(None);
        }
        let expr = self.expression()?;
        Ok(Some(Assignment { ident, expr }))
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
        Ok(ExprOr::new(left, rights))
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
        Ok(ExprAnd::new(left, rights))
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
        Ok(ExprCmp::new(left, rights))
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
        Ok(ExprAS::new(left, rights))
    }
    pub fn expr_mdr(&mut self) -> Parse<ExprMDR> {
        let left = self.expr_not()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .matches_as(Token::Asterisk, OpMDR::Mul)
            .or_else(|| self.matches_as(Token::Slash, OpMDR::Div))
            .or_else(|| self.matches_as(Token::PeArcent, OpMDR::Rem))
            .map(|op| self.expr_not().map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        Ok(ExprMDR::new(left, rights))
    }
    #[allow(dead_code)]
    fn dbg(&self, line: u32) {
        print!("{}: ", line);
        for token in &self.history {
            print!("{} ", token);
        }
        println!("{}", self.cursor);
    }
    pub fn expr_not(&mut self) -> Parse<ExprNot> {
        let mut count = 0;
        while self.matches(Token::Not) {
            count += 1;
        }
        let expr = self.expr_call()?;
        Ok(ExprNot {
            op: OpNot,
            count,
            expr,
        })
    }
    pub fn expr_call(&mut self) -> Parse<ExprCall> {
        let first = self.term()?;
        let mut method_call_syntax = false;
        let mut calls = Vec::new();
        while self.matches(Token::Colon) {
            method_call_syntax = true;
            let term = self.term()?;
            let args = self.require(Self::args, "arguments")?;
            calls.push(Call { term, args });
        }
        if method_call_syntax {
            return Ok(ExprCall::Method { first, calls });
        }
        let args = self.args()?;
        Ok(ExprCall::Regular { term: first, args })
    }
    pub fn args(&mut self) -> MaybeParse<Vec<Expression>> {
        Ok(if self.matches(Token::OpenParen) {
            let mut args = Vec::new();
            loop {
                if self.matches(Token::CloseParen) {
                    break;
                }
                let expr = self.expression()?;
                args.push(expr);
                if !self.matches(Token::Comma) {
                    break;
                }
            }
            Some(args)
        } else {
            None
        })
    }
    pub fn term(&mut self) -> Parse<Term> {
        Ok(if self.matches(Token::OpenParen) {
            let expr = self.expression()?;
            self.require_token(Token::CloseParen)?;
            Term::Expr(expr.into())
        } else if let Some(num) = self.num()? {
            Term::Num(num)
        } else if let Some(ident) = self.ident()? {
            Term::Ident(ident)
        } else if let Some(s) = self.string_literal()? {
            Term::String(s)
        } else if let Some(b) = self.boolean()? {
            Term::Bool(b)
        } else if let Some(function) = self.inline_function()? {
            Term::Function(function.into())
        } else if self.matches(Token::Nil) {
            Term::Nil
        } else {
            return Err(ParseError::Expected("term".into()));
        })
    }
    pub fn inline_function(&mut self) -> MaybeParse<Function> {
        let mut bar = false;
        let opening = self.matches(Token::Fn) || {
            bar = true;
            self.matches(Token::Bar)
        };
        Ok(if opening {
            if !bar {
                self.require_token(Token::OpenParen)?;
            }
            let mut args = Vec::new();
            while let Some(ident) = self.ident()? {
                args.push(ident);
                if !self.matches(Token::Comma) {
                    break;
                }
            }
            self.require_token(if bar { Token::Bar } else { Token::CloseParen })?;
            let expr = self.expression()?;
            Some(Function {
                args,
                body: expr.into(),
                env: Default::default(),
                bar,
            })
        } else {
            None
        })
    }
    pub fn _list_or_table(&mut self) -> MaybeParse<Value> {
        let _start = if let Some(open_curly) = self.take_if(Token::OpenCurly) {
            open_curly
        } else {
            return Ok(None);
        };
        todo!()
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{} = {}", "ident.bright_white()", expr)]
pub struct Assignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(bound = "O: StdDisplay, T: StdDisplay")]
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
    fn new(op: O, expr: T) -> Self {
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

pub fn parse<R>(input: R) -> Result<Command, ParseError>
where
    R: Read,
{
    Tokens::new(input)?.command()
}
