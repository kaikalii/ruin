use std::{fmt::Display as StdDisplay, io::Read, path::PathBuf, str::FromStr};

use colored::Colorize;
use derive_more::Display;
use itertools::Itertools;
use tokenate::{LexError, LexResult, Sp};

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
    Eval(Expression),
    Command,
}

struct Tokens {
    iter: std::vec::IntoIter<Sp<Token>>,
    history: Vec<Sp<Token>>,
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
    pub fn take(&mut self) -> Option<Sp<Token>> {
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
    pub fn peek(&mut self) -> Option<&Sp<Token>> {
        if self.cursor == self.history.len() {
            self.take()?;
            self.put_back();
        }
        self.history.get(self.cursor)
    }
    pub fn take_as<F, T>(&mut self, f: F) -> Option<Sp<T>>
    where
        F: Fn(&Token) -> Option<T>,
    {
        if let Some(sp_token) = self.take() {
            match f(&sp_token.data) {
                Some(val) => Some(sp_token.span.sp(val)),
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
    pub fn take_if<P>(&mut self, pattern: P) -> Option<Sp<Token>>
    where
        P: TokenPattern,
    {
        if let Some(token) = self.take() {
            if pattern.matches(&token.data) {
                Some(token)
            } else {
                self.put_back();
                None
            }
        } else {
            None
        }
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
    pub fn path(&mut self) -> MaybeParse<Path> {
        let mut idents = Vec::new();
        let mut start = None;
        let mut end = None;
        while let Some(ident) = self.ident()? {
            start.get_or_insert(ident.span.start);
            end = Some(ident.span.end);
            idents.push(ident.data);
            if !self.matches(Token::Period) {
                break;
            }
        }
        Ok(if let Some(name) = idents.pop() {
            Some(start.unwrap().to(end.unwrap()).sp(Path::new(idents, name)))
        } else {
            None
        })
    }
    pub fn command(&mut self) -> Result<Command, ParseError> {
        if let Some(ass) = self.assigment()? {
            Ok(Command::Assignment(ass.data))
        } else if self.matches(Token::Slash) {
            Ok(Command::Command)
        } else {
            self.expression().map(|expr| Command::Eval(expr.data))
        }
    }
    pub fn assigment(&mut self) -> MaybeParse<Assignment> {
        let tracker = self.track();
        let path = if let Some(path) = self.path()? {
            path
        } else {
            return Ok(None);
        };
        if !self.matches(Token::Equals) {
            self.revert(tracker);
            return Ok(None);
        }
        let expr = self.expression()?;
        let ass = path.join(expr, |path, expr| Assignment { path, expr });
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
        Ok(span.sp(ExprOr::new(left, rights)))
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
        Ok(span.sp(ExprAnd::new(left, rights)))
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
        Ok(span.sp(ExprCmp::new(left, rights)))
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
        Ok(span.sp(ExprAS::new(left, rights)))
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
        let span = if let Some(right) = rights.last() {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprMDR::new(left, rights)))
    }
    #[allow(dead_code)]
    fn dbg(&self, line: u32) {
        print!("{}: ", line);
        for token in &self.history {
            print!("{} ", token.data);
        }
        println!("{}", self.cursor);
    }
    pub fn expr_not(&mut self) -> Parse<ExprNot> {
        let mut count = 0;
        let mut start = None;
        while let Some(not) = self.take_if(Token::Not) {
            start = Some(not.span.start);
            count += 1;
        }
        let expr = self.expr_call()?;
        Ok(start
            .unwrap_or(expr.span.start)
            .to(expr.span.end)
            .sp(ExprNot {
                op: OpNot,
                count,
                expr: expr.data,
            }))
    }
    pub fn expr_call(&mut self) -> Parse<ExprCall> {
        let first = self.term()?;
        let mut end = first.span.end;
        let mut args: Option<Vec<Expression>> = None;
        let (term, method_call_syntax) = if self.matches(Token::Colon) {
            let span = first.span;
            args = Some(vec![ExprOr::wrapping(span.sp(ExprAnd::wrapping(span.sp(
                ExprCmp::wrapping(span.sp(ExprAS::wrapping(span.sp(ExprMDR::wrapping(
                    first.map(ExprCall::wrapping).map(ExprNot::wrapping),
                ))))),
            ))))]);
            (self.require(Self::ident, "ident")?.map(Term::Ident), true)
        } else {
            (first, false)
        };
        let has_args = if method_call_syntax {
            self.require_token(Token::OpenParen)?;
            true
        } else {
            self.matches(Token::OpenParen)
        };
        if has_args {
            args.get_or_insert_with(Vec::new);
        }
        if has_args {
            while !self.matches(Token::CloseParen) {
                let expr = self.expression()?;
                end = expr.span.end;
                args.as_mut().unwrap().push(expr.data);
                if !self.matches(Token::Comma) {
                    self.require_token(Token::CloseParen)?;
                    break;
                }
            }
        }
        Ok(term.span.start.to(end).sp(ExprCall {
            term: term.data,
            args,
            method_call_syntax,
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
                body: expr.data.into(),
                env: Default::default(),
            }))
        } else {
            None
        })
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(fmt = "{} = {}", "path.to_string().bright_white()", expr)]
pub struct Assignment {
    pub path: Path,
    pub expr: Expression,
}

#[derive(Debug, Display, Clone, PartialEq, Eq)]
#[display(bound = "O: StdDisplay, T: StdDisplay")]
#[display(
    fmt = "{}{}",
    "left.data",
    r#"rights.iter().map(|r| { format!(" {} {}", r.op.data, r.expr.data) }).collect::<String>()"#
)]
pub struct BinExpr<O, T> {
    pub left: Box<Sp<T>>,
    pub rights: Vec<Right<O, T>>,
}

impl<O, T> BinExpr<O, T> {
    pub fn new(left: Sp<T>, rights: Vec<Right<O, T>>) -> Self {
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
    type Child = Sp<T>;
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
    fn wrapping(child: Self::Child) -> Self {
        BinExpr::new(child, Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Right<O, T> {
    pub op: Sp<O>,
    pub expr: Sp<T>,
}

impl<O, T> Right<O, T> {
    fn new(op: Sp<O>, expr: Sp<T>) -> Self {
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
#[display(fmt = "{}", "self.format()")]
pub struct ExprCall {
    pub term: Term,
    pub args: Option<Vec<Expression>>,
    pub method_call_syntax: bool,
}

impl ExprCall {
    fn format(&self) -> String {
        if let Some(args) = &self.args {
            // if self.method_call_syntax {
            //     format!(
            //         "{}:{}({})",
            //         args[0],
            //         self.term,
            //         args.iter()
            //             .skip(1)
            //             .map(ToString::to_string)
            //             .intersperse(", ".into())
            //             .collect::<String>()
            //     )
            // } else {
            format!(
                "{}({})",
                self.term,
                args.iter()
                    .map(ToString::to_string)
                    .intersperse(", ".into())
                    .collect::<String>()
            )
        // }
        } else {
            self.term.to_string()
        }
    }
}

impl Node for ExprCall {
    type Child = Term;
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
    fn wrapping(child: Self::Child) -> Self {
        ExprCall {
            term: child,
            args: None,
            method_call_syntax: false,
        }
    }
}

#[derive(Debug, Display, Clone, PartialEq, Eq, Hash)]
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

#[allow(dead_code)]
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
    pub fn as_path_buf(&self) -> PathBuf {
        let mut path = PathBuf::new();
        path.extend(&self.disam);
        path.extend(&self.name);
        path.with_extension("ruin")
    }
    pub fn parent(&self) -> Option<Self> {
        let mut disam = self.disam.clone();
        if let Some(name) = disam.pop() {
            Some(Path::new(disam, name))
        } else {
            None
        }
    }
}

impl FromStr for Path {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Tokens::new(s.as_bytes())?
            .require(Tokens::path, "path")
            .map(|path| path.data)
    }
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
