use std::fmt::Display as StdDisplay;

use colored::Colorize;
use derive_more::Display;
use tokenate::{LexError, Sp};

use crate::{lexer::*, num::Num};

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

#[derive(Debug, Display)]
pub enum Command {
    #[display(fmt = "{}", _0)]
    Assignment(Assignment),
}

struct Tokens {
    iter: std::vec::IntoIter<Sp<Token>>,
    put_back: Option<Sp<Token>>,
}

impl Tokens {
    pub fn take(&mut self) -> Option<Sp<Token>> {
        self.put_back.take().or_else(|| self.iter.next())
    }
    pub fn take_as<F, T>(&mut self, f: F) -> Option<Sp<T>>
    where
        F: Fn(Token) -> Result<T, Token>,
    {
        if let Some(sp_token) = self.take() {
            match f(sp_token.data) {
                Ok(val) => Some(sp_token.span.sp(val)),
                Err(token) => {
                    self.put_back = Some(sp_token.span.sp(token));
                    None
                }
            }
        } else {
            None
        }
    }
    pub fn matching<P>(&mut self, pattern: P) -> Option<Sp<Token>>
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
        self.matching(pattern).map(|token| token.span.sp(val))
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
        self.matching(&pattern)
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
    pub fn assigment(&mut self) -> MaybeParse<Assignment> {
        let ident = if let Some(ident) = self.ident()? {
            ident
        } else {
            return Ok(None);
        };
        self.require_token(Token::Equals)?;
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
        let left = self.term()?;
        let mut rights = Vec::new();
        while let Some(right) = self
            .matches_as(Token::Asterisk, OpMDR::Mul)
            .or_else(|| self.matches_as(Token::Slash, OpMDR::Div))
            .or_else(|| self.matches_as(Token::Percent, OpMDR::Rem))
            .map(|op| self.term().map(|expr| Right::new(op, expr)))
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
    pub fn term(&mut self) -> Parse<Term> {
        Ok(if self.matches(Token::OpenParen) {
            let expr = self.expression()?;
            self.require_token(Token::CloseParen)?;
            expr.map(Box::new).map(Term::Expr)
        } else if let Some(num) = self.num()? {
            num.map(Term::Num)
        } else if let Some(b) = self.ident()? {
            b.map(Term::Ident)
        } else if let Some(s) = self.string_literal()? {
            s.map(Term::String)
        } else if let Some(ident) = self.boolean()? {
            ident.map(Term::Bool)
        } else if let Some(nil) = self.matching(Token::Nil) {
            nil.span.sp(Term::Nil)
        } else {
            return Err(ParseError::Expected("term".into()));
        })
    }
}

#[derive(Debug, Display)]
#[display(fmt = "{} = {}", "ident.bright_white()", expr)]
pub struct Assignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Display)]
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

#[derive(Debug)]
pub struct Right<T, O> {
    pub op: Sp<O>,
    pub expr: Sp<T>,
}

impl<T, O> Right<T, O> {
    fn new(op: Sp<O>, expr: Sp<T>) -> Self {
        Right { op, expr }
    }
}

#[derive(Debug, Display)]
#[display(fmt = "or")]
pub struct OpOr;
#[derive(Debug, Display)]
#[display(fmt = "and")]
pub struct OpAnd;

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

#[derive(Debug, Display, Clone, Copy)]
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
pub type ExprMDR = BinExpr<Term, OpMDR>;

#[derive(Debug, Display)]
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
    #[display(fmt = "{}", "\"nil\".blue()")]
    Nil,
}

impl Node for Term {
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Term::Expr(expr) => expr.contains_ident(ident),
            Term::Ident(i) => i == ident,
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
        put_back: None,
    }
    .command()
    .map(|com| com.data)
}
