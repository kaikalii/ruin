use std::{fmt::Display as StdDisplay, io::Read};

use derive_more::Display;
use tokenate::{LexError, LexResult};

use crate::{ast::*, lexer::*, num::Num, value::*};

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

pub fn parse<R>(input: R) -> Result<Command, ParseError>
where
    R: Read,
{
    Tokens::new(input)?.command()
}
