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

pub type Parsed<T> = Result<T, ParseError>;
pub type MaybeParsed<T> = Result<Option<T>, ParseError>;

pub fn parse<R>(input: R) -> Result<Command, ParseError>
where
    R: Read,
{
    Command::parse(&mut Tokens::new(input)?)
}

pub trait TokenPattern: StdDisplay {
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

pub struct Tokens {
    iter: std::vec::IntoIter<Token>,
    history: Vec<Token>,
    cursor: usize,
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
    pub fn require_token<P>(&mut self, pattern: P) -> Parsed<Token>
    where
        P: TokenPattern,
    {
        self.take_if(&pattern)
            .ok_or_else(|| ParseError::Expected(pattern.to_string()))
    }
    pub fn require<F, T>(&mut self, f: F, name: &str) -> Parsed<T>
    where
        F: Fn(&mut Self) -> MaybeParsed<T>,
    {
        f(self).and_then(|op| op.ok_or_else(|| ParseError::Expected(name.into())))
    }
}

impl Parse for String {
    const EXPECTATION: &'static str = "identifier";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        Ok(tokens.take_as(|token| {
            if let Token::Ident(s) = token {
                Some(s.clone())
            } else {
                None
            }
        }))
    }
}

impl Parse for Num {
    const EXPECTATION: &'static str = "number";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        Ok(tokens.take_as(|token| {
            if let Token::Num(num) = token {
                Some(*num)
            } else {
                None
            }
        }))
    }
}

impl Parse for bool {
    const EXPECTATION: &'static str = "boolean";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        Ok(tokens.take_as(|token| {
            if let Token::Bool(b) = token {
                Some(*b)
            } else {
                None
            }
        }))
    }
}

impl Parse for OpCmp {
    const EXPECTATION: &'static str = "comparison";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        Ok(tokens.take_as(|token| {
            if let Token::Cmp(cmp) = token {
                Some(*cmp)
            } else {
                None
            }
        }))
    }
}

impl Parse for Command {
    const EXPECTATION: &'static str = "command";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        if let Some(decl) = FunctionDecl::try_parse(tokens)? {
            Ok(Command::FunctionDecl(decl))
        } else if let Some(ass) = Assignment::try_parse(tokens)? {
            Ok(Command::Assignment(ass))
        } else if tokens.matches(Token::Slash) {
            Ok(Command::Command)
        } else {
            Expression::parse(tokens).map(Command::Eval)
        }
        .map(Some)
    }
}

impl Parse for Assignment {
    const EXPECTATION: &'static str = "assignment";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let tracker = tokens.track();
        let ident = if let Some(ident) = String::try_parse(tokens)? {
            ident
        } else {
            return Ok(None);
        };
        if !tokens.matches(Token::Equals) {
            tokens.revert(tracker);
            return Ok(None);
        }
        let expr = Expression::parse(tokens)?;
        Ok(Some(Assignment { ident, expr }))
    }
}

impl Parse for FunctionDecl {
    const EXPECTATION: &'static str = "function declaration";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        Ok(if tokens.matches(Token::Fn) {
            let ident = String::parse(tokens)?;
            tokens.require_token(Token::OpenParen)?;
            let args = Args::parse(tokens)?;
            tokens.require_token(Token::CloseParen)?;
            let expr = Expression::parse(tokens)?;
            Some(FunctionDecl {
                ident,
                function: Function {
                    args,
                    body: expr.into(),
                    env: Default::default(),
                    bar: false,
                },
            })
        } else {
            None
        })
    }
}

impl Parse for Args {
    const EXPECTATION: &'static str = "arguments";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let mut idents = Vec::new();
        while let Some(ident) = String::try_parse(tokens)? {
            idents.push(ident);
            if !tokens.matches(Token::Comma) {
                break;
            }
        }
        Ok(Some(Args { idents }))
    }
}

impl Parse for ExprOr {
    const EXPECTATION: &'static str = "'or' expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let left = ExprAnd::parse(tokens)?;
        let mut rights = Vec::new();
        while let Some(right) = tokens
            .matches_as(Token::Or, OpOr)
            .map(|op| ExprAnd::parse(tokens).map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        Ok(Some(ExprOr::new(left, rights)))
    }
}

impl Parse for ExprAnd {
    const EXPECTATION: &'static str = "'and' expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let left = ExprCmp::parse(tokens)?;
        let mut rights = Vec::new();
        while let Some(right) = tokens
            .matches_as(Token::And, OpAnd)
            .map(|op| ExprCmp::parse(tokens).map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        Ok(Some(ExprAnd::new(left, rights)))
    }
}

impl Parse for ExprCmp {
    const EXPECTATION: &'static str = "comparison expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let left = ExprAS::parse(tokens)?;
        let mut rights = Vec::new();
        while let Some(right) = OpCmp::try_parse(tokens)?
            .map(|op| ExprAS::parse(tokens).map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        Ok(Some(ExprCmp::new(left, rights)))
    }
}

impl Parse for ExprAS {
    const EXPECTATION: &'static str = "add-substract expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let left = ExprMDR::parse(tokens)?;
        let mut rights = Vec::new();
        while let Some(right) = tokens
            .matches_as(Token::Plus, OpAS::Add)
            .or_else(|| tokens.matches_as(Token::Hyphen, OpAS::Sub))
            .map(|op| ExprMDR::parse(tokens).map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        Ok(Some(ExprAS::new(left, rights)))
    }
}

impl Parse for ExprMDR {
    const EXPECTATION: &'static str = "multiply-divide-remainder expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let left = ExprNot::parse(tokens)?;
        let mut rights = Vec::new();
        while let Some(right) = tokens
            .matches_as(Token::Asterisk, OpMDR::Mul)
            .or_else(|| tokens.matches_as(Token::Slash, OpMDR::Div))
            .or_else(|| tokens.matches_as(Token::PeArcent, OpMDR::Rem))
            .map(|op| ExprNot::parse(tokens).map(|expr| Right::new(op, expr)))
            .transpose()?
        {
            rights.push(right);
        }
        Ok(Some(ExprMDR::new(left, rights)))
    }
}

impl Parse for ExprNot {
    const EXPECTATION: &'static str = "'not' expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let mut count = 0;
        while tokens.matches(Token::Not) {
            count += 1;
        }
        let expr = ExprCall::parse(tokens)?;
        Ok(Some(ExprNot {
            op: OpNot,
            count,
            expr,
        }))
    }
}

impl Parse for ExprCall {
    const EXPECTATION: &'static str = "call expression";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        let first = Term::parse(tokens)?;
        let mut method_call_syntax = false;
        let mut calls = Vec::new();
        while tokens.matches(Token::Colon) {
            method_call_syntax = true;
            let term = Term::parse(tokens)?;
            let args = tokens.require(Tokens::arg_exprs, "arguments")?;
            calls.push(Call { term, args });
        }
        if method_call_syntax {
            return Ok(Some(ExprCall::Method { first, calls }));
        }
        let args = tokens.arg_exprs()?;
        Ok(Some(ExprCall::Regular { term: first, args }))
    }
}

impl Parse for Term {
    const EXPECTATION: &'static str = "term";
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self> {
        Ok(Some(if tokens.matches(Token::OpenParen) {
            let expr = Expression::parse(tokens)?;
            tokens.require_token(Token::CloseParen)?;
            Term::Expr(expr.into())
        } else if let Some(num) = Num::try_parse(tokens)? {
            Term::Num(num)
        } else if let Some(ident) = String::try_parse(tokens)? {
            Term::Ident(ident)
        } else if let Some(s) = tokens.string_literal()? {
            Term::String(s)
        } else if let Some(b) = bool::try_parse(tokens)? {
            Term::Bool(b)
        } else if let Some(function) = tokens.inline_function()? {
            Term::Function(function.into())
        } else if tokens.matches(Token::Nil) {
            Term::Nil
        } else {
            return Ok(None);
        }))
    }
}

impl Tokens {
    pub fn string_literal(&mut self) -> MaybeParsed<String> {
        Ok(self.take_as(|token| {
            if let Token::String(s) = token {
                Some(s.clone())
            } else {
                None
            }
        }))
    }
    pub fn arg_exprs(&mut self) -> MaybeParsed<Vec<Expression>> {
        Ok(if self.matches(Token::OpenParen) {
            let mut args = Vec::new();
            loop {
                if self.matches(Token::CloseParen) {
                    break;
                }
                let expr = Expression::parse(self)?;
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
    pub fn inline_function(&mut self) -> MaybeParsed<Function> {
        let mut bar = false;
        let opening = self.matches(Token::Fn) || {
            bar = true;
            self.matches(Token::Bar)
        };
        Ok(if opening {
            if !bar {
                self.require_token(Token::OpenParen)?;
            }
            let mut args = Args::default();
            while let Some(ident) = String::try_parse(self)? {
                args.idents.push(ident);
                if !self.matches(Token::Comma) {
                    break;
                }
            }
            self.require_token(if bar { Token::Bar } else { Token::CloseParen })?;
            let expr = Expression::parse(self)?;
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
}

pub trait Parse: Sized {
    const EXPECTATION: &'static str;
    fn try_parse(tokens: &mut Tokens) -> MaybeParsed<Self>;
    fn parse(tokens: &mut Tokens) -> Parsed<Self> {
        tokens.require(Self::try_parse, Self::EXPECTATION)
    }
}
