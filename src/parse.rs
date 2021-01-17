use std::fmt::Display as StdDisplay;

use derive_more::Display;
use tokenate::{LexError, Sp};

use crate::{lexer::*, num::Num, types::Type};

#[derive(Debug, Display)]
pub enum ParseError {
    #[display(fmt = "{}", _0)]
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
        let right = self
            .matches_as(Token::Or, OpOr)
            .map(|op| self.expr_and().map(|expr| Right::new(op, expr)))
            .transpose()?;
        let span = if let Some(right) = &right {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprOr { left, right }))
    }
    pub fn expr_and(&mut self) -> Parse<ExprAnd> {
        let left = self.expr_as()?;
        let right = self
            .matches_as(Token::And, OpAnd)
            .map(|op| self.expr_as().map(|expr| Right::new(op, expr)))
            .transpose()?;
        let span = if let Some(right) = &right {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprAnd { left, right }))
    }
    pub fn expr_as(&mut self) -> Parse<ExprAS> {
        let left = self.expr_mdr()?;
        let right = self
            .matches_as(Token::Plus, OpAS::Add)
            .or_else(|| self.matches_as(Token::Hyphen, OpAS::Sub))
            .map(|op| self.expr_mdr().map(|expr| Right::new(op, expr)))
            .transpose()?;
        let span = if let Some(right) = &right {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprAS { left, right }))
    }
    pub fn expr_mdr(&mut self) -> Parse<ExprMDR> {
        let left = self.term()?;
        let right = self
            .matches_as(Token::Asterisk, OpMDR::Mul)
            .or_else(|| self.matches_as(Token::Slash, OpMDR::Div))
            .or_else(|| self.matches_as(Token::Percent, OpMDR::Rem))
            .map(|op| self.term().map(|expr| Right::new(op, expr)))
            .transpose()?;
        let span = if let Some(right) = &right {
            left.span | right.op.span
        } else {
            left.span
        };
        Ok(span.sp(ExprMDR { left, right }))
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
#[display(fmt = "{} = {}", ident, expr)]
pub struct Assignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Display)]
#[display(bound = "O: StdDisplay, T: StdDisplay")]
#[display(
    fmt = "{}{}",
    "left.data",
    r#"if let Some(r) = &right { format!(" {} {}", r.op.data, r.expr.data) } else { "".into() }"#
)]
pub struct BinExpr<T, O> {
    pub left: Sp<T>,
    pub right: Option<Right<T, O>>,
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

#[derive(Debug, Display)]
pub enum OpAS {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Debug, Display)]
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
pub type ExprAnd = BinExpr<ExprAS, OpAnd>;
pub type ExprAS = BinExpr<ExprMDR, OpAS>;
pub type ExprMDR = BinExpr<Term, OpMDR>;

#[derive(Debug, Display)]
pub enum Term {
    #[display(fmt = "({})", _0)]
    Expr(Box<Expression>),
    #[display(fmt = "{}", _0)]
    Num(Num),
    #[display(fmt = "{}", _0)]
    Ident(String),
    #[display(fmt = "{}", _0)]
    Bool(bool),
    #[display(fmt = "nil")]
    Nil,
}

impl Term {
    pub fn ty(&self) -> Type {
        match self {
            Term::Expr(expr) => todo!(),
            Term::Num(_) => Type::Number,
            Term::Ident(_) => todo!(),
            Term::Bool(_) => Type::Bool,
            Term::Nil => Type::Nil,
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
