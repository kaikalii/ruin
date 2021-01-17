use std::fmt::{self, Display, Formatter};

use tokenate::*;

use crate::{num::Num, parse::OpCmp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Num(Num),
    Bool(bool),
    Nil,
    Equals,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Percent,
    OpenParen,
    CloseParen,
    And,
    Or,
    Cmp(OpCmp),
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Ident(ident) => ident.fmt(f),
            Token::Num(num) => num.fmt(f),
            Token::Bool(b) => b.fmt(f),
            Token::Nil => "nil".fmt(f),
            Token::Equals => '='.fmt(f),
            Token::Plus => '+'.fmt(f),
            Token::Hyphen => '-'.fmt(f),
            Token::Asterisk => '*'.fmt(f),
            Token::Slash => '/'.fmt(f),
            Token::Percent => '%'.fmt(f),
            Token::OpenParen => '('.fmt(f),
            Token::CloseParen => ')'.fmt(f),
            Token::And => "and".fmt(f),
            Token::Or => "or".fmt(f),
            Token::Cmp(cmp) => cmp.fmt(f),
        }
    }
}

fn num_pattern() -> impl Pattern<Token = Token> {
    pattern::chars(|c: char| c.is_digit(10) || "+-.e".contains(c))
        .parse::<Num>()
        .map(Token::Num)
}

fn ident_head(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn ident_body(c: char) -> bool {
    ident_head(c) || c.is_numeric()
}

fn ident_pattern() -> impl Pattern<Token = Token> {
    pattern::ident(ident_head, ident_body).map(Token::Ident)
}

fn command_pattern() -> impl Pattern<Token = Token> {
    ("<=".is(Token::Cmp(OpCmp::LessOrEqual)))
        .or(">=".is(Token::Cmp(OpCmp::GreaterOrEqual)))
        .or("<".is(Token::Cmp(OpCmp::Less)))
        .or(">".is(Token::Cmp(OpCmp::Greater)))
        .or('='.is(Token::Equals))
        .or('+'.is(Token::Plus))
        .or('-'.is(Token::Hyphen))
        .or('*'.is(Token::Asterisk))
        .or('/'.is(Token::Slash))
        .or('%'.is(Token::Percent))
        .or('('.is(Token::OpenParen))
        .or(')'.is(Token::CloseParen))
        .or("and".is(Token::And))
        .or("or".is(Token::Or))
        // Num
        .or(num_pattern())
        // Simple literals
        .or("nil".is(Token::Nil))
        .or("true".is(Token::Bool(true)))
        .or("false".is(Token::Bool(false)))
        .or("isnt".is(Token::Cmp(OpCmp::Isnt)))
        .or("is".is(Token::Cmp(OpCmp::Is)))
        // Ident
        .or(ident_pattern())
}

thread_local! {
    static COMMAND_PATTERN: Box<dyn Pattern<Token = Token>> = Box::new(command_pattern());
}

pub fn lex(input: &str) -> LexResult<Vec<Sp<Token>>> {
    COMMAND_PATTERN.with(move |pattern| {
        Chars::new(input.as_bytes()).tokenize(pattern, &char::is_whitespace.any())
    })
}
