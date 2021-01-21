use std::{
    fmt::{self, Display, Formatter},
    io::Read,
};

use tokenate::*;

use crate::{num::Num, parse::OpCmp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    Num(Num),
    Bool(bool),
    String(String),
    Nil,
    Equals,
    Plus,
    Hyphen,
    Asterisk,
    Slash,
    PeArcent,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    And,
    Or,
    Cmp(OpCmp),
    Fn,
    Period,
    Comma,
    Not,
    Colon,
    Bar,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Token::Ident(ident) => ident.fmt(f),
            Token::Num(num) => num.fmt(f),
            Token::Bool(b) => b.fmt(f),
            Token::String(s) => write!(f, "{:?}", s),
            Token::Nil => "nil".fmt(f),
            Token::Equals => '='.fmt(f),
            Token::Plus => '+'.fmt(f),
            Token::Hyphen => '-'.fmt(f),
            Token::Asterisk => '*'.fmt(f),
            Token::Slash => '/'.fmt(f),
            Token::PeArcent => '%'.fmt(f),
            Token::OpenParen => '('.fmt(f),
            Token::CloseParen => ')'.fmt(f),
            Token::OpenCurly => '{'.fmt(f),
            Token::CloseCurly => '}'.fmt(f),
            Token::And => "and".fmt(f),
            Token::Or => "or".fmt(f),
            Token::Cmp(cmp) => cmp.fmt(f),
            Token::Fn => "fn".fmt(f),
            Token::Period => '.'.fmt(f),
            Token::Comma => ','.fmt(f),
            Token::Not => "not".fmt(f),
            Token::Colon => ':'.fmt(f),
            Token::Bar => '|'.fmt(f),
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

fn string_literal(chars: &mut Chars) -> TokenResult<String> {
    Ok(if chars.take_if(|c| c == '"')?.is_some() {
        let mut arg = String::new();
        let mut escaped = false;
        while let Some(c) = chars.take()? {
            match c {
                '\\' if escaped.take() => arg.push('\\'),
                '\\' => escaped = true,
                '"' if escaped.take() => arg.push('"'),
                '"' => break,
                'n' if escaped.take() => arg.push('\n'),
                'r' if escaped.take() => arg.push('\r'),
                't' if escaped.take() => arg.push('\t'),
                c if escaped => {
                    return Err(LexError::Custom(format!("Invalid escape char: {:?}", c)))
                }
                c => arg.push(c),
            }
        }
        Some(arg)
    } else {
        None
    })
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
        .or('%'.is(Token::PeArcent))
        .or('('.is(Token::OpenParen))
        .or(')'.is(Token::CloseParen))
        .or('{'.is(Token::OpenCurly))
        .or('}'.is(Token::CloseCurly))
        .or('.'.is(Token::Period))
        .or(','.is(Token::Comma))
        .or(':'.is(Token::Colon))
        .or("|".is(Token::Bar))
        .or("and".is(Token::And))
        .or("or".is(Token::Or))
        // Num
        .or(num_pattern())
        // String
        .or(string_literal.map(Token::String))
        // Keywords
        .or("nil".is(Token::Nil))
        .or("true".is(Token::Bool(true)))
        .or("false".is(Token::Bool(false)))
        .or("isnt".is(Token::Cmp(OpCmp::Isnt)))
        .or("is".is(Token::Cmp(OpCmp::Is)))
        .or("fn".is(Token::Fn))
        .or("not".is(Token::Not))
        // Ident
        .or(ident_pattern())
}

thread_local! {
    static COMMAND_PATTERN: Box<dyn Pattern<Token = Token>> = Box::new(command_pattern());
}

pub fn lex<R>(input: R) -> LexResult<Vec<Sp<Token>>>
where
    R: Read,
{
    COMMAND_PATTERN
        .with(move |pattern| Chars::new(input).tokenize(pattern, &char::is_whitespace.any()))
}
