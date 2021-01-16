use tokenate::*;

use crate::num::Num;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Num(Num),
    Equals,
}

fn num_pattern() -> impl Pattern<Token = Token> {
    pattern::chars(|c: char| c.is_alphanumeric() || "-.".contains(c))
        .parse::<Num>()
        .map(Token::Num)
}

fn command_pattern() -> impl Pattern<Token = Token> {
    ('='.any().is(Token::Equals))
        // Num
        .or(num_pattern())
        // Ident
        .or(pattern::not_whitespace.any().map(Token::Ident))
}

thread_local! {
    static COMMAND_PATTERN: Box<dyn Pattern<Token = Token>> = Box::new(command_pattern());
}

pub fn lex(input: &str) -> LexResult<Vec<Sp<Token>>> {
    COMMAND_PATTERN.with(move |pattern| {
        Chars::new(input.as_bytes()).tokenize(pattern, &char::is_whitespace.any())
    })
}
