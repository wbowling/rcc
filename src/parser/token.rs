extern crate itertools;
use self::itertools::Itertools;
use std::iter::Peekable;
use std::str::Chars;

pub struct TokenParser<'a> {
    pub tokens: Vec<Token>,
    iter: Peekable<Chars<'a>>,
}

impl<'a> TokenParser<'a> {
    pub fn new(source: &str) -> TokenParser {
        TokenParser { tokens: vec![], iter: source.chars().peekable() }
    }

    pub fn push(&mut self, token: Token) {
        self.iter.next();
        self.tokens.push(token);
    }

    pub fn push_back(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    pub fn drop(&mut self) {
        self.iter.next();
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    pub fn get_string<F>(&mut self, func: F) -> String
        where F : Fn(&char) -> bool {
        self.iter.peeking_take_while(|c| func(c)).collect()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Keyword {
    Int,
    Return
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    SemiColon,
    Keyword(Keyword),
    Identifier(String),
    Literal(u32),
    BitComp,
    LogicalNeg,
    Negation,
    Addition,
    Multiplication,
    Division,
    Modulus,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    BitwiseLeft,
    BitwiseRight,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    Assign,
    Comma
}
