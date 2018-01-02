extern crate itertools;
use self::itertools::Itertools;
use std::ascii::AsciiExt;
use std::iter::Peekable;
use std::str::Chars;

struct TokenParser<'a> {
    tokens: Vec<Token>,
    iter: Peekable<Chars<'a>>,
}

impl<'a> TokenParser<'a> {
    pub fn new(source: &'a String) -> TokenParser<'a> {
        TokenParser { tokens: vec![], iter: source.chars().peekable() }
    }

    fn push(&mut self, token: Token) {
        self.iter.next();
        self.tokens.push(token);
    }

    fn push_back(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn drop(&mut self) {
        self.iter.next();
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn get_string<F>(&mut self, func: F) -> String
        where F : Fn(&char) -> bool {
        self.iter.peeking_take_while(|c| func(c)).collect()
    }
}

#[derive(Debug)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    SemiColon,
    Keyword(String),
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

}

pub fn lex(contents: String) -> Vec<Token> {
    let mut tokens = TokenParser::new(&contents);

    loop {
        match tokens.peek() {
            Some(&c) => {
                match c {
                    '{' => tokens.push(Token::OpenBrace),
                    '}' => tokens.push(Token::CloseBrace),
                    '(' => tokens.push(Token::OpenParen),
                    ')' => tokens.push(Token::CloseParen),
                    ';' => tokens.push(Token::SemiColon),
                    ' ' | '\t' | '\n' | '\r' => tokens.drop(),
                    'a'...'z' | 'A'...'Z' => {
                        let word = tokens.get_string(|x| x.is_ascii() && x.is_alphanumeric());
                        match is_keyword(&word) {
                            true =>  tokens.push_back(Token::Keyword(word)),
                            false => tokens.push_back(Token::Identifier(word))
                        }
                    },
                    '0'...'9' => {
                        let word = tokens.get_string(|x| x.is_ascii() && x.is_digit(10));
                        let int: u32 = word.parse().expect("Not a number");
                        tokens.push_back(Token::Literal(int))
                    },
                    '-' => tokens.push(Token::Negation),
                    '~' => tokens.push(Token::BitComp),
                    '+' => tokens.push(Token::Addition),
                    '*' => tokens.push(Token::Multiplication),
                    '/' => tokens.push(Token::Division),
                    '%' => tokens.push(Token::Modulus),
                    multi => {
                        match (tokens.next().unwrap(), tokens.peek()) {
                            ('&', Some(&'&')) => tokens.push(Token::And),
                            ('|', Some(&'|')) => tokens.push(Token::Or),
                            ('<', Some(&'=')) => tokens.push(Token::LessThanOrEqual),
                            ('>', Some(&'=')) => tokens.push(Token::GreaterThanOrEqual),
                            ('=', Some(&'=')) => tokens.push(Token::Equal),
                            ('!', Some(&'=')) => tokens.push(Token::NotEqual),
                            ('<', Some(&'<')) => tokens.push(Token::BitwiseLeft),
                            ('>', Some(&'>')) => tokens.push(Token::BitwiseRight),
                            ('<', _) => tokens.push_back(Token::LessThan),
                            ('>', _) => tokens.push_back(Token::GreaterThan),
                            ('!', _) => tokens.push_back(Token::LogicalNeg),
                            ('&', _) => tokens.push_back(Token::BitwiseAnd),
                            _ => panic!("Unknown token {:?}", multi),
                        }
                    }
                };
            },
            None => break
        }
    }
    tokens.tokens
}

fn is_keyword(word: &str) -> bool {
    let keywords = ["int", "return"];
    keywords.contains(&word)
}
