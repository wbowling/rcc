extern crate itertools;
use self::itertools::Itertools;
use std::ascii::AsciiExt;


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
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual

}

pub fn lex(contents: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut it = contents.chars().peekable();
    loop {
        match it.next() {
            Some(c) => {
                match c {
                    '{' => tokens.push(Token::OpenBrace),
                    '}' => tokens.push(Token::CloseBrace),
                    '(' => tokens.push(Token::OpenParen),
                    ')' => tokens.push(Token::CloseParen),
                    ';' => tokens.push(Token::SemiColon),
                    ' ' | '\t' | '\n' | '\r' => (),
                    'a'...'z' | 'A'...'Z' => {
                        let word = format!("{}{}",
                                           c,
                                           it.take_while_ref(|x| x.is_ascii() && x.is_alphanumeric()).collect::<String>());
                        match is_keyword(&word) {
                            true =>  tokens.push(Token::Keyword(word)),
                            false => tokens.push(Token::Identifier(word))
                        }
                    },
                    '0'...'9' => {
                        let word = format!("{}{}",
                                           c,
                                           it.take_while_ref(|x| x.is_ascii() && x.is_digit(10)).collect::<String>());
                        let int: u32 = word.parse().expect("Not a number");
                        tokens.push(Token::Literal(int))
                    },
                    '-' => tokens.push(Token::Negation),
                    '~' => tokens.push(Token::BitComp),
                    '+' => tokens.push(Token::Addition),
                    '*' => tokens.push(Token::Multiplication),
                    '/' => tokens.push(Token::Division),
                    '<' | '>' | '!' => {
                        match it.peek() {
                            Some(&'=') => {
                                it.next();
                                match c {
                                    '<' => tokens.push(Token::LessThanOrEqual),
                                    '>' => tokens.push(Token::GreaterThanOrEqual),
                                    '!' => tokens.push(Token::NotEqual),
                                    _ => ()
                                }
                            },
                            _ =>  match c {
                                '<' => tokens.push(Token::LessThan),
                                '>' => tokens.push(Token::GreaterThan),
                                '!' => tokens.push(Token::LogicalNeg),
                                _ => ()
                            }
                        }
                    }
                    other => {
                        match (other, it.next()) {
                            ('&', Some('&')) => tokens.push(Token::And),
                            ('|', Some('|')) => tokens.push(Token::Or),
                            ('<', Some('=')) => tokens.push(Token::LessThanOrEqual),
                            ('>', Some('=')) => tokens.push(Token::GreaterThanOrEqual),
                            ('=', Some('=')) => tokens.push(Token::Equal),
                            ('!', Some('=')) => tokens.push(Token::NotEqual),
                            _ => panic!("Unknown token {:?}", other),
                        }
                    }
                };
            },
            None => break
        }
    }
    tokens
}

fn is_keyword(word: &str) -> bool {
    let keywords = ["int", "return"];
    keywords.contains(&word)
}
