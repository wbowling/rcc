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
    Literal(i32),
    Operation(char)
}

pub fn lex(contents: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut it = contents.chars().peekable();
    loop {
        match it.peek() {
            Some(&c) => {
                match c {
                    '{' => {
                        it.next();
                        tokens.push(Token::OpenBrace);
                    },
                    '}' => {
                        it.next();
                        tokens.push(Token::CloseBrace);
                    },
                    '(' => {
                        it.next();
                        tokens.push(Token::OpenParen);
                    },
                    ')' => {
                        it.next();
                        tokens.push(Token::CloseParen);
                    },
                    ';' => {
                        it.next();
                        tokens.push(Token::SemiColon);
                    },
                    ' ' | '\t' | '\n' | '\r' => {
                        it.next();
                    },
                    'a'...'z' | 'A'...'Z' => {
                        let word = it.take_while_ref(|x| x.is_ascii() && x.is_alphanumeric()).collect::<String>();
                        match is_keyword(&word) {
                            true =>  tokens.push(Token::Keyword(word)),
                            false => tokens.push(Token::Identifier(word))
                        }
                    },
                    '0'...'9' => {
                        let word = it.take_while_ref(|x| x.is_ascii() && x.is_digit(10)).collect::<String>();
                        let int: i32 = word.parse().expect("Not a number");
                        tokens.push(Token::Literal(int))
                    },
                    op @ '-' | op @ '+' => {
                        it.next();
                        tokens.push(Token::Operation(op));
                    }
                    other => {
                        panic!("Unknown token {:?}", other)
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