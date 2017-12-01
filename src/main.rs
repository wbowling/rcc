use std::fs::File;
use std::io::prelude::*;
use std::ascii::AsciiExt;

extern crate itertools;
use itertools::Itertools;

fn main() {
    let tokens = lex("testcases/return_2.c");
    println!("{:?}", tokens);
}

#[derive(Debug)]
enum Token {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    SemiColon,
    Keyword(String),
    Identifier(String),
    Literal(String),
    Unknown
}

fn lex(path: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];
    let contents = read_file(path);

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
                        let word = it.take_while_ref(|x| x.is_ascii() && x.is_alphabetic()).collect::<String>();
                        match is_keyword(&word) {
                            true =>  tokens.push(Token::Keyword(word)),
                            false => tokens.push(Token::Identifier(word))
                        }
                    },
                    '0'...'9' => {
                        let word = it.take_while_ref(|x| x.is_ascii() && x.is_digit(10)).collect::<String>();
                        tokens.push(Token::Literal(word))
                    },
                    _ => {
                        it.next();
                        tokens.push(Token::Unknown);
                    }
                };
            },
            None => break
        }
    }
    tokens
}

fn read_file(path: &str) -> String {
    let mut file = File::open(path).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    contents
}

fn is_keyword(word: &str) -> bool {
    let keywords = ["int", "return"];
    keywords.contains(&word)
}