use std::fs::File;
use std::io::prelude::*;
use std::ascii::AsciiExt;
use std::slice::Iter;

extern crate itertools;
use itertools::Itertools;

fn main() {
    let tokens = lex("testcases/return_2.c");
    let prog = parse_program(tokens.iter());
    println!("{:?}", prog);
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
    Literal(u32),
    Unknown
}

#[derive(Debug)]
struct Program {
    func: Function
}

#[derive(Debug)]
struct Function {
    name: String,
    statement: Statement
}

#[derive(Debug)]
enum Statement {
    Return(Expression)
}

#[derive(Debug)]
enum Expression {
    Int(u32)
}

fn parse_program(tokens: Iter<Token>) -> Program {
    let (fun, mut tokens) = parse_function(tokens);
    match tokens.next() {
        None => Ok(()),
        _ => Err("Should be at the end")
    }.expect("parse error");

    Program { func: fun }
}

fn parse_function(mut tokens: Iter<Token>) -> (Function, Iter<Token>) {
    match tokens.next() {
        Some(&Token::Keyword(ref word)) if word == "int" => Ok(true),
        _ => Err("Expected int")
    }.expect("failed to parse");

    let name = match tokens.next() {
        Some(&Token::Identifier(ref n)) => Ok(n),
        _ => Err("Expected name")
    }.expect("failed to parse");

    match tokens.next() {
        Some(&Token::OpenParen) => Ok(()),
        _ => Err("Expected OpenParen")
    }.expect("failed to parse");

    match tokens.next() {
        Some(&Token::CloseParen) => Ok(()),
        _ => Err("Expected CloseParen")
    }.expect("failed to parse");

    match tokens.next() {
        Some(&Token::OpenBrace) => Ok(()),
        _ => Err("Expected OpenBrace")
    }.expect("failed to parse");

    let (statements, mut tokens) = parse_statement(tokens);

    match tokens.next() {
        Some(&Token::CloseBrace) => Ok(()),
        _ => Err("Expected CloseBrace")
    }.expect("failed to parse");

    (Function { name: name.to_owned(), statement: statements }, tokens)
}

fn parse_statement(mut tokens: Iter<Token>) -> (Statement, Iter<Token>) {
    match tokens.next() {
        Some(&Token::Keyword(ref word)) if word == "return" => Ok(true),
        _ => Err("Expected return")
    }.expect("failed to parse");

    let (exp, mut tokens) = parse_expression(tokens);
    let state = Statement::Return(exp);

    let res = match tokens.next() {
        Some(&Token::SemiColon) => Ok(state),
        _ => Err("Expected SemiColon")
    }.expect("failed to parse");

    (res, tokens)
}

fn parse_expression(mut tokens: Iter<Token>) -> (Expression, Iter<Token>) {
    let lit = match tokens.next() {
        Some(&Token::Literal(word)) => Ok(Expression::Int(word)),
        _ => Err("Expected int literal")
    };
    (lit.expect("Failed to parse"), tokens)
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
                        let word = it.take_while_ref(|x| x.is_ascii() && x.is_alphanumeric()).collect::<String>();
                        match is_keyword(&word) {
                            true =>  tokens.push(Token::Keyword(word)),
                            false => tokens.push(Token::Identifier(word))
                        }
                    },
                    '0'...'9' => {
                        let word = it.take_while_ref(|x| x.is_ascii() && x.is_digit(10)).collect::<String>();
                        let int: u32 = word.parse().expect("Not a number");
                        tokens.push(Token::Literal(int))
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