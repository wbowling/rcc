use super::token::Token;
use std::vec::IntoIter;

#[derive(Debug)]
pub struct Program {
    pub func: Function
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub statement: Statement
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

#[derive(Debug)]
pub enum Expression {
    Int(u32)
}

pub fn parse_program(tokens: &mut IntoIter<Token>) -> Program {
    let fun = parse_function(tokens);
    if tokens.next().is_some() { panic!("Should be at the end") };
    Program { func: fun }
}

fn next_token(tokens: &mut IntoIter<Token>) -> Token {
    match tokens.next() {
        Some(token) => Ok(token),
        _ => Err("Token not found")
    }.expect("failed to parse")
}

fn parse_function(tokens: &mut IntoIter<Token>) -> Function {
    match next_token(tokens) {
        Token::Keyword(ref word) if word == "int" => Ok(()),
        _ => Err("Expected int")
    }.and_then(|_| {
        match next_token(tokens) {
            Token::Identifier(n) => Ok(n),
            _ => Err("Expected name")
        }
    }).and_then(|name| {
        match next_token(tokens) {
            Token::OpenParen => Ok(name),
            _ => Err("Expected OpenParen")
        }
    }).and_then(|name| {
        match next_token(tokens) {
            Token::CloseParen => Ok(name),
            _ => Err("Expected CloseParen")
        }
    }).and_then(|name| {
        match next_token(tokens) {
            Token::OpenBrace => Ok(name),
            _ => Err("Expected OpenBrace")
        }
    }).and_then(|name| {
        let statement = parse_statement(tokens);
        match next_token(tokens) {
            Token::CloseBrace => Ok(Function { name, statement }),
            _ => Err("Expected CloseBrace")
        }
    }).expect("failed to parse")
}

fn parse_statement(tokens: &mut IntoIter<Token>) -> Statement {
    match tokens.next() {
        Some(Token::Keyword(ref word)) if word == "return" => Ok(true),
        _ => Err("Expected return")
    }.expect("failed to parse");

    let exp = parse_expression(tokens);
    let state = Statement::Return(exp);

    let res = match tokens.next() {
        Some(Token::SemiColon) => Ok(state),
        _ => Err("Expected SemiColon")
    }.expect("failed to parse");

    res
}

fn parse_expression(tokens: &mut IntoIter<Token>) -> Expression {
    let lit = match tokens.next() {
        Some(Token::Literal(word)) => Ok(Expression::Int(word)),
        _ => Err("Expected int literal")
    };
    lit.expect("Failed to parse")
}