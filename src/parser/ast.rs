use super::token::Token;
use std::vec::IntoIter;
use std::iter::Peekable;

#[derive(Debug)]
pub enum UnOp {
    Negation,
    BitComp,
    LogicalNeg,
}

#[derive(Debug)]
pub enum BinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Debug)]
pub struct Program {
    pub func: Vec<Function>
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub statement: Statement
}

#[derive(Debug)]
pub enum Expression {
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    UnOp(UnOp, Box<Expression>),
    Int(u64),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

pub fn parse_program(tokens: &mut Peekable<IntoIter<Token>>) -> Program {
    let mut functions = Vec::new();
    while let Some(_) = tokens.peek() {
        functions.push(parse_function(tokens));
    }

    if tokens.next().is_some() { panic!("Should be at the end") };
    Program { func: functions }
}

fn next_token(tokens: &mut Peekable<IntoIter<Token>>) -> Token {
    match tokens.next() {
        Some(token) => Ok(token),
        _ => Err("Token not found")
    }.expect("failed to parse")
}

fn parse_function(tokens: &mut Peekable<IntoIter<Token>>) -> Function {
    match next_token(tokens) {
        Token::Keyword(ref word) if word == "int" => Ok(()),
        other => Err(format!("Expected SemiColon, found {:?}", other))
    }.and_then(|_| {
        match next_token(tokens) {
            Token::Identifier(n) => Ok(n),
            other => Err(format!("Expected name, found {:?}", other))
        }
    }).and_then(|name| {
        match next_token(tokens) {
            Token::OpenParen => Ok(name),
            other => Err(format!("Expected OpenParen, found {:?}", other))
        }
    }).and_then(|name| {
        match next_token(tokens) {
            Token::CloseParen => Ok(name),
            other => Err(format!("Expected CloseParen, found {:?}", other))
        }
    }).and_then(|name| {
        match next_token(tokens) {
            Token::OpenBrace => Ok(name),
            other => Err(format!("Expected OpenBrace, found {:?}", other))
        }
    }).and_then(|name| {
        let statement = parse_statement(tokens);
        match next_token(tokens) {
            Token::CloseBrace => Ok(Function { name, statement }),
            other => Err(format!("Expected CloseBrace, found {:?}", other))
        }
    }).expect("failed to parse")
}

fn parse_statement(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    match tokens.next() {
        Some(Token::Keyword(ref word)) if word == "return" => Ok(true),
        other => Err(format!("Expected return, found {:?}", other))
    }.expect("failed to parse");

    let exp = parse_expression(tokens);
    let state = Statement::Return(exp);

    let res = match tokens.next() {
        Some(Token::SemiColon) => Ok(state),
        other => Err(format!("Expected SemiColon, found {:?}", other))
    }.expect("failed to parse");

    res
}

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_term(tokens);
    let mut gogo = true;
    while gogo {
        gogo = match tokens.peek() {
            Some(&Token::Negation) => {
                tokens.next();
                let next_term = parse_term(tokens);
                term = Expression::BinOp(BinOp::Subtraction, Box::new(term), Box::new(next_term));
                true
            },
            Some(&Token::Addition) => {
                tokens.next();
                let next_term = parse_term(tokens);
                term = Expression::BinOp(BinOp::Addition, Box::new(term), Box::new(next_term));
                true
            },
            _ => false
        }
    }
    term
}

fn parse_term(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_factor(tokens);
    let mut gogo = true;
    while gogo {
        gogo = match tokens.peek() {
            Some(&Token::Multiplication) => {
                tokens.next();
                let next_factor = parse_factor(tokens);
                term = Expression::BinOp(BinOp::Multiplication, Box::new(term), Box::new(next_factor));
                true
            },
            Some(&Token::Division) => {
                tokens.next();
                let next_factor = parse_factor(tokens);
                term = Expression::BinOp(BinOp::Division, Box::new(term), Box::new(next_factor));
                true
            },
            _ => false
        }
    }
    term
}

fn parse_factor(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let next = tokens.next();
    match next {
        Some(Token::OpenParen) => {
            let exp = parse_expression(tokens);
            if let Some(Token::CloseParen) = tokens.next() {
                return exp
            } else {
                panic!("Must close the paren")
            }
        },
        Some(op @ Token::Negation) | Some(op @ Token::LogicalNeg) | Some(op @ Token::BitComp) => {
            let factor = parse_factor(tokens);
            return Expression::UnOp(convert_op(op), Box::new(factor))
        },
        Some(Token::Literal(num)) => {
            return Expression::Int(num)
        },
        op @ _ => panic!("Unknown token: {:?}", op)

    }
}

fn convert_op(token: Token) -> UnOp {
    match token {
        Token::Negation => UnOp::Negation,
        Token::LogicalNeg => UnOp::LogicalNeg,
        Token::BitComp => UnOp::BitComp,
        _ => panic!("Unsupported token {:?}, can only use !,~,-")
    }
}
