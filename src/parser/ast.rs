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
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or
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
    Int(u32),
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
    let mut term = parse_logical_and_expression(tokens);

    loop {
        match tokens.peek() {
            Some(&Token::Or) => {
                let op = convert_binop(tokens.next());
                let next_term = parse_logical_and_expression(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn parse_logical_and_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_equality_expression(tokens);

    loop {
        match tokens.peek() {
            Some(&Token::And) => {
                let op = convert_binop(tokens.next());
                let next_term = parse_equality_expression(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn parse_equality_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_relational_expression(tokens);

    loop {
        match tokens.peek() {
            Some(&Token::Equal) | Some(&Token::NotEqual) => {
                let op = convert_binop(tokens.next());
                let next_term = parse_relational_expression(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn parse_relational_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_additive_expression(tokens);

    loop {
        match tokens.peek() {
            Some(&Token::LessThan)
            | Some(&Token::GreaterThan)
            | Some(&Token::LessThanOrEqual)
            | Some(&Token::GreaterThanOrEqual) => {
                let op = convert_binop(tokens.next());
                let next_term = parse_additive_expression(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn parse_additive_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_term(tokens);

    loop {
        match tokens.peek() {
            Some(&Token::Negation) | Some(&Token::Addition) => {
                let op = convert_binop(tokens.next());
                let next_term = parse_term(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn parse_term(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = parse_factor(tokens);

    loop {
        match tokens.peek() {
            Some(&Token::Multiplication) | Some(&Token::Division) => {
                let op = convert_binop(tokens.next());
                let next_factor = parse_factor(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_factor))
            },
            _ => break
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
                exp
            } else {
                panic!("Must close the paren")
            }
        },
        Some(op @ Token::Negation) | Some(op @ Token::LogicalNeg) | Some(op @ Token::BitComp) => {
            let factor = parse_factor(tokens);
            Expression::UnOp(convert_unop(op), Box::new(factor))
        },
        Some(Token::Literal(num)) => {
            Expression::Int(num)
        },
        op @ _ => panic!("Unknown token: {:?}", op)

    }
}

fn convert_binop(token: Option<Token>) -> BinOp {
    match token {
        Some(Token::Multiplication) => BinOp::Multiplication,
        Some(Token::Division) => BinOp::Division,
        Some(Token::Addition) => BinOp::Addition,
        Some(Token::Negation) => BinOp::Subtraction,
        Some(Token::LessThan) => BinOp::LessThan,
        Some(Token::LessThanOrEqual) => BinOp::LessThanOrEqual,
        Some(Token::GreaterThan) => BinOp::GreaterThan,
        Some(Token::GreaterThanOrEqual) => BinOp::GreaterThanOrEqual,
        Some(Token::Equal) => BinOp::Equal,
        Some(Token::NotEqual) => BinOp::NotEqual,
        Some(Token::And) => BinOp::And,
        Some(Token::Or) => BinOp::Or,
        _ => panic!("Unsupported token {:?}, can only use: * / + -")
    }
}

fn convert_unop(token: Token) -> UnOp {
    match token {
        Token::Negation => UnOp::Negation,
        Token::LogicalNeg => UnOp::LogicalNeg,
        Token::BitComp => UnOp::BitComp,
        _ => panic!("Unsupported token {:?}, can only use: ! ~ -")
    }
}
