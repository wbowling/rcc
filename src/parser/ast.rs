use super::token::Token;
use super::token::Keyword;

use std::vec::IntoIter;
use std::iter::Peekable;
use std::collections::HashSet;

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
    Modulus,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    BitwiseLeft,
    BitwiseRight,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
}

#[derive(Debug)]
pub struct Program {
    pub func: Vec<Function>,
    pub globals: Vec<String>
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<String>,
    pub statements: Vec<Statement>,
    pub variables: Vec<String>
}

#[derive(Debug)]
pub enum Expression {
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    UnOp(UnOp, Box<Expression>),
    Int(u32),
    FunctionCall(String, Vec<Expression>),
    Variable(String),
}

#[derive(Debug)]
pub enum Statement {
    Assign(String, Expression),
    Declare(String, Expression),
    Return(Expression),
}

pub fn parse_program(tokens: &mut Peekable<IntoIter<Token>>) -> Program {
    let mut functions = Vec::new();
    let globals = HashSet::new();
    loop {
        match tokens.peek() {
            Some(_) => functions.push(parse_function(tokens).expect("Failed to parse function")),
            None => break
        }
    }

    if tokens.next().is_some() { panic!("Should be at the end") };
    Program { func: functions, globals: globals.into_iter().collect() }
}

fn next_token(tokens: &mut Peekable<IntoIter<Token>>) -> Token {
    match tokens.next() {
        Some(token) => Ok(token),
        _ => Err("Token not found")
    }.expect("failed to parse")
}

fn match_token(token: Token, tokens: &mut Peekable<IntoIter<Token>>) -> Result<Token, String> {
    let t = next_token(tokens);
    match t {
        _ if t == token => Ok((t)),
        other => Err(format!("Expected {:?}, found {:?}", token, other))
    }
}

fn match_keyword(keyword: Keyword, tokens: &mut Peekable<IntoIter<Token>>) -> Result<(), String> {
    let token = next_token(tokens);
    match token {
        Token::Keyword(ref k) if k == &keyword => Ok(()),
        other => Err(format!("Expected SemiColon, found {:?}", other))
    }
}

fn match_identifier(tokens: &mut Peekable<IntoIter<Token>>) -> Result<String, String> {
    match next_token(tokens) {
        Token::Identifier(n) => Ok((n)),
        other => Err(format!("Expected Identifier, found {:?}", other))
    }
}

fn parse_arguments(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Vec<String>, String> {
    let mut arguments = vec![];
    loop {
        match_keyword(Keyword::Int, tokens)?;
        arguments.push(match_identifier(tokens)?);
        match tokens.peek() {
            Some(&Token::CloseParen) => break,
            _ => {
                match_token(Token::Comma, tokens)?;
            }
        }
    }
    Ok(arguments)
}

fn parse_function(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Function, String> {
    match_keyword(Keyword::Int, tokens)?;
    let name = match_identifier(tokens)?;
    match_token(Token::OpenParen, tokens)?;

    let arguments: Vec<String> = match tokens.peek() {
        Some(&Token::CloseParen) => vec![],
        _ => parse_arguments(tokens).expect("Failed to parse function arguments"),
    };

    match_token(Token::CloseParen, tokens)?;
    match_token(Token::OpenBrace, tokens)?;

    let mut statements = vec![];
    let mut variables: HashSet<String> = HashSet::new();
    loop {
        if let Some(&Token::CloseBrace) = tokens.peek() {
            tokens.next();
            break
        } else {
            let statement = parse_statement(tokens);
            if let Statement::Declare(ref name, _) = statement {
                if variables.contains(name) {
                    return Err(format!("Variable alreay defined: {}", name))
                } else {
                    variables.insert(name.clone());
                }
            }
            statements.push(statement);
        }
    }

    Ok(Function { name, arguments, statements, variables: variables.into_iter().collect() })
}

fn parse_statement(tokens: &mut Peekable<IntoIter<Token>>) -> Statement {
    let state: Statement = match tokens.next() {
        Some(Token::Keyword(Keyword::Int)) => {
            let name = match (next_token(tokens), next_token(tokens)) {
                (Token::Identifier(n), Token::Assign) => Ok(n),
                other => Err(format!("Expected name =, found {:?}", other))
            }.expect("failed to parse");

            let exp = parse_expression(tokens);
            Ok(Statement::Declare(name, exp))
        }
        Some(Token::Keyword(Keyword::Return)) => {
            let exp = parse_expression(tokens);
            Ok(Statement::Return(exp))
        },
        Some(Token::Identifier(name)) => {
            match tokens.next() {
                Some(Token::Assign) => {
                    let exp = parse_expression(tokens);
                    Ok(Statement::Assign(name, exp))
                },
                other => Err(format!("Expected Assign, found {:?}", other))
            }
        },
        other => Err(format!("Expected return, found {:?}", other))
    }.expect("failed to parse");



    let res = match tokens.next() {
        Some(Token::SemiColon) => Ok(state),
        other => Err(format!("Expected SemiColon, found {:?}", other))
    }.expect("failed to parse");

    res
}

fn parse_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Or],
        &parse_logical_and_expression
    )
}

fn parse_logical_and_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::And],
        &parse_bitwise_or_expression
    )
}

fn parse_bitwise_or_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseOr],
        &parse_bitwise_xor_expression
    )
}


fn parse_bitwise_xor_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseXor],
        &parse_bitwise_and_expression
    )
}

fn parse_bitwise_and_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseAnd],
        &parse_equality_expression
    )
}

fn parse_equality_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Equal, Token::NotEqual],
        &parse_relational_expression
    )
}

fn parse_relational_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::LessThan, Token::GreaterThan, Token::LessThanOrEqual, Token::GreaterThanOrEqual],
        &parse_bitshift_expression
    )
}

fn parse_bitshift_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseLeft, Token::BitwiseRight],
        &parse_additive_expression
    )
}

fn parse_additive_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Negation, Token::Addition],
        &parse_multiplicative_expression)
}

fn parse_multiplicative_expression(tokens: &mut Peekable<IntoIter<Token>>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Multiplication, Token::Division, Token::Modulus],
        &parse_factor)
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
        Some(Token::Identifier(name)) => {
            match tokens.peek() {
                Some(&Token::OpenParen) => Expression::FunctionCall(name, parse_function_arguments(tokens)),
                _ => Expression::Variable(name)
            }

        }
        op @ _ => panic!("Unknown token: {:?}", op)

    }
}

fn parse_function_arguments(tokens: &mut Peekable<IntoIter<Token>>) -> Vec<Expression> {
    let mut arguments = vec![];
    tokens.next();
    loop {
        if let Some(&Token::CloseParen) = tokens.peek() {
            tokens.next();
            break
        } else {
            let exp = parse_expression(tokens);
            arguments.push(exp);
            if let Some(&Token::CloseParen) = tokens.peek() {
                tokens.next();
                break
            } else {
                if let Some(&Token::Comma) = tokens.peek() {
                    tokens.next();
                } else {
                    panic!("Invalid function call")
                }
            }
        }
    }

    arguments
}

fn parse_gen_experssion<F>(tokens: &mut Peekable<IntoIter<Token>>, matching: Vec<Token>, next: F) -> Expression
    where F: Fn(&mut Peekable<IntoIter<Token>>) -> Expression {
    let mut term = next(tokens);

    loop {
        match tokens.peek().map(|c| matching.contains(c)) {
            Some(true) => {
                let op = convert_binop(tokens.next());
                let next_term = next(tokens);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn convert_binop(token: Option<Token>) -> BinOp {
    match token {
        Some(Token::Multiplication) => BinOp::Multiplication,
        Some(Token::Division) => BinOp::Division,
        Some(Token::Modulus) => BinOp::Modulus,
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
        Some(Token::BitwiseLeft) => BinOp::BitwiseLeft,
        Some(Token::BitwiseRight) => BinOp::BitwiseRight,
        Some(Token::BitwiseAnd) => BinOp::BitwiseAnd,
        Some(Token::BitwiseXor) => BinOp::BitwiseXor,
        Some(Token::BitwiseOr) => BinOp::BitwiseOr,
        _ => panic!("Unsupported token {:?}")
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
