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
    pub statements: Vec<Statement>
}

#[derive(Debug)]
pub enum Expression {
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    UnOp(UnOp, Box<Expression>),
    Int(u32),
    FunctionCall(String),
    Variable(String),
}

#[derive(Debug)]
pub enum Statement {
    Assign(String, Expression),
    Return(Expression),
}

pub fn parse_program(tokens: &mut Peekable<IntoIter<Token>>) -> Program {
    let mut functions = Vec::new();
    let mut globals: Vec<String> = Vec::new();
    while let Some(_) = tokens.peek() {
        functions.push(parse_function(tokens, &mut globals))
    }

    if tokens.next().is_some() { panic!("Should be at the end") };
    Program { func: functions, globals }
}

fn next_token(tokens: &mut Peekable<IntoIter<Token>>) -> Token {
    match tokens.next() {
        Some(token) => Ok(token),
        _ => Err("Token not found")
    }.expect("failed to parse")
}

fn parse_function(tokens: &mut Peekable<IntoIter<Token>>, globals: &mut Vec<String>) -> Function {
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
        let mut statements = vec![];
        loop {
            if let Some(&Token::CloseBrace) = tokens.peek() {
                tokens.next();
                break
            } else {
                statements.push(parse_statement(tokens, globals))
            }
        }

        Ok(Function { name, statements })
    }).expect("failed to parse")
}

fn parse_statement(tokens: &mut Peekable<IntoIter<Token>>, globals: &mut Vec<String>) -> Statement {
    let state: Statement = match tokens.next() {
        Some(Token::Keyword(ref word)) if word == "int" => {
            let name = match (next_token(tokens), next_token(tokens)) {
                (Token::Identifier(n), Token::Assign) => Ok(n),
                other => Err(format!("Expected name =, found {:?}", other))
            }.expect("failed to parse");
            globals.push(name.clone());
            let exp = parse_expression(tokens);
            Ok(Statement::Assign(name, exp))
        }
        Some(Token::Keyword(ref word)) if word == "return" => {
            let exp = parse_expression(tokens);
            Ok(Statement::Return(exp))
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
            Expression::Variable(name)
        }
        op @ _ => panic!("Unknown token: {:?}", op)

    }
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
