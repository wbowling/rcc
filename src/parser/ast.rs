extern crate itertools;

use super::token::Token;
use super::token::Keyword;
use super::ops::*;

use std::vec::IntoIter;

use self::itertools::structs::MultiPeek;
use itertools::chain;

pub fn parse_program(tokens: &mut MultiPeek<IntoIter<Token>>) -> Program {
    let mut functions = Vec::new();
    let globals = Vec::new();
    loop {
        tokens.reset_peek();
        match tokens.peek() {
            Some(_) => functions.push(parse_function(tokens).expect("Failed to parse function")),
            None => break
        }
    }

    if tokens.next().is_some() { panic!("Should be at the end") };
    Program { func: functions, globals: globals.into_iter().collect() }
}

fn parse_function(tokens: &mut MultiPeek<IntoIter<Token>>) -> Result<Function, String> {
    match_keyword(Keyword::Int, tokens)?;
    let name = match_identifier(tokens)?;
    match_token(Token::OpenParen, tokens)?;

    tokens.reset_peek();
    let arguments: Vec<String> = match tokens.peek() {
        Some(&Token::CloseParen) => Vec::new(),
        _ => parse_arguments(tokens).expect("Failed to parse function arguments"),
    };

    match_token(Token::CloseParen, tokens)?;
    match_token(Token::OpenBrace, tokens)?;

    let mut statements = vec![];
    let mut variables: Vec<String> = Vec::new();
    loop {
        tokens.reset_peek();
        if let Some(&Token::CloseBrace) = tokens.peek() {
            tokens.next();
            break
        } else {
            let statement = parse_statement(tokens, &chain(&variables, &arguments).collect());
            if let Statement::Declare(ref name, _) = statement {
                if variables.contains(name) || arguments.contains(name) {
                    return Err(format!("Variable alreay defined: {}", name))
                } else {
                    variables.push(name.clone());
                }
            }
            statements.push(statement);
        }
    }

    Ok(Function { name, arguments, statements, variables })
}

fn parse_statement(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Statement {
    tokens.reset_peek();
    let other = match tokens.peek() {
        Some(&Token::Keyword(Keyword::Int))
        | Some(&Token::Keyword(Keyword::Return)) => false,
        _ => true
    };

    let state: Statement = if other {
        let exp = parse_expression(tokens, variables);
        Ok(Statement::Exp(exp))
    } else {
        match tokens.next() {
            Some(Token::Keyword(Keyword::Int)) => {
                let name = match next_token(tokens) {
                    Token::Identifier(n) => Ok(n),
                    other => Err(format!("Expected identifier, found {:?}", other))
                }.expect("failed to parse");

                tokens.reset_peek();
                if let Some(&Token::SemiColon) = tokens.peek() {
                    Ok(Statement::Declare(name, None))
                } else if let Some(Token::Assign) = tokens.next() {
                    let exp = parse_expression(tokens, variables);
                    Ok(Statement::Declare(name, Some(exp)))
                } else {
                    Err(format!("Expected SemiColon or Assign"))
                }
            }
            Some(Token::Keyword(Keyword::Return)) => {
                let exp = parse_expression(tokens, variables);
                Ok(Statement::Return(exp))
            },
            o => Err(format!("Error, found {:?}", o))
        }
    }.expect("failed to parse");

    let res = match tokens.next() {
        Some(Token::SemiColon) => Ok(state),
        other => Err(format!("Expected SemiColon, found {:?}", other))
    }.expect("failed to parse");

    res
}

fn parse_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    tokens.reset_peek();
    let ident = if let Some(&Token::Identifier(_)) = tokens.peek() { true } else { false };
    let assign = if let Some(&Token::Assign) = tokens.peek() { true } else { false };
    if ident && assign {
        match (tokens.next(), tokens.next()) {
            (Some(Token::Identifier(name)), Some(Token::Assign)) => {
                if variables.contains(&&name) {
                    let exp = parse_expression(tokens, variables);
                    Expression::Assign(name, Box::new(exp))
                } else {
                    panic!("Variable {} not defined", name)
                }
            },
            _ => panic!("Should not be here")
        }
    } else {
        parse_or_expression(tokens, variables)
    }
}

fn parse_or_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Or],
        variables,
        &parse_logical_and_expression
    )
}

fn parse_logical_and_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::And],
        variables,
        &parse_bitwise_or_expression
    )
}

fn parse_bitwise_or_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseOr],
        variables,
        &parse_bitwise_xor_expression
    )
}


fn parse_bitwise_xor_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseXor],
        variables,
        &parse_bitwise_and_expression
    )
}

fn parse_bitwise_and_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseAnd],
        variables,
        &parse_equality_expression
    )
}

fn parse_equality_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Equal, Token::NotEqual],
        variables,
        &parse_relational_expression
    )
}

fn parse_relational_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::LessThan, Token::GreaterThan, Token::LessThanOrEqual, Token::GreaterThanOrEqual],
        variables,
        &parse_bitshift_expression
    )
}

fn parse_bitshift_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::BitwiseLeft, Token::BitwiseRight],
        variables,
        &parse_additive_expression
    )
}

fn parse_additive_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Negation, Token::Addition],
        variables,
        &parse_multiplicative_expression)
}

fn parse_multiplicative_expression(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    parse_gen_experssion(
        tokens,
        vec![Token::Multiplication, Token::Division, Token::Modulus],
        variables,
        &parse_factor)
}

fn parse_factor(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Expression {
    let next = tokens.next();
    match next {
        Some(Token::OpenParen) => {
            let exp = parse_expression(tokens, variables);
            if let Some(Token::CloseParen) = tokens.next() {
                exp
            } else {
                panic!("Must close the paren")
            }
        },
        Some(Token::Identifier(name)) => {
            tokens.reset_peek();
            match tokens.peek() {
                Some(&Token::OpenParen) => Expression::FunctionCall(name, parse_function_arguments(tokens, variables)),
                _ => Expression::Variable(name)
            }

        },
        Some(op @ Token::Negation) | Some(op @ Token::LogicalNeg) | Some(op @ Token::BitComp) => {
            let factor = parse_factor(tokens, variables);
            Expression::UnOp(op.into(), Box::new(factor))
        },
        Some(Token::Literal(num)) => {
            Expression::Int(num)
        },
        Some(Token::BitwiseAnd) => {
            match tokens.next() {
                Some(Token::Identifier(name)) => Expression::VariableRef(name),
                other => panic!("Only variables support &, found token: {:?}", other)
            }
        },
        op @ _ => panic!("Unknown token: {:?}", op)

    }
}

fn parse_function_arguments(tokens: &mut MultiPeek<IntoIter<Token>>, variables: &Vec<&String>) -> Vec<Expression> {
    let mut arguments = vec![];
    tokens.next();
    loop {
        tokens.reset_peek();
        if let Some(&Token::CloseParen) = tokens.peek() {
            tokens.next();
            break
        } else {
            let exp = parse_expression(tokens, variables);
            arguments.push(exp);
            tokens.reset_peek();
            if let Some(&Token::CloseParen) = tokens.peek() {
                tokens.next();
                break
            } else {
                tokens.reset_peek();
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

fn parse_arguments(tokens: &mut MultiPeek<IntoIter<Token>>) -> Result<Vec<String>, String> {
    let mut arguments = Vec::new();
    loop {
        match_keyword(Keyword::Int, tokens)?;
        let name = match_identifier(tokens)?;
        if arguments.contains(&name) {
            return Err(format!("Argument {} already defined", name))
        } else {
            arguments.push(name);
        }
        tokens.reset_peek();
        match tokens.peek() {
            Some(&Token::CloseParen) => break,
            _ => {
                match_token(Token::Comma, tokens)?;
            }
        }
    }
    Ok(arguments)
}

fn parse_gen_experssion<F>(tokens: &mut MultiPeek<IntoIter<Token>>, matching: Vec<Token>, variables: &Vec<&String>, next: F) -> Expression
    where F: Fn(&mut MultiPeek<IntoIter<Token>>, &Vec<&String>) -> Expression {
    let mut term = next(tokens, variables);

    loop {
        tokens.reset_peek();
        match tokens.peek().map(|c| matching.contains(c)) {
            Some(true) => {
                let op = tokens.next().unwrap().into();
                let next_term = next(tokens, variables);
                term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
            },
            _ => break
        }
    }
    term
}

fn next_token(tokens: &mut MultiPeek<IntoIter<Token>>) -> Token {
    match tokens.next() {
        Some(token) => Ok(token),
        _ => Err("Token not found")
    }.expect("failed to parse")
}

fn match_token(token: Token, tokens: &mut MultiPeek<IntoIter<Token>>) -> Result<Token, String> {
    let t = next_token(tokens);
    match t {
        _ if t == token => Ok((t)),
        other => Err(format!("Expected {:?}, found {:?}", token, other))
    }
}

fn match_keyword(keyword: Keyword, tokens: &mut MultiPeek<IntoIter<Token>>) -> Result<(), String> {
    let token = next_token(tokens);
    match token {
        Token::Keyword(ref k) if k == &keyword => Ok(()),
        other => Err(format!("Expected SemiColon, found {:?}", other))
    }
}

fn match_identifier(tokens: &mut MultiPeek<IntoIter<Token>>) -> Result<String, String> {
    match next_token(tokens) {
        Token::Identifier(n) => Ok((n)),
        other => Err(format!("Expected Identifier, found {:?}", other))
    }
}
