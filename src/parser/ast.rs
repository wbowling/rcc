extern crate itertools;

use super::token::Token;
use super::token::Keyword;
use super::ops::*;

use std::vec::IntoIter;

use self::itertools::structs::MultiPeek;
use itertools::chain;

pub struct Parser {
    tokens: MultiPeek<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens:  itertools::multipeek(tokens) }
    }

    pub fn parse(&mut self) -> Program {
        self.parse_program()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.reset_peek();
        self.tokens.peek()
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn is_empty(&mut self) -> bool {
        self.tokens.reset_peek();
        match self.peek() {
            Some(_) => false,
            _ => true,
        }
    }


    fn parse_program(&mut self) -> Program {
        let mut functions = Vec::new();
        let globals = Vec::new();
        loop {
            match self.peek() {
                Some(_) => functions.push(self.parse_function().expect("Failed to parse function")),
                None => break
            }
        }

        if !self.is_empty() { panic!("Should be at the end") };
        Program { func: functions, globals: globals.into_iter().collect() }
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        self.match_keyword(Keyword::Int)?;
        let name = self.match_identifier()?;
        self.match_token(Token::OpenParen)?;

        let arguments: Vec<String> = match self.peek() {
            Some(&Token::CloseParen) => Vec::new(),
            _ => self.parse_arguments().expect("Failed to parse function arguments"),
        };

        self.match_token(Token::CloseParen)?;
        self.match_token(Token::OpenBrace)?;

        let mut statements = vec![];
        let mut variables: Vec<String> = Vec::new();
        loop {
            if let Some(&Token::CloseBrace) = self.peek() {
                self.next();
                break
            } else {
                let statement = self.parse_statement(&chain(&variables, &arguments).collect());
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

    fn parse_statement(&mut self, variables: &Vec<&String>) -> Statement {
        let other = match self.peek() {
            Some(&Token::Keyword(Keyword::Int))
            | Some(&Token::Keyword(Keyword::Return)) => false,
            _ => true
        };

        let state: Statement = if other {
            let exp = self.parse_expression(variables);
            Ok(Statement::Exp(exp))
        } else {
            match self.next() {
                Some(Token::Keyword(Keyword::Int)) => {
                    let name = match self.next_token() {
                        Token::Identifier(n) => Ok(n),
                        other => Err(format!("Expected identifier, found {:?}", other))
                    }.expect("failed to parse");

                    if let Some(&Token::SemiColon) = self.peek() {
                        Ok(Statement::Declare(name, None))
                    } else if let Some(Token::Assign) = self.next() {
                        let exp = self.parse_expression(variables);
                        Ok(Statement::Declare(name, Some(exp)))
                    } else {
                        Err(format!("Expected SemiColon or Assign"))
                    }
                }
                Some(Token::Keyword(Keyword::Return)) => {
                    let exp = self.parse_expression(variables);
                    Ok(Statement::Return(exp))
                },
                o => Err(format!("Error, found {:?}", o))
            }
        }.expect("failed to parse");

        let res = match self.next() {
            Some(Token::SemiColon) => Ok(state),
            other => Err(format!("Expected SemiColon, found {:?}", other))
        }.expect("failed to parse");

        res
    }

    fn parse_expression(&mut self, variables: &Vec<&String>) -> Expression {
        let ident = if let Some(&Token::Identifier(_)) = self.peek() { true } else { false };
        let assign = if let Some(&Token::Assign) = self.peek() { true } else { false };
        if ident && assign {
            match (self.next(), self.next()) {
                (Some(Token::Identifier(name)), Some(Token::Assign)) => {
                    if variables.contains(&&name) {
                        let exp = self.parse_expression(variables);
                        Expression::Assign(name, Box::new(exp))
                    } else {
                        panic!("Variable {} not defined", name)
                    }
                },
                _ => panic!("Should not be here")
            }
        } else {
            self.parse_or_expression(variables)
        }
    }


    fn parse_or_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::Or],
            variables,
             &Parser::parse_logical_and_expression
        )
    }

    fn parse_logical_and_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::And],
            variables,
            &Parser::parse_bitwise_or_expression
        )
    }

    fn parse_bitwise_or_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::BitwiseOr],
            variables,
            &Parser::parse_bitwise_xor_expression
        )
    }


    fn parse_bitwise_xor_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::BitwiseXor],
            variables,
            &Parser::parse_bitwise_and_expression
        )
    }

    fn parse_bitwise_and_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::BitwiseAnd],
            variables,
            &Parser::parse_equality_expression
        )
    }

    fn parse_equality_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::Equal, Token::NotEqual],
            variables,
            &Parser::parse_relational_expression
        )
    }

    fn parse_relational_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::LessThan, Token::GreaterThan, Token::LessThanOrEqual, Token::GreaterThanOrEqual],
            variables,
            &Parser::parse_bitshift_expression
        )
    }

    fn parse_bitshift_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::BitwiseLeft, Token::BitwiseRight],
            variables,
            &Parser::parse_additive_expression
        )
    }

    fn parse_additive_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::Negation, Token::Addition],
            variables,
            &Parser::parse_multiplicative_expression
        )
    }

    fn parse_multiplicative_expression(&mut self, variables: &Vec<&String>) -> Expression {
        self.parse_gen_experssion(
            vec![Token::Multiplication, Token::Division, Token::Modulus],
            variables,
            &Parser::parse_factor
        )
    }

    fn parse_factor(&mut self, variables: &Vec<&String>) -> Expression {
        let next = self.next();
        match next {
            Some(Token::OpenParen) => {
                let exp = self.parse_expression(variables);
                if let Some(Token::CloseParen) = self.next() {
                    exp
                } else {
                    panic!("Must close the paren")
                }
            },
            Some(Token::Identifier(name)) => {
                match self.peek() {
                    Some(&Token::OpenParen) => Expression::FunctionCall(name, self.parse_function_arguments(variables)),
                    _ => Expression::Variable(name)
                }
            },
            Some(op @ Token::Negation) | Some(op @ Token::LogicalNeg) | Some(op @ Token::BitComp) => {
                let factor = self.parse_factor(variables);
                Expression::UnOp(op.into(), Box::new(factor))
            },
            Some(Token::Literal(num)) => {
                Expression::Int(num)
            },
            Some(Token::BitwiseAnd) => {
                match self.next() {
                    Some(Token::Identifier(name)) => Expression::VariableRef(name),
                    other => panic!("Only variables support &, found token: {:?}", other)
                }
            },
            op @ _ => panic!("Unknown token: {:?}", op)
        }
    }

    fn parse_function_arguments(&mut self, variables: &Vec<&String>) -> Vec<Expression> {
        let mut arguments = vec![];
        self.next();
        loop {
            if let Some(&Token::CloseParen) = self.peek() {
                self.next();
                break
            } else {
                let exp = self.parse_expression(variables);
                arguments.push(exp);
                if let Some(&Token::CloseParen) = self.peek() {
                    self.next();
                    break
                } else {
                    if let Some(&Token::Comma) = self.peek() {
                        self.next();
                    } else {
                        panic!("Invalid function call")
                    }
                }
            }
        }

        arguments
    }

    fn parse_arguments(&mut self) -> Result<Vec<String>, String> {
        let mut arguments = Vec::new();
        loop {
            self.match_keyword(Keyword::Int)?;
            let name = self.match_identifier()?;
            if arguments.contains(&name) {
                return Err(format!("Argument {} already defined", name))
            } else {
                arguments.push(name);
            }
            match self.peek() {
                Some(&Token::CloseParen) => break,
                _ => {
                    self.match_token(Token::Comma)?;
                }
            }
        }
        Ok(arguments)
    }

    fn parse_gen_experssion<F>(&mut self, matching: Vec<Token>, variables: &Vec<&String>, next: F) -> Expression
        where F: Fn(&mut Parser, &Vec<&String>) -> Expression {
        let mut term = next(self, variables);

        loop {
            match self.peek().map(|c| matching.contains(c)) {
                Some(true) => {
                    let op = self.next().unwrap().into();
                    let next_term = next(self, variables);
                    term = Expression::BinOp(op, Box::new(term), Box::new(next_term))
                },
                _ => break
            }
        }
        term
    }

    fn next_token(&mut self) -> Token {
        match self.next() {
            Some(token) => Ok(token),
            _ => Err("Token not found")
        }.expect("failed to parse")
    }

    fn match_token(&mut self, token: Token) -> Result<Token, String> {
        let t = self.next_token();
        match t {
            _ if t == token => Ok((t)),
            other => Err(format!("Expected {:?}, found {:?}", token, other))
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> Result<(), String> {
        let token = self.next_token();
        match token {
            Token::Keyword(ref k) if k == &keyword => Ok(()),
            other => Err(format!("Expected SemiColon, found {:?}", other))
        }
    }

    fn match_identifier(&mut self) -> Result<String, String> {
        match self.next_token() {
            Token::Identifier(n) => Ok((n)),
            other => Err(format!("Expected Identifier, found {:?}", other))
        }
    }
}