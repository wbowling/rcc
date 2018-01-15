use super::token::Token;

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

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    BinOp(BinOp, Box<Expression>, Box<Expression>),
    UnOp(UnOp, Box<Expression>),
    Int(u32),
    FunctionCall(String, Vec<Expression>),
    Variable(String),
    VariableRef(String),
    Assign(String, Box<Expression>),
    AssignPostfix(String, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Declare(String, Option<Expression>),
    Return(Expression),
    If(Expression, Box<Statement>, Box<Statement>),
    Exp(Expression),
    Compound(Vec<Statement>)
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnOp {
    Negation,
    BitComp,
    LogicalNeg,
}

#[derive(Debug, Eq, PartialEq)]
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
    Comma,
}

impl From<Token> for BinOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Multiplication => BinOp::Multiplication,
            Token::Division => BinOp::Division,
            Token::Modulus => BinOp::Modulus,
            Token::Addition => BinOp::Addition,
            Token::Negation => BinOp::Subtraction,
            Token::LessThan => BinOp::LessThan,
            Token::LessThanOrEqual => BinOp::LessThanOrEqual,
            Token::GreaterThan => BinOp::GreaterThan,
            Token::GreaterThanOrEqual => BinOp::GreaterThanOrEqual,
            Token::Equal => BinOp::Equal,
            Token::NotEqual => BinOp::NotEqual,
            Token::And => BinOp::And,
            Token::Or => BinOp::Or,
            Token::BitwiseLeft => BinOp::BitwiseLeft,
            Token::BitwiseRight => BinOp::BitwiseRight,
            Token::BitwiseAnd => BinOp::BitwiseAnd,
            Token::BitwiseXor => BinOp::BitwiseXor,
            Token::BitwiseOr => BinOp::BitwiseOr,
            Token::Comma => BinOp::Comma,
            other => panic!("Token {:?} cannot be converted into a BinOp", other)
        }

    }
}

impl From<Token> for UnOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Negation => UnOp::Negation,
            Token::LogicalNeg => UnOp::LogicalNeg,
            Token::BitComp => UnOp::BitComp,
            other => panic!("Unsupported token {:?}, can only use: ! ~ -", other)
        }
    }
}
