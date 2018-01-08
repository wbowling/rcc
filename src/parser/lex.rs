use super::token::*;

pub fn lex(contents: String) -> Vec<Token> {
    let mut tokens = TokenParser::new(&contents);

    loop {
        match tokens.peek() {
            Some(&c) => {
                match c {
                    '{' => tokens.push(Token::OpenBrace),
                    '}' => tokens.push(Token::CloseBrace),
                    '(' => tokens.push(Token::OpenParen),
                    ')' => tokens.push(Token::CloseParen),
                    ';' => tokens.push(Token::SemiColon),
                    ' ' | '\t' | '\n' | '\r' => tokens.drop(),
                    'a'...'z' | 'A'...'Z' => {
                        let word: &str = &tokens.get_string(|x| x.is_ascii() && x.is_alphanumeric());
                        match word {
                            "int" =>  tokens.push_back(Token::Keyword(Keyword::Int)),
                            "return" => tokens.push_back(Token::Keyword(Keyword::Return)),
                            _ => tokens.push_back(Token::Identifier(word.to_string()))
                        }
                    },
                    '0'...'9' => {
                        let word = tokens.get_string(|x| x.is_ascii() && (x.is_digit(16) || x == &'x'));
                        let int: u32 = if word.starts_with("0x") {
                            u32::from_str_radix(&word[2..], 16).expect("Not a number")
                        } else {
                            word.parse().expect("Not a number")
                        };
                        tokens.push_back(Token::Literal(int))
                    },
                    '-' => tokens.push(Token::Negation),
                    '~' => tokens.push(Token::BitComp),
                    '+' => tokens.push(Token::Addition),
                    '*' => tokens.push(Token::Multiplication),
                    '/' => tokens.push(Token::Division),
                    '%' => tokens.push(Token::Modulus),
                    '^' => tokens.push(Token::BitwiseXor),
                    ',' => tokens.push(Token::Comma),
                    multi => {
                        match (tokens.next().unwrap(), tokens.peek()) {
                            ('&', Some(&'&')) => tokens.push(Token::And),
                            ('|', Some(&'|')) => tokens.push(Token::Or),
                            ('<', Some(&'=')) => tokens.push(Token::LessThanOrEqual),
                            ('>', Some(&'=')) => tokens.push(Token::GreaterThanOrEqual),
                            ('=', Some(&'=')) => tokens.push(Token::Equal),
                            ('!', Some(&'=')) => tokens.push(Token::NotEqual),
                            ('<', Some(&'<')) => tokens.push(Token::BitwiseLeft),
                            ('>', Some(&'>')) => tokens.push(Token::BitwiseRight),
                            ('<', _) => tokens.push_back(Token::LessThan),
                            ('>', _) => tokens.push_back(Token::GreaterThan),
                            ('!', _) => tokens.push_back(Token::LogicalNeg),
                            ('&', _) => tokens.push_back(Token::BitwiseAnd),
                            ('|', _) => tokens.push_back(Token::BitwiseOr),
                            ('=', _) => tokens.push_back(Token::Assign),
                            _ => panic!("Unknown token {:?}", multi),
                        }
                    }
                };
            },
            None => break
        }
    }
    tokens.tokens
}
