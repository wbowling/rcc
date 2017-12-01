use std::fs::File;
use std::io::prelude::*;


mod parser;

fn main() {
    let contents = read_file("testcases/return_2.c");
    let tokens = parser::token::lex(contents);
    let prog = parser::ast::parse_program(&mut tokens.into_iter());
    println!("{:?}", prog);
}

fn read_file(path: &str) -> String {
    let mut file = File::open(path).expect("Failed to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    contents
}

