use std::fs;
mod lexer;
use lexer::Lexer;
mod ast;
mod parser;

fn main() {
    let contents = fs::read_to_string("test.js").expect("Should be able to read test file");
    println!("Source Code: \n\n{}\n", contents);
    let mut lexer = Lexer::new(contents);
    let tokens = lexer.tokenize();
    println!("Tokens:\n{:?}", tokens);
    println!("\nAST:\n{:?}",parser::Parser::new(tokens.clone()).parse());
}

