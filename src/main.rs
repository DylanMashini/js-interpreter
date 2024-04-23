use std::fs;
mod lexer;
use lexer::Lexer;
mod ast;
mod parser;
use parser::Parser;
mod runtime;
use runtime::Runtime;

fn main() {
    let contents = fs::read_to_string("test.js").expect("Should be able to read test file");
    // println!("Source Code: \n\n{}\n", contents);
    let mut lexer = Lexer::new(contents);
    let tokens = lexer.tokenize();
    // println!("Tokens:\n{:?}", tokens);

    let ast = Parser::new(tokens.clone()).parse();
    
    println!("\nAST:\n{:?}",parser::Parser::new(tokens.clone()).parse());

    let mut runtime = Runtime::new(ast);
    runtime.run();

}