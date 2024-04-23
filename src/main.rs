use std::fs;
mod lexer;
use lexer::Lexer;
mod ast;
mod parser;
use parser::Parser;
mod runtime;
use runtime::Runtime;
use std::env;


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_name = match args.get(1) {
        Some(arg) => arg,
        None => panic!("Please provide a file to run via the command line")
    };

    println!("{}", file_name);
    let contents = fs::read_to_string(file_name).expect("Should be able to read test file");
    // println!("Source Code: \n\n{}\n", contents);
    let mut lexer = Lexer::new(contents);
    let tokens = lexer.tokenize();
    // println!("Tokens:\n{:?}", tokens);

    let ast = Parser::new(tokens.clone()).parse();
    
    println!("\nAST:\n{:?}",parser::Parser::new(tokens.clone()).parse());

    let mut runtime = Runtime::new(ast);
    runtime.run();

}