extern crate wasm_bindgen;

use wasm_bindgen::prelude::*;

mod lexer;
mod parser;
mod runtime;


use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;
mod ast;
mod console;
mod math;

#[wasm_bindgen]
pub fn run_js(source_code: String) -> String {
    console::reset_output();

    let mut lexer = Lexer::new(source_code);
    let tokens = lexer.tokenize();

    let ast = Parser::new(tokens.clone()).parse();

    let mut runtime = Runtime::new(ast);
    runtime.run();

    return console::get_output()
}
