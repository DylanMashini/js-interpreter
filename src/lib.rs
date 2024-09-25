#![allow(unused_imports, dead_code)]
#![feature(test)]
extern crate test;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
extern crate wasm_bindgen;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use wasm_bindgen::prelude::*;

mod lexer;
mod parser;
mod runtime;
mod string;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::runtime::Runtime;
mod array;
mod ast;
mod console;
mod math;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
#[wasm_bindgen]
pub fn run_js(source_code: String) -> String {
    console::reset_output();

    let mut lexer = Lexer::new(source_code);
    let tokens = lexer.tokenize();

    let ast = Parser::new(tokens.clone()).parse();

    let mut runtime = Runtime::new(ast);
    runtime.run();

    return console::get_output();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_dir;
    use test::Bencher;

    fn run_js(source_code: &String) {
        let mut lexer = Lexer::new(source_code.clone());
        let tokens = lexer.tokenize();

        let ast = Parser::new(tokens).parse();

        let mut runtime = Runtime::new(ast);

        runtime.run();
    }

    #[bench]
    fn bench_for_loop(b: &mut Bencher) {
        let code = include_str!("../benchmarks/for.js").to_string();
        b.iter(|| {
            run_js(&code);
        })
    }

    #[bench]
    fn bench_fib_series(b: &mut Bencher) {
        let code = include_str!("../benchmarks/fibonacci.js").to_string();
        b.iter(|| {
            run_js(&code);
        })
    }

    #[bench]
    fn bench_arr_push(b: &mut Bencher) {
        let code = include_str!("../benchmarks/arrPush.js").to_string();
        b.iter(|| {
            run_js(&code);
        })
    }

    #[bench]
    fn bench_arr_add(b: &mut Bencher) {
        let code = include_str!("../benchmarks/arrAdd.js").to_string();
        b.iter(|| {
            run_js(&code);
        })
    }
    #[test]
    fn comments_test() {
        let code = "//Test Comment\nconsole.log(\"Hello World\")".to_string();
        run_js(&code);
    }
}
