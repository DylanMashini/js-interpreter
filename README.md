# JS Interpreter

A  simple JS interpreter written in Rust. Right now, some things don't behave the same way they do in ECMA-compliant runtimes like V8, but most JS syntax is supported. This is still unstable, and not suitable for usage in production, but there are applications for embedded computing in the future. 

## Project Structure
- ``interpreter/``
  The Rust project for the interpreter. There are a few key files. I'll list them in the order that they run within the interpreter.
  
  ``lexer.rs`` is the first step. The Lexer struct takes in a String for the source code, and outputs Vec<Token>. The Token struct and TokenValue enum are also defined in this file.

  ``ast.rs`` defines the structs and enums used to represent the syntax tree. Most notably, there is the StatementValue enum and the Expression enum.  This file doesn't have any structs or functions that would be used by someone trying to use the interpreter but rather defines the building blocks that the other files in the project use. 

  ``parser.rs`` contains the Parser struct, which takes in a Vec<Token>, and parses it into a Program. The Program struct is defined in ast.rs, and contains a body of type Vec<Statement>. Essentially, the parser takes in all the tokens and analyses the relationships between tokens to create a Program. 

  ``runtime.rs`` contains the Runtime struct, which evaluates a Program. The Runtime steps through the AST constructed in the parser and evaluates nodes recursively. This is by far the longest file, and if I restructure the project it will be split up somehow.

  ``string.rs``, ``console.rs``, ``array.rs``, and ``math.rs`` define behavior for features inside of the runtime. They all work by exporting a function that returns a prototype for the specific feature. This pattern was the best way for me to write primitive features into the interpreter. 

- ``frontend/``
  A Svelte frontend that utilizes the interpreter compiled into WASM. This is just a quick way to try out the interpreter in a browser. 
