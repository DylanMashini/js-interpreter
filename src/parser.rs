#![allow(dead_code)]

use crate::ast::{BinOp, BinaryExpr, Expression, IfStmt, Literal, Program, Statement, UnOp, UnaryExpr, VariableDecleration};
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.finished() {
            if let Some(stmt) = self.statement() {
                statements.push(stmt);
            }
        }
        Program::new(statements)
    }


    pub fn statement(&mut self) -> Option<Statement> {
        match self.peek() {
            Token::If => Some(self.if_statement()),
            Token::LCurlyBracket => Some(self.block_statement()),
            Token::Let => Some(self.variable_declaration()),
            Token::EOL => {
                self.consume(Token::EOL, "Token should be EOL");
                None
            },
            _ => Some(Statement::ExpressionStmt(self.expression())),
        }
    }

    pub fn if_statement(&mut self) -> Statement {
        self.consume(Token::If, "If Statement Should Start with If");
        self.consume(Token::LParentheses, "Expected LParentheses Before If Statement Condition");
        let condition = Box::new(self.expression());
        self.consume(Token::RParentheses, "Expected Closing Parentheses After If Statement Condition");

        let consequent = Box::new(self.statement().expect("Must be statement after 'if'"));

        let alternate = if self.match_token_consume(Token::Else).is_some() {
            Some(Box::new(self.statement().expect("Must be statement after 'else'")))
        } else {
            None
        };


        Statement::IfStmt(IfStmt::new(condition, consequent, alternate))
    }

    pub fn block_statement(&mut self) -> Statement {
        let mut statements: Vec<Statement> = Vec::new();
        self.consume(Token::LCurlyBracket, "LCurlyBracket should open block statement");

        while self.peek().clone() != Token::RCurlyBracket {
            if let Some(stmt) = self.statement() {
                statements.push(stmt);
            }
        }

        self.consume(Token::RCurlyBracket, "Expected RCurlyBracket to close block statement");

        Statement::BlockStmt(statements)
    }

    pub fn variable_declaration(&mut self) -> Statement {
        self.consume(Token::Let, "expected 'let'");
        if let Token::Identifier(name) = self.consume_identifier("Expect variable name.") {
            let init = if self.match_token_consume(Token::Equal).is_some() {
                Some(Box::new(self.expression()))
            } else {
                None
            };
            if self.match_token_consume(Token::EOL).is_some() || self.match_token_consume(Token::Semicolon).is_some() {
                // Handle both a semicolon and newline
                self.match_token_consume(Token::EOL);
                Statement::VariableStmt(VariableDecleration::new(name, init))
            } else {
                panic!("Parse Error at position {}\n Expected newline or semicolon after variable decleration. ", self.position);
            }
            
        } else {
            panic!("Parser error: Expected identifier after 'let'.");
        }
    }

    // Checks to see if token parameter matches current token, and if we are finished
    pub fn check(&self, token: Token) -> bool {
        if self.finished() {
            return false;
        } else {
            return *self.peek() == token;
        }
    }

    pub fn advance(&mut self) -> Token {
        if !self.finished() {
            self.position += 1;
        }
        self.previous()
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.position]
    }

    // Returns previous token
    fn previous(&self) -> Token {
        self.tokens[self.position - 1].clone()
    }

    pub fn consume(&mut self, token: Token, error_message: &str) -> Token {
        if self.check(token) {
            self.advance()
        } else {
            panic!("Error at {}: {}", self.position, error_message);
        }
    }

    fn match_token_consume(&mut self, token: Token) -> Option<Token> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    pub fn consume_identifier(&mut self, error_message: &str) -> Token {
        if matches!(self.peek(), Token::Identifier(_)) {
            self.advance()
        } else {
            panic!("Error at {}: {}", self.position, error_message);
        }
    }
    pub fn finished(&self) -> bool {
        self.position >= self.tokens.len()
    }
}

// Expression Parsing
impl Parser {
    fn expression(&mut self) -> Expression {
        self.equality()
    }

    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();
        while let Some(operator) = self.current_operator() {
            let _operator = match operator {
                Token::Equal => BinOp::EqualTo,
                Token::NotEqual => BinOp::NotEqual,
                _ => break,
            };
            self.advance(); // Consume token if it was an operator (Either == or != for now)
            let right = self.comparison();
            expr = Expression::BinaryExpr(BinaryExpr::new(
                Box::new(expr),
                BinOp::EqualTo,
                Box::new(right),
            ));
        }
        expr
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.addition();
        loop {
            let operator = if self.match_token_consume(Token::GreaterThan).is_some() {
                Some(BinOp::GreaterThan)
            } else if self.match_token_consume(Token::LessThan).is_some() {
                Some(BinOp::LessThan)
            } else if self.match_token_consume(Token::GreaterThanEqualTo).is_some() {
                Some(BinOp::GreaterThanEqual)
            } else if self.match_token_consume(Token::LessThanEqualTo).is_some() {
                Some(BinOp::LessThanEqual)
            } else {
                None
            };

            if let Some(op) = operator {
                let right = self.addition();
                expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), op, Box::new(right)));
            } else {
                break
            }
        }

        expr
    }

    fn addition(&mut self) -> Expression {
        let mut expr = self.multiplication();
        loop {
            let operator = if self.match_token_consume(Token::Plus).is_some() {
                Some(BinOp::Add)
            } else if self.match_token_consume(Token::Minus).is_some() {
                Some(BinOp::Subtract)
            } else {
                None
            };


            if let Some(op) = operator {
                let right = self.multiplication();
                expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), op, Box::new(right)));
            } else {
                break
            }
            
        }

        expr
    }

    fn multiplication(&mut self) -> Expression {
        let mut expr = self.unary();
        
        loop {
            let operator = if self.match_token_consume(Token::Multiply).is_some() {
                Some(BinOp::Multiply)
            } else if self.match_token_consume(Token::Divide).is_some() {
                Some(BinOp::Divide)
            } else {
                None
            };

            if let Some(op) = operator {
                let right = self.unary();
                expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), op, Box::new(right)));
            } else {
                break
            }
        }

        expr
    }

    fn unary(&mut self) -> Expression {

        if let Some(_) = self.match_token_consume(Token::Minus) {
            let operand = self.unary();
            Expression::UnaryExpr(UnaryExpr::new(UnOp::Negate, Box::new(operand)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expression {
        match self.peek().clone() {
            Token::Number(value) => {
                self.advance();  // Consume the number
                Expression::LiteralExpr(Literal::Number(value))
            },
            Token::String(value) => {
                self.advance();  // Consume the string
                Expression::LiteralExpr(Literal::String(value.clone()))
            },
            Token::Boolean(value) => {
                self.advance();  // Consume the boolean
                Expression::LiteralExpr(Literal::Boolean(value))
            },
            Token::Null => {
                self.advance();  // Consume 'null'
                Expression::LiteralExpr(Literal::Null)
            },
            Token::Undefined => {
                self.advance();  // Consume 'undefined'
                Expression::LiteralExpr(Literal::Null)  // Assuming 'undefined' is treated as 'null'
            },
            Token::Identifier(name) => {
                self.advance();  // Consume the identifier
                Expression::Identifier(name.clone())
            },
            Token::LParentheses => {
                self.advance();  // Consume '('
                let expr = self.expression();  // Parse the expression inside the parentheses
                self.consume(Token::RParentheses, "Expect ')' after expression.");  // Ensure closing ')'
                expr  // The expression is the result
            },
            _ => panic!("Unexpected token: {:?}", self.peek()),  // Error handling for unexpected tokens
        }
    }

    fn current_operator(&self) -> Option<Token> {
        match self.peek() {
            Token::Equal | Token::NotEqual => Some(self.peek().clone()),
            _ => None,
        }
    }
}
