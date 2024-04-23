use crate::ast::{
    BinOp, BinaryExpr, CallExpr, Expression, ForStmt, FuncDecleration, IfStmt, Literal, Program,
    Statement, StatementValue, UnOp, UnaryExpr, VariableDecleration, WhileStmt,
};

use crate::lexer::{Token, TokenValue};

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
        let line = self.peek().line;
        let value = match self.peek().value {
            TokenValue::If => Some(self.if_statement()),
            TokenValue::LCurlyBracket => Some(self.block_statement()),
            TokenValue::Let => Some(self.variable_declaration()),
            TokenValue::While => Some(self.while_loop_statement()),
            TokenValue::For => Some(self.for_loop_statement()),
            TokenValue::Function => Some(self.function_decleration()),
            TokenValue::Return => Some(self.return_statement()),
            TokenValue::EOL => {
                self.consume(TokenValue::EOL, "Token should be EOL");
                None
            }
            TokenValue::Semicolon => {
                self.consume(TokenValue::Semicolon, "Token should be semicolon");
                None
            }
            _ => Some(StatementValue::ExpressionStmt(self.expression())),
        };

        match value {
            Some(val) => Some(Statement::new(val, line)),
            None => None,
        }
    }

    pub fn if_statement(&mut self) -> StatementValue {
        self.consume(TokenValue::If, "If Statement Should Start with If");
        self.consume(
            TokenValue::LParentheses,
            "Expected LParentheses Before If Statement Condition",
        );
        let condition = Box::new(self.expression());

        self.consume(
            TokenValue::RParentheses,
            "Expected Closing Parentheses After If Statement Condition",
        );

        let consequent = Box::new(self.statement().expect("Must be statement after 'if'"));

        let alternate = if self.match_token_consume(TokenValue::Else).is_some() {
            Some(Box::new(
                self.statement().expect("Must be statement after 'else'"),
            ))
        } else {
            None
        };

        StatementValue::IfStmt(IfStmt::new(condition, consequent, alternate))
    }

    pub fn block_statement(&mut self) -> StatementValue {
        let mut statements: Vec<Statement> = Vec::new();
        self.consume(
            TokenValue::LCurlyBracket,
            "LCurlyBracket should open block statement",
        );

        while self.peek().value != TokenValue::RCurlyBracket {
            if let Some(stmt) = self.statement() {
                statements.push(stmt);
            }
        }

        self.consume(
            TokenValue::RCurlyBracket,
            "Expected RCurlyBracket to close block statement",
        );

        StatementValue::BlockStmt(statements)
    }

    pub fn variable_declaration(&mut self) -> StatementValue {
        self.consume(TokenValue::Let, "expected 'let'");
        if let TokenValue::Identifier(name) = self.consume_identifier("Expect variable name.").value
        {
            let init = if self.match_token_consume(TokenValue::Equal).is_some() {
                Some(Box::new(self.expression()))
            } else {
                None
            };
            if self.match_token_consume(TokenValue::EOL).is_some()
                || self.match_token_consume(TokenValue::Semicolon).is_some()
            {
                // Handle both a semicolon and newline
                self.match_token_consume(TokenValue::EOL);
                StatementValue::VariableStmt(VariableDecleration::new(name, init))
            } else {
                panic!("Parse Error at position {}\n Expected newline or semicolon after variable decleration. ", self.position);
            }
        } else {
            panic!("Parse error: Expected identifier after 'let'.");
        }
    }

    fn while_loop_statement(&mut self) -> StatementValue {
        self.consume(TokenValue::While, "While loop should start with while");
        self.consume(
            TokenValue::LParentheses,
            "While Condition requires Parentheses",
        );

        let condition = self.expression();

        self.consume(
            TokenValue::RParentheses,
            "Closing parenthases is required after condition for while loop",
        );

        let body = Box::new(
            self.statement()
                .expect("Body for while loop can not be empty"),
        );

        StatementValue::WhileStmt(WhileStmt::new(condition, body))
    }

    fn for_loop_statement(&mut self) -> StatementValue {
        self.consume(TokenValue::For, "For loop must start with for");
        self.consume(
            TokenValue::LParentheses,
            "For statement must start with LParentheses",
        );

        let init = Box::new(self.statement().expect("Condition is requried"));

        let condition = self.expression();

        self.consume(TokenValue::Semicolon, "Expect semicolon after condition");

        let afterthought = Box::new(
            self.statement()
                .expect("Afterthought is required in this implementation of javascript"),
        );

        self.consume(
            TokenValue::RParentheses,
            "Right parentheses is required after for loop afterthought",
        );

        let body = Box::new(
            self.statement()
                .expect("Statement is required for for loop"),
        );

        StatementValue::ForStmt(ForStmt::new(init, condition, afterthought, body))
    }

    fn function_decleration(&mut self) -> StatementValue {
        self.consume(
            TokenValue::Function,
            "Functions should start with function keyword",
        );

        let id = match &self.peek().value {
            TokenValue::Identifier(id) => id.clone(),
            _ => panic!("Functions must be named"),
        };
        // Advacne past function name
        self.advance();

        self.consume(
            TokenValue::LParentheses,
            "Function must have arguments in parentheses",
        );

        let mut arguments: Vec<String> = Vec::new();
        loop {
            let arg_name = match &self.peek().value {
                TokenValue::Identifier(id) => id.clone(),
                _ => break,
            };
            // Advance Identifier token with name of argument
            self.advance();

            arguments.push(arg_name);

            if let TokenValue::Comma = self.peek().value {
                self.consume(TokenValue::Comma, "Token was expected to be comma");
            } else {
                break;
            }
        }

        self.consume(
            TokenValue::RParentheses,
            "Expected Closing Parentheses after arguments",
        );

        // Function must be a block statement
        let body = Box::new(self.block_statement());

        StatementValue::FunctionDecleration(FuncDecleration::new(id, arguments, body))
    }

    fn return_statement(&mut self) -> StatementValue {
        self.consume(
            TokenValue::Return,
            "Return statement should start with 'return'",
        );

        StatementValue::ReturnStatement(self.expression())
    }

    // Checks to see if token parameter matches current token, and if we are finished
    pub fn check(&self, token: TokenValue) -> bool {
        if self.finished() {
            return false;
        } else {
            return self.peek().value == token;
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

    pub fn consume(&mut self, token: TokenValue, error_message: &str) -> Token {
        if self.check(token) {
            self.advance()
        } else {
            panic!("Error at {}: {}", self.position, error_message);
        }
    }

    fn match_token_consume(&mut self, token: TokenValue) -> Option<Token> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    pub fn consume_identifier(&mut self, error_message: &str) -> Token {
        if matches!(self.peek().value, TokenValue::Identifier(_)) {
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
        self.assignment()
    }

    fn assignment(&mut self) -> Expression {
        let mut expr = self.plus_equal();

        if let Some(_) = self.match_token_consume(TokenValue::Equal) {
            let right_side = self.logical_or();

            expr = Expression::BinaryExpr(BinaryExpr::new(
                Box::new(expr),
                BinOp::Assign,
                Box::new(right_side),
            ));
        }

        expr
    }
    // Also handles minus_equal
    fn plus_equal(&mut self) -> Expression {
        let mut expr = self.logical_or();

        let operator = match self.peek().value {
            TokenValue::PlusEqual => Some(BinOp::Add),
            TokenValue::MinusEqual => Some(BinOp::Subtract),
            _ => None,
        };

        if let Some(op) = operator {
            self.advance();
            expr = Expression::BinaryExpr(BinaryExpr::new(
                Box::new(expr.clone()),
                BinOp::Assign,
                Box::new(Expression::BinaryExpr(BinaryExpr::new(
                    Box::new(expr),
                    op,
                    Box::new(self.logical_or()),
                ))),
            ));
        };

        expr
    }

    fn logical_or(&mut self) -> Expression {
        let mut expr = self.logical_and();
        while let Some(_) = self.match_token_consume(TokenValue::Or) {
            let right = Box::new(self.logical_and());
            expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), BinOp::LogicalOr, right));
        }

        expr
    }

    fn logical_and(&mut self) -> Expression {
        let mut expr = self.equality();

        while let Some(_) = self.match_token_consume(TokenValue::And) {
            let right = Box::new(self.equality());
            expr =
                Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), BinOp::LogicalAnd, right));
        }
        expr
    }

    fn equality(&mut self) -> Expression {
        let mut expr = self.comparison();
        while let Some(operator) = self.current_operator() {
            let operator = match operator.value {
                TokenValue::DoubleEqual => BinOp::EqualTo,
                TokenValue::NotEqual => BinOp::NotEqual,
                _ => break,
            };
            self.advance(); // Consume token if it was an operator (Either == or != for now)
            let right = self.comparison();
            expr =
                Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), operator, Box::new(right)));
        }
        expr
    }

    fn comparison(&mut self) -> Expression {
        let mut expr = self.addition();
        loop {
            let operator = if self.match_token_consume(TokenValue::GreaterThan).is_some() {
                Some(BinOp::GreaterThan)
            } else if self.match_token_consume(TokenValue::LessThan).is_some() {
                Some(BinOp::LessThan)
            } else if self
                .match_token_consume(TokenValue::GreaterThanEqualTo)
                .is_some()
            {
                Some(BinOp::GreaterThanEqual)
            } else if self
                .match_token_consume(TokenValue::LessThanEqualTo)
                .is_some()
            {
                Some(BinOp::LessThanEqual)
            } else {
                None
            };

            if let Some(op) = operator {
                let right = self.addition();
                expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), op, Box::new(right)));
            } else {
                break;
            }
        }

        expr
    }

    fn addition(&mut self) -> Expression {
        let mut expr = self.multiplication();
        loop {
            let operator = if self.match_token_consume(TokenValue::Plus).is_some() {
                Some(BinOp::Add)
            } else if self.match_token_consume(TokenValue::Minus).is_some() {
                Some(BinOp::Subtract)
            } else {
                None
            };

            if let Some(op) = operator {
                let right = self.multiplication();
                expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), op, Box::new(right)));
            } else {
                break;
            }
        }

        expr
    }

    fn multiplication(&mut self) -> Expression {
        let mut expr = self.modulo();

        loop {
            let operator = if self.match_token_consume(TokenValue::Multiply).is_some() {
                Some(BinOp::Multiply)
            } else if self.match_token_consume(TokenValue::Divide).is_some() {
                Some(BinOp::Divide)
            } else {
                None
            };

            if let Some(op) = operator {
                let right = self.modulo();
                expr = Expression::BinaryExpr(BinaryExpr::new(Box::new(expr), op, Box::new(right)));
            } else {
                break;
            }
        }

        expr
    }

    fn modulo(&mut self) -> Expression {
        let mut expr = self.increment();

        loop {
            if self.match_token_consume(TokenValue::Modulo).is_some() {
                let right = self.increment();
                expr = Expression::BinaryExpr(BinaryExpr::new(
                    Box::new(expr),
                    BinOp::Modulo,
                    Box::new(right),
                ));
            } else {
                break;
            }
        }

        expr
    }

    // Also handles Decrements
    fn increment(&mut self) -> Expression {
        let expr = self.unary();

        match expr {
            Expression::Identifier(id) => {
                if self.match_token_consume(TokenValue::DoublePlus).is_some() {
                    Expression::Increment(id)
                } else if self.match_token_consume(TokenValue::DoubleMinus).is_some() {
                    Expression::Decrement(id)
                } else {
                    Expression::Identifier(id)
                }
            }
            _ => expr,
        }
    }

    fn unary(&mut self) -> Expression {
        if let Some(_) = self.match_token_consume(TokenValue::Minus) {
            let operand = self.unary();
            Expression::UnaryExpr(UnaryExpr::new(UnOp::Negate, Box::new(operand)))
        } else if let Some(_) = self.match_token_consume(TokenValue::Not) {
            let operand = self.unary();
            Expression::UnaryExpr(UnaryExpr::new(UnOp::Not, Box::new(operand)))
        } else {
            self.call_expression()
        }
    }

    fn call_expression(&mut self) -> Expression {
        let expr = self.primary();

        match expr {
            Expression::Identifier(id) => {
                if self.match_token_consume(TokenValue::LParentheses).is_some() {
                    // We are calling a function
                    let mut parameters: Vec<Expression> = Vec::new();
                    loop {
                        if self.peek().value == TokenValue::RParentheses {
                            break;
                        };
                        parameters.push(self.expression());
                        if self.match_token_consume(TokenValue::Comma).is_none() {
                            break;
                        }
                    }
                    self.consume(
                        TokenValue::RParentheses,
                        "Closing Parentheses required to end parameters in function call",
                    );
                    Expression::CallExpr(CallExpr::new(id, parameters))
                } else {
                    Expression::Identifier(id) // Expr is moved, so I have to manually copy it
                }
            }
            _ => expr,
        }
    }

    fn primary(&mut self) -> Expression {
        match self.peek().clone().value {
            TokenValue::Number(value) => {
                self.advance(); // Consume the number
                Expression::LiteralExpr(Literal::Number(value))
            }
            TokenValue::String(value) => {
                self.advance(); // Consume the string
                Expression::LiteralExpr(Literal::String(value.clone()))
            }
            TokenValue::Boolean(value) => {
                self.advance(); // Consume the boolean
                Expression::LiteralExpr(Literal::Boolean(value))
            }
            TokenValue::Null => {
                self.advance(); // Consume 'null'
                Expression::LiteralExpr(Literal::Null)
            }
            TokenValue::Undefined => {
                self.advance(); // Consume 'undefined'
                Expression::LiteralExpr(Literal::Null) // Assuming 'undefined' is treated as 'null'
            }
            TokenValue::Identifier(name) => {
                self.advance(); // Consume the identifier
                Expression::Identifier(name.clone())
            }
            TokenValue::LParentheses => {
                self.advance(); // Consume '('
                let expr = self.expression(); // Parse the expression inside the parentheses
                self.consume(TokenValue::RParentheses, "Expect ')' after expression."); // Ensure closing ')'
                expr // The expression is the result
            }
            _ => panic!("Unexpected token: {:?}", self.peek()), // Error handling for unexpected tokens
        }
    }

    fn current_operator(&self) -> Option<Token> {
        match self.peek().value {
            TokenValue::DoubleEqual | TokenValue::NotEqual => Some(self.peek().clone()),
            _ => None,
        }
    }
}
