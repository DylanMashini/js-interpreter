#![allow(dead_code)]

#[derive(Debug)]
pub enum Node {
    Literal(Literal),
    Identifier(String),
    Expression(Expression),
    Statement(Statement),
    Program(Vec<Statement>),
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
}

#[derive(Debug)]
pub enum Expression {
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    CallExpr(CallExpr),
    LiteralExpr(Literal),
    Identifier(String)
}

#[derive(Debug)]
pub struct CallExpr {
    callee: Box<Node>,
    arguments: Vec<Node>,
}

#[derive(Debug)]
pub struct BinaryExpr {
    left: Box<Expression>,
    operator: BinOp,
    right: Box<Expression>,
}

impl BinaryExpr {
    pub fn new(left: Box<Expression>, operator: BinOp, right: Box<Expression>) -> BinaryExpr {
        BinaryExpr {
            left,
            operator,
            right,
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    EqualTo,
    NotEqual,
}

#[derive(Debug)]
pub struct UnaryExpr {
    operator: UnOp,
    operand: Box<Expression>,
}

impl UnaryExpr {
    pub fn new(operator: UnOp, operand: Box<Expression>) -> UnaryExpr {
        UnaryExpr {
            operator,
            operand,
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    Negate,
    Not,
}

#[derive(Debug)]
pub enum Statement {
    ExpressionStmt(Expression),
    BlockStmt(Vec<Statement>),
    IfStmt(IfStmt),
    VariableStmt(VariableDecleration),
}


#[derive(Debug)]
pub struct VariableDecleration {
    id: String,
    // Boxed to prevent recursive types
    init: Option<Box<Expression>>,
}

impl VariableDecleration {
    pub fn new(id: String, init: Option<Box<Expression>>) -> Self {
        Self { id, init }
    }
}

#[derive(Debug)]
pub struct IfStmt {
    condition: Box<Expression>,
    consequent: Box<Statement>,
    alternate: Option<Box<Statement>>,
}

impl IfStmt {
    pub fn new(condition: Box<Expression>, consequent: Box<Statement>, alternate: Option<Box<Statement>>) -> IfStmt {
        IfStmt {
            condition,
            consequent,
            alternate
        }
    }
}

#[derive(Debug)]
pub struct Program {
    body: Vec<Statement>,
}

impl Program {
    pub fn new(body: Vec<Statement>) -> Self {
        Self { body }
    }
}
