#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    CallExpr(CallExpr),
    LiteralExpr(Literal),
    Identifier(String),
    Increment(String), // String is identifier
    Decrement(String),
    DotExpr(DotExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: String,
    pub arguments: Vec<Expression>,
}

impl CallExpr {
    pub fn new(callee: String, arguments: Vec<Expression>) -> CallExpr {
        CallExpr { callee, arguments }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expression>,
    pub operator: BinOp,
    pub right: Box<Expression>,
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

#[derive(Debug, Clone, PartialEq)]
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
    LogicalOr,
    LogicalAnd,
    Assign,
    Modulo,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnOp,
    pub operand: Box<Expression>,
}

impl UnaryExpr {
    pub fn new(operator: UnOp, operand: Box<Expression>) -> UnaryExpr {
        UnaryExpr { operator, operand }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DotExpr {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
}

impl DotExpr {
    pub fn new(object: Box<Expression>, property: Box<Expression>) -> DotExpr {
        DotExpr { object, property }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub value: StatementValue,
    pub line: usize,
}

impl Statement {
    pub fn new(value: StatementValue, line: usize) -> Statement {
        Statement { value, line }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementValue {
    ExpressionStmt(Expression),
    BlockStmt(Vec<Statement>),
    IfStmt(IfStmt),
    VariableStmt(VariableDecleration),
    WhileStmt(WhileStmt),
    ForStmt(ForStmt),
    FunctionDecleration(FuncDecleration),
    ReturnStatement(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecleration {
    pub id: String,
    // Boxed to prevent recursive types
    pub init: Option<Box<Expression>>,
}

impl VariableDecleration {
    pub fn new(id: String, init: Option<Box<Expression>>) -> Self {
        Self { id, init }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Box<Expression>,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

impl IfStmt {
    pub fn new(
        condition: Box<Expression>,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>,
    ) -> IfStmt {
        IfStmt {
            condition,
            consequent,
            alternate,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expression,
    pub body: Box<Statement>,
}

impl WhileStmt {
    pub fn new(condition: Expression, body: Box<Statement>) -> WhileStmt {
        WhileStmt { condition, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub initialization: Box<Statement>,
    pub condition: Expression,
    pub afterthought: Box<Statement>,
    pub body: Box<Statement>,
}

impl ForStmt {
    pub fn new(
        initialization: Box<Statement>,
        condition: Expression,
        afterthought: Box<Statement>,
        body: Box<Statement>,
    ) -> ForStmt {
        ForStmt {
            initialization,
            condition,
            afterthought,
            body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecleration {
    pub id: String,
    pub parameters: Vec<String>,
    pub body: Box<StatementValue>,
}

impl FuncDecleration {
    pub fn new(id: String, parameters: Vec<String>, body: Box<StatementValue>) -> FuncDecleration {
        FuncDecleration {
            id,
            parameters,
            body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>,
}

impl Program {
    pub fn new(body: Vec<Statement>) -> Self {
        Self { body }
    }
}
