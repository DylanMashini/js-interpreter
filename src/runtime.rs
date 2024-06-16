use crate::ast::{
    BinOp, BinaryExpr, CallExpr, DotExpr, Expression, ForStmt, FuncDecleration, IfStmt, Literal,
    Program, Statement, StatementValue, UnOp, UnaryExpr, VariableDecleration, WhileStmt,
};

use crate::console::log;
use crate::math::get_math_module;

use core::fmt;
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

type RustFunc = fn(Vec<Value>) -> Value;

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionBody {
    RustFunc(RustFunc),
    JSFunc(Statement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub arguments: Vec<String>,
    pub body: FunctionBody,
}

impl Function {
    pub fn new(arguments: Vec<String>, body: Statement) -> Function {
        Function {
            arguments,
            body: FunctionBody::JSFunc(body),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Object {
    pub fields: HashMap<String, Value>,
}

impl Object {
    pub fn new(fields: HashMap<String, Value>) -> Object {
        Object { fields }
    }
    pub fn get_property(&self, id: String) -> &Value {
        self.fields.get(&id).unwrap_or(&Value::Null)
    }
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Function),
    Null,
    Object(Object),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Number(val) => val.to_string(),
                Value::String(val) => val.to_string(),
                Value::Boolean(val) => val.to_string(),
                Value::Function(_) => format!("Printing Functions Not Supported"),
                Value::Null => "null".to_string(),
                _ => todo!(),
            }
        )
    }
}

impl Value {
    pub fn add(&self, other: &Value) -> Value {
        match self {
            Value::Number(n) => match other {
                Value::Number(n2) => return Value::Number(n + n2),
                Value::String(val) => return Value::String(n.to_string() + val),
                Value::Boolean(b) => return Value::Number(n + (*b as i8) as f64),
                Value::Null => return Value::Number(*n),
                Value::Function(_) => panic!("Can not add a function"),
                _ => todo!(),
            },
            Value::String(str1) => match other {
                Value::Number(n2) => return Value::String(str1.to_owned() + &n2.to_string()),
                Value::String(str2) => return Value::String(str1.to_owned() + str2),
                Value::Boolean(b2) => return Value::String(str1.to_owned() + &b2.to_string()),
                Value::Null => return Value::String(str1.to_owned() + "null"),
                Value::Function(_) => panic!("Can not add a function"),
                _ => todo!(),
            },
            Value::Boolean(b1) => match other {
                Value::Number(n2) => return Value::Number((*b1 as i8) as f64 + n2),
                Value::String(s2) => return Value::String(b1.to_string() + s2),
                Value::Boolean(b2) => return Value::Number(((*b1 as i8) + (*b2 as i8)) as f64),
                Value::Null => return Value::Boolean(*b1),
                Value::Function(_) => panic!("Can not add a function"),
                _ => todo!(),
            },
            Value::Function(_) => panic!("Can not add a function"),
            Value::Null => todo!("Im lazy"),
            _ => todo!(),
        }
    }

    pub fn subtract(&self, other: &Value) -> Value {
        match self {
            Value::Number(n) => match other {
                Value::Number(n2) => Value::Number(n - n2),
                Value::Boolean(b) => Value::Number(n - (*b as i8 as f64)),
                Value::Null => Value::Number(*n),
                _ => panic!("Subtraction with non-numeric type is not supported"),
            },
            Value::Boolean(b) => match other {
                Value::Number(n2) => Value::Number((*b as i8 as f64) - n2),
                Value::Boolean(b2) => Value::Number((*b as i8 as f64) - (*b2 as i8 as f64)),
                Value::Null => Value::Number(*b as i8 as f64),
                _ => panic!("Subtraction with non-numeric type is not supported"),
            },
            Value::Null => match other {
                Value::Number(n2) => Value::Number(0.0 - n2),
                Value::Boolean(b) => Value::Number(0.0 - (*b as i8 as f64)),
                Value::Null => Value::Number(0.0),
                _ => panic!("Subtraction with non-numeric type is not supported"),
            },
            _ => panic!("Subtraction with non-numeric type is not supported"),
        }
    }

    pub fn multiply(&self, other: &Value) -> Value {
        match self {
            Value::Number(n) => match other {
                Value::Number(n2) => Value::Number(n * n2),
                Value::Boolean(b) => Value::Number(n * (*b as i8 as f64)),
                Value::Null => Value::Number(0.0),
                _ => panic!("Multiplication with non-numeric type is not supported"),
            },
            Value::Boolean(b) => match other {
                Value::Number(n2) => Value::Number((*b as i8 as f64) * n2),
                Value::Boolean(b2) => Value::Number((*b as i8 as f64) * (*b2 as i8 as f64)),
                Value::Null => Value::Number(0.0),
                _ => panic!("Multiplication with non-numeric type is not supported"),
            },
            Value::Null => Value::Number(0.0),
            _ => panic!("Multiplication with non-numeric type is not supported"),
        }
    }

    pub fn divide(&self, other: &Value) -> Value {
        match self {
            Value::Number(n) => match other {
                Value::Number(n2) => {
                    if *n2 == 0.0 {
                        panic!("Division by zero");
                    } else {
                        Value::Number(n / n2)
                    }
                }
                Value::Boolean(b) => {
                    if *b {
                        Value::Number(*n)
                    } else {
                        panic!("Division by zero");
                    }
                }
                Value::Null => panic!("Division by zero"),
                _ => panic!("Division with non-numeric type is not supported"),
            },
            Value::Boolean(b) => match other {
                Value::Number(n2) => {
                    if *n2 == 0.0 {
                        panic!("Division by zero");
                    } else {
                        Value::Number((*b as i8 as f64) / n2)
                    }
                }
                Value::Boolean(b2) => {
                    if *b2 {
                        Value::Number(*b as i8 as f64)
                    } else {
                        panic!("Division by zero");
                    }
                }
                Value::Null => panic!("Division by zero"),
                _ => panic!("Division with non-numeric type is not supported"),
            },
            Value::Null => Value::Number(0.0),
            _ => panic!("Division with non-numeric type is not supported"),
        }
    }

    fn to_f64(&self) -> f64 {
        match self {
            Value::Number(n) => *n,
            Value::Boolean(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            Value::Null => 0.0,
            _ => panic!("Attempted to compare a non-comparable type"),
        }
    }

    pub fn less_than(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1 < s2,
            (Value::Number(_), _) | (Value::Boolean(_), _) | (Value::Null, _) => {
                self.to_f64() < other.to_f64()
            }
            _ => panic!("Comparison with incompatible or non-numeric type"),
        }
    }

    pub fn greater_than(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1 > s2,
            (Value::Number(_), _) | (Value::Boolean(_), _) | (Value::Null, _) => {
                self.to_f64() > other.to_f64()
            }
            _ => panic!("Comparison with incompatible or non-numeric type"),
        }
    }

    pub fn less_than_or_equal_to(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1 <= s2,
            (Value::Number(_), _) | (Value::Boolean(_), _) | (Value::Null, _) => {
                self.to_f64() <= other.to_f64()
            }
            _ => panic!("Comparison with incompatible or non-numeric type"),
        }
    }

    pub fn greater_than_or_equal_to(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => s1 >= s2,
            (Value::Number(_), _) | (Value::Boolean(_), _) | (Value::Null, _) => {
                self.to_f64() >= other.to_f64()
            }
            _ => panic!("Comparison with incompatible or non-numeric type"),
        }
    }

    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::Null, Value::Null) => true,
            (Value::Function(_), Value::Function(_)) => panic!("Cannot compare functions with =="),

            // Type coercion between different types
            (Value::Number(n), Value::Boolean(b)) | (Value::Boolean(b), Value::Number(n)) => {
                n == &(*b as i8 as f64)
            }
            (Value::Number(n), Value::Null) | (Value::Null, Value::Number(n)) => n == &0.0,
            (Value::Boolean(b), Value::Null) | (Value::Null, Value::Boolean(b)) => {
                (*b as i8 as f64) == 0.0
            }

            // Special cases for string conversions not handled, as JavaScript would convert
            // numbers and booleans to strings, or parse strings to numbers if possible.
            // Implementing these would need explicit parsing and conversion code.
            (Value::String(s), Value::Number(n)) | (Value::Number(n), Value::String(s)) => {
                s.parse::<f64>().ok().map_or(false, |num| &num == n)
            }
            (Value::String(s), Value::Boolean(b)) | (Value::Boolean(b), Value::String(s)) => s
                .parse::<f64>()
                .ok()
                .map_or(false, |num| num == (*b as i8 as f64)),
            _ => false, // All other combinations are considered not equal
        }
    }

    pub fn not_equals(&self, other: &Value) -> bool {
        !self.equals(other)
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Number(n) => *n != 0.0 && !n.is_nan(),
            Value::String(s) => !s.is_empty(),
            Value::Boolean(b) => *b,
            Value::Null => false,
            Value::Function(_) => true, // Functions are always truthy
            _ => todo!(),
        }
    }

    pub fn logical_and(&self, other: &Value) -> Value {
        if self.is_truthy() {
            other.clone()
        } else {
            self.clone()
        }
    }

    pub fn logical_or(&self, other: &Value) -> Value {
        if self.is_truthy() {
            self.clone()
        } else {
            other.clone()
        }
    }

    pub fn modulo(&self, other: &Value) -> Value {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => {
                if *b == 0.0 {
                    panic!("Division by zero in modulo operation");
                } else {
                    Value::Number(a % b)
                }
            }
            (Value::Number(a), Value::Boolean(b)) => {
                let b_as_f64 = if *b { 1.0 } else { 0.0 };
                if b_as_f64 == 0.0 {
                    panic!("Division by zero in modulo operation");
                } else {
                    Value::Number(a % b_as_f64)
                }
            }
            (Value::Boolean(a), Value::Number(b)) => {
                let a_as_f64 = if *a { 1.0 } else { 0.0 };
                if *b == 0.0 {
                    panic!("Division by zero in modulo operation");
                } else {
                    Value::Number(a_as_f64 % b)
                }
            }
            (Value::Boolean(a), Value::Boolean(b)) => {
                let a_as_f64 = if *a { 1.0 } else { 0.0 };
                let b_as_f64 = if *b { 1.0 } else { 0.0 };
                if b_as_f64 == 0.0 {
                    panic!("Division by zero in modulo operation");
                } else {
                    Value::Number(a_as_f64 % b_as_f64)
                }
            }
            _ => panic!("Modulo operation not supported for non-numeric types"),
        }
    }
}

struct Enviorment {
    variables: HashMap<String, Value>,
    parent: Option<Weak<RefCell<Enviorment>>>,
}

impl Enviorment {
    pub fn new(parent: Option<Weak<RefCell<Enviorment>>>) -> Enviorment {
        let mut variables: HashMap<String, Value> = HashMap::new();

        if parent.is_none() {
            let mut console = Object::new(HashMap::new());

            console.fields.insert(
                "log".to_string(),
                Value::Function(Function {
                    arguments: vec!["val".to_string()],
                    body: FunctionBody::RustFunc(log),
                }),
            );

            variables.insert("console".to_string(), Value::Object(console));
            variables.insert("Math".to_string(), get_math_module());
        }

        Enviorment { variables, parent }
    }

    pub fn assign_variable(&mut self, id: String, value: Value) {
        match self.variables.get_mut(&id) {
            Some(val) => *val = value,
            None => {
                match &mut self.parent {
                    Some(parent) => {
                        let env_rc = parent
                            .upgrade()
                            .expect("Parent should always drop after child");
                        env_rc.borrow_mut().assign_variable(id, value);
                        // parent.upgrade().expect("Parent should always exist if child does").get_mut().assign_variable(id, value);
                    }
                    None => panic!("Assigning Variable Failed, Variable not Found"),
                }
            }
        }
    }

    pub fn create_variable(&mut self, id: String, init: Option<Value>) {
        self.variables.insert(id, init.unwrap_or(Value::Null));
    }

    pub fn get_variable(&self, id: &String) -> Value {
        match self.variables.get(id) {
            Some(val) => val.clone(),
            None => match &self.parent {
                Some(parent) => {
                    let env = parent
                        .upgrade()
                        .expect("Parents should always drop after child");
                    let val = env.borrow().get_variable(id).clone();
                    val
                }
                None => Value::Null,
            },
        }
    }
}

pub struct Runtime {
    ast: Program,
    position: usize,
    line: RefCell<usize>,
    root_enviorment: Rc<RefCell<Enviorment>>,
}

impl Runtime {
    pub fn new(ast: Program) -> Runtime {
        Runtime {
            ast,
            position: 0,
            line: RefCell::new(0),
            root_enviorment: Rc::new(RefCell::new(Enviorment::new(None))),
        }
    }

    pub fn run(&mut self) {
        while !self.finished() {
            self.execute_next_statement();
        }
    }

    // Excecutes the next statement in the AST
    fn execute_next_statement(&mut self) {
        let statement = &self.ast.body[self.position];
        self.execute_statement(statement, self.root_enviorment.clone());

        self.position += 1;
    }

    // Excecutes a Statement
    fn execute_statement(&self, statement: &Statement, scoped_enviorment: Rc<RefCell<Enviorment>>) {
        *self.line.borrow_mut() = statement.line.into();
        match &statement.value {
            StatementValue::ExpressionStmt(expression) => {
                self.evaluate_expression(expression, scoped_enviorment);
            }
            StatementValue::VariableStmt(decleration) => {
                self.declare_variable(decleration, scoped_enviorment)
            }
            StatementValue::IfStmt(if_statement) => {
                self.if_statement(if_statement, scoped_enviorment)
            }
            StatementValue::BlockStmt(block_statement) => {
                self.block_statement(block_statement, scoped_enviorment)
            }
            StatementValue::FunctionDecleration(function_decleration) => {
                self.declare_function(function_decleration, scoped_enviorment)
            }
            StatementValue::ReturnStatement(expression) => {
                self.return_statement(expression, scoped_enviorment)
            }
            StatementValue::WhileStmt(while_statement) => {
                self.while_statement(while_statement, scoped_enviorment)
            }
            StatementValue::ForStmt(for_statement) => {
                self.for_statement(for_statement, scoped_enviorment)
            }
        };
    }

    fn evaluate_expression(
        &self,
        expression: &Expression,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        let val = match expression {
            Expression::BinaryExpr(bin_exp) => {
                self.evaluate_binary_expression(bin_exp, scoped_enviorment)
            }
            Expression::UnaryExpr(un_exp) => {
                self.evaluate_unary_expression(un_exp, scoped_enviorment)
            }
            Expression::CallExpr(call_exp) => self.invoke_function(call_exp, scoped_enviorment),
            Expression::LiteralExpr(lit_exp) => {
                self.evaluate_literal_expression(lit_exp, scoped_enviorment)
            }
            Expression::Identifier(id) => self.evaluate_identifier(id, scoped_enviorment),
            Expression::Increment(id) => self.evaluate_binary_expression(
                &BinaryExpr::new(
                    Box::new(Expression::Identifier(id.to_string())),
                    BinOp::Assign,
                    Box::new(Expression::BinaryExpr(BinaryExpr::new(
                        Box::new(Expression::Identifier(id.to_string())),
                        BinOp::Add,
                        Box::new(Expression::LiteralExpr(Literal::Number(1.0))),
                    ))),
                ),
                scoped_enviorment,
            ),
            Expression::Decrement(id) => self.evaluate_binary_expression(
                &BinaryExpr::new(
                    Box::new(Expression::Identifier(id.to_string())),
                    BinOp::Assign,
                    Box::new(Expression::BinaryExpr(BinaryExpr::new(
                        Box::new(Expression::Identifier(id.to_string())),
                        BinOp::Subtract,
                        Box::new(Expression::LiteralExpr(Literal::Number(1.0))),
                    ))),
                ),
                scoped_enviorment,
            ),
            Expression::DotExpr(dot_expr) => {
                self.evaluate_dot_expression(dot_expr, scoped_enviorment)
            }
        };

        val
    }

    fn evaluate_literal_expression(
        &self,
        expression: &Literal,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        match expression {
            Literal::Boolean(b) => return Value::Boolean(*b),
            Literal::Number(n) => return Value::Number(*n),
            Literal::String(s) => return Value::String(s.clone()),
            Literal::Null => return Value::Null,
            Literal::Json(obj) => {
                let mut obj_value: HashMap<String, Value> = HashMap::new();

                for (key, expr) in obj {
                    let value = self.evaluate_expression(expr, scoped_enviorment.clone());
                    obj_value.insert(key.clone(), value);
                }

                return Value::Object(Object::new(obj_value));
            }
            _ => todo!(),
        }
    }

    fn evaluate_binary_expression(
        &self,
        expression: &BinaryExpr,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        match expression.operator {
            BinOp::Add => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .add(&self.evaluate_expression(&*expression.right, scoped_enviorment.clone())),
            BinOp::Subtract => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .subtract(&self.evaluate_expression(&*expression.right, scoped_enviorment.clone())),
            BinOp::Multiply => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .multiply(&self.evaluate_expression(&*expression.right, scoped_enviorment.clone())),
            BinOp::Divide => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .divide(&self.evaluate_expression(&*expression.right, scoped_enviorment.clone())),
            BinOp::LessThan => Value::Boolean(
                self.evaluate_expression(&*expression.left, scoped_enviorment.clone())
                    .less_than(
                        &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                    ),
            ),
            BinOp::Modulo => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .modulo(&self.evaluate_expression(&*expression.right, scoped_enviorment.clone())),
            BinOp::GreaterThan => Value::Boolean(
                self.evaluate_expression(&*expression.left, scoped_enviorment.clone())
                    .greater_than(
                        &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                    ),
            ),
            BinOp::LessThanEqual => Value::Boolean(
                self.evaluate_expression(&*expression.left, scoped_enviorment.clone())
                    .less_than_or_equal_to(
                        &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                    ),
            ),
            BinOp::GreaterThanEqual => Value::Boolean(
                self.evaluate_expression(&*expression.left, scoped_enviorment.clone())
                    .greater_than_or_equal_to(
                        &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                    ),
            ),
            BinOp::EqualTo => Value::Boolean(
                self.evaluate_expression(&*expression.left, scoped_enviorment.clone())
                    .equals(
                        &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                    ),
            ),
            BinOp::NotEqual => Value::Boolean(
                self.evaluate_expression(&*expression.left, scoped_enviorment.clone())
                    .not_equals(
                        &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                    ),
            ),
            BinOp::LogicalOr => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .logical_or(
                    &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                )
                .clone(),
            BinOp::LogicalAnd => self
                .evaluate_expression(&*expression.left, scoped_enviorment.clone())
                .logical_and(
                    &self.evaluate_expression(&*expression.right, scoped_enviorment.clone()),
                )
                .clone(),
            BinOp::Assign => {
                let right = self.evaluate_expression(&*expression.right, scoped_enviorment.clone());

                let id = match &*expression.left {
                    Expression::Identifier(id) => id,
                    _ => self.error("Left side of Assign BinOp must be identifier"),
                };

                scoped_enviorment
                    .borrow_mut()
                    .assign_variable(id.clone(), right.clone());

                right
            }
        }
    }

    fn evaluate_unary_expression(
        &self,
        expression: &UnaryExpr,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        match expression.operator {
            UnOp::Not => Value::Boolean(
                !self
                    .evaluate_expression(&*expression.operand, scoped_enviorment)
                    .is_truthy(),
            ),
            UnOp::Negate => self
                .evaluate_expression(&*expression.operand, scoped_enviorment)
                .multiply(&Value::Number(-1.0)),
        }
    }

    fn evaluate_identifier(
        &self,
        identifier: &String,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        scoped_enviorment.borrow().get_variable(identifier).clone()
    }

    fn declare_variable(
        &self,
        decleration: &VariableDecleration,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) {
        let id = &decleration.id;

        let init = if let Some(init_expr) = &decleration.init {
            Some(self.evaluate_expression(init_expr, scoped_enviorment.clone()))
        } else {
            None
        };

        scoped_enviorment
            .borrow_mut()
            .create_variable(id.clone(), init);
    }

    fn if_statement(&self, statement: &IfStmt, scoped_enviorment: Rc<RefCell<Enviorment>>) {
        let condition = self
            .evaluate_expression(&statement.condition, scoped_enviorment.clone())
            .is_truthy();

        if condition {
            self.execute_statement(&statement.consequent, scoped_enviorment.clone());
        } else if let Some(alt) = &statement.alternate {
            self.execute_statement(alt, scoped_enviorment.clone());
        }
    }

    fn block_statement(
        &self,
        statement: &Vec<Statement>,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) {
        let scope = Rc::new(RefCell::new(Enviorment::new(Some(
            Rc::<RefCell<Enviorment>>::downgrade(&scoped_enviorment),
        ))));
        let in_function = scoped_enviorment
            .borrow()
            .variables
            .get("return-value")
            .is_some();
        for single_statement in statement {
            if in_function
                && scoped_enviorment
                    .borrow()
                    .variables
                    .get("return-value")
                    .expect("Already Checked")
                    .clone()
                    != Value::Null
            {
                break;
            }
            self.execute_statement(single_statement, scope.clone())
        }
    }

    fn declare_function(
        &self,
        decleration: &FuncDecleration,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) {
        scoped_enviorment.borrow_mut().create_variable(
            decleration.id.clone(),
            Some(Value::Function(Function::new(
                decleration.parameters.clone(),
                Statement::new(*decleration.body.clone(), *self.line.borrow()),
            ))),
        )
    }

    fn invoke_function(
        &self,
        function_call: &CallExpr,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        if let Value::Function(callee) = scoped_enviorment
            .borrow()
            .get_variable(&function_call.callee)
        {
            if function_call.arguments.len() != callee.arguments.len() {
                panic!("Arguments don't match up");
            }

            let mut arg_values: Vec<Value> = Vec::new();
            arg_values.reserve(callee.arguments.len());

            let scope = Rc::new(RefCell::new(Enviorment::new(Some(
                Rc::<RefCell<Enviorment>>::downgrade(&scoped_enviorment.clone()),
            ))));
            // Hyphonated to make it an invalid JS variable
            scope
                .borrow_mut()
                .create_variable("return-value".to_string(), None);
            for arg_expression in function_call.arguments.iter() {
                let arg_value = self.evaluate_expression(arg_expression, scoped_enviorment.clone());
                arg_values.push(arg_value);
            }
            return match &callee.body {
                FunctionBody::JSFunc(body) => {
                    for (arg_value, arg_name) in arg_values.iter().zip(callee.arguments.iter()) {
                        scope
                            .borrow_mut()
                            .create_variable(arg_name.clone(), Some(arg_value.clone()));
                    }

                    self.execute_statement(body, scope.clone());

                    return scope
                        .borrow()
                        .variables
                        .get("return-value")
                        .expect("Should Always exist")
                        .clone();
                }
                FunctionBody::RustFunc(func) => (func)(arg_values),
            };
        } else {
            self.error("Function Not Found")
        };
    }

    fn invoke_method(
        &self,
        function_call: &CallExpr,
        object: &Object,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        // Guard to make sure that Method actually exists on object
        match object
            .fields
            .get(&function_call.callee)
            .unwrap_or(&Value::Null)
        {
            Value::Function(_) => (),
            _ => panic!("Method Not Callable on Object"),
        };

        let env = Rc::new(RefCell::new(Enviorment::new(Some(
            Rc::<RefCell<Enviorment>>::downgrade(&scoped_enviorment),
        ))));

        env.borrow_mut().variables = object.fields.clone();

        self.invoke_function(function_call, env)
    }

    fn return_statement(
        &self,
        return_expression: &Expression,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) {
        let evaluated_expr =
            self.evaluate_expression(&return_expression, scoped_enviorment.clone());
        scoped_enviorment
            .borrow_mut()
            .assign_variable("return-value".to_string(), evaluated_expr)
    }

    fn while_statement(
        &self,
        while_statement: &WhileStmt,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) {
        while self
            .evaluate_expression(&while_statement.condition, scoped_enviorment.clone())
            .is_truthy()
        {
            self.execute_statement(&while_statement.body, scoped_enviorment.clone())
        }
    }

    fn for_statement(&self, for_statement: &ForStmt, scoped_enviorment: Rc<RefCell<Enviorment>>) {
        self.execute_statement(&for_statement.initialization, scoped_enviorment.clone());
        while self
            .evaluate_expression(&for_statement.condition, scoped_enviorment.clone())
            .is_truthy()
        {
            self.execute_statement(&for_statement.body, scoped_enviorment.clone());
            self.execute_statement(&for_statement.afterthought, scoped_enviorment.clone());
        }
    }

    fn evaluate_dot_expression(
        &self,
        dot_expression: &DotExpr,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        let obj_id = match &*dot_expression.object {
            Expression::Identifier(id) => id,
            _ => panic!("LEFT HAND OF DOT EXPR MUST BE ID"),
        };

        let object = match scoped_enviorment.borrow().get_variable(obj_id) {
            Value::Object(obj) => obj,
            _ => panic!("Can not use dot expression with non-object value"),
        };

        match &*dot_expression.property {
            Expression::Identifier(id) => object.get_property(id.clone()).clone(),
            Expression::DotExpr(expr) => self.evaluate_accessor(expr, &object, scoped_enviorment),
            Expression::CallExpr(expr) => self.invoke_method(expr, &object, scoped_enviorment),
            _ => panic!("IDK THIS SHOULDN'T HAPPEN: {:?}", dot_expression.property),
        }
    }

    fn evaluate_accessor(
        &self,
        dot_expression: &DotExpr,
        object: &Object,
        scoped_enviorment: Rc<RefCell<Enviorment>>,
    ) -> Value {
        let id = match &*dot_expression.object {
            Expression::Identifier(id) => id,
            _ => panic!("Left hand of Dot Expr must be ID"),
        };

        let val = match object.get_property(id.clone()) {
            Value::Object(obj) => obj,
            _ => panic!("left hand must eval to Object"),
        };

        match &*dot_expression.property {
            Expression::DotExpr(expr) => self.evaluate_accessor(expr, val, scoped_enviorment),
            Expression::Identifier(id) => return val.get_property(id.clone()).clone(),
            Expression::CallExpr(expr) => return self.invoke_method(expr, val, scoped_enviorment),
            _ => panic!("Dot Expression Right Side MUST be DotExpr or ID"),
        }
    }

    fn finished(&mut self) -> bool {
        self.position >= self.ast.body.len()
    }

    fn error(&self, message: &str) -> ! {
        panic!("Runtime ERROR\nLine:{}\n{}", *self.line.borrow(), message)
    }
}
