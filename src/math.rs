use crate::runtime::{Function, FunctionBody, Object, Value};
use std::collections::HashMap;

pub fn sqrt(args: Vec<Value>) -> Value {
    let x = args.get(0).expect("SQRT must have one argument");
    match x {
        Value::Number(n) => return Value::Number(n.sqrt()),
        _ => return Value::Number(f64::NAN),
    }
}

pub fn cos(args: Vec<Value>) -> Value {
    let x = args.get(0).expect("COS must have one argument");
    match x {
        Value::Number(n) => return Value::Number(n.cos()),
        _ => return Value::Number(f64::NAN),
    }
}

pub fn sin(args: Vec<Value>) -> Value {
    let x = args.get(0).expect("SIN must have one argument");
    match x {
        Value::Number(n) => return Value::Number(n.sin()),
        _ => return Value::Number(f64::NAN),
    }
}

pub fn tan(args: Vec<Value>) -> Value {
    let x = args.get(0).expect("TAN must have one argument");
    match x {
        Value::Number(n) => return Value::Number(n.tan()),
        _ => return Value::Number(f64::NAN),
    }
}

pub fn get_math_module() -> Value {
    let mut math = Object::new(HashMap::new());

    let sqrt = Value::Function(Function {
        arguments: vec!["x".to_string()],
        body: FunctionBody::RustFunc(sqrt),
    });

    let cos = Value::Function(Function {
        arguments: vec!["x".to_string()],
        body: FunctionBody::RustFunc(cos),
    });

    let sin = Value::Function(Function {
        arguments: vec!["x".to_string()],
        body: FunctionBody::RustFunc(sin),
    });

    let tan = Value::Function(Function {
        arguments: vec!["x".to_string()],
        body: FunctionBody::RustFunc(tan),
    });

    math.fields.insert("sqrt".to_string(), sqrt);
    math.fields.insert("cos".to_string(), cos);
    math.fields.insert("sin".to_string(), sin);
    math.fields.insert("sin".to_string(), tan);
    math.fields
        .insert("PI".to_string(), Value::Number(std::f64::consts::PI));

    Value::Object(math)
}
