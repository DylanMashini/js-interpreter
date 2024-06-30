use std::collections::HashMap;

use crate::runtime::{Function, FunctionBody, Prototype, Value};

fn push(this: &mut Value, args: Vec<Value>) -> Value {
    match this {
        Value::Array(arr, _) => {
            arr.push(args[0].clone());
        }
        _ => unreachable!("This should always be Value::Array"),
    }

    Value::Null
}

pub fn get_array_prototype() -> Prototype {
    let mut methods: HashMap<String, Value> = HashMap::new();
    methods.insert(
        "push".to_string(),
        Value::Function(Function {
            arguments: vec!["self".to_string(), "element".to_string()],
            body: FunctionBody::RustMutFunc(push),
        }),
    );

    Prototype::new(methods)
}
