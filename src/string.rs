use std::collections::HashMap;

use crate::runtime::{Function, FunctionBody, Prototype, Value};

pub fn includes(this: &mut Value, args: Vec<Value>) -> Value {
    let this = match this {
        Value::String(str, _) => str,
        _ => unreachable!("this will always be string type"),
    };

    let search_string = match args.get(0).unwrap_or(&Value::Null) {
        Value::String(str, _) => str,
        _ => panic!("String.prototype.includes() requires a searchString parameter"),
    };

    let position = args.get(1);

    if let Value::Number(pos) = position.unwrap_or(&Value::Null) {
        return Value::Boolean(this.split_at(*pos as usize).1.contains(search_string));
    }

    return Value::Boolean(this.contains(search_string));
}

pub fn get_string_prototype() -> Prototype {
    let mut methods: HashMap<String, Value> = HashMap::new();

    methods.insert(
        "includes".to_string(),
        Value::Function(Function {
            arguments: vec!["self".to_string(), "element".to_string()],
            body: FunctionBody::RustMutFunc(includes),
        }),
    );

    Prototype::new(methods)
}
