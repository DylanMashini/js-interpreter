use std::collections::HashMap;

use crate::runtime::{Function, FunctionBody, Prototype, Value};

fn get_array_ref(this: &mut Value) -> &mut Vec<Value> {
    match this {
        Value::Array(arr, _) => arr,
        _ => panic!("This must be of type Array"),
    }
}

fn push(this: &mut Value, args: Vec<Value>) -> Value {
    match this {
        Value::Array(arr, _) => {
            arr.push(args[0].clone());
        }
        _ => unreachable!("This should always be Value::Array"),
    }

    Value::Null
}

pub fn for_each(this: &mut Value, args: Vec<Value>) -> Value {
    let this = get_array_ref(this);

    let callback = args.get(0).expect("forEach requires a callback function");

    let function = match callback {
        Value::Function(f) => f,
        _ => panic!("Callback Argument must be function"),
    };

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
