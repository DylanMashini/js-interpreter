use crate::runtime::Value;

pub fn log(args: Vec<Value>) -> Value {
    println!("{:?}", args[0]);
    Value::Null
}
