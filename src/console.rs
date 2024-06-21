use crate::runtime::Value;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
static mut OUTPUT: String = String::new();

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub fn log(args: Vec<Value>) -> Value {
    let str = &format!("{:?}\n", args[0]);
    unsafe {
        // Single Threaded, so no potential data-races
        OUTPUT = OUTPUT.clone() + str;
    }
    Value::Null
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub fn get_output() -> String {
    unsafe { return OUTPUT.clone() }
}

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub fn reset_output() {
    unsafe {
        OUTPUT = String::new();
    }
}

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub fn log(args: Vec<Value>) -> Value {
    println!("{:?}", args[0]);
    Value::Null
}
