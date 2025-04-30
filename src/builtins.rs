use crate::interpreter::{rte, RuntimeError, Value};

pub fn len(value: Value) -> Result<Value, RuntimeError> {
    Ok(match value {
        Value::Array(arr) => Value::Double(arr.len() as f64),
        _ => {
            if let Ok(Value::Array(arr)) = value.as_array() {
                return Ok(Value::Double(arr.len() as f64));
            }

            return rte("`len` only works on strings and arrays");
        }
    })
}

pub fn print(value: Value) -> Result<Value, RuntimeError> {
    print!("{}", value.as_string()?);
    Ok(Value::Void)
}

pub fn println(value: Value) -> Result<Value, RuntimeError> {
    println!("{}", value.as_string()?);
    Ok(Value::Void)
}

pub fn lines(value: Value) -> Result<Value, RuntimeError> {
    if let Value::String(s) = value {
        let lines = s
            .split('\n')
            .map(|s| Value::String(s.to_string()))
            .collect::<Vec<_>>();
        return Ok(Value::Array(lines));
    }

    rte("`lines` only works on strings")
}
