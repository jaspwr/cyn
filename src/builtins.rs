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

pub fn print(values: Vec<Value>) -> Result<Value, RuntimeError> {
    print!(
        "{}",
        values
            .iter()
            .map(|v| v.as_string())
            .collect::<Result<Vec<_>, _>>()?
            .join(" ")
    );
    Ok(Value::Void)
}

pub fn println(values: Vec<Value>) -> Result<Value, RuntimeError> {
    print(values.clone())?;
    println!();
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

pub fn try_builtin(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    Some(match name {
        "len" => len(args[0].clone()),
        "print" => print(args),
        "println" => println(args),
        "lines" => lines(args[0].clone()),
        _ => return None,
    })
}
