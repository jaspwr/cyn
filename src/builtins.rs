use std::path::PathBuf;

use crate::interpreter::{rte, ExecutionState, RuntimeError, RuntimeState, Value};

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

fn cd(args: Vec<Value>, state: &mut RuntimeState) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return rte("Usage: cd <path>");
    }

    let path = args[0].as_string()?;

    let path_pathbuf = PathBuf::from(path.clone());

    if !path_pathbuf.exists() {
        return rte("Path does not exist");
    }

    state.working_directory.set(&path)?;

    if let Err(e) = std::env::set_current_dir(path_pathbuf) {
        return rte(format!("{:?}", e));
    }

    Ok(Value::Void)
}

pub fn try_builtin(
    name: &str,
    args: Vec<Value>,
    state: &mut RuntimeState,
) -> Option<Result<Value, RuntimeError>> {
    Some(match name {
        "len" => len(args[0].clone()),
        "print" => print(args),
        "println" => println(args),
        "lines" => lines(args[0].clone()),
        "pwd" => Ok(Value::String(state.working_directory.as_str().to_string())),
        "cd" => cd(args, state),
        _ => return None,
    })
}
