use std::collections::HashMap;

use crate::interpreter::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Never,
    Integer,
    Double,
    Bool,
    RangeGenerator,
    Array(Box<Type>),
    String,
    Lambda(Vec<Type>, Box<Type>),
    Struct(String, HashMap<String, Type>),
}

pub fn type_of(value: &Value) -> Type {
    match value {
        Value::Void => Type::Void,
        Value::Integer(_) => Type::Integer,
        Value::Double(_) => Type::Double,
        Value::Bool(_) => Type::Bool,
        Value::RangeGenerator { .. } => Type::RangeGenerator,
        Value::Array(arr) => {
            if arr.is_empty() {
                Type::Array(Box::new(Type::Void))
            } else {
                let first = &arr[0];
                let mut t = type_of(&first);
                for item in arr.iter().skip(1) {
                    if type_of(&item) != t {
                        t = Type::Void;
                        break;
                    }
                }
                Type::Array(Box::new(t))
            }
        }
        Value::String(_) => Type::String,
        Value::Lambda { .. } => todo!(),
    }
}
