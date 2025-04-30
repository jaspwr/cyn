use std::{collections::HashMap, process::Command};

use crate::grammar::Node;

#[derive(Debug)]
pub struct ExecutionState {
    functions: HashMap<String, Function>,
    constants: HashMap<String, Value>,
    runtime_state: RuntimeState,
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    args: Vec<String>,
    body: Box<Node>,
    owned_functions: Vec<Function>,
    owner: Option<String>,
}

impl Function {
    fn eval(&self, args: Vec<Value>, state: &mut ExecutionState) -> Result<Value, RuntimeError> {
        if self.args.len() != args.len() {
            return rte(format!(
                "Function {} expected {} arguments, got {}",
                self.owner.as_ref().unwrap(),
                self.args.len(),
                args.len()
            ));
        }

        let previous_constants = state.constants.clone();

        for (arg, value) in self.args.iter().zip(args) {
            state.constants.insert(arg.clone(), value);
        }

        let ret = eval(*self.body.clone(), state).map_err(|e| {
            let mut new_e = e.clone();
            new_e.callstack.push(self.name.clone());
            new_e
        })?;

        state.constants = previous_constants;

        Ok(ret)
    }

}

fn eval_as_command(
    name: String,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let cmd = Command::new(&name)
        .args(args.iter().map(|arg| arg.as_string()).collect::<Result<Vec<_>, _>>()?)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn();

    if let Err(e) = cmd {
        return rte(format!(
            "Failed to execute command {}: {}",
            name, e
        ));
    }

    let mut child = cmd.unwrap();
    let stdin = child.stdin.as_mut().unwrap();

    // if let Some(input) = piped_input {
    //     stdin.write_all(input.as_bytes()).await.unwrap();
    // }

    let cmd_output = child.wait_with_output();

    let status_code = cmd_output.as_ref().unwrap().status.code().unwrap_or(1);

    let out = match cmd_output {
        Ok(output) => String::from_utf8_lossy(&output.stdout).to_string(),
        Err(e) => e.to_string(),
    };

    // if status_code != 0 {
    //     return rte(format!(
    //         "Command {} failed with status code {}: {}",
    //         self.name, status_code, out
    //     ));
    // }

    Ok(Value::String(out))
}

impl ExecutionState {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            constants: HashMap::new(),
            runtime_state: RuntimeState {},
        }
    }

    pub fn run_main(&mut self, args: Vec<String>) -> Result<Value, RuntimeError> {
        if let Some(main) = self.functions.get("main").cloned() {
            main.eval(vec![], self)
        } else {
            return rte("No main function defined");
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    String(String),
    Double(f64),
}

impl Value {
    fn as_string(&self) -> Result<String, RuntimeError> {
        Ok(match self {
            Value::Void => return rte("Cannot convert void to string"),
            Value::String(s) => s.clone(),
            Value::Double(d) => d.to_string(),
        })
    }

    fn as_double(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::Double(d) => Ok(*d),
            _ => self.as_string().and_then(|s| s.trim().parse::<f64>().map_err(|_| {
                RuntimeError {
                    message: format!("Recieved {}. expected double", s),
                    callstack: vec![],
                }
            })),
        }
    }
}

#[repr(C)]
#[derive(Debug)]
struct RuntimeState {}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    callstack: Vec<String>,
    message: String,
}

fn rte<T>(message: impl ToString) -> Result<T, RuntimeError> {
    Err(RuntimeError {
        message: message.to_string(),
        callstack: vec![],
    })
}

pub fn eval(node: Node, state: &mut ExecutionState) -> Result<Value, RuntimeError> {
    Ok(match node {
        Node::String(s) => Value::String(s),
        Node::Indentifier(s) => {
            if let Some(value) = state.constants.get(&s) {
                return Ok(value.clone());
            }

            if let Some(function) = state.functions.get(&s).cloned() {
                return function.eval(vec![], state);
            }

            // return rte(format!("Undefined identifier: {}", s));
            
            return eval_as_command(s, vec![]);
        }
        Node::DoubleLiteral(d) => Value::Double(d),
        Node::Add(a, b) => Value::Double(
            eval(*a, state)?.as_double().unwrap() + eval(*b, state)?.as_double().unwrap(),
        ),
        Node::Sub(a, b) => Value::Double(
            eval(*a, state)?.as_double().unwrap() - eval(*b, state)?.as_double().unwrap(),
        ),
        Node::Mul(a, b) => Value::Double(
            eval(*a, state)?.as_double().unwrap() * eval(*b, state)?.as_double().unwrap(),
        ),
        Node::Div(a, b) => Value::Double(
            eval(*a, state)?.as_double().unwrap() / eval(*b, state)?.as_double().unwrap(),
        ),
        Node::Negate(a) => Value::Double(
            -eval(*a, state)?.as_double().unwrap()
        ),
        Node::Call(name, arguments) => {
            let name = as_identifier(*name)?;

            if let Some(function) = state.functions.get(&name).cloned() {
                let args = arguments
                    .into_iter()
                    .map(|arg| eval(arg, state))
                    .collect::<Result<Vec<_>, _>>()?;

                return function.eval(args, state);
            }

            let args = arguments.into_iter()
                .map(|arg| eval(arg, state))
                .collect::<Result<Vec<_>, _>>()?;

            return eval_as_command(name, args);

            // return rte(format!("Undefined function: {}", name));
        }
        Node::Assignment {
            name,
            args,
            body,
            where_definitions,
        } => {
            let name = as_identifier(*name)?;

            let args = args
                .into_iter()
                .map(|arg| as_identifier(arg))
                .collect::<Result<Vec<_>, _>>()?;

            let function = Function {
                name: name.clone(),
                args,
                body,
                owned_functions: vec![],
                owner: None,
            };

            state.functions.insert(name, function);

            Value::Void
        }
        Node::DefinitionList(defs) => {
            for def in defs {
                eval(def, state)?;
            }

            Value::Void
        },
        Node::Pow(a, b) => todo!(),

    })
}

fn as_identifier(node: Node) -> Result<String, RuntimeError> {
    match node {
        Node::Indentifier(s) => Ok(s),
        Node::String(s) => Ok(s),
        _ => rte("Invalid identifier"),
    }
}
