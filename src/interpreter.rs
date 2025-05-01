use std::{collections::HashMap, env, io::Write, path::PathBuf, process::Command};

use crate::{
    builtins::try_builtin,
    grammar::{self, Node},
    heapless, tokenizer,
};

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
    function_prefix: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ExecutionContext {
    pub piped: bool,
    pub piped_input: Option<String>,
    pub function_prefix: Option<String>,
}

impl ExecutionContext {
    pub fn new() -> Self {
        Self {
            piped: false,
            piped_input: None,
            function_prefix: None,
        }
    }

    pub fn function_name(&self, name: String) -> String {
        if let Some(prefix) = &self.function_prefix {
            format!("{}.{}", prefix, name)
        } else {
            name
        }
    }
}

impl Function {
    fn eval(
        &self,
        args: Vec<Value>,
        state: &mut ExecutionState,
        ctx: ExecutionContext,
    ) -> Result<Value, RuntimeError> {
        let ctx = ExecutionContext {
            function_prefix: self.function_prefix.clone(),
            ..ctx.clone()
        };

        if self.args.len() > args.len() {
            // Curry!

            let captured = args
                .iter()
                .zip(self.args.iter())
                .map(|(value, arg)| (arg.clone(), value.clone()))
                .collect::<HashMap<_, _>>();

            return Ok(Value::Lambda {
                args: self.args.iter().skip(args.len()).cloned().collect(),
                captured,
                body: self.body.clone(),
            });
        }

        let previous_constants = state.constants.clone();

        for (arg, value) in self.args.iter().zip(args.iter()) {
            state.constants.insert(arg.clone(), value.clone());
        }

        let mut ret = eval(*self.body.clone(), state, ctx.clone()).map_err(|e| {
            let mut new_e = e.clone();
            new_e.callstack.push(self.name.clone());
            new_e
        })?;

        state.constants = previous_constants;

        let leftover = args.into_iter().skip(self.args.len()).collect::<Vec<_>>();

        if !leftover.is_empty() {
            ret = eval_lambda(ret, leftover, state, ctx.clone())?;
        }

        Ok(ret)
    }
}

fn eval_lambda(
    lambda: Value,
    arg_values: Vec<Value>,
    state: &mut ExecutionState,
    ctx: ExecutionContext,
) -> Result<Value, RuntimeError> {
    let lambda_string = lambda.as_string().unwrap_or_default();

    if let Value::Lambda {
        args: args_names,
        captured,
        body,
    } = lambda
    {
        let previous_constants = state.constants.clone();

        for (arg, value) in args_names.iter().zip(arg_values.into_iter()) {
            state.constants.insert(arg.clone(), value.clone());
        }

        state.constants.extend(captured);

        let ret = eval(*body, state, ctx.clone()).map_err(|e| {
            let mut new_e = e.clone();
            new_e.callstack.push(lambda_string.clone());
            new_e
        })?;

        state.constants = previous_constants;

        return Ok(ret);
    } else {
        return rte(format!("Can not evaluate `{}`", lambda.as_string()?));
    }
}

fn eval_as_command(
    name: String,
    args: Vec<Value>,
    ctx: ExecutionContext,
) -> Result<Value, RuntimeError> {
    let mut cmd = Command::new(&name);

    cmd.args(
        args.iter()
            .map(|arg| arg.as_string())
            .collect::<Result<Vec<_>, _>>()?,
    );

    if ctx.piped {
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());
    } else {
        cmd.stdout(std::process::Stdio::inherit());
        cmd.stderr(std::process::Stdio::inherit());
    }

    if ctx.piped_input.is_some() {
        cmd.stdin(std::process::Stdio::piped());
    } else {
        cmd.stdin(std::process::Stdio::inherit());
    }

    let cmd = cmd.spawn();

    if let Err(e) = cmd {
        return rte(format!("Failed to execute command {}: {}", name, e));
    }

    let mut child = cmd.unwrap();

    if let Some(piped_input) = ctx.piped_input {
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(piped_input.as_bytes()).unwrap();
    }

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
            runtime_state: RuntimeState::new(),
        }
    }

    pub fn run_main(&mut self, args: Vec<String>) -> Result<Value, RuntimeError> {
        if let Some(main) = self.functions.get("main").cloned() {
            let ctx = ExecutionContext::new();
            main.eval(vec![], self, ctx.clone())
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
    Integer(i64),
    Bool(bool),
    RangeGenerator {
        start: i64,
        end: i64,
    },
    Lambda {
        args: Vec<String>,
        captured: HashMap<String, Value>,
        body: Box<Node>,
    },
    Array(Vec<Value>),
}

impl Value {
    pub fn as_string(&self) -> Result<String, RuntimeError> {
        Ok(match self {
            Value::Void => return rte("Cannot convert void to string"),
            Value::String(s) => s.clone(),
            Value::Double(d) => d.to_string(),
            Value::Integer(i) => i.to_string(),
            Value::Lambda { args, body, .. } => {
                // TODO: partially apply
                let args = args.join(" ");
                format!("λ{} -> {}", args, (*body).stringify())
            }
            Value::Array(items) => {
                let items = items
                    .iter()
                    .map(|item| item.as_string())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(", ");
                format!("[{}]", items)
            }
            Value::Bool(value) => if *value { "true" } else { "false" }.to_string(),
            Value::RangeGenerator { .. } => self.as_array()?.as_string()?,
        })
    }

    fn as_double(&self) -> Result<f64, RuntimeError> {
        match self {
            Value::Double(d) => Ok(*d),
            Value::Integer(i) => Ok(*i as f64),
            _ => self.as_string().and_then(|s| {
                s.trim().parse::<f64>().map_err(|_| RuntimeError {
                    message: format!("Recieved {}. expected double", s),
                    callstack: vec![],
                    range: None,
                })
            }),
        }
    }

    fn as_int(&self) -> Result<i64, RuntimeError> {
        match self {
            Value::Integer(d) => Ok(*d),
            _ => self.as_string().and_then(|s| {
                s.trim().parse::<i64>().map_err(|_| RuntimeError {
                    message: format!("Recieved {}. expected integer", s),
                    callstack: vec![],
                    range: None,
                })
            }),
        }
    }

    pub fn as_array(&self) -> Result<Value, RuntimeError> {
        match self {
            Value::Array(arr) => Ok(Value::Array(arr.clone())),
            Value::String(s) => Ok(Value::Array(
                s.chars().map(|c| Value::String(c.to_string())).collect(),
            )),
            Value::RangeGenerator { start, end } => {
                let mut arr = Vec::new();
                for i in *start..*end {
                    arr.push(Value::Integer(i));
                }
                Ok(Value::Array(arr))
            }
            _ => rte(format!("`{}` is not a valid array", self.as_string()?)),
        }
    }

    pub fn as_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => self.as_string().and_then(|s| {
                if s == "true" {
                    Ok(true)
                } else if s == "false" {
                    Ok(false)
                } else {
                    rte(format!("Recieved {}. expected bool", s))
                }
            }),
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct RuntimeState {
    pub working_directory: heapless::Str<1024>,
}

impl RuntimeState {
    pub fn new() -> Self {
        Self {
            working_directory: heapless::Str::from_str(
                env::current_dir()
                    .unwrap_or_default()
                    .to_str()
                    .unwrap_or_default(),
            )
            .unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub callstack: Vec<String>,
    pub message: String,
    pub range: Option<(usize, usize)>,
}

pub fn rte<T>(message: impl ToString) -> Result<T, RuntimeError> {
    Err(RuntimeError {
        message: message.to_string(),
        callstack: vec![],
        range: None,
    })
}

pub fn eval(
    node: Node,
    state: &mut ExecutionState,
    ctx: ExecutionContext,
) -> Result<Value, RuntimeError> {
    Ok(match node {
        Node::String(s) => Value::String(s),
        Node::Indentifier(s) => {
            // println!("constants: {:#?} ........ {}", state, s);
            if let Some(value) = state.constants.get(&s) {
                return Ok(value.clone());
            }

            if let Some(function) = state.functions.get(&ctx.function_name(s.clone())).cloned() {
                return function.eval(
                    vec![],
                    state,
                    ExecutionContext {
                        piped: true,
                        ..ctx.clone()
                    },
                );
            }

            for (key, value) in env::vars() {
                if key == s {
                    return Ok(Value::String(value));
                }
            }

            // return rte(format!("Undefined identifier: {}", s));

            return eval_as_command(s, vec![], ctx.clone());
        }
        Node::DoubleLiteral(d) => Value::Double(d),
        Node::BinaryOperation(oper, a, b) => {
            let ctx = ExecutionContext {
                piped: true,
                ..ctx.clone()
            };

            match oper {
                crate::grammar::BinaryOperation::Add => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Double(a + b)
                }
                crate::grammar::BinaryOperation::Sub => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Double(a - b)
                }
                crate::grammar::BinaryOperation::Mul => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Double(a * b)
                }
                crate::grammar::BinaryOperation::Div => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Double(a / b)
                }
                crate::grammar::BinaryOperation::Pow => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Double(a.powf(b))
                }
                crate::grammar::BinaryOperation::Eq => {
                    // TODO: try checking types first
                    let a = eval(*a, state, ctx.clone())?.as_string()?;
                    let b = eval(*b, state, ctx.clone())?.as_string()?;
                    Value::Bool(a == b)
                }
                crate::grammar::BinaryOperation::Lt => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Bool(a < b)
                }
                crate::grammar::BinaryOperation::Gt => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Bool(a > b)
                }
                crate::grammar::BinaryOperation::Lte => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Bool(a <= b)
                }
                crate::grammar::BinaryOperation::Gte => {
                    let a = eval(*a, state, ctx.clone())?.as_double()?;
                    let b = eval(*b, state, ctx.clone())?.as_double()?;
                    Value::Bool(a >= b)
                }
                crate::grammar::BinaryOperation::And => {
                    let a = eval(*a, state, ctx.clone())?.as_bool()?;
                    let b = eval(*b, state, ctx.clone())?.as_bool()?;
                    Value::Bool(a && b)
                }
                crate::grammar::BinaryOperation::Or => {
                    let a = eval(*a, state, ctx.clone())?.as_bool()?;
                    let b = eval(*b, state, ctx.clone())?.as_bool()?;
                    Value::Bool(a || b)
                }
                crate::grammar::BinaryOperation::Index => {
                    let a = eval(*a, state, ctx.clone())?.as_array()?;
                    let b = eval(*b, state, ctx.clone())?.as_int()?;

                    if let Value::Array(arr) = a {
                        if b < 0 {
                            return rte(format!("Negative index: {}", b));
                        }
                        if b >= arr.len() as i64 {
                            return rte(format!("Index out of bounds: {}", b));
                        }
                        return Ok(arr[b as usize].clone());
                    } else {
                        panic!();
                    }
                }
                crate::grammar::BinaryOperation::Custon(_) => todo!(),
                crate::grammar::BinaryOperation::Range => {
                    let a = eval(*a, state, ctx.clone())?.as_int()?;
                    let b = eval(*b, state, ctx.clone())?.as_int()?;
                    range(state, a, b, ctx.clone())?
                }
                crate::grammar::BinaryOperation::RangeInclusive => {
                    let a = eval(*a, state, ctx.clone())?.as_int()?;
                    let b = eval(*b, state, ctx.clone())?.as_int()? + 1;
                    range(state, a, b, ctx.clone())?
                }
                crate::grammar::BinaryOperation::WriteFile => {
                    let data = eval(*a, state, ctx.clone())?.as_string()?;
                    let path = eval(*b, state, ctx.clone())?.as_string()?;

                    if let Err(e) = std::fs::write(path, data) {
                        return rte(format!("Failed to write file: {}", e));
                    }

                    Value::Void
                }
                crate::grammar::BinaryOperation::Pipe => {
                    let data = eval(*a, state, ctx.clone())?.as_string()?;
                    eval(
                        *b,
                        state,
                        ExecutionContext {
                            piped_input: Some(data),
                            ..ctx.clone()
                        },
                    )?
                }
                crate::grammar::BinaryOperation::EnvAssign => {
                    let var = eval(*a, state, ctx.clone())?.as_string()?;
                    let data = eval(*b, state, ctx.clone())?.as_string()?;

                    std::env::set_var(var, data);

                    Value::Void
                }
                crate::grammar::BinaryOperation::Concat => {
                    let a = eval(*a, state, ctx.clone())?.as_array()?;
                    let b = eval(*b, state, ctx.clone())?.as_array()?;

                    if let Value::Array(mut a) = a {
                        if let Value::Array(b) = b {
                            a.extend(b);
                            return Ok(Value::Array(a));
                        }
                    }

                    panic!();
                }
            }
        }
        Node::UnaryOperation(oper, a) => match oper {
            crate::grammar::UnaryOperation::Negate => {
                let a = eval(*a, state, ctx.clone())?.as_double()?;
                Value::Double(-a)
            }
        },
        Node::Call(name, arguments) => {
            let args = arguments
                .into_iter()
                .map(|arg| eval(arg, state, ctx.clone()))
                .collect::<Result<Vec<_>, _>>()?;

            let name = match *name.clone() {
                Node::String(s) => s,
                Node::Indentifier(s) => s,
                Node::Call(b, _) => {
                    if let Node::String(s) = *b {
                        s
                    } else {
                        return rte(format!("Invalid function name: {}", name.stringify()));
                    }
                }
                _ => {
                    let value = eval(*name, state, ctx.clone())?;
                    if let Value::Lambda { .. } = value {
                        return eval_lambda(value, args, state, ctx.clone());
                    } else {
                        return rte(format!("Invalid function name: {}", value.as_string()?));
                    }
                }
            };

            if let Some(function) = state
                .functions
                .get(&ctx.function_name(name.clone()))
                .cloned()
            {
                return function.eval(args, state, ctx.clone());
            }

            if let Some(value) = state.constants.get(&name) {
                return eval_lambda(value.clone(), args, state, ctx.clone());
            }

            if let Some(value) = try_builtin(&name, args.clone(), &mut state.runtime_state) {
                return value;
            }

            return eval_as_command(name, args, ctx.clone());

            // return rte(format!("Undefined function: {}", name));
        }
        Node::Assignment {
            name,
            args,
            body,
            where_definitions,
        } => {
            let name = ctx.function_name(as_identifier(*name)?);

            let args = args
                .into_iter()
                .map(|arg| as_identifier(arg))
                .collect::<Result<Vec<_>, _>>()?;

            let function = Function {
                name: name.clone(),
                args,
                body,
                function_prefix: ctx.function_prefix.clone(),
                owned_functions: vec![],
                owner: None,
            };

            state.functions.insert(name, function);

            Value::Void
        }
        Node::DefinitionList(defs) => {
            for def in defs {
                eval(def, state, ctx.clone())?;
            }

            Value::Void
        }
        Node::Lambda { args, body } => {
            let args = args
                .into_iter()
                .map(|arg| as_identifier(arg))
                .collect::<Result<Vec<_>, _>>()?;

            Value::Lambda {
                args,
                // TODO: only use values that are used in the body
                captured: state.constants.clone(),
                body,
            }
        }
        Node::ArrayLiteral(items) => Value::Array(
            items
                .into_iter()
                .map(|i| eval(i, state, ctx.clone()))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Node::IfThenElse {
            condition,
            then_branch,
            else_branch,
        } => {
            if eval(*condition, state, ctx.clone())?.as_bool()? {
                eval(*then_branch, state, ctx.clone())?
            } else {
                eval(*else_branch, state, ctx.clone())?
            }
        }
        Node::Import { qualified, path } => {
            let path = eval(*path, state, ctx)?.as_string()?;
            let path_pathbuf = PathBuf::from(path.clone());
            if !path_pathbuf.exists() {
                return rte(format!("File {} does not exist", path));
            }
            let Some(module) = path_pathbuf
                .file_stem()
                .map(|s| s.to_string_lossy().to_string())
            else {
                return rte(format!("Module in {} didn't have a valid name.", path));
            };
            let Ok(source) = std::fs::read_to_string(path_pathbuf) else {
                return rte(format!("Failed to read file {}", path));
            };

            let tokens = tokenizer::tokenize(&source);

            let mut ctx = ExecutionContext::new();

            if qualified {
                ctx.function_prefix = Some(module);
            }

            match grammar::parse(tokens) {
                Ok(ast) => eval(ast, state, ctx)?,
                Err(e) => {
                    return Err(RuntimeError {
                        message: format!("Syntax error: {}", e.message),
                        callstack: vec![],
                        range: Some(e.range),
                    });
                }
            }
        }
    })
}

fn range(
    state: &mut ExecutionState,
    a: i64,
    b: i64,
    ctx: ExecutionContext,
) -> Result<Value, RuntimeError> {
    if a > b {
        return rte(format!("Invalid range: {}..{}", a, b));
    }
    Ok(Value::RangeGenerator { start: a, end: b })
}

fn as_identifier(node: Node) -> Result<String, RuntimeError> {
    match node {
        Node::Indentifier(s) => Ok(s),
        Node::String(s) => Ok(s),
        _ => rte("Invalid identifier"),
    }
}

fn as_string(node: Node) -> Result<String, RuntimeError> {
    match node {
        Node::String(s) => Ok(s),
        _ => rte("Invalid identifier"),
    }
}
