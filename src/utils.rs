use std::collections::HashSet;

use crate::{
    grammar,
    interpreter::{self, ExecutionContext, ExecutionState, RuntimeError, Value},
    tokenizer,
};

pub fn eval_string(
    source: &str,
    state: &mut ExecutionState,
    ctx: ExecutionContext,
) -> Result<Value, RuntimeError> {
    let tokens = tokenizer::tokenize(source);
    // println!("tokens: {:#?}", tokens);

    match grammar::parse(&tokens) {
        Ok(ast) => {
            // println!("ast: {:#?}", ast);
            interpreter::eval(ast, state, ctx)
        }
        Err(e) => {
            return Err(RuntimeError {
                message: format!("Syntax error: {}", e.message),
                callstack: vec![],
                range: Some(e.range),
            });
        }
    }
}

#[derive(Debug)]
pub struct Args {
    pub files: Vec<String>,
    pub short_flags: HashSet<String>,
    pub long_flags: HashSet<String>,
}

pub fn parse_args(args: Vec<Value>) -> Result<Args, RuntimeError> {
    let mut files = vec![];
    let mut short_flags = HashSet::new();
    let mut long_flags = HashSet::new();

    for arg in args {
        let s = arg.as_string()?;

        if s.is_empty() {
            continue;
        }

        if s.chars().nth(0) == Some('-') {
            if s.chars().nth(1) == Some('-') {
                long_flags.insert(s.chars().skip(2).collect());
                continue;
            }

            for char in s.chars().skip(1) {
                short_flags.insert(char.to_string());
            }
            continue;
        }

        files.push(s);
    }

    Ok(Args {
        files,
        short_flags,
        long_flags,
    })
}
