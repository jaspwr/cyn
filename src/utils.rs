use crate::{
    grammar,
    interpreter::{self, ExecutionContext, ExecutionState, RuntimeError, Value},
    tokenizer,
};

pub fn eval_string(source: &str, state: &mut ExecutionState) -> Result<Value, RuntimeError> {
    let tokens = tokenizer::tokenize(source);
    // println!("tokens: {:#?}", tokens);

    match grammar::parse(tokens) {
        Ok(ast) => {
            // println!("ast: {:#?}", ast);
            interpreter::eval(ast, state, ExecutionContext::new())
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
