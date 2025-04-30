use crate::{
    grammar,
    interpreter::{self, ExecutionContext, ExecutionState, RuntimeError, Value},
    tokenizer,
};

pub fn eval_string(source: &str, state: &mut ExecutionState) -> Result<Value, RuntimeError> {
    let tokens = tokenizer::tokenize(source);
    // println!("tokens: {:#?}", tokens);
    if let Some((_, ast)) = grammar::parse(tokens) {
        // println!("ast: {:#?}", ast);
        interpreter::eval(ast, state, ExecutionContext::new())
    } else {
        // TODO
        panic!();
    }
}
