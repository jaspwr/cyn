use std::env;

use interactive::start_interactive;

pub mod tokenizer;
pub mod grammar;
pub mod interpreter;
pub mod interactive;
pub mod utils;

fn main() {
    if env::args().len() == 2 {
        let filename = env::args().nth(1).unwrap();
        let source = std::fs::read_to_string(filename).unwrap();

        let tokens = tokenizer::tokenize(&source);

        println!("tokens: {:#?}", tokens);

        let ast = grammar::parse(tokens);

        println!("ast: {:#?}", ast);

        if let Some((_, ast)) = ast {
            let mut state = interpreter::ExecutionState::new();
            let _ = interpreter::eval(ast, &mut state);

            let result = state.run_main(vec![]);
            println!("result: {:#?}", result);
        }
    }

    
    let _ = start_interactive();
}
