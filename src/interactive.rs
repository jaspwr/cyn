use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use crate::interpreter;
use crate::utils::eval_string;

pub fn start_interactive() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    let _ = rl.load_history("history.txt");

    let mut state = interpreter::ExecutionState::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());

                let out = eval_string(&line, &mut state);

                let message = match out {
                    Ok(value) => value.as_string().unwrap_or_else(|e| format!("[!] {:?}", e)),
                    Err(e) => format!("[!] {:?}", e),
                };

                println!("{}", message);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    let _ = rl.save_history("history.txt");
    Ok(())
}
