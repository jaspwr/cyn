use std::env;

use std::borrow::Cow::{self, Owned};

use rustyline::error::ReadlineError;
use rustyline::{Cmd, DefaultEditor, KeyEvent, Result};

use rustyline::completion::FilenameCompleter;
use rustyline::highlight::{CmdKind, Highlighter, MatchingBracketHighlighter};
use rustyline::hint::HistoryHinter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Hinter, Validator};
use rustyline::{CompletionType, Config, Editor};

use crate::interpreter::{self, ExecutionContext};
use crate::utils::eval_string;

use owo_colors::OwoColorize;

#[derive(Helper, Completer, Hinter, Validator)]
struct MyHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Owned(prompt.bold().to_string())
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned(hint.dimmed().to_string())
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize, kind: CmdKind) -> bool {
        self.highlighter.highlight_char(line, pos, kind)
    }
}

pub fn start_interactive(mut state: interpreter::ExecutionState) -> Result<()> {
    let mut rl = DefaultEditor::new()?;

    println!(
        "  ....  .... ... .. ...  
.|   ''  '|.  |   ||  || 
||        '|.|    ||  || 
 '|...'    '|    .||. ||.
        .. |             
         ''"
    );
    println!("-------- v{} --------", env!("CARGO_PKG_VERSION"));

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .build();

    let h = MyHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter::new(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyEvent::alt('l'), Cmd::Insert(1, "λ".to_string()));

    let mut history_path = None;

    if let Ok(Some(home)) = homedir::my_home() {
        let history = home.join(".cynhistory");
        history_path = Some(history);
        if history_path.as_ref().unwrap().exists() {
            rl.load_history(history_path.as_ref().unwrap())
                .unwrap_or_else(|_| {
                    eprintln!("Could not load history from .cynhistory");
                });
        }
    }

    loop {
        let readline = rl.readline(prompt(&state).as_str());
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());

                let out = eval_string(&line, &mut state, ExecutionContext::new());

                let message = match out {
                    Ok(value) => value.as_string().unwrap_or_default(),
                    Err(e) => format!("[!] {:?}", e),
                };

                if message.trim().is_empty() {
                    continue;
                }

                println!("{}", message);

                if let Some(history_path) = history_path.as_ref() {
                    if let Err(e) = rl.save_history(history_path) {
                        eprintln!("Could not save history to .cynhistory: {:?}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                // println!("CTRL-C");
                // break;
            }
            Err(ReadlineError::Eof) => {
                // println!("CTRL-D");
                // break;
            }
            Err(err) => {
                println!("Terminal Error: {:?}", err);
                break;
            }
        }
    }

    if let Some(history_path) = history_path.as_ref() {
        if let Err(e) = rl.save_history(history_path) {
            eprintln!("Could not save history to .cynhistory: {:?}", e);
        }
    }

    Ok(())
}

fn prompt(state: &interpreter::ExecutionState) -> String {
    let cwd = state.runtime_state.working_directory.as_str().to_string();
    let cwd = cwd.to_string();

    format!("{} >> ", cwd,).to_string()
}
