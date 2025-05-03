use std::{env, path::PathBuf};

use crate::{
    interpreter::{rte, RuntimeError, RuntimeState, Value},
    utils::parse_args,
};

pub fn len(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return rte("Usage: lines <string>");
    }

    let value = args[0].clone();

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

pub fn readline(_values: Vec<Value>) -> Result<Value, RuntimeError> {
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .or(rte("Failed to read line"))?;

    Ok(Value::String(buffer.trim().to_string()))
}

pub fn lines(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return rte("Usage: lines <string>");
    }

    let value = args[0].clone();

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

    if let Err(e) = std::env::set_current_dir(path_pathbuf) {
        return rte(format!("{:?}", e));
    }

    state.working_directory.set(&pwd()?.as_string()?)?;

    Ok(Value::Void)
}

fn ls(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let args = parse_args(args)?;

    let path = if !args.files.is_empty() {
        args.files[0].clone()
    } else {
        pwd()?.as_string()?
    };

    let path_pathbuf = PathBuf::from(path.clone());

    if !path_pathbuf.exists() {
        return rte("Path does not exist");
    }

    if args.long_flags.contains("array") {
        let entries = std::fs::read_dir(path_pathbuf)
            .or(rte("Failed to read directory"))?
            .filter_map(|entry| entry.ok())
            .map(|entry| Value::String(entry.file_name().to_string_lossy().to_string()))
            .collect::<Vec<_>>();

        return Ok(Value::Array(entries));
    }

    let all = args.short_flags.contains("a") || args.long_flags.contains("all");

    let files = std::fs::read_dir(&path_pathbuf)
        .or(rte("Failed to read directory"))?
        .filter_map(|entry| entry.ok())
        .filter(|e| e.path().is_file())
        .map(|entry| entry.file_name().to_string_lossy().to_string());

    let dirs = std::fs::read_dir(&path_pathbuf)
        .or(rte("Failed to read directory"))?
        .filter_map(|entry| entry.ok())
        .filter(|e| e.path().is_dir())
        .map(|entry| {
            let mut name = entry.file_name().to_string_lossy().to_string();
            if entry.path().is_dir() {
                name.push('/');
            }
            name
        });

    let entries = dirs.chain(files)
        .filter(|name| !(!all && name.chars().next() == Some('.')))
        .collect::<Vec<_>>()
        .join("\n");

    Ok(Value::String(entries))
}

fn mkdir(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return rte("Usage: mkdir <path>");
    }

    let path = args[0].as_string()?;

    let path_pathbuf = PathBuf::from(path.clone());

    if path_pathbuf.exists() {
        return rte("Path already exists");
    }

    std::fs::create_dir(path_pathbuf).or(rte("Failed to create directory"))?;

    Ok(Value::Void)
}

fn cat(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let args = parse_args(args)?;

    if args.files.len() != 1 {
        return rte("Usage: cat <path>");
    }

    let file = args.files[0].clone();
    let path = PathBuf::from(file);

    if !path.exists() {
        return rte(format!("File {} does not exist", path.display()));
    }

    if !path.is_file() {
        return rte(format!("Path {} is not a file", path.display()));
    }

    let Ok(contents) = std::fs::read_to_string(path) else {
        return rte("Failled to read file");
    };

    Ok(Value::String(contents))
}

fn mkcd(args: Vec<Value>, state: &mut RuntimeState) -> Result<Value, RuntimeError> {
    mkdir(args.clone())?;
    cd(args, state)
}

fn rm(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let args = parse_args(args)?;

    if args.files.is_empty() {
        return rte("Usage: rm <path>");
    }

    let recurse = args.short_flags.contains("r") || args.long_flags.contains("recurse");
    // let force = args.short_flags.contains("f") || args.long_flags.contains("force");

    for path in &args.files {
        let path = PathBuf::from(path.clone());

        if !path.exists() {
            return rte(format!("Path {} does not exist", path.display()));
        }
    }

    let confirm = !args.long_flags.contains("no-confirm") && !args.short_flags.contains("n");

    if confirm {
        println!("Removing {}. Are you sure? (y/n)", args.files.join(" "));
        if readline(vec![])?.as_string()? != "y" {
            println!("Aborting");
            return Ok(Value::Void);
        }
    }

    for path in args.files {
        let path = PathBuf::from(path.clone());

        if path.is_dir() {
            if !recurse {
                return rte(format!(
                    "Path {} is a directory, use -r to recurse",
                    path.display()
                ));
            }

            std::fs::remove_dir_all(&path)
                .or(rte(format!("Failed to remove {}", path.display())))?;
        } else {
            std::fs::remove_file(&path).or(rte(format!("Failed to remove {}", path.display())))?;
        }
    }

    Ok(Value::Void)
}

pub fn touch(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let args = parse_args(args)?;

    for path in &args.files {
        let path = PathBuf::from(path.clone());

        std::fs::write(&path, "").or(rte(format!("Failed to create file {}", path.display())))?;
    }

    Ok(Value::Void)
}

pub fn assert(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return rte("Usage: assert <condition>");
    }

    let condition = args[0].as_bool()?;

    if !condition {
        return rte("Assertion failed!");
    }

    Ok(Value::Void)
}

pub fn pwd() -> Result<Value, RuntimeError> {
    Ok(Value::String(
        env::current_dir()
            .or(rte("Failed to get cwd"))?
            .to_string_lossy()
            .to_string(),
    ))
}

pub fn echo(args: Vec<Value>) -> Result<Value, RuntimeError> {
    Ok(Value::String(format!(
        "{}\n",
        args.iter()
            .map(|v| v.as_string())
            .collect::<Result<Vec<_>, _>>()?
            .join(" ")
    )))
}

static STEAM_LOCOMOTIVE: &str = "Steam Locomotive:
   _____                 . . . . . o o o o o
  __|[_]|__ ___________ _______    ____      o
 |[] [] []| [] [] [] [] [_____(__  ][]]_n_n__][.
_|________|_[_________]_[________]_|__|________)<
  oo    oo 'oo      oo ' oo    oo 'oo 0000---oo\\_
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
";

pub fn try_builtin(
    name: &str,
    args: Vec<Value>,
    state: &mut RuntimeState,
) -> Option<Result<Value, RuntimeError>> {
    Some(match name {
        "len" => len(args),
        "print" => print(args),
        "println" => println(args),
        "readline" => readline(args),
        "lines" => lines(args),
        "pwd" => pwd(),
        "cd" => cd(args, state),
        "assert" => assert(args),
        "ls" => ls(args),
        "mkdir" => mkdir(args),
        "cat" => cat(args),
        "mkcd" => mkcd(args, state),
        "touch" => touch(args),
        "rm" => rm(args),
        "echo" => echo(args),
        "sl" => Ok(Value::String(STEAM_LOCOMOTIVE.to_string())),
        _ => return None,
    })
}
