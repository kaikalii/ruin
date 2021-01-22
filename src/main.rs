mod ast;
mod codebase;
mod compile;
mod lexer;
mod num;
mod parse;
mod stdlib;
mod value;

use std::{
    fs,
    io::{self, stdin, stdout, BufRead, Write},
    iter::once,
    path::PathBuf,
    sync::Arc,
};

use clap::Clap;
use colored::Colorize;

use ast::Command;
use codebase::Codebase;
use compile::*;
use parse::parse;
use value::Value;

fn main() {
    color_backtrace::install();

    let mut cb = Codebase::default();
    stdlib::add_std_lib(&mut cb);
    let mut cb = Arc::new(cb);
    let _ = load(&mut cb, None, false);
    cb.eval_all();
    cb.print(usize::MAX);
    print_prompt();
    for input in stdin().lock().lines().filter_map(Result::ok) {
        handle_input(&input, &mut cb, true);
        print_prompt();
    }
}

fn handle_input(input: &str, cb: &mut Arc<Codebase>, eval: bool) {
    let input = input.trim();
    match parse(input.as_bytes()) {
        Ok(command) => match command {
            Command::Assignment(ass) => {
                cb.as_mut().insert(ass.ident, ass.expr);
                if eval {
                    cb.eval_all();
                    cb.print(10);
                }
            }
            Command::Command => {
                let args = once("ruin").chain(input[1..].split_whitespace());
                match App::try_parse_from(args) {
                    Ok(App::Load { path }) => {
                        if load(cb, path.clone(), true).is_err() {
                            if let Some(path) = path {
                                println!("Unable to open file with path {}", path)
                            } else {
                                println!("Unable to open main file")
                            }
                        }
                    }
                    Ok(App::Run { path }) => run(cb, path),
                    Err(e) => println!("{}", e),
                }
            }
            Command::Eval(expr) => {
                println!();
                let val = expr.eval(&mut CompileState::new(cb.clone()), true);
                println!("{}", val);
                println!();
            }
            Command::FunctionDecl(decl) => {
                cb.as_mut().insert_function_decl(decl);
                if eval {
                    cb.eval_all();
                    cb.print(10);
                }
            }
        },
        Err(e) => println!("Error: {}\n", e),
    }
}

fn print_prompt() {
    print!("{}", "\r> ".bright_yellow());
    let _ = stdout().flush();
}

#[derive(Clap)]
enum App {
    Load { path: Option<String> },
    Run { path: Option<String> },
}

fn load(cb: &mut Arc<Codebase>, path: Option<String>, eval: bool) -> io::Result<()> {
    let path = PathBuf::from(path.unwrap_or_else(|| "main".into())).with_extension("ruin");
    let text = fs::read_to_string(path)?;
    for line in text.lines() {
        handle_input(line, cb, false);
    }
    if eval {
        cb.eval_all();
        cb.print(10);
    }
    Ok(())
}

fn run(cb: &mut Arc<Codebase>, path: Option<String>) {
    let path = path.unwrap_or_else(|| "main".into());
    println!();
    let function = eval_ident(cb, &path, true);
    if let Err(e) = eval_function(function, vec![Value::Seq]).ok() {
        println!("{}", e);
    }
    println!();
}
