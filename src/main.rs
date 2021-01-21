mod codebase;
mod eval;
mod lexer;
mod num;
mod parse;
mod stdlib;
mod value;

use std::{
    fs,
    io::{self, stdin, stdout, BufRead, Write},
    iter::once,
    sync::Arc,
};

use clap::Clap;
use colored::Colorize;

use codebase::Codebase;
use eval::*;
use parse::{parse, Command, Path};

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
                cb.as_mut().insert(ass.path, ass.expr);
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
                let val = expr.eval(EvalState::new(cb.clone(), once(Path::GLOBAL).collect()));
                println!("{}", val);
                println!();
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
    Load { path: Option<Path> },
    Run { path: Option<Path> },
}

fn load(cb: &mut Arc<Codebase>, path: Option<Path>, eval: bool) -> io::Result<()> {
    let path = path.unwrap_or_else(|| "main".into());
    let text = fs::read_to_string(path.as_path_buf())?;
    for line in text.lines() {
        handle_input(line, cb, false);
    }
    if eval {
        cb.eval_all();
        cb.print(10);
    }
    Ok(())
}

fn run(cb: &mut Arc<Codebase>, path: Option<Path>) {
    let path = path.unwrap_or_else(|| "main".into());
    println!();
    if let Some(_val) = cb.get(&path) {
        todo!()
    } else {
        println!("Unknown function: {}", path)
    }
    println!();
}
