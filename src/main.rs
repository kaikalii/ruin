mod codebase;
mod eval;
mod jit;
mod lexer;
mod num;
mod parse;
mod value;

use std::{
    fs,
    io::{stdin, stdout, BufRead, Write},
    iter::once,
};

use clap::Clap;
use colored::Colorize;

use codebase::Codebase;
use parse::{parse, Command, Path};

fn main() {
    color_backtrace::install();

    let mut cb = Codebase::default();
    print_prompt();
    for input in stdin().lock().lines().filter_map(Result::ok) {
        handle_input(&input, &mut cb, true);
        print_prompt();
    }
}

fn handle_input(input: &str, cb: &mut Codebase, eval: bool) {
    let input = input.trim();
    match parse(input.as_bytes()) {
        Ok(command) => match command {
            Command::Assignment(ass) => {
                cb.insert(ass.ident, ass.expr);
                if eval {
                    cb.eval_all();
                    cb.print(10);
                }
            }
            Command::Command => {
                let args = once("ruin").chain(input[1..].split_whitespace());
                match App::try_parse_from(args) {
                    Ok(App::Load { path }) => match fs::read_to_string(path.as_path_buf()) {
                        Ok(text) => {
                            for line in text.lines() {
                                handle_input(line, cb, false);
                            }
                            if eval {
                                cb.eval_all();
                                cb.print(10);
                            }
                        }
                        Err(_) => println!("Unable to open file with path {}", path),
                    },
                    Err(e) => println!("{}", e),
                }
            }
            Command::Eval(expr) => {
                println!();
                match expr.eval(cb, "") {
                    Ok(val) => println!("{}", val),
                    Err(e) => println!("{}", e.to_string().red()),
                }
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
    Load { path: Path },
}
