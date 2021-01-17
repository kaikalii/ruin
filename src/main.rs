mod codebase;
mod eval;
mod lexer;
mod num;
mod parse;
mod value;

use std::io::{stdin, stdout, BufRead, Write};

use colored::Colorize;

use codebase::Env;
use parse::{parse, Command};

fn main() {
    let mut env = Env::default();
    print_prompt();
    for input in stdin().lock().lines().filter_map(Result::ok) {
        match parse(&input) {
            Ok(command) => match command {
                Command::Assignment(ass) => {
                    env.assign(ass);
                    env.eval_all();
                    env.print(10);
                }
            },
            Err(e) => println!("Error: {}\n", e),
        }
        print_prompt();
    }
}

fn print_prompt() {
    print!("{}", "\r> ".bright_yellow());
    let _ = stdout().flush();
}
