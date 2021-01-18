mod codebase;
mod eval;
mod lexer;
mod num;
mod parse;
mod value;

use std::io::{stdin, stdout, BufRead, Write};

use colored::Colorize;

use codebase::Codebase;
use parse::{parse, Command};

fn main() {
    color_backtrace::install();

    let mut cb = Codebase::default();
    print_prompt();
    for input in stdin().lock().lines().filter_map(Result::ok) {
        match parse(&input) {
            Ok(command) => match command {
                Command::Assignment(ass) => {
                    cb.insert(ass.ident, ass.expr);
                    cb.eval_all();
                    cb.print(10);
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
