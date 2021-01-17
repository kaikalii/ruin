mod lexer;
mod num;
mod parse;

use std::io::{stdin, stdout, BufRead, Write};

use parse::parse;

fn main() {
    print_prompt();
    for input in stdin().lock().lines().filter_map(Result::ok) {
        match parse(&input) {
            Ok(command) => {
                println!("{}\n", command);
            }
            Err(e) => println!("Error: {}\n", e),
        }
        print_prompt();
    }
}

fn print_prompt() {
    print!("\r> ");
    let _ = stdout().flush();
}
