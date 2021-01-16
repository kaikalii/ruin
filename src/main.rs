mod lexer;
mod num;

use std::io::{stdin, stdout, BufRead, Write};

use lexer::lex;

fn main() {
    print_prompt();
    for input in stdin().lock().lines().filter_map(Result::ok) {
        match lex(&input) {
            Ok(tokens) => {
                for token in tokens {
                    println!("{:?}", token.data);
                }
                println!();
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
