use std::io::{stdout, Write};

use crate::{codebase::*, value::*};

pub fn add_std_lib(cb: &mut Codebase) {
    cb.insert(
        "print".into(),
        Function::new_builtin(&["seq", "message"], |stack| {
            stack.pop().seq()?;
            let message = stack.pop();
            colored::control::set_override(false);
            print!("{}", message);
            let _ = stdout().flush();
            colored::control::unset_override();
            stack.push(Value::Seq);
            Ok(())
        }),
    );
    cb.insert(
        "println".into(),
        Function::new_builtin(&["seq", "message"], |stack| {
            stack.pop().seq()?;
            let message = stack.pop();
            colored::control::set_override(false);
            println!("{}", message);
            colored::control::unset_override();
            stack.push(Value::Seq);
            Ok(())
        }),
    );
    cb.insert(
        "let".into(),
        Function::new_builtin(&["val", "function"], |_| todo!()),
    );
}
