use std::io::{stdout, Write};

use crate::{codebase::*, compile::*, value::*};

pub fn add_std_lib(cb: &mut Codebase) {
    cb.insert(
        "print".into(),
        Function::new_builtin(&["seq", "message"], |stack| {
            let (seq, message) = stack.pop2();
            seq.seq()?;
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
            let (seq, message) = stack.pop2();
            seq.seq()?;
            colored::control::set_override(false);
            println!("{}", message);
            colored::control::unset_override();
            stack.push(Value::Seq);
            Ok(())
        }),
    );
    cb.insert(
        "let".into(),
        Function::new_builtin(&["val", "function"], |stack| Instr::Call(1).execute(stack)),
    );
}
