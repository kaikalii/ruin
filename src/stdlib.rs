use std::io::{stdout, Write};

use crate::{codebase::*, eval::*, value::*};

pub fn add_std_lib(cb: &mut Codebase) {
    cb.insert(
        "print".into(),
        Function::new_builtin(&["seq", "message"], |state| {
            state["seq"].seq()?;
            colored::control::set_override(false);
            print!("{}", state["message"]);
            let _ = stdout().flush();
            colored::control::unset_override();
            Ok(state["seq"].clone())
        }),
    );
    cb.insert(
        "println".into(),
        Function::new_builtin(&["seq", "message"], |state| {
            state["seq"].seq()?;
            colored::control::set_override(false);
            println!("{}", state["message"]);
            colored::control::unset_override();
            Ok(state["seq"].clone())
        }),
    );
    cb.insert(
        "let".into(),
        Function::new_builtin(&["val", "function"], |state| {
            let function = state["function"].clone();
            eval_function(&function, &function, vec![state["val"].clone()], state)
        }),
    );
}
