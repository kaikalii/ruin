use std::io::{stdout, Write};

use crate::{codebase::*, value::*};

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
}
