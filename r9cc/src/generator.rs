use crate::ast::*;
use std::fs::File;
use std::io::prelude::*;

pub struct Generator {
}

impl Generator {
    pub fn gen(self, ast :Ast, output : &mut File) -> Result<(), std::io::Error> {
        match ast.value {
            AstKind::Num(n) => {
                writeln!(output, "  push {}", n)?;
                Ok(())
            },
            _ =>  {
                eprintln!("not implenetd");
                Ok(())
            },
        }
    }

}