use crate::ast::*;
use std::fs::File;
use std::io::prelude::*;

pub struct Generator {
}

pub fn gen(ast :&Ast, output : &mut File) -> Result<(), std::io::Error> {


    match ast.value.clone() {
        AstKind::Num(n) => {
            writeln!(output, "  push {}", n)?;
            Ok(())
        },
        AstKind::BinOp{op, l , r} => {
            gen(&l, output)?;
            gen(&r, output)?;

            writeln!(output, "  pop rdi")?;
            writeln!(output, "  pop rax")?;


            match op.value {
                BinOpKind::Add => writeln!(output, "  add rax, rdi")?,
                BinOpKind::Sub => writeln!(output, "  sub rax, rdi")?,
                BinOpKind::Mult => writeln!(output, "  imul rax, rdi")?,
                BinOpKind::Div => {
                    writeln!(output, "  cqo")?;
                    writeln!(output, "  idiv rdi")?;
                },
                _ => eprintln!("not implemented Binop {:?}", op.value),
            }

            writeln!(output, "  push rax")?;

            Ok(())
        }
        _ =>  {
            eprintln!("not implemented");
            Ok(())
        },
    }
}
