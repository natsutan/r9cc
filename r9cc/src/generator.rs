use crate::ast::*;
use std::fs::File;
use std::io::prelude::*;

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
                BinOpKind::Eq => {
                    writeln!(output, "  cmp rax, rdi\n")?;
                    writeln!(output, "  sete al\n")?;
                    writeln!(output, "  movzb rax, al\n")?;
                }
                BinOpKind::Ne => {
                    writeln!(output, "  cmp rax, rdi\n")?;
                    writeln!(output, "  setne al\n")?;
                    writeln!(output, "  movzb rax, al\n")?;
                }
                BinOpKind::Lt => {
                    writeln!(output, "  cmp rax, rdi\n")?;
                    writeln!(output, "  setl al\n")?;
                    writeln!(output, "  movzb rax, al\n")?;
                }
                BinOpKind::Le => {
                    writeln!(output, "  cmp rax, rdi\n")?;
                    writeln!(output, "  setle al\n")?;
                    writeln!(output, "  movzb rax, al\n")?;
                }

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
