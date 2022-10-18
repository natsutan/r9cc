use crate::ast::*;
use std::fs::File;
use std::io::prelude::*;

use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct CodeGenError {
    pub err: String,
}

impl Error for CodeGenError {}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CodeGen: error :{}", self.err)
    }
}

struct GenCnt {
    pub depth:i64,
    pub label:i64,
}

impl GenCnt {
    pub fn depth_add(&mut self, d: u64) {
        self.depth += d as i64;
    }
    pub fn depth_sub(&mut self, d: u64) {
        self.depth -= d as i64;
    }
    pub fn label_up(&mut self) { self.label += 1 ;}
}



fn gen_stmt(node :&Ast, output :&mut File, dc: &mut GenCnt) -> Result<(), Box<dyn Error>>  {
    match &node.value {
        AstKind::UniOp {op, l} => {
            match op.value {
                UniOpKind::NdReturn => {
                    gen_expr(l, output, dc)?;
                    writeln!(output, "  jmp .L.return")?;
                    Ok(())
                }
                UniOpKind::NdExprStmt => gen_expr(&l, output, dc)
            }
        }
        AstKind::Block { body } => {
            for node in body.iter() {
                gen_stmt(node, output, dc)?;
            }
            Ok(())
        },
        AstKind::If_{cond, then, els} => {
            let c = dc.label;
            dc.label_up();

            gen_expr(cond, output, dc)?;
            writeln!(output, "  cmp $0, %rax")?;
            writeln!(output, "  je  .L.else.{:?}", c)?;
            gen_stmt(then, output, dc)?;
            writeln!(output, "  jmp .L.end.{:?}", c)?;
            writeln!(output, ".L.else.{:?}:", c)?;
            gen_stmt(els, output, dc)?;
            writeln!(output, ".L.end.{:?}:", c)?;
            Ok(())
        },
        AstKind::For{init, cond, inc, then} => {
            let c = dc.label;
            dc.label_up();

            gen_stmt(init, output, dc)?;
            writeln!(output, ".L.begin.{}:", c)?;
            if !is_empty_block(cond) {
                gen_expr(cond, output, dc)?;
                writeln!(output, "  cmp $0, %rax")?;
                writeln!(output, "  je  .L.end.{}", c)?;
            }
            gen_stmt(then, output, dc)?;

            if !is_empty_block(inc) {
                gen_expr(inc, output, dc)?;
            }
            writeln!(output, "  jmp .L.begin.{}", c)?;
            writeln!(output, ".L.end.{}:", c)?;

            Ok(())
        },

        _ => Err(Box::new(CodeGenError{err: format!("invalid statement")}))
    }
}


pub fn codegen(program :&Program, frame :&Frame, output :&mut File) -> Result<(),  Box<dyn Error>> {
    let mut dc = GenCnt {depth:0, label: 1};
    let stack_size = frame.len() * 8;

    //writeln!(output, ".intel_syntax noprefix")?;
    writeln!(output, ".globl main")?;
    writeln!(output, "main:")?;

    // Prologue
    writeln!(output, "  push %rbp")?;       //ベースポインタを保存
    writeln!(output, "  mov %rsp, %rbp")?; //ベースポインタに関数に入った時のスタックポインタを保存
    writeln!(output, "  sub ${}, %rsp", stack_size)?;  //変数の領域確保
    writeln!(output, "")?;

    match &program[0].value {
        AstKind::Block{body} => {
          for node in body.iter() {
              gen_stmt(node, output, &mut dc)?;
          }
        },
        _ => return Err(Box::new(CodeGenError{err: format!("Program must be block")})),
    }
    assert_eq!(dc.depth, 0);

    writeln!(output, ".L.return:")?;
    writeln!(output, "  mov %rbp, %rsp")?; //スタックポインタの復元
    writeln!(output, "  pop %rbp")?;        //ベースポインタの復元
    writeln!(output, "  ret")?;
    Ok(())
}

fn gen_addr(node: &Ast, output : &mut File) -> Result<(), Box<dyn Error>> {
    match &node.value {
        AstKind::LocalVar { name: _, offset } => {
            writeln!(output, "  lea {}(%rbp), %rax", -offset)?;
            Ok(())
        }
        _ => Err(Box::new(CodeGenError{err: format!("GEN: not an lvalue {:?}.", node.value)}))
    }
}


fn push(output : &mut File, dc :&mut GenCnt) -> Result<(), Box<dyn Error>>  {
    writeln!(output, "  push %rax")?;
    dc.depth_add(1);
    Ok(())
}

fn pop(s: &String, output : &mut File, dc :&mut GenCnt) -> Result<(), Box<dyn Error>> {
    writeln!(output, "  pop {}", s)?;
    dc.depth_sub(1);
    Ok(())
}



fn gen_expr(node :&Ast, output : &mut File, dc :&mut GenCnt) -> Result<(), Box<dyn Error>> {

    match node.value.clone() {
        AstKind::Num(n) => {
            writeln!(output, "  mov ${}, %rax", n)?;
            return Ok(());
        },
        AstKind::LocalVar { name: _s, offset: _ } => {
            gen_addr(node, output)?;
            writeln!(output, "  mov (%rax), %rax")?;
            return Ok(());
        }
        AstKind::BinOp { op, l, r } => {
            if op.value == BinOpKind::Assign {
                gen_addr(&l, output)?;
                push(output, dc)?;
                gen_expr(&r, output, dc)?;
                pop(&"%rdi".to_string(), output, dc)?;
                writeln!(output, "  mov %rax, (%rdi)")?;
                return Ok(())
            }
            gen_expr(&r, output, dc)?;
            push(output, dc)?;
            gen_expr(&l, output, dc)?;
            pop(&"%rdi".to_string(), output, dc)?;

            match op.value {
                BinOpKind::Add => writeln!(output, "  add %rdi, %rax")?,
                BinOpKind::Sub => writeln!(output, "  sub %rdi, %rax")?,
                BinOpKind::Mult => writeln!(output, "  imul %rdi, %rax")?,
                BinOpKind::Div => {
                    writeln!(output, "  cqo")?;
                    writeln!(output, "  idiv %rdi")?;
                },
                BinOpKind::Eq => {
                    writeln!(output, "  cmp %rdi, %rax")?;
                    writeln!(output, "  sete %al")?;
                    writeln!(output, "  movzb %al, %rax")?;
                }
                BinOpKind::Ne => {
                    writeln!(output, "  cmp %rdi, %rax")?;
                    writeln!(output, "  setne %al")?;
                    writeln!(output, "  movzb %al, %rax")?;
                }
                BinOpKind::Lt => {
                    writeln!(output, "  cmp %rdi, %rax")?;
                    writeln!(output, "  setl %al")?;
                    writeln!(output, "  movzb %al, %rax")?;
                }
                BinOpKind::Le => {
                    writeln!(output, "  cmp %rdi, %rax")?;
                    writeln!(output, "  setle %al")?;
                    writeln!(output, "  movzb %al, %rax")?;
                }
                _ => return Err(Box::new(CodeGenError { err: format!("GEN: Invalid Binop {:?}.", op) }))
            }
            Ok(())
        }
        _ => return  Err(Box::new(CodeGenError{err: format!("GEN: Invalid expression.")})),
    }
}

fn is_empty_block(node: &Ast) -> bool {
    match node.value.clone() {
        AstKind::Block{body}=> {
            body.len() == 0
        },
        _ => false
    }
}