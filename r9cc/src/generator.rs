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



fn gen_stmt(node :&Ast, func_name: &String, locals: &Vec<Obj>, output :&mut File, dc: &mut GenCnt) -> Result<(), Box<dyn Error>>  {
    match node {
        AstKind::UniOp(uniop) => {
            match uniop.op {
                UniOpKind::NdReturn => {
                    gen_expr(&uniop.l, locals, output, dc)?;
                    writeln!(output, "  jmp .L.return.{}", func_name)?;
                }
                UniOpKind::NdExprStmt => gen_expr(&uniop.l, locals, output, dc)?,
                _ => return Err(Box::new(CodeGenError{err: format!("invalid statement")})),
            }
            Ok(())
        }
        AstKind::Block { body } => {
            for node in body.iter() {
                gen_stmt(node, func_name, locals, output, dc)?;
            }
            Ok(())
        },
        AstKind::If_{cond, then, els} => {
            let c = dc.label;
            dc.label_up();

            gen_expr(cond, locals, output, dc)?;
            writeln!(output, "  cmp $0, %rax")?;
            writeln!(output, "  je  .L.else.{:?}", c)?;
            gen_stmt(then, func_name, locals,output, dc)?;
            writeln!(output, "  jmp .L.end.{:?}", c)?;
            writeln!(output, ".L.else.{:?}:", c)?;
            gen_stmt(els, func_name, locals, output, dc)?;
            writeln!(output, ".L.end.{:?}:", c)?;
            Ok(())
        },
        AstKind::For{init, cond, inc, then} => {
            let c = dc.label;
            dc.label_up();

            if !is_empty_block(init) {
                gen_stmt(init, func_name, locals, output, dc)?;
            }

            writeln!(output, ".L.begin.{}:", c)?;
            if !is_empty_block(cond) {
                gen_expr(cond, locals, output, dc)?;
                writeln!(output, "  cmp $0, %rax")?;
                writeln!(output, "  je  .L.end.{}", c)?;
            }
            gen_stmt(then, func_name, locals, output, dc)?;

            if !is_empty_block(inc) {
                gen_expr(inc, locals, output, dc)?;
            }
            writeln!(output, "  jmp .L.begin.{}", c)?;
            writeln!(output, ".L.end.{}:", c)?;

            Ok(())
        },

        _ => Err(Box::new(CodeGenError{err: format!("invalid statement")}))
    }
}


pub fn codegen(program: &mut Program, globals :&Frame, output :&mut File) -> Result<(),  Box<dyn Error>> {
    let mut dc = GenCnt {depth:0, label: 1};

    assign_lver_offset(program);
    emit_data(globals, output);
    writeln!(output, "");
    for function in program {
        if !function.is_func {
            continue;
        }

        writeln!(output, ".globl {}", function.name)?;
        writeln!(output, "{}:", function.name)?;
        // Prologue
        writeln!(output, "  push %rbp")?;       //ベースポインタを保存
        writeln!(output, "  mov %rsp, %rbp")?; //ベースポインタに関数に入った時のスタックポインタを保存
        writeln!(output, "  sub ${}, %rsp", function.stack_size)?;  //変数の領域確保
        writeln!(output, "")?;

        //引数の処理
        for (i, val) in function.locals.iter().enumerate() {
            writeln!(output, "# val.offset {}", val.offset)?;
            writeln!(output, "  mov {}, {}(%rbp)", argreg(i), val.offset)?;
        }

        gen_stmt(&function.body, &function.name, &function.locals, output, &mut dc)?;
        assert_eq!(dc.depth, 0);
        writeln!(output, ".L.return.{}:", function.name)?;
        writeln!(output, "  mov %rbp, %rsp")?; //スタックポインタの復元
        writeln!(output, "  pop %rbp")?;        //ベースポインタの復元
        writeln!(output, "  ret")?;
        writeln!(output, "")?;

    }

    Ok(())
}

fn argreg(i: usize) -> String {
    let table = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
    table[i].to_string()
}

fn assign_lver_offset(program :&mut Program) {
    for f in program {
        if !f.is_func {
            continue;
        }

        let mut offset = 0;
        for v in &mut f.locals {
            offset += v.ntype.size as i64;
            v.offset = -offset;
        }
        f.stack_size = aligin_to(offset, 16);
    }
}

fn emit_data(globals :&Frame, output :&mut File) -> Result<(),  Box<dyn Error>> {
    for var in globals {
        writeln!(output, "  .data")?;
        writeln!(output, "  .globl {}", var.name)?;
        writeln!(output, "{}:", var.name)?;
        writeln!(output, "  .zero {}", var.ntype.size)?;
    }
    Ok(())
}

fn aligin_to(n: i64, align :i64) -> u64 {
    let x:f64 = (n + align - 1) as f64 / align as f64;
    x.floor() as u64 * align as u64
}

fn gen_addr(node: &Ast, locals: &Vec<Obj>, output : &mut File, dc :&mut GenCnt) -> Result<(), Box<dyn Error>> {
    match node {
        AstKind::LocalVar { name, ntype: _, offset: _ } => {
            let mut offset = 0;
            for v in locals {
                //local variable
                if v.name == *name {
                    offset = v.offset;
                    writeln!(output, "  lea {}(%rbp), %rax", offset)?;
                    return Ok(());
                }
            }
            //global variable
            writeln!(output, "  lea {}(%rip), %rax", name)?;

            Ok(())
        }
        AstKind::UniOp(uniop)=> {
            match uniop.op {
                UniOpKind::Deref => {
                    gen_expr(&uniop.l, locals, output, dc)?;
                    Ok(())
                },
                _ => Err(Box::new(CodeGenError{err: format!("GEN: not an lvalue {:?}.", node)}))
            }
        }
        _ => Err(Box::new(CodeGenError{err: format!("GEN: not an lvalue {:?}.", node)}))
    }
}

fn load(node: &Ast, output : &mut File, _dc :&mut GenCnt) -> Result<(), Box<dyn Error>> {
    match node {
        AstKind::LocalVar { name: _, ntype, offset: _ } => {
            if ntype.kind == NodeTypeKind::Array {
                return Ok(())
            }
        }
        AstKind::UniOp(uniop) => {
            if uniop.ntype.kind == NodeTypeKind::Array {
                return Ok(())
            }
        },
        _ => ()
    }
    writeln!(output, "  mov (%rax), %rax")?;

    Ok(())
}

fn store(output : &mut File, dc :&mut GenCnt) -> Result<(), Box<dyn Error>>  {
    pop(&"%rdi".to_string(), output, dc)?;
    writeln!(output, "  mov %rax, (%rdi)")?;
    Ok(())
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



fn gen_expr(node :&Ast, locals: &Vec<Obj>, output : &mut File, dc :&mut GenCnt) -> Result<(), Box<dyn Error>> {

    match node.clone() {
        AstKind::Num{n, ntype:_}=> {
            writeln!(output, "  mov ${}, %rax", n)?;
            return Ok(());
        },
        AstKind::LocalVar { name: _s, ntype: _, offset: _ } => {
            gen_addr(node, locals, output, dc)?;
            load(node, output, dc)?;
            return Ok(());
        }
        AstKind::BinOp(binop) => {
            if binop.op == BinOpKind::Assign {
                writeln!(output, "# assign")?;
                gen_addr(&binop.l, locals, output, dc)?;
                push(output, dc)?;
                gen_expr(&binop.r, locals, output, dc)?;
                store(output, dc)?;
                writeln!(output, "")?;
                return Ok(())
            }
            gen_expr(&binop.r, locals, output, dc)?;
            push(output, dc)?;
            gen_expr(&binop.l, locals, output, dc)?;
            pop(&"%rdi".to_string(), output, dc)?;

            writeln!(output, "# op {:?}", binop.op)?;

            match binop.op {
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
                _ => return Err(Box::new(CodeGenError { err: format!("GEN: Invalid Binop {:?}.", binop) }))
            }
            Ok(())
        }
        AstKind::UniOp(uniop) => {
            match uniop.op {
                UniOpKind::Deref => {
                    writeln!(output, "# deref")?;
                    gen_expr(&uniop.l, locals, output, dc)?;
                    writeln!(output, "# deref load")?;
                    load(&node, output, dc)?;

                }
                UniOpKind::Addr => {
                    writeln!(output, "# addr")?;
                    gen_addr(&uniop.l, locals, output, dc)?;
                }
                _ => return  Err(Box::new(CodeGenError{err: format!("GEN: Invalid expression.")})),
            }
            Ok(())
        } ,
        AstKind::FunCall{funcname, args, ntype:_} => {
            let mut nargs = 0;
            for arg in args.iter() {
                gen_expr(arg, locals, output, dc)?;
                push(output, dc)?;
                nargs += 1;
            }

            while nargs > 0 {
                nargs -= 1;
                pop(&argreg(nargs), output, dc)?;
            }

            writeln!(output, "  mov $0, %rax")?;
            writeln!(output, "  call {}", funcname)?;
            Ok(())
        }
        _ => return  Err(Box::new(CodeGenError{err: format!("GEN: Invalid expression.")})),
    }
}

fn is_empty_block(node: &Ast) -> bool {
    match node.clone() {
        AstKind::Block{body}=> {
            body.len() == 0
        },
        _ => false
    }
}
