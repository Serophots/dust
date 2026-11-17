use colored::Colorize as _;
use dust_ctxt::GblCtxt;
use dust_hir::{Block, Expr, Func, Krate, Let, Stmt};

use crate::SourceLabeller;

impl<'hir> SourceLabeller for &Func<'hir> {
    fn label(self, labels: &mut Vec<miette::LabeledSpan>) {
        todo!()
    }
}

pub fn print_hir(krate: &Krate, ctx: GblCtxt) {
    for &func in krate.funcs.iter() {
        let ident = ctx.symbols.resolve(func.ident.symbol).unwrap();
        let func_sig = format!("{} {}{}", "fn".magenta(), ident.blue(), "()".white());
        let details = format!("({:?})", func.id).white();

        println!("{:<20} {}", func_sig, details);

        print_block(func.block, ctx);

        println!("{}", "end".magenta());
        print!("\n")
    }
}

fn print_block(block: &Block, ctx: GblCtxt) {
    println!("{}", "{".white());
    for &stmt in block.stmts.iter() {
        print_stmt(*stmt, ctx);
    }

    if let Some(expr) = block.expr {
        print_expr(*expr, ctx);
    }
    println!("{}", "}".white());
}

fn print_stmt(stmt: Stmt, ctx: GblCtxt) {
    match stmt {
        Stmt::Let(r#let) => {
            print_let(r#let, ctx);
        }
        Stmt::Expr(expr) => {
            print_expr(*expr, ctx);
        }
    }
}

fn print_let(r#let: &Let, ctx: GblCtxt) {
    let ident = ctx.symbols.resolve(r#let.ident.symbol).unwrap();
    match r#let.expr {
        Some(expr) => {
            println!("{} {} {}", "let".magenta(), ident.blue(), "= {".white());
            print_expr(*expr, ctx);
            println!("{}", "};".white());
        }
        None => {
            println!("{} {}{}", "let".magenta(), ident.blue(), ";".white());
        }
    }
}

fn print_expr(expr: Expr, ctx: GblCtxt) {
    match expr {
        Expr::Call(call) => {
            println!("{:#?}", call);
        }
        Expr::Binary(binary) => {
            print_expr(*binary.lhs, ctx);
            print!(
                " {} ",
                match binary.op {
                    utils::BinaryOp::Add => "+",
                    utils::BinaryOp::Sub => "-",
                    utils::BinaryOp::Mul => "*",
                    utils::BinaryOp::Div => "/",
                    utils::BinaryOp::Equal => "==",
                    utils::BinaryOp::NotEqual => "!=",
                    utils::BinaryOp::Greater => ">",
                    utils::BinaryOp::GreaterEqual => ">=",
                    utils::BinaryOp::Lesser => "<",
                    utils::BinaryOp::LesserEqual => "<=",
                    utils::BinaryOp::And => "&&",
                    utils::BinaryOp::Or => "||",
                }
                .white()
            );

            print_expr(*binary.rhs, ctx);
        }
        Expr::Unary(unary) => {
            println!("{:#?}", unary);
        }
        Expr::Literal(literal) => match literal.lit {
            utils::Lit::Number(n) => print!("{}", n.to_string().white()),
            utils::Lit::String(symbol) => {
                let ident = ctx.symbols.resolve(*symbol).unwrap();
                println!("{}", ident.white());
            }
            utils::Lit::Bool(bool) => {
                print!(
                    "{}",
                    match bool {
                        true => "true",
                        false => "false",
                    }
                    .white()
                );
            }
            utils::Lit::Nil => {
                print!("{}", "nil".white());
            }
        },
        Expr::Assign => {}
        Expr::Local(ident) => {
            let ident = ctx.symbols.resolve(ident.symbol).unwrap();
            println!("{}", ident.white());
        }
        Expr::Func(func_expr) => {
            println!("{:#?}", func_expr);
        }
        Expr::Block(block) => {
            print_block(block, ctx);
        }
        Expr::If => {}
        Expr::Loop => {}
    }
}
