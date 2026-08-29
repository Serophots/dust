use dust_ast::Parser;
use dust_ctxt::{CtxtRunner, GblCtx, create_and_enter_ast_ctxt, create_and_enter_global_ctxt};
use dust_lexer::Lexer;
use miette::LabeledSpan;

use crate::args::{Args, Command};

mod args;
mod compiler;
mod lexer;
mod parser;

fn main() -> miette::Result<()> {
    create_and_enter_global_ctxt(|ctx| main_in_gbl_ctx(ctx))
}

fn main_in_gbl_ctx<'gcx>(ctx: GblCtx<'gcx>) -> miette::Result<()> {
    let args = <Args as clap::Parser>::parse();

    match args.cmd {
        Some(Command::Lex { input }) => {
            create_and_enter_ast_ctxt(ctx, |ctx| {
                let contents = ctx.arena.alloc(input.content()?);
                let lexer = Lexer::new(contents, ctx);

                Err(miette::miette!(
                    labels = lexer
                        .map(|token| {
                            let token = token.unwrap();
                            LabeledSpan::at(token.span, format!("{:?}", token.kind))
                        })
                        .collect::<Vec<_>>(),
                    "debug"
                )
                .with_source_code(contents.clone()))
            })?;
        }
        Some(Command::Parse { input, tree: false }) => {
            create_and_enter_ast_ctxt(ctx, |ctx| {
                use dust_ast_print::LabelPrinter;

                let contents = ctx.arena.alloc(input.content()?);
                let ast = Parser::new(contents, ctx).mod_file(ctx)?;

                let mut labels = Vec::new();
                ast.label(&mut labels);

                Err(miette::miette!(labels = labels, "debug").with_source_code(contents.clone()))
            })?;
        }
        Some(Command::Parse { input, tree: true }) => {
            create_and_enter_ast_ctxt(ctx, |ctx| -> Result<_, miette::Report> {
                let contents = ctx.arena.alloc(input.content()?);
                let ast = Parser::new(contents, ctx).mod_file(ctx)?;

                println!("{:#?}", ast.items);

                Ok(())
            })?;
        }
        Some(Command::Calculate { input }) => {
            create_and_enter_ast_ctxt(ctx, |ctx| -> Result<_, miette::Report> {
                let contents = ctx.arena.alloc(input.content()?);

                let mut parser = Parser::new(&contents, ctx);
                println!("{:?}", parser.arithmetic(ctx));

                Ok(())
            })?;
        }

        Some(Command::Compile { input }) => {
            compiler::Compiler { root_module: input }.run(ctx)?;
        }
        Some(Command::Run { input }) => {
            compiler::Compiler { root_module: input }.run(ctx)?;
        }

        _ => todo!(),
    }

    Ok(())
}
