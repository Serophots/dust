use dust_ast::Parser;
use dust_ctxt::{GblCtx, create_and_enter_ast_ctxt};
use dust_lexer::Lexer;
use miette::LabeledSpan;

use crate::compiler::Compiler as _;

mod args;
pub mod compiler;

pub use args::*;

pub struct Compiler;

impl<'gcx> crate::compiler::Compiler<'gcx> for Compiler {}

pub fn main_in_gbl_ctx<'gcx>(args: Args, ctx: GblCtx<'gcx>) -> miette::Result<()> {
    match args.cmd {
        Command::Lex { input } => {
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
        Command::Parse { input, tree: false } => {
            create_and_enter_ast_ctxt(ctx, |ctx| {
                use dust_ast_print::LabelPrinter;

                let contents = ctx.arena.alloc(input.content()?);
                let ident = ctx.gcx.symbols.get_or_intern("parse");
                let ast = Parser::new(contents, vec![ident], ctx).parse(ctx)?;

                let mut labels = Vec::new();
                ast.label(&mut labels);

                Err(miette::miette!(labels = labels, "debug").with_source_code(contents.clone()))
            })?;
        }
        Command::Parse { input, tree: true } => {
            create_and_enter_ast_ctxt(ctx, |ctx| -> Result<_, miette::Report> {
                let contents = ctx.arena.alloc(input.content()?);
                let ident = ctx.gcx.symbols.get_or_intern("parse");
                let ast = Parser::new(contents, vec![ident], ctx).parse(ctx)?;

                println!("{:#?}", ast.items);

                Ok(())
            })?;
        }
        Command::Calculate { input } => {
            create_and_enter_ast_ctxt(ctx, |ctx| -> Result<_, miette::Report> {
                let contents = ctx.arena.alloc(input.content()?);

                let mut parser =
                    Parser::new(&contents, vec![ctx.gcx.symbols.get_or_intern("calc")], ctx);
                println!("{:?}", parser.expr(ctx));

                Ok(())
            })?;
        }

        Command::Compile { input } => {
            Compiler.run(&input, ctx)?;
        }
        Command::Run { input } => {
            Compiler.run(&input, ctx)?;
        }

        Command::Interpret { input } => todo!(),
    }

    Ok(())
}
