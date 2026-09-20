use dust_ctxt::{GblCtxt, create_and_enter_ast_ctxt};
use dust_lexer::Lexer;
use miette::LabeledSpan;

use crate::compiler::Compiler as _;

mod args;
pub mod compiler;

pub use args::*;

/// An implementation of the compiler parses
/// the AST then stops and prints it.
pub struct Parser {
    tree: bool,
}

impl<'gcx> crate::compiler::Compiler<'gcx> for Parser {
    fn hook_ast<'ast, 'a>(
        &'a self,
        ast: &'ast dust_ast::Krate<'gcx, 'ast>,
    ) -> std::ops::ControlFlow<()> {
        use dust_ast_print::LabelPrinter;

        match self.tree {
            false => {
                let mut labels = Vec::new();
                ast.root.label(&mut labels);

                println!(
                    "{:?}",
                    miette::miette!(labels = labels, "debug")
                        .with_source_code(ast.root.source.to_owned())
                );
            }
            true => {
                println!("{:#?}", ast.root.items);
            }
        }

        std::ops::ControlFlow::Break(())
    }
}

pub struct Compiler;

impl<'gcx> crate::compiler::Compiler<'gcx> for Compiler {}

pub fn main_in_gbl_ctx<'gcx>(args: Args, ctx: GblCtxt<'gcx>) -> miette::Result<()> {
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
        Command::Parse { input, tree } => {
            Parser { tree }.run(&input, ctx)?;
        }
        Command::Calculate { input } => {
            create_and_enter_ast_ctxt(ctx, |ctx| -> Result<_, miette::Report> {
                let contents = ctx.arena.alloc(input.content()?);

                let mut parser = dust_ast::Parser::new(
                    &contents,
                    vec![ctx.gcx.symbols.get_or_intern("calc")],
                    ctx,
                );
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
