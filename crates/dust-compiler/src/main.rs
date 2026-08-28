use dust_ast::Parser;
use dust_ctxt::{CtxtRunner, GblCtx, create_and_enter_global_ctxt};
use dust_lexer::Lexer;
use miette::LabeledSpan;

use crate::args::{Args, Command};

mod args;
mod compiler;
mod lexer;
mod parser;

trait ArenaVec<T> {
    fn push_arena(&mut self, v: T) -> &T;
}

impl<T> ArenaVec<T> for Vec<T> {
    fn push_arena(&mut self, v: T) -> &T {
        self.push(v);
        self.last().unwrap()
    }
}

fn main() -> miette::Result<()> {
    create_and_enter_global_ctxt(|ctx| main_in_gbl_ctx(ctx))
}

fn main_in_gbl_ctx<'gcx>(ctx: GblCtx<'gcx>) -> miette::Result<()> {
    let args = <Args as clap::Parser>::parse();

    let mut arena = Vec::new();

    match args.cmd {
        Some(Command::Lex { input }) => {
            let contents = arena.push_arena(input.content()?);
            let lexer = Lexer::new(contents, ctx);

            return Err(miette::miette!(
                labels = lexer
                    .map(|token| {
                        let token = token.unwrap();
                        LabeledSpan::at(token.src, format!("{:?}", token.kind))
                    })
                    .collect::<Vec<_>>(),
                "debug"
            )
            .with_source_code(contents.clone()));
        }
        Some(Command::Parse { input, tree: false }) => {
            use dust_ast_print::LabelPrinter;

            let contents = arena.push_arena(input.content()?);
            let ast = Parser::new(contents, ctx).mod_file()?;

            let mut labels = Vec::new();
            ast.label(&mut labels);

            return Err(
                miette::miette!(labels = labels, "debug").with_source_code(contents.clone())
            );
        }
        Some(Command::Parse { input, tree: true }) => {
            let contents = arena.push_arena(input.content()?);
            let ast = Parser::new(contents, ctx).mod_file()?;

            println!("{:#?}", ast.items);
        }
        Some(Command::Calculate { input }) => {
            let contents = arena.push_arena(input.content()?);

            let mut parser = Parser::new(&contents, ctx);
            println!("{:?}", parser.arithmetic());
        }

        Some(Command::Compile { input }) => {
            compiler::Compiler { root_module: input }.run(ctx);
        }
        Some(Command::Run { input }) => {
            compiler::Compiler { root_module: input }.run(ctx);
        }

        _ => todo!(),
    }

    Ok(())
}
