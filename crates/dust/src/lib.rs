use dust_byt::Func;
use dust_byt_interpret::VirtualMachine;
use dust_ctxt::{GblCtxt, create_and_enter_ast_ctxt};
use dust_lexer::Lexer;
use miette::LabeledSpan;

use crate::compiler::Compiler as _;

mod args;
pub mod compiler;

pub use args::*;

pub enum Printer {
    AstLabel,
    AstTree,
    HirTree,
}

impl<'gcx> crate::compiler::Compiler<'gcx> for Printer {
    fn hook_ast<'ast, 'a>(
        &'a self,
        ast: &'ast dust_ast::Krate<'gcx, 'ast>,
    ) -> std::ops::ControlFlow<()> {
        use dust_ir_print::SourceLabeller;

        match self {
            Self::AstTree => {
                let mut labels = Vec::new();
                ast.root.label(&mut labels);

                println!(
                    "{:?}",
                    miette::miette!(labels = labels, "debug")
                        .with_source_code(ast.root.source.to_owned())
                );
            }
            Self::AstLabel => {
                println!("{:#?}", ast.root.items);
            }
            _ => {
                return std::ops::ControlFlow::Continue(());
            }
        }

        std::ops::ControlFlow::Break(())
    }

    fn hook_ast_lw<'hir, 'a>(
        &'a self,
        hir: &'hir dust_hir::Krate<'hir>,
    ) -> std::ops::ControlFlow<()> {
        match self {
            Self::HirTree => {
                println!("{:#?}", hir.main);
            }
            _ => {
                return std::ops::ControlFlow::Continue(());
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
        Command::PrintAst { input, tree: true } => {
            Printer::AstTree.run(&input, ctx)?;
        }
        Command::PrintAst { input, tree: false } => {
            Printer::AstLabel.run(&input, ctx)?;
        }
        Command::PrintHir { input } => {
            Printer::HirTree.run(&input, ctx)?;
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
            let chunk = Compiler.run(&input, ctx)?.unwrap();
            dust_byt_print::print_func(&chunk, ctx);
            let chunk = Func::from(&chunk);

            println!("---- interpretting!");

            let mut vm = VirtualMachine::new(&chunk);
            vm.exec_chunk(&chunk);

            dust_byt_print::print_stack(&vm);
        }
        Command::Run { input } => {
            Compiler.run(&input, ctx)?;
        }

        Command::Interpret { input } => todo!(),
    }

    Ok(())
}
