use dust_ast::Parser;
use dust_lexer::Lexer;
use miette::LabeledSpan;

use crate::args::{Args, Command};

mod args;

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
    let args = <Args as clap::Parser>::parse();

    let mut arena = Vec::new();

    match args.cmd {
        Some(Command::Lex { input }) => {
            let contents = arena.push_arena(input.read()?);
            let lexer = Lexer::new(contents);

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
        Some(Command::Parse { input }) => {
            use dust_ast_print::LabelPrinter;

            let contents = arena.push_arena(input.read()?);
            let ast = Parser::new(contents).mod_file()?;

            let mut labels = Vec::new();
            ast.kind.label(&mut labels);

            return Err(
                miette::miette!(labels = labels, "debug").with_source_code(contents.clone())
            );
        }
        Some(Command::Calculate { input }) => {
            let contents = arena.push_arena(input.read()?);

            let mut parser = Parser::new(&contents);
            println!("{:?}", parser.arithmetic());
        }
        Some(Command::Run { input }) => {
            let contents = arena.push_arena(input.read()?);
            let parser = Parser::new(contents);

            // for statement in parser {
            //     println!("{:?}", statement?);
            // }
        }
        Some(Command::Interactive) | None => {
            println!("Dust Interactive");
            todo!()
        }
        _ => todo!(),
    }

    Ok(())
}
