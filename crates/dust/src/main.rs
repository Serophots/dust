use std::fs;

use miette::{IntoDiagnostic, LabeledSpan, WrapErr};

use crate::{cli::Args, lexer::Lexer, parser::Parser};

mod cli;
mod lexer;
mod ops;
mod parser;
mod token;

fn main() -> miette::Result<()> {
    // init_logging();
    let args = <Args as clap::Parser>::parse();

    let mut arena = Vec::new();

    match args.cmd {
        Some(cli::Command::Tokenize { file }) => {
            arena.push(
                fs::read_to_string(&file)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("failed to read '{}'", file.display()))?,
            );
            let contents = arena.last().unwrap();
            let lexer = Lexer::new(contents);

            return Err(miette::miette!(
                labels = lexer
                    .map(|token| {
                        let token = token.unwrap();
                        LabeledSpan::at(token.src, format!("{:?}", token))
                    })
                    .collect::<Vec<_>>(),
                "debug"
            )
            .with_source_code(contents.clone()));
        }
        Some(cli::Command::Interpret { file }) => {
            arena.push(
                fs::read_to_string(&file)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("failed to read '{}'", file.display()))?,
            );
            let contents = arena.last().unwrap();

            let mut parser = Parser::new(contents);
            println!("{:?}", parser.equality());
        }
        Some(cli::Command::Interactive) => {
            todo!()
        }
        None => todo!(),
    }

    Ok(())
}
