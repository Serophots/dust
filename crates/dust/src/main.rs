use std::fs;

use miette::{IntoDiagnostic, LabeledSpan, WrapErr};

use crate::{calculator::Calculator, cli::Args, lexer::Lexer};

mod calculator;
mod cli;
mod lexer;
mod ops;
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
        Some(cli::Command::Calculate { input }) => {
            let mut calculator = Calculator::new(&input);
            println!("{:?}", calculator.parse());
        }
        Some(cli::Command::Interpret { file }) => {
            arena.push(
                fs::read_to_string(&file)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("failed to read '{}'", file.display()))?,
            );
            let contents = arena.last().unwrap();

            let mut parser = Calculator::new(contents);
            println!("{:?}", parser.parse());
        }
        Some(cli::Command::Interactive) => {
            todo!()
        }
        None => todo!(),
    }

    Ok(())
}
