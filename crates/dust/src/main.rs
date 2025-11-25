use std::{fs, path::PathBuf};

use clap::Subcommand;
use dust_sys::lexer::Lexer;
use dust_sys::parser::Parser;
use miette::{Context as _, IntoDiagnostic as _, LabeledSpan};

#[derive(clap::Parser)]
#[command(author, version, about)]
pub struct Args {
    #[command(subcommand)]
    pub cmd: Option<Command>,
}

#[derive(Subcommand)]
pub enum Command {
    /// Tokenize a .dst file
    Tokenize {
        /// Path of the .dst file to tokenize
        file: PathBuf,
    },
    /// Interpret a .dst file
    Interpret {
        /// Path of the .dst file to interpret
        file: PathBuf,
    },
    /// Start an interactive dust terminal
    Interactive,
    /// Evaluate a static expression,
    /// e.g.
    /// `1 + 1 == 2` -> TRUE
    /// `1 + 1 < 2` -> FALSE
    /// `1 + 1 == 2 == false` -> FALSE
    Calculate { input: String },
}

fn main() -> miette::Result<()> {
    let args = <Args as clap::Parser>::parse();

    let mut arena = Vec::new();

    match args.cmd {
        Some(Command::Tokenize { file }) => {
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
        Some(Command::Calculate { .. }) => {
            // let mut calculator = Calculator::new(&input);
            // println!("{:?}", calculator.parse());
        }
        Some(Command::Interpret { file }) => {
            arena.push(
                fs::read_to_string(&file)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("failed to read '{}'", file.display()))?,
            );
            let contents = arena.last().unwrap();

            let mut parser = Parser::new(contents);

            for statement in parser {
                println!("{:?}", statement?);
            }
        }
        Some(Command::Interactive) => {
            todo!()
        }
        None => todo!(),
    }

    Ok(())
}
