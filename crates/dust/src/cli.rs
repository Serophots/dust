use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser)]
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
