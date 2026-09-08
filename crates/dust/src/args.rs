use camino::Utf8PathBuf;
use utils::{ByteSource, TextSource};

#[derive(clap::Parser)]
#[command(author, version, about)]
pub struct Args {
    #[command(subcommand)]
    pub cmd: Option<Command>,
}

#[derive(clap::Subcommand)]
pub enum Command {
    /// Tokenize a source file
    Lex { input: TextSource },
    /// Parse a source file into Abstract Syntax Tree
    Parse {
        input: TextSource,
        #[arg(long)]
        tree: bool,
    },
    /// Use dusts' parser as a comp-time calculator
    /// to evaluate static expressions from a text
    /// source e.g.
    /// `1 + 1 == 2` -> TRUE
    /// `1 + 1 < 2` -> FALSE
    /// `1 + 1 == 2 == false` -> FALSE
    Calculate { input: TextSource },

    /// Compile a source file into bytecode
    Compile { input: Utf8PathBuf },
    /// Compile a source file to bytecode, then immediately interpret it
    Run { input: Utf8PathBuf },
    /// Interpret pre-compiled bytecode
    ///
    /// Note: You also use the dedicated, thinner
    /// `dust-interpretter` for interpretting pre-
    /// compiled bytecode files.
    Interpret { input: ByteSource },
}
