use camino::Utf8PathBuf;
use miette::{Context as _, IntoDiagnostic as _};

#[derive(clap::Parser)]
#[command(author, version, about)]
pub struct Args {
    #[command(subcommand)]
    pub cmd: Option<Command>,
}

#[derive(clap::Subcommand)]
pub enum Command {
    /// Tokenize
    Tokenize { input: TextSource },
    /// Interpret
    Interpret { input: TextSource },
    /// Start an interactive dust terminal
    Interactive,
    /// Evaluate a static expression,
    /// e.g.
    /// `1 + 1 == 2` -> TRUE
    /// `1 + 1 < 2` -> FALSE
    /// `1 + 1 == 2 == false` -> FALSE
    Calculate { input: TextSource },
}

#[derive(Clone, Debug)]
pub enum TextSource {
    File(Utf8PathBuf),
    Text(String),
}

impl core::fmt::Display for TextSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TextSource::File(path_buf) => write!(f, "{}", path_buf),
            TextSource::Text(s) => write!(f, "<text source len={}>", s.len()),
        }
    }
}

impl std::str::FromStr for TextSource {
    type Err = std::io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let path = Utf8PathBuf::from(s);

        if path.is_file() {
            Ok(Self::File(path))
        } else {
            Ok(Self::Text(s.to_owned()))
        }
    }
}

impl TextSource {
    pub fn read(&self) -> miette::Result<String> {
        match self {
            Self::File(path) => std::fs::read_to_string(path),
            Self::Text(text) => Ok(text.clone()),
        }
        .into_diagnostic()
        .wrap_err_with(|| format!("failed to read '{}'", self))
    }
}
