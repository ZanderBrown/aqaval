use crate::location::Range;
use crate::token::Tokens;

use std::error::Error;
use std::fmt;

#[derive(Debug)]
/// An error that occured whilst parsing the input
pub enum Syntax {
    /// General errors
    /// Turple is line, column & message
    General(String, Range),
    /// Errors that occur when further input is expected
    /// Different because they are expected in REPL environments
    EndOfInput(String, Range),
}

impl Syntax {
    /// Creates a general error for the line, column and message
    pub fn new(m: String, at: Range) -> Self {
        Syntax::General(m, at)
    }

    /// Creates an error about unexpected end
    pub fn eof(m: String, at: Range) -> Self {
        Syntax::EndOfInput(m, at)
    }

    pub fn at(&self) -> Range {
        match self {
            Self::General(_, at) => *at,
            Self::EndOfInput(_, at) => *at,
        }
    }

    pub fn message(&self) -> &str {
        match self {
            Self::General(m, _) => m,
            Self::EndOfInput(m, _) => m,
        }
    }

    pub fn print(&self, src: Option<&Tokens>) {
        if let Some(src) = src {
            let at = self.at();
            let line = format!("{}", at.start().line());
            if let Some(source) = src.get_source(at.start().line()) {
                eprintln!("{} ❘{}", line, source);
            } else {
                eprintln!("{} ❘ [err]", line);
            }
            eprintln!(
                "{:idt$} ❘{:pad$}{:↑>num$}",
                " ",
                "",
                "↑",
                idt = line.len(),
                pad = at.start().column(),
                num = at.end().column() - at.start().column()
            );
            eprintln!(
                "{:idt$} ❘{:pad$}{}",
                " ",
                " ",
                self.message(),
                idt = line.len(),
                pad = at.start().column()
            );
        } else {
            eprintln!("{}", self.message());
        }
    }
}

impl fmt::Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Syntax::General(m, at) => write!(f, "Syntax Error: {} {}", m, at),
            Syntax::EndOfInput(m, at) => write!(f, "End of input: {} {}", m, at),
        }
    }
}

impl Error for Syntax {
    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

#[derive(Debug)]
/// Errors that occur during evaluation
/// Things like "a" * 5 which is syntatically
/// valid but logical nonsense
pub struct Runtime(String);

impl Runtime {
    pub fn new(m: String) -> Self {
        Runtime(m)
    }
}

impl fmt::Display for Runtime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Anomaly: {}", self.0)
    }
}

impl Error for Runtime {
    fn description(&self) -> &str {
        "Runtime Anomaly"
    }
}
