use std::error::Error;
use std::fmt;

#[derive(Debug)]
/// An error that occured whilst parsing the input
pub enum Syntax {
    /// General errors
    /// Turple is line, column & message
    General(usize, usize, String),
    /// Errors that occur when further input is expected
    /// Different because they are expected in REPL environments
    EndOfInput(String),
}

impl Syntax {
    /// Creates a general error for the line, column and message
    pub fn new(l: usize, c: usize, m: String) -> Self {
        Syntax::General(l, c, m)
    }
}

impl fmt::Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Syntax::General(l, c, m) => write!(f, "Syntax Error: {} [{}:{}]", m, l, c),
            Syntax::EndOfInput(m) => write!(f, "End of input: {}", m),
        }
    }
}

impl Error for Syntax {
    fn description(&self) -> &str {
        "Bad Syntax"
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
