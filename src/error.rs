use crate::location::Range;

use std::error;
use std::fmt;

#[derive(Debug)]
/// An error that occured whilst parsing the input
pub enum Error {
    /// Parse errors
    /// Turple is line, column & message
    Parse(String, Range),
    /// Errors that occur when further input is expected
    /// Different because they are expected in REPL environments
    EndOfInput(String, Range),
    Runtime(String, Range),
}

impl Error {
    /// Creates a general error for the line, column and message
    pub fn parse(m: String, at: Range) -> Self {
        Error::Parse(m, at)
    }

    /// Creates an error about unexpected end
    pub fn eof(m: String, at: Range) -> Self {
        Error::EndOfInput(m, at)
    }

    /// Creates an error about runtime anomoly
    pub fn runtime(m: String, at: Range) -> Self {
        Error::Runtime(m, at)
    }

    pub fn at(&self) -> Range {
        match self {
            Self::Parse(_, at) | Self::EndOfInput(_, at) | Self::Runtime(_, at) => *at,
        }
    }

    pub fn message(&self) -> &str {
        match self {
            Self::Parse(m, _) | Self::EndOfInput(m, _) | Self::Runtime(m, _) => m,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parse(m, at) => write!(f, "Error Error: {} {}", m, at),
            Error::EndOfInput(m, at) => write!(f, "End of input: {} {}", m, at),
            Error::Runtime(m, at) => write!(f, "Anomaly: {} {}", m, at),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}
