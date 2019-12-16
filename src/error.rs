use crate::location::Point;
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
    Runtime(String, Option<Range>),
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
    pub fn runtime(m: String, at: Option<Range>) -> Self {
        Error::Runtime(m, at)
    }

    pub fn at(&self) -> Range {
        match self {
            Self::Parse(_, at) => *at,
            Self::EndOfInput(_, at) => *at,
            Self::Runtime(_, Some(at)) => *at,
            // Not great
            Self::Runtime(_, None) => Range::new(Point::new(0, 0), Point::new(0, 0)),
        }
    }

    pub fn message(&self) -> &str {
        match self {
            Self::Parse(m, _) => m,
            Self::EndOfInput(m, _) => m,
            Self::Runtime(m, _) => m,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Parse(m, at) => write!(f, "Error Error: {} {}", m, at),
            Error::EndOfInput(m, at) => write!(f, "End of input: {} {}", m, at),
            Error::Runtime(m, Some(at)) => write!(f, "Anomaly: {} {}", m, at),
            Error::Runtime(m, None) => write!(f, "Anomaly: {}", m),
        }
    }
}

impl error::Error for Error {
    fn cause(&self) -> Option<&dyn error::Error> {
        None
    }
}
