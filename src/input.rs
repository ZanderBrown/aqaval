use crate::location::Point;

/// Wraps a string in an iter-like structure
/// that tracks the current line-number & column
pub struct Stream {
    input: String,
    line: usize,
    col: usize,
    pos: usize,
}

impl Stream {
    /// Create a stream from a string
    pub fn new(input: String) -> Self {
        Self {
            input,
            line: 1,
            col: 0,
            pos: 0,
        }
    }

    /// Consume a char and advance position
    pub fn next(&mut self) -> Option<char> {
        // If further chars are avalible
        if let Some(ch) = self.input.chars().nth(self.pos) {
            // Advance the position
            self.pos += 1;
            // If the charecter is a newline
            if ch == '\n' {
                // Record the new line position
                self.line += 1;
                // Reset the column position
                self.col = 0;
            } else {
                // Still on the same line, advance the column
                self.col += 1;
            }
            // Return the character
            Some(ch)
        } else {
            // No characters left
            None
        }
    }

    /// Fetch the next character without consuming it
    pub fn peek(&self) -> Option<char> {
        // Return the character
        self.input.chars().nth(self.pos)
    }

    pub fn here(&self) -> Point {
        Point::new(self.line, self.col)
    }

    pub fn get_source(&self, line: usize) -> Option<&str> {
        let lines: Vec<&str> = self.input.split('\n').collect();
        if lines.len() >= line {
            Some(lines[line - 1])
        } else {
            None
        }
    }
}

/// Allow casting String to Stream
impl From<String> for Stream {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}
