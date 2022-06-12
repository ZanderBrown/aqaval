use error::Syntax;
use input::Stream;
use std::collections::HashMap;
use std::fmt;

// Valid keybords in the language
const KEYWORDS: [&str; 20] = [
    "constant",
    "NOT",
    "REPEAT",
    "UNTIL",
    "WHILE",
    "ENDWHILE",
    "FOR",
    "TO",
    "ENDFOR",
    "IF",
    "THEN",
    "ENDIF",
    "ELSE",
    "SUBROUTINE",
    "ENDSUBROUTINE",
    "RETURN",
    "USERINPUT",
    "OUTPUT",
    "True",
    "False",
];

#[derive(PartialEq, Clone)]
/// Characters grouped by type
pub enum Token {
    // Punctuation that isn't an operator
    Punctuation(String),
    // The use of a keyword
    Keyword(String),
    // + - and so forth
    Operator(String),
    // A symbol that isn't a keyword
    Value(String),
    // A numeric literal value
    Number(String),
    // A quoted value (without quotes)
    Text(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Punctuation(s) =>
                    format!("'{}'", if s == "\n" { "\\n".into() } else { s.clone() }),
                Token::Keyword(s) => s.to_string(),
                Token::Operator(s) => format!("[{}]", s),
                Token::Value(s) => format!("`{}`", s),
                Token::Number(s) => format!("Number {}", s),
                Token::Text(s) => format!("'{}'", s),
            }
        )
    }
}

/// Classifies and groups chars into tokens
pub struct Tokens {
    // Caches a token produced for peek()
    current: Option<Token>,
    // The stream we are reading
    input: Stream,
    // Key/value of operators and there precedence
    operations: HashMap<String, u8>,
}

/// A possibly none type that may have caused a syntax error
pub type TokResult = Result<Option<Token>, Syntax>;

impl Tokens {
    /// Checks if peek() matches t
    pub fn is(&mut self, t: &Token) -> TokResult {
        // If there is a token to peek
        Ok(if let Some(tok) = self.peek()? {
            // And it equals t
            if t == &tok {
                // Return it
                Some(tok)
            } else {
                None
            }
        } else {
            None
        })
    }

    /// Requires the next token to be t an advances past it
    pub fn skip(&mut self, t: &Token) -> Result<(), Syntax> {
        // If the token doesn't match
        if self.is(t)?.is_none() {
            // But there is a token
            if let Some(next) = self.next()? {
                // Fail stating what we wanted vs found
                Err(self.croak(format!("Expecting {} got {}", t, next)))
            } else {
                // Fail stating what we wanted
                Err(self.croak(format!("Expecting {}", t)))
            }
        } else {
            // Advance
            self.next()?;
            // Success!
            Ok(())
        }
    }

    /// Fetch a token without consuming it
    pub fn peek(&mut self) -> TokResult {
        // If we don't have a cached token
        if self.current.is_none() {
            // Get one
            self.current = self.read_next()?;
        }
        // Return the cached token
        Ok(self.current.clone())
    }

    /// Fetch & consume a token
    pub fn next(&mut self) -> TokResult {
        // If there is a cached token
        if let Some(tok) = self.current.take() {
            // Clear it & return it
            Ok(Some(tok))
        // If we can read another token
        } else if let Some(ret) = self.read_next()? {
            // Return it
            Ok(Some(ret))
        } else {
            // We are out of tokens
            Ok(None)
        }
    }

    /// Wrapper of Stream::croak
    pub fn croak(&self, msg: String) -> Syntax {
        self.input.croak(msg)
    }

    /// Is ch valid to start a symbol
    fn is_value_char(&self, ch: char) -> bool {
        // We only allow alphabetic character at
        // the start but are more forgivin further on
        ch.is_alphabetic()
            || ch.is_numeric()
            || ch == '_'
            || ch == '?'
            || ch == '!'
            || ch == '-'
            || ch == '<'
            || ch == '>'
            || ch == '='
    }

    /// Is ch an operator character
    fn is_op_char(&self, ch: char) -> bool {
        // Most operators are a single character
        // so are a key in the table
        self.operations.contains_key(&*ch.to_string())
            || ch == 'O'
            || ch == 'R'
            || ch == 'A'
            || ch == 'N'
            || ch == 'D'
            || ch == 'M'
            || ch == 'I'
            || ch == 'V'
    }

    /// Checks if ch is a known form of punctuation
    fn is_punc_char(&self, ch: char) -> bool {
        ch == ',' // Seperator in lists
            || ch == '\n' // Newline
            || ch == '(' // Start params/args
            || ch == ')' // End params/args
            || ch == '[' // Start an array/indexing
            || ch == ']' // End an array/indexing
    }

    /// Fetch the precedence of op (if known)
    pub fn precedence(&self, op: &str) -> Option<u8> {
        self.operations.get(&*op).cloned()
    }

    /// Consumes characters while predicate returns true
    fn read_while(&mut self, predicate: &mut dyn FnMut(&mut Self, char) -> bool) -> String {
        // The string we will return
        let mut s = String::new();
        // Read whilst charecters are still available
        while let Some(ch) = self.input.peek() {
            // If ch satisfies predicate
            if predicate(self, ch) {
                // Consume ch appending it to the result
                s.push(self.input.next().unwrap());
            } else {
                // Break out of the while loop
                break;
            }
        }
        s
    }

    /// Read in a (possibly decimal) number
    fn read_number(&mut self) -> Option<Token> {
        // Number only allowed one decimal place, track
        // if we have had one yet
        let mut has_dot = false;
        // Keep reading
        let number = self.read_while(&mut |_, ch| -> bool {
            // If we found a dot
            if ch == '.' {
                // And we have already had one
                if has_dot {
                    // This number is over
                    return false;
                }
                // Mark that we have had one
                has_dot = true;
                // Move to the next number
                return true;
            }
            // Returns true for numeric values
            ch.is_numeric()
        });
        // Return the built number
        Some(Token::Number(number))
    }

    /// Consume an identifier
    fn read_ident(&mut self) -> Option<Token> {
        // Read the identifier
        let id = self.read_while(&mut |s, ch| s.is_value_char(ch));
        // If this is a keyword
        Some(if KEYWORDS.contains(&id.as_str()) {
            // Return it as a keyword
            Token::Keyword(id)
        // Some operators are works like MOD
        } else if self.operations.contains_key(&id) {
            // So return them as an operator
            Token::Operator(id)
        } else {
            // Just a boring symbol
            Token::Value(id)
        })
    }

    /// Read a quoted string
    fn read_textual(&mut self, end: char) -> Result<String, Syntax> {
        // Are we currently reading an escaped value
        let mut escaped = false;
        // The read string
        let mut s = String::new();
        // Skip the opening charecter
        self.input.next();
        // Consume a character
        while let Some(ch) = self.input.next() {
            // If we are currently escaped
            if escaped {
                // Just push it to the response
                s.push(ch);
                // Escape is over
                escaped = false;
            // If this is a \
            } else if ch == '\\' {
                // Then start an escape (and ignore the \)
                escaped = true;
            // If this is the closing quote
            } else if ch == end {
                // Return the built string
                return Ok(s);
            } else {
                // Just append normal characters
                s.push(ch);
            }
        }
        // Opps we ran out of characters
        Err(Syntax::EndOfInput("Still in string".into()))
    }

    /// Get the next token
    fn read_next(&mut self) -> TokResult {
        // Skip whitespace that isn't newlines
        self.read_while(&mut |_, ch| ch != '\n' && ch.is_whitespace());
        // Peek the next char
        if let Some(ch) = self.input.peek() {
            // Comments start with #
            if ch == '#' {
                // Just read until the end of the line
                self.read_while(&mut |_, ch| ch != '\n');
                // Return a newline token
                Ok(Some(Token::Punctuation("\n".into())))
            // Strings start with '
            } else if ch == '\'' {
                // Read in a string and return it
                Ok(Some(Token::Text(self.read_textual(ch)?)))
            // Number start with numbers
            } else if ch.is_numeric() {
                Ok(self.read_number())
            // It's punctuation
            } else if self.is_punc_char(ch) {
                Ok(Some(Token::Punctuation(
                    self.input.next().unwrap().to_string(),
                )))
            // Indentifiers must start with an alphabetic char
            } else if ch.is_alphabetic() {
                Ok(self.read_ident())
            // Is this an operation
            } else if self.is_op_char(ch) {
                let op = self.read_while(&mut |s, ch| s.is_op_char(ch));
                Ok(Some(Token::Operator(op)))
            // Don't understand this char, error out
            } else {
                Err(self.croak(format!("Unexpected character: {}", ch)))
            }
        } else {
            // No more chars means no more tokens
            Ok(None)
        }
    }

    /// Skip newlines
    pub fn absorb_newlines(&mut self) -> Result<(), Syntax> {
        // While there are newlines
        while let Some(m) = self.is(&Token::Punctuation("\n".into()))? {
            // Skip 'em
            self.skip(&m)?;
        }
        Ok(())
    }

    // Some more obscure punctuation used in the language
    pub const ASSIGN: &'static str = "\u{2190}"; // ←
    pub const LESS_EQUAL: &'static str = "\u{2264}"; // ≤
    pub const GREATER_EQUAL: &'static str = "\u{2265}"; // ≥
    pub const NOT_EQUAL: &'static str = "\u{2260}"; // ≠

    /// Produces the map of operators and their precedence
    pub fn operations_map() -> HashMap<String, u8> {
        let mut operations = HashMap::new();
        // Asignment is the lowest because everything must
        // be calculated before it
        operations.insert(Self::ASSIGN.into(), 1);

        // Boolean and/or are low so they can compare
        // the results of < & >
        operations.insert("OR".into(), 2);
        operations.insert("AND".into(), 3);

        // Comparisons come next so we can compare
        // the results of add/sub ext
        operations.insert("<".into(), 7);
        operations.insert(">".into(), 7);
        operations.insert(Self::LESS_EQUAL.into(), 7);
        operations.insert(Self::GREATER_EQUAL.into(), 7);
        operations.insert("=".into(), 7);
        operations.insert(Self::NOT_EQUAL.into(), 7);

        // Now boring old +/-
        operations.insert("+".into(), 10);
        operations.insert("-".into(), 10);

        // BODMAS puts mul/div after add/sub so here they are
        operations.insert("*".into(), 20);
        operations.insert("/".into(), 20);
        operations.insert("MOD".into(), 20);
        operations.insert("DIV".into(), 20);

        operations
    }
}

// Construct a Tokens from a Stream
impl From<Stream> for Tokens {
    fn from(input: Stream) -> Self {
        Self {
            current: None,
            input,
            // Ideally this would be a constant but
            // constant HashMaps are not supported (yet?)
            operations: Self::operations_map(),
        }
    }
}
