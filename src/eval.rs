use error::Runtime;
use node::params_as_str;
use node::Subroutine;
use std::fmt;

// Everything is pass by value
#[derive(Clone)]
/// A value that exists in the runtime
pub enum Value {
    /// Cannot be explicity created rather
    /// used as a null-like placeholder
    None,
    /// A string or char
    Textual(String),
    /// Integers & reals
    Number(f64),
    /// True or False
    Boolean(bool),
    /// Something callable
    /// Turple: parameters, body
    Subroutine(Vec<String>, Subroutine),
    /// Marker for a value returned from subs
    Return(Box<Value>),
    /// Marker for constant values
    Constant(Box<Value>),
    // An array
    Array(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::None => write!(f, "[NONE]"),
            Value::Boolean(p) => write!(f, "{}", if *p { "[TRUE]" } else { "[FALSE]" }),
            Value::Textual(p) => write!(f, "'{}'", p),
            Value::Number(p) => write!(f, "{}", p),
            Value::Subroutine(p, _) => write!(f, "[SUBROUTINE] ({})", params_as_str(p)),
            Value::Return(v) => write!(f, "[RETURN] {}", v),
            Value::Constant(v) => write!(f, "[CONSTANT] {}", v),
            Value::Array(arr) => write!(f, "[{}]", params_as_str(arr)),
        }
    }
}

impl Value {
    /// Does this value evaluate to true
    pub fn truthy(&self) -> Result<bool, Runtime> {
        match self {
            // Nice and simple
            Value::Boolean(b) => Ok(*b),
            // Empty things are falsy
            Value::None => Ok(false),
            // Numbers that aren't 0 are truthy
            Value::Number(n) => Ok(*n != 0.0),
            // Things that exist are truthy
            Value::Textual(_) => Ok(true),
            // Recur for constants
            Value::Constant(cons) => cons.truthy(),
            _ => Err(Runtime::new(format!("Can't interpret {} as boolean", self))),
        }
    }

    /// Requires this value to be an integer number
    pub fn int(&self) -> Result<f64, Runtime> {
        match self {
            // If the value if integer
            Value::Number(n) => {
                if (n.trunc() - *n).abs() == 0.0 {
                    Ok(*n)
                } else {
                    Err(Runtime::new(format!("Expected integer got {}", n)))
                }
            }
            // Allow boolean to be integer
            Value::Boolean(b) => {
                if *b {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
            Value::Constant(cons) => cons.int(),
            _ => Err(Runtime::new(format!("Expected integer got {}", self))),
        }
    }

    /// Get the integer of this string truncating if real
    pub fn intt(&self) -> Result<f64, Runtime> {
        match self {
            Value::Number(n) => Ok(n.trunc()),
            // Allow boolean to be integer
            Value::Boolean(b) => {
                if *b {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
            Value::Constant(cons) => cons.intt(),
            _ => Err(Runtime::new(format!("Expected integer got {}", self))),
        }
    }

    /// Just get the contents of a number
    pub fn real(&self) -> Result<f64, Runtime> {
        match self {
            Value::Number(n) => Ok(*n),
            Value::Boolean(b) => {
                if *b {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
            Value::Constant(cons) => cons.real(),
            _ => Err(Runtime::new(format!("Expected real got {}", self))),
        }
    }

    /// Get the string value
    pub fn string(&self) -> Result<String, Runtime> {
        match self {
            // Booleans are implicitly strings
            Value::Boolean(b) => Ok(if *b { "True".into() } else { "False".into() }),
            Value::Textual(s) => Ok(s.to_string()),
            Value::Constant(cons) => cons.string(),
            _ => Err(Runtime::new(format!("Can't interpret {} as string", self))),
        }
    }

    /// Interpret a textual value as char
    pub fn chr(&self) -> Result<char, Runtime> {
        match self {
            Value::Textual(s) => {
                // chars can only be 1 char long
                if s.len() > 1 {
                    Err(Runtime::new(format!("Can't interpret {} as char", self)))
                } else {
                    // Get the characters
                    s.chars()
                        // From that the first character
                        .nth(0)
                        // Return an error is None
                        .ok_or_else(|| Runtime::new(format!("Can't interpret {} as char", self)))
                }
            }
            Value::Constant(cons) => cons.chr(),
            _ => Err(Runtime::new(format!("Can't interpret {} as char", self))),
        }
    }

    /// Add two values together creating a new value
    pub fn add(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            Value::Number(l) => match other {
                // Numbers are easy
                Value::Number(r) => Ok(Value::Number(l + r)),
                // Number & strings can be concatenated
                Value::Textual(s) => Ok(Value::Textual(format!("{}{}", l, s))),
                _ => Err(Runtime::new(format!(
                    "Operation {} + {} isn't supported",
                    self, other
                ))),
            },
            // Concatenate strings
            Value::Textual(l) => match other {
                Value::Number(r) => Ok(Value::Textual(format!("{}{}", l, r))),
                _ => Ok(Value::Textual(format!("{}{}", l, other.string()?))),
            },
            Value::Constant(cons) => cons.add(other),
            _ => Err(Runtime::new(format!(
                "Operation {} + {} isn't supported",
                self, other
            ))),
        }
    }

    /// Subtract other from self returning the result
    pub fn sub(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            // Only numbers can be subtracted from each other
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l - r)),
                _ => Err(Runtime::new(format!(
                    "Operation {} - {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.sub(other),
            _ => Err(Runtime::new(format!(
                "Operation {} - {} isn't supported",
                self, other
            ))),
        }
    }

    /// Multiply this value by other
    pub fn mul(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            // Only numbers can be multiplied
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l * r)),
                _ => Err(Runtime::new(format!(
                    "Operation {} * {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.mul(other),
            _ => Err(Runtime::new(format!(
                "Operation {} * {} isn't supported",
                self, other
            ))),
        }
    }

    /// Divide self by other
    pub fn div(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            // Only numbers can be divided
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l / r)),
                _ => Err(Runtime::new(format!(
                    "Operation {} / {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.div(other),
            _ => Err(Runtime::new(format!(
                "Operation {} / {} isn't supported",
                self, other
            ))),
        }
    }

    /// Integer division
    pub fn idiv(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            Value::Number(l) => match other {
                // Truncate the result of the division
                Value::Number(r) => Ok(Value::Number((l / r).trunc())),
                _ => Err(Runtime::new(format!(
                    "Operation {} DIV {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.idiv(other),
            _ => Err(Runtime::new(format!(
                "Operation {} DIV {} isn't supported",
                self, other
            ))),
        }
    }

    /// Modulo (remainder) of self & other
    pub fn imod(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l % r)),
                _ => Err(Runtime::new(format!(
                    "Operation {} MOD {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.imod(other),
            _ => Err(Runtime::new(format!(
                "Operation {} MOD {} isn't supported",
                self, other
            ))),
        }
    }

    /// Is self less than/before other
    pub fn lt(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok((*l < r).into()),
                _ => Err(Runtime::new(format!(
                    "Operation {} < {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.lt(other),
            _ => Err(Runtime::new(format!(
                "Operation {} < {} isn't supported",
                self, other
            ))),
        }
    }

    /// Is self greater than other
    pub fn gt(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok((*l > r).into()),
                _ => Err(Runtime::new(format!(
                    "Operation {} > {} isn't supported",
                    self, other
                ))),
            },
            Value::Constant(cons) => cons.gt(other),
            _ => Err(Runtime::new(format!(
                "Operation {} > {} isn't supported",
                self, other
            ))),
        }
    }

    /// Are self & other equal
    pub fn eq(&self, other: Self) -> Result<Self, Runtime> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok((*l == r).into()),
                Value::Textual(r) => Ok((l.to_string() == r).into()),
                _ => Ok(false.into()),
            },
            Value::Textual(l) => match other {
                Value::Number(r) => Ok((*l == r.to_string()).into()),
                Value::Textual(r) => Ok((*l == r).into()),
                _ => Ok(false.into()),
            },
            Value::Boolean(b) => Ok((*b == other.truthy()?).into()),
            Value::None => match other {
                Value::None => Ok(true.into()),
                _ => Ok(false.into()),
            },
            Value::Subroutine(takes_a, _) => match other {
                Value::Subroutine(takes_b, _) => {
                    // HACK: This just checks they take the same arguments
                    Ok((*takes_a == takes_b).into())
                }
                _ => Ok(false.into()),
            },
            // This should never happen
            Value::Return(_) => Err(Runtime::new(
                "Internal Error comparison against a placeholder".into(),
            )),
            Value::Constant(cons) => cons.eq(other),
            Value::Array(array_a) => match other {
                Value::Array(array_b) => {
                    // If the arrays are of different lengths
                    // they can't possibly be equal
                    if array_a.len() != array_b.len() {
                        return Ok(false.into());
                    }
                    // Pair up the values of the two arrays
                    for (a, b) in array_a.iter().zip(array_b.iter()) {
                        // If a & b are not equal
                        if !a.eq(b.clone())?.truthy()? {
                            // Then the arrays aren't
                            return Ok(false.into());
                        }
                    }
                    // It seems they are equal
                    Ok(true.into())
                }
                _ => Ok(false.into()),
            },
        }
    }

    /// Not equal
    pub fn neq(&self, other: Self) -> Result<Self, Runtime> {
        // Just invert the value of eq
        match self.eq(other)? {
            Value::Boolean(b) => Ok((!b).into()),
            // This should never happen
            _ => Err(Runtime::new(
                "Internal Error eq returned a non-boolean".into(),
            )),
        }
    }

    /// Self less than or equal to other
    pub fn lteq(&self, other: Self) -> Result<Self, Runtime> {
        // Start by checking less than
        match self.lt(other.clone())? {
            Value::Boolean(b) => {
                if b {
                    Ok(true.into())
                } else {
                    // Not less than, but are they equal
                    match self.eq(other)? {
                        Value::Boolean(b) => Ok(b.into()),
                        // This should never happen
                        _ => Err(Runtime::new(
                            "Internal Error eq returned a non-boolean".into(),
                        )),
                    }
                }
            }
            // This should never happen
            _ => Err(Runtime::new(
                "Internal Error lt returned a non-boolean".into(),
            )),
        }
    }

    /// Greater than or equal to
    pub fn gteq(&self, other: Self) -> Result<Self, Runtime> {
        // Initaly check greater than
        match self.gt(other.clone())? {
            Value::Boolean(b) => {
                if b {
                    Ok(true.into())
                } else {
                    // Fall back to equal
                    match self.eq(other)? {
                        Value::Boolean(b) => Ok(b.into()),
                        // This should never happen
                        _ => Err(Runtime::new(
                            "Internal Error eq returned a non-boolean".into(),
                        )),
                    }
                }
            }
            // This should never happen
            _ => Err(Runtime::new(
                "Internal Error qt returned a non-boolean".into(),
            )),
        }
    }
}

// Wrap a bool with .into() / ::from()
impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Boolean(v)
    }
}
