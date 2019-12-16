use crate::error::Error;
use crate::location::Range;
use crate::node::params_as_str;
use crate::node::Subroutine;

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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    pub fn truthy(&self, r: Range) -> Result<bool, Error> {
        match self {
            // Empty things are falsy
            Value::None => Ok(false),
            // Nice and simple
            Value::Boolean(b) => Ok(*b),
            // Things that exist are truthy
            Value::Textual(_) => Ok(true),
            // Numbers that aren't 0 are truthy
            Value::Number(n) => Ok(*n != 0.0),
            // Recur for constants
            Value::Constant(cons) => cons.truthy(r),
            // Hard to define, throw an error
            _ => Err(Error::runtime(
                format!("Can't interpret {} as boolean", self),
                r,
            )),
        }
    }

    /// Requires this value to be an integer number
    pub fn int(&self, r: Range) -> Result<f64, Error> {
        match self {
            // If the value if integer
            Value::Number(n) => {
                if (n.trunc() - *n).abs() == 0.0 {
                    Ok(*n)
                } else {
                    Err(Error::runtime(format!("Expected integer got {}", n), r))
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
            Value::Constant(cons) => cons.int(r),
            _ => Err(Error::runtime(
                format!("Can't interpret {} as integer", self),
                r,
            )),
        }
    }

    /// Get the integer of this string truncating if real
    pub fn intt(&self, r: Range) -> Result<f64, Error> {
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
            Value::Constant(cons) => cons.intt(r),
            _ => Err(Error::runtime(
                format!("Can't interpret {} as integer", self),
                r,
            )),
        }
    }

    /// Just get the contents of a number
    pub fn real(&self, r: Range) -> Result<f64, Error> {
        match self {
            Value::Number(n) => Ok(*n),
            Value::Boolean(b) => {
                if *b {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
            Value::Constant(cons) => cons.real(r),
            _ => Err(Error::runtime(
                format!("Can't interpret {} as real", self),
                r,
            )),
        }
    }

    /// Get the string value
    pub fn string(&self, r: Range) -> Result<String, Error> {
        match self {
            // Booleans are implicitly strings
            Value::Boolean(b) => Ok(if *b { "True".into() } else { "False".into() }),
            Value::Textual(s) => Ok(s.to_string()),
            Value::Constant(cons) => cons.string(r),
            _ => Err(Error::runtime(
                format!("Can't interpret {} as string", self),
                r,
            )),
        }
    }

    /// Interpret a textual value as char
    pub fn chr(&self, r: Range) -> Result<char, Error> {
        match self {
            Value::Textual(s) => {
                // chars can only be 1 char long
                if s.len() > 1 {
                    Err(Error::runtime(
                        format!("Can't interpret {} as char", self),
                        r,
                    ))
                } else {
                    // Get the characters
                    s.chars()
                        // From that the first character
                        .nth(0)
                        // Return an error is None
                        .ok_or_else(|| {
                            Error::runtime(format!("Can't interpret {} as char", self), r)
                        })
                }
            }
            Value::Constant(cons) => cons.chr(r),
            _ => Err(Error::runtime(
                format!("Can't interpret {} as char", self),
                r,
            )),
        }
    }

    /// Add two values together creating a new value
    pub fn add(&self, other: Self, range: Range) -> Result<Self, Error> {
        match self {
            Value::Number(l) => match other {
                // Numbers are easy
                Value::Number(r) => Ok(Value::Number(l + r)),
                // Number & strings can be concatenated
                Value::Textual(s) => Ok(Value::Textual(format!("{}{}", l, s))),
                _ => Err(Error::runtime(
                    format!("Operation {} + {} isn't supported", self, other),
                    range,
                )),
            },
            // Concatenate strings
            Value::Textual(l) => match other {
                Value::Number(r) => Ok(Value::Textual(format!("{}{}", l, r))),
                _ => Ok(Value::Textual(format!("{}{}", l, other.string(range)?))),
            },
            Value::Constant(cons) => cons.add(other, range),
            _ => Err(Error::runtime(
                format!("Operation {} + {} isn't supported", self, other),
                range,
            )),
        }
    }

    /// Subtract other from self returning the result
    pub fn sub(&self, other: Self, range: Range) -> Result<Self, Error> {
        match self {
            // Only numbers can be subtracted from each other
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l - r)),
                _ => Err(Error::runtime(
                    format!("Operation {} - {} isn't supported", self, other),
                    range,
                )),
            },
            Value::Constant(cons) => cons.sub(other, range),
            _ => Err(Error::runtime(
                format!("Operation {} - {} isn't supported", self, other),
                range,
            )),
        }
    }

    /// Multiply this value by other
    pub fn mul(&self, other: Self, r: Range) -> Result<Self, Error> {
        match self {
            // Only numbers can be multiplied
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l * r)),
                _ => Err(Error::runtime(
                    format!("Operation {} * {} isn't supported", self, other),
                    r,
                )),
            },
            Value::Constant(cons) => cons.mul(other, r),
            _ => Err(Error::runtime(
                format!("Operation {} * {} isn't supported", self, other),
                r,
            )),
        }
    }

    /// Divide self by other
    pub fn div(&self, other: Self, r: Range) -> Result<Self, Error> {
        match self {
            // Only numbers can be divided
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l / r)),
                _ => Err(Error::runtime(
                    format!("Operation {} / {} isn't supported", self, other),
                    r,
                )),
            },
            Value::Constant(cons) => cons.div(other, r),
            _ => Err(Error::runtime(
                format!("Operation {} / {} isn't supported", self, other),
                r,
            )),
        }
    }

    /// Integer division
    pub fn idiv(&self, other: Self, r: Range) -> Result<Self, Error> {
        match self {
            Value::Number(l) => match other {
                // Truncate the result of the division
                Value::Number(r) => Ok(Value::Number((l / r).trunc())),
                _ => Err(Error::runtime(
                    format!("Operation {} DIV {} isn't supported", self, other),
                    r,
                )),
            },
            Value::Constant(cons) => cons.idiv(other, r),
            _ => Err(Error::runtime(
                format!("Operation {} DIV {} isn't supported", self, other),
                r,
            )),
        }
    }

    /// Modulo (remainder) of self & other
    pub fn imod(&self, other: Self, r: Range) -> Result<Self, Error> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok(Value::Number(l % r)),
                _ => Err(Error::runtime(
                    format!("Operation {} MOD {} isn't supported", self, other),
                    r,
                )),
            },
            Value::Constant(cons) => cons.imod(other, r),
            _ => Err(Error::runtime(
                format!("Operation {} MOD {} isn't supported", self, other),
                r,
            )),
        }
    }

    /// Is self less than/before other
    pub fn lt(&self, other: Self, r: Range) -> Result<Self, Error> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok((*l < r).into()),
                _ => Err(Error::runtime(
                    format!("Operation {} < {} isn't supported", self, other),
                    r,
                )),
            },
            Value::Constant(cons) => cons.lt(other, r),
            _ => Err(Error::runtime(
                format!("Operation {} < {} isn't supported", self, other),
                r,
            )),
        }
    }

    /// Is self greater than other
    pub fn gt(&self, other: Self, r: Range) -> Result<Self, Error> {
        match self {
            Value::Number(l) => match other {
                Value::Number(r) => Ok((*l > r).into()),
                _ => Err(Error::runtime(
                    format!("Operation {} > {} isn't supported", self, other),
                    r,
                )),
            },
            Value::Constant(cons) => cons.gt(other, r),
            _ => Err(Error::runtime(
                format!("Operation {} > {} isn't supported", self, other),
                r,
            )),
        }
    }

    /// Are self & other equal
    pub fn eq(&self, other: Self, r: Range) -> Result<Self, Error> {
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
            Value::Boolean(b) => Ok((*b == other.truthy(r)?).into()),
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
            Value::Return(_) => Err(Error::runtime(
                "Internal Error comparison against a placeholder".into(),
                r,
            )),
            Value::Constant(cons) => cons.eq(other, r),
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
                        if !a.eq(b.clone(), r)?.truthy(r)? {
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
    pub fn neq(&self, other: Self, r: Range) -> Result<Self, Error> {
        // Just invert the value of eq
        match self.eq(other, r)? {
            Value::Boolean(b) => Ok((!b).into()),
            // This should never happen
            _ => Err(Error::runtime(
                "Internal Error eq returned a non-boolean".into(),
                r,
            )),
        }
    }

    /// Self less than or equal to other
    pub fn lteq(&self, other: Self, r: Range) -> Result<Self, Error> {
        // Start by checking less than
        match self.lt(other.clone(), r)? {
            Value::Boolean(b) => {
                if b {
                    Ok(true.into())
                } else {
                    // Not less than, but are they equal
                    match self.eq(other, r)? {
                        Value::Boolean(b) => Ok(b.into()),
                        // This should never happen
                        _ => Err(Error::runtime(
                            "Internal Error eq returned a non-boolean".into(),
                            r,
                        )),
                    }
                }
            }
            // This should never happen
            _ => Err(Error::runtime(
                "Internal Error lt returned a non-boolean".into(),
                r,
            )),
        }
    }

    /// Greater than or equal to
    pub fn gteq(&self, other: Self, r: Range) -> Result<Self, Error> {
        // Initaly check greater than
        match self.gt(other.clone(), r)? {
            Value::Boolean(b) => {
                if b {
                    Ok(true.into())
                } else {
                    // Fall back to equal
                    match self.eq(other, r)? {
                        Value::Boolean(b) => Ok(b.into()),
                        // This should never happen
                        _ => Err(Error::runtime(
                            "Internal Error eq returned a non-boolean".into(),
                            r,
                        )),
                    }
                }
            }
            // This should never happen
            _ => Err(Error::runtime(
                "Internal Error qt returned a non-boolean".into(),
                r,
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
