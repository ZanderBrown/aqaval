use crate::error::Runtime;
use crate::eval::Value;
use crate::node::NativeSub;
use crate::node::Subroutine;
use rand::prelude::*;
use std::char;
use std::collections::HashMap;

/// A type which holds varaibles
pub trait Variables {
    /// Get a variable from the scope
    fn fetch(&mut self, name: &str) -> Value;

    /// Declare a native subroutine
    fn declare_sub(&mut self, name: &str, params: &[&str], sub: NativeSub);
}

impl Variables for HashMap<String, Value> {
    fn fetch(&mut self, name: &str) -> Value {
        // Return the variables value or None
        // if it doesn't exist
        self.get(name).map_or(Value::None, |v| v.clone())
    }

    fn declare_sub(&mut self, name: &str, params: &[&str], sub: NativeSub) {
        // Map the &str params to their String equivalents
        let params = params.iter().map(|param| (*param).into()).collect();
        // Store the sub
        self.insert(
            name.into(),
            Value::Subroutine(params, Subroutine::Native(sub)),
        );
    }
}

/// LEN(item), takes an array or string
fn builtin_len(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    // Get the parameter
    let item = store.fetch("item");
    // Match it's type
    match item {
        Value::Array(arr) => Ok(Value::Number(arr.len() as f64)),
        Value::Textual(text) => Ok(Value::Number(text.len() as f64)),
        _ => Err(Runtime::new(format!("Expecting Array/String got {}", item))),
    }
}

/// POSITION(string, char), gets the position of char in string
fn builtin_position(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    // Fetch the arguments
    let text = store.fetch("string");
    let chr = store.fetch("char");
    // Wrap rust's find function
    if let Some(pos) = text.string()?.find(chr.chr()?) {
        Ok(Value::Number(pos as f64))
    } else {
        Err(Runtime::new(format!("Couldn't find {} in {}", chr, text)))
    }
}

/// SUBSTRING(start, end, string), extracts the inclusive range start-end from string
/// TODO: Better handle unicode
fn builtin_substring(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    // Grab the arguments
    let start = store.fetch("start").int()? as usize;
    let end = store.fetch("end").int()? as usize + 1;
    let text = store.fetch("string").string()?;
    // Extract the range
    Ok(Value::Textual(text[start..end].into()))
}

/// STRING_TO_INT(string), Cast a string to integer
fn builtin_string_to_int(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    // Cast to integer and truncate the result
    Ok(Value::Number(builtin_string_to_real(store)?.intt()?))
}

/// STRING_TO_REAL(string), parse a string as a number
fn builtin_string_to_real(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    let text = store.fetch("string").string()?;
    // Fail if we can't parse the string
    Ok(Value::Number(text.parse().or_else(|_| {
        Err(Runtime::new(format!("Badly formatted number {}", text)))
    })?))
}

/// INT_TO_STRING(num), get a string representation of an integer
fn builtin_int_to_string(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    Ok(Value::Textual(store.fetch("num").int()?.to_string()))
}

/// REAL_TO_STRING(num), get a string representation of a number
fn builtin_real_to_string(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    Ok(Value::Textual(store.fetch("num").real()?.to_string()))
}

/// CHAR_TO_CODE(char), gets the integer value of a byte stored as a char
/// BE CAREFUL: Characters are not what you think they are!
fn builtin_char_to_code(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    // TODO: Handle unicode better
    Ok(Value::Number(f64::from(store.fetch("char").chr()? as u8)))
}

/// CODE_TO_CHAR(char), convert a unicode codepoint to it's char value
/// BE CAREFUL: The result may actually be more than one byte long so
/// incompatable with CHAR_TO_CODE. For example 128077 is valid here
/// but the resulting thumbs up character is incompatable with CHAR_TO_CODE
fn builtin_code_to_char(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    Ok(Value::Textual(
        char::from_u32(store.fetch("char").int()? as u32)
            .ok_or_else(|| Runtime::new("Bad codepoint".into()))?
            .to_string(),
    ))
}

/// RANDOM_INT(start, end), generate a random integer in the range start-end
fn builtin_random_int(store: &mut HashMap<String, Value>) -> Result<Value, Runtime> {
    // Generate a reasonably random number
    Ok(Value::Number(
        thread_rng().gen_range::<isize, isize, isize>(
            store.fetch("start").int()? as isize,
            // gen_range is exclusive but we want to be inclusive
            store.fetch("end").int()? as isize + 1,
        ) as f64,
    ))
}

/// Declares all the builtin subroutines
pub fn init(store: &mut dyn Variables) {
    store.declare_sub("LEN", &["item"], builtin_len);
    store.declare_sub("POSITION", &["string", "char"], builtin_position);
    store.declare_sub("SUBSTRING", &["start", "end", "string"], builtin_substring);
    store.declare_sub("STRING_TO_INT", &["string"], builtin_string_to_int);
    store.declare_sub("STRING_TO_REAL", &["string"], builtin_string_to_real);
    store.declare_sub("INT_TO_STRING", &["num"], builtin_int_to_string);
    store.declare_sub("REAL_TO_STRING", &["num"], builtin_real_to_string);
    store.declare_sub("CHAR_TO_CODE", &["char"], builtin_char_to_code);
    store.declare_sub("CODE_TO_CHAR", &["char"], builtin_code_to_char);
    store.declare_sub("RANDOM_INT", &["start", "end"], builtin_random_int);
}
