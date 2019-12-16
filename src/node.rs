use std::collections::HashMap;
use std::fmt;
use std::io;
use std::ops::Deref;

use crate::error::Error;
use crate::eval::Value;
use crate::location::Range;
use crate::token::Tokens;

/// Produces a text representation of an array of
/// items that impl Display
pub fn params_as_str<T>(b: &[T]) -> String
where
    T: fmt::Display,
{
    // The result string
    let mut p = String::new();
    // For each element
    for n in b {
        // 'Display' it + a comma
        p += &format!("{}, ", n);
    }
    // Remove overshoot
    p.pop();
    p.pop();
    p
}

/// The call signature of a subroutine defined outside the language
pub type NativeSub = fn(&mut HashMap<String, Value>) -> Result<Value, Error>;

#[derive(Clone)]
/// The different types of subroutine
pub enum Subroutine {
    // Declared outside the language
    Native(NativeSub),
    // Defined in the language
    Internal(Box<Node>),
}

impl fmt::Debug for Subroutine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Native(_) => write!(f, "Subroutine {{ [NATIVE] }}"),
            Self::Internal(sub) => write!(f, "Subroutine {{ {} }}", ***sub),
        }
    }
}

impl fmt::Display for Subroutine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Subroutine::Native(_) => write!(f, "[NATIVE]"),
            Subroutine::Internal(node) => write!(f, "{}", ***node),
        }
    }
}

#[derive(Clone, Debug)]
/// A node in the syntax tree
pub enum NodeType {
    /// Use of a symbol
    Value(String),
    /// Operation e.g. +, left exp, right exp
    Operation(String, Box<Node>, Box<Node>),
    /// Condition, then, else (recure for else if)
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    /// The contents of a loop or if ext
    Block(Vec<Node>),
    /// An array literal
    Array(Vec<Node>),
    /// The thing what we are accessing, rabit hole of indexes
    ArrayAccess(Box<Node>, Vec<Node>),
    /// Name, Parameters, Body
    Subroutine(String, Vec<String>, Subroutine),
    /// Subroutine, Arguments
    Call(Box<Node>, Vec<Node>),
    /// The result of the node is a return value
    Return(Box<Node>),
    /// Decalare constant with value
    Constant(String, Box<Node>),
    /// Literal number
    Number(String),
    /// String literal
    Str(String),
    /// Use of True/False
    Boolean(bool),
    /// Invert the result
    Not(Box<Node>),
    /// Condition, Body, pre (true for while)
    Loop(Box<Node>, Box<Node>, bool),
    /// Var name, Inital value, final value, body
    For(String, Box<Node>, Box<Node>, Box<Node>),
    /// Display the result
    Output(Box<Node>),
    /// Get user input
    Input,
}

impl fmt::Display for NodeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeType::Value(s) => write!(f, "{}", s),
            NodeType::Operation(op, l, r) => write!(f, "({} {} {})", ***l, op, ***r),
            NodeType::If(cond, yes, no) => {
                if let Some(no) = no {
                    writeln!(f, "IF {} THEN\n{}ELSE\n{}ENDIF", ***cond, ***yes, ***no)
                } else {
                    writeln!(f, "IF {} THEN\n{}ENDIF", ***cond, ***yes)
                }
            }
            NodeType::Block(b) => {
                let mut p = String::new();
                for n in b {
                    p += &format!("{}\n", **n);
                }
                write!(f, "{}", p)
            }
            NodeType::Array(arr) => write!(f, "[{}]", params_as_str(&**arr)),
            NodeType::ArrayAccess(arr, rabbit) => write!(f, "({}){}", ***arr, {
                let mut p = String::new();
                for r in rabbit {
                    p += &format!("[{}]", **r);
                }
                p
            }),
            NodeType::Subroutine(n, a, b) => write!(
                f,
                "SUBROUTINE {} ({})\n{}ENDSUBROUTINE",
                n,
                params_as_str(a),
                b
            ),
            NodeType::For(name, initial, end, body) => write!(
                f,
                "FOR {} {} ({}) TO ({})\n{}ENDFOR",
                name,
                Tokens::ASSIGN,
                initial,
                end,
                body
            ),
            NodeType::Call(t, b) => write!(f, "{} ({})", t, params_as_str(b)),
            NodeType::Return(v) => write!(f, "RETURN ({})", v),
            NodeType::Constant(name, v) => {
                write!(f, "constant {} {} ({})", name, Tokens::ASSIGN, v)
            }
            NodeType::Number(v) => write!(f, "{}", v),
            NodeType::Str(v) => write!(f, "\"{}\"", v),
            NodeType::Boolean(v) => write!(f, "{}", v),
            NodeType::Not(v) => write!(f, "NOT ({})", v),
            NodeType::Loop(cond, body, pre) => {
                if *pre {
                    write!(f, "WHILE {}\n{}ENDWHILE", cond, body)
                } else {
                    write!(f, "REPEAT\n{}UNTIL {}", body, cond)
                }
            }
            NodeType::Output(exp) => write!(f, "OUTPUT ({})", exp),
            NodeType::Input => write!(f, "USERINPUT"),
        }
    }
}

impl Node {
    /// 'Execute' this node
    pub fn eval(&self, store: &mut HashMap<String, Value>) -> Result<Value, Error> {
        match &self.data {
            // Fetch a variable falling back to Value::None
            NodeType::Value(s) => store.get(s).map_or(Ok(Value::None), |s| Ok(s.clone())),
            NodeType::Block(b) => {
                // Initially a None value
                let mut last = Value::None;
                // For each node in block
                for n in b {
                    // Evaluate the statement
                    last = n.eval(store)?;
                    // If it was a `RETURN`
                    if let Value::Return(v) = last {
                        // Get the returned value
                        last = *v;
                        // End evaluation early
                        break;
                    }
                }
                // Return the last statements value
                Ok(last)
            }
            NodeType::Array(arr) => {
                // This is an array declaration
                // Create a new array of the same length
                let mut collection = Vec::with_capacity(arr.len());
                // For each elment in the declaration
                for elm in arr {
                    // Evaluate it and add the the array
                    collection.push(elm.eval(store)?);
                }
                // Return the array
                Ok(Value::Array(collection))
            }
            NodeType::ArrayAccess(arr, hole) => {
                // Get the value we are trying to index
                let arr = arr.eval(store)?;
                let mut curr = arr;
                // For each [] given
                for idx in hole {
                    // If the value is an array we can index it
                    if let Value::Array(arr) = curr.clone() {
                        // We only support integer indexes
                        let i = idx.eval(store)?.int(Some(idx.range()))?;
                        if i < 0.0 {
                            // Negative indexes don't work
                            return Err(Error::runtime(
                                format!("Attempt to access negative index {} in {}", i, curr),
                                Some(self.range),
                            ));
                        } else if i as usize >= arr.len() {
                            // Index is too large for the array
                            return Err(Error::runtime(
                                format!("Index {} is out of bounds for length {}", i, arr.len()),
                                Some(self.range),
                            ));
                        }
                        // Move down a level
                        curr = arr[i as usize].clone();
                    } else {
                        // But not other things
                        return Err(Error::runtime(
                            format!("Can't apply index notation to {}", curr),
                            Some(self.range),
                        ));
                    }
                }
                // Return the value at the point indexed
                Ok(curr)
            }
            NodeType::Operation(op, left, right) => match op.as_str() {
                // It assignment
                Tokens::ASSIGN => match (***left).clone() {
                    // Nice & simple assign to variable
                    NodeType::Value(left) => {
                        // The value
                        let val = right.eval(store)?;
                        // Insert the value fetching the old one
                        if let Some(Value::Constant(existing)) =
                            store.insert(left.clone(), val.clone())
                        {
                            // If the value was a constant error out
                            Err(Error::runtime(
                                format!(
                                    "Can't assign {} to constant {} with value {}",
                                    val, left, existing
                                ),
                                Some(self.range),
                            ))
                        } else {
                            // Return the assigned value
                            Ok(val)
                        }
                    }
                    // Overwrite a value in an array
                    NodeType::ArrayAccess(left, hole) => {
                        // The value we are trying to assign
                        let val = right.eval(store)?;
                        match &**left {
                            NodeType::Value(left) => {
                                // Remove the current value
                                let curr = store
                                    .insert(left.clone(), Value::None)
                                    .map_or(Value::None, |s| s);
                                match curr {
                                    // If the current value is an array
                                    // we can continue
                                    Value::Array(arr) => {
                                        // Make a mutable copy of the indexes
                                        let mut hole = hole.clone();
                                        // Reverse the list
                                        hole.reverse();
                                        // Call the helper
                                        // TODO: Avoid recursion
                                        let val = array_assign_helper(
                                            store,
                                            &mut hole,
                                            &mut Value::Array(arr),
                                            val,
                                        )?;
                                        // Store the updated array, we would
                                        // have already failed for a constant
                                        // array so don't need to check here
                                        store.insert(left.clone(), val.clone());
                                        Ok(val)
                                    }
                                    // Can't do this with non arrays
                                    _ => Err(Error::runtime(
                                        format!("Can't apply index notation to {}", curr),
                                        Some(self.range),
                                    )),
                                }
                            }
                            // You can't do this to something that isn't a variable
                            _ => Err(Error::runtime(
                                format!("Can't assign {} to {}", right, left),
                                Some(right.range()),
                            )),
                        }
                    }
                    // Can't assign to things that are not an array position
                    // or variable
                    _ => Err(Error::runtime(
                        format!("Can't assign {} to {}", right, left),
                        Some(right.range()),
                    )),
                },
                // Map operators to their appropriate method
                "=" => left.eval(store)?.eq(right.eval(store)?, Some(self.range)),
                Tokens::NOT_EQUAL => left.eval(store)?.neq(right.eval(store)?, Some(self.range)),
                "+" => left.eval(store)?.add(right.eval(store)?, Some(self.range)),
                "-" => left.eval(store)?.sub(right.eval(store)?, Some(self.range)),
                "*" => left.eval(store)?.mul(right.eval(store)?, Some(self.range)),
                "/" => left.eval(store)?.div(right.eval(store)?, Some(self.range)),
                "<" => left.eval(store)?.lt(right.eval(store)?, Some(self.range)),
                ">" => left.eval(store)?.gt(right.eval(store)?, Some(self.range)),
                Tokens::LESS_EQUAL => left.eval(store)?.lteq(right.eval(store)?, Some(self.range)),
                Tokens::GREATER_EQUAL => {
                    left.eval(store)?.gteq(right.eval(store)?, Some(self.range))
                }
                "DIV" => left.eval(store)?.idiv(right.eval(store)?, Some(self.range)),
                "MOD" => left.eval(store)?.imod(right.eval(store)?, Some(self.range)),
                "OR" => Ok(Value::Boolean(
                    left.eval(store)?.truthy(Some(self.range))?
                        || right.eval(store)?.truthy(Some(self.range))?,
                )),
                "AND" => Ok(Value::Boolean(
                    left.eval(store)?.truthy(Some(self.range))?
                        && right.eval(store)?.truthy(Some(self.range))?,
                )),
                // Something has gone wrong somewhere
                _ => Err(Error::runtime(
                    format!("Can't handle operator {}", op),
                    Some(self.range),
                )),
            },
            NodeType::Subroutine(name, takes, body) => {
                // Declaration of a subroutine
                let val = Value::Subroutine(takes.clone(), body.clone());
                // Store it
                store.insert(name.clone(), val.clone());
                Ok(val)
            }
            // Evaluate a return expression
            NodeType::Return(v) => Ok(Value::Return(Box::new(v.eval(store)?))),
            NodeType::Constant(name, value) => {
                // Declaration of a constant
                let value = Value::Constant(Box::new(value.eval(store)?));
                store.insert(name.clone(), value.clone());
                Ok(value)
            }
            // Pass though strings
            NodeType::Str(v) => Ok(Value::Textual(v.clone())),
            NodeType::Number(v) => {
                // Parse a float from the token
                let num = v.parse();
                // Get the result or error out
                let num = num.or_else(|_| {
                    Err(Error::parse(
                        format!("Badly formatted number {}", v),
                        self.range,
                    ))
                })?;
                Ok(Value::Number(num))
            }
            // Pass though boolean values
            NodeType::Boolean(b) => Ok(Value::Boolean(*b)),
            NodeType::Call(func, arguments) => {
                // Call to a subroutine
                // Fetch the thing trying to be called
                if let Value::Subroutine(params, body) = func.eval(store)? {
                    // Check the number of arguments matches parameters
                    if arguments.len() == params.len() {
                        // Make a copy of the parent scope
                        // Functions have read only access to varibles in
                        // the global scope at the point they are called.
                        // Note this isn't constnant: If 'a' exists with
                        // value 2 in the parent they that can be accessed
                        // within the subroutine, hover an assignment to 'a'
                        // (say 42) would only affect the functions local
                        // scope. This behaviour is probably a bug not a
                        // feature but is an easy way to let subs call
                        // each other
                        let mut scope = store.clone();
                        // Pair up parameters & arguments
                        for (param, arg) in params.iter().zip(arguments) {
                            // Store arg as param in the subs scope
                            scope.insert(param.to_string(), arg.eval(store)?);
                        }
                        // What type of sub are we
                        match body {
                            // Evaluate the body block in the context of scope
                            Subroutine::Internal(body) => body.eval(&mut scope),
                            // Call the underlying native method
                            Subroutine::Native(func) => func(&mut scope),
                        }
                    } else {
                        // Number of arguments must match parameters
                        Err(Error::runtime(
                            format!(
                                "Wrong number of parameters need ({}) got ({})",
                                params_as_str(&params),
                                params_as_str(&arguments)
                            ),
                            Some(self.range),
                        ))
                    }
                } else {
                    // Can't call something that isn't a subroutine
                    Err(Error::runtime(
                        format!("Can't call {}", func),
                        Some(self.range),
                    ))
                }
            }
            // Return the inverted boolean value
            NodeType::Not(n) => Ok(Value::Boolean(!n.eval(store)?.truthy(Some(n.range()))?)),
            NodeType::If(cond, yes, no) => {
                // If the condition is true
                if cond.eval(store)?.truthy(Some(cond.range()))? {
                    // Run the then block
                    yes.eval(store)
                } else if let Some(no) = no {
                    // Otherwise the else block (if any)
                    no.eval(store)
                } else {
                    // Fallback to no value
                    Ok(Value::None)
                }
            }
            NodeType::Loop(cond, body, pre) => {
                let mut last = Value::None;
                // An indefinite loop
                loop {
                    // Pre indicates a while loop
                    if *pre {
                        // So check the condition
                        if cond.eval(store)?.truthy(Some(cond.range()))? {
                            // Then run the body
                            last = body.eval(store)?;
                        } else {
                            // End the loop when the
                            // condition isn't met
                            break;
                        }
                    } else {
                        // A repeat loop so run the body
                        last = body.eval(store)?;
                        // Then check the condition
                        if cond.eval(store)?.truthy(Some(cond.range()))? {
                            // Ending the loop if it is met
                            break;
                        }
                    }
                }
                Ok(last)
            }
            NodeType::Output(n) => {
                // Print the value of an expression
                let v = n.eval(store)?;
                println!(
                    "{}",
                    match v {
                        Value::Number(n) => n.to_string(),
                        _ => v.string(Some(n.range()))?,
                    }
                );
                Ok(v)
            }
            NodeType::Input => {
                // Buffer to read into
                let mut input = String::new();
                // Read a line into the buffer
                io::stdin().read_line(&mut input).or_else(|_| {
                    Err(Error::runtime(
                        "Failed to get input".into(),
                        Some(self.range),
                    ))
                })?;
                // Remove \n from the end
                input.pop();
                Ok(Value::Textual(input))
            }
            NodeType::For(name, start, end, body) => {
                // Initial value
                let start = start.eval(store)?.int(Some(start.range()))?;
                // Final value
                let end = end.eval(store)?.int(Some(end.range()))?;
                // Keep record of the orginal value
                let before = store.insert(name.clone(), Value::Number(start));
                // Are we counting up or down
                let change = if start < end { 1.0 } else { -1.0 };
                let goingup = change > 0.0;
                // The counter
                let mut current = start;
                Ok(loop {
                    // Run the body
                    let last = body.eval(store)?;
                    // in/decrement the counter
                    current += change;
                    // Update the variable
                    store.insert(name.clone(), Value::Number(current));
                    // If we have come to then end
                    if (goingup && current > end) || (!goingup && current < end) {
                        // Restore the orignal value
                        if let Some(v) = before {
                            store.insert(name.clone(), v.clone());
                        }
                        // End the loop
                        break last;
                    }
                })
            }
        }
    }
}

/// Mutates an array
fn array_assign_helper(
    store: &mut HashMap<String, Value>,
    hole: &mut Vec<Node>,
    of: &mut Value,
    val: Value,
) -> Result<Value, Error> {
    // If there a further level to iterate into
    if let Some(idx) = hole.pop() {
        // Indexes must be integer
        let i = idx.eval(store)?.int(Some(idx.range()))?;
        // Can only index arrays
        if let Value::Array(ref mut arr) = of {
            // An index must be positive
            if i < 0.0 {
                Err(Error::runtime(
                    format!("Attempt to access negative index {}", i),
                    Some(idx.range()),
                ))
            // And within the bounds of the array
            } else if i as usize >= arr.len() {
                Err(Error::runtime(
                    format!("Index {} is out of bounds for length {}", i, arr.len()),
                    Some(idx.range()),
                ))
            } else {
                // Check if we need to go a level deeper
                let res = array_assign_helper(store, hole, &mut arr[i as usize], val)?;
                // Update and return the array
                arr[i as usize] = res.clone();
                Ok(Value::Array(arr.to_vec()))
            }
        } else {
            Err(Error::runtime(
                format!("Can't apply index notation to {}", of),
                Some(idx.range()),
            ))
        }
    } else {
        // Reached the lowest level, start moving back
        // up the stack
        Ok(val)
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    data: NodeType,
    range: Range,
}

impl Node {
    pub fn new(data: NodeType, range: Range) -> Self {
        Self { data, range }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl Deref for Node {
    type Target = NodeType;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
