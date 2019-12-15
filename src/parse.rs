use crate::error::Syntax;
use crate::location::Range;
use crate::node::Node;
use crate::node::Subroutine;
use crate::token::Token;
use crate::token::TokenType;
use crate::token::Tokens;

pub type NodeResult = Result<Node, Syntax>;

/// Returns a Vec of T and the matched close token
fn delimited<T>(
    input: &mut Tokens,
    stop: &[TokenType],
    separator: &TokenType,
    parser: &mut dyn FnMut(&mut Tokens) -> Result<T, Syntax>,
    err: &str,
) -> Result<(Vec<T>, Token), Syntax> {
    // The list we will return
    let mut elements: Vec<T> = Vec::new();
    // Are we still on the first element
    let mut first = true;
    // Skip any newlines
    input.absorb_newlines()?;
    // Will be the matched close token
    let matched = 'read: loop {
        // Cache the start
        let start = input.here();
        // If there is more tokens available
        if input.peek()?.is_some() {
            // We have to support multiple close tokens for things like
            // if statments where we could be looking for ELSE or ENDIF
            for stop in stop {
                // If we matched a stop token
                if let Some(m) = input.is(&stop)? {
                    // Move past it
                    input.skip(&*m)?;
                    // End the read loop
                    break 'read Ok(m);
                }
            }
            // If we are still at the first element
            if first {
                // Clear the flag
                first = false;
            } else {
                // Every element past the first should be preceeded
                // by the seperator token so move over it
                input.skip(&separator)?;
                // The next element might be on a newline
                input.absorb_newlines()?;
            }
            // Same as above
            for stop in stop {
                if let Some(m) = input.is(&stop)? {
                    input.skip(&m)?;
                    break 'read Ok(m);
                }
            }
            // Call the provided parser appending the result
            elements.push(parser(input)?);
        } else {
            // Run out of input without finding close token
            break Err(Syntax::eof(err.into(), Range::new(start, input.here())));
        }
    };
    // Return the elements
    Ok((elements, matched?))
}

/// Parses an if statement
fn ifelse(input: &mut Tokens) -> NodeResult {
    input.absorb_newlines()?;
    // Parse the condition
    let cond = expression(input)?;
    // We allow newlines here
    input.absorb_newlines()?;
    // Expect THEN
    input.skip(&TokenType::Keyword("THEN".into()))?;
    // Parse the then block that may end with
    // an ELSE or ENDIF keyword
    let (yes, m) = block(
        input,
        &[
            TokenType::Keyword("ELSE".into()),
            TokenType::Keyword("ENDIF".into()),
        ],
        "Still in if statement",
    )?;
    // If the closing match was ELSE
    if *m == TokenType::Keyword("ELSE".into()) {
        // And this is an ELSE IF
        if input.is(&TokenType::Keyword("IF".into()))?.is_some() {
            input.next()?;
            // Recure to handle the ELSE IF, as parse as parsing
            // goes ELSE IF is just a shorthand for nested IF
            let elif = ifelse(input)?;
            Ok(Node::If(
                Box::new(cond),
                Box::new(yes),
                Some(Box::new(elif)),
            ))
        } else {
            // No more ELSE IFs, read the ELSE
            let (no, _) = block(
                input,
                &[TokenType::Keyword("ENDIF".into())],
                "Still in if statement",
            )?;
            Ok(Node::If(Box::new(cond), Box::new(yes), Some(Box::new(no))))
        }
    } else {
        // No else block
        Ok(Node::If(Box::new(cond), Box::new(yes), None))
    }
}

/// Checks if node should be wrapped in ArrayAccess
fn indexed(input: &mut Tokens, node: Node) -> NodeResult {
    // If there is a [ parse the indexing
    if input.is(&TokenType::Punctuation("[".into()))?.is_some() {
        // Stores the levels of indexing
        let mut hole = Vec::new();
        loop {
            // Skip the opening bracket
            input.skip(&TokenType::Punctuation("[".into()))?;
            // Parse the index
            hole.push(expression(input)?);
            // Move over the close bracket
            input.skip(&TokenType::Punctuation("]".into()))?;
            // If there isn't anymore opening [
            if input.is(&TokenType::Punctuation("[".into()))?.is_none() {
                // End the loop
                break;
            }
        }
        // Wrap the passed node with the parsed indexing
        Ok(Node::ArrayAccess(Box::new(node), hole))
    } else {
        // Nope just pass the node though
        Ok(node)
    }
}

/// node is possibly a call taget
fn call(input: &mut Tokens, node: Node) -> NodeResult {
    // If the next token is ( it is a call
    let node = match input.is(&TokenType::Punctuation(String::from("(")))? {
        // Parse any argument and wrap 'node' in a call
        Some(_) => {
            input.next()?;
            Node::Call(
            Box::new(node),
            delimited(
                input,
                &[TokenType::Punctuation(")".into())],
                &TokenType::Punctuation(",".into()),
                &mut expression,
                "Still providing arguments",
            )?.0,
        )}
        ,
        // It isn't just pass the node though
        None => node,
    };
    // The result may have index notation
    // (a[b]) applied to it
    indexed(input, node)
}

/// Part of a greater expression
fn atom(input: &mut Tokens) -> NodeResult {
    // Skip any random newlines
    input.absorb_newlines()?;
    let node = if let Some(t) = input.next()? {
        // Match the token
        match &*t {
            // It's a keyword
            TokenType::Keyword(kw) => match kw.as_str() {
                // A subroutine declaration
                "SUBROUTINE" => {
                    // Be forgiving about newlines
                    input.absorb_newlines()?;
                    // Where are we now
                    let start = input.here();
                    // Now we need a name
                    if let Some(name) = input.next()? {
                        if let TokenType::Value(name) = &*name {
                            // Parse the parameters
                            input.skip(&TokenType::Punctuation("(".into()))?;
                            let (params, _) = delimited(
                                input,
                                &[TokenType::Punctuation(")".into())],
                                &TokenType::Punctuation(",".into()),
                                &mut |input| {
                                    // We need a token
                                    if let Some(p) = input.next()? {
                                        match &*p {
                                            // That's a name
                                            TokenType::Value(p) => Ok(p.clone()),
                                            _ => Err(Syntax::new(
                                                "Expected param name".into(),
                                                p.range(),
                                            )),
                                        }
                                    } else {
                                        Err(Syntax::eof(
                                            "Still defining parameters".into(),
                                            Range::new(input.here(), input.here()),
                                        ))
                                    }
                                },
                                "Still defining parameters",
                            )?;
                            // Get the code block
                            let (body, _) = block(
                                input,
                                &[TokenType::Keyword("ENDSUBROUTINE".into())],
                                "Still in subroutine",
                            )?;
                            // Wrap together the name, params & body
                            Ok(Node::Subroutine(
                                name.clone(),
                                params,
                                Subroutine::Internal(Box::new(body)),
                            ))
                        } else {
                            Err(Syntax::new(
                                format!("Expecting subroutine name"),
                                name.range(),
                            ))
                        }
                    } else {
                        Err(Syntax::eof(
                            "Expecting subroutine name".into(),
                            Range::new(start, input.here()),
                        ))
                    }
                }
                // A loop that is run atleast once
                "REPEAT" => {
                    // Grab the code block
                    let (body, _) = block(
                        input,
                        &[TokenType::Keyword("UNTIL".into())],
                        "Still in repeat statement",
                    )?;
                    // Get the limiting expression
                    let cond = expression(input)?;
                    Ok(Node::Loop(Box::new(cond), Box::new(body), false))
                }
                // Only run whilst an expression in true
                "WHILE" => {
                    // The expression
                    let cond = expression(input)?;
                    // The loop body
                    let (body, _) = block(
                        input,
                        &[TokenType::Keyword("ENDWHILE".into())],
                        "Stil in while loop",
                    )?;
                    Ok(Node::Loop(Box::new(cond), Box::new(body), true))
                }
                // We seem to have reached the end of a block without being in one
                "UNTIL" | "ENDWHILE" | "ENDSUBROUTINE" | "ELSE" | "ENDIF" => {
                    Err(Syntax::new(format!("Unexpected {}", kw), t.range()))
                }
                // The next expression is the response of the block
                "RETURN" => Ok(Node::Return(Box::new(expression(input)?))),
                // If statements are complex so have
                // there own parsing methon
                "IF" => ifelse(input),
                // A loop with a counter
                "FOR" => {
                    input.absorb_newlines()?;
                    // We need a variable name to store the counter in
                    if let Some(name) = input.next()? {
                        if let TokenType::Value(name) = &*name {
                            // For stylistic reasons we have an assign operator now
                            input.skip(&TokenType::Operator(Tokens::ASSIGN.into()))?;
                            // Initial value of the counter
                            let start = expression(input)?;
                            input.skip(&TokenType::Keyword("TO".into()))?;
                            // Final counter value
                            let end = expression(input)?;
                            // Loop contents
                            let (body, _) = block(
                                input,
                                &[TokenType::Keyword("ENDFOR".into())],
                                "Still in for loop",
                            )?;
                            Ok(Node::For(
                                name.clone(),
                                Box::new(start),
                                Box::new(end),
                                Box::new(body),
                            ))
                        } else {
                            Err(Syntax::eof("Expected variable name".into(), name.range()))
                        }
                    } else {
                        Err(Syntax::eof(
                            "Expected variable name".into(),
                            Range::new(input.here(), input.here()),
                        ))
                    }
                }
                // Define a constant variable
                "constant" => {
                    // The name of the constant
                    if let Some(name) = input.next()? {
                        if let TokenType::Value(name) = &*name {
                            input.skip(&TokenType::Operator(Tokens::ASSIGN.into()))?;
                            // The value to give the constant
                            let val = expression(input)?;
                            Ok(Node::Constant(name.clone(), Box::new(val)))
                        } else {
                            Err(Syntax::eof("Expected constant name".into(), name.range()))
                        }
                    } else {
                        Err(Syntax::eof(
                            "Expected constant name".into(),
                            Range::new(input.here(), input.here()),
                        ))
                    }
                }
                // Marker that the following expression should be
                // treated as boolean and inverted
                "NOT" => Ok(Node::Not(Box::new(expression(input)?))),
                // Requires the next result to be a string
                // and prints it to stdout
                "OUTPUT" => Ok(Node::Output(Box::new(expression(input)?))),
                // A simple request for input
                "USERINPUT" => Ok(Node::Input),
                // A literal boolean
                "True" => Ok(Node::Boolean(true)),
                // And it's inverse
                "False" => Ok(Node::Boolean(false)),
                _ => Err(Syntax::new(format!("Unimplemented {}", kw), t.range())),
            },
            TokenType::Punctuation(p) => match p.as_str() {
                // A sub expression
                "(" => {
                    // Parse the embedded exp
                    let exp = expression(input)?;
                    // Close the bracket
                    input.skip(&TokenType::Punctuation(String::from(")")))?;
                    Ok(exp)
                }
                // Array declaration
                "[" => {
                    // Just a load of comma seperated expressions
                    let (params, _) = delimited(
                        input,
                        &[TokenType::Punctuation("]".into())],
                        &TokenType::Punctuation(",".into()),
                        &mut expression,
                        "Still in array",
                    )?;
                    Ok(Node::Array(params))
                }
                _ => Err(Syntax::new(
                    format!("Unexpected punctuation {}", p),
                    t.range(),
                )),
            },
            // A reference to a variable
            TokenType::Value(v) => Ok(Node::Value(v.clone())),
            // Literal number
            TokenType::Number(v) => Ok(Node::Number(v.clone())),
            // A string
            TokenType::Text(v) => Ok(Node::Str(v.clone())),
            _ => Err(Syntax::new(format!("Unexpected token: {}", *t), t.range())),
        }
    } else {
        // There should have been further tokens
        Err(Syntax::eof(
            "Expected Expression".into(),
            Range::new(input.here(), input.here()),
        ))
    };
    // This might be a call
    call(input, node?)
}

/// A block, like the contents of an IF
fn block(input: &mut Tokens, close: &[TokenType], err: &str) -> Result<(Node, Token), Syntax> {
    // A block is just a series of expressions seperated by newlines
    let (b, m) = delimited(
        input,
        close,
        &TokenType::Punctuation("\n".into()),
        &mut expression,
        err,
    )?;
    // Return the result with the Vec<Node> wrapped as a block
    Ok((Node::Block(b), m))
}

/// left maybe part of a greater operation
fn binary(input: &mut Tokens, left: Node, parent: u8) -> NodeResult {
    // The next token is an operator so this is an operation
    if let Some(tok) = input.peek()? {
        if let TokenType::Operator(v) = &*tok {
            // Lookup the precedence of the operator
            let this = input
                .precedence(&v)
                .ok_or_else(|| Syntax::new("Unknown operator".into(), tok.range()))?;
            // I'm more important
            // (I'm afraid i don't totally understand
            // this logic but it seems to work)
            if this > parent {
                // Move past the operator
                input.next()?;
                // Parse the right hand side
                let atom = atom(input)?;
                // The right hand side may itself be an operation
                let right = binary(input, atom, this)?;
                // And move back up the stack
                return binary(
                    input,
                    Node::Operation(v.clone(), Box::new(left), Box::new(right)),
                    parent,
                );
            }
        }
    }
    // Not an operation, pass the node though
    Ok(left)
}

/// Parse a compleate expression
fn expression(toks: &mut Tokens) -> NodeResult {
    // Parse the left hand side
    let atom = atom(toks)?;
    // Parse any right hand side
    let node = binary(toks, atom, 0)?;
    // Perhaps we are calling the node
    call(toks, node)
}

/// Things that can be parsed into a Node
pub trait Parsable {
    fn parse(&mut self) -> NodeResult;
}

impl Parsable for Tokens {
    fn parse(&mut self) -> NodeResult {
        // Parse the root of the program
        // The root is a list of nodes
        let mut prog: Vec<Node> = Vec::new();
        // Move past any opening newlines
        self.absorb_newlines()?;
        // While there are remaining tokens
        while let Some(_) = self.peek()? {
            // Parse an expression and add it to list
            prog.push(expression(self)?);
            // Skip any following newline
            self.absorb_newlines()?;
        }
        if let Some(end) = self.next()? {
            Err(Syntax::new(
                format!("Expected the end, got {}", *end),
                end.range(),
            ))
        } else {
            // Return the root node
            Ok(Node::Block(prog))
        }
    }
}
