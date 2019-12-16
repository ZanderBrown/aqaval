use crate::error::Error;
use crate::location::Range;
use crate::node::Node;
use crate::node::NodeType;
use crate::node::Subroutine;
use crate::token::Token;
use crate::token::TokenType;
use crate::token::Tokens;

pub type NodeResult = Result<Node, Error>;

/// Returns a Vec of T and the matched close token
fn delimited<T>(
    input: &mut Tokens,
    stop: &[TokenType],
    separator: &TokenType,
    parser: &mut dyn FnMut(&mut Tokens) -> Result<T, Error>,
    err: &str,
) -> Result<(Vec<T>, Token), Error> {
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
            break Err(Error::eof(err.into(), Range::new(start, input.here())));
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
            let r = cond.range() + elif.range();
            Ok(Node::new(
                NodeType::If(Box::new(cond), Box::new(yes), Some(Box::new(elif))),
                r,
            ))
        } else {
            // No more ELSE IFs, read the ELSE
            let (no, _) = block(
                input,
                &[TokenType::Keyword("ENDIF".into())],
                "Still in if statement",
            )?;
            let r = cond.range() + no.range();
            Ok(Node::new(
                NodeType::If(Box::new(cond), Box::new(yes), Some(Box::new(no))),
                r,
            ))
        }
    } else {
        // No else block
        let r = cond.range() + yes.range();
        Ok(Node::new(
            NodeType::If(Box::new(cond), Box::new(yes), None),
            r,
        ))
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
        if let Some(ref last) = hole.last() {
            let r = node.range() + last.range();
            Ok(Node::new(NodeType::ArrayAccess(Box::new(node), hole), r))
        } else {
            let r = node.range();
            Ok(Node::new(NodeType::ArrayAccess(Box::new(node), hole), r))
        }
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
            let args = delimited(
                input,
                &[TokenType::Punctuation(")".into())],
                &TokenType::Punctuation(",".into()),
                &mut expression,
                "Still providing arguments",
            )?.0;
            if let Some(ref last) = args.last() {
                let r = node.range() + last.range();
            Node::new(NodeType::Call(
            Box::new(node),
            args,
            ), r)
        } else {
            let r = node.range();
            Node::new(NodeType::Call(
                Box::new(node),
                args,
                ), r)
            }
    }
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
                    if let Some(nametok) = input.next()? {
                        if let TokenType::Value(name) = &*nametok {
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
                                            _ => Err(Error::parse(
                                                "Expected param name".into(),
                                                p.range(),
                                            )),
                                        }
                                    } else {
                                        Err(Error::eof(
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
                            let r = nametok.range() + body.range();
                            // Wrap together the name, params & body
                            Ok(Node::new(
                                NodeType::Subroutine(
                                    name.clone(),
                                    params,
                                    Subroutine::Internal(Box::new(body)),
                                ),
                                r,
                            ))
                        } else {
                            Err(Error::parse(
                                format!("Expecting subroutine name"),
                                nametok.range(),
                            ))
                        }
                    } else {
                        Err(Error::eof(
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
                    let r = cond.range() + body.range();
                    Ok(Node::new(
                        NodeType::Loop(Box::new(cond), Box::new(body), false),
                        r,
                    ))
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
                    let r = cond.range() + body.range();
                    Ok(Node::new(
                        NodeType::Loop(Box::new(cond), Box::new(body), true),
                        r,
                    ))
                }
                // We seem to have reached the end of a block without being in one
                "UNTIL" | "ENDWHILE" | "ENDSUBROUTINE" | "ELSE" | "ENDIF" => {
                    Err(Error::parse(format!("Unexpected {}", kw), t.range()))
                }
                // The next expression is the response of the block
                "RETURN" => {
                    let exp = expression(input)?;
                    let r = t.range() + exp.range();
                    Ok(Node::new(NodeType::Return(Box::new(exp)), r))
                }
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
                            let r = t.range() + body.range();
                            Ok(Node::new(
                                NodeType::For(
                                    name.clone(),
                                    Box::new(start),
                                    Box::new(end),
                                    Box::new(body),
                                ),
                                r,
                            ))
                        } else {
                            Err(Error::eof("Expected variable name".into(), name.range()))
                        }
                    } else {
                        Err(Error::eof(
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
                            let r = t.range() + val.range();
                            Ok(Node::new(
                                NodeType::Constant(name.clone(), Box::new(val)),
                                r,
                            ))
                        } else {
                            Err(Error::eof("Expected constant name".into(), name.range()))
                        }
                    } else {
                        Err(Error::eof(
                            "Expected constant name".into(),
                            Range::new(input.here(), input.here()),
                        ))
                    }
                }
                // Marker that the following expression should be
                // treated as boolean and inverted
                "NOT" => {
                    let exp = expression(input)?;
                    let r = t.range() + exp.range();
                    Ok(Node::new(NodeType::Not(Box::new(exp)), r))
                }
                // Requires the next result to be a string
                // and prints it to stdout
                "OUTPUT" => {
                    let exp = expression(input)?;
                    let r = t.range() + exp.range();
                    Ok(Node::new(NodeType::Output(Box::new(exp)), r))
                }
                // A simple request for input
                "USERINPUT" => Ok(Node::new(NodeType::Input, t.range())),
                // A literal boolean
                "True" => Ok(Node::new(NodeType::Boolean(true), t.range())),
                // And it's inverse
                "False" => Ok(Node::new(NodeType::Boolean(false), t.range())),
                _ => Err(Error::parse(format!("Unimplemented {}", kw), t.range())),
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
                    let (params, e) = delimited(
                        input,
                        &[TokenType::Punctuation("]".into())],
                        &TokenType::Punctuation(",".into()),
                        &mut expression,
                        "Still in array",
                    )?;
                    Ok(Node::new(NodeType::Array(params), t.range() + e.range()))
                }
                _ => Err(Error::parse(
                    format!("Unexpected punctuation {}", p),
                    t.range(),
                )),
            },
            // A reference to a variable
            TokenType::Value(v) => Ok(Node::new(NodeType::Value(v.clone()), t.range())),
            // Literal number
            TokenType::Number(v) => Ok(Node::new(NodeType::Number(v.clone()), t.range())),
            // A string
            TokenType::Text(v) => Ok(Node::new(NodeType::Str(v.clone()), t.range())),
            _ => Err(Error::parse(format!("Unexpected token: {}", *t), t.range())),
        }
    } else {
        // There should have been further tokens
        Err(Error::eof(
            "Expected Expression".into(),
            Range::new(input.here(), input.here()),
        ))
    };
    // This might be a call
    call(input, node?)
}

/// A block, like the contents of an IF
fn block(input: &mut Tokens, close: &[TokenType], err: &str) -> Result<(Node, Token), Error> {
    // A block is just a series of expressions seperated by newlines
    let (b, m) = delimited(
        input,
        close,
        &TokenType::Punctuation("\n".into()),
        &mut expression,
        err,
    )?;
    // Return the result with the Vec<Node> wrapped as a block
    if let Some(ref first) = b.first() {
        let r = first.range() + m.range();
        Ok((Node::new(NodeType::Block(b), r), m))
    } else {
        Ok((Node::new(NodeType::Block(b), m.range()), m))
    }
}

/// left maybe part of a greater operation
fn binary(input: &mut Tokens, left: Node, parent: u8) -> NodeResult {
    // The next token is an operator so this is an operation
    if let Some(tok) = input.peek()? {
        if let TokenType::Operator(v) = &*tok {
            // Lookup the precedence of the operator
            let this = input
                .precedence(&v)
                .ok_or_else(|| Error::parse("Unknown operator".into(), tok.range()))?;
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
                let r = tok.range() + right.range();
                return binary(
                    input,
                    Node::new(
                        NodeType::Operation(v.clone(), Box::new(left), Box::new(right)),
                        r,
                    ),
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
        let start = self.here();
        // While there are remaining tokens
        while let Some(_) = self.peek()? {
            // Parse an expression and add it to list
            prog.push(expression(self)?);
            // Skip any following newline
            self.absorb_newlines()?;
        }
        let end = self.here();
        if let Some(end) = self.next()? {
            Err(Error::parse(
                format!("Expected the end, got {}", *end),
                end.range(),
            ))
        } else {
            // Return the root node
            Ok(Node::new(NodeType::Block(prog), Range::new(start, end)))
        }
    }
}
