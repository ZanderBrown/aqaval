#![allow(unknown_lints)]
use builtin::init;
use error::Syntax;
use input::Stream;
use parse::Parsable;
use token::Tokens;

mod builtin;
mod error;
mod eval;
mod input;
mod node;
mod parse;
mod token;

use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;

use getopts::Options;
use linefeed::{Interface, ReadResult};

// Arguments parsing lib
extern crate getopts;
// For the builtin random_int function
extern crate rand;
// Nice way to get user input in the repl
extern crate linefeed;

/// When run without argument we start a REPL prompt
fn repl() -> Result<(), Box<dyn Error>> {
    // Where variables are stored
    let mut store = HashMap::new();
    // Adds all the built in function to store
    init(&mut store);

    let reader = Interface::new("aqaval")?;

    // The mainloop
    'repl: loop {
        // Input buffer for multiline statements
        let mut building = String::new();
        // Loop continues until a statement is
        // syntatically complete or invalid
        let ast = 'moreinput: loop {
            // We change prompt whilst building a multiline statement
            let prompt = if building.is_empty() { "← " } else { "- " };
            reader.set_prompt(prompt)?;
            // Try and get a line of user input
            match reader.read_line()? {
                // We got a line
                ReadResult::Input(line) => {
                    // They just hit enter without typing anything
                    if line.is_empty() {
                        // Get another line
                        continue;
                    }
                    // Add the line to history
                    reader.add_history(line.clone());
                    // If we aren't building a multiline
                    if building.is_empty() {
                        // Match the line aganist some repl
                        // specific keywords
                        match line.trim() {
                            // Quits the REPL
                            ".quit" => return Ok(()),
                            // Shows some information and reads a new line
                            ".about" => {
                                println!("AQAVal by Zander Brown");
                                continue 'moreinput;
                            }
                            _ => (),
                        }
                    }
                    // Add the line to the buffer
                    building += &(line + "\n");
                    // Try to parse the buffer
                    match Tokens::from(Stream::from(building.clone())).parse() {
                        // Success! Break out the loop returning
                        // the parsed statement
                        Ok(n) => break n,
                        // It failed because further input was expected
                        Err(e) => {
                            if let Syntax::EndOfInput(_) = e {
                                // So read more input to the buffer
                                continue 'moreinput;
                            // Bad syntax
                            } else {
                                // Report the error
                                println!("↑ {}", e);
                                // Clear the buffer and try again
                                continue 'repl;
                            }
                        }
                    }
                }
                ReadResult::Eof => {
                    // Quit the REPL
                    println!("[QUIT]");
                    return Ok(());
                }
                r => println!("Problem reading input: {:?}", r),
            }
        };
        // Evaluate the parsed statement
        match ast.eval(&mut store) {
            // Print the result
            Ok(v) => println!("→ {}", v),
            // Print the runtime error
            Err(e) => println!("↑ {}", e),
        }
    }
}

// The entry point
fn main() -> Result<(), Box<dyn Error>> {
    // Fetch the arguments into an array
    let arguments: Vec<String> = env::args().collect();
    let program = arguments[0].clone();

    // Setup the argument parser
    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optflag("s", "vars", "show the final state of variables");
    opts.optflag("a", "ast", "show the parsed ast");
    opts.optflag("r", "res", "show the evaluated value");
    // Try and parse the arguments
    let matches = match opts.parse(&arguments[1..]) {
        // Store the result
        Ok(m) => m,
        // Something went wrong
        Err(f) => {
            // Display the message
            println!("{}", f);
            // Quit early
            return Ok(());
        }
    };
    // Help was selected
    if matches.opt_present("h") {
        // Show some information
        let brief = format!("Usage: {} FILE", program);
        print!("{}", opts.usage(&brief));
        // Quit
        return Ok(());
    }
    // If a file wasn't passed
    let input = if matches.free.is_empty() {
        // Run the REPL
        repl()?;
        // And exit afterwards
        return Ok(());
    } else {
        // Get the filename
        matches.free[0].clone()
    };
    // Check the file exists
    let path = Path::new(&input);
    if path.exists() {
        // Read the file into a string
        match read_to_string(path) {
            Ok(script) => {
                // Root scope of variables
                let mut store = HashMap::new();
                // Setup the builtin functions
                init(&mut store);
                // Parse the string
                match Tokens::from(Stream::from(script)).parse() {
                    Ok(n) => {
                        // If they asked for the reverse AST debug option
                        if matches.opt_present("a") {
                            // Print it
                            println!("Reverse AST\n{}", n);
                        }
                        // Run the users program!
                        match n.eval(&mut store) {
                            // If they asked for the last result
                            Ok(v) => {
                                if matches.opt_present("r") {
                                    // Print it
                                    println!("-> {}", v);
                                }
                            }
                            // Something isn't right we their logic
                            Err(e) => println!("{}", e),
                        }
                    }
                    // Looks like the syntax was wrong
                    Err(e) => println!("{}", e),
                }
                // If variable dumping was requested
                if matches.opt_present("s") {
                    println!("Variables:");
                    // Looks though the key/values
                    for (k, v) in store {
                        // And print them
                        println!("{} = {}", k, v);
                    }
                }
            }
            // Or not...
            Err(e) => println!("Can't read {}: {}", input, e),
        }
    } else {
        // It didn't
        println!("{} doesn't exist", input);
    }

    Ok(())
}
