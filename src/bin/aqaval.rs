#![allow(unknown_lints)]

use std::collections::HashMap;
use std::env;
use std::error;
use std::fs::read_to_string;
use std::path::Path;

use ansi_term::{Colour, Style};
use getopts::Options;
use linefeed::{Interface, ReadResult};

use aqaval::builtin;
use aqaval::Error;
use aqaval::Parsable;
use aqaval::Stream;
use aqaval::Tokens;

pub fn print_error(err: &Error, src: &Tokens) {
    match err {
        Error::Parse(_, at) | Error::EndOfInput(_, at) | Error::Runtime(_, Some(at)) => {
            let line = Colour::Purple
                .bold()
                .paint(format!("{}", at.start().line()));
            let sep = Colour::White.dimmed().paint("|");
            if let Some(source) = src.get_source(at.start().line()) {
                eprintln!("{} {}{}", line, sep, Style::default().bold().paint(source));
            } else {
                eprintln!("{} {} [err]", line, sep);
            }
            eprintln!(
                "{:idt$} {}{:pad$}{:↑>num$}",
                " ",
                sep,
                "",
                "↑",
                idt = line.len(),
                pad = at.start().column(),
                num = at.end().column() - at.start().column()
            );
            eprintln!(
                "{:idt$} {}{:pad$}{}",
                " ",
                sep,
                " ",
                err.message(),
                idt = line.len(),
                pad = at.start().column()
            );
        }
        _ => eprintln!("{}", err.message()),
    }
}

/// When run without argument we start a REPL prompt
fn repl() -> Result<(), Box<dyn error::Error>> {
    // Where variables are stored
    let mut store = HashMap::new();
    // Adds all the built in function to store
    builtin(&mut store);

    let reader = Interface::new("aqaval")?;

    // The mainloop
    'repl: loop {
        // Input buffer for multiline statements
        let mut building = String::new();
        // Loop continues until a statement is
        // syntatically complete or invalid
        let (ast, tokens) = 'moreinput: loop {
            // We change prompt whilst building a multiline statement
            if building.is_empty() {
                let y = Colour::Yellow.bold();
                reader.set_prompt(&format!(
                    "\x01{}\x02{}\x01{}\x02",
                    y.prefix(),
                    "← ",
                    y.suffix()
                ))?;
            } else {
                let y = Colour::Yellow.normal();
                reader.set_prompt(&format!(
                    "\x01{}\x02{}\x01{}\x02",
                    y.prefix(),
                    "- ",
                    y.suffix()
                ))?;
            }
            // Try and get a line of user input
            if let ReadResult::Input(line) = reader.read_line()? {
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
                // Token stream
                let mut tokens = Tokens::from(Stream::from(building.clone()));
                // Try to parse the buffer
                match tokens.parse() {
                    // Success! Break out the loop returning
                    // the parsed statement
                    Ok(n) => break (n, tokens),
                    // It failed because further input was expected
                    Err(e) => {
                        if let Error::EndOfInput(_, _) = e {
                            // So read more input to the buffer
                            continue 'moreinput;
                        // Bad syntax
                        } else {
                            // Report the error
                            eprintln!("{} at {}", Colour::Red.bold().paint("↑"), e.at());
                            print_error(&e, &tokens);
                            // Clear the buffer and try again
                            continue 'repl;
                        }
                    }
                }
            }
        };
        // Evaluate the parsed statement
        match ast.eval(&mut store) {
            // Print the result
            Ok(v) => println!(
                "{} {}",
                Colour::Cyan.bold().paint("→"),
                Style::new().italic().paint(format!("{}", v))
            ),
            // Print the runtime error
            Err(e) => {
                eprintln!("{} Anomaly", Colour::Red.bold().paint("↑"));
                print_error(&e, &tokens);
            }
        }
    }
}

// The entry point
fn main() -> Result<(), Box<dyn error::Error>> {
    #[cfg(windows)]
    ansi_term::enable_ansi_support();

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
                builtin(&mut store);
                let mut tokens = Tokens::from(Stream::from(script));
                // Parse the string
                match tokens.parse() {
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
                            Err(e) => {
                                eprintln!("{} {}", e.at(), Colour::Red.bold().paint("Anomaly"));
                                print_error(&e, &tokens);
                            }
                        }
                    }
                    // Looks like the syntax was wrong
                    Err(e) => {
                        eprintln!("{} {}", e.at(), Colour::Red.bold().paint("Bad Input"));
                        print_error(&e, &tokens);
                    }
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
