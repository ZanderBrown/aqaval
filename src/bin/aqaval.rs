#![allow(unknown_lints)]

use std::error;
use std::fs::read_to_string;
use std::{collections::HashMap, path::PathBuf};

use clap::Parser;
use linefeed::{Interface, ReadResult};
use yansi::{Color::Yellow, Paint, Style};

use aqaval::{builtin, Error, Parsable, Stream, Tokens};

#[derive(Parser, Debug)]
#[clap(author="Zander Brown", version, about="AQA Pseudocode Interpreter", long_about = None)]
struct Args {
    #[clap(short = 's', long)]
    /// Show the final state of variables
    vars: bool,
    #[clap(short = 'a', long)]
    /// Show the parsed ast
    ast: bool,
    #[clap(short = 'r', long)]
    /// Show the evaluated value
    res: bool,
    /// A file to execute
    file: Option<PathBuf>,
}

pub fn print_error(err: &Error, src: &Tokens) {
    let at = err.at();
    let line = Paint::magenta(at.start().line()).bold();
    let line_len = line.to_string().len();
    let sep = Paint::white("|").dimmed();
    if let Some(source) = src.source(at.start().line()) {
        eprintln!("{} {}{}", line, sep, Paint::default(source).bold());
    } else {
        eprintln!("{} {} [err]", line, sep);
    }
    eprintln!(
        "{:idt$} {}{:pad$}{:↑>num$}",
        " ",
        sep,
        "",
        "↑",
        idt = line_len,
        pad = at.start().column(),
        num = at.end().column() - at.start().column()
    );
    eprintln!(
        "{:idt$} {}{:pad$}{}",
        " ",
        sep,
        " ",
        err.message(),
        idt = line_len,
        pad = at.start().column()
    );
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
                let prompt_style = Style::default().fg(Yellow).bold();
                let mut prompt = String::new();
                prompt.push('\x01');
                prompt_style.fmt_prefix(&mut prompt)?;
                prompt.push('\x02');
                prompt.push_str("← ");
                prompt.push('\x01');
                prompt_style.fmt_suffix(&mut prompt)?;
                prompt.push('\x02');

                reader.set_prompt(&prompt)?;
            } else {
                let prompt_style = Style::default().fg(Yellow);
                let mut prompt = String::new();
                prompt.push('\x01');
                prompt_style.fmt_prefix(&mut prompt)?;
                prompt.push('\x02');
                prompt.push_str("- ");
                prompt.push('\x01');
                prompt_style.fmt_suffix(&mut prompt)?;
                prompt.push('\x02');

                reader.set_prompt(&prompt)?;
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
                        }
                        // Report the error
                        eprintln!("{} at {}", Paint::red("↑").bold(), e.at());
                        print_error(&e, &tokens);
                        // Clear the buffer and try again
                        continue 'repl;
                    }
                }
            }
        };
        // Evaluate the parsed statement
        match ast.eval(&mut store) {
            // Print the result
            Ok(v) => println!(
                "{} {}",
                Paint::cyan("→").bold(),
                Style::default().italic().paint(v)
            ),
            // Print the runtime error
            Err(e) => {
                eprintln!("{} Anomaly", Paint::red("↑").bold());
                print_error(&e, &tokens);
            }
        }
    }
}

// The entry point
fn main() -> Result<(), Box<dyn error::Error>> {
    #[cfg(windows)]
    Paint::enable_windows_ascii();

    let args = Args::parse();

    // If a file was passed
    let path = if let Some(path) = args.file {
        // Get the filename
        path
    } else {
        // Run the REPL
        repl()?;
        // And exit afterwards
        return Ok(());
    };

    // Check the file exists
    if path.exists() {
        // Read the file into a string
        match read_to_string(&path) {
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
                        if args.ast {
                            // Print it
                            println!("Reverse AST\n{}", n);
                        }
                        // Run the users program!
                        match n.eval(&mut store) {
                            // If they asked for the last result
                            Ok(v) => {
                                if args.res {
                                    // Print it
                                    println!("-> {}", v);
                                }
                            }
                            // Something isn't right we their logic
                            Err(e) => {
                                eprintln!("{} {}", e.at(), Paint::red("Anomaly").bold());
                                print_error(&e, &tokens);
                            }
                        }
                    }
                    // Looks like the syntax was wrong
                    Err(e) => {
                        eprintln!("{} {}", e.at(), Paint::red("Bad Input").bold());
                        print_error(&e, &tokens);
                    }
                }
                // If variable dumping was requested
                if args.ast {
                    println!("Variables:");
                    // Looks though the key/values
                    for (k, v) in store {
                        // And print them
                        println!("{} = {}", k, v);
                    }
                }
            }
            // Or not...
            Err(e) => println!("Can't read {}: {}", path.display(), e),
        }
    } else {
        // It didn't
        println!("{} doesn't exist", path.display());
    }

    Ok(())
}
