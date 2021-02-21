use io::{Read, Write};
use nix::unistd::*;
use std::fs::{read_dir, File};
use std::io;
mod interpreter;

struct Command {
    cmd: &'static str,
}

fn get_directory_content() -> String {
    let f = getcwd().unwrap();
    let dirreader = read_dir(f).unwrap();
    dirreader.fold(String::new(), |acc, curr| {
        let name = curr.unwrap().file_name();
        acc + name.to_str().unwrap() + " "
    })
}

fn run_file(path: &str) {
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    interpreter::run_script(&contents);
}

fn run_prompt() {
    let mut line = String::new();
    loop {
        io::stdout().write_all(b"> ").unwrap();
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut line).unwrap();
        if line == ".exit" {
            return;
        }
        interpreter::run_script(&line);
        line.clear();
    }
}

fn dispatch(arg: &str, commands: &[Command]) {
    let mut cmd_iter = arg.split_ascii_whitespace();
    let command_string = cmd_iter.next().unwrap();
    let cmd = commands.iter().rfind(|x| x.cmd == command_string);
    match cmd {
        Some(x) => {
            match x.cmd {
                "run" => {
                    run_file(cmd_iter.next().unwrap());
                }
                "script" => {
                    run_prompt();
                }
                "cd" => {
                    chdir(cmd_iter.next().unwrap()).unwrap();
                }
                "pwd" => {
                    println!("{}", getcwd().unwrap().display());
                }
                "ls" => {
                    println!("{}", get_directory_content());
                }
                _ => println!("Command not found"),
            };
        }
        None => println!("Something happened"),
    }
}

fn main() -> io::Result<()> {
    let commands = vec![
        Command { cmd: "cd" },
        Command { cmd: "pwd" },
        Command { cmd: "ls" },
        Command { cmd: "script" },
        Command { cmd: "run" },
    ];

    let mut line: String = String::new();
    loop {
        io::stdin().read_line(&mut line)?;
        let trimmed = line.trim();
        if trimmed == "exit" {
            break;
        }
        if !trimmed.is_empty() {
            dispatch(&line, &commands);
        }
        line.clear();
    }
    Ok(())
}
