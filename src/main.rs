use interpreter::run_script;
use io::{Read, Write};
use nix::unistd::*;
use std::fs::{read_dir, File};
use std::io;

struct Command {
    cmd: &'static str,
    args: u32,
}

fn get_directory_content() -> String {
    let f = getcwd().unwrap();
    let dirreader = read_dir(f).unwrap();
    let res = dirreader.fold(String::new(), |acc, curr| {
        let name = curr.unwrap().file_name();
        acc + name.to_str().unwrap() + " "
    });
    res
}

fn run_file(path: &str) {
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    run_script(&contents);
}

fn run_prompt() {
    let mut line = String::new();
    loop {
        io::stdout().write(b"> ").unwrap();
        io::stdout().flush();
        io::stdin().read_line(&mut line).unwrap();
        if line == "exit" {
            return;
        }
        run_script(&line);
        line.clear();
    }
}

fn dispatch(arg: &String, commands: &Vec<Command>) {
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
        Command { cmd: "cd", args: 1 },
        Command {
            cmd: "pwd",
            args: 0,
        },
        Command { cmd: "ls", args: 0 },
        Command {
            cmd: "script",
            args: 0,
        },
        Command {
            cmd: "run",
            args: 1,
        },
    ];

    let mut line: String = String::new();
    loop {
        io::stdin().read_line(&mut line)?;
        if line.trim().len() != 0 {
            dispatch(&line, &commands);
            line.clear();
        }
    }
    Ok(())
}
