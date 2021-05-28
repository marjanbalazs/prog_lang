use std::env::{ args };
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
mod interpreter;

fn main() -> Result<(), String> {
    let args = args().collect::<Vec<String>>();
    let file = args.get(1);
    let file_path = match file {
        Some(x) => x,
        None => return Err("Problems".to_owned()),
    };
    let mut file = match File::open(file_path) {
        Ok(file) => file,
        Err(err) => return Err(err.to_string()),
    };
    let mut file_contents = String::new();
    file.read_to_string(&mut file_contents).unwrap();

    interpreter::run_script(&file_contents);

    Ok(())
}
