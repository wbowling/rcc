extern crate itertools;

use std::fs::File;
use std::io::prelude::*;
use std::env;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

mod parser;

fn main() {
    let arg1 = env::args().nth(1).expect("Must pass in arg");
    let path = Path::new(&arg1);

    let contents = read_file(path);
    let tokens = parser::lex::lex(&contents);
    println!("{:?}", tokens);

    let mut parser = parser::ast::Parser::new(tokens);
    let prog = parser.parse();
    println!("{:?}", prog);

    let mut generator = parser::generate::Generator::new();
    let asm = generator.generate(prog);


    let parent = path.parent().expect("can't get parent");
    let mut new_path = parent.join(path.file_stem().expect("Cannot get stem"));
    new_path.set_extension("s");

    write_file(&new_path, &asm);
    compile(&new_path);
}

fn read_file(path: &Path) -> String {
    let mut file = File::open(path).expect("Failed to open file for reading");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Failed to read file");

    contents
}

fn write_file(path: &PathBuf, contents: &str) {
    let mut file = File::create(path).expect("Failed to open file for writing");
    file.write_all(contents.as_bytes()).expect("Failed to write to file");
}

fn compile(path: &PathBuf) {
    let mut dest = path.clone();
    dest.set_file_name(path.file_stem().expect("cant get basename"));

    let output = Command::new("gcc")
        .arg("-masm=intel")
        .arg(path)
        .arg("-Wl,-no_pie")
        .arg("-o")
        .arg(dest)
        .output()
        .expect("failed to execute process");
    println!("status: {}", output.status);
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));

    assert!(output.status.success());
}
