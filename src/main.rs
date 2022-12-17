use std::env;
use rvrcc::{codegen, INPUT, parse, tokenize};

fn main() {
    // let input = "int x; int main() { return x; }";
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} invalid number of arguments", args[0]);
    }

    let input = args[1].clone();
    unsafe {
        INPUT = input.to_string();
    }
    let tokens = tokenize();

    let mut program = parse(&tokens);

    codegen(&mut program);
}

