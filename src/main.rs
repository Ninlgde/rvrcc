use std::env;
use rvrcc::{codegen, INPUT, parse, tokenize};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} invalid number of arguments", args[0]);
    }

    let input = args[1].clone();
    // let input = "5";
    unsafe {
        INPUT = input.to_string();
    }
    let tokens = tokenize();

    let node = parse(&tokens);

    codegen(node);
}

