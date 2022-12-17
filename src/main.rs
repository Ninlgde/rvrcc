use std::env;
use rvrcc::{codegen, parse, tokenize_file};

fn main() {
    // debug use
//     let path = "main.c";
//     let input = "int main() {
//     return sub_char(7, 3, 3);
// }
//
// int sub_char(char a, char b, char c)
// {
//     return a-b-d;
// }
// ";
//     use rvrcc::tokenize;
//     let tokens = tokenize(path.to_string(), input.to_string());
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} invalid number of arguments", args[0]);
    }

    let path = args[1].clone();
    let tokens = tokenize_file(path);

    let mut program = parse(&tokens);

    codegen(&mut program);
}

