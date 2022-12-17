use std::env;
use std::process::exit;
use rvrcc::{codegen, open_file_for_write, parse, tokenize_file};

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

    // let args = vec!["-o".to_string(), "test.c".to_string(), "-".to_string(), "aaaa".to_string()];

    let (input_path, output_path) = parse_args(args);

    let file = open_file_for_write(&output_path);

    let tokens = tokenize_file(input_path);

    let mut program = parse(&tokens);

    codegen(&mut program, file);
}

fn parse_args(args: Vec<String>) -> (String, String) {
    if args.len() < 2 {
        print_usage(1);
    }
    let mut output = "-";
    let mut input = "-";

    let mut i = 0;
    while i < args.len() {
        let arg = args[i].as_str();
        // println!("{}:{}", i, arg);
        // 如果存在help，则直接显示用法说明
        if arg.eq("--help") {
            print_usage(0);
        }

        if arg.eq("-o") {
            if i + 1 < args.len() {
                output = args[i + 1].as_str();
                i += 2;
                continue;
            } else {
                print_usage(1);
            }
        }

        if arg.starts_with("-o") {
            output = &arg[2..];
            i += 1;
            continue;
        }

        // 解析为-的参数
        if arg.starts_with('-') && arg.len() > 1 {
            eprintln!("unknown argument: {}", arg);
            exit(1);
        }

        input = arg;
        i += 1;
    }

    (input.to_string(), output.to_string())
}

/// 输出程序的使用说明
fn print_usage(status: i32) {
    eprintln!("rvrcc [ -o <path> ] <file>");
    exit(status);
}