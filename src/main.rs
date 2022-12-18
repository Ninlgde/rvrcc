use rvrcc::{codegen, open_file_for_write, parse, tokenize_file};
use std::env;
use std::io::Write;
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().collect();

    let (input_path, output_path) = parse_args(args);

    let mut file = open_file_for_write(&output_path);

    let tokens = tokenize_file(input_path.to_string());

    let mut program = parse(&tokens);

    write_file(
        &mut file,
        format!(".file 1 \"{}\"\n", input_path.to_string()).as_str(),
    );
    codegen(&mut program, file);
}

fn write_file(f: &mut impl Write, s: &str) {
    f.write_all(s.as_ref()).unwrap();
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
