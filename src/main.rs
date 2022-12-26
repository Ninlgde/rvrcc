use rvrcc::{codegen, open_file_for_write, parse, tokenize_file, write_file};
use std::env;
use std::process::exit;

fn main() {
    // 获取命令行参数
    let args: Vec<String> = env::args().collect();
    // 根据病例行参数,获取对应的输入输出文件
    let (input_path, output_path) = parse_args(args);

    // tokenize 输入文件
    let tokens = tokenize_file(input_path.to_string());
    // 将token列表解析成ast
    let mut program = parse(&tokens);

    // 打开输出文件
    let mut file = open_file_for_write(&output_path);
    // 写入文件名
    write_file(
        &mut file,
        format!(".file 1 \"{}\"\n", input_path.to_string()).as_str(),
    );
    // 根据ast,向输出文件中写入相关汇编
    codegen(&mut program, file);
}

/// 解析命令行参数
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
