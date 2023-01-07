//! 编译器驱动流程
//!
//! 源文件
//!   ↓
//! 预处理器预处理后的文件
//!   ↓
//! cc1编译为汇编文件
//!   ↓
//! as编译为可重定位文件
//!   ↓
//! ld链接为可执行文件

use rvrcc::{
    codegen, open_file_for_write, parse, parse_args, replace_extn, tokenize_file, write_file, Args,
    TempFile,
};
use std::env;
use std::process::{exit, Command};

/// rvcc的程序入口函数
fn main() {
    // 获取命令行参数
    let arg_strs: Vec<String> = env::args().collect();
    // 解析传入程序的参数
    let args = parse_args(arg_strs.to_vec());

    // 如果指定了-cc1选项
    // 直接编译C文件到汇编文件
    if args.opt_cc1 {
        cc1(args);
        exit(0);
    }

    if args.inputs.len() > 1 && !args.opt_o.eq("") {
        panic!("cannot specify '-o' with multiple files");
    }

    for input in args.inputs.iter() {
        let output = if !args.opt_o.eq("") {
            args.opt_o.to_string()
        } else if args.opt_s {
            // 若未指定输出的汇编文件名，则输出到后缀为.s的同名文件中
            replace_extn(input, ".s")
        } else {
            replace_extn(input, ".o")
        };

        if args.opt_s {
            run_cc1(
                &args,
                arg_strs.to_vec(),
                Some(input.to_string()),
                Some(output),
            );
            continue;
        }

        // 否则运行cc1和as
        // 临时文件TmpFile作为cc1输出的汇编文件
        let temp_file = TempFile::new_tmp();
        // cc1，编译C文件为汇编文件
        run_cc1(
            &args,
            arg_strs.to_vec(),
            Some(input.to_string()),
            Some(temp_file.get_path()),
        );
        // as，编译汇编文件为可重定位文件
        assemble(temp_file.get_path(), output);
    }
}

fn cc1(args: Args) {
    // tokenize 输入文件
    let tokens = tokenize_file(args.base.to_string());
    // 将token列表解析成ast
    let mut program = parse(&tokens);

    // 打开输出文件
    let mut file = open_file_for_write(&args.output_file);
    // 写入文件名
    write_file(&mut file, format!(".file 1 \"{}\"\n", args.base).as_str());
    // 根据ast,向输出文件中写入相关汇编
    codegen(&mut program, file);
}

/// 执行调用cc1程序
/// 因为rvrcc自身就是cc1程序
/// 所以调用自身，并传入-cc1参数作为子进程
fn run_cc1(args: &Args, mut arg_strs: Vec<String>, input: Option<String>, output: Option<String>) {
    // 在选项最后新加入"-cc1"选项
    arg_strs.push("-cc1".to_string());

    // 存入输入文件的参数
    if input.is_some() {
        arg_strs.push("-cc1-input".to_string());
        arg_strs.push(input.unwrap());
    }

    // 存入输出文件的参数
    if output.is_some() {
        arg_strs.push("-cc1-output".to_string());
        arg_strs.push(output.unwrap());
    }

    // 运行自身作为子进程，同时传入选项
    run_subprocess(args.opt_hash_hash_hash, arg_strs)
}

// 开辟子进程
fn run_subprocess(print_debug: bool, arg_strs: Vec<String>) {
    let progress = arg_strs[0].to_string();
    // 打印出子进程所有的命令行参数
    if print_debug {
        // 程序名
        eprint!("{}", progress);
        // 程序参数
        for i in 1..arg_strs.len() {
            eprint!(" {}", arg_strs[i])
        }
        eprintln!();
    }

    // Fork–exec模型
    // 创建当前进程的副本，这里开辟了一个子进程
    // 返回-1表示错位，为0表示成功
    let mut child = Command::new(progress)
        .args(&arg_strs[1..])
        .spawn()
        .unwrap_or_else(|e| panic!("file to exec child process: {}", e));

    child.wait().unwrap();
}

fn assemble(input: String, output: String) {
    let cmd = vec![
        "riscv64-unknown-linux-gnu-as".to_string(),
        "-fPIC".to_string(),
        "-c".to_string(),
        input,
        "-o".to_string(),
        output,
    ];
    run_subprocess(false, cmd);
}
