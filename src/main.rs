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
    codegen, find_file, open_file_for_write, parse, parse_args, preprocess, replace_extn,
    tokenize_file, write_file, Args, TempFile, TempFileCleaner,
};
use std::env;
use std::path::Path;
use std::process::{exit, Command};

const RISCV_HOME: &str = "/Users/malikma/Desktop/source/opt/riscv_linux";

/// rvcc的程序入口函数
fn main() {
    // 获取命令行参数
    let arg_strs: Vec<String> = env::args().collect();
    // 解析传入程序的参数
    let args = parse_args(arg_strs.to_vec());

    let mut cleaner = TempFileCleaner::new();

    // 如果指定了-cc1选项
    // 直接编译C文件到汇编文件
    if args.opt_cc1 {
        cc1(args);
        exit(0);
    }

    // 当前不能指定-c、-S后，将多个输入文件，输出到一个文件中
    if args.inputs.len() > 1 && !args.opt_o.eq("") && (args.opt_c || args.opt_cap_s) {
        panic!("cannot specify '-o' with '-c' or '-S' with multiple files");
    }

    let mut ld_args = vec![];

    for input in args.inputs.iter() {
        let output = if !args.opt_o.eq("") {
            args.opt_o.to_string()
        } else if args.opt_cap_s {
            // 若未指定输出的汇编文件名，则输出到后缀为.s的同名文件中
            replace_extn(input, ".s")
        } else {
            replace_extn(input, ".o")
        };

        // 处理.o文件
        if input.ends_with(".o") {
            // 存入链接器选项中
            ld_args.push(input.to_string());
            continue;
        }

        // 处理.s文件
        if input.ends_with(".s") {
            // 如果没有指定-S，那么需要进行汇编
            if !args.opt_cap_s {
                assemble(
                    input.to_string(),
                    output.to_string(),
                    args.opt_hash_hash_hash,
                );
                ld_args.push(output);
            }
            continue;
        }

        // 处理.c文件
        if !input.ends_with(".c") && !input.eq("-") {
            panic!("unknown file extension: {}", input);
        }

        if args.opt_cap_s {
            run_cc1(
                arg_strs.to_vec(),
                Some(input.to_string()),
                Some(output),
                args.opt_hash_hash_hash,
            );
            continue;
        }

        // 编译并汇编
        if args.opt_c {
            // 临时文件Tmp作为cc1输出的汇编文件
            let temp_file = TempFile::new_tmp(&mut cleaner);
            // cc1，编译C文件为汇编文件
            run_cc1(
                arg_strs.to_vec(),
                Some(input.to_string()),
                Some(temp_file.to_string()),
                args.opt_hash_hash_hash,
            );
            // as，编译汇编文件为可重定位文件
            assemble(temp_file, output, args.opt_hash_hash_hash);
            continue;
        }

        // 否则运行cc1和as
        // 临时文件Tmp1作为cc1输出的汇编文件
        // 临时文件Tmp2作为as输出的可重定位文件
        let temp_file1 = TempFile::new_tmp(&mut cleaner);
        let temp_file2 = TempFile::new_tmp(&mut cleaner);
        // cc1，编译C文件为汇编文件
        run_cc1(
            arg_strs.to_vec(),
            Some(input.to_string()),
            Some(temp_file1.to_string()),
            args.opt_hash_hash_hash,
        );
        // as，编译汇编文件为可重定位文件
        assemble(temp_file1, temp_file2.to_string(), args.opt_hash_hash_hash);
        // 将Tmp2存入链接器选项
        ld_args.push(temp_file2);
    }

    // 需要链接的情况
    // 未指定文件名时，默认为a.out
    if ld_args.len() > 0 {
        let out = if args.output_file.eq("") {
            "a.out"
        } else {
            args.output_file.as_str()
        };
        run_linker(ld_args, out.to_string(), args.opt_hash_hash_hash);
    }
}

fn cc1(args: Args) {
    // tokenize 输入文件
    let tokens = tokenize_file(args.base.to_string());
    let tokens = preprocess(tokens);
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
fn run_cc1(
    mut arg_strs: Vec<String>,
    input: Option<String>,
    output: Option<String>,
    print_debug: bool,
) {
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
    run_subprocess(arg_strs, print_debug)
}

// 开辟子进程
fn run_subprocess(arg_strs: Vec<String>, print_debug: bool) {
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

fn assemble(input: String, output: String, print_debug: bool) {
    let cmd = vec![
        "riscv64-unknown-linux-gnu-as".to_string(),
        "-fPIC".to_string(),
        "-c".to_string(),
        input,
        "-o".to_string(),
        output,
    ];
    run_subprocess(cmd, print_debug);
}

fn run_linker(inputs: Vec<String>, output: String, print_debug: bool) {
    // 需要传递ld子进程的参数
    let mut arr = vec![];

    // 链接器
    arr.push("riscv64-unknown-linux-gnu-ld".to_string());

    // 输出文件
    arr.push("-o".to_string());
    arr.push(output);
    arr.push("-m".to_string());
    arr.push("elf64lriscv".to_string());
    arr.push("-dynamic-linker".to_string());

    arr.push(format!(
        "{}/sysroot/lib/ld-linux-riscv64-lp64d.so.1",
        RISCV_HOME
    ));

    let lib_path = find_lib_path();
    let gcc_lib_path = find_gcc_lib_path();

    arr.push(format!("{}/crt1.o", lib_path.to_string()));
    arr.push(format!("{}/crti.o", lib_path.to_string()));
    arr.push(format!("{}/crtbegin.o", gcc_lib_path.to_string()));
    arr.push(format!("-L{}", gcc_lib_path.to_string()));
    arr.push(format!("-L{}", lib_path.to_string()));
    arr.push(format!("-L{}/..", lib_path.to_string()));

    arr.push(format!("-L{}/sysroot/usr/lib64", RISCV_HOME));
    arr.push(format!("-L{}/sysroot/lib64", RISCV_HOME));
    arr.push(format!(
        "-L{}/sysroot/usr/lib/riscv64-linux-gnu",
        RISCV_HOME
    ));
    arr.push(format!(
        "-L{}/sysroot/usr/lib/riscv64-pc-linux-gnu",
        RISCV_HOME
    ));
    arr.push(format!(
        "-L{}/sysroot/usr/lib/riscv64-redhat-linux",
        RISCV_HOME
    ));
    arr.push(format!("-L{}/sysroot/usr/lib", RISCV_HOME));
    arr.push(format!("-L{}/sysroot/lib", RISCV_HOME));

    // 输入文件，存入到链接器参数中
    for input in inputs.iter() {
        arr.push(input.to_string());
    }

    arr.push("-lc".to_string());
    arr.push("-lgcc".to_string());
    arr.push("--as-needed".to_string());
    arr.push("-lgcc_s".to_string());
    arr.push("--no-as-needed".to_string());
    arr.push(format!("{}/crtend.o", gcc_lib_path.to_string()));
    arr.push(format!("{}/crtn.o", lib_path.to_string()));

    run_subprocess(arr, print_debug);
}

fn find_lib_path() -> String {
    let lib_path = format!("{}/sysroot/usr/lib/crti.o", RISCV_HOME);
    if file_exists(lib_path) {
        return format!("{}/sysroot/usr/lib/", RISCV_HOME);
    }
    panic!("library path is not found")
}

fn find_gcc_lib_path() -> String {
    let lib_path_pattern = format!(
        "{}/lib/gcc/riscv64-unknown-linux-gnu/*/crtbegin.o",
        RISCV_HOME
    );
    let lib_path = find_file(lib_path_pattern);
    Path::new(&lib_path)
        .parent()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
}

fn file_exists(file: String) -> bool {
    Path::new(&file).exists()
}
