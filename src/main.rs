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
    append_tokens, codegen, dirname, file_exists, find_file, init_macros, open_file_for_write,
    parse, parse_args, preprocess, print_dependencies, print_tokens, replace_extn,
    search_include_paths, tokenize_file, Args, FileType, TempFile, TempFileCleaner, Token,
    BASE_FILE,
};
use std::env;
use std::io::Write;
use std::process::{exit, Command};

const RISCV_HOME: &str = "/Users/malikma/Desktop/source/opt/riscv_linux";

/// rvcc的程序入口函数
fn main() {
    // 初始化预定义的宏
    init_macros();
    // 获取命令行参数
    let arg_strs: Vec<String> = env::args().collect();
    // 解析传入程序的参数
    let mut args = parse_args(arg_strs.to_vec());

    let mut cleaner = TempFileCleaner::new();

    // 如果指定了-cc1选项
    // 直接编译C文件到汇编文件
    if args.opt_cc1 {
        add_default_include_paths(&mut args, arg_strs[0].to_string());
        cc1(args);
        exit(0);
    }

    // 当前不能指定-c、-S、-E后，将多个输入文件，输出到一个文件中
    if args.inputs.len() > 1
        && !args.opt_o.eq("")
        && (args.opt_c || args.opt_s_cap || args.opt_e_cap)
    {
        panic!("cannot specify '-o' with '-c', '-S' or '-E' with multiple files");
    }

    let mut ld_args = vec![];

    for input in args.inputs.iter() {
        // 链接时搜索指定的库文件
        if input.starts_with("-l") {
            ld_args.push(input.to_string());
            continue;
        }

        let output = if !args.opt_o.eq("") {
            args.opt_o.to_string()
        } else if args.opt_s_cap {
            // 若未指定输出的汇编文件名，则输出到后缀为.s的同名文件中
            replace_extn(input, ".s")
        } else {
            replace_extn(input, ".o")
        };

        // 获取输入文件的类型
        let ft = FileType::get_file_type(&args.opt_x, input);

        // 处理.o或.a或.so文件
        if ft == FileType::Obj || ft == FileType::Ar || ft == FileType::Dso {
            // 存入链接器选项中
            ld_args.push(input.to_string());
            continue;
        }

        // 处理.s文件
        if ft == FileType::Asm {
            // 如果没有指定-S，那么需要进行汇编
            if !args.opt_s_cap {
                assemble(
                    input.to_string(),
                    output.to_string(),
                    args.opt_hash_hash_hash,
                );
            }
            continue;
        }

        // 处理.c文件
        assert!(ft == FileType::C);

        // 只进行解析
        if args.opt_e_cap || args.opt_m_cap {
            run_cc1(
                arg_strs.to_vec(),
                Some(input.to_string()),
                None,
                args.opt_hash_hash_hash,
            );
            continue;
        }

        // 如果有-S选项，那么执行调用cc1程序
        if args.opt_s_cap {
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
        let out = if args.opt_o.eq("") {
            "a.out"
        } else {
            args.opt_o.as_str()
        };
        run_linker(ld_args, out.to_string(), &args);
    }
}

// 解析文件，生成终结符流
fn must_tokenize_file(path: &String) -> Vec<Token> {
    let tokens = tokenize_file(path);
    // 终结符流生成失败，对应文件报错
    if tokens.len() == 0 {
        panic!("{}: got error", path)
    }
    tokens
}

fn cc1(args: Args) {
    // tokenize 输入文件
    unsafe {
        BASE_FILE = args.base.to_string();
    }

    let mut all_tokens = vec![];
    for incl in args.opt_include.iter() {
        let path;
        if file_exists(incl) {
            // 如果文件存在，则直接使用路径
            path = incl.to_string();
        } else {
            // 否则搜索引入路径区
            path = search_include_paths(&args.include_paths, incl);
            if path.is_empty() {
                panic!("-include: cannot find include file {}", incl);
            }
        }
        // 解析文件，生成终结符流
        let tokens = must_tokenize_file(&path);
        all_tokens = append_tokens(all_tokens, tokens);
    }

    let tokens = must_tokenize_file(&args.base);
    all_tokens = append_tokens(all_tokens, tokens);

    // 预处理
    let tokens = preprocess(&mut all_tokens, &args.include_paths);

    // 如果指定了-M，打印出文件的依赖关系
    if args.opt_m_cap || args.opt_md_cap {
        print_dependencies(&args);
        if args.opt_m_cap {
            return;
        }
    }

    // 如果指定了-E那么打印出预处理过的C代码
    if args.opt_e_cap {
        // 打开输出文件
        let file = open_file_for_write(&args.opt_o);
        print_tokens(file, tokens);
        return;
    }

    // 将token列表解析成ast
    let mut program = parse(&tokens);

    // 打开输出文件
    let mut file = open_file_for_write(&args.output_file);
    // 根据ast,向输出文件中写入相关汇编
    unsafe {
        // 输出汇编到缓冲区中
        codegen(&mut program, &args, &mut BUF);

        // 从缓冲区中写入到文件中
        file.write(BUF.as_ref()).expect("write failed");
    }
}

/// 防止编译器在编译途中退出，而只生成了部分的文件
/// 开启临时输出缓冲区
static mut BUF: Vec<u8> = Vec::new();

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

/// 使用as编译汇编到目标文件
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

/// 使用ld来link目标文件到可执行文件
fn run_linker(inputs: Vec<String>, output: String, args: &Args) {
    // 需要传递ld子进程的参数
    let mut arr = vec![];

    // 链接器
    arr.push("riscv64-unknown-linux-gnu-ld".to_string());

    // 输出文件
    arr.push("-o".to_string());
    arr.push(output);
    arr.push("-m".to_string());
    arr.push("elf64lriscv".to_string());
    if args.opt_static {
        arr.push("-dynamic-linker".to_string());

        arr.push(format!(
            "{}/sysroot/lib/ld-linux-riscv64-lp64d.so.1",
            RISCV_HOME
        ));
    }

    let lib_path = find_lib_path();
    let gcc_lib_path = find_gcc_lib_path();

    if args.opt_shared {
        arr.push(format!("{}/crti.o", lib_path.to_string()));
        arr.push(format!("{}/crtbeginS.o", gcc_lib_path.to_string()));
    } else {
        arr.push(format!("{}/crt1.o", lib_path.to_string()));
        arr.push(format!("{}/crti.o", lib_path.to_string()));
        arr.push(format!("{}/crtbegin.o", gcc_lib_path.to_string()));
    }
    arr.push(format!("-L{}", gcc_lib_path.to_string()));

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

    // 链接器额外参数存入到链接器参数中
    for ea in args.ld_extra_args.iter() {
        arr.push(ea.to_string());
    }

    // 输入文件，存入到链接器参数中
    for input in inputs.iter() {
        arr.push(input.to_string());
    }

    if args.opt_static {
        arr.push("--start-group".to_string());
        arr.push("-lgcc".to_string());
        arr.push("-lgcc_eh".to_string());
        arr.push("-lc".to_string());
        arr.push("--end-group".to_string());
    } else {
        arr.push("-lc".to_string());
        arr.push("-lgcc".to_string());
        arr.push("--as-needed".to_string());
        arr.push("-lgcc_s".to_string());
        arr.push("--no-as-needed".to_string());
    }
    if args.opt_shared {
        arr.push(format!("{}/crtendS.o", gcc_lib_path.to_string()));
    } else {
        arr.push(format!("{}/crtend.o", gcc_lib_path.to_string()));
    }
    arr.push(format!("{}/crtn.o", lib_path.to_string()));

    run_subprocess(arr, args.opt_hash_hash_hash);
}

/// 查找库路径
fn find_lib_path() -> String {
    if file_exists("/usr/lib/riscv64-linux-gnu/crti.o") {
        return "/usr/lib/riscv64-linux-gnu".to_string();
    }
    if file_exists("/usr/lib64/crti.o") {
        return "/usr/lib64".to_string();
    }
    let lib_path = format!("{}/sysroot/usr/lib/crti.o", RISCV_HOME);
    if file_exists(&lib_path) {
        return format!("{}/sysroot/usr/lib/", RISCV_HOME);
    }
    panic!("library path is not found")
}

/// 查找gcc库路径
fn find_gcc_lib_path() -> String {
    let mut paths = vec![];
    paths.push("/usr/lib/gcc/riscv64-linux-gnu/*/crtbegin.o");
    paths.push("/usr/lib/gcc/riscv64-pc-linux-gnu/*/crtbegin.o");
    paths.push("/usr/lib/gcc/riscv64-redhat-linux/*/crtbegin.o");
    let rslbp = format!(
        "{}/lib/gcc/riscv64-unknown-linux-gnu/*/crtbegin.o",
        RISCV_HOME
    );
    paths.push(&rslbp);
    for path in paths {
        let lib_path = find_file(path);
        if lib_path.len() > 0 {
            return dirname(lib_path);
        }
    }

    panic!("gcc library path is not found")
}

/// 增加默认引入路径
fn add_default_include_paths(args: &mut Args, arg: String) {
    let include_paths = &mut args.include_paths;
    // rvcc特定的引入文件被安装到了argv[0]的./include位置
    include_paths.push(format!("{}/include", dirname(arg)));

    // 支持标准的引入路径
    include_paths.push("/usr/local/include".to_string());
    include_paths.push("/usr/include/riscv64-linux-gnu".to_string());
    include_paths.push("/usr/include".to_string());

    // 为-MMD选项，复制一份标准库引入路径
    let std_include_paths = &mut args.std_include_paths;
    for include in include_paths.iter() {
        std_include_paths.push(include.to_string());
    }
}
