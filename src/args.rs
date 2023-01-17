//! 命令行参数解析

use crate::cmacro::define;
use std::ffi::CString;
use std::fs::remove_file;
use std::process::exit;

/// 解析好的命令行参数
pub struct Args {
    /// 输入文件名
    pub base: String,
    /// 目标文件的路径
    pub opt_o: String,
    /// 输出文件名
    pub output_file: String,
    /// 输入文件区
    pub inputs: Vec<String>,
    /// cc1选项
    pub opt_cc1: bool,
    /// ###选项
    pub opt_hash_hash_hash: bool,
    /// -S选项
    pub opt_s_cap: bool,
    /// -c选项
    pub opt_c: bool,
    /// -E选项
    pub opt_e_cap: bool,
    /// 引入路径区
    pub include_path: Vec<String>,
}

impl Args {
    /// 创建空对象
    pub fn new() -> Args {
        Args {
            base: String::new(),
            opt_o: String::new(),
            output_file: "".to_string(),
            inputs: vec![],
            opt_cc1: false,
            opt_hash_hash_hash: false,
            opt_s_cap: false,
            opt_c: false,
            opt_e_cap: false,
            include_path: vec![],
        }
    }

    /// 判断需要一个参数的选项，是否具有一个参数
    fn take_arg(arg: &str) -> bool {
        let args = vec!["-o", "-I"];
        for a in args {
            if arg.eq(a) {
                return true;
            }
        }
        return false;
    }

    fn parse(args: Vec<String>) -> Self {
        if args.len() < 2 {
            print_usage(1);
        }
        // 确保需要一个参数的选项，存在一个参数
        for (i, arg) in args.iter().enumerate() {
            // 如果需要一个参数
            if Self::take_arg(arg) {
                // 如果不存在一个参数，则打印出使用说明
                if i + 1 >= args.len() {
                    print_usage(1);
                }
            }
        }
        let mut output = "";

        let mut result = Args::new();

        let mut i = 1;
        while i < args.len() {
            let arg = args[i].as_str();
            // 解析-###
            if arg.eq("-###") {
                result.opt_hash_hash_hash = true;
                i += 1;
                continue;
            }

            // 解析cc1
            if arg.eq("-cc1") {
                result.opt_cc1 = true;
                i += 1;
                continue;
            }

            // 如果存在help，则直接显示用法说明
            if arg.eq("--help") {
                print_usage(0);
            }

            if arg.eq("-o") {
                output = args[i + 1].as_str();
                i += 2;
                continue;
            }

            // 解析-S
            if arg.eq("-S") {
                result.opt_s_cap = true;
                i += 1;
                continue;
            }

            // 解析-c
            if arg.eq("-c") {
                result.opt_c = true;
                i += 1;
                continue;
            }

            // 解析-E
            if arg.eq("-E") {
                result.opt_e_cap = true;
                i += 1;
                continue;
            }

            // 解析-Ixxxx
            if arg.starts_with("-I") {
                let incl = &arg[2..];
                result.include_path.push(incl.to_string());
                i += 1;
                continue;
            }

            // 解析-D
            if arg.eq("-D") {
                define(args[i + 1].as_str());
                i += 2;
                continue;
            }

            // 解析-Dxxxx
            if arg.starts_with("-D") {
                let incl = &arg[2..];
                define(incl);
                i += 1;
                continue;
            }

            // 解析-cc1-input
            if arg.eq("-cc1-input") {
                result.base = args[i + 1].to_string();
                i += 1;
                continue;
            }

            // 解析-cc1-output
            if arg.eq("-cc1-output") {
                result.output_file = args[i + 1].to_string();
                i += 1;
                continue;
            }

            // 解析-oxxxx
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

            result.inputs.push(arg.to_string());
            i += 1;
        }

        if result.inputs.len() == 0 {
            panic!("no input files")
        }

        result.opt_o = output.to_string();
        result
    }
}

/// 解析命令行参数
pub fn parse_args(args: Vec<String>) -> Args {
    let args = Args::parse(args);
    args
}

/// 输出程序的使用说明
fn print_usage(status: i32) {
    eprintln!("rvrcc [ -o <path> ] <file>");
    exit(status);
}

/// 编译产生的临时文件
#[derive(Clone)]
pub struct TempFile {
    /// 文件名
    path: String,
    /// 是否自动删除
    auto_del: bool,
}

impl TempFile {
    /// 创建名为path的临时文件,auto_del标记是否在离开生命周期后删除文件
    pub fn new(path: &str, auto_del: bool, cleaner: &mut TempFileCleaner) -> String {
        let ptr = CString::new(path).unwrap().into_raw();
        let fd = unsafe { libc::mkstemp(ptr) };
        let path = unsafe { CString::from_raw(ptr) };

        if fd < 0 {
            panic!("mkstemp failed")
        }

        let result = path.into_string().unwrap();
        let tf = TempFile {
            path: result.to_string(),
            auto_del,
        };
        cleaner.add(tf);
        result
    }

    /// 创建rvrcc的临时文件
    pub fn new_tmp(cleaner: &mut TempFileCleaner) -> String {
        TempFile::new("/tmp/rvrcc-XXXXXXXX", true, cleaner)
    }

    /// 获取临时文件路径
    pub fn get_path(&self) -> String {
        self.path.to_string()
    }
}

/// 临时文件清理器
pub struct TempFileCleaner {
    temps: Vec<TempFile>,
}

impl TempFileCleaner {
    /// 创建清理器
    pub fn new() -> TempFileCleaner {
        TempFileCleaner { temps: Vec::new() }
    }

    /// 添加临时文件
    fn add(&mut self, tmp: TempFile) {
        self.temps.push(tmp)
    }
}

impl Drop for TempFileCleaner {
    fn drop(&mut self) {
        for temp in &self.temps {
            // eprintln!("dorp temp file {}", &temp.path);
            if temp.auto_del {
                remove_file(&temp.path).unwrap();
            }
        }
    }
}
