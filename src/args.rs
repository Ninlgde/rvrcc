//! 命令行参数解析

use std::ffi::CString;
use std::fs::remove_file;
use std::process::exit;

/// 解析好的命令行参数
pub struct Args {
    /// 输入文件
    pub input: String,
    /// 输出文件
    pub output: String,
    /// cc1选项
    pub opt_cc1: bool,
    /// ###选项
    pub opt_hash_hash_hash: bool,
    /// -S选项
    pub opt_s: bool,
}

impl Args {
    /// 创建空对象
    pub fn new() -> Args {
        Args {
            input: String::new(),
            output: String::new(),
            opt_cc1: false,
            opt_hash_hash_hash: false,
            opt_s: false,
        }
    }

    fn parse_io(args: Vec<String>) -> Self {
        if args.len() < 2 {
            print_usage(1);
        }
        let mut output = "-";
        let mut input = "-";

        let mut result = Args::new();

        let mut i = 0;
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
                if i + 1 < args.len() {
                    output = args[i + 1].as_str();
                    i += 2;
                    continue;
                } else {
                    print_usage(1);
                }
            }

            if arg.eq("-S") {
                result.opt_s = true;
                i += 1;
                continue;
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

        result.input = input.to_string();
        result.output = output.to_string();
        result
    }
}

/// 解析命令行参数
pub fn parse_args(args: Vec<String>) -> Args {
    let args = Args::parse_io(args);
    args
}

/// 输出程序的使用说明
fn print_usage(status: i32) {
    eprintln!("rvrcc [ -o <path> ] <file>");
    exit(status);
}

/// 编译产生的零时文件
pub struct TempFile {
    /// 文件名
    path: String,
    /// 是否自动删除
    auto_del: bool,
}

impl TempFile {
    /// 创建名为path的零时文件,auto_del标记是否在离开生命周期后删除文件
    pub fn new(path: &str, auto_del: bool) -> TempFile {
        let ptr = CString::new(path).unwrap().into_raw();
        let fd = unsafe { libc::mkstemp(ptr) };
        let path = unsafe { CString::from_raw(ptr) };

        if fd < 0 {
            panic!("mkstemp failed")
        }

        TempFile {
            path: path.into_string().unwrap(),
            auto_del,
        }
    }

    /// 创建rvrcc的零时文件
    pub fn new_tmp() -> TempFile {
        TempFile::new("/tmp/rvrcc-XXXXXXXX", true)
    }

    /// 获取零时文件路径
    pub fn get_path(&self) -> String {
        self.path.to_string()
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        // eprintln!("dorp temp file {}", self.auto_del);
        if self.auto_del {
            let _ = remove_file(&self.path);
        }
    }
}
