//! 命令行参数解析

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
    pub opt_s: bool,
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
            opt_s: false,
        }
    }

    fn take_arg(arg: &str) -> bool {
        return arg.eq("-o");
    }

    fn parse_io(args: Vec<String>) -> Self {
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
                result.opt_s = true;
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
