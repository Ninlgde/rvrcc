//! 命令行参数解析

use crate::cmacro::{define, undef_macro};
use crate::{in_include_paths, quote_makefile};
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
    /// -static选项
    pub opt_static: bool,
    /// -shared选项
    pub opt_shared: bool,
    /// -S选项
    pub opt_s_cap: bool,
    /// -c选项
    pub opt_c: bool,
    /// -E选项
    pub opt_e_cap: bool,
    /// -M选项
    pub opt_m_cap: bool,
    /// -MF选项
    pub opt_mf_cap: String,
    /// -MP选项
    pub opt_mp_cap: bool,
    /// -MT选项
    pub opt_mt_cap: String,
    /// -MD选项
    pub opt_md_cap: bool,
    /// -MMD选项
    pub opt_mmd_cap: bool,
    /// 引入路径区
    pub include_paths: Vec<String>,
    /// 标记是否生成common块
    pub opt_f_common: bool,
    /// -include所引入的文件
    pub opt_include: Vec<String>,
    /// -x选项
    pub opt_x: FileType,
    /// 链接器额外参数
    pub ld_extra_args: Vec<String>,
    /// 标准库所引入的路径，用于-MMD选项
    pub std_include_paths: Vec<String>,
    /// 位置无关代码的标记
    pub opt_fpic: bool,
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
            opt_static: false,
            opt_shared: false,
            opt_s_cap: false,
            opt_c: false,
            opt_e_cap: false,
            opt_m_cap: false,
            opt_mf_cap: "".to_string(),
            opt_mp_cap: false,
            opt_mt_cap: "".to_string(),
            opt_md_cap: false,
            opt_mmd_cap: false,
            include_paths: vec![],
            opt_f_common: true,
            opt_include: vec![],
            opt_x: FileType::None,
            ld_extra_args: vec![],
            std_include_paths: vec![],
            opt_fpic: false,
        }
    }

    /// 判断需要一个参数的选项，是否具有一个参数
    fn take_arg(arg: &str) -> bool {
        let args = vec!["-o", "-I", "-idirafter", "-include", "-x", "-MF", "-MT"];
        for a in args {
            if arg.eq(a) {
                return true;
            }
        }
        return false;
    }

    /// 解析-x选项
    fn parse_optx(x: &str) -> FileType {
        // -xc，解析为C语言源代码
        if x.eq("c") {
            return FileType::C;
        }
        // -xassembler，解析为汇编源代码
        if x.eq("assembler") {
            return FileType::Asm;
        }
        // -xnone，解析为空类型
        if x.eq("none") {
            return FileType::None;
        }
        panic!("<command line>: unknown argument for -x: {}", x);
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

        let mut idirafter = vec![];

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

            // 解析-o
            if arg.eq("-o") {
                output = args[i + 1].as_str();
                i += 2;
                continue;
            }

            // 解析-oxxxx
            if arg.starts_with("-o") {
                output = &arg[2..];
                i += 1;
                continue;
            }

            // 解析-S
            if arg.eq("-S") {
                result.opt_s_cap = true;
                i += 1;
                continue;
            }

            // 解析-fcommon
            if arg.eq("-fcommon") {
                result.opt_f_common = true;
                i += 1;
                continue;
            }

            // 解析-fno-common
            if arg.eq("-fno-common") {
                result.opt_f_common = false;
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
                result.include_paths.push(incl.to_string());
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

            // 解析-U
            if arg.eq("-U") {
                undef_macro(args[i + 1].as_str());
                i += 2;
                continue;
            }

            // 解析-Uxxxx
            if arg.starts_with("-U") {
                let incl = &arg[2..];
                undef_macro(incl);
                i += 1;
                continue;
            }

            // 解析-include
            if arg.eq("-include") {
                result.opt_include.push(args[i + 1].to_string());
                i += 2;
                continue;
            }

            // 解析-x
            if arg.eq("-x") {
                result.opt_x = Self::parse_optx(args[i + 1].as_str());
                i += 2;
                continue;
            }

            // 解析-xxxx
            if arg.starts_with("-x") {
                let optx = &arg[2..];
                result.opt_x = Self::parse_optx(optx);
                i += 1;
                continue;
            }

            // 解析-l
            if arg.starts_with("-l") {
                result.inputs.push(arg.to_string());
                i += 1;
                continue;
            }

            // 解析-s
            if arg.eq("-s") {
                result.ld_extra_args.push("-s".to_string());
                i += 1;
                continue;
            }

            // 解析-M
            if arg.eq("-M") {
                result.opt_m_cap = true;
                i += 1;
                continue;
            }

            // 解析-MF
            if arg.eq("-MF") {
                result.opt_mf_cap = args[i + 1].to_string();
                i += 2;
                continue;
            }

            // 解析-MP
            if arg.eq("-MP") {
                result.opt_mp_cap = true;
                i += 1;
                continue;
            }

            // 解析-MT
            // `-MT File`，指定File为依赖规则中的目标
            if arg.eq("-MT") {
                if result.opt_mt_cap.is_empty() {
                    result.opt_mt_cap = args[i + 1].to_string();
                } else {
                    result.opt_mt_cap = format!("{} {}", result.opt_mt_cap, args[i + 1]);
                }
                i += 2;
                continue;
            }

            // 解析-MD
            if arg.eq("-MD") {
                result.opt_md_cap = true;
                i += 1;
                continue;
            }

            // 解析-MQ
            if arg.eq("-MQ") {
                if result.opt_mt_cap.is_empty() {
                    // 无依赖规则中的目标
                    result.opt_mt_cap = quote_makefile(args[i + 1].to_string());
                } else {
                    // 合并依赖规则中的目标
                    result.opt_mt_cap = format!(
                        "{} {}",
                        result.opt_mt_cap,
                        quote_makefile(args[i + 1].to_string())
                    );
                }
                i += 2;
                continue;
            }

            // 解析-MMD
            if arg.eq("-MMD") {
                result.opt_mmd_cap = true;
                // 同时启用-MD选项
                result.opt_md_cap = true;
                i += 1;
                continue;
            }

            // 解析-fpic或-fPIC
            if arg.eq("-fpic") || arg.eq("-fPIC") {
                result.opt_fpic = true;
                i += 1;
                continue;
            }

            // 解析-cc1-input
            if arg.eq("-cc1-input") {
                result.base = args[i + 1].to_string();
                i += 2;
                continue;
            }

            // 解析-cc1-output
            if arg.eq("-cc1-output") {
                result.output_file = args[i + 1].to_string();
                i += 2;
                continue;
            }

            // 解析-idirafter
            if arg.eq("-idirafter") {
                idirafter.push(args[i + 1].to_string());
                i += 2;
                continue;
            }

            // 解析-static
            if arg.eq("-static") {
                result.opt_static = true;
                result.ld_extra_args.push("-static".to_string());
                i += 1;
                continue;
            }

            // 解析-shared
            if arg.eq("-shared") {
                result.opt_shared = true;
                result.ld_extra_args.push("-shared".to_string());
                i += 1;
                continue;
            }

            // 忽略多个选项
            if arg.starts_with("-O")
                || arg.starts_with("-W")
                || arg.starts_with("-g")
                || arg.starts_with("-std=")
                || arg.eq("-ffreestanding")
                || arg.eq("-fno-builtin")
                || arg.eq("-fno-omit-frame-pointer")
                || arg.eq("-fno-stack-protector")
                || arg.eq("-fno-strict-aliasing")
                || arg.eq("-m64")
                || arg.eq("-mno-red-zone")
                || arg.eq("-w")
            {
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

        // -E隐式包含输入是C语言的宏
        if result.opt_e_cap {
            result.opt_x = FileType::C;
        }

        result.include_paths.append(&mut idirafter);

        result.opt_o = output.to_string();
        result
    }

    /// 判断是否为标准库路径
    pub fn in_std_include_path(&self, path: &String) -> bool {
        in_include_paths(&self.std_include_paths, path)
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

/// 输入源文件类型
#[derive(Clone, PartialEq, Eq)]
pub enum FileType {
    /// 空类型
    None,
    /// C语言源代码类型
    C,
    /// 汇编代码类型
    Asm,
    /// 可重定位文件类型
    Obj,
    /// 静态库文件类型
    Ar,
    /// 动态库文件类型
    Dso,
}

impl FileType {
    /// 获取文件的类型
    pub fn get_file_type(optx: &FileType, filename: &str) -> FileType {
        // 若-x指定了不为空的类型，使用该类型
        if !FileType::None.eq(optx) {
            return optx.clone();
        }

        // 以.a结尾的文件，解析为静态库文件类型
        if filename.ends_with(".a") {
            return FileType::Ar;
        }
        // 以.so结尾的文件，解析为动态库文件类型
        if filename.ends_with(".so") {
            return FileType::Dso;
        }
        // 以.o结尾的文件，解析为空重定位文件类型
        if filename.ends_with(".o") {
            return FileType::Obj;
        }
        // 以.c结尾的文件，解析为C语言源代码类型
        if filename.ends_with(".c") {
            return FileType::C;
        }
        // 以.s结尾的文件，解析为汇编类型
        if filename.ends_with(".s") {
            return FileType::Asm;
        }

        panic!("<command line>: unknown file extension: {}", filename);
    }
}
