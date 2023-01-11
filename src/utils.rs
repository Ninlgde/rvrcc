use crate::token::Token;
use std::fs::File;
use std::io::{stdout, Read, Write};
use std::path::Path;
use std::{fs, io};

/// 从chars中拷贝[start, end)的字符
pub fn slice_to_string(chars: &Vec<u8>, start: usize, end: usize) -> String {
    // 使用vec和copy_from_slice 解决cannot move的问题
    let mut dst = vec![0; end - start];
    dst.copy_from_slice(&chars[start..end]);
    String::from_utf8(dst).unwrap()
}

/// 对齐到Align的整数倍
pub fn align_to(n: isize, align: isize) -> isize {
    // (0,Align]返回Align
    (n + align - 1) / align * align
}

/// 读取文件
pub fn read_file(path: &String) -> String {
    let mut buffer = String::new();
    if path.eq("-") {
        // 如果文件名是"-"，那么就从输入中读取
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        handle
            .read_to_string(&mut buffer)
            .expect("Error while reading file");
    } else {
        let mut file =
            File::open(path.to_string()).expect(format!("Open file ({}) failed", path).as_str());
        // read to file
        file.read_to_string(&mut buffer)
            .expect("Error while reading file");
    }

    return buffer;
}

/// 打开一个可写入的文件
pub fn open_file_for_write(path: &String) -> Box<dyn Write> {
    return if path.eq("-") || path.eq("") {
        let f = stdout();
        Box::new(f.lock())
    } else {
        let f = File::create(path);
        Box::new(f.expect("error to open file"))
    };
}

/// 向文件`file`写入字符串`string`
pub fn write_file(file: &mut impl Write, string: &str) {
    file.write_all(string.as_ref())
        .expect(format!("write file got error: {}", string).as_str());
}

/// 替换文件的后缀名
pub fn replace_extn(path: &String, extn: &str) -> String {
    if path.eq("-") || path.eq("") {
        return "-".to_string();
    }
    let s = path.split(".");
    format!("{}{}", s.collect::<Vec<_>>()[0], extn)
}

/// 查找文件
pub fn find_file(pattern: String) -> String {
    let files = glob::glob(pattern.as_str()).expect("Failed to read glob pattern");
    // 选择最后一条
    let last = files.last().unwrap();
    match last {
        Ok(path) => {
            return path.to_str().unwrap().to_string();
        }
        Err(_) => {
            panic!("Failed to read glob pattern")
        }
    }
}

/// 获取绝对目录名
pub fn dirname_absolute(path: String) -> String {
    // eprintln!("dirname_absolute : {}", path);
    // 先取自己的绝对路径
    let dir = Path::new(&path);
    let dir = fs::canonicalize(dir).expect("error path");
    let dir = dir.to_str().unwrap();
    // 再通过自己的绝对路径找到dir的绝对路径
    let dir = Path::new(dir).parent().unwrap();
    let dir = fs::canonicalize(dir).expect("error path");
    let dir = dir.to_str().unwrap();
    dir.to_string()
}

/// 获取相对目录名
pub fn dirname_relative(path: String) -> String {
    // eprintln!("dirname_relative : {}", path);
    let dir = Path::new(&path).parent().unwrap();
    if dir.to_str().unwrap().eq("") {
        // 当前目录
        return ".".to_string();
    }
    let dir = dir.to_str().unwrap();
    dir.to_string()
}

/// 获取目录名
pub fn dirname(path: String) -> String {
    // eprintln!("dirname : {}", path);
    return if path.starts_with("/") {
        dirname_absolute(path)
    } else {
        dirname_relative(path)
    };
}

/// vec[u8] to vec[i8]
pub fn vec_u8_into_i8(v: Vec<u8>) -> Vec<i8> {
    // ideally we'd use Vec::into_raw_parts, but it's unstable,
    // so we have to do it manually:

    // first, make sure v's destructor doesn't free the data
    // it thinks it owns when it goes out of scope
    let mut v = std::mem::ManuallyDrop::new(v);

    // then, pick apart the existing Vec
    let p = v.as_mut_ptr();
    let len = v.len();
    let cap = v.capacity();

    // finally, adopt the data into a new Vec
    unsafe { Vec::from_raw_parts(p as *mut i8, len, cap) }
}

/// vec[i8] to vec[u8]
#[allow(dead_code)]
pub fn vec_i8_into_u8(v: Vec<i8>) -> Vec<u8> {
    // ideally we'd use Vec::into_raw_parts, but it's unstable,
    // so we have to do it manually:

    // first, make sure v's destructor doesn't free the data
    // it thinks it owns when it goes out of scope
    let mut v = std::mem::ManuallyDrop::new(v);

    // then, pick apart the existing Vec
    let p = v.as_mut_ptr();
    let len = v.len();
    let cap = v.capacity();

    // finally, adopt the data into a new Vec
    unsafe { Vec::from_raw_parts(p as *mut u8, len, cap) }
}

/// 当指定-E选项时，打印出所有终结符
pub fn print_tokens(mut write_file: Box<dyn Write>, tokens: Vec<Token>) {
    // 记录行数
    let mut line = 1;
    // 遍历读取终结符
    for token in tokens {
        if token.at_eof() {
            break;
        }
        // 位于行首打印出换行符
        if line > 1 && token.at_bol() {
            write!(write_file, "\n").unwrap();
        }
        // 打印出需要空格的位置
        if token.has_space() && !token.at_bol() {
            write!(write_file, " ").unwrap();
        }
        // 打印出终结符
        write!(write_file, "{}", token.get_name()).unwrap();
        line += 1;
    }
    // 文件以换行符结尾
    write!(write_file, "\n").unwrap();
}
