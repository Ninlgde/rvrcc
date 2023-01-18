use crate::ctype::Type;
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

/// 向下对齐值
/// N % Align != 0 , 即 N 未对齐时,  AlignDown(N) = AlignTo(N) - Align
/// N % Align == 0 , 即 N 已对齐时， AlignDown(N) = AlignTo(N)
pub fn align_down(n: isize, align: isize) -> isize {
    align_to(n - align + 1, align)
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
pub fn find_file(pattern: &str) -> String {
    let files = glob::glob(pattern).expect("Failed to read glob pattern");
    // 选择最后一条
    let last = files.last();
    if last.is_some() {
        let last = last.unwrap();
        return match last {
            Ok(path) => path.to_str().unwrap().to_string(),
            Err(_) => "".to_string(),
        };
    }
    return "".to_string();
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

/// 文件是否存在
pub fn file_exists(file: &str) -> bool {
    Path::new(file).exists()
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

/// 返回一位十六进制转十进制
/// hexDigit = [0-9a-fA-F]
/// 16: 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
/// 10: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
pub fn from_hex(c: char) -> u8 {
    if '0' <= c && c <= '9' {
        return c as u8 - '0' as u8;
    }

    if 'a' <= c && c <= 'f' {
        return c as u8 - 'a' as u8 + 10;
    }

    return c as u8 - 'A' as u8 + 10;
}

/// 从chars里读取一直字符
/// 超过上限的返回\0
pub fn read_char(chars: &Vec<u8>, pos: usize) -> char {
    if pos >= chars.len() {
        return '\0';
    }
    chars[pos] as char
}

/// 判断chars中pos开头的字符是否与sub字符串匹配
pub fn starts_with(chars: &Vec<u8>, pos: usize, sub: &str) -> bool {
    let sub = sub.as_bytes();
    for i in 0..sub.len() {
        let char = read_char(chars, pos + i) as u8;
        if sub[i] != char {
            return false;
        }
    }
    true
}

/// 判断chars中pos开头的字符是否与sub字符串匹配
/// 忽略大小写
pub fn starts_with_ignore_case(chars: &Vec<u8>, pos: usize, sub: &str) -> bool {
    let binding = sub.to_ascii_lowercase();
    let sub = binding.as_bytes();
    for i in 0..sub.len() {
        // 'a' - 'A' = 32
        let char = read_char(chars, pos + i);
        if !char.eq_ignore_ascii_case(&(sub[i] as char)) {
            return false;
        }
    }
    true
}

/// Replaces \r or \r\n with \n.
pub fn canonicalize_newline(input: String) -> String {
    let mut chars = input.into_bytes();
    // 旧字符串的索引I（从0开始）
    // 新字符串的索引J（从0开始）
    // 因为J始终<=I，所以二者共用空间，不会有问题
    let mut i = 0;
    let mut j = 0;

    while i < chars.len() {
        if chars[i] == 0x0d && chars[i + 1] == 0x0a {
            i += 2;
            chars[j] = 0x0a;
        } else if chars[i] == 0x0d {
            i += 1;
            chars[j] = 0x0d;
        } else {
            chars[j] = chars[i];
            i += 1;
        }
        j += 1;
    }

    // 截取[0..j) 返回字符串
    String::from_utf8_lossy(&chars[0..j]).to_string()
}

/// 移除续行，即反斜杠+换行符的形式
/// Removes backslashes followed by a newline.
pub fn remove_backslash_newline(input: String) -> String {
    let mut chars = input.into_bytes();
    // 旧字符串的索引I（从0开始）
    // 新字符串的索引J（从0开始）
    // 因为J始终<=I，所以二者共用空间，不会有问题
    let mut i = 0;
    let mut j = 0;
    // 为了维持行号不变，这里记录了删除的行数
    let mut n = 0;

    while i < chars.len() {
        // 如果是 '\\'和'\n'
        if chars[i] == 0x5c && chars[i + 1] == 0x0a {
            // I跳过这两个字符
            i += 2;
            // 删除的行数+1
            n += 1;
        }
        // 如果是换行符
        else if chars[i] == 0x0a {
            // P[J]='\n'
            // I、J都+1
            chars[j] = chars[i];
            i += 1;
            j += 1;
            // 如果删除过N个续行，那么在这里增加N个换行
            // 以保证行号不变
            while n > 0 {
                chars[j] = 0x0a;
                j += 1;
                n -= 1;
            }
        }
        // 其他情况，P[J]=P[I]
        // I、J都+1
        else {
            chars[j] = chars[i];
            i += 1;
            j += 1;
        }
    }

    // 如果最后还删除过N个续行，那么在这里增加N个换行
    while n > 0 {
        chars[j] = 0x0a;
        j += 1;
        n -= 1;
    }

    // 截取[0..j) 返回字符串
    String::from_utf8_lossy(&chars[0..j]).to_string()
}

/// 拼接相邻的字符串
pub fn join_adjacent_string_literals(mut tokens: Vec<Token>) -> Vec<Token> {
    // 旧token的索引I（从0开始）
    // 新token的索引J（从0开始）
    // 因为J始终<=I，所以二者共用空间，不会有问题
    let mut i = 0;
    let mut j = 0;

    while i < tokens.len() {
        let cur = &tokens[i];
        if !cur.is_string() || !tokens[i + 1].is_string() {
            tokens[j] = cur.clone();
            i += 1;
            j += 1;
            continue;
        }

        // 拼接i 和i+1
        let (t1, _) = cur.get_string();
        let (mut t2, _) = tokens[i + 1].get_string();
        // 要去掉i末尾的\0
        let mut t = t1[0..t1.len() - 1].to_vec();
        t.append(&mut t2);
        let len = t.len() as isize;
        let mut nt = Token::form(cur);
        nt.set_string(t, Type::array_of(Type::new_char(), len));

        // 把拼好的放在i+1的位置上,并且i++ 继续往后找
        tokens[i + 1] = nt;
        i += 1;
    }

    tokens[0..j].to_vec()
}
