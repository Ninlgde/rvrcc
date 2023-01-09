//! 错误打印相关宏

use crate::token::Token;
use crate::{slice_to_string, FileLink, INPUT};
use std::fmt;

/// 根据偏移位置计算出行号和行的起始与结束
fn find_line_info(chars: &Vec<u8>, s_line_no: usize, offset: usize) -> (usize, usize, usize) {
    let mut line_start = offset;
    let mut line_end = offset;
    let mut line_no = 1;
    // 计算行开始字符
    loop {
        if !(line_start > 0 && chars[line_start - 1] as char != '\n') {
            break;
        }
        line_start -= 1;
    }
    // 计算行结束字符
    loop {
        if !(line_end < chars.len() && chars[line_end] as char != '\n') {
            break;
        }
        line_end += 1;
    }
    if s_line_no == 0 {
        // 计算行号
        let mut i = 0usize;
        while i < offset {
            let c = chars[i] as char;
            if c == '\n' {
                line_no += 1;
            }
            i += 1;
        }
    } else {
        line_no = s_line_no;
    }
    // 返回 行号, 起始, 结束
    (line_no, line_start, line_end)
}

/// 字符解析出错
pub fn print_with_error(file: FileLink, mut line_no: usize, offset: usize, args: fmt::Arguments) {
    let input = file.borrow().content.to_string().into_bytes();
    let file_name = file.borrow().name.to_string();
    let (n_line_no, line_start, line_end) = find_line_info(&input, line_no, offset);
    line_no = if line_no == 0 { n_line_no } else { line_no };

    let file_lineno = format!("{}:{}: ", file_name, line_no);
    let line = slice_to_string(&input, line_start, line_end);
    // 计算错误信息位置，在当前行内的偏移量+前面输出了多少个字符
    let pos = offset - line_start + file_lineno.len();

    // 输出 文件名:错误行
    print!("{}", file_lineno);
    // 输出line的行内所有字符（不含换行符）
    print!("{}\n", line);
    print!("{:1$}^", "", pos);
    print!(" {}\n", args);
}

/// Tok解析出错，并退出程序
pub fn print_error_with_token(token: &Token, args: fmt::Arguments) {
    print_with_error(
        token.get_file().unwrap(),
        token.get_line_no(),
        token.get_offset(),
        args,
    );
    panic!(
        "error at line no.{} offset: {}",
        token.get_line_no(),
        token.get_offset()
    );
}

/// token解析产生警告
pub fn print_warn_with_token(token: &Token, args: fmt::Arguments) {
    print_with_error(
        token.get_file().unwrap(),
        token.get_line_no(),
        token.get_offset(),
        args,
    );
}

/// 某个位置解析出错
pub fn print_error_without_token(line_no: usize, offset: usize, args: fmt::Arguments) {
    let file = unsafe { INPUT.as_ref().unwrap().clone() };
    print_with_error(file, line_no, offset, args);
    panic!("error at line no.{} offset: {}", line_no, offset);
}

/// error at offset
#[macro_export]
macro_rules! error_at {
    ($line_no:expr, $offset:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::error_print::print_error_without_token($line_no, $offset, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}

/// error when parse token
#[macro_export]
macro_rules! error_token {
    ($token:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::error_print::print_error_with_token($token, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}

/// warn when parse token
#[macro_export]
macro_rules! warn_token {
    ($token:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::error_print::print_warn_with_token($token, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}
