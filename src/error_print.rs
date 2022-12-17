use core::fmt;
use crate::{FILE_NAME, INPUT, slice_to_string, Token};

/// 根据偏移位置计算出行号和行的起始与结束
fn find_line_info(chars: &Vec<u8>, offset: usize) -> (usize, usize, usize) {
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
    // 计算行号
    let mut i = 0usize;
    while i < offset {
        let c = chars[i] as char;
        if c == '\n' {
            line_no += 1;
        }
        i += 1;
    }
    // 返回 行号, 起始, 结束
    (line_no, line_start, line_end)
}

// 字符解析出错，并退出程序
pub fn print_with_error(offset: usize, args: fmt::Arguments) {
    let input = unsafe { INPUT.to_string().into_bytes() };
    let file_name = unsafe { FILE_NAME.to_string() };
    let (line_no, line_start, line_end) = find_line_info(&input, offset);

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
    panic!("error at offset: {}", offset);
}

// Tok解析出错，并退出程序
pub fn print_with_token_error(token: &Token, args: fmt::Arguments) {
    print_with_error(token.get_offset(), args);
}


/// error at offset
#[macro_export]
macro_rules! error_at {
    ($offset:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::error_print::print_with_error($offset, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}


/// error token
#[macro_export]
macro_rules! error_token {
    ($token:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::error_print::print_with_token_error($token, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}