use crate::ctype::{Type, TypeLink};
use crate::keywords::KEYWORDS;
use crate::token::{File, Token};
use crate::unicode::{convert_universal_chars, decode_utf8};
use crate::{
    canonicalize_newline, error_at, error_token, from_hex, read_char, read_file,
    remove_backslash_newline, slice_to_string, starts_with, starts_with_ignore_case, FileLink,
    INPUT, INPUTS,
};

static mut FILE_NO: usize = 0;
/// 对文件的终结符解析
pub fn tokenize_file(path: String) -> Vec<Token> {
    let content = read_file(&path);
    if content.eq("") {
        return Vec::new();
    }
    let content = canonicalize_newline(content);
    let content = remove_backslash_newline(content);
    let content = convert_universal_chars(content);
    // 文件编号
    let file_no = unsafe { FILE_NO };
    // 文件路径，文件编号从1开始，文件内容
    let file = File::new_link(path, file_no + 1, content);
    unsafe {
        INPUTS.push(file.clone());
        FILE_NO += 1;
    }
    // tokenize
    tokenize(file)
}

/// 终结符解析
pub fn tokenize(input: FileLink) -> Vec<Token> {
    unsafe {
        INPUT = Some(input.clone());
    }

    let mut tokens: Vec<Token> = vec![];

    // 讲输入字符串转为字符vec,方便处理
    let chars = input.borrow().content.to_string().into_bytes();
    let mut pos = 0;
    let mut line_no = 1usize;
    let mut at_bol = true;
    let mut has_space = false;

    while pos < chars.len() {
        // 跳过行注释
        if starts_with(&chars, pos, "//") {
            pos += 2;

            loop {
                let c = read_char(&chars, pos);
                pos += 1;
                if c == '\n' {
                    at_bol = true;
                    break;
                }
            }
            line_no += 1;
            has_space = true;
            continue;
        }

        // 跳过块注释
        if starts_with(&chars, pos, "/*") {
            pos += 2;
            loop {
                if pos + 2 >= chars.len() {
                    error_at!(line_no, pos, "unclosed block comment")
                }
                // 查找第一个"*/"的位置
                if starts_with(&chars, pos, "*/") {
                    pos += 2; // maybe bug?
                    break;
                }
                // 统计多行测试里所有的回车
                let c = read_char(&chars, pos);
                if c == '\n' {
                    line_no += 1;
                    at_bol = true;
                }
                pos += 1;
            }
            has_space = true;
            continue;
        }

        let c = read_char(&chars, pos);
        // 统计代码里的回车
        if c == '\n' {
            pos += 1;
            line_no += 1;
            at_bol = true;
            has_space = true;
            continue;
        }
        let old_pos = pos;
        // 跳过所有空白符如：空格、回车
        if c.is_whitespace() {
            pos += 1;
            has_space = true;
            continue;
        }

        // 解析数值
        if c.is_digit(10) || (c == '.' && read_char(&chars, pos + 1).is_digit(10)) {
            pos += 1;
            loop {
                let c = read_char(&chars, pos);
                let cn = read_char(&chars, pos + 1);
                if c != '\0' && cn != '\0' && "eEpP".contains(c) && "+-".contains(cn) {
                    pos += 2;
                } else if c.is_ascii_alphanumeric() || c == '.' {
                    pos += 1;
                } else {
                    break;
                }
            }
            let name = slice_to_string(&chars, old_pos, pos);
            let token = Token::new_ppnum(has_space, at_bol, name, old_pos, line_no);
            tokens.push(token);
            at_bol = false;
            has_space = false;
            continue;
        }

        // 解析字符串字面量
        if c == '"' {
            let mut val = read_string_literal(&chars, &mut pos);
            val.push('\0' as u8);
            let len = val.len();
            let typ = Type::array_of(Type::new_char(), len as isize);
            let name = slice_to_string(&chars, old_pos, pos + 1);
            let token = Token::new_str(has_space, at_bol, name, old_pos, line_no, val, typ);
            tokens.push(token);
            at_bol = false;
            has_space = false;
            pos += 1; // 跳过"
            continue;
        }

        // UTF-8 string literal
        if starts_with(&chars, pos, "u8\"") {
            pos += 2;
            let mut val = read_string_literal(&chars, &mut pos);
            val.push('\0' as u8);
            let len = val.len();
            let typ = Type::array_of(Type::new_char(), len as isize);
            let name = slice_to_string(&chars, old_pos, pos + 1);
            let token = Token::new_str(has_space, at_bol, name, old_pos, line_no, val, typ);
            tokens.push(token);
            at_bol = false;
            has_space = false;
            pos += 1; // 跳过"
            continue;
        }

        // UTF-16 string literal
        if starts_with(&chars, pos, "u\"") {
            pos += 1;
            let mut val = read_utf16string_literal(&chars, &mut pos);
            val.push('\0' as u16);
            let len = val.len();
            let typ = Type::array_of(Type::new_unsigned_short(), len as isize);
            let name = slice_to_string(&chars, old_pos, pos + 1);
            let val = unsafe { val.align_to::<u8>().1.to_vec() };
            let token = Token::new_str(has_space, at_bol, name, old_pos, line_no, val, typ);
            tokens.push(token);
            at_bol = false;
            has_space = false;
            pos += 1; // 跳过"
            continue;
        }

        // UTF-32 string literal
        if starts_with(&chars, pos, "U\"") {
            pos += 1;
            let mut val = read_utf32string_literal(&chars, &mut pos);
            val.push('\0' as u32);
            let len = val.len();
            let typ = Type::array_of(Type::new_unsigned_int(), len as isize);
            let name = slice_to_string(&chars, old_pos, pos + 1);
            let val = unsafe { val.align_to::<u8>().1.to_vec() };
            let token = Token::new_str(has_space, at_bol, name, old_pos, line_no, val, typ);
            tokens.push(token);
            at_bol = false;
            has_space = false;
            pos += 1; // 跳过"
            continue;
        }

        // 解析字符字面量
        if c == '\'' {
            let c = read_char_literal(&chars, &mut pos, old_pos) as i8 as i64; // to char
            let name = slice_to_string(&chars, old_pos, pos);
            let token = Token::new_char_literal(
                has_space,
                at_bol,
                name,
                old_pos,
                line_no,
                c,
                Type::new_int(),
            );
            tokens.push(token);
            at_bol = false;
            has_space = false;
            continue;
        }

        // UTF-16 character literal
        if starts_with(&chars, pos, "u'") {
            let c = read_char_literal(&chars, &mut pos, old_pos + 1) & 0xffff;
            let name = slice_to_string(&chars, old_pos, pos);
            let token = Token::new_char_literal(
                has_space,
                at_bol,
                name,
                old_pos,
                line_no,
                c,
                Type::new_short(),
            );
            tokens.push(token);
            at_bol = false;
            has_space = false;
            continue;
        }

        // 宽字符字面量，占两个字节
        if starts_with(&chars, pos, "L'") {
            let c = read_char_literal(&chars, &mut pos, old_pos + 1);
            let name = slice_to_string(&chars, old_pos, pos);
            let token = Token::new_char_literal(
                has_space,
                at_bol,
                name,
                old_pos,
                line_no,
                c,
                Type::new_int(),
            );
            tokens.push(token);
            at_bol = false;
            has_space = false;
            continue;
        }

        // UTF-32 character literal
        if starts_with(&chars, pos, "U'") {
            let c = read_char_literal(&chars, &mut pos, old_pos + 1);
            let name = slice_to_string(&chars, old_pos, pos);
            let token = Token::new_char_literal(
                has_space,
                at_bol,
                name,
                old_pos,
                line_no,
                c,
                Type::new_unsigned_int(),
            );
            tokens.push(token);
            at_bol = false;
            has_space = false;
            continue;
        }

        // 解析标识符
        read_ident(&chars, &mut pos);
        if old_pos != pos {
            let name = slice_to_string(&chars, old_pos, pos);
            let t = Token::new_ident(has_space, at_bol, name, old_pos, line_no);
            tokens.push(t);
            at_bol = false;
            has_space = false;
            continue;
        }

        // 解析操作符
        read_punct(&chars, &mut pos);
        if pos != old_pos {
            let name = slice_to_string(&chars, old_pos, pos);
            let t = Token::new_punct(has_space, at_bol, name, old_pos, line_no);
            tokens.push(t);
            at_bol = false;
            has_space = false;
            continue;
        }

        // 处理无法识别的字符
        error_at!(line_no, pos, "invalid token");
    }

    // 解析结束，增加一个EOF，表示终止符。
    tokens.push(Token::new_eof(pos, line_no));

    // Head无内容，所以直接返回Next
    tokens
}

// 将所有关键字的终结符，都标记为KEYWORD
pub fn convert_pp_tokens(tokens: &mut Vec<Token>) {
    for token in tokens.iter_mut() {
        let name = token.get_name();
        if KEYWORDS.contains(&name.as_str()) {
            token.to_keyword();
        } else if token.is_ppnum() {
            convert_num(token);
        }
    }
}

/// 转换成num
fn convert_num(token: &mut Token) {
    let chars = token.get_name().into_bytes();
    let mut pos = 0;

    let (ival, fval, typ) = read_number(&chars, &mut pos);
    if pos != chars.len() {
        error_token!(token, "invalid numeric constant");
    }
    token.to_number(ival, fval, typ);
}

/// 读取一个操作符
fn read_punct(chars: &Vec<u8>, pos: &mut usize) {
    let ops = vec![
        "<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--",
        "%=", "&=", "|=", "^=", "&&", "||", "<<", ">>", "##",
    ];

    for op in ops {
        if starts_with(chars, *pos, op) {
            *pos += op.len();
            return;
        }
    }

    let c = read_char(chars, *pos);
    if c.is_ascii_punctuation() {
        *pos += 1;
    }
}

/// 读取一个id
fn read_ident(chars: &Vec<u8>, pos: &mut usize) {
    let c = read_char(chars, *pos);
    if c.is_alphabetic() || c == '_' {
        loop {
            *pos += 1;
            let c = read_char(chars, *pos);
            if !(c.is_alphabetic() || c == '_' || c.is_digit(10)) {
                break;
            }
        }
    }
}

/// 读取字符串字面量
fn read_string_literal(chars: &Vec<u8>, pos: &mut usize) -> Vec<u8> {
    let old_pos = *pos;
    string_literal_end(chars, pos);
    let mut new_chars = vec![];
    let mut i = old_pos + 1;
    while i < *pos {
        let mut c = read_char(chars, i);
        if c == '\\' {
            i += 1; // skip \\
            c = read_escaped_char(chars, &mut i) as u8 as char;
            new_chars.push(c as u8);
        } else {
            i += 1;
            new_chars.push(c as u8);
        }
    }

    new_chars
}

/// Read a UTF-8-encoded string literal and transcode it in UTF-16.
///
/// UTF-16 is yet another variable-width encoding for Unicode. Code
/// points smaller than U+10000 are encoded in 2 bytes. Code points
/// equal to or larger than that are encoded in 4 bytes. Each 2 bytes
/// in the 4 byte sequence is called "surrogate", and a 4 byte sequence
/// is called a "surrogate pair".
fn read_utf16string_literal(chars: &Vec<u8>, pos: &mut usize) -> Vec<u16> {
    let old_pos = *pos;
    string_literal_end(chars, pos);
    let mut new_chars = vec![];
    let mut i = old_pos + 1;
    while i < *pos {
        if read_char(chars, i) == '\\' {
            i += 1; // skip \\
            let c = read_escaped_char(chars, &mut i);
            new_chars.push(c as u16);
            continue;
        }
        let mut c = decode_utf8(chars, &mut i);
        if c < 0x10000 {
            // Encode a code point in 2 bytes.
            new_chars.push(c as u16);
        } else {
            // Encode a code point in 4 bytes.
            c -= 0x10000;
            new_chars.push(0xD800 + ((c >> 10) & 0x3FF) as u16);
            new_chars.push(0xDC00 + (c & 0x3FF) as u16);
        }
    }

    new_chars
}

/// Read a UTF-8-encoded string literal and transcode it in UTF-32.
///
/// UTF-32 is a fixed-width encoding for Unicode. Each code point is
/// encoded in 4 bytes.
fn read_utf32string_literal(chars: &Vec<u8>, pos: &mut usize) -> Vec<u32> {
    let old_pos = *pos;
    string_literal_end(chars, pos);
    let mut new_chars = vec![];
    let mut i = old_pos + 1;
    while i < *pos {
        if read_char(chars, i) == '\\' {
            i += 1; // skip \\
            let c = read_escaped_char(chars, &mut i);
            new_chars.push(c as u32);
        } else {
            let c = decode_utf8(chars, &mut i);
            new_chars.push(c);
        }
    }

    new_chars
}

/// 读取转义字符
fn read_escaped_char(chars: &Vec<u8>, pos: &mut usize) -> i32 {
    let mut c = read_char(chars, *pos);
    if '0' <= c && c <= '7' {
        // 读取一个八进制数字，不能长于三位
        // \abc = (a*8+b)*8+c
        let mut r = c as u8 - '0' as u8;
        *pos += 1;
        c = read_char(chars, *pos);
        if '0' <= c && c <= '7' {
            r = (r << 3) + (c as u8 - '0' as u8);
            *pos += 1;
            c = read_char(chars, *pos);
            if '0' <= c && c <= '7' {
                r = (r << 3) + (c as u8 - '0' as u8);
                *pos += 1;
            }
        }

        return r as i32;
    }

    if c == 'x' {
        *pos += 1;
        c = read_char(chars, *pos);
        if !c.is_digit(16) {
            error_at!(0, *pos, "invalid hex escape sequence");
            return '\0' as i32;
        }

        let mut r = 0i32;
        while c.is_digit(16) {
            r = (r << 4) + from_hex(c) as i32;
            *pos += 1;
            c = read_char(chars, *pos);
        }

        return r;
    }

    *pos += 1;
    match c {
        'a' => 7,  // 响铃（警报）
        'b' => 8,  // 退格
        't' => 9,  // 水平制表符，tab
        'n' => 10, // 换行
        'v' => 11, // 垂直制表符
        'f' => 12, // 换页
        'r' => 13, // 回车
        // 属于GNU C拓展
        'e' => 27,     // 转义符
        _ => c as i32, // 默认将原字符返回
    }
}

/// 读取到字符串字面量结尾
fn string_literal_end(chars: &Vec<u8>, pos: &mut usize) {
    *pos += 1; // 忽略"
    loop {
        let c = read_char(chars, *pos);
        if c == '"' {
            break;
        }
        if c == '\n' || c == '\0' {
            error_at!(0, *pos, "unclosed string literal");
            return;
        }
        if c == '\\' {
            // 遇到\\
            *pos += 1;
        }
        *pos += 1;
    }
}

/// 读取字符字面量
fn read_char_literal(chars: &Vec<u8>, pos: &mut usize, quote: usize) -> i64 {
    *pos = quote + 1; // 忽略'
    let mut c = read_char(chars, *pos);

    if c == '\0' {
        error_at!(0, *pos - 1, "unclosed char literal");
    }

    let r;
    if c == '\\' {
        *pos += 1;
        r = read_escaped_char(chars, pos) as i64;
    } else {
        r = decode_utf8(chars, pos) as i64;
    }

    c = read_char(chars, *pos);
    if c != '\'' {
        error_at!(0, *pos, "unclosed char literal");
    }

    *pos += 1; // 忽略尾部'
    return r;
}

/// 读取数字的字面量
fn read_int_literal(chars: &Vec<u8>, pos: &mut usize) -> (i64, TypeLink, u32) {
    let c = read_char(chars, *pos);
    let cnn = read_char(chars, *pos + 2); //0x() or 0b()

    // 读取二、八、十、十六进制
    // 默认为十进制
    let mut radix = 10;
    // 比较两个字符串前2个字符，忽略大小写，并判断是否为数字
    if starts_with_ignore_case(chars, *pos, "0x") && cnn.is_digit(16) {
        // 十六进制
        radix = 16;
        *pos += 2;
    } else if starts_with_ignore_case(chars, *pos, "0b") && cnn.is_digit(2) {
        // 二进制
        radix = 2;
        *pos += 2;
    } else if c == '0' {
        // 八进制
        radix = 8;
    }

    let c = read_char(chars, *pos);
    if c == '.' {
        // 可能是0x.111
        return (0, Type::new_float(), radix);
    }
    // 将字符串转换为Base进制的数字
    let result = strtol(chars, pos, radix);

    // 读取U L LL后缀
    let mut u = false;
    let mut l = false;

    if starts_with_ignore_case(chars, *pos, "llu") || starts_with_ignore_case(chars, *pos, "ull") {
        // LLU
        *pos += 3;
        u = true;
        l = true;
    } else if starts_with_ignore_case(chars, *pos, "lu")
        || starts_with_ignore_case(chars, *pos, "ul")
    {
        // LU
        *pos += 2;
        u = true;
        l = true;
    } else if starts_with_ignore_case(chars, *pos, "ll") {
        // LL
        *pos += 2;
        l = true;
    } else if starts_with_ignore_case(chars, *pos, "l") {
        // L
        *pos += 1;
        l = true;
    } else if starts_with_ignore_case(chars, *pos, "u") {
        // U
        *pos += 1;
        u = true;
    }

    // 推断出类型，采用能存下当前数值的类型
    let typ;
    if radix == 10 {
        if l && u {
            typ = Type::new_unsigned_long()
        } else if l {
            typ = Type::new_long()
        } else if u {
            typ = if (result >> 32) > 0 {
                Type::new_unsigned_long()
            } else {
                Type::new_unsigned_int()
            }
        } else {
            typ = if (result >> 31) > 0 {
                Type::new_long()
            } else {
                Type::new_int()
            }
        }
    } else {
        if l && u {
            typ = Type::new_unsigned_long()
        } else if l {
            typ = if (result >> 63) > 0 {
                Type::new_unsigned_long()
            } else {
                Type::new_long()
            }
        } else if u {
            typ = if (result >> 32) > 0 {
                Type::new_unsigned_long()
            } else {
                Type::new_unsigned_int()
            }
        } else if (result >> 63) > 0 {
            typ = Type::new_unsigned_long()
        } else if (result >> 32) > 0 {
            typ = Type::new_long();
        } else if (result >> 31) > 0 {
            typ = Type::new_unsigned_int();
        } else {
            typ = Type::new_int();
        }
    }

    (result as i64, typ, radix)
}

fn read_number(chars: &Vec<u8>, pos: &mut usize) -> (i64, f64, TypeLink) {
    let start_pos = *pos;
    // 尝试解析整型常量
    let (val, typ, raidx) = read_int_literal(chars, pos);
    let c = read_char(chars, *pos);
    // 2进制 | 非16进制 不带.或者e或者f后缀 | 16进制 不带.或者p后缀, 则为整型
    if raidx == 2 || (raidx != 16 && !".eEfF".contains(c)) || (raidx == 16 && !".pP".contains(c)) {
        return (val, 0.0, typ);
    }

    let mut result: f64 = 0.0;
    let mut check_suffix = false;
    if raidx == 16 && c == '.' {
        // 解析16进制+小数+pow x的形式
        let mut fval = read_hex_fraction(chars, pos);
        let pow_x = read_hex_exponent(chars, pos);
        fval += val as f64;
        result = fval * f64::powi(2.0, pow_x); // may be overflow
    } else if raidx == 16 && (c == 'p' || c == 'P') {
        // 解析16进制 pow x的形式
        let pow_x = read_hex_exponent(chars, pos);
        let fval = val as f64;
        result = f64::powi(fval, pow_x); // may be overflow
    } else if raidx == 10 && ".eEfF".contains(c) {
        // 解析10进制 + 小数 (e x) (f) 的形式
        let mut fval = read_fraction(chars, pos);
        let exp = read_exponent(chars, pos);
        fval += val as f64;
        result = fval * f64::powi(10.0, exp); // may be overflow
        check_suffix = true;
    } else if raidx == 8 && ".eEfF".contains(c) {
        // 从头按十进制解析整数部分 + 小数 (e x) (f) 的形式
        *pos = start_pos + 1; // 恢复到开头并跳过0
        let val = strtol(chars, pos, 10); // 把8进制按10进制读
        let mut fval = read_fraction(chars, pos);
        let exp = read_exponent(chars, pos);
        fval += val as f64;
        result = fval * f64::powi(10.0, exp); // may be overflow
        check_suffix = true;
    }

    let mut typ = Type::new_double();
    if check_suffix {
        let c = read_char(chars, *pos);
        if c == 'f' || c == 'F' {
            typ = Type::new_float();
            *pos += 1;
        } else if c == 'l' || c == 'L' {
            *pos += 1;
        }
    }

    (0, result + 0.000000, typ)
}

/// 读取十进制小数部分
fn read_fraction(chars: &Vec<u8>, pos: &mut usize) -> f64 {
    if read_char(chars, *pos) != '.' {
        // 没有小数部分,直接返回0
        return 0.0;
    }
    *pos += 1; // 跳过点

    let mut result = 0.0;
    let mut base = 10.0;
    while *pos < chars.len() {
        if let Some(i) = (read_char(chars, *pos)).to_digit(10) {
            result += i as f64 / base;
            *pos += 1;
            base *= 10.0;
        } else {
            break;
        }
    }

    result
}

fn read_exponent(chars: &Vec<u8>, pos: &mut usize) -> i32 {
    let c = read_char(chars, *pos);
    if c != 'e' && c != 'E' {
        return 0;
    }
    *pos += 1;

    let mut neg = false;
    let c = read_char(chars, *pos);
    if c == '-' || c == '+' {
        neg = c == '-';
        *pos += 1;
    }

    if !(read_char(chars, *pos)).is_digit(10) {
        error_at!(0, *pos, "malformed floating-point literal");
    }

    let mut val = strtol(chars, pos, 10) as i32;

    if neg {
        val *= -1; // 负号
    }
    val
}

/// 读取十六进制小数部分
fn read_hex_fraction(chars: &Vec<u8>, pos: &mut usize) -> f64 {
    *pos += 1; // 跳过点

    let mut result = 0.0;
    let mut base = 0;
    while *pos < chars.len() {
        if let Some(i) = (read_char(chars, *pos)).to_digit(16) {
            if i & 0x8 != 0 {
                result += f64::powi(2.0, base - 1);
            }
            if i & 0x4 != 0 {
                result += f64::powi(2.0, base - 2);
            }
            if i & 0x2 != 0 {
                result += f64::powi(2.0, base - 3);
            }
            if i & 0x1 != 0 {
                let a = f64::powi(2.0, base - 4);
                result += a;
            }
            *pos += 1;
            base -= 4;
        } else {
            break;
        }
    }

    result
}

/// 读取十六进制pow x
fn read_hex_exponent(chars: &Vec<u8>, pos: &mut usize) -> i32 {
    let c = read_char(chars, *pos);
    if c != 'p' && c != 'P' {
        error_at!(0, *pos, "malformed floating-point literal");
    }
    *pos += 1;

    let mut neg = false;
    let c = read_char(chars, *pos);
    if c == '-' || c == '+' {
        neg = c == '-';
        *pos += 1;
    }

    if !(read_char(chars, *pos)).is_digit(10) {
        error_at!(0, *pos, "malformed floating-point literal");
    }

    let mut val = strtol(chars, pos, 10) as i32;

    if neg {
        val *= -1; // 负号
    }
    val
}

/// 传入程序的参数为str类型，因为需要转换为需要long类型
/// strtol为“string to long”，
/// 参数为：被转换的str，str除去数字后的剩余部分，进制
/// 传入&p，即char**, 是为了修改P的值
fn strtol(chars: &Vec<u8>, pos: &mut usize, base: u32) -> u64 {
    let mut result: u64 = 0;
    while *pos < chars.len() {
        if let Some(i) = (read_char(chars, *pos)).to_digit(base) {
            result = result * base as u64 + i as u64;
            *pos += 1;
        } else {
            break;
        }
    }
    result
}
