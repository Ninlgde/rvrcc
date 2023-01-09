use crate::ctype::{Type, TypeLink};
use crate::keywords::KEYWORDS;
use crate::token::{File, Token};
use crate::{error_at, read_file, slice_to_string, FileLink, INPUT, INPUTS};

static mut FILE_NO: usize = 0;
/// 对文件的终结符解析
pub fn tokenize_file(path: String) -> Vec<Token> {
    let content = read_file(&path);
    if content.eq("") {
        return Vec::new();
    }
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

    while pos < chars.len() {
        // 跳过行注释
        if starts_with(&chars, pos, "//") {
            pos += 2;

            loop {
                let c = chars[pos] as char;
                pos += 1;
                if c == '\n' {
                    line_no += 1;
                    at_bol = true;
                    break;
                }
            }
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
                let c = chars[pos] as char;
                if c == '\n' {
                    line_no += 1;
                    at_bol = true;
                }
                pos += 1;
            }
            continue;
        }

        let c = chars[pos] as char;
        // 统计代码里的回车
        if c == '\n' {
            pos += 1;
            line_no += 1;
            at_bol = true;
            continue;
        }
        let old_pos = pos;
        // 跳过所有空白符如：空格、回车
        if c.is_whitespace() {
            pos += 1;
            continue;
        }

        // 解析数值
        if c.is_digit(10) || (c == '.' && (chars[pos + 1] as char).is_digit(10)) {
            // 初始化，类似于C++的构造函数
            // 我们不使用Head来存储信息，仅用来表示链表入口，这样每次都是存储在Cur->Next
            // 否则下述操作将使第一个Token的地址不在Head中。
            let (val, fval, typ) = read_number(&chars, &mut pos);
            let t_str = slice_to_string(&chars, old_pos, pos);
            let token = Token::new_num(at_bol, t_str, old_pos, line_no, val, fval, typ);
            tokens.push(token);
            at_bol = false;
            continue;
        }

        // 解析字符串字面量
        if c == '"' {
            let mut val = read_string_literal(&chars, &mut pos);
            val.push('\0' as u8);
            let len = val.len();
            let typ = Type::array_of(Type::new_char(), len as isize);
            let token = Token::new_str(at_bol, old_pos, line_no, val, typ);
            tokens.push(token);
            at_bol = false;
            pos += 1; // 跳过"
            continue;
        }

        // 解析字符字面量
        if c == '\'' {
            let c = read_char_literal(&chars, &mut pos);
            let token = Token::new_char_literal(at_bol, old_pos, line_no, c);
            tokens.push(token);
            at_bol = false;
            continue;
        }

        read_ident(&chars, &mut pos);
        if old_pos != pos {
            let t_str = slice_to_string(&chars, old_pos, pos);
            let t = Token::new_ident(at_bol, t_str, old_pos, line_no);
            tokens.push(t);
            at_bol = false;
            continue;
        }

        // 解析操作符
        read_punct(&chars, &mut pos);
        if pos != old_pos {
            let t_str = slice_to_string(&chars, old_pos, pos);
            let t = Token::new_punct(at_bol, t_str, old_pos, line_no);
            tokens.push(t);
            at_bol = false;
            continue;
        }

        // 处理无法识别的字符
        error_at!(line_no, pos, "invalid token");
    }

    // 解析结束，增加一个EOF，表示终止符。
    tokens.push(Token::new_eof(at_bol, pos, line_no));

    // Head无内容，所以直接返回Next
    tokens
}

// 将所有关键字的终结符，都标记为KEYWORD
pub fn convert_keywords(tokens: &mut Vec<Token>) {
    for token in tokens.iter_mut() {
        let name = token.get_name();
        if KEYWORDS.contains(&name.as_str()) {
            token.to_keyword();
        }
    }
}

/// 判断chars中pos开头的字符是否与sub字符串匹配
fn starts_with(chars: &Vec<u8>, pos: usize, sub: &str) -> bool {
    let sub = sub.as_bytes();
    for i in 0..sub.len() {
        if sub[i] != chars[pos + i] {
            return false;
        }
    }
    true
}

/// 判断chars中pos开头的字符是否与sub字符串匹配
/// 忽略大小写
fn starts_with_ignore_case(chars: &Vec<u8>, pos: usize, sub: &str) -> bool {
    let binding = sub.to_ascii_lowercase();
    let sub = binding.as_bytes();
    for i in 0..sub.len() {
        // 'a' - 'A' = 32
        if sub[i] != chars[pos + i] && sub[i] - 32 != chars[pos + i] {
            return false;
        }
    }
    true
}

/// 读取一个操作符
fn read_punct(chars: &Vec<u8>, pos: &mut usize) {
    let ops = vec![
        "<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--",
        "%=", "&=", "|=", "^=", "&&", "||", "<<", ">>",
    ];

    for op in ops {
        if starts_with(chars, *pos, op) {
            *pos += op.len();
            return;
        }
    }

    let c = chars[*pos] as char;
    if c.is_ascii_punctuation() {
        *pos += 1;
    }
}

/// 读取一个id
fn read_ident(chars: &Vec<u8>, pos: &mut usize) {
    let c = chars[*pos] as char;
    if c.is_alphabetic() || c == '_' {
        loop {
            *pos += 1;
            let c = chars[*pos] as char;
            if !(c.is_alphabetic() || c == '_' || c.is_digit(10)) {
                break;
            }
        }
    }
}

/// 读取字符串字面量
fn read_string_literal(chars: &Vec<u8>, pos: &mut usize) -> Vec<u8> {
    let old_pos = *pos; // +1 忽略"
    string_literal_end(chars, pos);
    let mut new_chars = vec![];
    let mut i = old_pos + 1;
    while i < *pos {
        let mut c = chars[i] as char;
        if c == '\\' {
            i += 1; // skip \\
            c = read_escaped_char(chars, &mut i);
            new_chars.push(c as u8);
        } else {
            i += 1;
            new_chars.push(c as u8);
        }
    }

    new_chars
}

/// 读取转义字符
fn read_escaped_char(chars: &Vec<u8>, pos: &mut usize) -> char {
    let mut c = chars[*pos] as char;
    if '0' <= c && c <= '7' {
        // 读取一个八进制数字，不能长于三位
        // \abc = (a*8+b)*8+c
        let mut r = c as u8 - '0' as u8;
        *pos += 1;
        c = chars[*pos] as char;
        if '0' <= c && c <= '7' {
            r = (r << 3) + (c as u8 - '0' as u8);
            *pos += 1;
            c = chars[*pos] as char;
            if '0' <= c && c <= '7' {
                r = (r << 3) + (c as u8 - '0' as u8);
                *pos += 1;
            }
        }

        return r as char;
    }

    if c == 'x' {
        *pos += 1;
        c = chars[*pos] as char;
        if !c.is_digit(16) {
            error_at!(0, *pos, "invalid hex escape sequence");
            return '\0';
        }

        let mut r = 0u8;
        while c.is_digit(16) {
            r = (r << 4) + from_hex(c);
            *pos += 1;
            c = chars[*pos] as char;
        }

        return r as char;
    }

    *pos += 1;
    match c {
        'a' => 7 as char,  // 响铃（警报）
        'b' => 8 as char,  // 退格
        't' => 9 as char,  // 水平制表符，tab
        'n' => 10 as char, // 换行
        'v' => 11 as char, // 垂直制表符
        'f' => 12 as char, // 换页
        'r' => 13 as char, // 回车
        // 属于GNU C拓展
        'e' => 27 as char, // 转义符
        _ => c,            // 默认将原字符返回
    }
}

/// 读取到字符串字面量结尾
fn string_literal_end(chars: &Vec<u8>, pos: &mut usize) {
    *pos += 1; // 忽略"
    loop {
        let c = chars[*pos] as char;
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

// 返回一位十六进制转十进制
// hexDigit = [0-9a-fA-F]
// 16: 0 1 2 3 4 5 6 7 8 9  A  B  C  D  E  F
// 10: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
fn from_hex(c: char) -> u8 {
    if '0' <= c && c <= '9' {
        return c as u8 - '0' as u8;
    }

    if 'a' <= c && c <= 'f' {
        return c as u8 - 'a' as u8 + 10;
    }

    return c as u8 - 'A' as u8 + 10;
}

/// 读取字符字面量
fn read_char_literal(chars: &Vec<u8>, pos: &mut usize) -> char {
    *pos += 1; // 忽略'
    let mut c = chars[*pos] as char;

    if c == '\0' {
        error_at!(0, *pos - 1, "unclosed char literal");
    }

    let r;
    if c == '\\' {
        *pos += 1;
        r = read_escaped_char(chars, pos);
    } else {
        r = chars[*pos] as char;
        *pos += 1;
    }

    c = chars[*pos] as char;
    if c != '\'' {
        error_at!(0, *pos, "unclosed char literal");
    }

    *pos += 1; // 忽略尾部'
    return r;
}

/// 读取数字的字面量
fn read_int_literal(chars: &Vec<u8>, pos: &mut usize) -> (i64, TypeLink, u32) {
    let c = chars[*pos] as char;
    let cnn = chars[*pos + 2] as char; //0x() or 0b()

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

    let c = chars[*pos] as char;
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
    let c = chars[*pos] as char;
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
        let c = chars[*pos] as char;
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
    if chars[*pos] as char != '.' {
        // 没有小数部分,直接返回0
        return 0.0;
    }
    *pos += 1; // 跳过点

    let mut result = 0.0;
    let mut base = 10.0;
    while *pos < chars.len() {
        if let Some(i) = (chars[*pos] as char).to_digit(10) {
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
    let c = chars[*pos] as char;
    if c != 'e' && c != 'E' {
        return 0;
    }
    *pos += 1;

    let mut neg = false;
    let c = chars[*pos] as char;
    if c == '-' || c == '+' {
        neg = c == '-';
        *pos += 1;
    }

    if !(chars[*pos] as char).is_digit(10) {
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
        if let Some(i) = (chars[*pos] as char).to_digit(16) {
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
    let c = chars[*pos] as char;
    if c != 'p' && c != 'P' {
        error_at!(0, *pos, "malformed floating-point literal");
    }
    *pos += 1;

    let mut neg = false;
    let c = chars[*pos] as char;
    if c == '-' || c == '+' {
        neg = c == '-';
        *pos += 1;
    }

    if !(chars[*pos] as char).is_digit(10) {
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
        if let Some(i) = (chars[*pos] as char).to_digit(base) {
            result = result * base as u64 + i as u64;
            *pos += 1;
        } else {
            break;
        }
    }
    result
}
