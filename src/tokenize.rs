use crate::keywords::KEYWORDS;
use crate::{error_at, read_file, slice_to_string, Token, Type, FILE_NAME, INPUT};

/// 对文件的终结符解析
pub fn tokenize_file(path: String) -> Vec<Token> {
    let input = read_file(&path);
    // tokenize
    tokenize(path, input)
}

/// 终结符解析
pub fn tokenize(path: String, input: String) -> Vec<Token> {
    unsafe {
        FILE_NAME = path.to_string();
        INPUT = input.to_string();
    }

    let mut tokens: Vec<Token> = vec![];

    let chars = input.clone().into_bytes();
    let mut pos = 0;

    let mut line_no = 1usize;

    while pos < chars.len() {
        // 跳过行注释
        if starts_with(&chars, pos, "//") {
            pos += 2;

            loop {
                let c = chars[pos] as char;
                if c == '\n' {
                    line_no += 1; // 单核注释 行号+1
                    break;
                }
                pos += 1;
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
                }
                pos += 1;
            }
            continue;
        }

        let c = chars[pos] as char;
        // 统计代码里的回车
        if c == '\n' {
            line_no += 1;
        }
        let old_pos = pos;
        // 跳过所有空白符如：空格、回车
        if c.is_whitespace() {
            pos += 1;
            continue;
        }

        if c.is_digit(10) {
            // 初始化，类似于C++的构造函数
            // 我们不使用Head来存储信息，仅用来表示链表入口，这样每次都是存储在Cur->Next
            // 否则下述操作将使第一个Token的地址不在Head中。
            let val = read_int_literal(&chars, &mut pos);
            let t_str = slice_to_string(&chars, old_pos, pos);
            let t = Token::Num {
                val,
                t_str,
                offset: old_pos,
                line_no,
            };
            tokens.push(t);
            continue;
        }

        // 解析字符串字面量
        if c == '"' {
            let mut val = read_string_literal(&chars, &mut pos);
            val.push('\0' as u8);
            let len = val.len();
            let type_ = Type::array_of(Type::new_char(), len as isize);
            let t = Token::Str {
                val,
                type_,
                offset: old_pos,
                line_no,
            };
            tokens.push(t);
            pos += 1; // 跳过"
            continue;
        }

        if c == '\'' {
            let c = read_char_literal(&chars, &mut pos);
            tokens.push(Token::Num {
                val: c as i64,
                t_str: "".to_string(),
                offset: old_pos,
                line_no,
            });
            continue;
        }

        read_ident(&chars, &mut pos);
        if old_pos != pos {
            let t_str = slice_to_string(&chars, old_pos, pos);
            if KEYWORDS.contains(&&*t_str) {
                tokens.push(Token::Keyword {
                    t_str,
                    offset: old_pos,
                    line_no,
                })
            } else {
                tokens.push(Token::Ident {
                    t_str,
                    offset: old_pos,
                    line_no,
                });
            }
            continue;
        }

        // 解析操作符
        read_punct(&chars, &mut pos);
        if pos != old_pos {
            let t_str = slice_to_string(&chars, old_pos, pos);
            tokens.push(Token::Punct {
                t_str,
                offset: old_pos,
                line_no,
            });
            continue;
        }

        // 处理无法识别的字符
        error_at!(line_no, pos, "invalid token");
    }

    // 解析结束，增加一个EOF，表示终止符。
    tokens.push(Token::Eof {
        offset: pos,
        line_no,
    });

    // Head无内容，所以直接返回Next
    tokens
}

/// 传入程序的参数为str类型，因为需要转换为需要long类型
/// strtol为“string to long”，
/// 参数为：被转换的str，str除去数字后的剩余部分，进制
/// 传入&p，即char**, 是为了修改P的值
fn strtol(chars: &Vec<u8>, pos: &mut usize, base: u32) -> i64 {
    let mut result: i64 = 0;
    while *pos < chars.len() {
        if let Some(i) = (chars[*pos] as char).to_digit(base) {
            result = result * base as i64 + i as i64;
            *pos += 1;
        } else {
            break;
        }
    }
    result
}

fn starts_with(chars: &Vec<u8>, pos: usize, sub: &str) -> bool {
    let sub = sub.as_bytes();
    for i in 0..sub.len() {
        if sub[i] != chars[pos + i] {
            return false;
        }
    }
    true
}

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

fn read_punct(chars: &Vec<u8>, pos: &mut usize) {
    let ops = vec![
        "==", "!=", "<=", ">=", "->", "+=", "-=", "*=", "/=", "++", "--", "%=", "&=", "|=", "^=",
        "&&", "||",
    ];

    for op in ops {
        if starts_with(chars, *pos, op) {
            *pos += 2;
            return;
        }
    }

    let c = chars[*pos] as char;
    if c.is_ascii_punctuation() {
        *pos += 1;
    }
}

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

fn read_int_literal(chars: &Vec<u8>, pos: &mut usize) -> i64 {
    let c = chars[*pos] as char;
    let cnn = chars[*pos + 2] as char; //0x() or 0b()

    let mut radix = 10;
    if starts_with_ignore_case(chars, *pos, "0x") && cnn.is_digit(16) {
        radix = 16;
        *pos += 2;
    } else if starts_with_ignore_case(chars, *pos, "0b") && cnn.is_digit(2) {
        radix = 2;
        *pos += 2;
    } else if c == '0' {
        radix = 8;
    }

    let r = strtol(chars, pos, radix);
    let c = chars[*pos] as char;
    if c.is_alphabetic() {
        error_at!(0, *pos, "invalid digit");
    }

    r
}
