use crate::{error_at, INPUT, Token, Type};
use crate::keywords::KEYWORDS;

/// 终结符解析
pub fn tokenize() -> Vec<Token> {
    let input = unsafe { &INPUT };

    let mut tokens: Vec<Token> = vec![];

    let chars = input.clone().into_bytes();
    let mut pos = 0;

    while pos < chars.len() {
        let c = chars[pos] as char;
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
            let val = strtol(&chars, &mut pos, 10) as i32;
            let t_str = slice_to_string(&chars, old_pos, pos);
            let t = Token::Num { val, t_str, offset: old_pos };
            tokens.push(t);
            continue;
        }

        // 解析字符串字面量
        if c == '"' {
            read_string_literal(&chars, &mut pos);
            let mut val = slice_to_string(&chars, old_pos + 1, pos); // +1 -1 是为了忽略两边的双引号
            val.push('\0');
            let len = pos - old_pos;
            let type_ = Type::array_of(Type::new_char(), len);
            let t = Token::Str { val, type_, offset: old_pos };
            tokens.push(t);
            pos += 1; // 跳过"
            continue;
        }

        read_ident(&chars, &mut pos);
        if old_pos != pos {
            let t_str = slice_to_string(&chars, old_pos, pos);
            if KEYWORDS.contains(&&*t_str) {
                tokens.push(Token::Keyword { t_str, offset: old_pos })
            } else {
                tokens.push(Token::Ident { t_str, offset: old_pos });
            }
            continue;
        }

        // 解析操作符
        read_punct(&chars, &mut pos);
        if pos != old_pos {
            let t_str = slice_to_string(&chars, old_pos, pos);
            tokens.push(Token::Punct { t_str, offset: old_pos });
            continue;
        }

        // 处理无法识别的字符
        error_at!(pos, "invalid token");
    }

    // 解析结束，增加一个EOF，表示终止符。
    tokens.push(Token::Eof { offset: pos });

    // Head无内容，所以直接返回Next
    tokens
}

fn slice_to_string(chars: &Vec<u8>, start: usize, end: usize) -> String {
    // 使用vec和copy_from_slice 解决cannot move的问题
    let mut dst = vec![0; end - start];
    dst.copy_from_slice(&chars[start..end]);
    String::from_utf8(dst).unwrap()
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

fn read_punct(chars: &Vec<u8>, pos: &mut usize) {
    if starts_with(chars, *pos, "==")
        || starts_with(chars, *pos, "!=")
        || starts_with(chars, *pos, ">=")
        || starts_with(chars, *pos, "<=") {
        *pos += 2;
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

fn read_string_literal(chars: &Vec<u8>, pos: &mut usize) {
    *pos += 1; // 忽略"
    loop {
        let c = chars[*pos] as char;
        if c == '"' {
            break;
        }
        if c == '\n' || c == '\0' {
            error_at!(*pos, "unclosed string literal");
            return;
        }
        *pos += 1;
    }
}