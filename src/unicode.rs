//! unicode

use crate::{from_hex, starts_with};

/// Replace \u or \U escape sequences with corresponding UTF-8 bytes.
pub fn convert_universal_chars(input: String) -> String {
    let mut chars = input.into_bytes();
    // 旧字符串的索引I（从0开始）
    // 新字符串的索引J（从0开始）
    // 因为J始终<=I，所以二者共用空间，不会有问题
    let mut i = 0;
    let mut j = 0;

    while i < chars.len() {
        if starts_with(&chars, i, "\\u") {
            let c = read_universal_char(&chars, i + 2, 4);
            if c != 0 {
                i += 6;
                j += encode_utf8(&mut chars, j, c);
            } else {
                chars[j] = chars[i];
                i += 1;
                j += 1;
            }
        } else if starts_with(&chars, i, "\\U") {
            let c = read_universal_char(&chars, i + 2, 8);
            if c != 0 {
                i += 10;
                j += encode_utf8(&mut chars, j, c);
            } else {
                chars[j] = chars[i];
                i += 1;
                j += 1;
            }
        } else if chars[i] == '\\' as u8 {
            chars[j] = chars[i];
            chars[j + 1] = chars[i + 1];
            i += 2;
            j += 2;
        } else {
            chars[j] = chars[i];
            i += 1;
            j += 1;
        }
    }

    // 截取[0..j) 返回字符串
    String::from_utf8_lossy(&chars[0..j]).to_string()
}

/// 读取 uni char
fn read_universal_char(chars: &Vec<u8>, pos: usize, len: usize) -> u32 {
    let mut result = 0u32;
    for i in 0..len {
        let c = chars[pos + i] as char;
        if !c.is_digit(16) {
            return 0;
        }
        result = (result << 4) + from_hex(c) as u32;
    }

    result
}

/// Encode a given character in UTF-8.
fn encode_utf8(chars: &mut Vec<u8>, pos: usize, c: u32) -> usize {
    if c <= 0x7f {
        chars[pos] = c as u8;
        return 1;
    }

    if c <= 0x7ff {
        chars[pos] = (c >> 6) as u8 | 0xc0;
        chars[pos + 1] = (c & 0x3f) as u8 | 0x80;
        return 2;
    }

    if c <= 0xffff {
        chars[pos] = (c >> 12) as u8 | 0xe0;
        chars[pos + 1] = ((c >> 6) & 0x3f) as u8 | 0x80;
        chars[pos + 2] = (c & 0x3f) as u8 | 0x80;
        return 3;
    }

    chars[pos] = (c >> 18) as u8 | 0xf0;
    chars[pos + 1] = ((c >> 12) & 0x3f) as u8 | 0x80;
    chars[pos + 2] = ((c >> 6) & 0x3f) as u8 | 0x80;
    chars[pos + 3] = (c & 0x3f) as u8 | 0x80;
    return 4;
}
