//! unicode

use crate::token::Token;
use crate::{error_at, error_token, from_hex, read_char, starts_with};

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
pub fn encode_utf8(chars: &mut Vec<u8>, pos: usize, c: u32) -> usize {
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

/// Read a UTF-8-encoded Unicode code point from a source file.
/// We assume that source files are always in UTF-8.
///
/// UTF-8 is a variable-width encoding in which one code point is
/// encoded in one to four bytes. One byte UTF-8 code points are
/// identical to ASCII. Non-ASCII characters are encoded using more
/// than one byte.
pub fn decode_utf8(chars: &Vec<u8>, pos: &mut usize) -> u32 {
    let c = read_char(chars, *pos) as u32;
    if c < 128 {
        *pos += 1;
        return c;
    }
    let start = *pos;
    let mut len = 0;
    let mut reuslt = 0u32;

    if c >= 0xf0 {
        len = 4;
        reuslt = c & 0x7;
    } else if c >= 0xe0 {
        len = 3;
        reuslt = c & 0xf
    } else if c >= 0xc0 {
        len = 2;
        reuslt = c & 0x1f;
    } else {
        error_at!(0, start, "invalid UTF-8 sequence");
    }

    for i in 1..len {
        let c = chars[*pos + i] as u32;
        if (c >> 6) != 0x2 {
            error_at!(0, start, "invalid UTF-8 sequence");
        }
        reuslt = (reuslt << 6) | (c & 0x3f)
    }

    *pos += len;
    reuslt
}

/// 判断是否在区间内
fn is_range(range: Vec<u32>, c: u32) -> bool {
    for i in (0..range.len() - 1).step_by(2) {
        if range[i] <= c && c <= range[i + 1] {
            return true;
        }
    }
    false
}

/// [https://www.sigbus.info/n1570#D] C11 allows not only ASCII but
/// some multibyte characters in certan Unicode ranges to be used in an
/// identifier.
///
/// This function returns true if a given character is acceptable as
/// the first character of an identifier.
///
/// For example, ¾ (U+00BE) is a valid identifier because characters in
/// 0x00BE-0x00C0 are allowed, while neither ⟘ (U+27D8) nor '　'
/// (U+3000, full-width space) are allowed because they are out of range.
pub fn is_ident1(c: u32) -> bool {
    let range = vec![
        0x5F, 0x5F, 0x61, 0x7A, 0x41, 0x5A, 0x24, 0x24, 0x00A8, 0x00A8, 0x00AA, 0x00AA, 0x00AD,
        0x00AD, 0x00AF, 0x00AF, 0x00B2, 0x00B5, 0x00B7, 0x00BA, 0x00BC, 0x00BE, 0x00C0, 0x00D6,
        0x00D8, 0x00F6, 0x00F8, 0x00FF, 0x0100, 0x02FF, 0x0370, 0x167F, 0x1681, 0x180D, 0x180F,
        0x1DBF, 0x1E00, 0x1FFF, 0x200B, 0x200D, 0x202A, 0x202E, 0x203F, 0x2040, 0x2054, 0x2054,
        0x2060, 0x206F, 0x2070, 0x20CF, 0x2100, 0x218F, 0x2460, 0x24FF, 0x2776, 0x2793, 0x2C00,
        0x2DFF, 0x2E80, 0x2FFF, 0x3004, 0x3007, 0x3021, 0x302F, 0x3031, 0x303F, 0x3040, 0xD7FF,
        0xF900, 0xFD3D, 0xFD40, 0xFDCF, 0xFDF0, 0xFE1F, 0xFE30, 0xFE44, 0xFE47, 0xFFFD, 0x10000,
        0x1FFFD, 0x20000, 0x2FFFD, 0x30000, 0x3FFFD, 0x40000, 0x4FFFD, 0x50000, 0x5FFFD, 0x60000,
        0x6FFFD, 0x70000, 0x7FFFD, 0x80000, 0x8FFFD, 0x90000, 0x9FFFD, 0xA0000, 0xAFFFD, 0xB0000,
        0xBFFFD, 0xC0000, 0xCFFFD, 0xD0000, 0xDFFFD, 0xE0000, 0xEFFFD,
    ];

    is_range(range, c)
}

/// Returns true if a given character is acceptable as a non-first
/// character of an identifier.
pub fn is_ident2(c: u32) -> bool {
    let range = vec![
        0x30, 0x39, 0x24, 0x24, 0x0300, 0x036F, 0x1DC0, 0x1DFF, 0x20D0, 0x20FF, 0xFE20, 0xFE2F,
    ];

    is_ident1(c) || is_range(range, c)
}

/// 字符串类型
#[derive(Clone, PartialEq, Eq)]
pub enum StringKind {
    /// 普通字符串
    None,
    /// utf-8
    Utf8,
    /// utf-16
    Utf16,
    /// utf-32
    Utf32,
    /// 宽字符串
    Wide,
}

/// 获取字符串的类型
pub fn get_string_kind(token: &Token) -> StringKind {
    let str = token.get_name();
    if str.starts_with("\"") {
        return StringKind::None;
    }
    if str.starts_with("u8") {
        return StringKind::Utf8;
    }
    if str.starts_with("u") {
        return StringKind::Utf16;
    }
    if str.starts_with("U") {
        return StringKind::Utf32;
    }
    if str.starts_with("L") {
        return StringKind::Wide;
    }
    error_token!(token, "invalid string");
    panic!()
}
