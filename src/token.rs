use crate::Type;

/// token
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Ident {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 操作符如： + -
    Punct {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    Keyword {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 数字
    Num {
        // 值
        val: i32,
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 字符串
    Str {
        // 值
        val: Vec<u8>,
        // 类型
        type_: Box<Type>,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 文件终止符，即文件的最后
    Eof {
        offset: usize,
    },
}

impl Token {
    pub fn get_offset(&self) -> usize {
        match self {
            Self::Ident { offset, .. } => *offset,
            Self::Punct { offset, .. } => *offset,
            Self::Keyword { offset, .. } => *offset,
            Self::Num { offset, .. } => *offset,
            Self::Str { offset, .. } => *offset,
            Self::Eof { offset } => *offset,
        }
    }
    pub fn at_eof(&self) -> bool {
        match self {
            Self::Eof { offset: _offset } => true,
            _ => false
        }
    }

    pub fn equal(&self, s: &str) -> bool {
        match self {
            Token::Punct { t_str, .. } => t_str.eq(s),
            Token::Keyword { t_str, .. } => t_str.eq(s),
            Token::Num { val: _val, t_str, .. } => t_str.eq(s),
            _ => false
        }
    }
}
