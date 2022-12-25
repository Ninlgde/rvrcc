use crate::ctype::TypeLink;
use crate::keywords::KW_TYPENAME;
use crate::Type;

/// token
#[derive(Clone)]
pub enum Token {
    Undefined,
    Ident {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
        // 行号
        line_no: usize,
    },
    // 操作符如： + -
    Punct {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
        // 行号
        line_no: usize,
    },
    Keyword {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
        // 行号
        line_no: usize,
    },
    // 数字
    Num {
        // 值
        val: i64,
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
        // 行号
        line_no: usize,
    },
    // 字符串
    Str {
        // 值
        val: Vec<u8>,
        // 类型
        type_: TypeLink,
        // 在解析的字符串内的位置
        offset: usize,
        // 行号
        line_no: usize,
    },
    // 文件终止符，即文件的最后
    Eof {
        offset: usize,
        // 行号
        line_no: usize,
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
            Self::Eof { offset, .. } => *offset,
            _ => 0,
        }
    }

    pub fn get_line_no(&self) -> usize {
        match self {
            Self::Ident { line_no, .. } => *line_no,
            Self::Punct { line_no, .. } => *line_no,
            Self::Keyword { line_no, .. } => *line_no,
            Self::Num { line_no, .. } => *line_no,
            Self::Str { line_no, .. } => *line_no,
            Self::Eof { line_no, .. } => *line_no,
            _ => 0,
        }
    }

    pub fn at_eof(&self) -> bool {
        matches!(self, Self::Eof { .. })
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident { .. })
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::Str { .. })
    }

    pub fn get_string(&self) -> (Vec<u8>, TypeLink) {
        match self {
            Self::Str { val, type_, .. } => return (val.to_vec(), type_.clone()),
            _ => (vec![], Type::new_char()),
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            Self::Ident { t_str, .. } => t_str.to_string(),
            Self::Punct { t_str, .. } => t_str.to_string(),
            Self::Keyword { t_str, .. } => t_str.to_string(),
            Self::Num { t_str, .. } => t_str.to_string(),
            _ => "".to_string(),
        }
    }

    pub fn equal(&self, s: &str) -> bool {
        match self {
            Token::Punct { t_str, .. } => t_str.eq(s),
            Token::Keyword { t_str, .. } => t_str.eq(s),
            Token::Ident { t_str, .. } => t_str.eq(s),
            Token::Num { t_str, .. } => t_str.eq(s),
            _ => false,
        }
    }

    pub fn is_typename(&self) -> bool {
        for name in KW_TYPENAME {
            if self.equal(name) {
                return true;
            }
        }
        false
    }
}
