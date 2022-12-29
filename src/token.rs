use crate::ctype::{Type, TypeLink};
use crate::keywords::KW_TYPENAME;

/// token 文法终结符
#[derive(Clone)]
pub enum Token {
    /// 未定义
    Undefined,
    /// id
    Ident {
        /// token 名
        t_str: String,
        /// 在解析的字符串内的位置
        offset: usize,
        /// 行号
        line_no: usize,
    },
    /// 操作符如： + -
    Punct {
        /// token 名
        t_str: String,
        /// 在解析的字符串内的位置
        offset: usize,
        /// 行号
        line_no: usize,
    },
    /// 关键字
    Keyword {
        /// token 名
        t_str: String,
        /// 在解析的字符串内的位置
        offset: usize,
        /// 行号
        line_no: usize,
    },
    /// 数字
    Num {
        /// 值
        val: i64,
        /// TK_NUM浮点值
        fval: f64,
        /// 类型
        typ: TypeLink,
        /// token 名
        t_str: String,
        /// 在解析的字符串内的位置
        offset: usize,
        /// 行号
        line_no: usize,
    },
    /// 字符串
    Str {
        /// 值
        val: Vec<u8>,
        /// 类型
        typ: TypeLink,
        /// 在解析的字符串内的位置
        offset: usize,
        /// 行号
        line_no: usize,
    },
    /// 文件终止符，即文件的最后
    Eof {
        offset: usize,
        /// 行号
        line_no: usize,
    },
}

impl Token {
    /// 获取token在输入文件中的offset
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

    /// 获取token在输入文件中的行号
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

    /// 是否是eof
    pub fn at_eof(&self) -> bool {
        matches!(self, Self::Eof { .. })
    }

    /// 是否是id
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident { .. })
    }

    /// 是否是字符串
    pub fn is_string(&self) -> bool {
        matches!(self, Self::Str { .. })
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Undefined { .. })
    }

    /// 获取字符串
    pub fn get_string(&self) -> (Vec<u8>, TypeLink) {
        match self {
            Self::Str { val, typ, .. } => return (val.to_vec(), typ.clone()),
            _ => (vec![], Type::new_char()),
        }
    }

    /// 获取终结符的name
    pub fn get_name(&self) -> String {
        match self {
            Self::Ident { t_str, .. } => t_str.to_string(),
            Self::Punct { t_str, .. } => t_str.to_string(),
            Self::Keyword { t_str, .. } => t_str.to_string(),
            Self::Num { t_str, .. } => t_str.to_string(),
            _ => "".to_string(),
        }
    }

    /// 判断终结符的name是相等
    pub fn equal(&self, s: &str) -> bool {
        match self {
            Token::Punct { t_str, .. } => t_str.eq(s),
            Token::Keyword { t_str, .. } => t_str.eq(s),
            Token::Ident { t_str, .. } => t_str.eq(s),
            Token::Num { t_str, .. } => t_str.eq(s),
            _ => false,
        }
    }

    /// 是否是KW_TYPENAME中定义的typename
    pub fn is_typename(&self) -> bool {
        for name in KW_TYPENAME {
            if self.equal(name) {
                return true;
            }
        }
        false
    }
}
