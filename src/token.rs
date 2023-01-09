use crate::ctype::{Type, TypeLink};
use crate::keywords::KW_TYPENAME;

// pub enum Token {
//     /// 未定义
//     Undefined,
//     /// id
//     Ident {
//         /// token 名
//         t_str: String,
//         /// 在解析的字符串内的位置
//         offset: usize,
//         /// 行号
//         line_no: usize,
//     },
//     /// 操作符如： + -
//     Punct {
//         /// token 名
//         t_str: String,
//         /// 在解析的字符串内的位置
//         offset: usize,
//         /// 行号
//         line_no: usize,
//     },
//     /// 关键字
//     Keyword {
//         /// token 名
//         t_str: String,
//         /// 在解析的字符串内的位置
//         offset: usize,
//         /// 行号
//         line_no: usize,
//     },
//     /// 数字
//     Num {
//         /// 值
//         val: i64,
//         /// TK_NUM浮点值
//         fval: f64,
//         /// 类型
//         typ: TypeLink,
//         /// token 名
//         t_str: String,
//         /// 在解析的字符串内的位置
//         offset: usize,
//         /// 行号
//         line_no: usize,
//     },
//     /// 字符串
//     Str {
//         /// 值
//         val: Vec<u8>,
//         /// 类型
//         typ: TypeLink,
//         /// 在解析的字符串内的位置
//         offset: usize,
//         /// 行号
//         line_no: usize,
//     },
//     /// 文件终止符，即文件的最后
//     Eof {
//         offset: usize,
//         /// 行号
//         line_no: usize,
//     },
// }
//
// impl Token {
//     /// 获取token在输入文件中的offset
//     pub fn get_offset(&self) -> usize {
//         match self {
//             Self::Ident { offset, .. } => *offset,
//             Self::Punct { offset, .. } => *offset,
//             Self::Keyword { offset, .. } => *offset,
//             Self::Num { offset, .. } => *offset,
//             Self::Str { offset, .. } => *offset,
//             Self::Eof { offset, .. } => *offset,
//             _ => 0,
//         }
//     }
//
//     /// 获取token在输入文件中的行号
//     pub fn get_line_no(&self) -> usize {
//         match self {
//             Self::Ident { line_no, .. } => *line_no,
//             Self::Punct { line_no, .. } => *line_no,
//             Self::Keyword { line_no, .. } => *line_no,
//             Self::Num { line_no, .. } => *line_no,
//             Self::Str { line_no, .. } => *line_no,
//             Self::Eof { line_no, .. } => *line_no,
//             _ => 0,
//         }
//     }
//
//     /// 是否是eof
//     pub fn at_eof(&self) -> bool {
//         matches!(self, Self::Eof { .. })
//     }
//
//     /// 是否是id
//     pub fn is_ident(&self) -> bool {
//         matches!(self, Self::Ident { .. })
//     }
//
//     /// 是否是字符串
//     pub fn is_string(&self) -> bool {
//         matches!(self, Self::Str { .. })
//     }
//
//     pub fn is_null(&self) -> bool {
//         matches!(self, Self::Undefined { .. })
//     }
//
//     /// 获取字符串
//     pub fn get_string(&self) -> (Vec<u8>, TypeLink) {
//         match self {
//             Self::Str { val, typ, .. } => return (val.to_vec(), typ.clone()),
//             _ => (vec![], Type::new_char()),
//         }
//     }
//
//     /// 获取终结符的name
//     pub fn get_name(&self) -> String {
//         match self {
//             Self::Ident { t_str, .. } => t_str.to_string(),
//             Self::Punct { t_str, .. } => t_str.to_string(),
//             Self::Keyword { t_str, .. } => t_str.to_string(),
//             Self::Num { t_str, .. } => t_str.to_string(),
//             _ => "".to_string(),
//         }
//     }
//
//     /// 判断终结符的name是相等
//     pub fn equal(&self, s: &str) -> bool {
//         match self {
//             Token::Punct { t_str, .. } => t_str.eq(s),
//             Token::Keyword { t_str, .. } => t_str.eq(s),
//             Token::Ident { t_str, .. } => t_str.eq(s),
//             Token::Num { t_str, .. } => t_str.eq(s),
//             _ => false,
//         }
//     }
//
//     /// 是否是KW_TYPENAME中定义的typename
//     pub fn is_typename(&self) -> bool {
//         for name in KW_TYPENAME {
//             if self.equal(name) {
//                 return true;
//             }
//         }
//         false
//     }
// }

/// c类型的种类
#[derive(Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// 未定义
    Undefined,
    /// id
    Ident,
    /// 操作符如： + -
    Punct,
    /// 关键字
    Keyword,
    /// 数字
    Num,
    /// 字符串
    Str,
    /// 文件终止符，即文件的最后
    Eof,
}

/// token 文法终结符
#[derive(Clone)]
pub struct Token {
    pub(crate) kind: TokenKind,
    /// token 名
    name: String,
    /// 在解析的字符串内的位置
    offset: usize,
    /// 行号
    line_no: usize,
    /// 终结符在行首（begin of line）时为true
    pub(crate) at_bol: bool,
    /// 值
    pub(crate) ival: i64,
    /// TK_NUM浮点值
    pub(crate) fval: f64,
    /// 类型
    pub(crate) typ: Option<TypeLink>,
    /// 值
    pub(crate) val: Vec<u8>,
}

impl Token {
    /// 构造undefined
    pub fn null() -> Self {
        Token {
            kind: TokenKind::Undefined,
            name: "".to_string(),
            offset: 0,
            line_no: 0,
            at_bol: false,
            ival: 0,
            fval: 0.0,
            typ: None,
            val: vec![],
        }
    }

    /// 构造方法
    pub fn new(at_bol: bool, kind: TokenKind, name: String, offset: usize, line_no: usize) -> Self {
        let mut token = Token::null();
        token.at_bol = at_bol;
        token.kind = kind;
        token.name = name;
        token.offset = offset;
        token.line_no = line_no;
        token
    }

    /// 构造num
    pub fn new_num(
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        ival: i64,
        fval: f64,
        typ: TypeLink,
    ) -> Self {
        let mut token = Token::new(at_bol, TokenKind::Num, name, offset, line_no);
        token.ival = ival;
        token.fval = fval;
        token.typ = Some(typ);
        token
    }

    /// 构造str
    pub fn new_str(
        at_bol: bool,
        offset: usize,
        line_no: usize,
        val: Vec<u8>,
        typ: TypeLink,
    ) -> Self {
        let mut token = Token::null();
        token.kind = TokenKind::Str;
        token.at_bol = at_bol;
        token.offset = offset;
        token.line_no = line_no;
        token.val = val;
        token.typ = Some(typ);
        token
    }

    /// 构造字符字面量
    pub fn new_char_literal(at_bol: bool, offset: usize, line_no: usize, c: char) -> Self {
        let mut token = Token::null();
        token.kind = TokenKind::Num;
        token.at_bol = at_bol;
        token.offset = offset;
        token.line_no = line_no;
        token.ival = c as i64;
        token.typ = Some(Type::new_int());
        token
    }

    /// 构造标识符
    pub fn new_ident(at_bol: bool, name: String, offset: usize, line_no: usize) -> Self {
        Token::new(at_bol, TokenKind::Ident, name, offset, line_no)
    }

    /// 构造punct
    pub fn new_punct(at_bol: bool, name: String, offset: usize, line_no: usize) -> Self {
        Token::new(at_bol, TokenKind::Punct, name, offset, line_no)
    }

    /// 构造eof
    pub fn new_eof(at_bol: bool, offset: usize, line_no: usize) -> Self {
        Token::new(at_bol, TokenKind::Eof, "".to_string(), offset, line_no)
    }

    /// 获取token在输入文件中的offset
    pub fn get_offset(&self) -> usize {
        self.offset
    }

    /// 获取token在输入文件中的行号
    pub fn get_line_no(&self) -> usize {
        self.line_no
    }

    /// 是否是eof
    pub fn at_eof(&self) -> bool {
        self.kind == TokenKind::Eof
    }

    /// 是否是id
    pub fn is_ident(&self) -> bool {
        self.kind == TokenKind::Ident
    }

    /// 是否是字符串
    pub fn is_string(&self) -> bool {
        self.kind == TokenKind::Str
    }

    /// 是否是空token
    pub fn is_null(&self) -> bool {
        self.kind == TokenKind::Undefined
    }

    /// 是否是数字
    pub fn is_num(&self) -> bool {
        self.kind == TokenKind::Num
    }

    /// 是否行首是#号
    pub fn is_hash(&self) -> bool {
        self.at_bol && self.equal("#")
    }

    /// 获取字符串
    pub fn get_string(&self) -> (Vec<u8>, TypeLink) {
        (self.val.to_vec(), self.typ.as_ref().unwrap().clone())
    }

    pub fn get_num(&self) -> (i64, f64, TypeLink) {
        (self.ival, self.fval, self.typ.as_ref().unwrap().clone())
    }

    /// 获取终结符的name
    pub fn get_name(&self) -> &String {
        &self.name
    }

    /// 判断终结符的name是相等
    pub fn equal(&self, s: &str) -> bool {
        self.name.eq(s)
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
