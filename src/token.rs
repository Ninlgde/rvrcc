use crate::ctype::{Type, TypeLink};
use crate::keywords::KW_TYPENAME;
use crate::INPUT;
use std::cell::RefCell;
use std::rc::Rc;

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

pub struct TokenInner {
    kind: TokenKind,
    /// token 名
    name: String,
    /// 在解析的字符串内的位置
    offset: usize,
    /// 行号
    line_no: usize,
    /// 源文件位置
    file: Option<FileLink>,
    /// 终结符在行首（begin of line）时为true
    at_bol: bool,
    /// 值
    ival: i64,
    /// TK_NUM浮点值
    fval: f64,
    /// 类型
    typ: Option<TypeLink>,
    /// 字符串字面量
    chars: Vec<u8>,
}

impl TokenInner {
    /// 构造undefined
    fn null() -> Self {
        TokenInner {
            kind: TokenKind::Undefined,
            name: "".to_string(),
            offset: 0,
            line_no: 0,
            file: Some(unsafe { INPUT.as_ref().unwrap().clone() }),
            at_bol: false,
            ival: 0,
            fval: 0.0,
            typ: None,
            chars: vec![],
        }
    }

    /// 构造方法
    fn new(kind: TokenKind, at_bol: bool, name: String, offset: usize, line_no: usize) -> Self {
        let mut token = Self::null();
        token.at_bol = at_bol;
        token.kind = kind;
        token.name = name;
        token.offset = offset;
        token.line_no = line_no;
        token.file = Some(unsafe { INPUT.as_ref().unwrap().clone() });
        token
    }

    /// 构造num
    fn new_num(
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        ival: i64,
        fval: f64,
        typ: TypeLink,
    ) -> Self {
        let mut token = Self::new(TokenKind::Num, at_bol, name, offset, line_no);
        token.ival = ival;
        token.fval = fval;
        token.typ = Some(typ);
        token
    }

    /// 构造str
    fn new_str(at_bol: bool, offset: usize, line_no: usize, chars: Vec<u8>, typ: TypeLink) -> Self {
        let mut token = Self::null();
        token.kind = TokenKind::Str;
        token.at_bol = at_bol;
        token.offset = offset;
        token.line_no = line_no;
        token.chars = chars;
        token.typ = Some(typ);
        token
    }

    /// 构造字符字面量
    fn new_char_literal(at_bol: bool, offset: usize, line_no: usize, c: char) -> Self {
        let mut token = Self::null();
        token.kind = TokenKind::Num;
        token.at_bol = at_bol;
        token.offset = offset;
        token.line_no = line_no;
        token.ival = c as i64;
        token.typ = Some(Type::new_int());
        token
    }

    /// 转化成keyword
    fn to_keyword(&mut self) {
        self.kind = TokenKind::Keyword
    }

    /// 获取文件编号
    fn get_file_no(&self) -> usize {
        self.file.as_ref().unwrap().borrow().no
    }

    /// 获取文件名
    fn get_file_name(&self) -> String {
        self.file.as_ref().unwrap().borrow().name.to_string()
    }

    /// 获取字符串字面量
    fn get_string_literal(&self) -> String {
        // 最后添加了\0 所以在转换rust string的时候要忽略掉
        String::from_utf8_lossy(&self.chars[0..self.chars.len() - 1]).to_string()
    }
}

/// token 文法终结符
pub struct Token {
    /// 内部实现,使用rc refcell来优化内存
    inner: Rc<RefCell<TokenInner>>,
}

/// 对token手动实现clone
impl Clone for Token {
    fn clone(&self) -> Self {
        Token {
            /// 就是inner的clone,这样来优化性能
            inner: self.inner.clone(),
        }
    }
}

impl Token {
    /// 构造undefined
    pub fn null() -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::null()));
        Token { inner }
    }

    /// 构造方法
    pub fn new(kind: TokenKind, at_bol: bool, name: String, offset: usize, line_no: usize) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new(
            kind, at_bol, name, offset, line_no,
        )));
        Token { inner }
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
        let inner = Rc::new(RefCell::new(TokenInner::new_num(
            at_bol, name, offset, line_no, ival, fval, typ,
        )));
        Token { inner }
    }

    /// 构造str
    pub fn new_str(
        at_bol: bool,
        offset: usize,
        line_no: usize,
        val: Vec<u8>,
        typ: TypeLink,
    ) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new_str(
            at_bol, offset, line_no, val, typ,
        )));
        Token { inner }
    }

    /// 构造字符字面量
    pub fn new_char_literal(at_bol: bool, offset: usize, line_no: usize, c: char) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new_char_literal(
            at_bol, offset, line_no, c,
        )));
        Token { inner }
    }

    /// 构造标识符
    pub fn new_ident(at_bol: bool, name: String, offset: usize, line_no: usize) -> Self {
        Token::new(TokenKind::Ident, at_bol, name, offset, line_no)
    }

    /// 构造punct
    pub fn new_punct(at_bol: bool, name: String, offset: usize, line_no: usize) -> Self {
        Token::new(TokenKind::Punct, at_bol, name, offset, line_no)
    }

    /// 构造eof
    pub fn new_eof(at_bol: bool, offset: usize, line_no: usize) -> Self {
        Token::new(TokenKind::Eof, at_bol, "".to_string(), offset, line_no)
    }

    /// 转化成keyword
    pub fn to_keyword(&mut self) {
        let mut inner = self.inner.borrow_mut();
        inner.to_keyword();
    }

    /// 获取文件
    pub fn get_file(&self) -> Option<FileLink> {
        let inner = self.inner.borrow();
        Some(inner.file.as_ref().unwrap().clone())
    }

    /// 获取文件编号
    pub fn get_file_no(&self) -> usize {
        let inner = self.inner.borrow();
        inner.get_file_no()
    }

    /// 获取文件名
    pub fn get_file_name(&self) -> String {
        let inner = self.inner.borrow();
        inner.get_file_name()
    }

    /// 获取token在输入文件中的offset
    pub fn get_offset(&self) -> usize {
        let inner = self.inner.borrow();
        inner.offset
    }

    /// 获取token在输入文件中的行号
    pub fn get_line_no(&self) -> usize {
        let inner = self.inner.borrow();
        inner.line_no
    }

    /// 是否是eof
    pub fn at_eof(&self) -> bool {
        let inner = self.inner.borrow();
        inner.kind == TokenKind::Eof
    }

    /// 是否是一行的开头
    pub fn at_bol(&self) -> bool {
        let inner = self.inner.borrow();
        inner.at_bol
    }

    /// 是否是id
    pub fn is_ident(&self) -> bool {
        let inner = self.inner.borrow();
        inner.kind == TokenKind::Ident
    }

    /// 是否是字符串
    pub fn is_string(&self) -> bool {
        let inner = self.inner.borrow();
        inner.kind == TokenKind::Str
    }

    /// 是否是空token
    pub fn is_null(&self) -> bool {
        let inner = self.inner.borrow();
        inner.kind == TokenKind::Undefined
    }

    /// 是否是数字
    pub fn is_num(&self) -> bool {
        let inner = self.inner.borrow();
        inner.kind == TokenKind::Num
    }

    /// 是否行首是#号
    pub fn is_hash(&self) -> bool {
        let inner = self.inner.borrow();
        inner.at_bol && self.equal("#")
    }

    /// 获取字符串
    pub fn get_string(&self) -> (Vec<u8>, TypeLink) {
        let inner = self.inner.borrow();
        (inner.chars.to_vec(), inner.typ.as_ref().unwrap().clone())
    }

    /// 获取字符串字面量
    pub fn get_string_literal(&self) -> String {
        let inner = self.inner.borrow();
        // 最后添加了\0 所以在转换rust string的时候要忽略掉
        inner.get_string_literal()
        // String::from_utf8_lossy(&inner.val[0..inner.val.len() - 1]).to_string()
    }

    /// 获取number
    pub fn get_num(&self) -> (i64, f64, TypeLink) {
        let inner = self.inner.borrow();
        (inner.ival, inner.fval, inner.typ.as_ref().unwrap().clone())
    }

    /// 获取终结符的name
    pub fn get_name(&self) -> String {
        if self.is_string() {
            // 字面量 转换转义字符
            return format!("{:?}", self.get_string_literal());
        }
        let inner = self.inner.borrow();
        inner.name.to_string()
    }

    /// 判断终结符的name是相等
    pub fn equal(&self, s: &str) -> bool {
        let inner = self.inner.borrow();
        inner.name.eq(s)
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

pub type FileLink = Rc<RefCell<File>>;

/// 源文件
#[derive(Clone)]
pub struct File {
    /// 文件名
    pub(crate) name: String,
    /// 文件编号，从1开始
    pub(crate) no: usize,
    /// 文件内容
    pub(crate) content: String,
}

impl File {
    pub fn new(name: String, no: usize, content: String) -> Self {
        File { name, no, content }
    }

    pub fn new_link(name: String, no: usize, content: String) -> FileLink {
        Rc::new(RefCell::new(File::new(name, no, content)))
    }
}
