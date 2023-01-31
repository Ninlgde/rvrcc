use crate::cmacro::HideSet;
use crate::ctype::TypeLink;
use crate::keywords::KW_TYPENAME;
use crate::{error_token, INPUT};
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
    // 预处理数值
    PpNum,
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
    /// Line number
    line_delta: isize,
    /// Filename
    file_name: String,
    /// 终结符在行首（begin of line）时为true
    at_bol: bool,
    /// 终结符前是否有空格
    has_space: bool,
    /// 值
    ival: i64,
    /// TK_NUM浮点值
    fval: f64,
    /// 类型
    typ: Option<TypeLink>,
    /// 字符串字面量
    chars: Vec<u8>,
    /// 数字
    hide_set: *mut HideSet,
    /// 宏展开前的原始终结符
    origin: Option<Token>,
}

impl TokenInner {
    /// copy from other token
    pub fn form(other: &Rc<RefCell<TokenInner>>) -> Self {
        let typ = if other.borrow().typ.is_some() {
            Some(other.borrow().typ.as_ref().unwrap().clone())
        } else {
            None
        };
        let origin = if other.borrow().origin.is_some() {
            Some(other.borrow().origin.as_ref().unwrap().clone())
        } else {
            None
        };
        TokenInner {
            kind: other.borrow().kind.clone(),
            name: other.borrow().name.clone(),
            offset: other.borrow().offset,
            line_no: other.borrow().line_no,
            line_delta: 0,
            file: Some(other.borrow().file.as_ref().unwrap().clone()),
            file_name: other.borrow().file_name.clone(),
            at_bol: other.borrow().at_bol,
            has_space: other.borrow().has_space,
            ival: other.borrow().ival,
            fval: other.borrow().fval,
            typ,
            chars: other.borrow().chars.clone(),
            hide_set: other.borrow().hide_set,
            origin,
        }
    }

    /// 构造undefined
    fn null() -> Self {
        TokenInner {
            kind: TokenKind::Undefined,
            name: "".to_string(),
            offset: 0,
            line_no: 0,
            line_delta: 0,
            file: Some(unsafe { INPUT.as_ref().unwrap().clone() }),
            file_name: "".to_string(),
            at_bol: false,
            has_space: false,
            ival: 0,
            fval: 0.0,
            typ: None,
            chars: vec![],
            hide_set: HideSet::head(),
            origin: None,
        }
    }

    /// 构造方法
    fn new(
        kind: TokenKind,
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
    ) -> Self {
        let mut token = Self::null();
        token.has_space = has_space;
        token.at_bol = at_bol;
        token.kind = kind;
        token.name = name;
        token.offset = offset;
        token.line_no = line_no;
        let current_file = unsafe { INPUT.as_ref().unwrap().clone() };
        token.file_name = current_file.borrow().display_name.clone();
        token.file = Some(current_file);
        token
    }

    /// 构造num
    fn new_num(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        ival: i64,
        fval: f64,
        typ: TypeLink,
    ) -> Self {
        let mut token = Self::new(TokenKind::Num, has_space, at_bol, name, offset, line_no);
        token.ival = ival;
        token.fval = fval;
        token.typ = Some(typ);
        token
    }

    /// 构造str
    fn new_str(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        chars: Vec<u8>,
        typ: TypeLink,
    ) -> Self {
        let mut token = Self::null();
        token.kind = TokenKind::Str;
        token.has_space = has_space;
        token.at_bol = at_bol;
        token.name = name;
        token.offset = offset;
        token.line_no = line_no;
        token.chars = chars;
        token.typ = Some(typ);
        token
    }

    /// 构造字符字面量
    fn new_char_literal(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        c: i64,
        typ: TypeLink,
    ) -> Self {
        let mut token = Self::null();
        token.kind = TokenKind::Num;
        token.has_space = has_space;
        token.at_bol = at_bol;
        token.offset = offset;
        token.line_no = line_no;
        token.ival = c;
        token.name = name; // name = '{c}'
        token.typ = Some(typ);
        token
    }

    /// 转化成keyword
    fn to_keyword(&mut self) {
        self.kind = TokenKind::Keyword
    }

    /// 转化成num
    fn to_number(&mut self, ival: i64, fval: f64, typ: TypeLink) {
        self.kind = TokenKind::Num;
        self.ival = ival;
        self.fval = fval;
        self.typ = Some(typ);
    }

    /// 获取文件编号
    fn get_file_no(&self) -> usize {
        self.file.as_ref().unwrap().borrow().no
    }

    /// 获取文件名
    fn get_file_name(&self) -> String {
        // self.file.as_ref().unwrap().borrow().name.to_string()
        self.file_name.to_string()
    }

    /// 获取字符串字面量
    fn get_string_literal(&self) -> String {
        // 最后添加了\0 所以在转换rust string的时候要忽略掉
        String::from_utf8_lossy(&self.chars[0..self.chars.len() - 1]).to_string()
    }

    /// 获取隐藏集
    /// 不带头指针
    fn get_hide_set(&self) -> *mut HideSet {
        unsafe { (*self.hide_set).next }
    }

    /// 遍历Tok之后的所有终结符，将隐藏集Hs都赋给每个终结符
    fn add_hide_set(&mut self, new_head: *mut HideSet) {
        self.hide_set = HideSet::union(self.get_hide_set(), new_head);
    }

    /// 判断是否处于隐藏集之中
    fn contain_hide_set(&self) -> bool {
        let head = self.get_hide_set();
        HideSet::contain(head, &self.name)
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
    /// copy from other token
    pub fn form(other: &Self) -> Self {
        let token = other.inner.clone();
        Token {
            inner: Rc::new(RefCell::new(TokenInner::form(&token))),
        }
    }

    /// 构造undefined
    pub fn null() -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::null()));
        Token { inner }
    }

    /// 构造方法
    pub fn new(
        kind: TokenKind,
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
    ) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new(
            kind, has_space, at_bol, name, offset, line_no,
        )));
        Token { inner }
    }

    /// 构造num
    pub fn new_num(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        ival: i64,
        fval: f64,
        typ: TypeLink,
    ) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new_num(
            has_space, at_bol, name, offset, line_no, ival, fval, typ,
        )));
        Token { inner }
    }

    /// 构造pp num
    pub fn new_ppnum(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
    ) -> Self {
        Token::new(TokenKind::PpNum, has_space, at_bol, name, offset, line_no)
    }

    /// 构造str
    pub fn new_str(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        val: Vec<u8>,
        typ: TypeLink,
    ) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new_str(
            has_space, at_bol, name, offset, line_no, val, typ,
        )));
        Token { inner }
    }

    /// 构造字符字面量
    pub fn new_char_literal(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
        c: i64,
        typ: TypeLink,
    ) -> Self {
        let inner = Rc::new(RefCell::new(TokenInner::new_char_literal(
            has_space, at_bol, name, offset, line_no, c, typ,
        )));
        Token { inner }
    }

    /// 构造标识符
    pub fn new_ident(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
    ) -> Self {
        Token::new(TokenKind::Ident, has_space, at_bol, name, offset, line_no)
    }

    /// 构造punct
    pub fn new_punct(
        has_space: bool,
        at_bol: bool,
        name: String,
        offset: usize,
        line_no: usize,
    ) -> Self {
        Token::new(TokenKind::Punct, has_space, at_bol, name, offset, line_no)
    }

    /// 构造eof
    pub fn new_eof(offset: usize, line_no: usize) -> Self {
        Token::new(TokenKind::Eof, false, true, "".to_string(), offset, line_no)
    }

    /// 转化成keyword
    pub fn to_keyword(&mut self) {
        let mut inner = self.inner.borrow_mut();
        inner.to_keyword();
    }

    /// 转化成num
    pub fn to_number(&mut self, ival: i64, fval: f64, typ: TypeLink) {
        let mut inner = self.inner.borrow_mut();
        inner.to_number(ival, fval, typ);
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

    /// 设置token在输入文件中的行号
    pub fn set_line_no(&mut self, ln: usize) {
        let mut inner = self.inner.borrow_mut();
        inner.line_no = ln;
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

    /// 是否是预处理数字
    pub fn is_ppnum(&self) -> bool {
        let inner = self.inner.borrow();
        inner.kind == TokenKind::PpNum
    }

    /// 是否行首是#号
    pub fn is_hash(&self) -> bool {
        let inner = self.inner.borrow();
        inner.at_bol && self.equal("#")
    }

    /// 是否保护空格
    pub fn has_space(&self) -> bool {
        let inner = self.inner.borrow();
        inner.has_space
    }

    /// 获取字符串
    pub fn get_string(&self) -> (Vec<u8>, TypeLink) {
        let inner = self.inner.borrow();
        (inner.chars.to_vec(), inner.typ.as_ref().unwrap().clone())
    }

    /// 设置字符串
    pub fn set_string(&mut self, chars: Vec<u8>, typ: TypeLink) {
        let mut inner = self.inner.borrow_mut();
        inner.chars = chars;
        inner.typ = Some(typ);
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

    /// 获取隐藏集
    pub fn get_hide_set(&self) -> *mut HideSet {
        let inner = self.inner.borrow();
        inner.get_hide_set()
    }

    /// 遍历Tok之后的所有终结符，将隐藏集Hs都赋给每个终结符
    pub fn add_hide_set(&mut self, new_head: *mut HideSet) {
        let mut inner = self.inner.borrow_mut();
        inner.add_hide_set(new_head);
    }

    /// 判断是否处于隐藏集之中
    pub fn contain_hide_set(&self) -> bool {
        let inner = self.inner.borrow();
        inner.contain_hide_set()
    }

    /// 设置 是否为行首 和 前面是否有空格
    pub fn set_bol_space(&mut self, bol: bool, space: bool) {
        let mut inner = self.inner.borrow_mut();
        inner.at_bol = bol;
        inner.has_space = space;
    }

    /// 获取宏展开前的原始终结符
    pub fn get_origin(&self) -> Option<Token> {
        let inner = self.inner.borrow();
        inner.origin.clone()
    }

    /// 设置宏展开前的原始终结符
    pub fn set_origin(&mut self, origin: Token) {
        let mut inner = self.inner.borrow_mut();
        inner.origin = Some(origin);
    }

    /// 设置文件名
    pub fn set_file_name(&mut self, name: String) {
        let mut inner = self.inner.borrow_mut();
        inner.file_name = name;
    }

    /// get line number
    pub fn get_line_delta(&self) -> isize {
        let inner = self.inner.borrow();
        inner.line_delta
    }

    /// set line number
    pub fn set_line_delta(&mut self, ld: isize) {
        let mut inner = self.inner.borrow_mut();
        inner.line_delta = ld;
    }
}

impl PartialEq<Self> for Token {
    /// 判断两个token的内部指针是否相等
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

/// file 指针
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
    /// For #line directive
    /// 显示名
    pub(crate) display_name: String,
    /// line number
    pub(crate) line_delta: isize,
}

impl File {
    pub fn new(name: String, no: usize, content: String) -> Self {
        File {
            display_name: name.to_string(),
            name,
            no,
            content,
            line_delta: 0,
        }
    }

    pub fn new_link(name: String, no: usize, content: String) -> FileLink {
        Rc::new(RefCell::new(File::new(name, no, content)))
    }

    /// 设置显示名
    pub fn set_display_name(&mut self, dn: &str) {
        self.display_name = dn.to_string();
    }

    /// 设置行号
    pub fn set_line_delta(&mut self, ld: isize) {
        self.line_delta = ld;
    }
}

/// token vector 的各种操作
pub trait TokenVecOps {
    /// 获取所有token的接口
    fn get_tokens(&self) -> &Vec<Token>;
    /// 获取当前游标的接口
    fn get_cursor(&self) -> usize;
    /// 游标向后移动的接口
    fn inc_cursor(&mut self, step: usize);

    /// 获取idx指向的token
    fn get_token(&self, mut idx: usize) -> &Token {
        let tokens = self.get_tokens();
        if idx >= tokens.len() {
            // 正常情况下 是不会超出的
            // 但是在预处理时可能会, 所以超出的一律已eof来作数
            idx = tokens.len() - 1;
        }
        &tokens[idx]
    }

    /// 获取当前游标和游标所指的token
    fn current(&self) -> (usize, &Token) {
        (self.get_cursor(), self.current_token())
    }

    /// 获取当前游标的token
    fn current_token(&self) -> &Token {
        let tokens = self.get_tokens();
        &tokens[self.get_cursor()]
    }

    /// 游标指向下一个
    fn next(&mut self) -> &mut Self {
        self.inc_cursor(1);
        self
    }

    /// 跳过某个名为`s`的token,如果不是则报错. 功效类似assert
    fn skip(&mut self, s: &str) {
        self.require(s);
        self.next();
    }

    /// 必须是名为`s`的token
    fn require(&self, s: &str) {
        let token = self.current_token();
        if !token.equal(s) {
            error_token!(token, "expect '{}'", s);
        }
    }

    /// 消费token,如果与`s`相等,则跳过并返回true,否则不跳过返回false
    fn consume(&mut self, s: &str) -> bool {
        let token = self.current_token();
        if token.equal(s) {
            self.next();
            return true;
        }
        return false;
    }
}
