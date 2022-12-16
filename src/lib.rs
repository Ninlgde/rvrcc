extern crate core;

use core::fmt;
use std::cell::RefCell;
use std::rc::Rc;

mod tokenize;
mod parse;
mod codegen;
mod keywords;
mod ctype;

pub use tokenize::tokenize;
pub use parse::parse;
pub use codegen::codegen;
use crate::ctype::Type;

pub static mut INPUT: String = String::new();


// 字符解析出错，并退出程序
fn print_with_error(offset: usize, args: fmt::Arguments) {
    println!("{}", unsafe { &INPUT });
    print!("{:1$}^", "", offset);
    print!(" {}\n", args);
    panic!("error at offset: {}", offset);
}

// Tok解析出错，并退出程序
fn print_with_token_error(token: &Token, args: fmt::Arguments) {
    print_with_error(token.get_offset(), args);
}


/// error at offset
#[macro_export]
macro_rules! error_at {
    ($offset:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::print_with_error($offset, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}


/// error token
#[macro_export]
macro_rules! error_token {
    ($token:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::print_with_token_error($token, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}


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
    // 文件终止符，即文件的最后
    Eof {
        offset: usize,
    },
}

impl Token {
    fn get_offset(&self) -> usize {
        match self {
            Self::Ident { t_str: _t_str, offset } => *offset,
            Self::Punct { t_str: _t_str, offset } => *offset,
            Self::Keyword { t_str: _t_str, offset } => *offset,
            Self::Num { val: _val, t_str: _t_str, offset } => *offset,
            Self::Eof { offset } => *offset,
        }
    }
    #[allow(dead_code)]
    fn at_eof(&self) -> bool {
        match self {
            Self::Eof { offset: _offset } => true,
            _ => false
        }
    }

    fn equal(&self, s: &str) -> bool {
        match self {
            Token::Punct { t_str, offset: _offset } => t_str.eq(s),
            Token::Keyword { t_str, offset: _offset } => t_str.eq(s),
            Token::Num { val: _val, t_str, offset: _offset } => t_str.eq(s),
            _ => false
        }
    }
}

//
// 生成AST（抽象语法树），语法解析
//

/// 本地变量
#[derive(Clone)]
pub struct Var {
    // 变量名
    name: String,
    // fp的偏移量
    offset: isize,
    // 类型
    type_: Box<Type>,
}

/// 函数
#[allow(dead_code)]
pub struct Function {
    // 函数名
    name: String,
    // 方法参数
    params: Vec<Rc<RefCell<Var>>>,
    // 函数体
    body: Node,
    // 本地变量
    locals: Vec<Rc<RefCell<Var>>>,
    // 栈大小
    stack_size: isize,
}

// AST的节点种类
#[derive(Clone)]
pub enum Node {
    // +
    Add {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // -
    Sub {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // *
    Mul {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // /
    Div {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 负号-
    Neg {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 一元单节点
        unary: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // ==
    Eq {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // !=
    Ne {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // <
    Lt {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // <=
    Le {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 赋值
    Assign {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 取地址 &
    Addr {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 一元单节点
        unary: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 解引用 *
    DeRef {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 一元单节点
        unary: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 返回
    Return {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 一元单节点
        unary: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // "if"，条件判断
    If {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 条件内的表达式
        cond: Option<Box<Node>>,
        // 符合条件后的语句
        then: Option<Box<Node>>,
        // 不符合条件后的语句
        els: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // "for" 或 "while"，循环
    For {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 初始化语句
        init: Option<Box<Node>>,
        // 递增语句
        inc: Option<Box<Node>>,
        // 条件内的表达式
        cond: Option<Box<Node>>,
        // 符合条件后的语句
        then: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // { ... }，代码块
    Block {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 代码块
        body: Vec<Node>,
        // 类型
        type_: Option<Box<Type>>,
    },
    FuncCall {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 变量名
        func_name: String,
        // 入参
        args: Vec<Node>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 表达式语句
    ExprStmt {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 一元单节点
        unary: Option<Box<Node>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 变量
    Var {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 存储ND_VAR的字符串
        var: Option<Rc<RefCell<Var>>>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 数字
    Num {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 存储ND_NUM种类的值
        val: i32,
        // 类型
        type_: Option<Box<Type>>,
    },
}

impl Node {
    pub fn get_token(&self) -> &Token {
        match self {
            Node::Add { token, .. } => token,
            Node::Sub { token, .. } => token,
            Node::Mul { token, .. } => token,
            Node::Div { token, .. } => token,
            Node::Neg { token, .. } => token,
            Node::Eq { token, .. } => token,
            Node::Ne { token, .. } => token,
            Node::Lt { token, .. } => token,
            Node::Le { token, .. } => token,
            Node::Assign { token, .. } => token,
            Node::Addr { token, .. } => token,
            Node::DeRef { token, .. } => token,
            Node::Return { token, .. } => token,
            Node::If { token, .. } => token,
            Node::For { token, .. } => token,
            Node::Block { token, .. } => token,
            Node::FuncCall { token, .. } => token,
            Node::ExprStmt { token, .. } => token,
            Node::Var { token, .. } => token,
            Node::Num { token, .. } => token,
        }
    }

    pub fn get_type(&self) -> &Option<Box<Type>> {
        match self {
            Node::Add { type_, .. } => type_,
            Node::Sub { type_, .. } => type_,
            Node::Mul { type_, .. } => type_,
            Node::Div { type_, .. } => type_,
            Node::Neg { type_, .. } => type_,
            Node::Eq { type_, .. } => type_,
            Node::Ne { type_, .. } => type_,
            Node::Lt { type_, .. } => type_,
            Node::Le { type_, .. } => type_,
            Node::Assign { type_, .. } => type_,
            Node::Addr { type_, .. } => type_,
            Node::DeRef { type_, .. } => type_,
            Node::Return { type_, .. } => type_,
            Node::If { type_, .. } => type_,
            Node::For { type_, .. } => type_,
            Node::Block { type_, .. } => type_,
            Node::FuncCall { type_, .. } => type_,
            Node::ExprStmt { type_, .. } => type_,
            Node::Var { type_, .. } => type_,
            Node::Num { type_, .. } => type_,
        }
    }
}