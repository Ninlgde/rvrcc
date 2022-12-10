extern crate core;

use core::fmt;

mod tokenize;
mod parse;
mod codegen;
mod keywords;

pub use tokenize::tokenize;
pub use parse::parse;
pub use codegen::codegen;

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
#[derive(Eq, PartialEq)]
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
}

/// 函数
#[allow(dead_code)]
pub struct Function {
    // 函数体
    body: Node,
    // 本地变量
    locals: Vec<Var>,
    // 栈大小
    stack_size: isize,
}

// AST的节点种类
pub enum Node {
    // +
    Add {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // -
    Sub {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // *
    Mul {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // /
    Div {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // 负号-
    Neg {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
    },
    // ==
    Eq {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // !=
    Ne {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // <
    Lt {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // <=
    Le {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // 赋值
    Assign {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
        // 右部，right-hand side
        rhs: Option<Box<Node>>,
    },
    // 返回
    Return {
        // 左部，left-hand side
        lhs: Option<Box<Node>>,
    },
    // "if"，条件判断
    If {
        // 条件内的表达式
        cond: Option<Box<Node>>,
        // 符合条件后的语句
        then: Option<Box<Node>>,
        // 不符合条件后的语句
        els: Option<Box<Node>>,
    },
    // "for" 或 "while"，循环
    For {
        // 初始化语句
        init: Option<Box<Node>>,
        // 递增语句
        inc: Option<Box<Node>>,
        // 条件内的表达式
        cond: Option<Box<Node>>,
        // 符合条件后的语句
        then: Option<Box<Node>>,
    },
    // { ... }，代码块
    Block {
        // 代码块
        body: Vec<Node>,
    },
    // 表达式语句
    ExprStmt {
        // 初始化语句
        lhs: Option<Box<Node>>,
    },
    // 变量
    Var {
        // 存储ND_VAR的字符串
        var: Option<Box<Var>>,
    },
    // 数字
    Num {
        // 存储ND_NUM种类的值
        val: i32,
    },
}