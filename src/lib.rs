extern crate core;

use core::fmt;

mod tokenize;
mod parse;
mod codegen;

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
    TkIdent {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 操作符如： + -
    TKPunct {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 数字
    TKNum {
        // 值
        val: i32,
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 文件终止符，即文件的最后
    TKEof {
        offset: usize,
    },
}

impl Token {
    fn get_offset(&self) -> usize {
        match self {
            Self::TkIdent { t_str: _t_str, offset } => *offset,
            Self::TKPunct { t_str: _t_str, offset } => *offset,
            Self::TKNum { val: _val, t_str: _t_str, offset } => *offset,
            Self::TKEof { offset } => *offset,
        }
    }

    fn at_eof(&self) -> bool {
        match self {
            Self::TKEof { offset: _offset } => true,
            _ => false
        }
    }

    fn equal(&self, s: &str) -> bool {
        match self {
            Token::TKPunct { t_str, offset: _offset } => t_str.eq(s),
            Token::TKNum { val: _val, t_str, offset: _offset } => t_str.eq(s),
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
    body: Vec<Node>,
    // 本地变量
    locals: Vec<Var>,
    // 栈大小
    stack_size: isize,
}

// AST的节点种类
#[derive(Eq, PartialEq)]
pub enum NodeKind {
    // +
    NdAdd,
    // -
    NdSub,
    // *
    NdMul,
    // /
    NdDiv,
    // 负号-
    NdNeg,
    // ==
    NdEq,
    // !=
    NdNe,
    // <
    NdLt,
    // <=
    NdLe,
    // 赋值
    NdAssign,
    // 表达式语句
    NdExprStmt,
    // 变量
    NdVar,
    // 数字
    NdNum,
}

/// AST中二叉树节点
pub struct Node {
    // 节点种类
    kind: NodeKind,
    // 左部，left-hand side
    lhs: Option<Box<Node>>,
    // 右部，right-hand side
    rhs: Option<Box<Node>>,
    // 存储ND_VAR的字符串
    var: Option<Box<Var>>,
    // 存储ND_NUM种类的值
    val: i32,
}

impl Node {
    fn new(kind: NodeKind) -> Self {
        Node {
            kind,
            lhs: None,
            rhs: None,
            var: None,
            val: 0,
        }
    }

    fn new_binary(kind: NodeKind, lhs: Node, rhs: Node) -> Self {
        Node {
            kind,
            lhs: Some(Box::new(lhs)),
            rhs: Some(Box::new(rhs)),
            var: None,
            val: 0,
        }
    }

    fn new_unary(kind: NodeKind, expr: Node) -> Self {
        let mut node = Node::new(kind);
        node.lhs = Some(Box::new(expr));
        node
    }

    fn new_num(val: i32) -> Self {
        let mut node = Node::new(NodeKind::NdNum);
        node.val = val;
        node
    }

    fn new_var(var: Var) -> Self {
        let mut node = Node::new(NodeKind::NdVar);
        node.var = Some(Box::new(var));
        node
    }
}