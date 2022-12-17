//
// 生成AST（抽象语法树），语法解析
//

use std::cell::RefCell;
use std::rc::Rc;
use crate::{Obj, Token, Type};

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
    // 语句表达式
    StmtExpr {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 代码块
        body: Vec<Node>,
        // 类型
        type_: Option<Box<Type>>,
    },
    // 变量
    Var {
        // 对应的token,增加翻译阶段的报错信息
        token: Token,
        // 存储ND_VAR的字符串
        var: Option<Rc<RefCell<Obj>>>,
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
            Node::Add { token, .. }
            | Node::Sub { token, .. }
            | Node::Mul { token, .. }
            | Node::Div { token, .. }
            | Node::Neg { token, .. }
            | Node::Eq { token, .. }
            | Node::Ne { token, .. }
            | Node::Lt { token, .. }
            | Node::Le { token, .. }
            | Node::Assign { token, .. }
            | Node::Addr { token, .. }
            | Node::DeRef { token, .. }
            | Node::Return { token, .. }
            | Node::If { token, .. }
            | Node::For { token, .. }
            | Node::Block { token, .. }
            | Node::FuncCall { token, .. }
            | Node::ExprStmt { token, .. }
            | Node::StmtExpr { token, .. }
            | Node::Var { token, .. }
            | Node::Num { token, .. } => token,
        }
    }

    pub fn get_type(&self) -> &Option<Box<Type>> {
        match self {
            Node::Add { type_, .. }
            | Node::Sub { type_, .. }
            | Node::Mul { type_, .. }
            | Node::Div { type_, .. }
            | Node::Neg { type_, .. }
            | Node::Eq { type_, .. }
            | Node::Ne { type_, .. }
            | Node::Lt { type_, .. }
            | Node::Le { type_, .. }
            | Node::Assign { type_, .. }
            | Node::Addr { type_, .. }
            | Node::DeRef { type_, .. }
            | Node::Return { type_, .. }
            | Node::If { type_, .. }
            | Node::For { type_, .. }
            | Node::Block { type_, .. }
            | Node::FuncCall { type_, .. }
            | Node::ExprStmt { type_, .. }
            | Node::StmtExpr { type_, .. }
            | Node::Var { type_, .. }
            | Node::Num { type_, .. } => type_,
        }
    }

    pub fn get_body(&self) -> &Vec<Node> {
        match self {
            Node::Block { body, .. } => { body }
            _ => panic!("get body from wrong node")
        }
    }
}