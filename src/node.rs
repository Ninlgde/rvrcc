//
// 生成AST（抽象语法树），语法解析
//

use crate::ctype::add_type;
use crate::obj::Member;
use crate::{Obj, Token, Type};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq)]
pub enum NodeKind {
    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,
    // 负号-
    Neg,
    // % 求余
    Mod,
    // &，按位与
    BitAnd,
    // |，按位或
    BitOr,
    // ^，按位异或
    BitXor,
    // ==
    Eq,
    // !=
    Ne,
    // <
    Lt,
    // <=
    Le,
    // 赋值
    Assign,
    // , 逗号
    Comma,
    // . 结构体成员访问
    Member,
    // 取地址 &
    Addr,
    // 解引用 *
    DeRef,
    // !, 非
    Not,
    // ~, 按位取非
    BitNot,
    // &&，与
    LogAnd,
    // ||，或
    LogOr,
    // 返回
    Return,
    // "if"，条件判断
    If,
    // "for" 或 "while"，循环
    For,
    // { ... }，代码块
    Block,
    // goto，直接跳转语句
    Goto,
    // 标签语句
    Label,
    // 函数调用
    FuncCall,
    // 表达式语句
    ExprStmt,
    // 语句表达式
    StmtExpr,
    // 变量
    Var,
    // 数字
    Num,
    // 类型转换
    Cast,
}

// AST的节点种类
#[derive(Clone)]
pub struct Node {
    pub(crate) kind: NodeKind,

    // 对应的token,增加翻译阶段的报错信息
    pub(crate) token: Token,
    // 类型
    pub(crate) type_: Option<Rc<RefCell<Type>>>,

    // 各种孩子
    // 左部，left-hand side(单节点)
    pub(crate) lhs: Option<Box<Node>>,
    // 右部，right-hand side
    pub(crate) rhs: Option<Box<Node>>,
    // 条件内的表达式
    pub(crate) cond: Option<Box<Node>>,
    // 符合条件后的语句
    pub(crate) then: Option<Box<Node>>,
    // 不符合条件后的语句
    pub(crate) els: Option<Box<Node>>,
    // 初始化语句
    pub(crate) init: Option<Box<Node>>,
    // 递增语句
    pub(crate) inc: Option<Box<Node>>,
    // 代码块
    pub(crate) body: Vec<Node>,
    // 变量名
    pub(crate) func_name: String,
    // 函数类型
    pub(crate) func_type: Option<Rc<RefCell<Type>>>,
    // 入参
    pub(crate) args: Vec<Node>,
    // 存储ND_VAR的字符串
    pub(crate) var: Option<Rc<RefCell<Obj>>>,
    // 存储ND_NUM种类的值
    pub(crate) val: i64,
    // 结构体成员访问
    pub(crate) member: Option<Box<Member>>,
    // goto和标签语句
    pub(crate) label_info: Option<Rc<RefCell<LabelInfo>>>,
}

impl Node {
    pub fn new(kind: NodeKind, token: Token) -> Self {
        Self {
            kind,
            token,
            type_: None,
            lhs: None,
            rhs: None,
            cond: None,
            then: None,
            els: None,
            init: None,
            inc: None,
            body: Vec::new(),
            func_name: String::new(),
            func_type: None,
            args: Vec::new(),
            var: None,
            val: 0,
            member: None,
            label_info: None,
        }
    }

    pub fn new_unary(kind: NodeKind, expr: Box<Node>, token: Token) -> Self {
        let mut node = Self::new(kind, token);
        node.lhs = Some(expr);
        node
    }

    pub fn new_binary(kind: NodeKind, lhs: Box<Node>, rhs: Box<Node>, token: Token) -> Self {
        let mut node = Self::new(kind, token);
        node.lhs = Some(lhs);
        node.rhs = Some(rhs);
        node
    }

    pub fn new_num(val: i64, token: Token) -> Self {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node
    }

    pub fn new_long(val: i64, token: Token) -> Self {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node.type_ = Some(Type::new_long());
        node
    }

    pub fn new_var(val: Rc<RefCell<Obj>>, token: Token) -> Self {
        let mut node = Self::new(NodeKind::Var, token);
        node.var = Some(val);
        node
    }

    pub fn new_block(kind: NodeKind, body: Vec<Node>, token: Token) -> Self {
        let mut node = Self::new(kind, token);
        node.body = body;
        node
    }

    pub fn new_cast(mut expr: Box<Node>, typ: Rc<RefCell<Type>>) -> Self {
        add_type(&mut expr);
        let mut node = Self::new(NodeKind::Cast, expr.token.clone());
        node.lhs = Some(expr);
        node.type_ = Some(typ);
        node
    }

    pub fn set_type(&mut self, type_: Rc<RefCell<Type>>) -> &Self {
        self.type_ = Some(type_);
        self
    }

    pub fn get_token(&self) -> &Token {
        &self.token
    }

    pub fn get_type(&self) -> &Option<Rc<RefCell<Type>>> {
        &self.type_
    }

    pub fn get_unique_label(&self) -> String {
        let label = self.label_info.as_ref().unwrap();
        let x = label.clone();
        let x = &*x.borrow();
        x.unique_label.to_string()
    }
}

pub struct LabelInfo {
    // goto和标签语句
    pub label: String,
    pub unique_label: String,
    pub token: Token,
}

impl LabelInfo {
    pub fn new(label: String, unique_label: String, token: Token) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(LabelInfo {
            label,
            unique_label,
            token,
        }))
    }

    pub fn new_goto(label: String, token: Token) -> Rc<RefCell<Self>> {
        Self::new(label, String::new(), token)
    }

    pub fn set_unique_label(&mut self, unique_label: String) {
        self.unique_label = unique_label;
    }

    pub fn equals(&self, other: &Self) -> bool {
        self.label == other.label
    }
}
