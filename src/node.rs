//
// 生成AST（抽象语法树），语法解析
//

use crate::{add_type, Member, ObjLink, Token, Type, TypeLink};
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
    // <<，左移
    ShL,
    // >>，右移
    ShR,
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
    // ?:，条件运算符
    Cond,
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
    // "switch"，分支语句
    Switch,
    // "case"
    Case,
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

pub type NodeLink = Box<Node>;

// AST的节点种类
#[derive(Clone)]
pub struct Node {
    pub(crate) kind: NodeKind,

    // 对应的token,增加翻译阶段的报错信息
    pub(crate) token: Token,
    // 类型
    pub(crate) type_: Option<TypeLink>,

    // 各种孩子
    // 左部，left-hand side(单节点)
    pub(crate) lhs: Option<NodeLink>,
    // 右部，right-hand side
    pub(crate) rhs: Option<NodeLink>,
    // 条件内的表达式
    pub(crate) cond: Option<NodeLink>,
    // 符合条件后的语句
    pub(crate) then: Option<NodeLink>,
    // 不符合条件后的语句
    pub(crate) els: Option<NodeLink>,
    // 初始化语句
    pub(crate) init: Option<NodeLink>,
    // 递增语句
    pub(crate) inc: Option<NodeLink>,
    // 代码块
    pub(crate) body: Vec<NodeLink>,
    // 变量名
    pub(crate) func_name: String,
    // 函数类型
    pub(crate) func_type: Option<TypeLink>,
    // 入参
    pub(crate) args: Vec<NodeLink>,
    // 存储ND_VAR的字符串
    pub(crate) var: Option<ObjLink>,
    // 存储ND_NUM种类的值
    pub(crate) val: i64,
    // 结构体成员访问
    pub(crate) member: Option<Box<Member>>,
    // goto和标签语句
    pub(crate) label_info: Option<Rc<RefCell<LabelInfo>>>,
    // "break" 标签
    pub(crate) break_label: Option<String>,
    // "continue" 标签 switch/case 时是case的label
    pub(crate) continue_label: Option<String>,
    // switch和case
    pub(crate) case_next: Vec<NodeLink>,
    pub(crate) default_case: Option<NodeLink>,
}

impl Node {
    pub fn new(kind: NodeKind, token: Token) -> NodeLink {
        Box::new(Self {
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
            break_label: None,
            continue_label: None,
            case_next: Vec::new(),
            default_case: None,
        })
    }

    pub fn new_unary(kind: NodeKind, expr: NodeLink, token: Token) -> NodeLink {
        let mut node = Self::new(kind, token);
        node.lhs = Some(expr);
        node
    }

    pub fn new_binary(kind: NodeKind, lhs: NodeLink, rhs: NodeLink, token: Token) -> NodeLink {
        let mut node = Self::new(kind, token);
        node.lhs = Some(lhs);
        node.rhs = Some(rhs);
        node
    }

    pub fn new_num(val: i64, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node
    }

    pub fn new_long(val: i64, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node.type_ = Some(Type::new_long());
        node
    }

    pub fn new_var(val: ObjLink, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Var, token);
        node.var = Some(val);
        node
    }

    pub fn new_block(kind: NodeKind, body: Vec<NodeLink>, token: Token) -> NodeLink {
        let mut node = Self::new(kind, token);
        node.body = body;
        node
    }

    pub fn new_cast(mut expr: NodeLink, typ: TypeLink) -> NodeLink {
        add_type(&mut expr);
        let mut node = Self::new(NodeKind::Cast, expr.token.clone());
        node.lhs = Some(expr);
        node.type_ = Some(typ);
        node
    }

    pub fn set_type(&mut self, type_: TypeLink) {
        self.type_ = Some(type_);
    }

    pub fn get_token(&self) -> &Token {
        &self.token
    }

    pub fn get_type(&self) -> &Option<TypeLink> {
        &self.type_
    }

    pub fn get_unique_label(&self) -> String {
        let label = self.label_info.as_ref().unwrap();
        let x = label.clone();
        let x = &*x.borrow();
        x.unique_label.to_string()
    }
}

/// 计算给定节点的常量表达式计算
pub fn eval(node: &mut Box<Node>) -> i64 {
    add_type(node);

    match node.kind {
        NodeKind::Add => {
            return eval(node.lhs.as_mut().unwrap()) + eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Sub => {
            return eval(node.lhs.as_mut().unwrap()) - eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Mul => {
            return eval(node.lhs.as_mut().unwrap()) * eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Div => {
            return eval(node.lhs.as_mut().unwrap()) / eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Neg => {
            return -eval(node.lhs.as_mut().unwrap());
        }
        NodeKind::Mod => {
            return eval(node.lhs.as_mut().unwrap()) % eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::BitAnd => {
            return eval(node.lhs.as_mut().unwrap()) & eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::BitOr => {
            return eval(node.lhs.as_mut().unwrap()) | eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::BitXor => {
            return eval(node.lhs.as_mut().unwrap()) ^ eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::ShL => {
            return eval(node.lhs.as_mut().unwrap()) << eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::ShR => {
            return eval(node.lhs.as_mut().unwrap()) >> eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Eq => {
            return bool_to_i64(
                eval(node.lhs.as_mut().unwrap()) == eval(node.rhs.as_mut().unwrap()),
            );
        }
        NodeKind::Ne => {
            return bool_to_i64(
                eval(node.lhs.as_mut().unwrap()) != eval(node.rhs.as_mut().unwrap()),
            )
        }
        NodeKind::Lt => {
            return bool_to_i64(
                eval(node.lhs.as_mut().unwrap()) < eval(node.rhs.as_mut().unwrap()),
            );
        }
        NodeKind::Le => {
            return bool_to_i64(
                eval(node.lhs.as_mut().unwrap()) <= eval(node.rhs.as_mut().unwrap()),
            );
        }
        NodeKind::Cond => {
            let cond = eval(node.cond.as_mut().unwrap());
            return if cond == 1 {
                eval(node.then.as_mut().unwrap())
            } else {
                eval(node.els.as_mut().unwrap())
            };
        }
        NodeKind::Comma => {
            return eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Not => {
            return eval(node.lhs.as_mut().unwrap()) ^ 1; // 0 ^ 1 = 1; 1 ^ 1 = 0;
        }
        NodeKind::BitNot => {
            return !eval(node.lhs.as_mut().unwrap()); // 按位取反
        }
        NodeKind::LogAnd => {
            return bool_to_i64(
                i64_to_bool(eval(node.lhs.as_mut().unwrap()))
                    && i64_to_bool(eval(node.rhs.as_mut().unwrap())),
            )
        }
        NodeKind::LogOr => {
            return bool_to_i64(
                i64_to_bool(eval(node.lhs.as_mut().unwrap()))
                    || i64_to_bool(eval(node.rhs.as_mut().unwrap())),
            )
        }
        NodeKind::Cast => {
            let t = node.type_.as_ref().unwrap().borrow();
            let r = eval(node.lhs.as_mut().unwrap());
            if t.is_int() {
                match t.size {
                    1 => {
                        return r as i8 as i64;
                    }
                    2 => {
                        return r as i16 as i64;
                    }
                    4 => {
                        return r as i32 as i64;
                    }
                    _ => {}
                }
            }
            return r;
        }
        NodeKind::Num => {
            return node.val;
        }
        _ => {}
    }
    0
}

fn bool_to_i64(result: bool) -> i64 {
    if result {
        1
    } else {
        0
    }
}

fn i64_to_bool(result: i64) -> bool {
    if result != 0 {
        true
    } else {
        false
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

    pub fn new_break(unique_label: String, token: Token) -> Rc<RefCell<Self>> {
        Self::new(String::new(), unique_label, token)
    }

    pub fn set_unique_label(&mut self, unique_label: String) {
        self.unique_label = unique_label;
    }

    pub fn equals(&self, other: &Self) -> bool {
        self.label == other.label
    }
}
