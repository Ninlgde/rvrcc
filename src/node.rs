//!
//! 生成AST（抽象语法树），语法解析
//!

use crate::ctype::{add_type, Type, TypeKind, TypeLink};
use crate::error_token;
use crate::obj::{Member, ObjLink};
use crate::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

/// ast节点类型
#[derive(Clone, Eq, PartialEq)]
pub enum NodeKind {
    /// 空表达式
    NullExpr,
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// 负号-
    Neg,
    /// % 求余
    Mod,
    /// &，按位与
    BitAnd,
    /// |，按位或
    BitOr,
    /// ^，按位异或
    BitXor,
    /// <<，左移
    Shl,
    /// >>，右移
    Shr,
    /// ==
    Eq,
    /// !=
    Ne,
    /// <
    Lt,
    /// <=
    Le,
    /// 赋值
    Assign,
    /// ?:，条件运算符
    Cond,
    /// , 逗号
    Comma,
    /// . 结构体成员访问
    Member,
    /// 取地址 &
    Addr,
    /// 解引用 *
    DeRef,
    /// !, 非
    Not,
    /// ~, 按位取非
    BitNot,
    /// &&，与
    LogAnd,
    /// ||，或
    LogOr,
    /// 返回
    Return,
    /// "if"，条件判断
    If,
    /// "for" 或 "while"，循环
    For,
    /// "do"，用于do while语句
    Do,
    /// "switch"，分支语句
    Switch,
    /// "case"
    Case,
    /// { ... }，代码块
    Block,
    /// goto，直接跳转语句
    Goto,
    /// 标签语句
    Label,
    /// 函数调用
    FuncCall,
    /// 表达式语句
    ExprStmt,
    /// 语句表达式
    StmtExpr,
    /// 变量
    Var,
    /// 数字
    Num,
    /// 类型转换
    Cast,
    /// 栈中变量清零
    MemZero,
}

/// box for node
pub type NodeLink = Box<Node>;

/// AST的节点
#[derive(Clone)]
pub struct Node {
    /// 类型
    pub(crate) kind: NodeKind,

    /// 对应的token,增加翻译阶段的报错信息
    pub(crate) token: Token,
    /// 类型
    pub(crate) typ: Option<TypeLink>,

    /// 各种孩子
    /// 左部，left-hand side(单节点)
    pub(crate) lhs: Option<NodeLink>,
    /// 右部，right-hand side
    pub(crate) rhs: Option<NodeLink>,
    /// 条件内的表达式
    pub(crate) cond: Option<NodeLink>,
    /// 符合条件后的语句
    pub(crate) then: Option<NodeLink>,
    /// 不符合条件后的语句
    pub(crate) els: Option<NodeLink>,
    /// 初始化语句
    pub(crate) init: Option<NodeLink>,
    /// 递增语句
    pub(crate) inc: Option<NodeLink>,
    /// 代码块
    pub(crate) body: Vec<NodeLink>,
    /// 变量名
    pub(crate) func_name: String,
    /// 函数类型
    pub(crate) func_type: Option<TypeLink>,
    /// 入参
    pub(crate) args: Vec<NodeLink>,
    /// 存储ND_VAR的字符串
    pub(crate) var: Option<ObjLink>,
    /// 存储ND_NUM种类的值
    pub(crate) val: i64,
    /// 结构体成员访问
    pub(crate) member: Option<Box<Member>>,
    /// goto和标签语句
    pub(crate) label_info: Option<Rc<RefCell<LabelInfo>>>,
    /// "break" 标签
    pub(crate) break_label: Option<String>,
    /// "continue" 标签 switch/case 时是case的label
    pub(crate) continue_label: Option<String>,
    /// switch和case
    pub(crate) case_next: Vec<NodeLink>,
    /// switch的default部分
    pub(crate) default_case: Option<NodeLink>,
}

impl Node {
    pub fn new(kind: NodeKind, token: Token) -> NodeLink {
        Box::new(Self {
            kind,
            token,
            typ: None,
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

    /// 创建单节点
    pub fn new_unary(kind: NodeKind, expr: NodeLink, token: Token) -> NodeLink {
        let mut node = Self::new(kind, token);
        node.lhs = Some(expr);
        node
    }

    /// 创建二叉节点
    pub fn new_binary(kind: NodeKind, lhs: NodeLink, rhs: NodeLink, token: Token) -> NodeLink {
        let mut node = Self::new(kind, token);
        node.lhs = Some(lhs);
        node.rhs = Some(rhs);
        node
    }

    /// 创建一个数值类型的节点
    pub fn new_num(val: i64, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node
    }

    /// 创建一个long类型的节点
    pub fn new_long(val: i64, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node.typ = Some(Type::new_long());
        node
    }

    /// 创建一个var类型的节点
    pub fn new_var(val: ObjLink, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Var, token);
        node.var = Some(val);
        node
    }

    /// 创建block节点,内含body数组的孩子节点
    pub fn new_block(kind: NodeKind, body: Vec<NodeLink>, token: Token) -> NodeLink {
        let mut node = Self::new(kind, token);
        node.body = body;
        node
    }

    /// 创建强转类型的节点
    pub fn new_cast(mut expr: NodeLink, typ: TypeLink) -> NodeLink {
        add_type(&mut expr);
        let mut node = Self::new(NodeKind::Cast, expr.token.clone());
        node.lhs = Some(expr);
        node.typ = Some(typ);
        node
    }

    /// 设置节点c类型
    pub fn set_type(&mut self, typ: TypeLink) {
        self.typ = Some(typ);
    }

    /// 获取节点所对应的token
    pub fn get_token(&self) -> &Token {
        &self.token
    }

    /// 获取节点的数据c类型
    pub fn get_type(&self) -> &Option<TypeLink> {
        &self.typ
    }

    /// 获取唯一的label值
    pub fn get_unique_label(&self) -> String {
        let label = self.label_info.as_ref().unwrap();
        let x = label.clone();
        let x = &*x.borrow();
        x.unique_label.to_string()
    }
}

/// 计算给定节点的常量表达式计算
pub fn eval(node: &mut Box<Node>) -> i64 {
    eval0(node, &mut None)
}

/// 计算给定节点的常量表达式计算
/// 常量表达式可以是数字或者是 ptr±n，ptr是指向全局变量的指针，n是偏移量。
pub fn eval0(node: &mut Box<Node>, label: &mut Option<String>) -> i64 {
    add_type(node);

    match node.kind {
        NodeKind::Add => {
            return eval0(node.lhs.as_mut().unwrap(), label) + eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Sub => {
            return eval0(node.lhs.as_mut().unwrap(), label) - eval(node.rhs.as_mut().unwrap());
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
        NodeKind::Shl => {
            return eval(node.lhs.as_mut().unwrap()) << eval(node.rhs.as_mut().unwrap());
        }
        NodeKind::Shr => {
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
                eval0(node.then.as_mut().unwrap(), label)
            } else {
                eval0(node.els.as_mut().unwrap(), label)
            };
        }
        NodeKind::Comma => {
            return eval0(node.rhs.as_mut().unwrap(), label);
        }
        NodeKind::Not => {
            return if eval(node.lhs.as_mut().unwrap()) != 0 {
                0
            } else {
                1
            };
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
            let t = node.typ.as_ref().unwrap().borrow();
            let r = eval0(node.lhs.as_mut().unwrap(), label);
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
        NodeKind::Addr => {
            return eval_rval(node.lhs.as_mut().unwrap(), label);
        }
        NodeKind::Member => {
            // 未开辟Label的地址，则表明不是表达式常量
            if label.is_some() {
                error_token!(&node.token, "not a compile-time constant");
            }
            if node.typ.as_ref().unwrap().borrow().kind != TypeKind::Array {
                error_token!(&node.token, "invalid initializer");
            }
            return eval_rval(node.lhs.as_mut().unwrap(), label)
                + node.member.as_ref().unwrap().offset as i64;
        }
        NodeKind::Var => {
            // 未开辟Label的地址，则表明不是表达式常量
            if label.is_some() {
                error_token!(&node.token, "not a compile-time constant");
            }
            let typ = node.typ.as_ref().unwrap().borrow();
            if typ.kind != TypeKind::Array && typ.kind != TypeKind::Func {
                error_token!(&node.token, "invalid initializer");
            }
            let var = node.var.as_ref().unwrap().borrow();
            *label = Some(var.get_name().clone());
            return 0;
        }
        NodeKind::Num => {
            return node.val;
        }
        _ => {
            error_token!(&node.token, "invalid initializer");
            return -1;
        }
    }
}

/// 计算重定位变量
fn eval_rval(node: &mut Box<Node>, label: &mut Option<String>) -> i64 {
    return match node.kind {
        NodeKind::Var => {
            // 局部变量不能参与全局变量的初始化
            let var = node.var.as_ref().unwrap().borrow();
            if var.is_local() {
                error_token!(&node.token, "not a compile-time constant");
            }
            *label = Some(var.get_name().clone());
            0
        }
        NodeKind::DeRef => {
            // 直接进入到解引用的地址
            eval0(node.lhs.as_mut().unwrap(), label)
        }
        NodeKind::Member => {
            // 加上成员变量的偏移量
            eval_rval(node.lhs.as_mut().unwrap(), label)
                + node.member.as_ref().unwrap().offset as i64
        }
        _ => {
            error_token!(&node.token, "invalid initializer");
            -1
        }
    };
}

/// bool转int true=1 false=0
fn bool_to_i64(result: bool) -> i64 {
    if result {
        1
    } else {
        0
    }
}

/// int转bool 0=false other=true
fn i64_to_bool(result: i64) -> bool {
    if result != 0 {
        true
    } else {
        false
    }
}

/// 标签信息
pub struct LabelInfo {
    // goto和标签语句
    pub label: String,
    /// 唯一标签,用于汇编内的跳转
    pub unique_label: String,
    /// 用于报错的token
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

    /// 创建goto标签
    pub fn new_goto(label: String, token: Token) -> Rc<RefCell<Self>> {
        Self::new(label, String::new(), token)
    }

    /// 创建break标签
    pub fn new_break(unique_label: String, token: Token) -> Rc<RefCell<Self>> {
        Self::new(String::new(), unique_label, token)
    }

    /// 设置唯一标签
    pub fn set_unique_label(&mut self, unique_label: String) {
        self.unique_label = unique_label;
    }

    /// 判断标签是否相等
    pub fn equals(&self, other: &Self) -> bool {
        self.label == other.label
    }
}

/// 解析各种type的加法
pub fn add_with_type(mut lhs: NodeLink, mut rhs: NodeLink, nt: Token) -> Option<NodeLink> {
    // 为左右部添加类型
    add_type(&mut lhs);
    add_type(&mut rhs);

    let lhs_t = lhs.get_type().as_ref().unwrap().clone();
    let rhs_t = rhs.get_type().as_ref().unwrap().clone();
    // num + num
    if lhs_t.borrow().is_int() && rhs_t.borrow().is_int() {
        return Some(Node::new_binary(NodeKind::Add, lhs, rhs, nt));
    }

    // 不能解析 ptr + ptr
    if lhs_t.borrow().has_base() && rhs_t.borrow().has_base() {
        error_token!(&nt, "invalid operands");
        return None;
    }

    // 将 num + ptr 转换为 ptr + num
    let n_lhs;
    let n_rhs;
    let size;
    if !lhs_t.borrow().has_base() && rhs_t.borrow().has_base() {
        n_lhs = rhs;
        n_rhs = lhs;
        size = rhs_t.borrow().get_base_size() as i64;
    } else {
        n_lhs = lhs;
        n_rhs = rhs;
        size = lhs_t.borrow().get_base_size() as i64;
    }

    // ptr + num
    // 指针加法，ptr+1，这里的1不是1个字节，而是1个元素的空间，所以需要 ×size 操作
    let size = Node::new_long(size, nt.clone());
    let f_rhs = Node::new_binary(NodeKind::Mul, n_rhs, size, nt.clone());
    Some(Node::new_binary(NodeKind::Add, n_lhs, f_rhs, nt))
}

/// 解析各种type的减法
pub fn sub_with_type(mut lhs: NodeLink, mut rhs: NodeLink, nt: Token) -> Option<NodeLink> {
    // 为左右部添加类型
    add_type(&mut lhs);
    add_type(&mut rhs);

    let lhs_t = lhs.get_type().as_ref().unwrap().clone();
    let rhs_t = rhs.get_type().as_ref().unwrap().clone();
    // num + num
    if lhs_t.borrow().is_int() && rhs_t.borrow().is_int() {
        return Some(Node::new_binary(NodeKind::Sub, lhs, rhs, nt));
    }

    // ptr - num
    if lhs_t.borrow().has_base() && rhs_t.borrow().is_int() {
        let size: i64 = lhs_t.borrow().get_base_size() as i64;
        let size = Node::new_long(size, nt.clone());
        let mut f_rhs = Node::new_binary(NodeKind::Mul, rhs, size, nt.clone());
        add_type(&mut f_rhs);
        let mut node = Node::new_binary(NodeKind::Sub, lhs, f_rhs, nt);
        node.set_type(lhs_t);
        return Some(node);
    }

    // ptr - ptr，返回两指针间有多少元素
    if lhs_t.borrow().has_base() && rhs_t.borrow().has_base() {
        let mut node = Node::new_binary(NodeKind::Sub, lhs, rhs, nt.clone());
        node.set_type(Type::new_long());
        let size: i64 = lhs_t.borrow().get_base_size() as i64;
        let size = Node::new_num(size, nt.clone());
        // size.set_type(Type::new_int());
        return Some(Node::new_binary(NodeKind::Div, node, size, nt));
    }

    error_token!(&nt, "invalid operands");
    return None;
}
