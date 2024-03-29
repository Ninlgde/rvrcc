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
    /// "goto" 的对应的地址表达式
    GotoExpr,
    /// 标签语句
    Label,
    /// "goto" 标签值
    LabelVal,
    /// 函数调用
    FuncCall,
    /// 表达式语句
    ExprStmt,
    /// 语句表达式
    StmtExpr,
    /// 变量
    Var,
    /// VLA指派器
    VLAPtr,
    /// 数字
    Num,
    /// 类型转换
    Cast,
    /// 栈中变量清零
    MemZero,
    /// "asm"汇编
    Asm,
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
    /// 函数类型
    pub(crate) func_type: Option<TypeLink>,
    /// 入参
    pub(crate) args: Vec<NodeLink>,
    /// 通过栈传递
    pub(crate) pass_by_stack: bool,
    /// 返回值缓冲区
    pub(crate) ret_buf: Option<ObjLink>,
    /// 存储ND_VAR的字符串
    pub(crate) var: Option<ObjLink>,
    /// 存储ND_NUM种类的值
    pub(crate) val: i64,
    /// 存储ND_NUM种类的浮点值
    pub(crate) fval: f64,
    /// 结构体成员访问
    pub(crate) member: Option<Box<Member>>,
    /// goto和标签语句
    pub(crate) label_info: Option<Rc<RefCell<LabelInfo>>>,
    /// "break" 标签
    pub(crate) break_label: Option<String>,
    /// "continue" 标签 switch/case 时是case的label
    pub(crate) continue_label: Option<String>,
    /// switch语句
    pub(crate) case_next: Vec<NodeLink>,
    /// switch的default部分
    pub(crate) default_case: Option<NodeLink>,
    /// case后面的数值
    pub(crate) case_begin: i64,
    /// case ...后面的数值
    pub(crate) case_end: i64,
    /// "asm" 字符串字面量
    pub(crate) asm_str: String,
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
            func_type: None,
            args: Vec::new(),
            pass_by_stack: false,
            ret_buf: None,
            var: None,
            val: 0,
            fval: 0.0,
            member: None,
            label_info: None,
            break_label: None,
            continue_label: None,
            case_next: Vec::new(),
            default_case: None,
            case_begin: 0,
            case_end: 0,
            asm_str: "".to_string(),
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

    pub fn new_unsigned_long(val: i64, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Num, token);
        node.val = val;
        node.typ = Some(Type::new_unsigned_long());
        node
    }

    /// 创建一个var类型的节点
    pub fn new_var(val: ObjLink, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::Var, token);
        node.var = Some(val);
        node
    }

    /// 创建一个vla ptr类型的节点
    pub fn new_vla_ptr(val: ObjLink, token: Token) -> NodeLink {
        let mut node = Self::new(NodeKind::VLAPtr, token);
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
        node.typ = Some(Type::from(&typ));
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
    pub fn get_unique_label(&self) -> Rc<RefCell<String>> {
        let label = self.label_info.as_ref().unwrap();
        let x = label.clone();
        let x = &*x.borrow();
        x.unique_label.clone()
    }

    pub fn get_unique_label_str(&self) -> String {
        self.get_unique_label().borrow().to_string()
    }
}

/// 计算给定节点的常量表达式计算
pub fn eval(node: &mut Box<Node>) -> i64 {
    eval0(node, &mut None)
}

/// 计算给定节点的常量表达式计算
/// 常量表达式可以是数字或者是 ptr±n，ptr是指向全局变量的指针，n是偏移量。
pub fn eval0(node: &mut Box<Node>, label: &mut Option<Rc<RefCell<String>>>) -> i64 {
    add_type(node);

    if node.typ.as_ref().unwrap().borrow().is_float() {
        return eval_double(node) as i64;
    }

    return match node.kind {
        NodeKind::Add => {
            eval0(node.lhs.as_mut().unwrap(), label) + eval(node.rhs.as_mut().unwrap())
        }
        NodeKind::Sub => {
            eval0(node.lhs.as_mut().unwrap(), label) - eval(node.rhs.as_mut().unwrap())
        }
        NodeKind::Mul => eval(node.lhs.as_mut().unwrap()) * eval(node.rhs.as_mut().unwrap()),
        NodeKind::Div => {
            if node.typ.as_ref().unwrap().borrow().is_unsigned {
                return (eval(node.lhs.as_mut().unwrap()) as u64
                    / eval(node.rhs.as_mut().unwrap()) as u64) as i64;
            }
            eval(node.lhs.as_mut().unwrap()) / eval(node.rhs.as_mut().unwrap())
        }
        NodeKind::Neg => -eval(node.lhs.as_mut().unwrap()),
        NodeKind::Mod => {
            if node.typ.as_ref().unwrap().borrow().is_unsigned {
                return (eval(node.lhs.as_mut().unwrap()) as u64
                    % eval(node.rhs.as_mut().unwrap()) as u64) as i64;
            }
            eval(node.lhs.as_mut().unwrap()) % eval(node.rhs.as_mut().unwrap())
        }
        NodeKind::BitAnd => eval(node.lhs.as_mut().unwrap()) & eval(node.rhs.as_mut().unwrap()),
        NodeKind::BitOr => eval(node.lhs.as_mut().unwrap()) | eval(node.rhs.as_mut().unwrap()),
        NodeKind::BitXor => eval(node.lhs.as_mut().unwrap()) ^ eval(node.rhs.as_mut().unwrap()),
        NodeKind::Shl => {
            let typ = node.typ.as_ref().unwrap().borrow();
            if typ.is_unsigned && typ.size == 8 {
                return ((eval(node.lhs.as_mut().unwrap()) as u64)
                    << eval(node.rhs.as_mut().unwrap())) as i64;
            }
            eval(node.lhs.as_mut().unwrap()) << eval(node.rhs.as_mut().unwrap())
        }
        NodeKind::Shr => eval(node.lhs.as_mut().unwrap()) >> eval(node.rhs.as_mut().unwrap()),
        NodeKind::Eq => {
            bool_to_i64(eval(node.lhs.as_mut().unwrap()) == eval(node.rhs.as_mut().unwrap()))
        }
        NodeKind::Ne => {
            bool_to_i64(eval(node.lhs.as_mut().unwrap()) != eval(node.rhs.as_mut().unwrap()))
        }
        NodeKind::Lt => {
            let r;
            let lhs = node.lhs.as_ref().unwrap();
            if lhs.typ.as_ref().unwrap().borrow().is_unsigned {
                r = (eval(node.lhs.as_mut().unwrap()) as u64)
                    < eval(node.rhs.as_mut().unwrap()) as u64
            } else {
                r = eval(node.lhs.as_mut().unwrap()) < eval(node.rhs.as_mut().unwrap())
            }
            bool_to_i64(r)
        }
        NodeKind::Le => {
            let r;
            let lhs = node.lhs.as_ref().unwrap();
            if lhs.typ.as_ref().unwrap().borrow().is_unsigned {
                r = (eval(node.lhs.as_mut().unwrap()) as u64)
                    <= eval(node.rhs.as_mut().unwrap()) as u64
            } else {
                r = eval(node.lhs.as_mut().unwrap()) <= eval(node.rhs.as_mut().unwrap())
            }
            bool_to_i64(r)
        }
        NodeKind::Cond => {
            let cond = eval(node.cond.as_mut().unwrap());
            if cond == 1 {
                eval0(node.then.as_mut().unwrap(), label)
            } else {
                eval0(node.els.as_mut().unwrap(), label)
            }
        }
        NodeKind::Comma => eval0(node.rhs.as_mut().unwrap(), label),
        NodeKind::Not => {
            if eval(node.lhs.as_mut().unwrap()) != 0 {
                0
            } else {
                1
            }
        }
        NodeKind::BitNot => {
            !eval(node.lhs.as_mut().unwrap()) // 按位取反
        }
        NodeKind::LogAnd => bool_to_i64(
            i64_to_bool(eval(node.lhs.as_mut().unwrap()))
                && i64_to_bool(eval(node.rhs.as_mut().unwrap())),
        ),
        NodeKind::LogOr => bool_to_i64(
            i64_to_bool(eval(node.lhs.as_mut().unwrap()))
                || i64_to_bool(eval(node.rhs.as_mut().unwrap())),
        ),
        NodeKind::Cast => {
            let t = node.typ.as_ref().unwrap().borrow();
            let r = eval0(node.lhs.as_mut().unwrap(), label);
            if t.is_int() {
                match t.size {
                    1 => {
                        return if t.is_unsigned {
                            r as u8 as i64
                        } else {
                            r as i8 as i64
                        };
                    }
                    2 => {
                        return if t.is_unsigned {
                            r as u16 as i64
                        } else {
                            r as i16 as i64
                        };
                    }
                    4 => {
                        return if t.is_unsigned {
                            r as u32 as i64
                        } else {
                            r as i32 as i64
                        };
                    }
                    _ => {}
                }
            }
            r
        }
        NodeKind::Addr => eval_rval(node.lhs.as_mut().unwrap(), label),
        NodeKind::LabelVal => {
            // 将标签值也作为常量
            *label = Some(node.get_unique_label());
            0
        }
        NodeKind::Member => {
            // 未开辟Label的地址，则表明不是表达式常量
            if label.is_some() {
                error_token!(&node.token, "not a compile-time constant");
            }
            if node.typ.as_ref().unwrap().borrow().kind != TypeKind::Array {
                error_token!(&node.token, "invalid initializer");
            }
            eval_rval(node.lhs.as_mut().unwrap(), label)
                + node.member.as_ref().unwrap().offset as i64
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
            *label = Some(Rc::new(RefCell::new(var.get_name().clone())));
            0
        }
        NodeKind::Num => node.val,
        _ => {
            error_token!(&node.token, "invalid initializer");
            -1
        }
    };
}

/// 计算重定位变量
fn eval_rval(node: &mut Box<Node>, label: &mut Option<Rc<RefCell<String>>>) -> i64 {
    return match node.kind {
        NodeKind::Var => {
            // 局部变量不能参与全局变量的初始化
            let var = node.var.as_ref().unwrap().borrow();
            if var.is_local() {
                error_token!(&node.token, "not a compile-time constant");
            }
            *label = Some(Rc::new(RefCell::new(var.get_name().clone())));
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

/// 解析浮点表达式
pub fn eval_double(node: &mut Box<Node>) -> f64 {
    add_type(node);

    let binding = node.typ.as_ref().unwrap().clone();
    let t = binding.borrow();
    if t.is_int() {
        if t.is_unsigned {
            return eval(node) as u64 as f64;
        }
        return eval(node) as f64;
    }

    return match node.kind {
        NodeKind::Add => {
            eval_double(node.lhs.as_mut().unwrap()) + eval_double(node.rhs.as_mut().unwrap())
        }
        NodeKind::Sub => {
            eval_double(node.lhs.as_mut().unwrap()) - eval_double(node.rhs.as_mut().unwrap())
        }
        NodeKind::Mul => {
            eval_double(node.lhs.as_mut().unwrap()) * eval_double(node.rhs.as_mut().unwrap())
        }
        NodeKind::Div => {
            eval_double(node.lhs.as_mut().unwrap()) / eval_double(node.rhs.as_mut().unwrap())
        }
        NodeKind::Neg => -eval_double(node.lhs.as_mut().unwrap()),
        NodeKind::Cond => {
            let cond = eval_double(node.cond.as_mut().unwrap());
            if cond != 0.0 {
                eval_double(node.then.as_mut().unwrap())
            } else {
                eval_double(node.els.as_mut().unwrap())
            }
        }
        NodeKind::Comma => eval_double(node.rhs.as_mut().unwrap()),
        NodeKind::Cast => {
            let lhs = node.lhs.as_mut().unwrap();
            if lhs.typ.as_ref().unwrap().borrow().is_float() {
                return eval_double(lhs);
            }
            eval(lhs) as f64
        }
        NodeKind::Num => node.fval,
        _ => {
            error_token!(&node.token, "invalid initializer");
            -1.0
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
    pub unique_label: Rc<RefCell<String>>,
    /// 用于报错的token
    pub token: Token,
}

impl LabelInfo {
    pub fn new(label: String, unique_label: String, token: Token) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(LabelInfo {
            label,
            unique_label: Rc::new(RefCell::new(unique_label)),
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
        self.unique_label.replace(unique_label);
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
    if lhs_t.borrow().is_numeric() && rhs_t.borrow().is_numeric() {
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

    // VLA + num
    // 指针加法，需要num×VLASize操作
    let lhs_t = n_lhs.get_type().as_ref().unwrap().clone();
    let lbt = lhs_t.borrow().get_base_type();
    if lbt.is_some() {
        let lbt = lbt.unwrap();
        if lbt.borrow().kind == TypeKind::VLA {
            let vn = Node::new_var(lbt.borrow().vla_size.as_ref().unwrap().clone(), nt.clone());
            let f_rhs = Node::new_binary(NodeKind::Mul, n_rhs, vn, nt.clone());
            return Some(Node::new_binary(NodeKind::Add, n_lhs, f_rhs, nt));
        }
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
    if lhs_t.borrow().is_numeric() && rhs_t.borrow().is_numeric() {
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

pub fn is_const_expr(node: &mut NodeLink) -> bool {
    add_type(node);

    return match node.kind {
        NodeKind::Add
        | NodeKind::Sub
        | NodeKind::Mul
        | NodeKind::Div
        | NodeKind::BitAnd
        | NodeKind::BitOr
        | NodeKind::BitXor
        | NodeKind::Shl
        | NodeKind::Shr
        | NodeKind::Eq
        | NodeKind::Ne
        | NodeKind::Lt
        | NodeKind::Le
        | NodeKind::LogAnd
        | NodeKind::LogOr => {
            // 左部右部 都为常量表达式时 为真
            is_const_expr(node.lhs.as_mut().unwrap()) && is_const_expr(node.rhs.as_mut().unwrap())
        }
        NodeKind::Cond => {
            // 条件不为常量表达式时 为假
            if !is_const_expr(node.cond.as_mut().unwrap()) {
                return false;
            }
            // 条件为常量表达式时，判断相应分支语句是否为真
            if eval(node.cond.as_mut().unwrap()) != 0 {
                is_const_expr(node.then.as_mut().unwrap())
            } else {
                is_const_expr(node.els.as_mut().unwrap())
            }
        }
        // 判断逗号最右表达式是否为 常量表达式
        NodeKind::Comma => is_const_expr(node.rhs.as_mut().unwrap()),
        // 判断左部是否为常量表达式
        NodeKind::Neg | NodeKind::Not | NodeKind::BitNot | NodeKind::Cast => {
            is_const_expr(node.lhs.as_mut().unwrap())
        }
        // 数字恒为常量表达式
        NodeKind::Num => true,
        // 其他情况默认为假
        _ => false,
    };
}
