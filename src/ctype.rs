//! C语言的类型实现

use crate::error_token;
use crate::node::{Node, NodeKind, NodeLink};
use crate::obj::Member;
use crate::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

/// c类型的种类
#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
    /// void类型
    Void,
    /// _Bool布尔类型
    Bool,
    /// char字符类型
    Char,
    /// short短整型
    Short,
    /// int整型
    Int,
    /// long长整型
    Long,
    /// enum枚举类型
    Enum,
    /// 指针
    Ptr,
    /// 函数
    Func,
    /// 数组
    Array,
    /// 结构体
    Struct,
    /// 联合体
    Union,
}

pub type TypeLink = Rc<RefCell<Type>>;

/// c语言类型
#[derive(Clone)]
pub struct Type {
    /// 种类
    pub(crate) kind: TypeKind,
    /// 名称
    pub(crate) name: Token,
    /// 名称字符串
    pub(crate) name_string: String,
    /// 大小, sizeof返回的值
    pub(crate) size: isize,
    /// 对齐
    pub(crate) align: isize,
    /// 指向的类型
    pub(crate) base: Option<TypeLink>,
    /// 返回的类型
    pub(crate) return_type: Option<TypeLink>,
    /// 形参
    pub(crate) params: Vec<TypeLink>,
    /// 数组长度, 元素总个数
    pub(crate) len: isize,
    /// 结构体
    pub(crate) members: Vec<Box<Member>>,
}

impl Type {
    /// 通过类型`kind`,大小`size`,对齐`align`来创建一个类型
    pub fn new(kind: TypeKind, size: isize, align: isize) -> Self {
        Self {
            kind,
            name: Token::Undefined,
            name_string: "".to_string(),
            size,
            align,
            base: None,
            return_type: None,
            params: vec![],
            len: 0,
            members: vec![],
        }
    }

    /// 创建一个void类型
    pub fn new_void() -> TypeLink {
        let type_ = Self::new(TypeKind::Void, 1, 1);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个bool类型
    pub fn new_bool() -> TypeLink {
        let type_ = Self::new(TypeKind::Bool, 1, 1);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个char类型
    pub fn new_char() -> TypeLink {
        let type_ = Self::new(TypeKind::Char, 1, 1);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个short类型
    pub fn new_short() -> TypeLink {
        let type_ = Self::new(TypeKind::Short, 2, 2);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个int类型
    pub fn new_int() -> TypeLink {
        let type_ = Self::new(TypeKind::Int, 4, 4);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个long类型
    pub fn new_long() -> TypeLink {
        let type_ = Self::new(TypeKind::Long, 8, 8);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个enum类型
    pub fn new_enum() -> TypeLink {
        let type_ = Self::new(TypeKind::Enum, 4, 4);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个指向`base`的指针类型
    pub fn pointer_to(base: TypeLink) -> TypeLink {
        let mut type_ = Self::new(TypeKind::Ptr, 8, 8);
        type_.base = Some(base);
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个返回值类型为`return_type`,形参列表为`params`的函数类型
    pub fn func_type(return_type: TypeLink, params: Vec<TypeLink>) -> TypeLink {
        let mut type_ = Self::new(TypeKind::Func, 8, 8);
        type_.return_type = Some(return_type);
        type_.params = params;
        Rc::new(RefCell::new(type_))
    }

    /// 创建一个长度为`len`的`base`类型的数组
    pub fn array_of(base: TypeLink, len: isize) -> TypeLink {
        let type_ = Self::array_of0(base, len);
        Rc::new(RefCell::new(type_))
    }

    pub fn array_of0(base: TypeLink, len: isize) -> Type {
        let size = base.borrow().size * len;
        let mut type_ = Self::new(TypeKind::Array, size, base.borrow().align);
        type_.base = Some(base);
        type_.len = len;
        type_
    }

    /// 创建一个union/struct类型
    /// 后续代码中会确定其为union还是struct
    pub fn new_union_struct() -> Type {
        let type_ = Self::new(TypeKind::Struct, 0, 1);
        type_
    }

    /// 是否是整数类型
    pub fn is_int(&self) -> bool {
        self.kind == TypeKind::Char
            || self.kind == TypeKind::Short
            || self.kind == TypeKind::Int
            || self.kind == TypeKind::Long
            || self.kind == TypeKind::Bool
            || self.kind == TypeKind::Enum
    }

    /// 是否含有基础类型
    pub fn has_base(&self) -> bool {
        self.kind == TypeKind::Ptr || self.kind == TypeKind::Array
    }

    /// 获取type的名称字符串
    pub fn get_name(&self) -> &str {
        self.name_string.as_str()
    }

    /// 通过`Token`设置type的`name`和`name_string`
    pub fn set_name(&mut self, token: Token) {
        self.name = token.clone();
        self.name_string = token.get_name();
    }

    /// 指针和数组类型获取基础类型的size
    pub fn get_base_size(&self) -> isize {
        if self.has_base() {
            return self.base.as_ref().unwrap().borrow().size;
        } else {
            0
        }
    }

    /// 获取函数类型的形参
    pub fn get_params(&self) -> &Vec<TypeLink> {
        &self.params
    }

    /// 根据`typ1`和`typ2`获取容纳左右部的类型
    pub fn get_common_type(typ1: TypeLink, typ2: TypeLink) -> TypeLink {
        if typ1.borrow().has_base() {
            return Self::pointer_to(typ1.borrow().base.as_ref().unwrap().clone());
        }
        if typ1.borrow().size == 8 || typ2.borrow().size == 8 {
            return Self::new_long();
        }

        Self::new_int()
    }
}

/// 进行常规的算术转换
pub fn usual_arith_conv(lhs: NodeLink, rhs: NodeLink) -> (NodeLink, NodeLink) {
    let typ = Type::get_common_type(
        lhs.type_.as_ref().unwrap().clone(),
        rhs.type_.as_ref().unwrap().clone(),
    );

    // 将左右部转换到兼容的类型
    let lhs = Node::new_cast(lhs, typ.clone());
    let rhs = Node::new_cast(rhs, typ);
    (lhs, rhs)
}

/// 为节点内的所有节点添加类型
pub fn add_type(node: &mut NodeLink) {
    // 判断 节点类型已经有值，那么就直接返回
    if node.get_type().is_some() {
        return;
    }

    // 递归访问所有节点以增加类型
    add_type_option(node.lhs.as_mut());
    add_type_option(node.rhs.as_mut());
    add_type_option(node.cond.as_mut());
    add_type_option(node.then.as_mut());
    add_type_option(node.els.as_mut());
    add_type_option(node.init.as_mut());
    add_type_option(node.inc.as_mut());

    // 访问body的所有节点以增加类型
    for i in 0..node.body.len() {
        add_type(&mut node.body[i])
    }
    // 访问所有参数节点`args`以增加类型
    for i in 0..node.args.len() {
        add_type(&mut node.args[i])
    }

    match node.kind {
        // 判断是否Val强制转换为int后依然完整，完整则用int否则用long
        NodeKind::Num => {
            let val = node.val;
            if val < i32::MIN as i64 || val > i32::MAX as i64 {
                node.type_ = Some(Type::new_long())
            } else {
                node.type_ = Some(Type::new_int())
            }
        }
        // 将节点类型设为 节点左部的类型
        NodeKind::Add
        | NodeKind::Sub
        | NodeKind::Mul
        | NodeKind::Div
        | NodeKind::Mod
        | NodeKind::BitAnd
        | NodeKind::BitOr
        | NodeKind::BitXor => {
            // 对左右部转换
            let (lhs, rhs) = usual_arith_conv(node.lhs.take().unwrap(), node.rhs.take().unwrap());
            node.lhs = Some(lhs);
            node.rhs = Some(rhs);
            node.type_ = node.lhs.as_ref().unwrap().type_.clone();
        }
        NodeKind::Neg => {
            // 对左部转换
            let typ = Type::get_common_type(
                Type::new_int(),
                node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone(),
            );
            node.lhs = Some(Node::new_cast(node.lhs.take().unwrap(), typ.clone()));
            node.type_ = Some(typ);
        }
        // 将节点类型设为 节点左部的类型
        // 左部不能是数组节点
        NodeKind::Assign => {
            let t = node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone();
            if t.borrow().kind == TypeKind::Array {
                let token = &node.token;
                error_token!(token, "not an lvalue");
                unreachable!()
            }
            if t.borrow().kind != TypeKind::Struct {
                node.rhs = Some(Node::new_cast(node.rhs.take().unwrap(), t.clone()))
            }
            node.type_ = Some(t);
        }
        // 将节点类型设为 int
        NodeKind::Eq | NodeKind::Ne | NodeKind::Lt | NodeKind::Le => {
            // 对左右部转换
            let (lhs, rhs) = usual_arith_conv(node.lhs.take().unwrap(), node.rhs.take().unwrap());
            node.lhs = Some(lhs);
            node.rhs = Some(rhs);
            node.type_ = Some(Type::new_int());
        }
        NodeKind::FuncCall => {
            node.type_ = Some(Type::new_long());
        }
        // 将节点类型设为 int
        NodeKind::Not | NodeKind::LogAnd | NodeKind::LogOr => {
            node.type_ = Some(Type::new_int());
        }
        // 将节点类型设为 左部的类型
        NodeKind::BitNot | NodeKind::Shl | NodeKind::Shr => {
            node.type_ = node.lhs.as_ref().unwrap().type_.clone();
        }
        // 将节点类型设为 变量的类型
        NodeKind::Var => {
            let var = &*node.var.as_ref().unwrap().clone();
            let vt = var.borrow().get_type().clone();
            node.type_ = Some(vt.clone());
        }
        // 如果:左或右部为void则为void，否则为二者兼容的类型
        NodeKind::Cond => {
            let then_t = node.then.as_ref().unwrap().type_.as_ref().unwrap().clone();
            let els_t = node.els.as_ref().unwrap().type_.as_ref().unwrap().clone();
            if then_t.borrow().kind == TypeKind::Void || els_t.borrow().kind == TypeKind::Void {
                node.type_ = Some(Type::new_void());
            } else {
                // 对左右部转换
                let (lhs, rhs) =
                    usual_arith_conv(node.then.take().unwrap(), node.els.take().unwrap());
                node.then = Some(lhs);
                node.els = Some(rhs);
                node.type_ = node.then.as_ref().unwrap().type_.clone();
            }
        }
        // 将节点类型设为 右部的类型
        NodeKind::Comma => {
            node.type_ = node.rhs.as_ref().unwrap().type_.clone();
        }
        // 将节点类型设为 成员的类型
        NodeKind::Member => {
            node.type_ = node.member.as_ref().unwrap().type_.clone();
        }
        // 将节点类型设为 指针，并指向左部的类型
        NodeKind::Addr => {
            let t = node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone();
            if t.borrow().kind == TypeKind::Array {
                node.type_ = Some(Type::pointer_to(t.borrow().base.as_ref().unwrap().clone()));
            } else {
                node.type_ = Some(Type::pointer_to(t.clone()));
            }
        }
        // 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则报错
        NodeKind::DeRef => {
            let lhs = node.lhs.as_ref().unwrap();
            let t = lhs.type_.as_ref().unwrap().clone();
            if t.borrow().has_base() {
                if t.borrow().kind == TypeKind::Void {
                    let token = node.lhs.as_ref().unwrap().get_token();
                    error_token!(token, "dereferencing a void pointer");
                    return;
                }
                node.type_ = Some(t.borrow().base.as_ref().unwrap().clone());
            } else {
                let token = node.lhs.as_ref().unwrap().get_token();
                error_token!(token, "invalid pointer dereference");
            }
        }
        // 节点类型为 最后的表达式语句的类型
        NodeKind::StmtExpr => {
            if node.body.len() > 0 {
                let last = node.body.last().unwrap();
                if last.kind == NodeKind::ExprStmt {
                    node.type_ = last.lhs.as_ref().unwrap().type_.clone();
                    return;
                }
            }
            error_token!(
                &node.token,
                "statement expression returning void is not supported"
            );
        }
        _ => {}
    }
}

/// 为节点内的所有节点添加类型
fn add_type_option(node: Option<&mut NodeLink>) {
    if node.is_some() {
        add_type(node.unwrap());
    }
}
