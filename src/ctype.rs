use crate::node::NodeKind;
use crate::obj::Member;
use crate::{error_token, Node};

#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
    Void,
    Char,
    Short,
    Int,
    Long,
    Ptr,
    Func,
    Array,
    Struct,
    Union,
}

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
    // 名称
    pub name: String,
    // 大小, sizeof返回的值
    pub size: usize,
    // 对齐
    pub align: usize,
    // 指向的类型
    base: Option<Box<Type>>,
    // 返回的类型
    return_type: Option<Box<Type>>,
    // 形参
    params: Vec<Type>,
    // 数组长度, 元素总个数
    len: usize,
    // 结构体
    pub members: Vec<Member>,
}

impl Type {
    pub fn new(kind: TypeKind, size: usize, align: usize) -> Self {
        Self {
            kind,
            name: String::new(),
            size,
            align,
            base: None,
            return_type: None,
            params: vec![],
            len: 0,
            members: vec![],
        }
    }

    pub fn new_void() -> Box<Self> {
        let type_ = Self::new(TypeKind::Void, 1, 1);
        Box::new(type_)
    }

    pub fn new_char() -> Box<Self> {
        let type_ = Self::new(TypeKind::Char, 1, 1);
        Box::new(type_)
    }

    pub fn new_short() -> Box<Self> {
        let type_ = Self::new(TypeKind::Short, 2, 2);
        Box::new(type_)
    }

    pub fn new_int() -> Box<Self> {
        let type_ = Self::new(TypeKind::Int, 4, 4);
        Box::new(type_)
    }

    pub fn new_long() -> Box<Self> {
        let type_ = Self::new(TypeKind::Long, 8, 8);
        Box::new(type_)
    }

    pub fn pointer_to(base: Box<Type>) -> Box<Self> {
        let mut type_ = Self::new(TypeKind::Ptr, 8, 8);
        type_.base = Some(base);
        Box::new(type_)
    }

    pub fn func_type(return_type: Box<Type>, params: Vec<Type>) -> Box<Self> {
        let mut type_ = Self::new(TypeKind::Func, 8, 8);
        type_.return_type = Some(return_type);
        type_.params = params;
        Box::new(type_)
    }

    pub fn array_of(base: Box<Type>, len: usize) -> Box<Self> {
        let size = base.get_size() * len;
        let mut type_ = Self::new(TypeKind::Array, size, base.align);
        type_.base = Some(base);
        type_.len = len;
        Box::new(type_)
    }

    pub fn new_union_struct() -> Box<Self> {
        let type_ = Self::new(TypeKind::Struct, 0, 0);
        Box::new(type_)
    }

    pub fn is_int(&self) -> bool {
        self.kind == TypeKind::Char
            || self.kind == TypeKind::Short
            || self.kind == TypeKind::Int
            || self.kind == TypeKind::Long
    }

    pub fn has_base(&self) -> bool {
        self.kind == TypeKind::Ptr || self.kind == TypeKind::Array
    }

    pub fn is_func(&self) -> bool {
        self.kind == TypeKind::Func
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn set_name(&mut self, s: String) {
        self.name = s;
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_base_size(&self) -> usize {
        if self.has_base() {
            return self.base.as_ref().unwrap().get_size();
        } else {
            0
        }
    }

    pub fn add_param(&mut self, param: Type) {
        self.params.insert(0, param);
    }

    pub fn get_params(&self) -> Vec<Type> {
        self.params.to_vec()
    }

    pub fn get_common_type(typ1: Box<Self>, typ2: Box<Self>) -> Box<Self> {
        if typ1.has_base() {
            return Self::pointer_to(typ1.base.unwrap());
        }
        if typ1.size == 8 || typ2.size == 8 {
            return Self::new_long();
        }

        Self::new_int()
    }
}

pub fn usual_arith_conv(lhs: &mut Box<Node>, rhs: &mut Box<Node>) {
    let typ = Type::get_common_type(lhs.type_.clone().unwrap(), rhs.type_.clone().unwrap());

    *lhs = Box::new(Node::new_cast(lhs.clone(), typ.clone()));
    *rhs = Box::new(Node::new_cast(rhs.clone(), typ));
}

pub fn add_type(node: &mut Node) {
    // 判断 节点类型已经有值，那么就直接返回
    if node.get_type().is_some() {
        return;
    }

    if node.lhs.is_some() {
        add_type(node.lhs.as_mut().unwrap());
    }
    if node.rhs.is_some() {
        add_type(node.rhs.as_mut().unwrap());
    }
    if node.cond.is_some() {
        add_type(node.cond.as_mut().unwrap());
    }
    if node.then.is_some() {
        add_type(node.then.as_mut().unwrap());
    }
    if node.els.is_some() {
        add_type(node.els.as_mut().unwrap());
    }
    if node.init.is_some() {
        add_type(node.init.as_mut().unwrap());
    }
    if node.inc.is_some() {
        add_type(node.inc.as_mut().unwrap());
    }

    for i in 0..node.body.len() {
        add_type(&mut node.body[i])
    }
    for i in 0..node.args.len() {
        add_type(&mut node.args[i])
    }

    match node.kind {
        NodeKind::Num => {
            let val = node.val;
            if val < i32::MIN as i64 || val > i32::MAX as i64 {
                node.type_ = Some(Type::new_long())
            } else {
                node.type_ = Some(Type::new_int())
            }
        }
        NodeKind::Add | NodeKind::Sub | NodeKind::Mul | NodeKind::Div => {
            // 对左右部转换
            usual_arith_conv(node.lhs.as_mut().unwrap(), node.rhs.as_mut().unwrap());
            node.type_ = node.lhs.as_ref().unwrap().type_.clone();
        }
        NodeKind::Neg => {
            // 对左部转换
            let typ = Type::get_common_type(
                Type::new_int(),
                node.lhs.as_ref().unwrap().type_.clone().unwrap(),
            );
            node.lhs = Some(Box::new(Node::new_cast(
                node.lhs.as_ref().unwrap().clone(),
                typ.clone(),
            )));
            node.type_ = Some(typ);
        }
        NodeKind::Assign => {
            let t = node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone();
            if t.kind == TypeKind::Array {
                let token = &node.token;
                error_token!(token, "not an lvalue");
                unreachable!()
            }
            if t.kind != TypeKind::Struct {
                node.rhs = Some(Box::new(Node::new_cast(
                    node.rhs.as_ref().unwrap().clone(),
                    t.clone(),
                )))
            }
            node.type_ = Some(t);
        }
        NodeKind::Eq | NodeKind::Ne | NodeKind::Lt | NodeKind::Le => {
            // 对左右部转换
            usual_arith_conv(node.lhs.as_mut().unwrap(), node.rhs.as_mut().unwrap());
            node.type_ = Some(Type::new_int());
        }
        NodeKind::FuncCall => {
            node.type_ = Some(Type::new_long());
        }
        NodeKind::Var => {
            let var = &*node.var.as_ref().unwrap().clone();
            let vt = *var.borrow().get_type().clone();
            node.type_ = Some(Box::new(vt));
        }
        NodeKind::Comma => {
            node.type_ = node.rhs.as_ref().unwrap().type_.clone();
        }
        NodeKind::Member => {
            node.type_ = node.member.as_ref().unwrap().type_.clone();
        }
        NodeKind::Addr => {
            let t = node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone();
            if t.kind == TypeKind::Array {
                node.type_ = Some(Type::pointer_to(t.base.unwrap()));
            } else {
                node.type_ = Some(Type::pointer_to(t.clone()));
            }
        }
        NodeKind::DeRef => {
            let lhs = node.lhs.as_ref().unwrap();
            let t = lhs.type_.as_ref().unwrap().clone();
            if t.has_base() {
                if t.kind == TypeKind::Void {
                    let token = node.lhs.as_ref().unwrap().get_token();
                    error_token!(token, "dereferencing a void pointer");
                    return;
                }
                node.type_ = Some(t.base.unwrap());
            } else {
                let token = node.lhs.as_ref().unwrap().get_token();
                error_token!(token, "invalid pointer dereference");
            }
        }
        NodeKind::StmtExpr => {
            if node.body.len() > 0 {
                let last = node.body.last().unwrap().clone();
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
