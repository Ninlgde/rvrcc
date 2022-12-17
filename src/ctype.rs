use crate::{error_token, Node};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // int 整型
    Int {
        // 名称
        name: String,
        // 大小, sizeof返回的值
        size: usize,
    },
    // char 字符
    Char {
        // 名称
        name: String,
        // 大小, sizeof返回的值
        size: usize,
    },
    // 指针类型
    Ptr {
        // 名称
        name: String,
        // 大小, sizeof返回的值
        size: usize,
        // 指向的类型
        base: Option<Box<Type>>,
    },
    Func {
        // 名称
        name: String,
        // 大小, sizeof返回的值
        size: usize,
        // 返回的类型
        return_type: Option<Box<Type>>,
        // 形参
        params: Vec<Type>,
    },
    Array {
        // 名称
        name: String,
        // 大小, sizeof返回的值
        size: usize,
        // 指向的类型
        base: Option<Box<Type>>,
        // 数组长度, 元素总个数
        len: usize,
    },
}

impl Type {
    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int {..}) || matches!(self, Type::Char {..})
    }

    pub fn has_base(&self) -> bool {
        matches!(self, Type::Ptr {..}) || matches!(self, Type::Array {..})
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Type::Func {..})
    }

    pub fn get_name(&self) -> &str {
        match self {
            Type::Int { name, .. } => name,
            Type::Char { name, .. } => name,
            Type::Ptr { name, .. } => name,
            Type::Func { name, .. } => name,
            Type::Array { name, .. } => name,
        }
    }

    pub fn set_name(&mut self, s: String) {
        match self {
            Type::Int { name, .. } => *name = s,
            Type::Char { name, .. } => *name = s,
            Type::Ptr { name, .. } => *name = s,
            Type::Func { name, .. } => *name = s,
            Type::Array { name, .. } => *name = s,
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Type::Int { size, .. } => *size,
            Type::Char { size, .. } => *size,
            Type::Ptr { size, .. } => *size,
            Type::Func { size, .. } => *size,
            Type::Array { size, .. } => *size,
        }
    }

    pub fn get_base_size(&self) -> usize {
        match self {
            Type::Ptr { base, .. }
            | Type::Array { base, .. } => {
                base.as_ref().unwrap().get_size()
            }
            _ => {
                0
            }
        }
    }

    pub fn add_param(&mut self, param: Type) {
        match self {
            Type::Func { params, .. } => {
                params.insert(0, param);
            }
            _ => ()
        }
    }

    pub fn get_params(&self) -> Vec<Type> {
        match self {
            Type::Func { params, .. } => params.to_vec(),
            _ => panic!("get params for error type")
        }
    }

    pub fn new_int() -> Box<Self> {
        Box::new(Type::Int { name: "".to_string(), size: 8 })
    }

    pub fn new_char() -> Box<Self> {
        Box::new(Type::Char { name: "".to_string(), size: 1 })
    }

    pub fn pointer_to(base: Box<Type>) -> Box<Self> {
        Box::new(Type::Ptr { name: String::new(), size: 8, base: Some(base) })
    }

    pub fn func_type(return_type: Box<Type>, params: Vec<Type>) -> Box<Self> {
        Box::new(Type::Func { name: String::new(), size: 8, return_type: Some(return_type), params })
    }

    pub fn array_of(base: Box<Type>, len: usize) -> Box<Self> {
        let size = base.get_size() * len;
        Box::new(Type::Array { name: String::new(), size, base: Some(base), len })
    }
}

pub fn add_type(node: &mut Node) {
    // 判断 节点类型已经有值，那么就直接返回
    if node.get_type().is_some() {
        return;
    }
    match node {
        // 将节点类型设为 节点左部的类型
        Node::Add { lhs, rhs, type_, .. }
        | Node::Sub { lhs, rhs, type_, .. }
        | Node::Mul { lhs, rhs, type_, .. }
        | Node::Div { lhs, rhs, type_, .. } => {
            add_type(lhs.as_mut().unwrap());
            add_type(rhs.as_mut().unwrap());
            *type_ = Some(lhs.as_mut().unwrap().get_type().as_ref().unwrap().clone());
        }
        Node::Assign { lhs, rhs, type_, .. } => {
            add_type(lhs.as_mut().unwrap());
            add_type(rhs.as_mut().unwrap());
            let t = lhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
            match *t {
                // 左部不能是数组节点
                Type::Array { .. } => {
                    let token = lhs.as_ref().unwrap().get_token();
                    error_token!(token, "not an lvalue");
                }
                _ => {}
            }
            *type_ = Some(lhs.as_mut().unwrap().get_type().as_ref().unwrap().clone());
        }
        Node::Neg { unary, type_, .. } => {
            add_type(unary.as_mut().unwrap());
            *type_ = Some(unary.as_mut().unwrap().get_type().as_ref().unwrap().clone());
        }
        // 将节点类型设为 int
        Node::Eq { lhs, rhs, type_, .. }
        | Node::Ne { lhs, rhs, type_, .. }
        | Node::Lt { lhs, rhs, type_, .. }
        | Node::Le { lhs, rhs, type_, .. } => {
            add_type(lhs.as_mut().unwrap());
            add_type(rhs.as_mut().unwrap());
            *type_ = Some(Type::new_int());
        }
        Node::Num { type_, .. } => {
            *type_ = Some(Type::new_int())
        }
        Node::FuncCall { type_, args, .. } => {
            for node in args {
                add_type(node);
            }
            *type_ = Some(Type::new_int())
        }
        Node::Var { type_, var, .. } => {
            let var = var.as_ref().unwrap().borrow();
            let vt = var.get_type().as_ref().clone();
            *type_ = Some(Box::new(vt));
        }
        // 将节点类型设为 指针，并指向左部的类型
        Node::Addr { unary, type_, .. } => {
            add_type(unary.as_mut().unwrap());
            let unary_t = unary.as_ref().unwrap().get_type().as_ref().unwrap().clone();
            match *unary_t {
                Type::Array { base, .. } => {
                    *type_ = Some(Type::pointer_to(base.unwrap()))
                }
                _ => {
                    *type_ = Some(Type::pointer_to(unary_t.clone()));
                }
            }
        }
        // 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则为int
        Node::DeRef { unary, type_, .. } => {
            add_type(unary.as_mut().unwrap());
            let unary_t = unary.as_ref().unwrap().get_type().as_ref().unwrap().clone();
            match *unary_t {
                Type::Ptr { base, .. }
                | Type::Array { base, .. } => {
                    *type_ = Some(base.unwrap().clone());
                }
                _ => {
                    let token = unary.as_ref().unwrap().get_token();
                    error_token!(token, "invalid pointer dereference");
                }
            }
        }
        // 其他节点, 只递归设置孩子
        Node::Return { unary, .. }
        | Node::ExprStmt { unary, .. } => {
            add_type(unary.as_mut().unwrap());
        }
        Node::StmtExpr { token, body, type_, .. } => {
            let last = body.last().unwrap().clone();
            for node in body {
                add_type(node);
            }
            match last {
                Node::ExprStmt { unary, .. } => {
                    let t = unary.as_ref().unwrap().get_type().as_ref().unwrap().clone();
                    *type_ = Some(t);
                }
                _ => {
                    error_token!(token, "statement expression returning void is not supported");
                }
            }
        }
        Node::If { cond, then, els, .. } => {
            add_type(cond.as_mut().unwrap());
            add_type(then.as_mut().unwrap());
            if els.is_some() {
                add_type(els.as_mut().unwrap());
            }
        }
        Node::For { init, inc, cond, then, .. } => {
            if cond.is_some() {
                add_type(cond.as_mut().unwrap());
            }
            add_type(then.as_mut().unwrap());
            if init.is_some() {
                add_type(init.as_mut().unwrap());
            }
            if inc.is_some() {
                add_type(inc.as_mut().unwrap());
            }
        }
        Node::Block { body, .. } => {
            for node in body {
                add_type(node);
            }
        }
    }
}