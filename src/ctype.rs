use crate::{error_token, Node};
use crate::node::NodeKind;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Int,
    Char,
    Ptr,
    Func,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub kind: TypeKind,
    // 名称
    pub name: String,
    // 大小, sizeof返回的值
    pub size: usize,
    // 指向的类型
    base: Option<Box<Type>>,
    // 返回的类型
    return_type: Option<Box<Type>>,
    // 形参
    params: Vec<Type>,
    // 数组长度, 元素总个数
    len: usize,
}

impl Type {
    pub fn new(kind: TypeKind, size: usize) -> Self {
        Self {
            kind,
            name: String::new(),
            size,
            base: None,
            return_type: None,
            params: vec![],
            len: 0,
        }
    }

    pub fn new_int() -> Box<Self> {
        let type_ = Self::new(TypeKind::Int, 8);
        Box::new(type_)
    }

    pub fn new_char() -> Box<Self> {
        let type_ = Self::new(TypeKind::Char, 1);
        Box::new(type_)
    }

    pub fn pointer_to(base: Box<Type>) -> Box<Self> {
        let mut type_ = Self::new(TypeKind::Ptr, 8);
        type_.base = Some(base);
        Box::new(type_)
    }

    pub fn func_type(return_type: Box<Type>, params: Vec<Type>) -> Box<Self> {
        let mut type_ = Self::new(TypeKind::Func, 8);
        type_.return_type = Some(return_type);
        type_.params = params;
        Box::new(type_)
    }

    pub fn array_of(base: Box<Type>, len: usize) -> Box<Self> {
        let size = base.get_size() * len;
        let mut type_ = Self::new(TypeKind::Array, size);
        type_.base = Some(base);
        type_.len = len;
        Box::new(type_)
    }

    pub fn is_int(&self) -> bool {
        self.kind == TypeKind::Int || self.kind == TypeKind::Char
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
}

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Type {
//     // int 整型
//     Int {
//         // 名称
//         name: String,
//         // 大小, sizeof返回的值
//         size: usize,
//     },
//     // char 字符
//     Char {
//         // 名称
//         name: String,
//         // 大小, sizeof返回的值
//         size: usize,
//     },
//     // 指针类型
//     Ptr {
//         // 名称
//         name: String,
//         // 大小, sizeof返回的值
//         size: usize,
//         // 指向的类型
//         base: Option<Box<Type>>,
//     },
//     Func {
//         // 名称
//         name: String,
//         // 大小, sizeof返回的值
//         size: usize,
//         // 返回的类型
//         return_type: Option<Box<Type>>,
//         // 形参
//         params: Vec<Type>,
//     },
//     Array {
//         // 名称
//         name: String,
//         // 大小, sizeof返回的值
//         size: usize,
//         // 指向的类型
//         base: Option<Box<Type>>,
//         // 数组长度, 元素总个数
//         len: usize,
//     },
// }
//
// impl Type {
//     pub fn is_int(&self) -> bool {
//         matches!(self, Type::Int {..}) || matches!(self, Type::Char {..})
//     }
//
//     pub fn has_base(&self) -> bool {
//         matches!(self, Type::Ptr {..}) || matches!(self, Type::Array {..})
//     }
//
//     pub fn is_func(&self) -> bool {
//         matches!(self, Type::Func {..})
//     }
//
//     pub fn get_name(&self) -> &str {
//         match self {
//             Type::Int { name, .. } => name,
//             Type::Char { name, .. } => name,
//             Type::Ptr { name, .. } => name,
//             Type::Func { name, .. } => name,
//             Type::Array { name, .. } => name,
//         }
//     }
//
//     pub fn set_name(&mut self, s: String) {
//         match self {
//             Type::Int { name, .. } => *name = s,
//             Type::Char { name, .. } => *name = s,
//             Type::Ptr { name, .. } => *name = s,
//             Type::Func { name, .. } => *name = s,
//             Type::Array { name, .. } => *name = s,
//         }
//     }
//
//     pub fn get_size(&self) -> usize {
//         match self {
//             Type::Int { size, .. } => *size,
//             Type::Char { size, .. } => *size,
//             Type::Ptr { size, .. } => *size,
//             Type::Func { size, .. } => *size,
//             Type::Array { size, .. } => *size,
//         }
//     }
//
//     pub fn get_base_size(&self) -> usize {
//         match self {
//             Type::Ptr { base, .. }
//             | Type::Array { base, .. } => {
//                 base.as_ref().unwrap().get_size()
//             }
//             _ => {
//                 0
//             }
//         }
//     }
//
//     pub fn add_param(&mut self, param: Type) {
//         match self {
//             Type::Func { params, .. } => {
//                 params.insert(0, param);
//             }
//             _ => ()
//         }
//     }
//
//     pub fn get_params(&self) -> Vec<Type> {
//         match self {
//             Type::Func { params, .. } => params.to_vec(),
//             _ => panic!("get params for error type")
//         }
//     }
//
//     pub fn new_int() -> Box<Self> {
//         Box::new(Type::Int { name: "".to_string(), size: 8 })
//     }
//
//     pub fn new_char() -> Box<Self> {
//         Box::new(Type::Char { name: "".to_string(), size: 1 })
//     }
//
//     pub fn pointer_to(base: Box<Type>) -> Box<Self> {
//         Box::new(Type::Ptr { name: String::new(), size: 8, base: Some(base) })
//     }
//
//     pub fn func_type(return_type: Box<Type>, params: Vec<Type>) -> Box<Self> {
//         Box::new(Type::Func { name: String::new(), size: 8, return_type: Some(return_type), params })
//     }
//
//     pub fn array_of(base: Box<Type>, len: usize) -> Box<Self> {
//         let size = base.get_size() * len;
//         Box::new(Type::Array { name: String::new(), size, base: Some(base), len })
//     }
// }

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
        NodeKind::Add
        | NodeKind::Sub
        | NodeKind::Mul
        | NodeKind::Div
        | NodeKind::Neg => {
            node.type_ = node.lhs.as_ref().unwrap().type_.clone();
        }
        NodeKind::Assign => {
            let t = node.lhs.as_ref().unwrap().type_.as_ref().unwrap().clone();
            if t.kind == TypeKind::Array {
                let token = &node.token;
                error_token!(token, "not an lvalue");
            }
            node.type_ = node.lhs.as_ref().unwrap().type_.clone();
        }
        NodeKind::Eq
        | NodeKind::Ne
        | NodeKind::Lt
        | NodeKind::Le
        | NodeKind::Num
        | NodeKind::FuncCall => {
            node.type_ = Some(Type::new_int());
        }
        NodeKind::Var => {
            let var = &*node.var.as_ref().unwrap().clone();
            let vt = *var.borrow().get_type().clone();
            node.type_ = Some(Box::new(vt));
        }
        NodeKind::Comma => {
            node.type_ = node.rhs.as_ref().unwrap().type_.clone();
        }
        NodeKind::Addr => {
            let t = node.lhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
            if t.kind == TypeKind::Array {
                node.type_ = Some(Type::pointer_to(t.base.unwrap()));
            } else {
                node.type_ = Some(Type::pointer_to(t.clone()));
            }
        }
        NodeKind::DeRef => {
            let t = node.lhs.as_ref().unwrap().get_type().as_ref().unwrap().clone();
            if t.has_base() {
                node.type_ = Some(t.base.unwrap());
            } else {
                let token = node.lhs.as_ref().unwrap().get_token();
                error_token!(token, "invalid pointer dereference");
            }
        }
        NodeKind::StmtExpr => {
            let last = node.body.last().unwrap().clone();
            if last.kind == NodeKind::ExprStmt {
                node.type_ = last.lhs.as_ref().unwrap().type_.clone();
            } else {
                error_token!(&node.token, "statement expression returning void is not supported");
            }
        }
        _ => {}
    }
}