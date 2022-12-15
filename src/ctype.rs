use crate::Node;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // int 整型
    Int {},
    // 指针类型
    Ptr {
        // 指向的类型
        base: Option<Box<Type>>,
    },
}

impl Type {
    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int {..})
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr { .. })
    }

    pub fn pointer_to(base: Box<Type>) -> Box<Self> {
        Box::new(Type::Ptr { base: Some(base) })
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
        | Node::Div { lhs, rhs, type_, .. }
        | Node::Assign { lhs, rhs, type_, .. } => {
            add_type(lhs.as_mut().unwrap());
            add_type(rhs.as_mut().unwrap());
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
            *type_ = Some(Box::new(Type::Int {}));
        }
        Node::Var { type_, .. }
        | Node::Num { type_, .. } => {
            *type_ = Some(Box::new(Type::Int {}))
        }
        // 将节点类型设为 指针，并指向左部的类型
        Node::Addr { unary, type_, .. } => {
            add_type(unary.as_mut().unwrap());
            *type_ = Some(Type::pointer_to(unary.as_mut().unwrap().get_type().as_ref().unwrap().clone()));
        }
        // 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则为int
        Node::DeRef { unary, type_, .. } => {
            add_type(unary.as_mut().unwrap());
            let unary_t = unary.as_mut().unwrap().get_type().as_ref().unwrap().clone();
            match unary_t.as_ref() {
                Type::Ptr { .. } => {
                    *type_ = Some(unary_t);
                }
                Type::Int { .. } => {
                    *type_ = Some(Box::new(Type::Int {}));
                }
            }
        }
        // 其他节点, 只递归设置孩子
        Node::Return { unary, .. }
        | Node::ExprStmt { unary, .. } => {
            add_type(unary.as_mut().unwrap());
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