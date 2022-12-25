//! 初始化器

use crate::{add_with_type, Node, NodeKind, NodeLink, ObjLink, Token, TypeKind, TypeLink};

/// 可变的初始化器。此处为树状结构。
/// 因为初始化器可以是嵌套的，
/// 类似于 int x[2][2] = {{1, 2}, {3, 4}} ，
#[derive(Clone)]
pub struct Initializer<'a> {
    // 原始类型
    pub(crate) typ: Option<TypeLink>,
    // 终结符
    #[allow(dead_code)]
    pub(crate) token: Option<&'a Token>,
    // 如果不是聚合类型，并且有一个初始化器，Expr 有对应的初始化表达式。
    pub(crate) expr: Option<NodeLink>,
    // 如果是聚合类型（如数组或结构体），Children有子节点的初始化器
    pub(crate) children: Vec<Box<Initializer<'a>>>,
}

impl<'a> Initializer<'a> {
    pub fn new(typ: TypeLink) -> Box<Self> {
        let mut init = Box::new(Initializer {
            typ: Some(typ.clone()),
            token: None,
            expr: None,
            children: vec![],
        });

        let t = typ.borrow();
        if t.kind == TypeKind::Array {
            for _ in 0..t.len {
                init.children
                    .push(Initializer::new(t.base.as_ref().unwrap().clone()))
            }
        }

        init
    }
}

/// 指派初始化，用于局部变量的初始化器
#[derive(Clone)]
pub struct InitDesig {
    // 下一个
    next: Option<Box<InitDesig>>,
    // 数组中的索引
    idx: isize,
    // 对应的变量
    var: Option<ObjLink>,
}

impl InitDesig {
    pub fn new() -> Box<Self> {
        Box::new(InitDesig {
            next: None,
            idx: 0,
            var: None,
        })
    }

    pub fn new_with_var(var: ObjLink, idx: isize) -> Box<Self> {
        let mut desig = Self::new();
        desig.var = Some(var);
        desig.idx = idx;
        desig
    }

    pub fn new_with_next(next: Box<InitDesig>, idx: isize) -> Box<Self> {
        let mut desig = Self::new();
        desig.next = Some(next);
        desig.idx = idx;
        desig
    }
}

/// 创建局部变量的初始化
pub fn create_lvar_init(
    init: Box<Initializer>,
    typ: TypeLink,
    desig: Box<InitDesig>,
    token: Token,
) -> Option<NodeLink> {
    let t = typ.borrow();
    if t.kind == TypeKind::Array {
        // 预备空表达式的情况
        let mut node = Node::new(NodeKind::NullExpr, token.clone());
        for i in 0..t.len {
            // 这里next指向了上一级Desig的信息，以及在其中的偏移量。
            let id = InitDesig::new_with_next(desig.clone(), i);
            // 局部变量进行初始化
            let rhs = create_lvar_init(
                init.children[i as usize].clone(),
                t.base.as_ref().unwrap().clone(),
                id,
                token.clone(),
            )
            .unwrap();
            // 构造一个形如：NULL_EXPR，EXPR1，EXPR2…的二叉树
            node = Node::new_binary(NodeKind::Comma, node, rhs, token.clone());
        }
        return Some(node);
    }

    // 如果需要作为右值的表达式为空，则设为空表达式
    if init.expr.is_none() {
        return Some(Node::new(NodeKind::NullExpr, token.clone()));
    }
    // 变量等可以直接赋值的左值
    let lhs = init_desig_expr(desig, token.clone()).unwrap();
    let rhs = init.expr.unwrap();
    return Some(Node::new_binary(NodeKind::Assign, lhs, rhs, token.clone()));
}

/// 指派初始化表达式
pub fn init_desig_expr(desig: Box<InitDesig>, token: Token) -> Option<NodeLink> {
    // 返回Desig中的变量
    if desig.var.is_some() {
        return Some(Node::new_var(
            desig.var.as_ref().unwrap().clone(),
            token.clone(),
        ));
    }

    // 需要赋值的变量名
    // 递归到次外层Desig，有此时最外层有Desig->Var
    // 然后逐层计算偏移量
    let lhs = init_desig_expr(desig.next.unwrap(), token.clone()).unwrap();
    // 偏移量
    let rhs = Node::new_num(desig.idx as i64, token.clone());
    // 返回偏移后的变量地址
    let unary = add_with_type(lhs, rhs, token.clone()).unwrap();
    return Some(Node::new_unary(NodeKind::DeRef, unary, token.clone()));
}
