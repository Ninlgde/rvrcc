//! 初始化器

use crate::ctype::{TypeKind, TypeLink};
use crate::node::{add_with_type, Node, NodeKind, NodeLink};
use crate::obj::{Member, ObjLink};
use crate::token::Token;

/// 可变的初始化器。此处为树状结构。
/// 因为初始化器可以是嵌套的，
/// 类似于 int x[2][2] = {{1, 2}, {3, 4}} ，
#[derive(Clone)]
pub struct Initializer<'a> {
    /// 原始类型
    pub(crate) typ: Option<TypeLink>,
    /// 终结符
    #[allow(dead_code)]
    pub(crate) token: Option<&'a Token>,
    /// 如果不是聚合类型，并且有一个初始化器，Expr 有对应的初始化表达式。
    pub(crate) expr: Option<NodeLink>,
    /// 如果是聚合类型（如数组或结构体），Children有子节点的初始化器
    pub(crate) children: Vec<Box<Initializer<'a>>>,
    /// 可调整的，表示需要重新构造
    pub(crate) is_flexible: bool,
}

impl<'a> Initializer<'a> {
    pub fn new(typ: TypeLink, is_flexible: bool) -> Box<Self> {
        let mut init = Box::new(Initializer {
            typ: Some(typ.clone()),
            token: None,
            expr: None,
            children: vec![],
            is_flexible: false,
        });

        let t = typ.borrow();
        if t.kind == TypeKind::Array {
            // 判断是否需要调整数组元素数并且数组不完整
            if is_flexible && t.size < 0 {
                // 设置初始化器为可调整的，之后进行完数组元素数的计算后，再构造初始化器
                init.is_flexible = true;
                return init;
            }
            for _ in 0..t.len {
                init.children
                    .push(Initializer::new(t.base.as_ref().unwrap().clone(), false))
            }
        }

        if t.kind == TypeKind::Struct {
            // let len = t.members.len();
            // let children: Vec<Box<Initializer<'a>>> = Vec::with_capacity(len);
            for member in t.members.iter() {
                init.children.push(Initializer::new(
                    member.type_.as_ref().unwrap().clone(),
                    false,
                ))
            }
        }

        init
    }

    /// 修改init的type
    pub fn replace_type(&mut self, typ: TypeLink) {
        typ.borrow_mut()
            .set_name(self.typ.as_ref().unwrap().borrow().clone().name);
        self.typ = Some(typ.clone());

        // 根据新类型判断是否重新添加children
        let t = typ.borrow();
        if t.kind == TypeKind::Array {
            for _ in 0..t.len {
                self.children
                    .push(Initializer::new(t.base.as_ref().unwrap().clone(), false))
            }
        }
    }
}

/// 指派初始化，用于局部变量的初始化器
#[derive(Clone)]
pub struct InitDesig {
    /// 下一个
    next: Option<Box<InitDesig>>,
    /// 数组中的索引
    idx: usize,
    /// 对应的变量
    var: Option<ObjLink>,
    /// 成员变量
    member: Option<Box<Member>>,
}

impl InitDesig {
    pub fn new() -> Box<Self> {
        Box::new(InitDesig {
            next: None,
            idx: 0,
            var: None,
            member: None,
        })
    }

    /// 根据var创建
    pub fn new_with_var(var: ObjLink) -> Box<Self> {
        let mut desig = Self::new();
        desig.var = Some(var);
        desig
    }

    /// 根据next和idx创建
    pub fn new_with_next(
        next: Box<InitDesig>,
        idx: usize,
        member: Option<Box<Member>>,
    ) -> Box<Self> {
        let mut desig = Self::new();
        desig.next = Some(next);
        desig.idx = idx;
        desig.member = member;
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
        for i in 0..t.len as usize {
            // 这里next指向了上一级Desig的信息，以及在其中的偏移量。
            let id = InitDesig::new_with_next(desig.clone(), i, None);
            // 局部变量进行初始化
            let rhs = create_lvar_init(
                init.children[i].clone(),
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

    if t.kind == TypeKind::Struct {
        // 构造结构体的初始化器结构
        let mut node = Node::new(NodeKind::NullExpr, token.clone());

        for member in t.members.iter() {
            let id = InitDesig::new_with_next(desig.clone(), member.idx, Some(member.clone()));
            let rhs = create_lvar_init(
                init.children[member.idx].clone(),
                member.type_.as_ref().unwrap().clone(),
                id,
                token.clone(),
            )
            .unwrap();
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

    if desig.member.is_some() {
        let lhs = init_desig_expr(desig.next.unwrap(), token.clone()).unwrap();
        let mut node = Node::new_unary(NodeKind::Member, lhs, token.clone());
        node.member = Some(desig.member.as_ref().unwrap().clone());
        return Some(node);
    }

    // 需要赋值的变量名
    // 递归到次外层Desig，有此时最外层有Desig->Var或者Desig->Mem
    // 然后逐层计算偏移量
    let lhs = init_desig_expr(desig.next.unwrap(), token.clone()).unwrap();
    // 偏移量
    let rhs = Node::new_num(desig.idx as i64, token.clone());
    // 返回偏移后的变量地址
    let unary = add_with_type(lhs, rhs, token.clone()).unwrap();
    return Some(Node::new_unary(NodeKind::DeRef, unary, token.clone()));
}
