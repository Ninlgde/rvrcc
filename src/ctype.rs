//! C语言的类型实现

use crate::node::{Node, NodeKind, NodeLink};
use crate::obj::Member;
use crate::token::Token;
use crate::{error_token, FP_MAX, GP_MAX};
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
    /// float类型
    Float,
    /// double类型
    Double,
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
    // 名称位置
    pub(crate) name_pos: Token,
    /// 大小, sizeof返回的值
    pub(crate) size: isize,
    /// 对齐
    pub(crate) align: isize,
    /// 是否为无符号的
    pub(crate) is_unsigned: bool,
    /// 指向的类型
    pub(crate) base: Option<TypeLink>,
    /// 返回的类型
    pub(crate) return_type: Option<TypeLink>,
    /// 形参
    pub(crate) params: Vec<TypeLink>,
    /// 是否为可变参数
    pub(crate) is_variadic: bool,
    /// 数组长度, 元素总个数
    pub(crate) len: isize,
    /// 结构体
    pub(crate) members: Vec<Box<Member>>,
    /// 是否为灵活的，表示需要重新构造
    pub(crate) is_flexible: bool,
    /// 浮点结构体的对应寄存器
    pub(crate) fs_reg1ty: Option<TypeLink>,
    /// 浮点结构体的对应寄存器
    pub(crate) fs_reg2ty: Option<TypeLink>,
}

impl Type {
    /// 通过类型`kind`,大小`size`,对齐`align`来创建一个类型
    fn new(kind: TypeKind, size: isize, align: isize) -> Self {
        Self {
            kind,
            name: Token::null(),
            name_string: "".to_string(),
            name_pos: Token::null(),
            size,
            align,
            is_unsigned: false,
            base: None,
            return_type: None,
            params: vec![],
            is_variadic: false,
            len: 0,
            members: vec![],
            is_flexible: false,
            fs_reg1ty: None,
            fs_reg2ty: None,
        }
    }

    /// 创建无符号的某种类型
    fn new_unsigned(kind: TypeKind, size: isize, align: isize) -> Self {
        let mut typ = Self::new(kind, size, align);
        typ.is_unsigned = true;
        typ
    }

    /// 创建一个void类型
    pub fn new_void() -> TypeLink {
        let typ = Self::new(TypeKind::Void, 1, 1);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个bool类型
    pub fn new_bool() -> TypeLink {
        let typ = Self::new(TypeKind::Bool, 1, 1);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个char类型
    pub fn new_char() -> TypeLink {
        let typ = Self::new(TypeKind::Char, 1, 1);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个short类型
    pub fn new_short() -> TypeLink {
        let typ = Self::new(TypeKind::Short, 2, 2);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个int类型
    pub fn new_int() -> TypeLink {
        let typ = Self::new(TypeKind::Int, 4, 4);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个long类型
    pub fn new_long() -> TypeLink {
        let typ = Self::new(TypeKind::Long, 8, 8);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个unsigned char类型
    pub fn new_unsigned_char() -> TypeLink {
        let typ = Self::new_unsigned(TypeKind::Char, 1, 1);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个unsigned short类型
    pub fn new_unsigned_short() -> TypeLink {
        let typ = Self::new_unsigned(TypeKind::Short, 2, 2);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个unsigned int类型
    pub fn new_unsigned_int() -> TypeLink {
        let typ = Self::new_unsigned(TypeKind::Int, 4, 4);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个unsigned long类型
    pub fn new_unsigned_long() -> TypeLink {
        let typ = Self::new_unsigned(TypeKind::Long, 8, 8);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个float类型
    pub fn new_float() -> TypeLink {
        let typ = Self::new(TypeKind::Float, 4, 4);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个double类型
    pub fn new_double() -> TypeLink {
        let typ = Self::new(TypeKind::Double, 8, 8);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个enum类型
    pub fn new_enum() -> TypeLink {
        let typ = Self::new(TypeKind::Enum, 4, 4);
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个指向`base`的指针类型
    pub fn pointer_to(base: TypeLink) -> TypeLink {
        let mut typ = Self::new(TypeKind::Ptr, 8, 8);
        typ.base = Some(base);
        // 将指针作为无符号类型进行比较
        typ.is_unsigned = true;
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个返回值类型为`return_type`,形参列表为`params`的函数类型
    pub fn func_type(return_type: TypeLink, params: Vec<TypeLink>, is_variadic: bool) -> TypeLink {
        let mut typ = Self::new(TypeKind::Func, 8, 8);
        typ.return_type = Some(return_type);
        typ.params = params;
        typ.is_variadic = is_variadic;
        Rc::new(RefCell::new(typ))
    }

    /// 创建一个长度为`len`的`base`类型的数组
    pub fn array_of(base: TypeLink, len: isize) -> TypeLink {
        let typ = Self::array_of0(base, len);
        Rc::new(RefCell::new(typ))
    }

    pub fn array_of0(base: TypeLink, len: isize) -> Type {
        let size = base.borrow().size * len;
        let mut typ = Self::new(TypeKind::Array, size, base.borrow().align);
        typ.base = Some(base);
        typ.len = len;
        typ
    }

    /// 创建一个union/struct类型
    /// 后续代码中会确定其为union还是struct
    pub fn new_union_struct() -> Type {
        let mut typ = Self::new(TypeKind::Struct, 0, 1);
        typ.fs_reg1ty = Some(Type::new_void());
        typ.fs_reg2ty = Some(Type::new_void());
        typ
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

    /// 是否是浮点数类型
    pub fn is_float(&self) -> bool {
        self.kind == TypeKind::Float || self.kind == TypeKind::Double
    }

    /// 判断是否为数字
    pub fn is_numeric(&self) -> bool {
        return self.is_int() || self.is_float();
    }

    /// 是否是方法类型
    pub fn is_func(&self) -> bool {
        self.kind == TypeKind::Func
            || (self.kind == TypeKind::Ptr
                && self.get_base_type().unwrap().borrow().kind == TypeKind::Func)
    }

    /// 是否是struct或者union
    pub fn is_struct_union(&self) -> bool {
        self.kind == TypeKind::Struct || self.kind == TypeKind::Union
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
        self.name_string = token.get_name().to_string();
    }

    /// 指针和数组类型获取基础类型的size
    pub fn get_base_size(&self) -> isize {
        if self.has_base() {
            return self.base.as_ref().unwrap().borrow().size;
        } else {
            0
        }
    }

    /// 获取基础类型
    pub fn get_base_type(&self) -> Option<TypeLink> {
        if self.has_base() {
            return Some(self.base.as_ref().unwrap().clone());
        } else {
            None
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

        // 为函数指针进行常规算术转换
        if typ1.borrow().kind == TypeKind::Func {
            return Self::pointer_to(typ1);
        }
        if typ2.borrow().kind == TypeKind::Func {
            return Self::pointer_to(typ2);
        }

        // 处理浮点类型
        // 优先使用double类型
        if typ1.borrow().kind == TypeKind::Double || typ2.borrow().kind == TypeKind::Double {
            return Type::new_double();
        }
        // 其次使用float类型
        if typ1.borrow().kind == TypeKind::Float || typ2.borrow().kind == TypeKind::Float {
            return Type::new_float();
        }

        let s1 = typ1.borrow().size;
        let s2 = typ2.borrow().size;

        let mut r1 = Type::new_int();
        let mut r2 = Type::new_int();
        if s1 >= 4 {
            r1 = typ1.clone()
        }
        if s2 >= 4 {
            r2 = typ2.clone()
        }

        if s1 != s2 {
            return if s1 < s2 { r2 } else { r1 };
        }

        if typ2.borrow().is_unsigned {
            return r2;
        }

        r1
    }

    /// 复制结构体的类型
    pub fn copy_struct_type(src: &TypeLink) -> TypeLink {
        let st = src.borrow().kind.clone();
        let size = src.borrow().size;
        let align = src.borrow().align;
        let mut dst = Type::new(st, size, align);
        let mut members = vec![];
        for member in src.borrow().members.iter() {
            members.push(member.clone());
        }
        dst.members = members;
        dst.is_flexible = src.borrow().is_flexible;
        Rc::new(RefCell::new(dst))
    }
}

// 是否为一或两个含浮点成员变量的结构体
pub fn cal_flo_st_mems_ty(typ: TypeLink, gp: usize, fp: usize) -> Vec<TypeLink> {
    let ct = typ.clone();
    let t = typ.borrow();

    // 联合体不通过浮点寄存器传递
    if t.kind == TypeKind::Union {
        return vec![Type::new_void(), Type::new_void()];
    }

    // RTy：RegsType，结构体的第一、二个寄存器的类型
    let mut rty = vec![Type::new_void(), Type::new_void()];
    // 记录可以使用的寄存器的索引值
    let mut regs_ty_idx = 0;
    // 获取浮点结构体的寄存器类型，如果不是则为TyVoid
    get_flo_st_mems_ty(ct, &mut rty, &mut regs_ty_idx);

    // 不是浮点结构体，直接退出
    if regs_ty_idx > 2 {
        return vec![Type::new_void(), Type::new_void()];
    }

    let rt0 = rty[0].borrow();
    let rt1 = rty[1].borrow();
    // 只有一个浮点成员的结构体，使用1个FP
    let bool1 = rt0.is_float() && rt1.kind == TypeKind::Void && fp < FP_MAX;
    // 一个浮点成员和一个整型成员的结构体，使用1个FP和1个GP
    let bool2 = (rt0.is_float() && rt1.is_int() && fp < FP_MAX && gp < GP_MAX)
        || (rt0.is_int() && rt1.is_float() && fp < FP_MAX && gp < GP_MAX);
    // 两个浮点成员的结构体，使用2个FP
    let bool3 = rt0.is_float() && rt1.is_float() && fp + 1 < FP_MAX;
    if bool1 || bool2 || bool3 {
        return rty.to_vec();
    }
    vec![Type::new_void(), Type::new_void()]
}

// 获取浮点结构体的成员类型
pub fn get_flo_st_mems_ty(typ: TypeLink, result: &mut Vec<TypeLink>, idx: &mut usize) {
    match typ.borrow().kind {
        TypeKind::Struct => {
            // 遍历结构体的成员，获取成员类型
            for member in typ.borrow().members.iter() {
                let mt = member.typ.as_ref().unwrap();
                get_flo_st_mems_ty(mt.clone(), result, idx);
            }
        }
        TypeKind::Union => {
            if *idx < 2 && typ.borrow().size <= 8 {
                // 联合体若不超过8字节，视为Long类型处理
                result[*idx] = Type::new_long();
                *idx += 1;
            } else {
                // 否则不是浮点结构体
                *idx += 2;
            }
        }
        TypeKind::Array => {
            // 遍历数组的成员，计算是否为浮点结构体
            for _ in 0..typ.borrow().len {
                let b = typ.borrow();
                let bt = b.base.as_ref().unwrap();
                get_flo_st_mems_ty(bt.clone(), result, idx);
            }
        }
        _ => {
            // 若为基础类型，且存在可用寄存器时，填充成员的类型
            if *idx < 2 {
                result[*idx] = typ.clone();
            }
            *idx += 1;
        }
    }
}

/// 进行常规的算术转换
pub fn usual_arith_conv(lhs: NodeLink, rhs: NodeLink) -> (NodeLink, NodeLink) {
    let typ = Type::get_common_type(
        lhs.typ.as_ref().unwrap().clone(),
        rhs.typ.as_ref().unwrap().clone(),
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
        // 将节点类型设为 int
        NodeKind::Num => {
            node.typ = Some(Type::new_int());
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
            node.typ = node.lhs.as_ref().unwrap().typ.clone();
        }
        NodeKind::Neg => {
            // 对左部转换
            let typ = Type::get_common_type(
                Type::new_int(),
                node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone(),
            );
            node.lhs = Some(Node::new_cast(node.lhs.take().unwrap(), typ.clone()));
            node.typ = Some(typ);
        }
        // 将节点类型设为 节点左部的类型
        // 左部不能是数组节点
        NodeKind::Assign => {
            let t = node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone();
            if t.borrow().kind == TypeKind::Array {
                let token = &node.token;
                error_token!(token, "not an lvalue");
                unreachable!()
            }
            if t.borrow().kind != TypeKind::Struct {
                node.rhs = Some(Node::new_cast(node.rhs.take().unwrap(), t.clone()))
            }
            node.typ = Some(t);
        }
        // 将节点类型设为 int
        NodeKind::Eq | NodeKind::Ne | NodeKind::Lt | NodeKind::Le => {
            // 对左右部转换
            let (lhs, rhs) = usual_arith_conv(node.lhs.take().unwrap(), node.rhs.take().unwrap());
            node.lhs = Some(lhs);
            node.rhs = Some(rhs);
            node.typ = Some(Type::new_int());
        }
        NodeKind::FuncCall => {
            node.typ = Some(Type::new_long());
        }
        // 将节点类型设为 int
        NodeKind::Not | NodeKind::LogAnd | NodeKind::LogOr => {
            node.typ = Some(Type::new_int());
        }
        // 将节点类型设为 左部的类型
        NodeKind::BitNot | NodeKind::Shl | NodeKind::Shr => {
            node.typ = node.lhs.as_ref().unwrap().typ.clone();
        }
        // 将节点类型设为 变量的类型
        NodeKind::Var => {
            let var = &*node.var.as_ref().unwrap().clone();
            let vt = var.borrow().get_type().clone();
            node.typ = Some(vt.clone());
        }
        // 如果:左或右部为void则为void，否则为二者兼容的类型
        NodeKind::Cond => {
            let then_t = node.then.as_ref().unwrap().typ.as_ref().unwrap().clone();
            let els_t = node.els.as_ref().unwrap().typ.as_ref().unwrap().clone();
            if then_t.borrow().kind == TypeKind::Void || els_t.borrow().kind == TypeKind::Void {
                node.typ = Some(Type::new_void());
            } else {
                // 对左右部转换
                let (lhs, rhs) =
                    usual_arith_conv(node.then.take().unwrap(), node.els.take().unwrap());
                node.then = Some(lhs);
                node.els = Some(rhs);
                node.typ = node.then.as_ref().unwrap().typ.clone();
            }
        }
        // 将节点类型设为 右部的类型
        NodeKind::Comma => {
            node.typ = node.rhs.as_ref().unwrap().typ.clone();
        }
        // 将节点类型设为 成员的类型
        NodeKind::Member => {
            node.typ = node.member.as_ref().unwrap().typ.clone();
        }
        // 将节点类型设为 指针，并指向左部的类型
        NodeKind::Addr => {
            let t = node.lhs.as_ref().unwrap().typ.as_ref().unwrap().clone();
            if t.borrow().kind == TypeKind::Array {
                node.typ = Some(Type::pointer_to(t.borrow().base.as_ref().unwrap().clone()));
            } else {
                node.typ = Some(Type::pointer_to(t.clone()));
            }
        }
        // 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则报错
        NodeKind::DeRef => {
            let lhs = node.lhs.as_ref().unwrap();
            let t = lhs.typ.as_ref().unwrap().clone();
            if t.borrow().has_base() {
                if t.borrow().kind == TypeKind::Void {
                    let token = node.lhs.as_ref().unwrap().get_token();
                    error_token!(token, "dereferencing a void pointer");
                    return;
                }
                node.typ = Some(t.borrow().base.as_ref().unwrap().clone());
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
                    node.typ = last.lhs.as_ref().unwrap().typ.clone();
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
