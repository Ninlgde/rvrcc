use crate::ctype::{Type, TypeLink};
use crate::initializer::Relocation;
use crate::node::NodeLink;
use crate::token::Token;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

/// rc refcell for object
pub type ObjLink = Rc<RefCell<Obj>>;

/// c语言的对象: 变量和函数
#[derive(Clone)]
pub enum Obj {
    /// 变量
    Var {
        /// 变量名
        name: String,
        /// fp的偏移量
        offset: isize,
        /// 类型
        typ: TypeLink,
        /// 是 局部或全局 变量
        is_local: bool,
        /// 全局变量
        init_data: Option<Vec<i8>>,
        /// 是否为文件域内的
        is_static: bool,
        /// 指向其他全局变量的指针
        relocation: *mut Relocation,
        /// 是否为函数定义
        is_definition: bool,
        /// 对齐量
        align: isize,
        /// 对应的终结符
        token: Option<Token>,
        /// 结构体类型 一半用寄存器，一半用栈
        is_half_by_stack: bool,
    },
    /// 函数
    Func {
        /// 函数名
        name: String,
        /// 类型
        typ: TypeLink,
        /// 方法参数
        params: Vec<ObjLink>,
        /// 函数体
        body: Option<NodeLink>,
        /// 本地变量
        locals: Vec<ObjLink>,
        /// 可变参数区域
        va_area: Option<ObjLink>,
        /// 栈大小
        stack_size: isize,
        /// 是否为函数定义
        is_definition: bool,
        /// 是否为文件域内的
        is_static: bool,
        /// 对应的终结符
        token: Option<Token>,
    },
}

impl Obj {
    /// var获取偏移值
    pub fn get_offset(&self) -> isize {
        match self {
            Self::Var { offset, .. } => *offset,
            _ => 0,
        }
    }

    /// 设置var的偏移值
    pub fn set_offset(&mut self, of: isize) {
        match self {
            Self::Var { offset, .. } => *offset = of,
            _ => (),
        }
    }

    /// 获取名称
    pub fn get_name(&self) -> &String {
        match self {
            Self::Var { name, .. } => name,
            Self::Func { name, .. } => name,
        }
    }

    /// 获取类型
    pub fn get_type(&self) -> &TypeLink {
        match self {
            Self::Var { typ, .. } | Self::Func { typ, .. } => typ,
        }
    }

    /// 设置type
    pub fn set_type(&mut self, t: TypeLink) {
        match self {
            Self::Var { typ, .. } | Self::Func { typ, .. } => {
                *typ = t;
            }
        }
    }

    /// 获取函数返回值类型
    pub fn get_func_return_type(&self) -> Option<TypeLink> {
        match self {
            Self::Func { typ, .. } => {
                let t = typ.borrow();
                Some(t.return_type.as_ref().unwrap().clone())
            }
            _ => None,
        }
    }

    /// 获取一半用寄存器，一半用栈
    pub fn get_is_half_by_stack(&self) -> bool {
        match self {
            Self::Var {
                is_half_by_stack, ..
            } => *is_half_by_stack,
            _ => false,
        }
    }

    /// 设置一半用寄存器，一半用栈
    pub fn set_is_half_by_stack(&mut self, b: bool) {
        match self {
            Self::Var {
                is_half_by_stack, ..
            } => {
                *is_half_by_stack = b;
            }
            _ => (),
        }
    }

    /// 获取参数
    pub fn get_params(&self) -> &Vec<ObjLink> {
        match self {
            Self::Func { params, .. } => params,
            _ => unreachable!(),
        }
    }

    /// 获取参数
    pub fn get_va_area(&self) -> &Option<ObjLink> {
        match self {
            Self::Func { va_area, .. } => va_area,
            _ => unreachable!(),
        }
    }

    pub fn set_va_area(&mut self, va: Option<ObjLink>) {
        match self {
            Self::Func { va_area, .. } => *va_area = va,
            _ => unreachable!(),
        }
    }

    /// 是否是函数对象
    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func { .. })
    }

    /// 是否为文件域内的对象
    pub fn is_static(&self) -> bool {
        match self {
            Self::Var { is_static, .. } | Self::Func { is_static, .. } => *is_static,
        }
    }

    /// 设置文件内对象
    pub fn set_static(&mut self, is_static_: bool) {
        match self {
            Self::Var { is_static, .. } | Self::Func { is_static, .. } => *is_static = is_static_,
        }
    }

    /// 是否是本地变量
    pub fn is_local(&self) -> bool {
        match self {
            Self::Var { is_local, .. } => *is_local,
            _ => false,
        }
    }

    /// 创建变量对象
    pub fn new_var(
        name: String,
        typ: TypeLink,
        is_local: bool,
        init_data: Option<Vec<i8>>,
        align: isize,
    ) -> Self {
        Self::Var {
            name,
            typ,
            is_local,
            init_data,
            offset: 0,
            is_static: false,
            relocation: ptr::null_mut(),
            is_definition: false,
            align,
            token: None,
            is_half_by_stack: false,
        }
    }

    /// 创建本地变量对象
    pub fn new_lvar(name: String, typ: TypeLink) -> Self {
        let align = typ.borrow().align;
        Self::new_var(name, typ, true, None, align)
    }

    /// 创建全局变量对象
    pub fn new_gvar(name: String, typ: TypeLink) -> Self {
        let align = typ.borrow().align;
        Self::new_var(name, typ, false, None, align)
    }

    /// 设置字面数据
    pub fn set_init_data(&mut self, buf: Vec<i8>) {
        match self {
            Self::Var { init_data, .. } => {
                *init_data = Some(buf);
            }
            _ => (),
        }
    }

    /// 设置变量的relocation
    pub fn set_relocation(&mut self, rel: *mut Relocation) {
        match self {
            Self::Var { relocation, .. } => {
                *relocation = rel;
            }
            _ => {}
        }
    }

    /// 获取变量的relocation
    pub fn get_relocation(&self) -> *mut Relocation {
        match self {
            Self::Var { relocation, .. } => *relocation,
            _ => ptr::null_mut(),
        }
    }

    /// 设置是否是定义
    pub fn set_definition(&mut self, is_def: bool) {
        match self {
            Self::Var { is_definition, .. } | Self::Func { is_definition, .. } => {
                *is_definition = is_def;
            }
        }
    }

    /// 获取是否是定义
    pub fn is_definition(&self) -> bool {
        match self {
            Self::Var { is_definition, .. } | Self::Func { is_definition, .. } => *is_definition,
        }
    }

    /// 设置变量的对齐量
    pub fn set_align(&mut self, align_: isize) {
        match self {
            Self::Var { align, .. } => {
                *align = align_;
            }
            _ => {}
        }
    }

    /// 获取变量的对齐量
    pub fn get_align(&self) -> isize {
        match self {
            Self::Var { align, .. } => *align,
            _ => 0,
        }
    }

    /// 函数对象设置相关信息
    pub fn set_function(
        &mut self,
        params_: Vec<ObjLink>,
        va_area_: Option<ObjLink>,
        locals_: Vec<ObjLink>,
        body_: Option<NodeLink>,
        definition: bool,
        is_static_: bool,
    ) {
        match self {
            Self::Func {
                params,
                locals,
                body,
                is_definition,
                is_static,
                va_area,
                ..
            } => {
                *params = params_;
                *locals = locals_;
                *body = body_;
                *is_definition = definition;
                *is_static = is_static_;
                *va_area = va_area_
            }
            _ => panic!("error object!"),
        }
    }

    /// 函数对象
    pub fn new_func(name: String, typ: TypeLink) -> Self {
        Self::Func {
            name,
            params: vec![],
            locals: vec![],
            body: None,
            typ,
            stack_size: 0,
            is_definition: false,
            is_static: false,
            va_area: None,
            token: None,
        }
    }
}

/// 局部和全局变量或是typedef的域
#[derive(Clone)]
pub struct VarScope {
    /// 变量域名称
    pub(crate) name: String,
    /// 对应的变量
    pub(crate) var: Option<ObjLink>,
    /// 别名
    pub(crate) typedef: Option<TypeLink>,
    /// 枚举的类型
    pub(crate) enum_type: Option<TypeLink>,
    /// 枚举的值
    pub(crate) enum_val: i64,
}

impl VarScope {
    pub fn new(name: String) -> Self {
        Self {
            name,
            var: None,
            typedef: None,
            enum_type: None,
            enum_val: 0,
        }
    }

    /// 设置var
    pub fn set_var(&mut self, var: ObjLink) {
        self.var = Some(var);
    }

    /// 设置typedef
    pub fn set_typedef(&mut self, typedef: TypeLink) {
        self.typedef = Some(typedef);
    }

    /// 设置enum
    pub fn set_enum(&mut self, enum_type: TypeLink, enum_val: i64) {
        self.enum_type = Some(enum_type);
        self.enum_val = enum_val;
    }
}

/// 结构体标签，联合体标签，枚举标签的域
#[derive(Clone)]
pub struct TagScope {
    // 域名称
    name: String,
    // 域类型
    typ: TypeLink,
}

// 变量属性
#[derive(Clone)]
pub struct VarAttr {
    /// 是否为类型别名
    pub(crate) is_typedef: bool,
    /// 是否为文件域内的
    pub(crate) is_static: bool,
    /// 是否为外部变量
    pub(crate) is_extern: bool,
    /// 对齐量
    pub(crate) align: isize,
}

impl VarAttr {
    pub fn new() -> Self {
        Self {
            is_typedef: false,
            is_static: false,
            is_extern: false,
            align: 0,
        }
    }
}

// C有两个域：变量（或类型别名）域，结构体（或联合体，枚举）标签域
pub struct Scope {
    /// 指向当前域内的变量
    vars: Vec<Rc<RefCell<VarScope>>>,
    /// 指向当前域内的结构体标签
    tags: Vec<TagScope>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            vars: vec![],
            tags: vec![],
        }
    }

    /// 添加一个var
    pub fn add_var(&mut self, name: String) -> Rc<RefCell<VarScope>> {
        // 设置变量默认的对齐量为类型的对齐量
        let vs = Rc::new(RefCell::new(VarScope::new(name)));
        self.vars.insert(0, vs.clone());
        vs
    }

    /// 找到对应的var
    pub fn get_var(&self, name: &str) -> Option<Rc<RefCell<VarScope>>> {
        // 设置变量默认的对齐量为类型的对齐量
        for scope in self.vars.iter() {
            if name.eq(scope.borrow().name.as_str()) {
                return Some(scope.clone());
            }
        }
        return None;
    }

    /// 添加一个tag
    pub fn add_tag(&mut self, name: String, typ: TypeLink) {
        self.tags.push(TagScope { name, typ })
    }

    /// 找到对应的tag
    pub fn get_tag(&self, name: &str) -> Option<TypeLink> {
        for scope in self.tags.iter() {
            if name.eq(scope.name.as_str()) {
                return Some(scope.typ.clone());
            }
        }
        return None;
    }

    /// 替换某个`name`的`tag`的`type`
    pub fn replace_tag(&mut self, name: &str, typ: Type) -> Option<TypeLink> {
        for i in 0..self.tags.len() {
            let ts = &mut self.tags[i];
            if name.eq(ts.name.as_str()) {
                ts.typ.replace(typ);
                return Some(ts.typ.clone());
            }
        }
        None
    }
}

/// 结构体成员
#[derive(Clone)]
pub struct Member {
    // 名称
    pub(crate) name: String,
    // 类型
    pub(crate) typ: Option<TypeLink>,
    // 偏移量
    pub(crate) offset: isize,
    // 用于报错信息
    #[allow(dead_code)]
    pub(crate) token: Option<Token>,
    // 索引值
    pub(crate) idx: usize,
    /// 对齐量
    pub(crate) align: isize,
    /// 位域
    /// 是否为位域
    pub(crate) is_bitfield: bool,
    /// 位偏移量
    pub(crate) bit_offset: isize,
    /// 位宽度
    pub(crate) bit_width: isize,
}

impl Member {
    pub fn new(name: String, typ: Option<TypeLink>, idx: usize) -> Box<Self> {
        Box::new(Self {
            name,
            typ,
            offset: 0,
            token: None,
            idx,
            align: 0,
            is_bitfield: false,
            bit_offset: 0,
            bit_width: 0,
        })
    }

    #[allow(dead_code)]
    pub fn set_token(&mut self, token: Token) {
        self.token = Some(token);
    }
}
