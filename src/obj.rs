use crate::ctype::{Type, TypeLink};
use crate::initializer::Relocation;
use crate::node::NodeLink;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
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
        /// 是否为文件域内的
        is_static: bool,
        /// 全局变量
        /// 是否为试探性的变量
        is_tentative: bool,
        /// 是否为线程局部存储，Thread Local Storage
        is_tls: bool,
        /// 用于初始化的数据
        init_data: Option<Vec<i8>>,
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
        /// Alloca区域底部
        alloca_bottom: Option<ObjLink>,
        /// 栈大小
        stack_size: isize,
        /// 是否为函数定义
        is_definition: bool,
        /// 是否为文件域内的
        is_static: bool,
        /// 对应的终结符
        token: Option<Token>,
        /// 内联
        is_inline: bool,
        /// 静态内联函数
        /// 是否活跃
        is_live: bool,
        /// 是否是根
        is_root: bool,
        /// 引用列表
        refs: Vec<String>,
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

    /// 是否为 thread local
    pub fn is_tls(&self) -> bool {
        match self {
            Self::Var { is_tls, .. } => *is_tls,
            _ => false,
        }
    }

    /// 修改 thread local
    pub fn set_tls(&mut self, tls: bool) {
        match self {
            Self::Var { is_tls, .. } => *is_tls = tls,
            _ => {}
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
            is_tentative: false,
            is_tls: false,
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

    /// 获取全局变量是否为试探性的
    pub fn is_tentative(&self) -> bool {
        match self {
            Self::Var { is_tentative, .. } => *is_tentative,
            _ => false,
        }
    }

    /// 设置全局变量为试探性的
    pub fn set_tentative(&mut self, tentative: bool) {
        match self {
            Self::Var { is_tentative, .. } => *is_tentative = tentative,
            _ => {}
        }
    }

    /// 增加引用
    pub fn push_refs(&mut self, rf: String) {
        match self {
            Obj::Func { refs, .. } => {
                refs.push(rf);
            }
            _ => {}
        }
    }

    /// 设置root
    pub fn set_root(&mut self, root: bool) {
        match self {
            Obj::Func { is_root, .. } => {
                *is_root = root;
            }
            _ => {}
        }
    }

    /// 是否未根函数
    pub fn is_root(&self) -> bool {
        match self {
            Obj::Func { is_root, .. } => *is_root,
            _ => false,
        }
    }

    /// 是否活跃
    pub fn is_live(&self) -> bool {
        match self {
            Obj::Func { is_live, .. } => *is_live,
            _ => false,
        }
    }

    /// 设置活跃
    pub fn make_live(&mut self) {
        match self {
            Obj::Func { is_live, .. } => {
                *is_live = true;
            }
            _ => {}
        }
    }

    /// 获取引用列表
    pub fn get_refs(&self) -> &Vec<String> {
        match self {
            Obj::Func { refs, .. } => refs,
            _ => panic!("error object"),
        }
    }

    pub fn get_alloca_bottom(&self) -> Option<ObjLink> {
        match self {
            Obj::Func { alloca_bottom, .. } => Some(alloca_bottom.as_ref().unwrap().clone()),
            _ => None,
        }
    }

    /// 函数对象设置相关信息
    pub fn set_function(
        &mut self,
        params_: Vec<ObjLink>,
        va_area_: Option<ObjLink>,
        alloca_bottom_: Option<ObjLink>,
        locals_: Vec<ObjLink>,
        body_: Option<NodeLink>,
        definition: bool,
        is_static_: bool,
        is_inline_: bool,
    ) {
        match self {
            Self::Func {
                params,
                locals,
                body,
                is_definition,
                is_static,
                is_inline,
                is_root,
                va_area,
                alloca_bottom,
                ..
            } => {
                *params = params_;
                *locals = locals_;
                *body = body_;
                *is_definition = definition;
                *is_static = is_static_;
                *is_inline = is_inline_;
                *is_root = !(is_static_ && is_inline_);
                *va_area = va_area_;
                *alloca_bottom = alloca_bottom_;
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
            is_inline: false,
            is_live: false,
            is_root: false,
            refs: vec![],
            alloca_bottom: None,
        }
    }
}

/// 局部和全局变量或是typedef的域
#[derive(Clone)]
pub struct VarScope {
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
    pub fn new() -> Self {
        Self {
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
    /// 是否为内联
    pub(crate) is_inline: bool,
    /// 是否为线程局部存储，Thread Local Storage
    pub(crate) is_tls: bool,
    /// 对齐量
    pub(crate) align: isize,
}

impl VarAttr {
    pub fn new() -> Self {
        Self {
            is_typedef: false,
            is_static: false,
            is_extern: false,
            is_inline: false,
            is_tls: false,
            align: 0,
        }
    }
}

// C有两个域：变量（或类型别名）域，结构体（或联合体，枚举）标签域
pub struct Scope {
    /// 指向当前域内的变量
    vars: HashMap<String, Rc<RefCell<VarScope>>>,
    /// 指向当前域内的结构体标签
    tags: HashMap<String, TagScope>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    /// 添加一个var
    pub fn add_var(&mut self, name: String) -> Rc<RefCell<VarScope>> {
        // 设置变量默认的对齐量为类型的对齐量
        let vs = Rc::new(RefCell::new(VarScope::new()));
        self.vars.insert(name, vs.clone());
        vs
    }

    /// 找到对应的var
    pub fn get_var(&self, name: &str) -> Option<Rc<RefCell<VarScope>>> {
        let var = self.vars.get(name);
        if let Some(var) = var {
            return Some(var.clone());
        }
        None
    }

    /// 添加一个tag
    pub fn add_tag(&mut self, name: String, typ: TypeLink) {
        self.tags.insert(name, TagScope { typ });
    }

    /// 找到对应的tag
    pub fn get_tag(&self, name: &str) -> Option<TypeLink> {
        let tag = self.tags.get(name);
        if let Some(tag) = tag {
            return Some(tag.typ.clone());
        }
        None
    }

    /// 替换某个`name`的`tag`的`type`
    pub fn replace_tag(&mut self, name: &str, typ: Type) -> Option<TypeLink> {
        let tag = self.tags.get(name);
        if let Some(tag) = tag {
            tag.typ.replace(typ);
            return Some(tag.typ.clone());
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
