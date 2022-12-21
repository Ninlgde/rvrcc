use crate::{Node, Type};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub enum Obj {
    Var {
        // 变量名
        name: String,
        // fp的偏移量
        offset: isize,
        // 类型
        type_: Box<Type>,
        // 是 局部或全局 变量
        is_local: bool,
        // 全局变量
        init_data: Option<Vec<u8>>,
    },
    Func {
        // 变量名
        name: String,
        // 类型
        type_: Box<Type>,
        // 方法参数
        params: Vec<Rc<RefCell<Obj>>>,
        // 函数体
        body: Option<Node>,
        // 本地变量
        locals: Vec<Rc<RefCell<Obj>>>,
        // 栈大小
        stack_size: isize,
        // 是否为函数定义
        is_definition: bool,
    },
}

impl Obj {
    pub fn get_offset(&self) -> isize {
        match self {
            Self::Var { offset, .. } => *offset,
            _ => 0,
        }
    }

    pub fn set_offset(&mut self, of: isize) {
        match self {
            Self::Var { offset, .. } => *offset = of,
            _ => (),
        }
    }

    pub fn get_name(&self) -> &String {
        match self {
            Self::Var { name, .. } => name,
            Self::Func { name, .. } => name,
        }
    }

    pub fn get_type(&self) -> &Box<Type> {
        match self {
            Self::Var { type_, .. } | Self::Func { type_, .. } => type_,
        }
    }

    pub fn get_func_return_type(&self) -> &Option<Box<Type>> {
        match self {
            Self::Func { type_, .. } => &type_.return_type,
            _ => &None,
        }
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func { .. })
    }

    pub fn new_var(
        name: String,
        type_: Box<Type>,
        is_local: bool,
        init_data: Option<Vec<u8>>,
    ) -> Self {
        Self::Var {
            name,
            type_,
            is_local,
            init_data,
            offset: 0,
        }
    }

    pub fn new_lvar(name: String, type_: Box<Type>) -> Self {
        Self::new_var(name, type_, true, None)
    }

    pub fn new_gvar(name: String, type_: Box<Type>, init_data: Option<Vec<u8>>) -> Self {
        Self::new_var(name, type_, false, init_data)
    }

    pub fn set_function(
        &mut self,
        definition: bool,
        params_: Vec<Rc<RefCell<Obj>>>,
        locals_: Vec<Rc<RefCell<Obj>>>,
        body_: Option<Node>,
    ) {
        match self {
            Self::Func {
                is_definition,
                params,
                locals,
                body,
                ..
            } => {
                *params = params_;
                *locals = locals_;
                *body = body_;
                *is_definition = definition
            }
            _ => panic!("error object!"),
        }
    }

    pub fn new_func(name: String, type_: Box<Type>) -> Self {
        Self::Func {
            name,
            params: vec![],
            locals: vec![],
            body: None,
            type_,
            stack_size: 0,
            is_definition: false,
        }
    }
}

/// 局部和全局变量或是typedef的域
#[derive(Clone)]
pub struct VarScope {
    // 变量域名称
    pub(crate) name: String,
    // 对应的变量
    pub(crate) var: Option<Rc<RefCell<Obj>>>,
    // 别名
    pub(crate) typedef: Option<Box<Type>>,
}

impl VarScope {
    pub fn new(name: String) -> Self {
        Self {
            name,
            var: None,
            typedef: None,
        }
    }

    pub fn set_var(&mut self, var: Rc<RefCell<Obj>>) {
        self.var = Some(var);
    }

    pub fn set_typedef(&mut self, typedef: Box<Type>) {
        self.typedef = Some(typedef);
    }
}

/// 结构体标签和联合体标签的域
#[derive(Clone)]
pub struct TagScope {
    // 域名称
    name: String,
    // 域类型
    typ: Box<Type>,
}

// 变量属性
#[derive(Clone)]
pub struct VarAttr {
    // 是否为类型别名
    pub is_typedef: bool,
}

// C有两个域：变量域，结构体标签域
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
        let vs = Rc::new(RefCell::new(VarScope::new(name)));
        self.vars.insert(0, vs.clone());
        vs
    }

    /// 找到对应的var
    pub fn get_var(&self, name: &str) -> Option<Rc<RefCell<VarScope>>> {
        for scope in self.vars.to_vec() {
            if name.eq(scope.borrow().name.as_str()) {
                return Some(scope);
            }
        }
        return None;
    }

    /// 添加一个tag
    pub fn add_tag(&mut self, name: String, typ: Box<Type>) {
        self.tags.push(TagScope { name, typ })
    }

    /// 找到对应的tag
    pub fn get_tag(&self, name: &str) -> Option<Box<Type>> {
        for scope in self.tags.to_vec() {
            if name.eq(scope.name.as_str()) {
                return Some(scope.typ);
            }
        }
        return None;
    }
}

#[derive(Clone)]
pub struct Member {
    // 名称
    pub(crate) name: String,
    // 类型
    pub(crate) type_: Option<Box<Type>>,
    // 偏移量
    pub(crate) offset: usize,
}

impl Member {
    pub fn new(name: String, type_: Option<Box<Type>>) -> Self {
        Self {
            name,
            type_,
            offset: 0,
        }
    }

    pub fn set_offset(&mut self, offset: usize) {
        self.offset = offset
    }
}
