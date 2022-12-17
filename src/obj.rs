use std::cell::RefCell;
use std::rc::Rc;
use crate::{Node, Type};

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
    },
}

impl Obj {
    pub fn get_offset(&self) -> isize {
        match self {
            Self::Var { offset, .. } => *offset,
            _ => 0
        }
    }

    pub fn set_offset(&mut self, of: isize) {
        match self {
            Self::Var { offset, .. } => *offset = of,
            _ => ()
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

    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func {..})
    }

    pub fn new_var(name: String, type_: Box<Type>, is_local: bool, init_data: Option<Vec<u8>>) -> Self {
        Self::Var { name, type_, is_local, init_data, offset: 0 }
    }

    pub fn new_lvar(name: String, type_: Box<Type>) -> Self {
        Self::new_var(name, type_, true, None)
    }

    pub fn new_gvar(name: String, type_: Box<Type>, init_data: Option<Vec<u8>>) -> Self {
        Self::new_var(name, type_, false, init_data)
    }

    pub fn new_func(name: String, params: Vec<Rc<RefCell<Obj>>>, locals: Vec<Rc<RefCell<Obj>>>, body: Option<Node>, type_: Box<Type>) -> Self {
        Self::Func { name, params, locals, body, type_, stack_size: 0 }
    }
}

#[derive(Clone)]
pub struct VarScope {
    // 变量域名称
    name: String,
    // 对应的变量
    var: Rc<RefCell<Obj>>,
}

pub struct Scope {
    vars: Vec<VarScope>,
}

impl Scope {
    pub fn new() -> Self {
        Self { vars: vec![] }
    }

    pub fn add_var(&mut self, name: String, var: Rc<RefCell<Obj>>) {
        self.vars.insert(0, VarScope { name, var })
    }

    pub fn get_var(&self, name: &str) -> Option<Rc<RefCell<Obj>>> {
        for scope in self.vars.to_vec() {
            if name.eq(scope.name.as_str()) {
                return Some(scope.var);
            }
        }
        return None;
    }
}