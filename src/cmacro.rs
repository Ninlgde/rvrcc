use crate::preprocess::{new_num_token, new_str_token};
use crate::token::{File, Token};
use crate::tokenize::tokenize;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

/// 宏函数形参
#[derive(Clone)]
pub struct MacroParam {
    /// 名称
    pub(crate) name: String,
}

#[derive(Clone)]
pub struct MacroArg {
    /// 名称
    pub(crate) name: String,
    /// 对应的终结符链表
    pub(crate) tokens: Vec<Token>,
}

impl MacroArg {
    /// 替换tokens
    #[allow(dead_code)]
    pub fn replace(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
    }
}

/// 宏处理函数
type MacroHandlerFn = fn(Vec<Token>) -> Vec<Token>;

/// 定义的宏变量
struct MacroInner {
    /// 名称
    name: String,
    /// 对应的终结符
    body: Vec<Token>,
    /// 宏函数参数
    params: Vec<MacroParam>,
    /// 是否被删除了
    deleted: bool,
    /// 宏变量为真，或者宏函数为假
    is_obj_like: bool,
    /// 宏处理函数
    handler: Option<MacroHandlerFn>,
}

impl MacroInner {
    /// 构造inner
    pub fn new(
        name: &str,
        body: Vec<Token>,
        params: Vec<MacroParam>,
        deleted: bool,
        is_obj_like: bool,
    ) -> Self {
        MacroInner {
            name: name.to_string(),
            body,
            params,
            deleted,
            is_obj_like,
            handler: None,
        }
    }
}

/// 定义的宏变量
pub struct Macro {
    inner: Rc<RefCell<MacroInner>>,
}

/// 实现clone
impl Clone for Macro {
    fn clone(&self) -> Self {
        Macro {
            inner: self.inner.clone(),
        }
    }
}

impl Macro {
    /// 创建宏
    pub fn new(
        name: &str,
        body: Vec<Token>,
        params: Vec<MacroParam>,
        deleted: bool,
        is_obj_like: bool,
    ) -> Self {
        Self {
            inner: Rc::new(RefCell::new(MacroInner::new(
                name,
                body,
                params,
                deleted,
                is_obj_like,
            ))),
        }
    }

    /// 是否相等
    pub fn eq(&self, name: &str) -> bool {
        let inner = self.inner.borrow();
        inner.name.eq(name)
    }

    /// 获取宏名称
    pub fn get_name(&self) -> String {
        let inner = self.inner.borrow();
        inner.name.to_string()
    }

    /// 获取宏内容
    pub fn get_body(&self) -> Vec<Token> {
        let inner = self.inner.borrow();
        inner.body.to_vec()
    }

    /// 宏函数参数
    pub fn get_params(&self) -> Vec<MacroParam> {
        let inner = self.inner.borrow();
        inner.params.to_vec()
    }

    /// 是否被标记删除
    pub fn deleted(&self) -> bool {
        let inner = self.inner.borrow();
        inner.deleted
    }

    /// 是否为宏变量
    pub fn is_obj_like(&self) -> bool {
        let inner = self.inner.borrow();
        inner.is_obj_like
    }

    /// 获取宏处理函数
    pub fn get_handler(&self) -> Option<MacroHandlerFn> {
        let inner = self.inner.borrow();
        inner.handler
    }

    /// 设置宏处理函数
    pub fn set_handler(&mut self, handler: MacroHandlerFn) {
        let mut inner = self.inner.borrow_mut();
        inner.handler = Some(handler);
    }
}

/// 预处理语言的设计方式使得即使存在递归宏也可以保证停止。
/// 一个宏只对每个终结符应用一次。
///
/// 宏展开时的隐藏集
pub struct HideSet {
    /// 下一个
    pub(crate) next: *mut HideSet,
    /// 名称
    pub(crate) name: String,
}

impl Drop for HideSet {
    fn drop(&mut self) {
        unsafe {
            while self.next != ptr::null_mut() {
                let head = Box::from_raw(self.next);
                self.next = head.next;
                drop(head);
            }
        }
    }
}

impl HideSet {
    /// 新建hide set 裸指针
    pub fn new(name: String) -> *mut Self {
        Box::into_raw(Box::new(HideSet {
            next: ptr::null_mut(),
            name,
        }))
    }

    /// 新建头指针
    pub fn head() -> *mut Self {
        Self::new("".to_string())
    }

    /// 链表里是否包含
    pub fn contain(mut head: *mut Self, name: &str) -> bool {
        unsafe {
            while !head.is_null() {
                if (*head).name.eq(name) {
                    return true;
                }
                head = (*head).next;
            }
        }
        false
    }

    /// 合并两个链表返回新的
    pub fn union(mut head1: *mut Self, head2: *mut Self) -> *mut Self {
        unsafe {
            let new_head = Self::head();
            // 先把head2里的内容整体添加到new_head链表中
            (*new_head).next = head2;

            // 再挨个把head1添加到new_head里
            while !head1.is_null() {
                let new_node = HideSet::new((*head1).name.to_string());
                (*new_node).next = (*new_head).next;
                (*new_head).next = new_node;
                head1 = (*head1).next;
            }

            new_head
        }
    }

    /// 取两个隐藏集的交集
    pub fn intersection(mut head1: *mut Self, head2: *mut Self) -> *mut Self {
        unsafe {
            let new_head = Self::head();

            // 遍历Hs1，如果Hs2也有，那么就加入链表当中
            while !head1.is_null() {
                let name = (*head1).name.to_string();
                if Self::contain(head2, &name) {
                    let new_node = HideSet::new(name);
                    (*new_node).next = (*new_head).next;
                    (*new_head).next = new_node;
                }
                head1 = (*head1).next;
            }

            new_head
        }
    }

    /// 打印列表
    #[allow(dead_code)]
    pub fn print(mut head: *mut Self, prefix: &str) {
        eprint!("{}: ", prefix);
        unsafe {
            while !head.is_null() {
                eprint!("{:?}={} ", head, (*head).name);
                head = (*head).next
            }
        }
        eprintln!();
    }
}

// 定义预定义的宏
pub fn define_macro(name: &str, buf: &str) -> Macro {
    let body = tokenize(File::new_link("<built-in>".to_string(), 1, buf.to_string()));
    Macro::new(name, body, vec![], false, true)
}

// 增加内建的宏和相应的宏处理函数
pub fn add_builtin(name: &str, handler: MacroHandlerFn) -> Macro {
    let mut macro_ = Macro::new(name, vec![], vec![], false, true);
    macro_.set_handler(handler);
    macro_
}

// 初始化预定义的宏
pub fn init_macros() -> Vec<Macro> {
    let mut macros = vec![];
    macros.push(define_macro("_LP64", "1"));
    macros.push(define_macro("__C99_MACRO_WITH_VA_ARGS", "1"));
    macros.push(define_macro("__ELF__", "1"));
    macros.push(define_macro("__LP64__", "1"));
    macros.push(define_macro("__SIZEOF_DOUBLE__", "8"));
    macros.push(define_macro("__SIZEOF_FLOAT__", "4"));
    macros.push(define_macro("__SIZEOF_INT__", "4"));
    macros.push(define_macro("__SIZEOF_LONG_DOUBLE__", "8"));
    macros.push(define_macro("__SIZEOF_LONG_LONG__", "8"));
    macros.push(define_macro("__SIZEOF_LONG__", "8"));
    macros.push(define_macro("__SIZEOF_POINTER__", "8"));
    macros.push(define_macro("__SIZEOF_PTRDIFF_T__", "8"));
    macros.push(define_macro("__SIZEOF_SHORT__", "2"));
    macros.push(define_macro("__SIZEOF_SIZE_T__", "8"));
    macros.push(define_macro("__SIZE_TYPE__", "unsigned long"));
    macros.push(define_macro("__STDC_HOSTED__", "1"));
    macros.push(define_macro("__STDC_NO_ATOMICS__", "1"));
    macros.push(define_macro("__STDC_NO_COMPLEX__", "1"));
    macros.push(define_macro("__STDC_NO_THREADS__", "1"));
    macros.push(define_macro("__STDC_NO_VLA__", "1"));
    macros.push(define_macro("__STDC_VERSION__", "201112L"));
    macros.push(define_macro("__STDC__", "1"));
    macros.push(define_macro("__USER_LABEL_PREFIX__", ""));
    macros.push(define_macro("__alignof__", "_Alignof"));
    macros.push(define_macro("__rvrcc__", "1"));
    macros.push(define_macro("__rvcc__", "1"));
    macros.push(define_macro("__const__", "const"));
    macros.push(define_macro("__gnu_linux__", "1"));
    macros.push(define_macro("__inline__", "inline"));
    macros.push(define_macro("__linux", "1"));
    macros.push(define_macro("__linux__", "1"));
    macros.push(define_macro("__signed__", "signed"));
    macros.push(define_macro("__typeof__", "typeof"));
    macros.push(define_macro("__unix", "1"));
    macros.push(define_macro("__unix__", "1"));
    macros.push(define_macro("__volatile__", "volatile"));
    macros.push(define_macro("linux", "1"));
    macros.push(define_macro("unix", "1"));
    macros.push(define_macro("__riscv_mul", "1"));
    macros.push(define_macro("__riscv_muldiv", "1"));
    macros.push(define_macro("__riscv_fdiv", "1"));
    macros.push(define_macro("__riscv_xlen", "64"));
    macros.push(define_macro("__riscv", "1"));
    macros.push(define_macro("__riscv64", "1"));
    macros.push(define_macro("__riscv_div", "1"));
    macros.push(define_macro("__riscv_float_abi_double", "1"));
    macros.push(define_macro("__riscv_flen", "64"));

    macros.push(add_builtin("__FILE__", file_macro));
    macros.push(add_builtin("__LINE__", line_macro));

    macros
}

/// 文件标号函数
fn file_macro(input: Vec<Token>) -> Vec<Token> {
    // 如果存在原始的宏，则遍历后使用原始的宏
    let mut head = input.first().unwrap().clone();
    while head.get_origin().is_some() {
        head = head.get_origin().unwrap().clone();
    }
    // 根据原始宏的文件名构建字符串终结符
    let nt = new_str_token(head.get_file_name(), &head);
    vec![nt, Token::new_eof(0, 0)]
}

/// 行标号函数
fn line_macro(input: Vec<Token>) -> Vec<Token> {
    // 如果存在原始的宏，则遍历后使用原始的宏
    let mut head = input.first().unwrap().clone();
    while head.get_origin().is_some() {
        head = head.get_origin().unwrap().clone();
    }
    // 根据原始的宏的行号构建数值终结符
    let nt = new_num_token(head.get_line_no() as i64, &head);
    vec![nt, Token::new_eof(0, 0)]
}
