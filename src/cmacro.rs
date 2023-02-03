use crate::preprocess::{new_num_token, new_str_token};
use crate::token::{File, Token};
use crate::tokenize::tokenize;
use crate::BASE_FILE;
use chrono::{DateTime, Datelike, Timelike, Utc};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::{fs, ptr};

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
    /// 是否为可变参数
    pub(crate) is_va_arg: bool,
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
    /// 宏变量为真，或者宏函数为假
    is_obj_like: bool,
    /// 宏处理函数
    handler: Option<MacroHandlerFn>,
    /// 是否为可变参数的
    va_args_name: String,
}

impl MacroInner {
    /// 构造inner
    pub fn new(name: &str, body: Vec<Token>, is_obj_like: bool) -> Self {
        MacroInner {
            name: name.to_string(),
            body,
            params: vec![],
            is_obj_like,
            handler: None,
            va_args_name: "".to_string(),
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
    pub fn new(name: &str, body: Vec<Token>, is_obj_like: bool) -> Self {
        Self {
            inner: Rc::new(RefCell::new(MacroInner::new(name, body, is_obj_like))),
        }
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

    /// 设置宏函数参数
    pub fn set_params(&mut self, params: Vec<MacroParam>) {
        let mut inner = self.inner.borrow_mut();
        inner.params = params
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

    /// 宏是否为可变参数
    pub fn get_va_args_name(&self) -> String {
        let inner = self.inner.borrow();
        inner.va_args_name.to_string()
    }

    /// 设置宏为可变
    pub fn set_va_args_name(&mut self, name: String) {
        let mut inner = self.inner.borrow_mut();
        inner.va_args_name = name;
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

/// C语言内置宏
pub static mut BUILTIN_MACROS: Option<HashMap<String, Macro>> = None;

/// 定义宏
pub fn define(s: &str) {
    let eq = s.find("=");
    if eq.is_some() {
        let eq = eq.unwrap();
        // 存在赋值，使用该值
        define_macro(&s[0..eq], &s[eq + 1..s.len()])
    } else {
        // 不存在赋值，则设为1
        define_macro(s, "1");
    }
}

/// 定义预定义的宏
pub fn define_macro(name: &str, buf: &str) {
    let body = tokenize(File::new_link("<built-in>".to_string(), 1, buf.to_string()));
    unsafe {
        BUILTIN_MACROS
            .as_mut()
            .unwrap()
            .insert(name.to_string(), Macro::new(name, body, true));
    }
}

/// 取消定义宏
pub fn undef_macro(name: &str) {
    unsafe {
        BUILTIN_MACROS.as_mut().unwrap().remove(name);
    }
}

/// 增加内建的宏和相应的宏处理函数
pub fn add_builtin(name: &str, handler: MacroHandlerFn) {
    let mut macro_ = Macro::new(name, vec![], true);
    macro_.set_handler(handler);
    unsafe {
        BUILTIN_MACROS
            .as_mut()
            .unwrap()
            .insert(name.to_string(), macro_);
    }
}

/// 初始化预定义的宏
pub fn init_macros() {
    unsafe { BUILTIN_MACROS = Some(HashMap::new()) }
    define_macro("_LP64", "1");
    define_macro("__C99_MACRO_WITH_VA_ARGS", "1");
    define_macro("__ELF__", "1");
    define_macro("__LP64__", "1");
    define_macro("__SIZEOF_DOUBLE__", "8");
    define_macro("__SIZEOF_FLOAT__", "4");
    define_macro("__SIZEOF_INT__", "4");
    define_macro("__SIZEOF_LONG_DOUBLE__", "8");
    define_macro("__SIZEOF_LONG_LONG__", "8");
    define_macro("__SIZEOF_LONG__", "8");
    define_macro("__SIZEOF_POINTER__", "8");
    define_macro("__SIZEOF_PTRDIFF_T__", "8");
    define_macro("__SIZEOF_SHORT__", "2");
    define_macro("__SIZEOF_SIZE_T__", "8");
    define_macro("__SIZE_TYPE__", "unsigned long");
    define_macro("__STDC_HOSTED__", "1");
    define_macro("__STDC_NO_ATOMICS__", "1");
    define_macro("__STDC_NO_COMPLEX__", "1");
    define_macro("__STDC_UTF_16__", "1");
    define_macro("__STDC_UTF_32__", "1");
    define_macro("__STDC_VERSION__", "201112L");
    define_macro("__STDC__", "1");
    define_macro("__USER_LABEL_PREFIX__", "");
    define_macro("__alignof__", "_Alignof");
    define_macro("__rvrcc__", "1");
    define_macro("__rvcc__", "1");
    define_macro("__const__", "const");
    define_macro("__gnu_linux__", "1");
    define_macro("__inline__", "inline");
    define_macro("__linux", "1");
    define_macro("__linux__", "1");
    define_macro("__signed__", "signed");
    define_macro("__typeof__", "typeof");
    define_macro("__unix", "1");
    define_macro("__unix__", "1");
    define_macro("__volatile__", "volatile");
    define_macro("linux", "1");
    define_macro("unix", "1");
    define_macro("__riscv_mul", "1");
    define_macro("__riscv_muldiv", "1");
    define_macro("__riscv_fdiv", "1");
    define_macro("__riscv_xlen", "64");
    define_macro("__riscv", "1");
    define_macro("__riscv64", "1");
    define_macro("__riscv_div", "1");
    define_macro("__riscv_float_abi_double", "1");
    define_macro("__riscv_flen", "64");

    add_builtin("__FILE__", file_macro);
    add_builtin("__LINE__", line_macro);
    // 支持__COUNTER__
    add_builtin("__COUNTER__", counter_macro);
    // 支持__TIMESTAMP__
    add_builtin("__TIMESTAMP__", timestamp_macro);
    // 支持__BASE_FILE__
    add_builtin("__BASE_FILE__", base_file_macro);

    // 支持__DATE__和__TIME__
    let now = Utc::now();
    define_macro("__DATE__", &format_date(now));
    define_macro("__TIME__", &format_time(now));
}

const MONTH: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];

/// __DATE__ is expanded to the current date, e.g. "May 17 2020".
fn format_date(now: DateTime<Utc>) -> String {
    format!(
        "\"{} {:02} {}\"",
        MONTH[now.month0() as usize],
        now.day(),
        now.year()
    )
}

/// __TIME__ is expanded to the current time, e.g. "13:34:03".
fn format_time(now: DateTime<Utc>) -> String {
    format!(
        "\"{:02} {:02} {:02}\"",
        now.hour(),
        now.minute(),
        now.second()
    )
}

/// 给__COUNTER__宏使用的计数器
static mut COUNTER: i64 = 0;
/// __COUNTER__ is expanded to serial values starting from 0.
fn counter_macro(input: Vec<Token>) -> Vec<Token> {
    unsafe {
        let nt = new_num_token(COUNTER, input.first().unwrap());
        COUNTER = COUNTER + 1;
        vec![nt, Token::new_eof(0, 0)]
    }
}

/// 文件标号函数
fn file_macro(input: Vec<Token>) -> Vec<Token> {
    // 如果存在原始的宏，则遍历后使用原始的宏
    let mut head = input.first().unwrap().clone();
    while head.get_origin().is_some() {
        head = head.get_origin().unwrap().clone();
    }
    // 根据原始宏的文件名构建字符串终结符
    let file = head.get_file().unwrap();
    let nt = new_str_token(file.borrow().display_name.to_string(), &head);
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
    let file = head.get_file().unwrap();
    let line = head.get_line_no() as isize + file.borrow().line_delta;
    let nt = new_num_token(line as i64, &head);
    vec![nt, Token::new_eof(0, 0)]
}

/// __TIMESTAMP__ is expanded to a string describing the last
/// modification time of the current file. E.g.
/// "Fri Jul 24 01:32:50 2020"
fn timestamp_macro(input: Vec<Token>) -> Vec<Token> {
    let head = input.first().unwrap().clone();
    let file = head.get_file().unwrap();
    let file_name = file.borrow().name.to_string();
    let metadate = fs::metadata(file_name);
    let mut time = "??? ??? ?? ??:??:?? ????".to_string();
    if metadate.is_ok() {
        let mtime = metadate.unwrap().modified().unwrap();
        let datetime: DateTime<Utc> = mtime.into();
        time = format!(
            "{:?} {} {:02} {:02}:{:02}:{:02} {}",
            datetime.weekday(),
            MONTH[datetime.month0() as usize],
            datetime.day(),
            datetime.hour(),
            datetime.minute(),
            datetime.second(),
            datetime.year()
        );
    }
    let nt = new_str_token(time, &head);
    vec![nt, Token::new_eof(0, 0)]
}

/// __BASE_FILE__宏
fn base_file_macro(input: Vec<Token>) -> Vec<Token> {
    let head = input.first().unwrap().clone();
    let nt = new_str_token(unsafe { BASE_FILE.to_string() }, &head);
    vec![nt, Token::new_eof(0, 0)]
}
