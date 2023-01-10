use crate::token::Token;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

/// 定义的宏变量
struct MacroInner {
    /// 名称
    name: String,
    /// 对应的终结符
    body: Vec<Token>,
    /// 是否被删除了
    deleted: bool,
    /// 宏变量为真，或者宏函数为假
    is_obj_like: bool,
}

impl MacroInner {
    /// 构造inner
    pub fn new(name: &str, body: Vec<Token>, deleted: bool, is_obj_like: bool) -> Self {
        MacroInner {
            name: name.to_string(),
            body,
            deleted,
            is_obj_like,
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
    pub fn new(name: &str, body: Vec<Token>, deleted: bool, is_obj_like: bool) -> Self {
        Self {
            inner: Rc::new(RefCell::new(MacroInner::new(
                name,
                body,
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
