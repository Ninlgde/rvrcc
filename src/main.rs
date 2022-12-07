use core::cell::{RefCell, RefMut};
use core::cmp::Ordering;
use std::env;
use lazy_static::lazy_static;

lazy_static! {
    /// 全局对象
    static ref INPUT: UPSafeCell<String> = unsafe {
        let args: Vec<String> = env::args().collect();

        if args.len() != 2 {
            panic!("Usage: {} invalid number of arguments", args[0]);
        }

        UPSafeCell::new(args[1].clone())
    };
}

fn main() {
    let mut cur = &mut tokenize(&INPUT.exclusive_access());

    // 声明一个全局main段，同时也是程序入口段
    print!("  .globl main\n");
    // main段标签
    print!("main:\n");
    // li为addi别名指令，加载一个立即数到寄存器中

    // 这里我们将算式分解为 num (op num) (op num)...的形式
    // 所以先将第一个num传入a0
    print!("  li a0, {}\n", get_number(&cur));
    cur = cur.next.as_mut().unwrap();

    // 解析 (op num)
    while cur.kind != TokenKind::TKEof {
        if equal(&cur, "+".to_string()) {
            cur = cur.next.as_mut().unwrap();
            print!("  addi a0, a0, {}\n", get_number(&cur));
            cur = cur.next.as_mut().unwrap();
            continue;
        }

        // 不是+，则判断-
        // 没有subi指令，但是addi支持有符号数，所以直接对num取反
        cur = skip(cur, "-".to_string());
        print!("  addi a0, a0, -{}\n", get_number(&cur));
        cur = cur.next.as_mut().unwrap();
    }

    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    // 返回的为a0的值
    print!("  ret\n");
}

/// 传入程序的参数为str类型，因为需要转换为需要long类型
/// strtol为“string to long”，
/// 参数为：被转换的str，str除去数字后的剩余部分，进制
/// 传入&p，即char**, 是为了修改P的值
fn strtol(chars: &Vec<u8>, pos: &mut usize, base: u32) -> i64 {
    let mut result: i64 = 0;
    while *pos < chars.len() {
        if let Some(i) = (chars[*pos] as char).to_digit(base) {
            result = result * base as i64 + i as i64;
            *pos += 1;
        } else {
            break;
        }
    }
    result
}

#[derive(Eq, PartialEq)]
enum TokenKind {
    // none
    TKNone,
    // 操作符如： + -
    TKPunct,
    // 数字
    TKNum,
    // 文件终止符，即文件的最后
    TKEof,
}

struct Token {
    // 种类
    kind: TokenKind,
    // 指向下一终结符
    next: Option<Box<Token>>,
    // 值
    val: i32,
    // token 名
    name: String,
    // 在解析的字符串内的位置
    offset: usize,
}

impl Token {
    /// 根据kind和name创建token
    pub fn new(kind: TokenKind, name: String, offset: usize) -> Token {
        Token { kind, next: None, val: 0, name, offset }
    }

    /// 创建某种kind的token
    pub fn new_kind(kind: TokenKind) -> Token {
        Token { kind, next: None, val: 0, name: String::new(), offset: 0 }
    }
}

/// 比较token和name
fn equal(token: &Box<Token>, s: String) -> bool {
    token.name.cmp(&s) == Ordering::Equal
}

/// 跳过特定字符的token
fn skip(token: &mut Box<Token>, s: String) -> &mut Box<Token> {
    if !equal(token, s.clone()) {
        panic!("expect {}", s)
    }
    token.next.as_mut().unwrap()
}

/// 从token中获取数字
fn get_number(token: &Token) -> i32 {
    if token.kind != TokenKind::TKNum {
        panic!("expect a number")
    }
    token.val
}

/// 终结符解析
fn tokenize(input: &String) -> Box<Token> {
    let mut head = Box::new(Token::new_kind(TokenKind::TKNone));
    let mut cur = &mut head;

    let chars = input.clone().into_bytes();
    let mut pos = 0;

    while pos < chars.len() {
        let c = chars[pos] as char;
        // 跳过所有空白符如：空格、回车
        if c.is_whitespace() {
            pos += 1;
            continue;
        }

        if c.is_digit(10) {
            // 初始化，类似于C++的构造函数
            // 我们不使用Head来存储信息，仅用来表示链表入口，这样每次都是存储在Cur->Next
            // 否则下述操作将使第一个Token的地址不在Head中。
            cur.next = Some(Box::new(Token::new_kind(TokenKind::TKNum)));
            // 指针前进
            cur = cur.next.as_mut().unwrap();
            let old_pos = pos;
            cur.val = strtol(&chars, &mut pos, 10) as i32;
            // 使用vec和copy_from_slice 解决cannot move的问题
            let mut dst = vec![0; pos - old_pos];
            dst.copy_from_slice(&chars[old_pos..pos]);
            cur.name = String::from_utf8(dst).unwrap();
            cur.offset = old_pos;
            continue;
        }

        // 解析操作符
        if c == '+' || c == '-' {
            // 操作符长度都为1
            cur.next = Some(Box::new(Token::new(TokenKind::TKPunct, c.to_string(), pos)));
            cur = cur.next.as_mut().unwrap();
            pos += 1;
            continue;
        }

        // 处理无法识别的字符
        panic!("invalid token: {:?}", chars[pos] as char);
    }

    // 解析结束，增加一个EOF，表示终止符。
    cur.next = Some(Box::new(Token::new_kind(TokenKind::TKEof)));

    // Head无内容，所以直接返回Next
    head.next.unwrap()
}

pub struct UPSafeCell<T> {
    /// inner data
    inner: RefCell<T>,
}

unsafe impl<T> Sync for UPSafeCell<T> {}

impl<T> UPSafeCell<T> {
    /// User is responsible to guarantee that inner struct is only used in
    /// uniprocessor.
    pub unsafe fn new(value: T) -> Self {
        Self {
            inner: RefCell::new(value),
        }
    }
    /// Exclusive access inner data in UPSafeCell. Panic if the data has been borrowed.
    pub fn exclusive_access(&self) -> RefMut<'_, T> {
        self.inner.borrow_mut()
    }
}