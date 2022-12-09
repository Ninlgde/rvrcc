use core::fmt;
use std::env;

// 字符解析出错，并退出程序
fn print_with_error(offset: usize, args: fmt::Arguments) {
    println!("{}", unsafe { &INPUT });
    print!("{:1$}^", "", offset);
    print!(" {}\n", args);
    panic!("error at offset: {}", offset);
}

// Tok解析出错，并退出程序
fn print_with_token_error(token: &Token, args: fmt::Arguments) {
    print_with_error(token.get_offset(), args);
}


/// error at offset
#[macro_export]
macro_rules! error_at {
    ($offset:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::print_with_error($offset, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}


/// error token
#[macro_export]
macro_rules! error_token {
    ($token:expr, $fmt: literal $(, $($arg: tt)+)?) => {
        $crate::print_with_token_error($token, format_args!(concat!($fmt, "") $(, $($arg)+)?))
    }
}

static mut INPUT: String = String::new();

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Usage: {} invalid number of arguments", args[0]);
    }

    let input = args[1].clone();
    // let input = "5";
    unsafe {
        INPUT = input.to_string();
    }
    let tokens = tokenize();
    let mut tokens_pos = 0;
    let node = expr(&mut tokens_pos, &tokens);


    let token = &tokens[tokens_pos];
    if !token.at_eof() {
        error_token!(token, "extra token");
    }
    // if tokens[tokens_pos] != Token::TKEof {}

    // 声明一个全局main段，同时也是程序入口段
    print!("  .globl main\n");
    // main段标签
    print!("main:\n");

    let mut depth = 0;
    gen_expr(&Box::new(node.unwrap()), &mut depth);

    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    // 返回的为a0的值
    print!("  ret\n");

    assert_eq!(depth, 0);
}

#[derive(Eq, PartialEq)]
enum Token {
    // 操作符如： + -
    TKPunct {
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 数字
    TKNum {
        // 值
        val: i32,
        // token 名
        t_str: String,
        // 在解析的字符串内的位置
        offset: usize,
    },
    // 文件终止符，即文件的最后
    TKEof {
        offset: usize,
    },
}

impl Token {
    pub fn get_offset(&self) -> usize {
        match self {
            Self::TKPunct { t_str: _t_str, offset } => *offset,
            Self::TKNum { val: _val, t_str: _t_str, offset } => *offset,
            Self::TKEof { offset } => *offset,
        }
    }

    pub fn at_eof(&self) -> bool {
        match self {
            Self::TKEof { offset: _offset } => true,
            _ => false
        }
    }

    pub fn equal(&self, s: &str) -> bool {
        match self {
            Token::TKPunct { t_str, offset: _offset } => t_str.eq(s),
            Token::TKNum { val: _val, t_str, offset: _offset } => t_str.eq(s),
            _ => false
        }
    }
}

/// 跳过特定字符的token
fn skip(token: &Token, s: &str, pos: &mut usize) {
    if !token.equal(s) {
        error_token!(token, "expect {}", s)
    }
    *pos += 1;
}

/// 终结符解析
fn tokenize() -> Vec<Token> {
    let input = unsafe { &INPUT };

    let mut tokens: Vec<Token> = vec![];

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
            let old_pos = pos;
            let val = strtol(&chars, &mut pos, 10) as i32;
            // 使用vec和copy_from_slice 解决cannot move的问题
            let mut dst = vec![0; pos - old_pos];
            dst.copy_from_slice(&chars[old_pos..pos]);
            let t_str = String::from_utf8(dst).unwrap();
            let offset = old_pos;
            let t = Token::TKNum { val, t_str, offset };
            tokens.push(t);
            continue;
        }

        // 解析操作符
        let old_pos = pos;
        read_punct(&chars, &mut pos);
        if pos != old_pos {
            // 使用vec和copy_from_slice 解决cannot move的问题
            let mut dst = vec![0; pos - old_pos];
            dst.copy_from_slice(&chars[old_pos..pos]);
            let t_str = String::from_utf8(dst).unwrap();
            tokens.push(Token::TKPunct { t_str, offset: pos });
            continue;
        }

        // 处理无法识别的字符
        error_at!(pos, "invalid token");
    }

    // 解析结束，增加一个EOF，表示终止符。
    tokens.push(Token::TKEof { offset: pos });

    // Head无内容，所以直接返回Next
    tokens
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

fn starts_with(chars: &Vec<u8>, pos: usize, sub: &str) -> bool {
    let sub = sub.as_bytes();
    for i in 0..sub.len() {
        if sub[i] != chars[pos + i] {
            return false;
        }
    }
    true
}

fn read_punct(chars: &Vec<u8>, pos: &mut usize) {
    if starts_with(chars, *pos, "==")
        || starts_with(chars, *pos, "!=")
        || starts_with(chars, *pos, ">=")
        || starts_with(chars, *pos, "<=") {
        *pos += 2;
    }

    let c = chars[*pos] as char;
    if c.is_ascii_punctuation() {
        *pos += 1;
    }
}

//
// 生成AST（抽象语法树），语法解析
//

// AST的节点种类
#[derive(Eq, PartialEq)]
enum NodeKind {
    // +
    NdAdd,
    // -
    NdSub,
    // *
    NdMul,
    // /
    NdDiv,
    // 负号-
    NdNeg,
    // ==
    NdEq,
    // !=
    NdNe,
    // <
    NdLt,
    // <=
    NdLe,
    // 整形
    NdNum,
}

// AST中二叉树节点
struct Node {
    // 节点种类
    kind: NodeKind,
    // 左部，left-hand side
    lhs: Option<Box<Node>>,
    // 右部，right-hand side
    rhs: Option<Box<Node>>,
    // 存储ND_NUM种类的值
    val: i32,
}

impl Node {
    fn new(kind: NodeKind) -> Self {
        Node { kind, lhs: None, rhs: None, val: 0 }
    }

    fn new_binary(kind: NodeKind, lhs: Node, rhs: Node) -> Self {
        Node { kind, lhs: Some(Box::new(lhs)), rhs: Some(Box::new(rhs)), val: 0 }
    }

    fn new_unary(kind: NodeKind, expr: Node) -> Self {
        let mut node = Node::new(kind);
        node.lhs = Some(Box::new(expr));
        node
    }

    fn new_num(val: i32) -> Self {
        let mut node = Node::new(NodeKind::NdNum);
        node.val = val;
        node
    }
}

// 解析表达式
// expr = equality
fn expr(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    equality(pos, tokens)
}

// 解析相等性
// equality = relational ("==" relational | "!=" relational)*
fn equality(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // relational
    let mut node = relational(pos, tokens);

    // ("==" relational | "!=" relational)*
    loop {
        let token = &tokens[*pos];
        // "==" relational
        if token.equal("==") {
            *pos += 1;
            let rhs = relational(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdEq, node.unwrap(), rhs));
            continue;
        }

        // "!=" relational
        if token.equal("!=") {
            *pos += 1;
            let rhs = relational(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdNe, node.unwrap(), rhs));
            continue;
        }

        return node;
    }
}

// 解析比较关系
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn relational(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // add
    let mut node = add(pos, tokens);

    // ("<" add | "<=" add | ">" add | ">=" add)*
    loop {
        let token = &tokens[*pos];
        // "<" add
        if token.equal("<") {
            *pos += 1;
            let rhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLt, node.unwrap(), rhs));
            continue;
        }

        // "<=" add
        if token.equal("<=") {
            *pos += 1;
            let rhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLe, node.unwrap(), rhs));
            continue;
        }

        // ">" add
        // X>Y等价于Y<X
        if token.equal(">") {
            *pos += 1;
            let lhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLt, lhs, node.unwrap()));
            continue;
        }

        // ">=" add
        // X>=Y等价于Y<=X
        if token.equal(">=") {
            *pos += 1;
            let lhs = add(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdLe, lhs, node.unwrap()));
            continue;
        }

        return node;
    }
}

// 解析加减
// add = mul ("+" mul | "-" mul)*
fn add(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // mul
    let mut node = mul(pos, tokens);

    // ("+" mul | "-" mul)*
    loop {
        let token = &tokens[*pos];
        // "+" mul
        if token.equal("+") {
            *pos += 1;
            let rhs = mul(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdAdd, node.unwrap(), rhs));
            continue;
        }

        // "-" mul
        if token.equal("-") {
            *pos += 1;
            let rhs = mul(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdSub, node.unwrap(), rhs));
            continue;
        }

        return node;
    }
}

// 解析乘除
// mul = unary ("*" unary | "/" unary)*
fn mul(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    // unary
    let mut node = unary(pos, tokens);

    // ("*" unary | "/" unary)*
    loop {
        let token = &tokens[*pos];
        // "*" unary
        if token.equal("*") {
            *pos += 1;
            let rhs = unary(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdMul, node.unwrap(), rhs));
            continue;
        }

        // "/" unary
        if token.equal("/") {
            *pos += 1;
            let rhs = unary(pos, tokens).unwrap();
            node = Some(Node::new_binary(NodeKind::NdDiv, node.unwrap(), rhs));
            continue;
        }

        return node;
    }
}

// 解析一元运算
// unary = ("+" | "-") unary | primary
fn unary(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    let token = &tokens[*pos];

    // "+" unary
    if token.equal("+") {
        *pos += 1;
        return unary(pos, tokens);
    }

    // "-" unary
    if token.equal("-") {
        *pos += 1;
        return Some(Node::new_unary(NodeKind::NdNeg, unary(pos, tokens).unwrap()));
    }

    // primary
    primary(pos, tokens)
}

// 解析括号、数字
// primary = "(" expr ")" | num
fn primary(pos: &mut usize, tokens: &Vec<Token>) -> Option<Node> {
    let token = &tokens[*pos];
    if token.equal("(") {
        *pos += 1;
        let node = expr(pos, tokens);
        skip(&tokens[*pos], ")", pos);
        return node;
    }
    match token {
        Token::TKNum { val, t_str: _t_str, offset: _offset } => {
            let node = Node::new_num(*val);
            *pos += 1;
            return Some(node);
        }
        _ => {}
    }

    error_token!(&tokens[*pos], "expected an expression");

    None
}

/// 压栈，将结果临时压入栈中备用
/// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
/// 当前栈指针的地址就是sp，将a0的值压入栈
/// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
fn push(depth: &mut usize) {
    print!("  addi sp, sp, -8\n");
    print!("  sd a0, 0(sp)\n");
    *depth += 1;
}

/// 弹栈，将sp指向的地址的值，弹出到a1
fn pop(reg: &str, depth: &mut usize) {
    print!("  ld {}, 0(sp)\n", reg);
    print!("  addi sp, sp, 8\n");
    *depth -= 1;
}

/// 生成表达式
fn gen_expr(node: &Box<Node>, depth: &mut usize) {
    match node.kind {
        // 加载数字到a0
        NodeKind::NdNum => {
            print!("  li a0, {}\n", node.val);
            return;
        }
        // 对寄存器取反
        NodeKind::NdNeg => {
            gen_expr(node.lhs.as_ref().unwrap(), depth);
            // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
            print!("  neg a0, a0\n");
            return;
        }
        _ => {}
    }
    if node.kind == NodeKind::NdNum {
        print!("  li a0, {}\n", node.val);
        return;
    }

    // 递归到最右节点
    gen_expr(node.rhs.as_ref().unwrap(), depth);
    // 将结果压入栈
    push(depth);
    // 递归到左节点
    gen_expr(node.lhs.as_ref().unwrap(), depth);
    // 将结果弹栈到a1
    pop("a1", depth);

    // 生成各个二叉树节点
    match node.kind {
        NodeKind::NdAdd => {
            // + a0=a0+a1
            print!("  add a0, a0, a1\n");
            return;
        }
        NodeKind::NdSub => {
            // - a0=a0-a1
            print!("  sub a0, a0, a1\n");
            return;
        }
        NodeKind::NdMul => {
            // * a0=a0*a1
            print!("  mul a0, a0, a1\n");
            return;
        }
        NodeKind::NdDiv => {
            // / a0=a0/a1
            print!("  div a0, a0, a1\n");
            return;
        }
        NodeKind::NdEq => {
            // a0=a0^a1，异或指令
            print!("  xor a0, a0, a1\n");
            // a0==a1
            // a0=a0^a1, sltiu a0, a0, 1
            // 等于0则置1
            print!("  seqz a0, a0\n");
            return;
        }
        NodeKind::NdNe => {
            // a0=a0^a1，异或指令
            print!("  xor a0, a0, a1\n");
            // a0!=a1
            // a0=a0^a1, sltu a0, x0, a0
            // 不等于0则置1
            print!("  snez a0, a0\n");
            return;
        }
        NodeKind::NdLt => {
            print!("  slt a0, a0, a1\n");
            return;
        }
        NodeKind::NdLe => {
            // a0<=a1等价于
            // a0=a1<a0, a0=a1^1
            print!("  slt a0, a1, a0\n");
            print!("  xori a0, a0, 1\n");
            return;
        }
        _ => {}
    }

    panic!("invalid expression");
}