use std::env;
use std::process::exit;
use core::str::Chars;
use std::borrow::BorrowMut;
use std::iter::Peekable;


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} invalid number of arguments", args[0]);
        exit(1);
    }

    let chars: Chars = args[1].chars();
    let mut chars: Peekable<Chars> = chars.peekable();

    // 声明一个全局main段，同时也是程序入口段
    print!("  .globl main\n");
    // main段标签
    print!("main:\n");
    // li为addi别名指令，加载一个立即数到寄存器中

    // 传入程序的参数为str类型，因为需要转换为需要long类型
    // strtol为“string to long”，
    // 参数为：被转换的str，str除去数字后的剩余部分，进制
    // 传入&p，即char**, 是为了修改P的值

    // 这里我们将算式分解为 num (op num) (op num)...的形式
    // 所以先将第一个num传入a0
    print!("  li a0, {}\n", strtol(chars.borrow_mut()));

    // 解析 (op num)
    // *P在这里判断P是否为NULL
    while chars.peek().is_some() {
        // 解析op
        if *chars.peek().unwrap() == '+' {
            chars.next(); // 跳过‘+’
            // 解析num
            // addi rd, rs1, imm 表示 rd = rs1 + imm
            print!("  addi a0, a0, {}\n", strtol(chars.borrow_mut()));
            continue;
        }

        if *chars.peek().unwrap() == '-' {
            chars.next();
            // 解析num
            // addi中imm为有符号立即数，所以减法表示为 rd = rs1 + (-imm)
            print!("  addi a0, a0, -{}\n", strtol(chars.borrow_mut()));
            continue;
        }

        // 如果存在未解析的字符，则报错
        eprintln!("unexpected character: '{:?}'\n", chars.peek());
    }

    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    print!("  ret\n");

    exit(0);
}

/// 传入程序的参数为str类型，因为需要转换为需要long类型
/// strtol为“string to long”，
/// 参数为：被转换的str，str除去数字后的剩余部分，进制
/// 传入&p，即char**, 是为了修改P的值
fn strtol(chars: &mut Peekable<Chars<'_>>) -> i64 {
    let mut result: i64 = 0;
    loop {
        match chars.peek() {
            Some(c) => {
                match c.to_digit(10) {
                    Some(i) => {
                        result = result * 10 + i64::from(i);
                        chars.next();
                    }
                    None => break,
                }
            }
            None => break,
        }
    }
    // println!("{:?}", chars.peek());
    result
}