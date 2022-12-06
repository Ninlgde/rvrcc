use std::env;
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} invalid number of arguments", args[0]);
        exit(1);
    }

    // 声明一个全局main段，同时也是程序入口段
    print!("  .globl main\n");
    // main段标签
    print!("main:\n");
    // li为addi别名指令，加载一个立即数到寄存器中
    // 传入程序的参数为str类型，因为需要转换为需要int类型，
    // atoi为“ASCII to integer”
    print!("  li a0, {}\n", args[1].parse::<i32>().unwrap());
    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    print!("  ret\n");

    exit(0);
}
