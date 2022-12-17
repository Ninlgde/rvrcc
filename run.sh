#!/bin/bash

# 如果输入有错误,会将编译信息直接写入tmp.s中
print_error() {
  # 所以直接输出tmp.s
  cat tmp.s
  # 并退出shell
  exit
}

# 使用rvrcc变异输入,并将汇编代码输入到tmp.s中
# 如果运行不成功，则会执行print_error并退出。成功时会短路
./target/release/rvrcc test.c > tmp.s || print_error

# 使用riscv64-gcc编译tmp.s成可执行文件tmp
riscv64-unknown-elf-gcc -o tmp tmp.s

# 使用riscv64-unknown-elf-run 直接运行编译好的可执行文件tmp
riscv64-unknown-elf-run ./tmp

# 获取实际输出
actual="$?"

# 打印相关结果
echo "run input, got result: $actual"