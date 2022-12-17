# 构建测试项目用的riscv64-unknown-elf-gcc编译器
CC=riscv64-unknown-elf-gcc
# 运行器riscv64-unknown-elf-run
RUN=riscv64-unknown-elf-run
# test/文件夹的c测试文件
TEST_SRCS=$(wildcard test/*.c)
# test/文件夹的c测试文件编译出的可执行文件
TESTS=$(TEST_SRCS:.c=.exe)

# rvrcc标签，使用cargo build 创建可执行文件
rvrcc:
	@cargo build --release

# 测试标签，运行测试
test/%.exe: rvrcc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./target/release/rvrcc -o test/$*.s -
	$(CC) -static -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; $(RUN) ./$$i || exit 1; echo; done
	test/driver.sh

# 清理标签，清理所有非源代码文件
clean:
	rm -rf rvrcc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' -o -name '*.s' ')' -exec rm {} ';'
	@cargo clean

# 伪目标，没有实际的依赖文件
.PHONY: test clean
