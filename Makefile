# 构建测试项目用的riscv64-unknown-elf-gcc编译器
CC=riscv64-unknown-elf-gcc
# 运行器riscv64-unknown-elf-run
RUN=riscv64-unknown-elf-run
# test/文件夹的c测试文件
TEST_SRCS=$(wildcard test/*.c)
# test/文件夹的c测试文件编译出的可执行文件
TESTS=$(TEST_SRCS:.c=.exe)

# Stage 1

# rvrcc标签，使用cargo build 创建可执行文件
rvrcc:
	@cargo build --release

# 测试标签，运行测试
test/%.exe: rvrcc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./target/release/rvrcc -o test/$*.s -
	$(CC) -static -o $@ test/$*.s -xc test/common

test: $(TESTS)
	for i in $^; do echo "\033[34m"$$i"\033[0m"; $(RUN) ./$$i || exit 1; echo; done
	test/driver.sh ./target/release/rvrcc

test-all: test test-stage2

# Stage 2

# 此时构建的stage2/rvcc是RISC-V版本的，跟平台无关
stage2/rvcc: $(OBJS:%=stage2/%)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# 利用stage1的rvcc去将rvcc的源代码编译为stage2的汇编文件
stage2/%.s: rvcc self.py %.c
	mkdir -p stage2/test
	./self.py rvcc.h $*.c > stage2/$*.c
	./target/release/rvrcc -o stage2/$*.s stage2/$*.c

# stage2的汇编编译为可重定位文件
stage2/%.o: stage2/%.s
	$(CC) -c stage2/$*.s -o stage2/$*.o

# 利用stage2的rvcc去进行测试
stage2/test/%.exe: stage2/rvcc test/%.c
	mkdir -p stage2/test
	$(CC) -o- -E -P -C test/$*.c | ./stage2/rvcc -o stage2/test/$*.s -
	$(CC) -o $@ stage2/test/$*.s -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./stage2/rvcc

# 清理标签，清理所有非源代码文件
clean:
	rm -rf rvrcc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' -o -name '*.s' ')' -exec rm {} ';'
	@cargo clean

# 伪目标，没有实际的依赖文件
.PHONY: test clean
