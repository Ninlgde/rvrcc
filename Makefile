# C编译器参数：使用C11标准，生成debug信息，禁止将未初始化的全局变量放入到common段
CFLAGS=-std=c11 -g -fno-common
# 构建测试项目用的riscv64-unknown-linux-gnu-gcc编译器
CC=riscv64-unknown-linux-gnu-gcc
# test/文件夹的c测试文件
TEST_SRCS=$(wildcard test/*.c)
# test/文件夹的c测试文件编译出的可执行文件
TESTS=$(TEST_SRCS:.c=.exe)
# riscv目录
RISCV=/Users/malikma/Desktop/source/opt/riscv_linux
# run
RUN=spike --isa=rv64gc $(RISCV)/riscv64-unknown-linux-gnu/bin/pk

# rvcc的代码
# C源代码文件，表示所有的.c结尾的文件
SRCS=$(wildcard ../rvcc/*.c)
STAGE2_SRCS=$(SRCS:../rvcc/%.c=%.c)
# C文件编译生成的未链接的可重定位文件，将所有.c文件替换为同名的.o结尾的文件名
OBJS=$(STAGE2_SRCS:.c=.o)

# Stage 1

# 所有的可重定位文件依赖于rvcc.h的头文件
$(OBJS): ../rvcc/rvcc.h

# rvrcc标签，使用cargo build 创建可执行文件
rvrcc:
	@cargo build --release

# 只使用rvcc进行宏的测试
test/macro.exe: rvrcc test/macro.c
	./target/release/rvrcc -c -o test/macro.o test/macro.c
	$(CC) -static -o $@ test/macro.o -xc test/common
	$(RUN) ./test/macro.exe
#	$(RISCV)/bin/riscv64-unknown-linux-gnu-gcc -o $@ test/macro.o -xc test/common

# 测试标签，运行测试
test/%.exe: rvrcc test/%.c
	./target/release/rvrcc -c -o test/$*.o test/$*.c
	$(CC) -static -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo "\033[34m"$$i"\033[0m"; $(RUN) ./$$i || exit 1; echo; done
	test/driver.sh ./target/release/rvrcc

test-all: test test-stage2

# Stage 2 虽然我不能自举rust,但是我可以举rvcc啊

# 此时构建的stage2/rvcc是RISC-V版本的，跟平台无关
$(STAGE2_SRCS):
	mkdir -p stage2/test
	cp ../rvcc/$*.c stage2/$*.c
	cp ../rvcc/rvcc.h stage2/rvcc.h

stage2: $(STAGE2_SRCS) stage2/rvcc

stage2/rvcc: $(OBJS:%=stage2/%)
	$(CC) $(CFLAGS) -static -o $@ $^

# 利用stage1的rvcc去将rvcc的源代码编译为stage2的可重定位文件
stage2/%.o: rvrcc self.py stage2/%.c
	mkdir -p stage2/test
	./self.py stage2/rvcc.h stage2/$*.c > stage2/$*.c2
	mv stage2/$*.c2 stage2/$*.c
	./target/release/rvrcc -c -o stage2/$*.o stage2/$*.c -###

# 只使用stage2的rvcc进行宏的测试
stage2/test/macro.exe: stage2 test/macro.c
	mkdir -p stage2/test
	$(RUN) ./stage2/rvcc -c test/macro.c -cc1 -cc1-input test/macro.c -cc1-output stage2/test/macro.s
	$(CC) -static -o $@ stage2/test/macro.s -xc test/common
	$(RUN) ./stage2/test/macro.exe

# 利用stage2的rvcc去进行测试
stage2/test/%.exe: stage2 test/%.c
	$(CC) -o stage2/test/$*.c -E -P -C test/$*.c
	$(RUN) ./stage2/rvcc -c stage2/test/$*.c -cc1 -cc1-input stage2/test/$*.c -cc1-output stage2/test/$*.s
	$(CC) -static -o $@ stage2/test/$*.s -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i;  $(RUN) ./$$i || exit 1; echo; done
	test/driver.sh "$(RUN) ./stage2/rvcc -cc1"

# 清理标签，清理所有非源代码文件
clean:
	rm -rf rvcc tmp* $(TESTS) test/*.s test/*.exe stage2/
	find * -type f '(' -name '*~' -o -name '*.o' -o -name '*.s' ')' -exec rm {} ';'
	@cargo clean

# 伪目标，没有实际的依赖文件
.PHONY: test clean
