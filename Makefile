
# rvrcc标签，使用cargo build 创建可执行文件
rvrcc:
	@cargo build --release

# 测试标签，运行测试脚本
test: rvrcc
	./test.sh
	./test-driver.sh

# 清理标签，清理所有非源代码文件
clean:
	rm -f rvcc *.o *.s tmp* a.out
	@cargo clean

# 伪目标，没有实际的依赖文件
.PHONY: test clean
