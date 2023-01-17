#!/bin/bash
rvrcc=$1

# riscv目录
RISCV=/Users/malikma/Desktop/source/opt/riscv_linux
# run
RUN="spike --isa=rv64gc $RISCV/riscv64-unknown-linux-gnu/bin/pk"

# 创建一个临时文件夹，XXXXXX会被替换为随机字符串
tmp=$(mktemp -d /tmp/rvcc-test-XXXXXX)
# 清理工作
# 在接收到 中断（ctrl+c），终止，挂起（ssh掉线，用户退出），退出 信号时
# 执行rm命令，删除掉新建的临时文件夹
trap 'rm -rf $tmp' INT TERM HUP EXIT
# 在临时文件夹内，新建一个空文件，名为empty.c
echo >$tmp/empty.c

# 判断返回值是否为0来判断程序是否成功执行
check() {
  if [ $? -eq 0 ]; then
    echo -e "testing $1 ... \033[32m passed \033[0m"
  else
    echo -e "testing $1 ... \033[31m failed \033[0m"
    exit 1
  fi
}

# -o
# 清理掉$tmp中的out文件
rm -f $tmp/out
# 编译生成out文件
$rvrcc -c -o $tmp/out $tmp/empty.c
# 条件判断，是否存在out文件
[ -f $tmp/out ]
# 将-o传入check函数
check -o

# --help
# 将--help的结果传入到grep进行 行过滤
# -q不输出，是否匹配到存在rvrcc字符串的行结果
$rvrcc --help 2>&1 | grep -q -E "rv(r)*cc"
# 将--help传入check函数
check --help

# -S -q应该是会直接打断进程所以先grep一下,再grep-q不打印结果
echo 'int main() {}' | $rvrcc -S -o- - | grep 'main:' | grep -q 'main'
check -S

# 默认输出的文件
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' >$tmp/out.c
($rvrcc -c $tmp/out.c >$tmp/out.o)
[ -f $tmp/out.o ]
check 'default output file'

($rvrcc -c -S $tmp/out.c >$tmp/out.s)
[ -f $tmp/out.s ]
check 'default output file'

# [156] 接受多个输入文件
rm -f $tmp/foo.o $tmp/bar.o
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $OLDPWD/$rvrcc -c $tmp/foo.c $tmp/bar.c)
[ -f $tmp/foo.o ] && [ -f $tmp/bar.o ]
check 'multiple input files'

rm -f $tmp/foo.s $tmp/bar.s
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $OLDPWD/$rvrcc -c -S $tmp/foo.c $tmp/bar.c)
[ -f $tmp/foo.s ] && [ -f $tmp/bar.s ]
check 'multiple input files'

# [157] 无-c时调用ld
# 调用链接器
#rm -f $tmp/foo
#echo 'int main() { return 0; }' | $rvrcc -o $tmp/foo -
#if [ "$RISCV" = "" ];then
#  $tmp/foo
#else
#  $RUN $tmp/foo
#fi
#check linker

rm -f $tmp/foo
echo 'int bar(); int main() { return bar(); }' > $tmp/foo.c
echo 'int bar() { return 42; }' > $tmp/bar.c
#$rvrcc -o $tmp/foo $tmp/foo.c $tmp/bar.c
#if [ "$RISCV" = "" ];then
#  $tmp/foo
#else
#  $RUN $tmp/foo
#fi
#[ "$?" = 42 ]
#check linker
#
# 生成a.out
rm -f $tmp/a.out
echo 'int main() {}' > $tmp/foo.c
(cd $tmp; $OLDPWD/$rvrcc foo.c)
[ -f $tmp/a.out ]
check a.out

# -E
# [162] 支持-E选项
echo foo > $tmp/out
echo "#include \"$tmp/out\"" | $rvrcc -E - | grep -q foo
check -E

echo foo > $tmp/out1
echo "#include \"$tmp/out1\"" | $rvrcc -E -o $tmp/out2 -
cat $tmp/out2 | grep -q foo
check '-E and -o'

# [185] 支持 -I<Dir> 选项
# -I
mkdir $tmp/dir
echo foo > $tmp/dir/i-option-test
echo "#include \"i-option-test\"" | $rvrcc -I$tmp/dir -E - | grep -q foo
check -I

# [208] 支持-D选项
# -D
echo foo | $rvrcc -Dfoo -E - | grep -q 1
check -D

# -D
echo foo | $rvrcc -Dfoo=bar -E - | grep -q bar
check -D

echo OK
