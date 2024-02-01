#!/bin/bash
repo='https://github.com/lua/lua.git'
. test/thirdparty/common
#git reset --hard be908a7d4d8130264ad67c5789169769f824c5d1

sed -i '' -e 's/CC= gcc/CC?= gcc/' makefile
sed -i '' -e 's/RANLIB= ranlib/RANLIB= riscv64-unknown-linux-gnu-ranlib/' makefile

CC=$rvrcc $make all
./all
