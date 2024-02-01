// [179] 使用内建的预处理器用于所有测试
#include "test.h"

// [176] 宏函数中只展开一次
int dbl2(int x, int y, int z) { return x * x + y - z; }

typedef long __int64_t;
typedef __int64_t       __darwin_blkcnt_t;

int main() {
#define M32 ((sizeof(char) * 8 / 7) + 1)
#define M33(x, y, z) dbl2(x,y,z*sizeof(y))
    ASSERT(87, M33(10, M32 + 1, 2));

    printf("\033[32mOK\033[0m\n");
}