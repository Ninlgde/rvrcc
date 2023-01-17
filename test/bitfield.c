#include "test.h"

// [211] 支持全局结构体位域初始化器
struct {
    char a;
    int b : 5;
    int c : 10;
} g45 = {1, 2, 3}, g46={};

int main() {
    printf("[210] 支持位域\n");
    ASSERT(4, sizeof(struct {int x:1; }));
    ASSERT(8, sizeof(struct {long x:1; }));

    struct bit1 {
        short a;
        char b;
        int c : 2;
        int d : 3;
        int e : 3;
    };

    ASSERT(4, sizeof(struct bit1));
    ASSERT(1, ({ struct bit1 x; x.a=1; x.b=2; x.c=3; x.d=4; x.e=5; x.a; }));
    ASSERT(1, ({ struct bit1 x={1,2,3,4,5}; x.a; }));
    ASSERT(2, ({ struct bit1 x={1,2,3,4,5}; x.b; }));
    ASSERT(-1, ({ struct bit1 x={1,2,3,4,5}; x.c; }));
    ASSERT(-4, ({ struct bit1 x={1,2,3,4,5}; x.d; }));
    ASSERT(-3, ({ struct bit1 x={1,2,3,4,5}; x.e; }));

    printf("[211] 支持全局结构体位域初始化器\n");
    ASSERT(1, g45.a);
    ASSERT(2, g45.b);
    ASSERT(3, g45.c);

    ASSERT(0, g46.a);
    ASSERT(0, g46.b);
    ASSERT(0, g46.c);

    printf("[212] 支持op=风格的位域赋值\n");
    typedef struct {
        int a : 10;
        int b : 10;
        int c : 10;
    } T3;

    ASSERT(1, ({ T3 x={1,2,3}; x.a++; }));
    ASSERT(2, ({ T3 x={1,2,3}; x.b++; }));
    ASSERT(3, ({ T3 x={1,2,3}; x.c++; }));

    ASSERT(2, ({ T3 x={1,2,3}; ++x.a; }));
    ASSERT(3, ({ T3 x={1,2,3}; ++x.b; }));
    ASSERT(4, ({ T3 x={1,2,3}; ++x.c; }));

    ASSERT(3, ({ T3 x={1,2,3}; x.a+=2; }));
    ASSERT(4, ({ T3 x={1,2,3}; x.b*=2; }));
    ASSERT(1, ({ T3 x={1,2,3}; x.c/=3; }));

    printf("OK\n");
    return 0;
}
