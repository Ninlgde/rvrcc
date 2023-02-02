#include "test.h"

int main() {
    printf("[254] 支持typeof\n");
    ASSERT(3, ({ typeof(int) x=3; x; }));
    ASSERT(3, ({ typeof(1) x=3; x; }));
    ASSERT(4, ({ int x; typeof(x) y; sizeof(y); }));
    ASSERT(8, ({ int x; typeof(&x) y; sizeof(y); }));
    ASSERT(4, ({ typeof("foo") x; sizeof(x); }));
    ASSERT(12, sizeof(typeof(struct { int a,b,c; })));

    printf("\033[32mOK\033[0m\n");
    return 0;
}
