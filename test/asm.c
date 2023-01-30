#include "test.h"

// [259] 支持asm语句
static inline char *asm_fn1(void) {
    asm("li a0, 50\n\t");
}

static inline char *asm_fn2(void) {
    asm inline volatile("li a0, 55\n\t");
}

static inline char *asm_fn3(void) {
    return asm_fn1();
}

static inline char *asm_fn4(void) {
    return asm_fn3();
}

static inline char *asm_fn5(void) {
    return asm_fn4();
}

int main() {
    printf("[259] 支持asm语句\n");
    ASSERT(50, asm_fn1());
    ASSERT(55, asm_fn2());
    ASSERT(50, asm_fn3());

    printf("OK\n");
    return 0;
}
