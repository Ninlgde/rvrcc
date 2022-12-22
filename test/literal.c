#include "test.h"

int main() {
    // [73] 支持字符字面量
    ASSERT(97, 'a');
    ASSERT(10, '\n');
    ASSERT(127, '\x7f');

    printf("\033[32mOK\033[0m\n");
    return 0;
}
