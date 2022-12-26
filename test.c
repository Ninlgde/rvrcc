/**
 *
 *
 *
 *
 *
 */
// [105] 支持全局变量初始化器
char g3 = 3;
short g4 = 4;
int g5 = 5;
long g6 = 6666666;
char g7[] = "dsfsfsfsfsfsf";
long g8[] = {1, 2, 5, 77777777777};

int main() {
    union {
        struct {
            char a, b, c, d;
        } e;
        int f;
    } x = {{4, 3, 2, 1}};
    x.f;
}