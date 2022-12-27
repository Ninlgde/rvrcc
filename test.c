/**
 *
 *
 *
 *
 */
// [105] 支持全局变量初始化器
//char g3 = 3;
//short g4 = 4;
//int g5 = 5;
//long g6 = 6666666;
//char g7[] = "dsfsfsfsfsfsf";
//long g8[] = {1, 2, 5, 77777777777};
//
//// [106] 为结构体支持全局变量初始化器
//int g9[3] = {0, 1, 2};
//struct {char a; int b;} g11[2] = {{1, 2}, {3, 4}};
//struct {int a[2];} g12[2] = {{{1, 2}}};

struct { struct { int a[3]; } a; } g30 = {{{1,2,3}}};
int *g31=g30.a.a;

int main() {
    union {
        struct {
            char a, b, c, d;
        } e;
        int f;
    } x = {{4, 3, 2, 1}};
    x.f;
}