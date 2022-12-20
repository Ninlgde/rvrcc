int main() {
    struct t {
        int a, b;
    };
    struct t x;
    x.a = 7;
    struct t y, *p = &x, *q = &y;
    *q = *p;
    y.a;
}