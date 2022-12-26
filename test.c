
int main() {
    union {
        struct {
            char a, b, c, d;
        } e;
        int f;
    } x = {{4, 3, 2, 1}};
    x.f;
}