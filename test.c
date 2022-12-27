
struct {int a[2];} g40[2] = {{1, 2}, 3, 4};

int main() {
    union {
        struct {
            char a, b, c, d;
        } e;
        int f;
    } x = {{4, 3, 2, 1}};
    x.f;
}