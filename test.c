
int main() {
    struct T {
        struct T *next;
        int x;
    } a;
    struct T b;
    b.x = 1;
    a.next = &b;
    a.next->x;
}