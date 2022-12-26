
int main() {
    struct {
        int a;
        int b;
        short c[4];
    } x[2] = {{1, 2, "ssss"}};
    sizeof(x[0].c);
}