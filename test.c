
int fib(int x) {
    if (x<=1)
        return 1;
    return fib(x-1) + fib(x-2);
}

int main() { int a = 5, b = 3; fib(a+b); }