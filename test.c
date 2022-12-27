
//typedef char T60[];
//T60 g60 = {1, 2, 3};
//T60 g61 = {1, 2, 3, 4, 5, 6};

typedef struct {char a, b[];} T65;
T65 g65 = {'f', 'o', 'o', 0};
T65 g66 = {'f', 'o', 'o', 'b', 'a', 'r', 0};

int main() {
    g66.b[0];
}