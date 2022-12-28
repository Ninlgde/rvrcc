
// [121] 支持复合字面量
typedef struct Tree {
    int val;
    struct Tree *lhs;
    struct Tree *rhs;
} Tree;

Tree *tree = &(Tree){
        1,
        &(Tree){
                2,
                &(Tree){ 3, 0, 0 },
                &(Tree){ 4, 0, 0 }
        },
        0
};

int main() {
    tree->val;
}