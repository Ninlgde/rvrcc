
int g1;

int *g1_ptr() { return &g1; }

int main() { *g1_ptr(); }