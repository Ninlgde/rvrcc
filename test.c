
int sprintf(char *buf, char *fmt, ...);
int strcmp(char *p, char *q);

int main() { char buf[100]; sprintf(buf, "%.1f", (float)3.5); strcmp(buf, "3.5"); }