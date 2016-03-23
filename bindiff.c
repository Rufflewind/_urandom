#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#undef BUFSIZ
#define BUFSIZ (1024 * 8)
#define SIZE 4

static int check(const void *ptr1, const void *ptr2, size_t size,
                 int same, size_t pos)
{
    if (size) {
        int equal = !memcmp(ptr1, ptr2, size);
        if (equal && !same) {
            printf("Diff stop:  %15zu\n", pos);
            fflush(stdout);
            same = !same;
        } else if (!equal && same) {
            printf("Diff start: %15zu\n", pos);
            fflush(stdout);
            same = !same;
        }
    }
    return same;
}

static int compare(FILE *f1, FILE *f2, size_t pos)
{
    if (fseek(f1, pos, SEEK_CUR)) {
        abort();
    }
    if (fseek(f2, pos, SEEK_CUR)) {
        abort();
    }

    int same = 1;

    char buf1[BUFSIZ];
    char buf2[BUFSIZ];

    size_t n1, n2, n, m;

    do {

        n1 = fread(buf1, 1, BUFSIZ, f1);
        n2 = fread(buf2, 1, BUFSIZ, f2);
        n = n1 < n2 ? n1 : n2;
        m = n / SIZE * SIZE;

        for (size_t i = 0; i != m; i += SIZE) {
            same = check(buf1 + i, buf2 + i, SIZE, same, pos + i);
        }
        same = check(buf1 + m, buf2 + m, n - m, same, pos + m);
        pos += n;

    } while (n == BUFSIZ);

    if (n1 < n2) {
        printf("A ended early at: %zu\n", pos);
    } else if (n2 < n1) {
        printf("B ended early at: %zu\n", pos);
    } else {
        printf("A and B both ended at: %zu\n", pos);
    }

    return 0;
}

int main(int argc, char **argv)
{
    const char *fn1 = argv[1];
    const char *fn2 = argv[2];
    size_t pos = 0;
    if (argc >= 4) {
        pos = atoll(argv[3]);
    }

    FILE *f1 = fopen(fn1, "rb");
    FILE *f2 = fopen(fn2, "rb");

    compare(f1, f2, pos);

    fclose(f1);
    fclose(f2);
    return 0;
}
