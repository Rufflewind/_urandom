/* compiled with: gcc -std=c99 -O3 -funroll-loops
   Blas:   0.75s (single-threaded OpenBlas)
   manual: 0.90s
 */
#include <stdio.h>
#include <cblas.h>
#define NOINLINE __attribute__ ((noinline))
#define N 100000

typedef size_t uint;

NOINLINE void proc(uint n, double *restrict y, double const *restrict dy, double dt) {
    uint i;
#if 1
    cblas_daxpy(n, dt, dy, 1, y, 1);
#else
    for (i = 0; i != n; ++i)
        y[i] += dy[i] * dt;
#endif
}

int main(void) {
    uint i;
    double y[N], dy[N], dt = 0.1, sum = 0;

    for (i = 0; i != N; ++i) {
        y[i] = 1 / (i + 1);
        dy[i] = i;
    }

    for (i = 0; i != 10000; ++i) {
        proc(N, y, dy, dt);
    }

    for (i = 0; i != N; ++i) {
        sum += y[i];
    }

    printf("%f\n", sum);

    return 0;
}
