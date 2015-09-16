/* an inefficient implementation of exact Gaussian elimination where the
   coefficients are all integers

   note that we use `long long` to represent numerators/denominators, so if it
   overflows, you're screwed.
 */
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef long long num;
#define PRI_num "lli"
#define abs_num(x) llabs(x)

/* calculate: x <- alpha * x */
void scal(size_t n, num alpha, num *x)
{
    size_t i;
    for (i = 0; i != n; ++i) {
        x[i] *= alpha;
    }
}

/* calculate: y <- alpha * x + y */
void axpy(size_t n, num alpha, const num *x, num *y)
{
    size_t i;
    for (i = 0; i != n; ++i) {
        y[i] += alpha * x[i];
    }
}

/* calculate: x, y <- y, x */
void swap(size_t n, num *x, num *y)
{
    size_t i;
    if (x == y) {
        return;
    }
    for (i = 0; i != n; ++i) {
        const num t = x[i];
        x[i] = y[i];
        y[i] = t;
    }
}

/* elementary row operation: scale a row by some factor */
void el_scale(size_t n, num *equation, size_t r, num c)
{
    if (!c) {
        fprintf(stderr, "multiplying a row by 0 is disallowed\n");
        abort();
    }
    scal(n + 1, c, equation + r * (n + 1));
}

/* elementary row operation: add a multiple of one row to another */
void el_add(size_t n, num *equation, size_t r2, size_t r1, num c)
{
    axpy(n + 1, c, equation + r1 * (n + 1), equation + r2 * (n + 1));
}

/* elementary row operation: swap two rows */
void el_swap(size_t n, num *equation, size_t r2, size_t r1)
{
    swap(n + 1, equation + r1 * (n + 1), equation + r2 * (n + 1));
}

void print_equation(size_t n, const num *equation)
{
    size_t i, j;
    for (i = 0; i != n; ++i) {
        for (j = 0; j != n + 1; ++j) {
            printf("%15" PRI_num " ", equation[i * (n + 1) + j]);
        }
        printf("\n");
    }
    printf("\n");
}

int all_divisible_by(size_t n, const num *x, num m)
{
    size_t i;
    for (i = 0; i != n; ++i) {
        if (x[i] % m) {
            return 0;
        }
    }
    return 1;
}

size_t smallest_abs_nonzero(size_t n, const num *x)
{
    size_t i, j = (size_t)(-1);
    for (i = 0; i != n; ++i) {
        if (x[i] && (j == (size_t)(-1) ||
                     abs_num(x[i]) < abs_num(x[j]))) {
            j = i;
        }
    }
    return j;
}

void remove_common_factors(size_t n, num *x)
{
    const size_t imin = smallest_abs_nonzero(n, x);
    size_t i;
    num m;

    if (imin == (size_t)(-1)) {
        return;
    }

    /* not the most efficient algorithm */
    for (m = 2; m <= abs_num(x[imin]); ++m) {
        while (all_divisible_by(n, x, m)) {
            size_t i;
            for (i = 0; i != n; ++i) {
                x[i] /= m;
            }
        }
    }

    m = 0;
    for (i = n - 1; i < n; --i) {
        if (x[i] && !m) {
            m = x[i] > 0 ? 1 : -1;
        }
        x[i] *= m;
    }
}

/* remove common factors */
void simplify_row(size_t n, num *equation, size_t i)
{
    remove_common_factors(n + 1, equation + (n + 1) * i);
}

void eliminate(size_t n, num *equation, size_t i, size_t j, size_t k)
{
    num c[] = {
        equation[i * (n + 1) + k],
        equation[j * (n + 1) + k]
    };

    remove_common_factors(sizeof(c) / sizeof(*c), c);
    if (c[0]) {
        el_scale(n, equation, j, c[0]);
        el_add(n, equation, j, i, -c[1]);
    }
    simplify_row(n, equation, j);

#ifndef NDEBUG
    print_equation(n, equation);
#endif
}

size_t find_first_nonzero(size_t n, const num *x, size_t incx)
{
    size_t i, j = (size_t)(-1);
    for (i = 0; i != n; ++i) {
        if (x[i * incx]) {
            j = i;
            break;
        }
    }
    return j;
}

/* reduce to row echelon form */
void row_echelon(size_t n, num *equation)
{
    size_t i, j;
    for (i = 0; i != n; ++i) {
        const size_t k = find_first_nonzero(
            n - i,
            equation + i * (n + 1) + i,
            n + 1
        );
        if (k == (size_t)(-1)) {
            continue;
        }
        el_swap(n, equation, i, i + k);
        for (j = i + 1; j < n; ++j) {
            eliminate(n, equation, i, j, i);
        }
    }
}

/* solve a linear equation whose coefficient matrix is upper triangular */
void solve_triangular(size_t n, num *equation)
{
    size_t i, j;
    for (i = n - 1; i < n; --i) {
        for (j = i - 1; j < i; --j) {
            eliminate(n, equation, i, j, i);
        }
    }
}

/* Solve a linear system of equations defined by an augmented matrix;
   `equation` is a row-major augmented matrix with dimensions `n * (n + 1)`;
   The final solution is returned as diagonal augmented matrix. */
void gauss_elim(size_t n, num *equation)
{
#ifndef NDEBUG
    printf("initial:\n");
    print_equation(n, equation);
#endif
    row_echelon(n, equation);
    solve_triangular(n, equation);
#ifndef NDEBUG
    printf("final:\n");
    print_equation(n, equation);
#endif
}

void get_solution(num *numer, num *denom,
                  size_t n, const num *equation, size_t i)
{
    *numer = equation[i * (n + 1) + n];
    *denom = equation[i * (n + 1) + i];
}

/* for compatibilty with Treeki's code */
int solve(const num *equation, num *solutions, int n)
{
    const size_t size = n * (n + 1) * sizeof(*equation);
    size_t i;
    int is_unique = 1;
    num *equation2 = (num *)malloc(size);
    memcpy(equation2, equation, size);
    gauss_elim((size_t)n, equation2);
    for (i = 0; i != n; ++i) {
        num numer, denom;
        get_solution(&numer, &denom, n, equation2, i);
        if (denom) {
            solutions[i] = numer / denom;
        } else {
            solutions[i] = 0;
            fprintf(stderr, "warning: ");
            if (numer) {
                fprintf(stderr,
                        "solution for variable #%zu does not exist\n", i);
            } else {
                fprintf(stderr,
                        "multiple solutions exist for #%zu\n", i);
            }
            is_unique = 0;
        }
    }
    free(equation2);
    return is_unique;
}

void print_solutions(size_t n, const num *equation)
{
    size_t i;
    printf("solutions:\n");
    for (i = 0; i != n; ++i) {
        num numer, denom;
        get_solution(&numer, &denom, n, equation, i);
        printf("%15f\n", (double)numer / denom);
    }
}

void example(void)
{
#define M 4
    static const size_t n = M;
    num equation[M * (M + 1)] = {
        12, 8,  1,  1,  69,
        10, 13, 10, 2,  14,
        15, 9,  2,  7,  194,
        5,  10, 13, 14, 193
    };
    gauss_elim(n, equation);
    print_solutions(n, equation);
}

int main(void)
{
    example();
    return 0;
}
