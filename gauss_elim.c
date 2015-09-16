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

void print_augmatrix(size_t n, const num *equation)
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

/* eliminate cell (j, k) using row i */
void eliminate(size_t n, num *equation, size_t i, size_t j, size_t k)
{
    num c[] = {
        equation[i * (n + 1) + k],
        equation[j * (n + 1) + k]
    };

    remove_common_factors(sizeof(c) / sizeof(*c), c);
    el_scale(n, equation, j, c[0]);
    el_add(n, equation, j, i, -c[1]);
    simplify_row(n, equation, j);

#ifndef NDEBUG
    print_augmatrix(n, equation);
#endif
}

size_t find_first_nonzero(size_t n, const num *x, ptrdiff_t incx)
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
    size_t i = 0, k;
    for (k = 0; k != n; ++k) {
        const size_t h = find_first_nonzero(
            n - i,
            equation + i * (n + 1) + k,
            (ptrdiff_t)(n + 1)
        );
        if (h != (size_t)(-1)) {
            size_t j;
            el_swap(n, equation, i, i + h);
            for (j = i + 1; j < n; ++j) {
                eliminate(n, equation, i, j, k);
            }
            ++i;
        }
    }
}

/* solve a linear system whose coefficient matrix is upper triangular */
void solve_triangular(size_t n, num *equation)
{
    size_t k;
    for (k = n - 1; k < n; --k) {
        const size_t h = find_first_nonzero(
            k + 1,
            equation + k * (n + 1) + k,
            -(ptrdiff_t)(n + 1)
        );
        if (h != (size_t)(-1)) {
            size_t j;
            const size_t i = k - h;
            for (j = i - 1; j < k; --j) {
                eliminate(n, equation, i, j, k);
            }
        }
    }
}

/* solve a linear system of equations defined by an augmented matrix;
   the matrix is tranformed into reduced row echelon form,
   although the leading coefficients need not be 1
   (since we are working with only integers)
   `equation` is a row-major augmented matrix with dimensions `n * (n + 1)`;
   The final solution is returned as diagonal augmented matrix. */
void gauss_elim(size_t n, num *equation)
{
#ifndef NDEBUG
    print_augmatrix(n, equation);
#endif
    row_echelon(n, equation);
    solve_triangular(n, equation);
}

/* extract solutions from a matrix in reduced row echeleon form
   (leading coefficients do not need to be 1, however);
   returns 0 if there is at least one solution;
   returns 1 if there are no solutions;
   `solutions` is an `n * 2` matrix containing numerators and denominators;
   1/0 == no solution (in which case every variable will be so);
   0/0 == multiple solutions for this variable */
int get_solutions(num *solutions, size_t n, const num *equation)
{
    size_t i, j;

    /* see if there is at least one solution */
    for (i = n - 1; i < n; --i) {
        const num *row = equation + i * (n + 1);
        /* if LHS is entirely zero ... */
        if (find_first_nonzero(n, row, 1) == (size_t)(-1)) {
            /* ... but RHS is not */
            if (row[n]) {
                for (i = 0; i != n; ++i) {
                    solutions[i * 2]     = 1;
                    solutions[i * 2 + 1] = 0;
                }
                return 1;
            }
        } else {
            break;
        }
    }

    /* default each solution to 0/0 */
    for (i = 0; i != n; ++i) {
        solutions[i * 2]     = 0;
        solutions[i * 2 + 1] = 0;
    }

    /* find the parts of the solution vector that's unique */
    j = 0;
    for (i = 0; i != n; ++i) {
        size_t h;
        const num *diag = equation + i * (n + 1) + j;

        /* find the leading coefficient */
        const size_t k = find_first_nonzero(n - j, diag, 1);
        if (k == (size_t)(-1)) {
            break;
        }

        /* check if this solution is unique */
        h = find_first_nonzero(n - j - k - 1, diag + k + 1, 1);
        if (h == (size_t)(-1)) {
            solutions[(j + k) * 2]     = diag[n - j];
            solutions[(j + k) * 2 + 1] = diag[k];
        }
        j += k + 1;
    }
    return 0;
}

/* for compatibilty with Treeki's code */
int solve(const num *equation, num *solutions, int n)
{
    const size_t eqlen = n * (n + 1);
    const size_t size = (eqlen + n * 2) * sizeof(*equation);
    size_t i;
    int is_unique = 1;

    num *buf = (num *)malloc(size);
    num *equation2 = buf;
    num *solutions2 = buf + eqlen;
    memcpy(equation2, equation, size);

    /* do the magic */
    gauss_elim((size_t)n, equation2);

    if (get_solutions(solutions2, n, equation2) == 1) {
        fprintf(stderr, "warning: solution does not exist\n");
        is_unique = 0;
    }

    for (i = 0; i != n; ++i) {
        const num numer = solutions2[i * 2];
        const num denom = solutions2[i * 2 + 1];
        if (denom) {
            solutions[i] = numer / denom;
            continue;
        }
        solutions[i] = 0;
        is_unique = 0;
        if (numer) {
            continue;
        }
        fprintf(stderr, "warning: multiple solutions "
                "exist for variable #%zu\n", i);
    }

    free(buf);
    return is_unique;
}

void print_solutions(size_t n, const num *equation)
{
    size_t i;
    num *solutions = (num *)malloc(2 * n * sizeof(*solutions));
    get_solutions(solutions, n, equation);
    printf("solutions:\n");
    for (i = 0; i != n; ++i) {
        const num numer = solutions[i * 2];
        const num denom = solutions[i * 2 + 1];
        printf("%8" PRI_num " / %8" PRI_num " = %10g\n",
               numer, denom, denom ? (double)numer / denom : NAN);
    }
    free(solutions);
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
