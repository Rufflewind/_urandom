#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include "bernoulli.h"
#ifdef __cplusplus
extern "C" {
#endif

/* check for overflow */
static int checked_add_assign(unsigned *left, unsigned right) {
    if (UINT_MAX - right < *left)
        return 75;
    *left += right;
    return 0;
}

void *euler_zigzag_create(unsigned count) {
    /* reserve the initial elements of `row` for bookkeeping */
    static const unsigned reserved = 3;
    unsigned *row = (unsigned *) calloc(count + reserved, sizeof(unsigned));
    if (row) {
        row[0] = count;
        row[1] = 0;
        if (count) {
            unsigned center = (count - 1) / 2 + reserved;
            row[2] = center;
            row[center] = 1;
        }
    }
    return row;
}

void euler_zigzag_destroy(void *iter) {
    free(iter);
}

int euler_zigzag_next(void *iter, unsigned *out) {
    unsigned *row = (unsigned *) iter;
    unsigned count, center, i, offset, start, stop, j;

    /* check for invalid arguments */
    if (!row)
        return 22;

    /* obtain the bookkeeping parameters */
    count  = row[0];
    i      = row[1];
    center = row[2];

    /* termination condition */
    if (i >= count)
        return 1;

    /* compute using Seidel triangle method */
    offset = i ? (i - 1) / 2 : (unsigned) -1;
    if (i % 2) {
        start = center + offset;
        stop  = start  - i;
        for (j = start; j != stop; --j) {
            if (checked_add_assign(&row[j], row[j + 1])) {
                row[0] = 0; /* terminate the sequence */
                return 75;
            }
        }
        ++stop;
    } else {
        start = center - offset;
        stop  = start  + i;
        for (j = start; j != stop; ++j) {
            if (checked_add_assign(&row[j], row[j - 1])) {
                row[0] = 0; /* terminate the sequence */
                return 75;
            }
        }
        --stop;
    }

    /* yield the result */
    if (out)
        *out = row[stop];

    /* update the counter */
    row[1] = i + 1;
    return 0;
}

struct bernoulli_iter {
    /* max count */
    unsigned count;

    /* current counter */
    unsigned i;

    /* Euler zigzag iterator */
    void *zs;
};

void *bernoulli_create(unsigned count) {
    struct bernoulli_iter *iter = NULL;
    void *zs = euler_zigzag_create(count ? count - 1 : 0);
    if (zs) {
        iter = (struct bernoulli_iter *) malloc(sizeof(*iter));
        if (iter) {
            iter->count = count;
            iter->i = 0;
            iter->zs = zs;

            /* discard the first number */
            euler_zigzag_next(iter->zs, NULL);
        } else {
            euler_zigzag_destroy(zs);
        }
    }
    return iter;
}

void bernoulli_destroy(void *iter) {
    euler_zigzag_destroy(((struct bernoulli_iter *) iter)->zs);
    free(iter);
}

int bernoulli_next(void *iter_, double *out) {
    int ret;
    unsigned z;
    double result;
    struct bernoulli_iter *iter = (struct bernoulli_iter *) iter_;

    /* check for invalid arguments */
    if (!iter)
        return 22;

    /* termination condition */
    if (iter->i >= iter->count)
        return 1;

    /* handle special cases */
    switch (iter->i) {
    case 0:
        result = 1.;
        break;
    case 1:
        result = -.5;
        break;
    default:
        if ((ret = euler_zigzag_next(iter->zs, &z)))
            return ret;
        if (iter->i % 2) {
            result = 0.;
        } else {
            double denom = pow(2., (double) iter->i)
                         - pow(4., (double) iter->i);
            result = iter->i * (iter->i / 2 % 2 ? -1. : 1.) * z / denom;
        }
    }

    /* yield the result */
    if (out)
        *out = result;

    /* update the counter */
    ++iter->i;
    return 0;
}

#ifdef __cplusplus
}
#endif
