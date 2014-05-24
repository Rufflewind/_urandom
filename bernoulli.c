#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "bernoulli.h"
#ifdef __cplusplus
extern "C" {
#endif

struct bernoulli_iter {

    /* max count */
    unsigned count;

    /* current counter */
    unsigned i;

    /* Euler zigzag iterator */
    void *zs;

};

void *bernoulli_create(unsigned count) {
    struct bernoulli_iter *iter;

    /* allocate struct */
    iter = (struct bernoulli_iter *) calloc(1, sizeof(*iter));
    if (!iter)
        return NULL;
    iter->count = count;

    /* create Euler zigzag iterator */
    iter->zs = euler_zigzag_create(count ? count - 1 : 0);
    if (!iter->zs) {
        free(iter);
        return NULL;
    }

    /* discard the first element */
    euler_zigzag_next(iter->zs, NULL);

    return iter;
}

void bernoulli_destroy(void *iter) {
    euler_zigzag_destroy(((struct bernoulli_iter *) iter)->zs);
    free(iter);
}

int bernoulli_next(void *iter_, double *out) {
    int ret;
    double z, result;
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
            result = (iter->i / 2 % 2 ? -1. : 1.) * iter->i * z / denom;
        }
    }

    /* yield the result */
    if (out)
        *out = result;

    /* update the counter */
    ++iter->i;
    return 0;
}

struct euler_zigzag_iter {

    /* max count */
    unsigned count;

    /* current counter */
    unsigned i;

    /* row of numbers used in the Seidel triangle method */
    double *row;

    /* pointer to the current location in the row */
    double *loc;

};

void *euler_zigzag_create(unsigned count) {
    struct euler_zigzag_iter *iter;

    /* allocate struct */
    iter = (struct euler_zigzag_iter *) calloc(1, sizeof(*iter));
    if (!iter)
        return NULL;
    iter->count = count;

    /* allocate row */
    if (count) {
        iter->row = calloc(count, sizeof(*iter->row));
        if (!iter->row) {
            free(iter);
            return NULL;
        }
        iter->loc = &iter->row[(count - 1) / 2];
        *iter->loc = 1;
        ++iter->loc;
    }

    return iter;
}

void euler_zigzag_destroy(void *iter) {
    free(((struct euler_zigzag_iter *) iter)->row);
    free(iter);
}

int euler_zigzag_next(void *iter_, double *out) {
    struct euler_zigzag_iter *iter = (struct euler_zigzag_iter *) iter_;
    double *stop;

    /* check for invalid arguments */
    if (!iter)
        return 22;

    /* termination condition */
    if (iter->i >= iter->count)
        return 1;

    /* compute using Seidel triangle method */
    if (iter->i % 2) {
        stop = iter->loc - iter->i;
        for (; iter->loc != stop; --iter->loc)
            *iter->loc += iter->loc[1];
        ++iter->loc;
    } else {
        stop = iter->loc + iter->i;
        for (; iter->loc != stop; ++iter->loc)
            *iter->loc += iter->loc[-1];
        --iter->loc;
    }

    /* yield the result */
    if (out)
        *out = *iter->loc;

    /* update the counter */
    ++iter->i;
    return 0;
}

#ifdef __cplusplus
}
#endif
