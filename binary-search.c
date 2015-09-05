#include <stdio.h>

/*@ part main/include */
#include <stddef.h>
/*@ end */

/*@ import restrict.c */

/*@ part main */
/*@ partadd head */
int binary_search_init(size_t *i,
                       size_t imin,
                       size_t imax)
/*@ end */
{
    if (imin >= imax) {
        *i = (size_t)(-1);
        return 0;
    }
    *i = imin + (imax - imin) / 2;
    return 1;
}

/*@ partadd head */
int binary_search_step(size_t *restrict i,
                       size_t *restrict imin,
                       size_t *restrict imax,
                       int cmp)
/*@ end */
{
    if (cmp == 0) {
        return 0;
    } else if (cmp < 0) {
        if (*i == *imin)
            goto not_found;
        *imax = *i;
    } else {
        if (*i == *imax - 1)
            goto not_found;
        *imin = *i + 1;
    }
    *i = *imin + (*imax - *imin) / 2;
    return 1;
not_found:
    *i = (size_t)(-1);
    return 0;
}
/*@ end */

int main(void)
{
    /* inputs */
    const char array[] = {2, 3, 5, 7, 11};

    /* search */
    const char query = 3;
    int found;
    size_t i;
    {
        size_t imin = 0;
        size_t imax = sizeof(array) / sizeof(*array);
        if (binary_search_init(&i, imin, imax))
            while (binary_search_step(&i, &imin, &imax,
                                      query - array[i]));
        found = i != (size_t)(-1);
    }

    if (found)
        printf("found %i at %zu\n", query, i);
    else
        printf("not_found\n");
    return 0;
}
