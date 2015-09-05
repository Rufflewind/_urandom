/* Pure C definitions of the functions `i1mach`, `d1mach`, and `r1mach`, which
   are used to obtain platform-dependent constants needed for numerical
   calculations.

   These functions are needed by various Netlib libraries in Fortran, but
   unfortunately their implementations are quite ugly due to legacy support.
   This code, on the hand, provides a more modernized implementation that is
   compatible with existing Netlib libraries.

   For some compilers it may be necessary to tweak the name of the functions
   to match the appropriate Fortran name-mangling convention.
   https://en.wikipedia.org/wiki/Name_mangling#Name_mangling_in_Fortran

   Although these definitions are not 100% portable -- they make certain
   assumptions about the platform -- it's going to take a truly exotic machine
   for these assumptions to not hold.

   License: CC0 (Public Domain).
   https://creativecommons.org/publicdomain/zero/1.0
*/
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef __cplusplus
extern "C" {
#endif

/* Obtain various platform-dependent constants.
   Allowed values of `what` are:

   -  1: standard input unit
   -  2: standard output unit
   -  3: standard punch unit
   -  4: standard error unit
   -  5: size of `int` in bits
   -  6: size of `int` in bytes
   -  7: radix of integers
   -  8: number of magnitude bits in `int`
   -  9: maximum value of `int`
   - 10: radix of floating-point numbers
   - 11: number of mantissa bits in `float`
   - 12: minimum exponent of `float`
   - 13: maximum exponent of `float`
   - 14: number of mantissa bits in `double`
   - 15: minimum exponent of `double`
   - 16: maximum exponent of `double`
*/
int i1mach_(const int *what)
{
    switch (*what) {
    case 1:  return 5;
    case 2:  return 6;
    case 3:  return 7;
    case 4:  return 0;
    case 5:  return sizeof(*what) * CHAR_BIT;
    case 6:  return sizeof(*what);
    case 7:  return 2;
    case 8:  return sizeof(*what) * CHAR_BIT - 1;
    case 9:  return INT_MAX;
    case 10: return FLT_RADIX;
    case 11: return FLT_MANT_DIG;
    case 12: return FLT_MIN_EXP;
    case 13: return FLT_MAX_EXP;
    case 14: return DBL_MANT_DIG;
    case 15: return DBL_MIN_EXP;
    case 16: return DBL_MAX_EXP;
    }
    fprintf(stderr, "i1mach: %i is not a valid argument\n", *what);
    fflush(stderr);
    exit(EXIT_FAILURE);
    return 0;
}

/* Obtain various platform-dependent constants for `double`.
   Allowed values of `what` are:

   -  1: minimum normalized positive value
   -  2: maximum finite value
   -  3: smallest relative spacing
   -  4: largest relative spacing
   -  5: base-10 logarithm of radix
*/
double d1mach_(const int *what)
{
    switch (*what) {
    case 1: return DBL_MIN;
    case 2: return DBL_MAX;
    case 3: return DBL_EPSILON / FLT_RADIX;
    case 4: return DBL_EPSILON;
    case 5: return log10((double)FLT_RADIX);
    }
    fprintf(stderr, "d1mach: %i is not a valid argument\n", *what);
    fflush(stderr);
    exit(EXIT_FAILURE);
    return 0;
}

/* Obtain various platform-dependent constants for `float`.
   Allowed values of `what` are:

   -  1: minimum normalized positive value
   -  2: maximum finite value
   -  3: smallest relative spacing
   -  4: largest relative spacing
   -  5: base-10 logarithm of radix
*/
float r1mach_(const int *what)
{
    switch (*what) {
    case 1: return FLT_MIN;
    case 2: return FLT_MAX;
    case 3: return FLT_EPSILON / FLT_RADIX;
    case 4: return FLT_EPSILON;
    case 5: return log10f((float)FLT_RADIX);
    }
    fprintf(stderr, "f1mach: %i is not a valid argument\n", *what);
    fflush(stderr);
    exit(EXIT_FAILURE);
    return 0;
}

#ifdef __cplusplus
}
#endif
