#include <cmath>
#include <tuple>

/// Calculates floored division and modulus (rounded towards minus infinity),
/// returning a tuple of the form `(quotient, remainder)`.
///
/// This function requires C++11, not simply because of `std::tuple` but also
/// because the behavior of integer division and modulus was
/// implementation-defined until C99/C++11.
///
/// If you really want this to be compatible with earlier versions, replace
/// the division and modulus in this function with `std::div` instead.
template<class Integral>
std::tuple<Integral, Integral> div_floor(Integral x, Integral y);

template<class Integral> inline
std::tuple<Integral, Integral> div_floor(const Integral x, const Integral y) {
    typedef std::tuple<Integral, Integral> result_type;
    const Integral quot = x / y;
    const Integral rem  = x % y;
    if (rem && (x < 0) != (y < 0))
        return result_type(quot - 1, rem + y);
    return result_type(quot, rem);
}

//////////////////////////////////////////////////////////////////////////////
// The rest of this file is for testing only
//////////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include <stdio.h>

void div_trunc_int(int x, int y, int& w, int& z) {
    const std::div_t r = std::div(x, y);
    w = r.quot;
    z = r.rem;
}

void div_floor_int(int x, int y, int& w, int& z) {
    std::tie(w, z) = div_floor(x, y);
}

int check_mod(int x, int y, int w_expected, int z_expected) {
    int w, z;
    assert(w_expected * y + z_expected == x);
    div_floor_int(x, y, w, z);
    if (w != w_expected) {
        fprintf(stderr, "**FAILED** while calculating %d div %d:\n", x, y);
        fprintf(stderr, "  expected %d, got %d instead\n", w_expected, w);
        return 1;
    }
    if (z != z_expected) {
        fprintf(stderr, "**FAILED** while calculating %d div %d:\n", x, y);
        fprintf(stderr, "  expected %d, got %d instead\n", z_expected, z);
        return 1;
    }
    return 0;
}

int test_mod(void);

int main() {
    int x, y, w, z;

    x =  3;
    y =  9;
    div_trunc_int(x, y, w, z);
    printf("trunc: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);
    div_floor_int(x, y, w, z);
    printf("floor: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);

    x = -3;
    y =  9;
    div_trunc_int(x, y, w, z);
    printf("trunc: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);
    div_floor_int(x, y, w, z);
    printf("floor: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);

    x =  3;
    y = -9;
    div_trunc_int(x, y, w, z);
    printf("trunc: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);
    div_floor_int(x, y, w, z);
    printf("floor: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);

    x = -3;
    y = -9;
    div_trunc_int(x, y, w, z);
    printf("trunc: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);
    div_floor_int(x, y, w, z);
    printf("floor: %2d / %2d ==> %2d, %2d\n", x, y, w, z);
    assert(w * y + z == x);

    return test_mod();
}

/*

# generating using the following Python script
s = []
for x in range(-10, 10):
    for y in range(-10, 10):
        if y == 0:
            continue
        s.append("check_mod({}, {}, {}, {})".format(x, y, x // y, x % y))
print('''
int test_mod(void) {{
    return {};
}}
'''[1:-1].format(" || \n           ".join(s)))

*/
int test_mod(void) {
    return check_mod(-10, -10, 1, 0) ||
           check_mod(-10, -9, 1, -1) ||
           check_mod(-10, -8, 1, -2) ||
           check_mod(-10, -7, 1, -3) ||
           check_mod(-10, -6, 1, -4) ||
           check_mod(-10, -5, 2, 0) ||
           check_mod(-10, -4, 2, -2) ||
           check_mod(-10, -3, 3, -1) ||
           check_mod(-10, -2, 5, 0) ||
           check_mod(-10, -1, 10, 0) ||
           check_mod(-10, 1, -10, 0) ||
           check_mod(-10, 2, -5, 0) ||
           check_mod(-10, 3, -4, 2) ||
           check_mod(-10, 4, -3, 2) ||
           check_mod(-10, 5, -2, 0) ||
           check_mod(-10, 6, -2, 2) ||
           check_mod(-10, 7, -2, 4) ||
           check_mod(-10, 8, -2, 6) ||
           check_mod(-10, 9, -2, 8) ||
           check_mod(-9, -10, 0, -9) ||
           check_mod(-9, -9, 1, 0) ||
           check_mod(-9, -8, 1, -1) ||
           check_mod(-9, -7, 1, -2) ||
           check_mod(-9, -6, 1, -3) ||
           check_mod(-9, -5, 1, -4) ||
           check_mod(-9, -4, 2, -1) ||
           check_mod(-9, -3, 3, 0) ||
           check_mod(-9, -2, 4, -1) ||
           check_mod(-9, -1, 9, 0) ||
           check_mod(-9, 1, -9, 0) ||
           check_mod(-9, 2, -5, 1) ||
           check_mod(-9, 3, -3, 0) ||
           check_mod(-9, 4, -3, 3) ||
           check_mod(-9, 5, -2, 1) ||
           check_mod(-9, 6, -2, 3) ||
           check_mod(-9, 7, -2, 5) ||
           check_mod(-9, 8, -2, 7) ||
           check_mod(-9, 9, -1, 0) ||
           check_mod(-8, -10, 0, -8) ||
           check_mod(-8, -9, 0, -8) ||
           check_mod(-8, -8, 1, 0) ||
           check_mod(-8, -7, 1, -1) ||
           check_mod(-8, -6, 1, -2) ||
           check_mod(-8, -5, 1, -3) ||
           check_mod(-8, -4, 2, 0) ||
           check_mod(-8, -3, 2, -2) ||
           check_mod(-8, -2, 4, 0) ||
           check_mod(-8, -1, 8, 0) ||
           check_mod(-8, 1, -8, 0) ||
           check_mod(-8, 2, -4, 0) ||
           check_mod(-8, 3, -3, 1) ||
           check_mod(-8, 4, -2, 0) ||
           check_mod(-8, 5, -2, 2) ||
           check_mod(-8, 6, -2, 4) ||
           check_mod(-8, 7, -2, 6) ||
           check_mod(-8, 8, -1, 0) ||
           check_mod(-8, 9, -1, 1) ||
           check_mod(-7, -10, 0, -7) ||
           check_mod(-7, -9, 0, -7) ||
           check_mod(-7, -8, 0, -7) ||
           check_mod(-7, -7, 1, 0) ||
           check_mod(-7, -6, 1, -1) ||
           check_mod(-7, -5, 1, -2) ||
           check_mod(-7, -4, 1, -3) ||
           check_mod(-7, -3, 2, -1) ||
           check_mod(-7, -2, 3, -1) ||
           check_mod(-7, -1, 7, 0) ||
           check_mod(-7, 1, -7, 0) ||
           check_mod(-7, 2, -4, 1) ||
           check_mod(-7, 3, -3, 2) ||
           check_mod(-7, 4, -2, 1) ||
           check_mod(-7, 5, -2, 3) ||
           check_mod(-7, 6, -2, 5) ||
           check_mod(-7, 7, -1, 0) ||
           check_mod(-7, 8, -1, 1) ||
           check_mod(-7, 9, -1, 2) ||
           check_mod(-6, -10, 0, -6) ||
           check_mod(-6, -9, 0, -6) ||
           check_mod(-6, -8, 0, -6) ||
           check_mod(-6, -7, 0, -6) ||
           check_mod(-6, -6, 1, 0) ||
           check_mod(-6, -5, 1, -1) ||
           check_mod(-6, -4, 1, -2) ||
           check_mod(-6, -3, 2, 0) ||
           check_mod(-6, -2, 3, 0) ||
           check_mod(-6, -1, 6, 0) ||
           check_mod(-6, 1, -6, 0) ||
           check_mod(-6, 2, -3, 0) ||
           check_mod(-6, 3, -2, 0) ||
           check_mod(-6, 4, -2, 2) ||
           check_mod(-6, 5, -2, 4) ||
           check_mod(-6, 6, -1, 0) ||
           check_mod(-6, 7, -1, 1) ||
           check_mod(-6, 8, -1, 2) ||
           check_mod(-6, 9, -1, 3) ||
           check_mod(-5, -10, 0, -5) ||
           check_mod(-5, -9, 0, -5) ||
           check_mod(-5, -8, 0, -5) ||
           check_mod(-5, -7, 0, -5) ||
           check_mod(-5, -6, 0, -5) ||
           check_mod(-5, -5, 1, 0) ||
           check_mod(-5, -4, 1, -1) ||
           check_mod(-5, -3, 1, -2) ||
           check_mod(-5, -2, 2, -1) ||
           check_mod(-5, -1, 5, 0) ||
           check_mod(-5, 1, -5, 0) ||
           check_mod(-5, 2, -3, 1) ||
           check_mod(-5, 3, -2, 1) ||
           check_mod(-5, 4, -2, 3) ||
           check_mod(-5, 5, -1, 0) ||
           check_mod(-5, 6, -1, 1) ||
           check_mod(-5, 7, -1, 2) ||
           check_mod(-5, 8, -1, 3) ||
           check_mod(-5, 9, -1, 4) ||
           check_mod(-4, -10, 0, -4) ||
           check_mod(-4, -9, 0, -4) ||
           check_mod(-4, -8, 0, -4) ||
           check_mod(-4, -7, 0, -4) ||
           check_mod(-4, -6, 0, -4) ||
           check_mod(-4, -5, 0, -4) ||
           check_mod(-4, -4, 1, 0) ||
           check_mod(-4, -3, 1, -1) ||
           check_mod(-4, -2, 2, 0) ||
           check_mod(-4, -1, 4, 0) ||
           check_mod(-4, 1, -4, 0) ||
           check_mod(-4, 2, -2, 0) ||
           check_mod(-4, 3, -2, 2) ||
           check_mod(-4, 4, -1, 0) ||
           check_mod(-4, 5, -1, 1) ||
           check_mod(-4, 6, -1, 2) ||
           check_mod(-4, 7, -1, 3) ||
           check_mod(-4, 8, -1, 4) ||
           check_mod(-4, 9, -1, 5) ||
           check_mod(-3, -10, 0, -3) ||
           check_mod(-3, -9, 0, -3) ||
           check_mod(-3, -8, 0, -3) ||
           check_mod(-3, -7, 0, -3) ||
           check_mod(-3, -6, 0, -3) ||
           check_mod(-3, -5, 0, -3) ||
           check_mod(-3, -4, 0, -3) ||
           check_mod(-3, -3, 1, 0) ||
           check_mod(-3, -2, 1, -1) ||
           check_mod(-3, -1, 3, 0) ||
           check_mod(-3, 1, -3, 0) ||
           check_mod(-3, 2, -2, 1) ||
           check_mod(-3, 3, -1, 0) ||
           check_mod(-3, 4, -1, 1) ||
           check_mod(-3, 5, -1, 2) ||
           check_mod(-3, 6, -1, 3) ||
           check_mod(-3, 7, -1, 4) ||
           check_mod(-3, 8, -1, 5) ||
           check_mod(-3, 9, -1, 6) ||
           check_mod(-2, -10, 0, -2) ||
           check_mod(-2, -9, 0, -2) ||
           check_mod(-2, -8, 0, -2) ||
           check_mod(-2, -7, 0, -2) ||
           check_mod(-2, -6, 0, -2) ||
           check_mod(-2, -5, 0, -2) ||
           check_mod(-2, -4, 0, -2) ||
           check_mod(-2, -3, 0, -2) ||
           check_mod(-2, -2, 1, 0) ||
           check_mod(-2, -1, 2, 0) ||
           check_mod(-2, 1, -2, 0) ||
           check_mod(-2, 2, -1, 0) ||
           check_mod(-2, 3, -1, 1) ||
           check_mod(-2, 4, -1, 2) ||
           check_mod(-2, 5, -1, 3) ||
           check_mod(-2, 6, -1, 4) ||
           check_mod(-2, 7, -1, 5) ||
           check_mod(-2, 8, -1, 6) ||
           check_mod(-2, 9, -1, 7) ||
           check_mod(-1, -10, 0, -1) ||
           check_mod(-1, -9, 0, -1) ||
           check_mod(-1, -8, 0, -1) ||
           check_mod(-1, -7, 0, -1) ||
           check_mod(-1, -6, 0, -1) ||
           check_mod(-1, -5, 0, -1) ||
           check_mod(-1, -4, 0, -1) ||
           check_mod(-1, -3, 0, -1) ||
           check_mod(-1, -2, 0, -1) ||
           check_mod(-1, -1, 1, 0) ||
           check_mod(-1, 1, -1, 0) ||
           check_mod(-1, 2, -1, 1) ||
           check_mod(-1, 3, -1, 2) ||
           check_mod(-1, 4, -1, 3) ||
           check_mod(-1, 5, -1, 4) ||
           check_mod(-1, 6, -1, 5) ||
           check_mod(-1, 7, -1, 6) ||
           check_mod(-1, 8, -1, 7) ||
           check_mod(-1, 9, -1, 8) ||
           check_mod(0, -10, 0, 0) ||
           check_mod(0, -9, 0, 0) ||
           check_mod(0, -8, 0, 0) ||
           check_mod(0, -7, 0, 0) ||
           check_mod(0, -6, 0, 0) ||
           check_mod(0, -5, 0, 0) ||
           check_mod(0, -4, 0, 0) ||
           check_mod(0, -3, 0, 0) ||
           check_mod(0, -2, 0, 0) ||
           check_mod(0, -1, 0, 0) ||
           check_mod(0, 1, 0, 0) ||
           check_mod(0, 2, 0, 0) ||
           check_mod(0, 3, 0, 0) ||
           check_mod(0, 4, 0, 0) ||
           check_mod(0, 5, 0, 0) ||
           check_mod(0, 6, 0, 0) ||
           check_mod(0, 7, 0, 0) ||
           check_mod(0, 8, 0, 0) ||
           check_mod(0, 9, 0, 0) ||
           check_mod(1, -10, -1, -9) ||
           check_mod(1, -9, -1, -8) ||
           check_mod(1, -8, -1, -7) ||
           check_mod(1, -7, -1, -6) ||
           check_mod(1, -6, -1, -5) ||
           check_mod(1, -5, -1, -4) ||
           check_mod(1, -4, -1, -3) ||
           check_mod(1, -3, -1, -2) ||
           check_mod(1, -2, -1, -1) ||
           check_mod(1, -1, -1, 0) ||
           check_mod(1, 1, 1, 0) ||
           check_mod(1, 2, 0, 1) ||
           check_mod(1, 3, 0, 1) ||
           check_mod(1, 4, 0, 1) ||
           check_mod(1, 5, 0, 1) ||
           check_mod(1, 6, 0, 1) ||
           check_mod(1, 7, 0, 1) ||
           check_mod(1, 8, 0, 1) ||
           check_mod(1, 9, 0, 1) ||
           check_mod(2, -10, -1, -8) ||
           check_mod(2, -9, -1, -7) ||
           check_mod(2, -8, -1, -6) ||
           check_mod(2, -7, -1, -5) ||
           check_mod(2, -6, -1, -4) ||
           check_mod(2, -5, -1, -3) ||
           check_mod(2, -4, -1, -2) ||
           check_mod(2, -3, -1, -1) ||
           check_mod(2, -2, -1, 0) ||
           check_mod(2, -1, -2, 0) ||
           check_mod(2, 1, 2, 0) ||
           check_mod(2, 2, 1, 0) ||
           check_mod(2, 3, 0, 2) ||
           check_mod(2, 4, 0, 2) ||
           check_mod(2, 5, 0, 2) ||
           check_mod(2, 6, 0, 2) ||
           check_mod(2, 7, 0, 2) ||
           check_mod(2, 8, 0, 2) ||
           check_mod(2, 9, 0, 2) ||
           check_mod(3, -10, -1, -7) ||
           check_mod(3, -9, -1, -6) ||
           check_mod(3, -8, -1, -5) ||
           check_mod(3, -7, -1, -4) ||
           check_mod(3, -6, -1, -3) ||
           check_mod(3, -5, -1, -2) ||
           check_mod(3, -4, -1, -1) ||
           check_mod(3, -3, -1, 0) ||
           check_mod(3, -2, -2, -1) ||
           check_mod(3, -1, -3, 0) ||
           check_mod(3, 1, 3, 0) ||
           check_mod(3, 2, 1, 1) ||
           check_mod(3, 3, 1, 0) ||
           check_mod(3, 4, 0, 3) ||
           check_mod(3, 5, 0, 3) ||
           check_mod(3, 6, 0, 3) ||
           check_mod(3, 7, 0, 3) ||
           check_mod(3, 8, 0, 3) ||
           check_mod(3, 9, 0, 3) ||
           check_mod(4, -10, -1, -6) ||
           check_mod(4, -9, -1, -5) ||
           check_mod(4, -8, -1, -4) ||
           check_mod(4, -7, -1, -3) ||
           check_mod(4, -6, -1, -2) ||
           check_mod(4, -5, -1, -1) ||
           check_mod(4, -4, -1, 0) ||
           check_mod(4, -3, -2, -2) ||
           check_mod(4, -2, -2, 0) ||
           check_mod(4, -1, -4, 0) ||
           check_mod(4, 1, 4, 0) ||
           check_mod(4, 2, 2, 0) ||
           check_mod(4, 3, 1, 1) ||
           check_mod(4, 4, 1, 0) ||
           check_mod(4, 5, 0, 4) ||
           check_mod(4, 6, 0, 4) ||
           check_mod(4, 7, 0, 4) ||
           check_mod(4, 8, 0, 4) ||
           check_mod(4, 9, 0, 4) ||
           check_mod(5, -10, -1, -5) ||
           check_mod(5, -9, -1, -4) ||
           check_mod(5, -8, -1, -3) ||
           check_mod(5, -7, -1, -2) ||
           check_mod(5, -6, -1, -1) ||
           check_mod(5, -5, -1, 0) ||
           check_mod(5, -4, -2, -3) ||
           check_mod(5, -3, -2, -1) ||
           check_mod(5, -2, -3, -1) ||
           check_mod(5, -1, -5, 0) ||
           check_mod(5, 1, 5, 0) ||
           check_mod(5, 2, 2, 1) ||
           check_mod(5, 3, 1, 2) ||
           check_mod(5, 4, 1, 1) ||
           check_mod(5, 5, 1, 0) ||
           check_mod(5, 6, 0, 5) ||
           check_mod(5, 7, 0, 5) ||
           check_mod(5, 8, 0, 5) ||
           check_mod(5, 9, 0, 5) ||
           check_mod(6, -10, -1, -4) ||
           check_mod(6, -9, -1, -3) ||
           check_mod(6, -8, -1, -2) ||
           check_mod(6, -7, -1, -1) ||
           check_mod(6, -6, -1, 0) ||
           check_mod(6, -5, -2, -4) ||
           check_mod(6, -4, -2, -2) ||
           check_mod(6, -3, -2, 0) ||
           check_mod(6, -2, -3, 0) ||
           check_mod(6, -1, -6, 0) ||
           check_mod(6, 1, 6, 0) ||
           check_mod(6, 2, 3, 0) ||
           check_mod(6, 3, 2, 0) ||
           check_mod(6, 4, 1, 2) ||
           check_mod(6, 5, 1, 1) ||
           check_mod(6, 6, 1, 0) ||
           check_mod(6, 7, 0, 6) ||
           check_mod(6, 8, 0, 6) ||
           check_mod(6, 9, 0, 6) ||
           check_mod(7, -10, -1, -3) ||
           check_mod(7, -9, -1, -2) ||
           check_mod(7, -8, -1, -1) ||
           check_mod(7, -7, -1, 0) ||
           check_mod(7, -6, -2, -5) ||
           check_mod(7, -5, -2, -3) ||
           check_mod(7, -4, -2, -1) ||
           check_mod(7, -3, -3, -2) ||
           check_mod(7, -2, -4, -1) ||
           check_mod(7, -1, -7, 0) ||
           check_mod(7, 1, 7, 0) ||
           check_mod(7, 2, 3, 1) ||
           check_mod(7, 3, 2, 1) ||
           check_mod(7, 4, 1, 3) ||
           check_mod(7, 5, 1, 2) ||
           check_mod(7, 6, 1, 1) ||
           check_mod(7, 7, 1, 0) ||
           check_mod(7, 8, 0, 7) ||
           check_mod(7, 9, 0, 7) ||
           check_mod(8, -10, -1, -2) ||
           check_mod(8, -9, -1, -1) ||
           check_mod(8, -8, -1, 0) ||
           check_mod(8, -7, -2, -6) ||
           check_mod(8, -6, -2, -4) ||
           check_mod(8, -5, -2, -2) ||
           check_mod(8, -4, -2, 0) ||
           check_mod(8, -3, -3, -1) ||
           check_mod(8, -2, -4, 0) ||
           check_mod(8, -1, -8, 0) ||
           check_mod(8, 1, 8, 0) ||
           check_mod(8, 2, 4, 0) ||
           check_mod(8, 3, 2, 2) ||
           check_mod(8, 4, 2, 0) ||
           check_mod(8, 5, 1, 3) ||
           check_mod(8, 6, 1, 2) ||
           check_mod(8, 7, 1, 1) ||
           check_mod(8, 8, 1, 0) ||
           check_mod(8, 9, 0, 8) ||
           check_mod(9, -10, -1, -1) ||
           check_mod(9, -9, -1, 0) ||
           check_mod(9, -8, -2, -7) ||
           check_mod(9, -7, -2, -5) ||
           check_mod(9, -6, -2, -3) ||
           check_mod(9, -5, -2, -1) ||
           check_mod(9, -4, -3, -3) ||
           check_mod(9, -3, -3, 0) ||
           check_mod(9, -2, -5, -1) ||
           check_mod(9, -1, -9, 0) ||
           check_mod(9, 1, 9, 0) ||
           check_mod(9, 2, 4, 1) ||
           check_mod(9, 3, 3, 0) ||
           check_mod(9, 4, 2, 1) ||
           check_mod(9, 5, 1, 4) ||
           check_mod(9, 6, 1, 3) ||
           check_mod(9, 7, 1, 2) ||
           check_mod(9, 8, 1, 1) ||
           check_mod(9, 9, 1, 0);
}
