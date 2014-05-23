#!/usr/bin/env python
from fractions import Fraction

def bernoulli(n):
    '''
    Returns an iterator that generates the first `n` Bernoulli numbers
    (sequence A027641 divided by sequence A027642) as `Fraction`s.  The
    convention used here sets the second Bernoulli number to -1/2.

    The sequence is computed using Euler zigzag numbers.
    '''
    if n <= 0:
        return
    yield Fraction(1, 1)
    if n == 1:
        return
    yield Fraction(-1, 2)
    zs = euler_zigzag(n - 1)
    next(zs)              # skip first element
    for i, z in enumerate(zs, 2):
        if i % 2 == 1:
            yield 0
        else:
            yield Fraction(z * i * (-1) ** (i // 2), (1 << i) - (1 << 2 * i))

def euler_zigzag(n):
    '''
    Returns an iterator that generates the first `n` Euler zigzag numbers
    (sequence A000111, also known as up/down numbers) as `int`s.

    The sequence is computed using the Seidel triangle method.
    '''
    if n <= 0:
        return
    center = (n - 3) // 2 + 1
    row = [0] * n
    row[center] = 1
    for i in range(-1, n - 1):
        offset = i // 2
        length = i  + 1
        if i % 2 == 1:
            start = center - offset
            stop  = start  + length
            for j in range(start, stop):
                row[j] += row[j - 1]
            yield row[stop - 1]
        else:
            start = center + offset
            stop  = start  - length
            for j in range(start, stop, -1):
                row[j] += row[j + 1]
            yield row[stop + 1]

# example usage
if __name__ == "__main__":
    for i in range(10):
        print(list(euler_zigzag(i)))
    for i in range(10):
        print(list(map(str, bernoulli(i))))
